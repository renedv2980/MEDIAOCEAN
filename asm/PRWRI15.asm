*          DATA SET PRWRI15    AT LEVEL 232 AS OF 11/08/12                      
*PHASE T40515A,*                                                                
*INCLUDE GETCOST                                                                
*INCLUDE PPFMTINO                                                               
*        TITLE 'T40515 - KRTAPE CREATION'                                       
         TITLE 'T40515 - KRTAPE CREATION - PROGRAM CHANGE LOG'                  
*                                                                               
* BPLA  10/12    USE SPECIAL EDICT RECORDS FOR KRAFT SPLIT BILLING              
*                                                                               
* BPLA  08/12    ALTER FILE NAME SUFFIX BASED ON USER ID                        
*                                                                               
* BOBY  2/03/02  REVAMP FOR NEW KRAFT TAPE REQUIREMENTS'                        
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INIT'                                
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T40515   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40515,RR=RE                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     RA,=A(WORKSTG)      ESTABLISH WORKING STORAGE                    
         AR    RA,RE               RELOCATE ADDRESS                             
         USING WORKSTG,RA                                                       
*                                                                               
*        GET A(DYNALLOC)                                                        
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   INITTABX                                                         
*                                                                               
         L     RF,TWADCONS               FROM DDGENTWA                          
         MVC   VDYNALLO,TDYNALLO-TWADCOND(RF)   V(DYNALLOC)                     
*                                                                               
*        ALTER FILE NAMES FOR AGENCIES                                          
*                                                                               
         MVC   DSNKRINV+13(2),TWAAGY                                            
         MVC   DSNKREST+13(2),TWAAGY                                            
         MVI   DSNKRINV+15,C'1'      RESET DEFAULTS TO BE SAFE                  
         MVI   DSNKREST+15,C'2'      RESET DEFAULTS TO BE SAFE                  
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
         USING MASTD,R1                                                         
         CLC   MCORIGID,=H'10273'    SEE IF MVCTOK (KRAFT GROCERIES)            
         BNE   INIT5                                                            
         MVI   DSNKRINV+15,C'3'      ALTER SUFFIX                               
         MVI   DSNKREST+15,C'4'      ALTER SUFFIX                               
         B     INIT10                                                           
*                                                                               
INIT5    CLC   MCORIGID,=H'16377'    SEE IF MVCMON (KRAFT SNACKS)               
         BNE   INIT10                                                           
         MVI   DSNKRINV+15,C'5'      ALTER SUFFIX                               
         MVI   DSNKREST+15,C'6'      ALTER SUFFIX                               
         B     INIT10                                                           
         DROP  R1                                                               
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
*                                                                               
*        GET A(AREA) PRESERVED BETWEEN REQUESTS                                 
*                                                                               
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1) AREA PRESERVED BETWEEN REQUESTS         
         ST    R1,ASPFUSER                                                      
*                                                                               
*        DETERMINE ADDRESSES OF DCBS STORED IN PRESERVED AREA                   
*                                                                               
         LA    RF,KRINVFL-SPFAREA(R1)                                           
         ST    RF,AKRINVFL          NEW DCB ADDRESS                             
         LA    RF,KRCINVFL-SPFAREA(R1)                                          
         ST    RF,AKRCINVF          NEW DCB ADDRESS                             
         LA    RF,KRESTFL-SPFAREA(R1)                                           
         ST    RF,AKRESTFL          NEW DCB ADDRESS                             
*                                                                               
INITSPFX DS    0H                                                               
INITTABX DS    0H                                                               
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - CKRPMODE'                            
***********************************************************************         
*                                                                     *         
*        ANALYZE REPORT CALLING MODE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKRPMODE DS    0H                                                               
*                                                                               
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
*                                                                               
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
*                                                                               
         CLI   RPMODE,RPINPUT      PRNTIO HOOK                                  
         BE    INPUT                                                            
*                                                                               
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
*                                                                               
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FINAL                                                            
*                                                                               
         CLI   RPMODE,RPRUNLST     RUNLAST                                      
         BNE   *+12                                                             
         BRAS  RE,LST                                                           
         B     XIT                                                              
*                                                                               
         B     XIT                 UNKNOWN RP CALL                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INIT'                                
***********************************************************************         
*                                                                     *         
*        REPORT INITIALIZATION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIT     DS    0H                                                               
*                                                                               
*        ON START OF RUN MOVE DCBS TO SPFUSER AREA                              
*                                                                               
         CLI   TWAFIRST,0          IF FIRST REQUEST                             
         BNE   INIT1                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        AND IF OFF-LINE                              
         BNE   INOCLEAR                                                         
*                                                                               
*        INITIALIZE TABLE AREAS                                                 
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(500*PRTENTL)        500  PRODUCT BUFFER                      
*                                                                               
         LA    R4,SPFAPRDS         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     R1,SPFAPRDS         A(PRODUCT TABLE)                             
*                                                                               
         MVC   0(28,R1),=C'** PRWRI15  PRD TABLE    ***'                        
         LA    R1,32(R1)                                                        
         ST    R1,APRDTAB                                                       
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(300*ETBENTL)        300  ESTIMATE BUFFER                     
*                                                                               
         LA    R4,SPFAESTS         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     R1,SPFAESTS        A(ESTIMATE TABLE)                             
         MVC   0(28,R1),=C'** PRWRI15  EST TABLE    ***'                        
         LA    R1,32(R1)                                                        
         ST    R1,AESTTAB                                                       
*                                                                               
         MVI   SPINVSW,C'N'        INVOICE  FILE NOT OPEN                       
         MVI   SPESTSW,C'N'        ESTIMATE FILE NOT OPEN                       
*                                                                               
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
*                                                                               
         MVC   SPFAGYCD,AGENCYCD                                                
         MVC   SPFTITLE,TITLE                                                   
         MVC   SPFTTLSB,SUBTITLE                                                
         MVC   SPFSPLID,SPOOLID                                                 
*                                                                               
         ZAP   SPIBTCTR,=P'0'      INIT BATCH RECORD COUNTER                    
         MVI   SPIHEAD,C'H'        INDICATE INVOICE HDR TO BE PRINTED           
         MVC   SPIVBTID,SPACES     INIT BATCH ID                                
*                                                                               
*        CREATE INVOICE BATCH ID OF DATE AND TIME                               
*                                                                               
         TIME  DEC                 R0 - TIME PUS 0HHMMSS.SS                     
*                                  R1 - DATE  P   0CYYDDDF                      
         ST    R1,FULL             DATE                                         
         OI    FULL+3,X'0F'        FORCE SIGN                                   
         UNPK  SPIVBTID(5),FULL+1(3)  DISPLAY DATE                              
         MVC   WKYEAR,SPIVBTID     SAVE YEAR FOR INVOICE NUMBER                 
         MVI   SPIVBTID+05,C' '    CLEAR EXTRA BYTE                             
*                                    RETURNED IN R0                             
         ZAP   DUB,=P'0'           INIT PACKED FIELD                            
         STCM  R0,15,DUB+3         SAVE P(HHMMSSSS0C)                           
*                                                                               
         AP    DUB,=P'60000000'   ADD SIX HOURS                                 
*                                                                               
         UNPK  SPIVBTID+5(9),DUB+3(5)   PRINT TIME                              
         MVI   SPIVBTID+13,C' '    CLEAR EXTRA BYTE                             
*                                                                               
         ZAP   SPIVNET,=P'0'       INIT MONEY ACCUMULATORS                      
         ZAP   SPIVCOM,=P'0'                                                    
         ZAP   SPIVADJ,=P'0'                                                    
         ZAP   SPIVGRS,=P'0'                                                    
*                                                                               
*        CREATE ESTIMATE BATCH ID OF DATE AND TIME                              
*                                                                               
         ZAP   SPEBTCTR,=P'0'      INIT BATCH RECORD COUNTER                    
         MVI   SPEHEAD,C'H'        INDICATE INVOICE HDR TO BE PRINTED           
         MVC   SPESBTID,SPACES     INIT BATCH ID                                
*                                                                               
         TIME  DEC                 R0 - TIME PUS 0HHMMSS.SS                     
*                                  R1 - DATE  P   0CYYDDDF                      
         ST    R1,FULL             DATE                                         
         OI    FULL+3,X'0F'        FORCE SIGN                                   
         UNPK  SPESBTID(5),FULL+1(3) DISPLAY DATE                               
         MVI   SPESBTID+05,C' '    CLEAR EXTRA BYTE                             
*                                    RETURNED IN R0                             
         ZAP   DUB,=P'0'           INIT PACKED FIELD                            
         STCM  R0,15,DUB+3         SAVE P(HHMMSSSS0C)                           
*                                                                               
         AP    DUB,=P'60000000'   ADD SIX HOURS                                 
         AP    DUB,=P'00000010'   ADD ONE TO FORCE NEW ID                       
*                                                                               
         UNPK  SPESBTID+5(9),DUB+3(5)  PRINT TIME                               
         MVI   SPESBTID+13,C' '    CLEAR EXTRA BYTE                             
*                                                                               
         ZAP   SPESNET,=P'0'       INIT MONEY ACCUMULATORS                      
         ZAP   SPESCOM,=P'0'                                                    
         ZAP   SPESADJ,=P'0'                                                    
         ZAP   SPESGRS,=P'0'                                                    
*                                                                               
*        READ IN AGENCY RECORD - VARIES BY MEDIA                                
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
*                                                                               
         LA    R2,KEY              ESTABLISH AS PAGYREC KEY                     
         USING PAGYRECD,R2                                                      
*                                                                               
         MVC   PAGYKAGY,TWAAGY     SET AGENCY                                   
         MVI   PAGYKMED,C'M'       SET MEDIA                                    
         MVI   PAGYKRCD,PAGYKIDQ   SET RECORD ID                                
*                                                                               
         GOTOR HIGH                                                             
*                                                                               
         CLC   KEY(L'PAGYKEY),KEYSAVE  SKIP IF RECORD KEY NOT FOUND             
         BE    *+6                                                              
         DC    H'0'                            NO ERRORS TOLERATED              
*                                                                               
         GOTOR GETREC                ELSE READ RECORD INTO IO1                  
         BE    *+6                                                              
         DC    H'0'                            NO ERRORS TOLERATED              
*                                                                               
         L     R2,AIO1             POINT TO AGENCY RECORD                       
         MVC   KRCTRY,PAGYNAT      SAVE COUNTRY CODE                            
*                                                                               
         CLI   GFTTAP,C'Y'         SKIP IF SUPPRESSING TAPE                     
         BE    INITDYNX                                                         
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+10                                                             
         MVC   DDKRINV,=C'KRCINVFL'   CHANGE DCB                                
*                                                                               
         GOTO1 VDYNALLO,DMCB,DDKRINV,DSNKRINV  ALLOCATE DATASET                 
*                                                                               
         LINKX MF=(E,PARMLST1),SF=(E,LINKLST)   GET GENERATION NAME             
*                                                                               
         GOTO1 VDYNALLO,DMCB,DDKREST,DSNKREST  ALLOCATE DATASET                 
*                                                                               
         LINKX MF=(E,PARMLST2),SF=(E,LINKLST)   GET GENERATION NAME             
*                                                                               
INITDYNX DS    0H                                                               
*                                                                               
         L     R0,ASPFUSER         SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL         AND DCBS                                     
         LA    RE,SPFAREA                                                       
         LR    RF,R1                                                            
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
         MVI   LASTHEAD,9         SET NUMBER OF HEADLINES                       
         MVI   MYFIRSTH,9         SET NUMBER OF HEADLINES                       
*                                                                               
         B     INIT2                                                            
*                                                                               
INIT1    DS    0H                                                               
*                                                                               
         L     RE,ASPFUSER         NO-RESTORE SAVED VALUES                      
         LA    RF,SPFAREAL                                                      
         LA    R0,SPFAREA                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
INIT2    DS    0H                                                               
*                                                                               
         BRAS  RE,INIESTID         INIT EST ID TABLE                            
*                                                                               
         OI    PBQREAD,PBQRDBLS    READ BILL RECORDS                            
         OI    PBQREAD,PBQRDBKS    READ BUCKET RECS - PASS ELEM AT              
*                                  A TIME                                       
         CLI   OFFLINE,C'Y'        IF ONLINE BYPASS                             
         BNE   INOCLEAR                                                         
*                                                                               
         XC    PROGPROX,PROGPROX   CLEAR PROFILE                                
*                                                                               
*        MUST READ PB1X PROFILE FOR                                             
*        INV MTH DISPLAY                                                        
*                                                                               
         XC    WORK,WORK                                          L05           
         MVC   WORK(4),=C'PB1X'                                   L05           
         NI    WORK,X'BF'                   LOWER CASE            L05           
         MVC   WORK+4(2),PBQAGY                                   L05           
         MVC   WORK+6(1),PBMED                                   L05            
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROGPROX,DATAMGR             L05               
*                                                                               
         MVI   MYFIRSTH,10         SET DRIVER'S FIRST HEADLINE                  
         XC    MONTAB,MONTAB       INITIALIZE MONTH TABLE                       
         XC    SVPRD,SVPRD                                                      
         XC    SVEST,SVEST                                                      
*                                                                               
                                                                                
         L     RE,APRDTAB          INIT  PRODUCT AND EST TABLES                 
         XC    0(PRTENTL,RE),0(RE)                                              
*                                                                               
         L     RE,AESTTAB                                                       
         XC    0(ETBENTL,RE),0(RE)                                              
*                                                                               
         MVI   IVREC,C' '          INIT INVOICE RECORD                          
         MVC   IVREC+1(IVRECL-1),IVREC                                          
*                                                                               
INOCLEAR DS    0H                                                               
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
*                                                                               
         LA    R2,GFTTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   TITLE,WORK                                                       
         OC    TITLE,SPACES                                                     
*                                                                               
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         MVI   PBQPRTYP,PBQPRMOS   INDICATE PERIOD IS FOR                       
*                                    MONTH OF SERVICE                           
INITX    B     XIT                                                              
*                                                                               
PROGPROX DC    XL16'0'                                                          
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - VALID'                               
***********************************************************************         
*                                                                     *         
*        REQUEST VALIDATION                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALID    DS    0H                  TEST PRD=POL REQUEST                         
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
         USING MASTD,R1                                                         
         CLC   MCORIGID,=H'10273'    SEE IF MVCTOK (KRAFT GROCERIES)            
         BNE   VALID5                                                           
         MVC   PBQDIV,=C'006'        SET GROCERY DIVISION                       
         MVC   GFTDIV(3),=C'006'                                                
         NI    GFTDIVH+1,X'FF'-X'20'  UNPROTECT DIV                             
         OI    GFTDIVH+1,X'01'       MODIFIED                                   
         OI    GFTDIVH+6,X'80'       TRANSMIT - NEEDED?                         
         OI    GFTDIVH+5,X'03'       INPUT LENGTH                               
         MVC   GFTDNA(3),=C'DIV'     ANNOTATION                                 
*                                                                               
VALID5   CLC   MCORIGID,=H'16377'    SEE IF MVCMON (KRAFT SNACKS)               
         BNE   VALID10                                                          
         MVC   PBQDIV,=C'007'        SET SNACK DIVISION                         
         MVC   GFTDIV(3),=C'007'                                                
         NI    GFTDIVH+1,X'FF'-X'20'  UNPROTECT DIV                             
         OI    GFTDIVH+1,X'01'       MODIFIED                                   
         OI    GFTDIVH+6,X'80'       TRANSMIT - NEEDED?                         
         OI    GFTDIVH+5,X'03'       INPUT LENGTH                               
         MVC   GFTDNA(3),=C'DIV'     ANNOTATION                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
VALID10  LA    R2,GFTMONH          VALIDATE BILLING MONTH                       
         GOTO1 ANY                                                              
*                                                                               
*        CHECK IF RFP SYMBOLIC PARAMETER ENTERED                                
*                                                                               
         OC    ARFPBLK,ARFPBLK     SKIP IF RFP NOT BEING USED                   
         BZ    VBMNRFPN                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP BLOCK                          
         USING RFPBLK,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF VIABLE SYMBOLIC NAMES              
         BZ    VBMNRFPN            NONE                                         
*                                                                               
         CLC   RFPVSYMB,WORK       MATCH INPUT TO SYMBOLIC PARAMETER            
         BE    *+10                   EXPANDED FORMAT                           
         CLC   RFPVSYME(L'RFPVSYME-1),WORK MATCH INPUT TO SYMBOLIC PARM         
         BE    *+16                  ESCAPE SEQ IN CASE LEFT OVER               
*                                     FROM LAST TRANSACTION                     
         LA    R4,RFPVSYML(R4)     BUMP TO NEXT SYMBOLIC PARAMETER              
         BCT   R0,*-24                                                          
         B     VBMNRFPN            NO SYMBOLIC PARAMETER FOUND                  
*                                                                               
         CLC   =AL2(PP#BMON),RFPVSYME+1 MUST BE BMON SYMBOLIC                   
         BNE   ININVE                                                           
*                                                                               
         XC    8(L'GFTMON,R2),8(R2) INIT PERIOD FIELD                           
         MVC   8(L'RFPVSYME,R2),RFPVSYME REPLACE KEYWORD WITH ESC SEQ           
*                                                                               
         LA    RF,L'GFTMON         MAX LENGTH FOR PERIOD                        
         STC   RF,5(R2)            CHANGE LENGTH TO MAX INPUT FOR RFP           
         STC   RF,L'RFPVSYME-1+8(R2)   CHG LEN IN ESCAPE SEQ                    
*                                                                               
         B     VBMONX                                                           
*                                                                               
VBMNRFPN DS    0H                                                               
*                                                                               
         GOTO1 DATVAL,DMCB,(2,WORK),DUB                                         
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    ININVE                                                           
*                                                                               
         MVC   BILLMON,DUB         SAVE BILLING MONTH YYMM                      
         MVC   DUB+4(2),=C'01'                                                  
*                                                                               
         GOTO1 DATCON,DMCB,DUB,(3,PBQBLLST)    MONTH OF BILLING                 
*                                                                               
         MVC   DUB+4(2),=C'31'                                                  
*                                                                               
         GOTO1 (RF),(R1),DUB,(3,PBQBLLND)      START/END DATES                  
*                                                                               
         MVC   DUB+4(2),=C'01'                                                  
*                                                                               
         GOTO1 (RF),(R1),DUB,(X'20',WORK)      REQUEST CARD FORMAT              
*                                                                               
         MVC   BILLMON(4),WORK                                                  
*                                                                               
         MVC   SUBTITLE(12),=C'PERIOD FROM '                                    
         GOTO1 DATCON,DMCB,(3,PBQBST),(6,SUBTITLE+12)                           
         MVC   SUBTITLE+19(3),=C'TO '                                           
         GOTO1 DATCON,DMCB,(3,PBQBEND),(6,SUBTITLE+22)                          
*                                                                               
         MVI   LASTHEAD,9                                                       
         MVI   MYFIRSTH,9                                                       
*                                                                               
         OC    SUBTITLE,SPACES                                                  
         GOTO1 CENTER,DMCB,SUBTITLE,36                                          
*                                                                               
VBMONX   DS    0H                                                               
*                                                                               
         MVI   SUPTAP,C'N'                                                      
*                                                                               
         LA    R2,GFTTAPH          OPTION TO SUPPRESS TAPE                      
         CLI   5(R2),0                                                          
         BE    VSUPTP1                                                          
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   SUPTAP,WORK                                                      
*                                                                               
         CLI   SUPTAP,C'Y'                                                      
         BE    *+8                                                              
         CLI   SUPTAP,C'N'                                                      
         BE    *+8                                                              
         CLI   SUPTAP,C'T'         TAPE BUT NOT EDICT                           
         BNE   ININVE                                                           
*                                                                               
         CLI   SUPTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    VSUPTPX                                                          
*                                                                               
VSUPTP1  DS    0H                                                               
*                                                                               
         CLC   =C'SOON',CONWHEN     MUST NOT BE SOON REQUEST                    
         BE    ININVE                                                           
*                                                                               
VSUPTPX  DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ININVE'                              
***********************************************************************         
*                                                                     *         
*        ERROR MESSAGES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ININVE   MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INPUT'                               
***********************************************************************         
*                                                                     *         
*        SORT IN                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INPUT    DS    0H                                                               
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
*        HANDLE FIRST TIME FOR CLIENT                                           
*                                                                               
         CLI   PBMODE,PBPROCCL     CLIENT FIRST                                 
         BNE   INCLX                                                            
*                                                                               
         MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         MVI   AGENCYCD,C' '                                                    
*                                                                               
         CLC   PBAGY,=C'YN'        FOR YN                                       
         BNE   INCLYNN                                                          
         CLC   PBCLT,=C'KRM'       CLT KRM GETS AGENCY CODE 5                   
         BNE   INCLAGY1                                                         
         MVI   AGENCYCD,C'5'                                                    
         B     INCLX                                                            
*                                                                               
INCLYNN  DS    0H                                                               
*                                                                               
         CLC   PBAGY,=C'H7'        IF MINDSHARE IS THE AGENCY                   
         BNE   INCLH7N                                                          
*                                                                               
         CLC   PBCLT,=C'KR '          CLIENT KR  IS JWT                         
         BE    *+10                                                             
         CLC   PBCLT,=C'KYP'          CLIENT KYP IS JWT                         
         BE    *+10                                                             
         CLC   PBCLT,=C'MG '          CLIENT MG  IS JWT                         
         BNE   *+12                                                             
         MVI   AGENCYCD,C'W'                                                    
         B     INCLX                                                            
*                                                                               
         CLC   PBCLT,=C'KRG'          CLIENT KRG IS OM                          
         BNE   *+12                                                             
         MVI   AGENCYCD,C'M'                                                    
         B     INCLX                                                            
*                                                                               
         OI    ERRCD,ERRAGY           ELSE INVALID AGENCY                       
         B     ININVE                                                           
*                                                                               
INCLH7N  DS    0H                                                               
*                                                                               
INCLAGY1 DS    0H                                                               
*                                                                               
*        FIND AGENCY IN TABLE                                                   
*                                                                               
         LA    R1,AGYTAB           FIND GF AGENCY CODE                          
*                                                                               
INCLAGYL DS    0H                                                               
*                                                                               
         CLI   0(R1),0             DONE AT END OF TABLE                         
         BE    INCLAGYD                                                         
*                                                                               
         CLC   PBAGY,0(R1)         MATCH ON AGENCY CODE                         
         BE    INCLAGYF                                                         
*                                                                               
INCLAGYC DS    0H                                                               
*                                                                               
         LA    R1,3(R1)            BUMP TO NEXT ENTRY IN TABLE                  
         B     INCLAGYL                                                         
*                                                                               
INCLAGYD DS    0H                                                               
*                                                                               
         OI    ERRCD,ERRAGY        INVALID AGENCY ERROR                         
         B     ININVE                                                           
*                                                                               
INCLAGYF DS    0H                                                               
*                                                                               
         MVC   AGENCYCD,2(R1)      SAVE AGENCY ID                               
*                                                                               
         MVI   GLFHEADL,9          SET FIRST LINE FOR HEADLINES                 
         MVI   GLLHEADL,14         SET LAST  LINE FOR HEADLINES                 
*                                                                               
         B     INCLX                                                            
*                                                                               
AGYTAB   DC    CL2'OM',CL1'M'                                                   
         DC    CL2'H9',CL1'Q'      MEDIAVEST                                    
         DC    CL2'O0',CL1'Q'      MEDIAVEST TORONTO                            
         DC    CL2'YN',CL1'A'                                                   
         DC    CL2'JW',CL1'W'                                     L04           
         DC    CL2'JT',CL1'W'      JWT TORONTO                    L04           
         DC    CL2'FC',CL1'C'                                     L04           
         DC    CL2'WW',CL1'H'                                     L04           
         DC    CL2'SJ',CL1'S'   SJR TEST                                        
         DC    X'00'                                                            
*                                                                               
INCLX    DS    0H                                                               
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INBILL'                              
***********************************************************************         
*                                                                     *         
*        BILL RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INBILL   DS    0H                                                               
*                                                                               
*        PROCESS BILL HEADER RECORDS                                            
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   INPUTX                                                           
*                                                                               
         CLI   PBMODE,PBPROCBL     IF PROCESSING BILL RECORDS                   
         BNE   INBILLN                                                          
*                                                                               
         L     RF,AIO1             POINT TO BILL HEADER RECORD    L04           
         USING PBILLRCD,RF                                        L04           
*                                                                               
*        SKIP AOR CHECK FOR YNR CLIENT KRX AND SJR                              
*                                                                               
         CLI   AGENCYCD,C'S'       SKIP IF SJR                                  
         BE    INBLAORX                                                         
*                                                                               
         CLI   AGENCYCD,C'Y'       SKIP IF YNR AND                              
         BNE   *+14                                                             
         CLC   PBCLT,=C'KRX'       CLIENT KRX                                   
         BE    INBLAORX                                                         
*                                                                               
         TM    PBILCMSW,X'20'      IF AOR BILL                                  
         BNO   INBLAORX                                                         
*                                                                               
         MVI   PBMODE,00              CHANGE MODE TO BYPASS PROCESS             
*                                     OF BILLING RECORD                         
         B     INPUTX                                                           
*                                                                               
INBLAORX DS    0H                                                               
*                                                                               
         B     INMNTB              GO BUILD MONTH TABLE                         
*                                                                               
         DROP  RF                                                               
*                                                                               
INBILLN  DS    0H                                                               
*                                                                               
*        HANDLE ESTIMATE BUCKET RECORDS                                         
*                                                                               
         CLI   PBMODE,PBPROCBK     BUCKET RECORDS                               
         BNE   INPUTX                                                           
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INMNTB'                              
***********************************************************************         
*                                                                     *         
*        MONTH TABLE                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                  MONTAB IS A TABLE OF BINARY                  
*                                  YY/MM FROM REQ START TO REQ END              
*                                                                               
INMNTB   DS    0H                                                               
*                                                                               
         OC    MONTAB,MONTAB       SKIP IF MONTH TABLE SET                      
         BNZ   INMNTBX                                                          
*                                                                               
         LA    R3,MONTAB                                                        
         MVC   0(2,R3),PBQBST      BINARY                                       
         MVC   2(4,R3),PBQSTART    CHARACTERS                                   
*                                                                               
INMNTBLP DS    0H                                                               
*                                                                               
         MVC   FULL(2),0(R3)                                                    
         ZIC   R1,FULL+1           BUMP MONTH                                   
         LA    R1,1(R1)                                                         
         STC   R1,FULL+1                                                        
*                                                                               
         CH    R1,=H'13'                                                        
         BL    INMNTB10            SAME YEAR                                    
*                                                                               
         MVI   FULL+1,1                                                         
         ZIC   R4,FULL                                                          
         LA    R4,1(R4)                                                         
         STC   R4,FULL                                                          
*                                                                               
INMNTB10 DS    0H                                                               
*                                                                               
         CLC   FULL(2),PBQBEND   CHECK VS. END YR MTH                           
         BH    INMNTBDN                                                         
*                                                                               
         LA    R3,6(R3)            BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         MVC   0(2,R3),FULL        BINARY MONTH                                 
         MVI   FULL+3,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(0,DUB) CHARACTER MONTH                     
         MVC   2(4,R3),DUB                                                      
*                                                                               
INMNTBCN DS    0H                                                               
*                                                                               
         BCT   R0,INMNTBLP                                                      
*                                                                               
INMNTBDN DS    0H                                                               
*                                                                               
         MVC   0(2,R3),=X'FFFF'    SET END OF TABLE                             
*                                                                               
INMNTBX  DS    0H                                                               
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INPRD'                               
***********************************************************************         
*                                                                     *         
*        CHANGE IN PRODUCT AND/OR ESTIMATE                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INPRD    DS    0H                                                               
*                                                                               
         CLC   PBPROD,SVPRD       ON CHANGE OF PRODUCT                          
         BE    INPRDX                                                           
*                                                                               
*        BUILD ESTIMATE TABLES                                                  
*                                                                               
*                                 MUST BUILD FOR EACH PRODUCT                   
         L     RE,AESTTAB                                         L01           
         XC    0(ETBENTL,RE),0(RE)                                              
*                                                                               
         XC    SVBFEST,SVBFEST     FORCE CHANGE IN ESTIMATE                     
         XC    SVEST,SVEST         FORCE CHANGE IN ESTIMATE                     
*                                                                               
         BRAS  RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
*                                                                               
         MVC   SVPRD,PBPROD        SAVE CURRENT PRODUCT                         
*                                                                               
INPRDX   DS    0H                                                               
*                                                                               
         CLC   PBEST,SVEST        TEST CHANGE OF ESTIMATE                       
         BE    INESTX                                                           
*                                                                               
         BRAS  RE,GETEST           YES-GET GF CODES FOR ESTIMATE                
         MVC   SVEST,PBEST                                                      
*                                                                               
INESTX   DS    0H                                                               
*                                                                               
         CLC   PBEST,SVBFEST                                                    
         BE    INBFESTX                                                         
*                                                                               
         MVC   SVBFEST,PBEST                                                    
*                                                                               
INBFESTX DS    0H                                                               
*                                                                               
         B     INPUTX                                                           
*                                                                               
INPUTX   B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - DRHOOK'                              
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRHKNTR  NTR1  ,                                                                
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+8                                                              
         MVI   GLOPTS+3,C'C'          CANADIAN REPORT                           
*                                                                               
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT                                  
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - RESOLVE'                             
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RESOLVE  DS    0H                                                               
         GOTOR RESOLVER                                                         
         B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - DRVINIT'                             
***********************************************************************         
*                                                                     *         
*        DRIVER INITIALIZATION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         MVI   GLOPTS+2,3          REPORT 3 = TAPE RECORD MAP                   
         B     XIT                                                              
         TITLE 'T40515 - KRTAPE CREATION - EXEC'                                
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK TO EXECUTE ROUTINES                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R6,PBGRS      PBGRS SHOULD HAVE ADDRESS OF BKELEM                
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         CLI   REJECT,C'D'         RESET REJECTION SWITCHES                     
         BNE   *+8                                                              
         MVI   REJECT,C'N'                                                      
*                                                                               
         CLI   MONEY,C'D'         RESET REJECTION SWITCHES                      
         BNE   *+8                                                              
         MVI   MONEY,C'N'                                                       
*                                                                               
         L     R5,PBAIO1                                                        
         USING PBILLRCD,R5                                                      
*                                                                               
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC2                                                            
*                                                                               
         L     R1,GLADTENT                                                      
*                                                                               
         CLI   DRINLEV-DRIND(R1),1 TEST LEVEL 1                                 
         BH    EXEC2                                                            
*                                                                               
         MVI   REJECT,C'N'         YES-RESET THE REJECT SWITCH                  
         MVI   MONEY,C'N'          RESET MONEY SWITCH                           
*                                                                               
EXEC2    DS    0H                                                               
EXIINGO  DS    0H                                                               
*                                                                               
*        BECAUSE NOT ALL ROUTINES SAVE REGISTERS ON ENTRY                       
*        WE NEED SPECIAL CODE TO FORCE RETURN TO HERE                           
*                                                                               
         LA    RE,EXIINRET         POINT TO RETURN POINT                        
EXINSAV  NTR1                      SAVE CURRENT REGISTERS                       
*                                                                               
         L     RF,GLAROUT       ** BRANCH TO I/O ROUTINE **                     
         BASR  RE,RF                                                            
*                               ROUTINES THAT SAVE REGS RETURN HERE             
         XIT1                   UNSAVE REGISTERS                                
*                                                                               
EXIINRET DS    0H                  EVERYONE WINDS UP HERE                       
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PUTSRT'                              
***********************************************************************         
*                                                                     *         
*        DRIVER ABOUT TO PUT TO SORT                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTSRT   DS    0H                                                               
*                                                                               
         CLI   REJECT,C'Y'         IF REJECT SWITCH IS ON                       
         BE    *+8                                                              
         CLI   REJECT,C'D'         IF REJECT SWITCH IS ON                       
         BNE   *+12                                                             
         MVI   GLHOOK,GLDONT          DON'T PUT RECORD TO SORT                  
         MVI   REJECT,C'D'            INDICATE A DELETION DONE                  
*                                                                               
         CLI   MONEY,C'Y'          IF MONEY SWITCH IS ON                        
         BE    *+8                                                              
         CLI   MONEY,C'D'          IF MONEY SWITCH IS ON                        
         BNE   *+12                                                             
         MVI   GLHOOK,GLDONT          DON'T PUT RECORD TO SORT                  
         MVI   MONEY,C'D'             INDICATE A DELETION DONE                  
*                                                                               
*        NEED THIS TRICK BECAUSE DRIVER ADDS EXTRA RECORD FOR EACH              
*        LEVEL OF TOTALLING AND SO NEED TO DELETE SEVERAL RECORDS BUT           
*        ALSO NEED TO KNOW WHEN TO RESET SWITCH. SINCE ALL RECORDS ARE          
*        ADDED CONSECUTIVELY WITHOUT INVOKING ANY OF OUR ROUTINES               
*        WE SHOULD BE SAFE IN RESETTING WHEN NEXT EXECUTED ROUTINE              
*        SEES A C'D' AS SWITCH VALUE.                                           
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - HEADHOOK'                            
***********************************************************************         
*                                                                     *         
*        HEADLINE HOOK                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HEADHK   L     R2,AH4                                                           
         A     R2,PWIDTH           R2=A(HEADLINE 5)                             
         A     R2,PWIDTH           R2=A(HEADLINE 6)                             
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE REPORT                             
         BNE   *+14                                                             
         MVC   70(26,R2),=C'** KRAFT INVOICE REPORT **'                         
         B     HEADHKX                                                          
*                                                                               
         CLI   SVRECID,C'2'       IF ESTIMATE REPORT                            
         BNE   *+14                                                             
         MVC   70(26,R2),=C'** KRAFT ESTIMATE REPORT **'                        
         B     HEADHKX                                                          
*                                                                               
HEADHKX  B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PRINT'                               
***********************************************************************         
*                                                                     *         
*        ABOUT TO PRINT A LINE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRINT    DS    0H                                                               
*                                                                               
         CLI   REJECT,C'Y'         IF LINE REJECTED                             
         BE    *+8                                                              
         CLI   MONEY,C'Y'          OR $0,                                       
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT       TELL DRIVER                                  
*                                                                               
         MVI   REJECT,C'N'                                                      
         MVI   MONEY,C'N'                                                       
*                                                                               
PRINTX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - FINAL'                               
***********************************************************************         
*                                                                     *         
*        FINAL HOOK                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FINAL    DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   FINALX                                                           
*                                                                               
         L     R0,ASPFUSER         SAVE INTER-REQUEST VALUES                    
         LA    R1,SPFAREAL                                                      
         LA    RE,SPFAREA                                                       
         LR    RF,R1                                                            
*                                                                               
         MVC   SPFAGYCD,AGENCYCD                                                
         MVC   SPFTITLE,TITLE                                                   
         MVC   SPFTTLSB,SUBTITLE                                                
         MVC   SPFSPLID,SPOOLID                                                 
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
FINALX   DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - LST'                                 
***********************************************************************         
*                                                                     *         
*        RUNLAST                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LST      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   LSTX                                                             
*                                                                               
         L     RE,ASPFUSER         RESTORE SAVED VALUES                         
         LA    RF,SPFAREAL                                                      
         LA    R0,SPFAREA                                                       
*                                                                               
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   GFTTAP,C'Y'         SKIP IF SUPPRESSING TAPE                     
         BE    LSTX                                                             
*                                                                               
*        FIND DESTINATION ID FOR EDICT TRANSMISSION                             
*                                                                               
         LA    R1,AIDTAB           POINT TO DESTINATION TABLE                   
*                                                                               
LSTAIDLP DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR END OF TABLE                       
         BNE   *+6                                                              
         DC    H'0'                MUST FIND AN ENTRY IN THE TABLE              
*                                                                               
         CLC   TWAAGY,0(R1)        MATCH ON 2 CH ALPHA                          
         BE    LSTAIDFD                                                         
*                                                                               
LSTAIDCN DS    0H                                                               
         LA    R1,AIDTABLQ(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     LSTAIDLP                                                         
*                                                                               
LSTAIDFD DS    0H                                                               
*                                                                               
         MVC   AGYID,2(R1)         SAVE AGENCY ID                               
*                                                                               
         CLI   SPIHEAD,C'H'        SKIP IF HEADER STILL TO BE WRITTEN           
         BE    LSTIVX                NO DETAILS HAVE BEEN WRITTEN               
*                                                                               
         MVI   IVCREC,C' '         INIT INVOICE CONTROL RECORD                  
         MVC   IVCREC+1(256),IVCREC                                             
         MVC   IVCREC+257(L'IVCREC-256-1),IVCREC                                
*                                                                               
         MVC   IVCBTID,SPIVBTID    SET BATCH ID                                 
         MVC   IVCBLAGY,SPIBLAGY   SET BILLING AGENCY                           
*                                                                               
         EDIT  (P8,SPIVNET),IVCNET,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   IVCNET+L'IVCNET-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   IVCNET+L'IVCNET-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (P8,SPIVCOM),IVCCOM,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   =C'YN',TWAAGY       IF YNR                                       
         BNE   *+14                                                             
         MVC   IVCCOM,SPACES          RETURN SPACES                             
         B     LSTCOMX                                                          
*                                                                               
         CLC   IVCCOM+L'IVCCOM-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   IVCCOM+L'IVCCOM-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
LSTCOMX  DS    0H                                                               
*                                                                               
         EDIT  (P8,SPIVADJ),IVCADJ,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   IVCADJ+L'IVCADJ-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   IVCADJ+L'IVCADJ-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (P8,SPIVGRS),IVCGRS,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   IVCGRS+L'IVCGRS-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   IVCGRS+L'IVCGRS-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         OI    SPIBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  IVCRECCT,SPIBTCTR                                                
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         LA    R6,IVCREC           POINT TO OUTPUT BATCH CONTROL REC            
*                                                                               
         PUT   (R3),(R6)           PUT TO FILE                                  
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
         MVI   IBTREC,C' '         INIT TRAILER RECORD                          
         MVC   IBTREC+1(256),IBTREC                                             
         MVC   IBTREC+257(L'IBTREC-256-1),IBTREC                                
*                                                                               
         MVC   IBTRECID,=C'AI '                                                 
         MVC   IBTSTRT,=CL17'TTRL'                                              
*                                                                               
         OI    SPIBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  IBTRECCT,SPIBTCTR                                                
*                                                                               
         LA    R6,IBTREC           PUT TRAILER RECORD TO TAPE                   
*                                                                               
         PUT   (R3),(R6)                                                        
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
         L     R3,AKRINVFL                                                      
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         CLOSE ((R3))              CLOSE INVOICE  FILE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   GFTTAP,C'T'         SKIP IF ONLY PRODUCING TAPE                  
         BE    LSTIVX                                                           
*                                                                               
         MVC   P,SPACES            CLOSE REPORT                                 
         GOTO1 VPRINT,DMCB,P-1,=C'CLOSE'  CLOSE REPORT                          
*                                                                               
*        DIRECT OUTPUT TO CLASS G OF PRINT QUEUE                                
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTKEY(6),=C'KRTAPE' REPORT TITLE                              
         MVC   REMOTDST,=X'0011'   SEND TO SJR                                  
*                                                                               
         CLI   REMOTLPP,68         FORCE SOME CHANGE IN REMOTE AREA             
         BNE   *+12                 THIS SHOULD FORCE THE OPENING OF            
         MVI   REMOTLPP,66          A NEW REPORT                                
         B     *+8                                                              
         MVI   REMOTLPP,68                                                      
*                                                                               
         MVC   REMOTJID,=C'PKR'                                                 
*                                                                               
         MVI   REMOTCLS,C'G'       SET FOR CLASS G                              
*                                                                               
         CLI   PQSW,0              IF PRINT QUEUE NOT OPEN                      
         BNE   LSTOPNX                                                          
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
LSTOPNX  DS    0H                                                               
*                                                                               
*        GENERATE EDICT PQ ENTRY                                                
*                                                                               
*                                                                               
*        BUILD EDICT HEADER LINE                                                
*                                                                               
         MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
*                                                                               
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
*                                                                               
         MVI   KRFTYPE,C' '        CLEAR FILE TYPE                              
*                                  G= GROCERY, S= SNACK                         
         ICM   R2,15,TWAMASTC      POINT TO MASTC                               
         USING MASTD,R2                                                         
         CLC   MCORIGID,=H'10273'    SEE IF MVCTOK (KRAFT GROCERIES)            
         BNE   LSTIV5                                                           
         MVC   15(07,R1),=C'MVWC007' FOR GROCERY INVOICE FILE                   
         MVI   KRFTYPE,C'G'                                                     
         B     LSTIV15                                                          
*                                                                               
LSTIV5   CLC   MCORIGID,=H'16377'    SEE IF MVCMON (KRAFT SNACKS)               
         BNE   LSTIV10                                                          
         MVC   15(07,R1),=C'MVWC001' FOR SNACK INVOICE FILE                     
         MVI   KRFTYPE,C'S'                                                     
         B     LSTIV15                                                          
         DROP  R2                                                               
*                                                                               
LSTIV10  DS    0H                                                               
         MVC   15(2,R1),AGYID      AGENCY ID                                    
         MVC   17(3,R1),=C'INV'    FILE TYPE                                    
         MVC   20(1,R1),=C'P'      MEDIA                                        
         MVC   21(2,R1),TWAAGY     AGENCY ALPHA                                 
*                                                                               
LSTIV15  MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   37(R1),C'D'         THIS IS A DATASET                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS PKR  TRN'                                      
         MVC   P+9(2),TWAAGY                                                    
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES            ++DDS DSN CARD                               
         MVC   P(5),=CL5'++DDS'                                                 
         MVC   P+11(3),=C'DSN'                                                  
         MVC   P+15(44),SPFKRINV   GENERATION NAME                              
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(8),=C'SUB CLA('                                             
*                                                                               
         MVC   P+19(2),AGYID       AGENCY ID                                    
         MVC   P+21(3),=C'INV'     FILE TYPE                                    
         MVC   P+24(1),=C'P'       MEDIA                                        
*                                                                               
         MVC   P+27(39),=C'),CHA(3),ACC(GXS),USE(U9973315),MOD(1)'              
*                                                                               
         CLI   KRFTYPE,C'G'          SEE IF GROCERY FILE                        
         BNE   *+10                                                             
         MVC   P+49(08),=C'U9973305'                                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+1(40),=C'*TAPE GENERATED, SENDING DATA VIA EDICT*'             
         MVI   P+43,X'5E'         MOVE IN SEMI COLON FOR EDICT SCAN             
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
LSTIVX   DS    0H                                                               
*                                                                               
         CLI   SPEHEAD,C'H'       SKIP IF HEADER STILL TO BE WRITTEN            
         BE    LSTESX                NO DETAILS HAVE BEEN WRITTEN               
*                                                                               
         MVI   ESCREC,C' '         INIT ESTIMATE CONTROL RECORD                 
         MVC   ESCREC+1(235),ESCREC                                             
*                                                                               
         MVC   ESCBTID,SPESBTID    SET BATCH ID                                 
         MVC   ESCBLAGY,SPEBLAGY   SET BILLING AGENCY                           
*                                                                               
         EDIT  (P8,SPESNET),ESCNET,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   ESCNET+L'ESCNET-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESCNET+L'ESCNET-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (P8,SPESGRS),ESCGRS,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   ESCGRS+L'ESCGRS-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESCGRS+L'ESCGRS-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         OI    SPEBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  ESCRECCT,SPEBTCTR                                                
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCBS                    
         LA    R6,ESCREC           POINT TO OUTPUT BATCH CONTROL REC            
*                                                                               
         PUT   (R3),(R6)           PUT TO FILE                                  
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
         MVI   ESTREC,C' '         INIT TRAILER RECORD                          
         MVC   ESTREC+1(235),ESTREC                                             
*                                                                               
         MVC   ESTRECID,=C'AE '                                                 
         MVC   ESTSTRT,=CL17'TTRL'                                              
*                                                                               
         OI    SPEBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  ESTRECCT,SPEBTCTR                                                
*                                                                               
         LA    R6,ESTREC           PUT TRAILER RECORD TO TAPE                   
*                                                                               
         PUT   (R3),(R6)                                                        
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
         L     R3,AKRESTFL                                                      
         CLOSE ((R3))              CLOSE ESTIMATE FILE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   GFTTAP,C'T'         SKIP IF ONLY PRODUCING TAPE                  
         BE    LSTESX                                                           
*                                                                               
*        GENERATE EDICT PQ ENTRY                                                
*                                                                               
*        FORCE NEW REPORT                                                       
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         CLI   REMOTLPP,68         FORCE SOME CHANGE IN REMOTE AREA             
         BNE   *+12                 THIS SHOULD FORCE THE OPENING OF            
         MVI   REMOTLPP,66          A NEW REPORT                                
         B     *+8                                                              
         MVI   REMOTLPP,68                                                      
*                                                                               
*        BUILD EDICT HEADER LINE                                                
*                                                                               
         MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
*                                                                               
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
*                                                                               
         ICM   R2,15,TWAMASTC      POINT TO MASTC                               
         USING MASTD,R2                                                         
         CLC   MCORIGID,=H'10273'    SEE IF MVCTOK (KRAFT GROCERIES)            
         BNE   LSTES5                                                           
         MVC   15(07,R1),=C'MVWC008' FOR GROCERY ESTIMATE FILE                  
         B     LSTES15                                                          
*                                                                               
LSTES5   CLC   MCORIGID,=H'16377'    SEE IF MVCMON (KRAFT SNACKS)               
         BNE   LSTES10                                                          
         MVC   15(07,R1),=C'MVWC002' FOR SNACK ESTIMATE FILE                    
         B     LSTES15                                                          
         DROP  R2                                                               
*                                                                               
LSTES10  DS    0H                                                               
         MVC   15(2,R1),AGYID      AGENCY ID                                    
         MVC   17(3,R1),=C'EST'    FILE TYPE                                    
         MVC   20(1,R1),=C'P'      MEDIA                                        
         MVC   21(2,R1),TWAAGY     AGENCY ALPHA                                 
*                                                                               
LSTES15  MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   37(R1),C'D'         THIS IS A DATASET                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS PKR  TRN'                                      
         MVC   P+9(2),TWAAGY                                                    
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES            ++DDS DSN CARD                               
         MVC   P(5),=CL5'++DDS'                                                 
         MVC   P+11(3),=C'DSN'                                                  
         MVC   P+15(44),SPFKREST   GENERATION NAME                              
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(8),=C'SUB CLA('                                             
*                                                                               
         MVC   P+19(2),AGYID       AGENCY ID                                    
         MVC   P+21(3),=C'EST'     FILE TYPE                                    
         MVC   P+24(1),=C'P'       MEDIA                                        
*                                                                               
         MVC   P+27(39),=C'),CHA(3),ACC(GXS),USE(U9973315),MOD(1)'              
*                                                                               
         CLI   KRFTYPE,C'G'          SEE IF GROCERY FILE                        
         BNE   *+10                                                             
         MVC   P+49(08),=C'U9973305'                                            
*                                                                               
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+1(40),=C'*TAPE GENERATED, SENDING DATA VIA EDICT*'             
         MVI   P+43,X'5E'         MOVE IN SEMI COLON FOR EDICT SCAN             
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
LSTESX   DS    0H                                                               
*                                                                               
LSTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
KRFTYPE  DS    CL1                                                              
*                                                                               
AGYID    DC    CL2' '              AGENCY ID                                    
*                                                                               
*        AGENCY ID TABLE                                                        
*                                                                               
*        DS    C'2 CH ALPHA',CL2'AGYID'                                         
*                                                                               
AIDTAB   DS    0D                                                               
         DC    C'H9',C'MV'         MEDIAVEST                                    
AIDTABLQ EQU   *-AIDTAB            LENGTH OF TABLE ENTRY                        
         DC    C'O0',C'MV'         MEDIAVEST TORONTO                            
         DC    C'YN',C'BR'         BRAVO                                        
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - GETPRD'                              
***********************************************************************         
*                                                                     *         
* GET GF PRODUCT CODES                                                *         
*                         THIS ROUTINE BUILDS (IN PRDTAB)             *         
*                         A TABLE OF PRODUCT CODES AND THE            *         
*                         INFO FROM THE GF ESTIMATE RECS              *         
*                                                                     *         
*        FIND ENTRY FOR PRODUCT IN TABLE OR BUILD ONE                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETPRD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         USING CONHEADH-64,R7                                                   
*                                                                               
         NI    ERRCD,255-ERRPRD    INIT ERROR CODE                              
         XC    APRTENT,APRTENT     INIT A(PRODUCT ENTRY)                        
*                                                                               
*        SEARCH TABLE FOR CURRENT PRODUCT                                       
*                                                                               
         L     R6,APRDTAB          POINT TO PRODUCT TABLE                       
         USING PRDTABD,R6          ESTABLISH ENTRY IN TABLE                     
*                                                                               
GETPRDLP DS    0H                                                               
*                                                                               
         CLI   PRTPRD,0            DONE AT END OF TABLE                         
         BE    GETPRDDN                                                         
*                                                                               
         CLC   PRTPRD,PBPROD       MATCH ON PRODUCT                             
         BE    GETPRDFD                                                         
*                                                                               
GETPRDCN DS    0H                                                               
*                                                                               
         LA    R6,PRTENTL(R6)       BUMP TO NEXT ENTRY IN TABLE                 
         B     GETPRDLP                                                         
*                                                                               
GETPRDDN DS    0H                                                               
*                                                                               
*        PRODUCT NOT IN TABLE - READ GFEST RECORD FOR PRODUCT                   
*                                                                               
         XC    KEY,KEY             READ GF ESTIMATE RECORD                      
         LA    R5,KEY                 WITH ESTIMATE=0                           
         USING GFESTRD,R5                                                       
*                                                                               
         MVC   PGSTKAGY,AGENCY     SET AGENCY                                   
         MVC   PGSTKMED,PBMED      SET MEDIA                                    
         MVI   PGSTKRCD,X'0B'      SET RECID                                    
         MVC   PGSTKCLT,PBCLT      SET CLIENT                                   
         MVC   PGSTKPRD,PBPROD     SET PRODUCT                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PGSTKEST-PGSTKEY),KEYSAVE   SKIP IF NO RECORD                
         BE    GTP10                                                            
*                                                                               
         MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
         B     GETPRDX                                                          
*                                                                               
GTP10    DS    0H                                                               
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT AIO                             
         L     R5,AIO3             POINT TO FOUND RECORD                        
         ST    R5,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT AIO                          
*                                                                               
         MVC   PRTPRD,PBPROD       SET PRODUCT CODE IN TABLE ENTRY              
         LA    R5,PGSTKEDQ(R5)     POINT TO FIRST ENTRY IN RECORD               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
*                                                                               
GTPFLDLP DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE AT END OF RECORD                        
         BE    GTPFLDDN                                                         
*                                                                               
         CLI   0(R5),PGSTEIDQ      MUST BE PGEST ELEMENT                        
         BNE   GTPFLDCN                                                         
*                                                                               
         USING PGSTELEM,R5                                                      
*                                                                               
         LA    R1,QNAMETB          POINT TO FIRST FIELD NAME                    
         LA    RE,QNAMENOQ         NUMBER OF ENTRIES IN FIELD NAME TAB          
         LA    R4,PRTFIRST         POINT TO FIRST PRDTAB DATA FIELD             
*                                                                               
GTPNAMLP DS    0H                                                               
*                                                                               
         CLC   PGSTNAME,0(R1)      MATCH ON FIELD NAME                          
         BE    GTPNAMFD                                                         
*                                                                               
GTPNAMCN DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN PRDTAB                     
         LA    R4,0(RF,R4)         NEXT ITEM IN PRDTAB                          
         LA    R1,9(R1)            NEXT NAME                                    
         BCT   RE,GTPNAMLP                                                      
*                                                                               
GTPNAMDN DS    0H                  DATA NOT FOUND                               
         B     GTPFLDCN            IGNORE                                       
*                                                                               
GTPNAMFD DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN FIELD                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PGSTDATA    MOVE DATA TO TABLE                           
*                                                                               
GTPFLDCN DS    0H                                                               
*                                                                               
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GTPFLDLP                                                         
*                                                                               
GTPFLDDN DS    0H                                                               
*                                                                               
         XC    PRTENTL(PRTENTL,R6),PRTENTL(R6) INIT NEXT ENTRY                  
*                                                                               
         MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETPRDFD DS    0H                  PROD FOUND IN TABLE                          
*                                                                               
         ST    R6,APRTENT          SAVE TABLE ENTRY POINTER                     
*                                                                               
GETPRDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - GETEST'                              
***********************************************************************         
*                                                                     *         
* GET GF ESTIMATE CODES                                               *         
*                         THIS ROUTINE BUILDS (IN ESTTAB)             *         
*                         A TABLE OF ESTIMATE CODES AND THE           *         
*                         INFO FROM THE GF ESTIMATE RECS              *         
*                                                                     *         
*        FIND ENTRY FOR ESTIMATE IN TABLE OR BUILD ONE                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         USING CONHEADH-64,R7                                                   
*                                                                               
         NI    ERRCD,255-ERREST    INIT ERROR CODE                              
         XC    AETBENT,AETBENT     INIT A(ESTIMATE ENTRY)                       
*                                                                               
*        SEARCH TABLE FOR CURRENT ESTIMATE                                      
*                                                                               
         L     R6,AESTTAB          POINT TO ESTIMATE TABLE                      
         USING ESTTABD,R6          ESTABLISH ENTRY IN TABLE                     
*                                                                               
GETESTLP DS    0H                                                               
*                                                                               
         CLI   ETBEST,0            DONE AT END OF TABLE                         
         BE    GETESTDN                                                         
*                                                                               
         CLC   ETBEST,PBEST        MATCH ON ESTIMATE                            
         BE    GETESTFD                                                         
*                                                                               
GETESTCN DS    0H                                                               
*                                                                               
         LA    R6,ETBENTL(R6)       BUMP TO NEXT ENTRY IN TABLE                 
         B     GETESTLP                                                         
*                                                                               
GETESTDN DS    0H                                                               
*                                                                               
*        ESTIMATE NOT IN TABLE - READ GFEST RECORD FOR ESTIMATE                 
*                                                                               
         XC    KEY,KEY             READ GF ESTIMATE RECORD                      
         LA    R5,KEY                 WITH ESTIMATE=0                           
         USING GFESTRD,R5                                                       
*                                                                               
         MVC   PGSTKAGY,AGENCY     SET AGENCY                                   
         MVC   PGSTKMED,PBMED      SET MEDIA                                    
         MVI   PGSTKRCD,X'0B'      SET RECID                                    
         MVC   PGSTKCLT,PBCLT      SET CLIENT                                   
         MVC   PGSTKPRD,=C'ZZZ'    SET PRODUCT TO ZZZ                           
         MVC   PGSTKEST,PBEST      SET ESTIMATE                                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PGSTKEST-PGSTKEY+L'PGSTKEST),KEYSAVE SKIP IF NONE            
         BNE   GETESTER                                                         
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT AIO                             
         L     R5,AIO3             POINT TO FOUND RECORD                        
         ST    R5,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT AIO                          
*                                                                               
         MVC   ETBEST,PBEST        SET ESTIMATE CODE IN TABLE ENTRY             
         LA    R5,PGSTKEDQ(R5)     POINT TO FIRST ENTRY IN RECORD               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
*                                                                               
GTEFLDLP DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE AT END OF RECORD                        
         BE    GTEFLDDN                                                         
*                                                                               
         CLI   0(R5),PGSTEIDQ      MUST BE PGEST ELEMENT                        
         BNE   GTEFLDCN                                                         
*                                                                               
         USING PGSTELEM,R5                                                      
*                                                                               
         LA    R1,QNAMETB          POINT TO FIRST FIELD NAME                    
         LA    RE,QNAMENOQ         NUMBER OF ENTRIES IN FIELD NAME TAB          
         LA    R4,ETBFIRST         POINT TO FIRST ESTTAB DATA FIELD             
*                                                                               
GTENAMLP DS    0H                                                               
*                                                                               
         CLC   PGSTNAME,0(R1)      MATCH ON FIELD NAME                          
         BE    GTENAMFD                                                         
*                                                                               
GTENAMCN DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN ESTTAB                     
         LA    R4,0(RF,R4)         NEXT ITEM IN ESTTAB                          
         LA    R1,9(R1)            NEXT NAME                                    
         BCT   RE,GTENAMLP                                                      
*                                                                               
GTENAMDN DS    0H                  DATA NOT FOUND                               
         B     GTEFLDCN            IGNORE                                       
*                                                                               
GTENAMFD DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN FIELD                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PGSTDATA    MOVE DATA TO TABLE                           
*                                                                               
GTEFLDCN DS    0H                                                               
*                                                                               
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GTEFLDLP                                                         
*                                                                               
GTEFLDDN DS    0H                                                               
*                                                                               
         XC    ETBENTL(ETBENTL,R6),ETBENTL(R6) INIT NEXT ENTRY                  
*                                                                               
         MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETESTFD DS    0H                  PROD FOUND IN TABLE                          
*                                                                               
         ST    R6,AETBENT          SAVE TABLE ENTRY POINTER                     
*                                                                               
         B     GETESTX                                                          
*                                                                               
GETESTER DS    0H                                                               
         OI    ERRCD,ERREST        INDICATE ESTIMATE ERROR                      
*                                                                               
         MVC   KEY,IOKEYSVE                                                     
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETESTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - RESOLVER'                            
***********************************************************************         
*                                                                     *         
*        RESOLVE ROUTINE ADDRESSES                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RESOLVER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'         CHECK FOR EOT                                
         BE    RESOLVEX                                                         
*                                                                               
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
*                                                                               
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
*                                                                               
RESOLVEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - RTNLIST'                             
***********************************************************************         
*                                                                     *         
*        ROUTINE ADDRESSES                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RTNLIST  DS    0F                                                               
*                                                                               
*        NEW VERSION OF TAPE                                                    
*                                                                               
         DC    CL8'IRECID  ',A(IRECID)                                          
         DC    CL8'ORECID  ',A(ORECID)                                          
*                                                                               
         DC    CL8'IBLAGY  ',A(IBLAGY)                                          
         DC    CL8'OBLAGY  ',A(OBLAGY)                                          
*                                                                               
         DC    CL8'IBTID   ',A(IBTID)                                           
         DC    CL8'OBTID   ',A(OBTID)                                           
*                                                                               
         DC    CL8'OMED    ',A(OMED)                                            
*                                                                               
         DC    CL8'OCLT    ',A(OCLT)                                            
*                                                                               
         DC    CL8'OPRD    ',A(OPRD)                                            
*                                                                               
         DC    CL8'OEST    ',A(OEST)                                            
*                                                                               
         DC    CL8'OINV    ',A(OINV)                                            
*                                                                               
         DC    CL8'OINVDT  ',A(OINVDT)                                          
*                                                                               
         DC    CL8'ISRCAG  ',A(ISRCAG)                                          
         DC    CL8'OSRCAG  ',A(OSRCAG)                                          
*                                                                               
         DC    CL8'IREVRS  ',A(IREVRS)                                          
         DC    CL8'OREVRS  ',A(OREVRS)                                          
*                                                                               
         DC    CL8'IBLSTA  ',A(IBLSTA)                                          
         DC    CL8'OBLSTA  ',A(OBLSTA)                                          
*                                                                               
         DC    CL8'IREQNM  ',A(IREQNM)                                          
         DC    CL8'OREQNM  ',A(OREQNM)                                          
*                                                                               
         DC    CL8'ITGTMK  ',A(ITGTMK)                                          
         DC    CL8'OTGTMK  ',A(OTGTMK)                                          
*                                                                               
         DC    CL8'ILINE#  ',A(ILINE#)                                          
         DC    CL8'OLINE#  ',A(OLINE#)                                          
*                                                                               
         DC    CL8'IDESC   ',A(IDESC)                                           
         DC    CL8'ODESC   ',A(ODESC)                                           
*                                                                               
         DC    CL8'IEXPTP  ',A(IEXPTP)                                          
         DC    CL8'OEXPTP  ',A(OEXPTP)                                          
*                                                                               
         DC    CL8'IPRDID  ',A(IPRDID)                                          
         DC    CL8'OPRDID  ',A(OPRDID)                                          
*                                                                               
         DC    CL8'IESTID  ',A(IESTID)                                          
         DC    CL8'OESTID  ',A(OESTID)                                          
*                                                                               
         DC    CL8'ICHGDT  ',A(ICHGDT)                                          
         DC    CL8'OCHGDT  ',A(OCHGDT)                                          
*                                                                               
         DC    CL8'ONET    ',A(ONET)                                            
*                                                                               
         DC    CL8'IBKNET  ',A(IBKNET)                                          
*                                                                               
         DC    CL8'OCOM    ',A(OCOM)                                            
*                                                                               
         DC    CL8'IBKCOM  ',A(IBKCOM)                                          
         DC    CL8'OBKCOM  ',A(OBKCOM)                                          
*                                                                               
         DC    CL8'OADJ    ',A(OADJ)                                            
*                                                                               
         DC    CL8'IBKADJ  ',A(IBKADJ)                                          
         DC    CL8'OBKADJ  ',A(OBKADJ)                                          
*                                                                               
         DC    CL8'OGRS    ',A(OGRS)                                            
*                                                                               
         DC    CL8'IBKGRS  ',A(IBKGRS)                                          
*                                                                               
         DC    CL8'IBKBLL  ',A(IBKBLL)                                          
         DC    CL8'OBKBLL  ',A(OBKBLL)                                          
*                                                                               
         DC    CL8'ICRAGY  ',A(ICRAGY)                                          
         DC    CL8'OCRAGY  ',A(OCRAGY)                                          
*                                                                               
         DC    CL8'IDEAL#  ',A(IDEAL#)                                          
         DC    CL8'ODEAL#  ',A(ODEAL#)                                          
*                                                                               
         DC    CL8'OFILLER ',A(OFILLER)                                         
*                                                                               
         DC    CL8'OHST    ',A(OHST)                                            
*                                                                               
         DC    CL8'OGST    ',A(OGST)                                            
*                                                                               
         DC    CL8'OPST    ',A(OPST)                                            
*                                                                               
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
*                                                                               
         DC    CL8'LASTCOL ',A(LASTCOL)                                         
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INIESTID'                            
***********************************************************************         
*                                                                     *         
*        INITIALIZE TABLE OF ESTIDS                                   *         
*                                                                     *         
*        INITIALIZE BXLE REGIOSTERS AND SAVE                          *         
*        CLEAR FIRST ENTRY IN TABLE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIESTID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,=A(ESTIDTB)      POINT TO START OF TABLE                      
         LA    R4,L'IVDESTID       LENGTH OF ENTRY IN TABLE                     
         LR    R5,R3               SET TABLE AS EMPTY                           
         AHI   R5,-1                                                            
*                                                                               
         STM   R3,R5,EITBBXLE      SAVE STARTING BXLE REGISTERS                 
*                                                                               
         XC    0(L'IVDESTID,R3),0(R3) INIT FIRST ENTRY                          
*                                                                               
INESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ADDESTID'                            
***********************************************************************         
*                                                                     *         
*        ADD ESTID TO A TABLE                                         *         
*                                                                     *         
*NTRY    R2==> ESTID FOR TABLE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ADDESTID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R3,R5,EITBBXLE      LOAD BXLE REGISTERS FOR ESTID TABLE          
*                                                                               
         CLC   0(L'IVDESTID,R3),0(R2) CHECK IF ALREADY IN TABLE                 
         BE    ADESX               YES                                          
         BXLE  R3,R4,*-10                                                       
*                                                                               
         MVC   0(L'IVDESTID,R3),0(R2)  ADD INCOMING TO TABLE                    
*                                                                               
         AR    R3,R4               BUMP TO NEXT AVAILABLE SLOT                  
         XC    0(L'IVDESTID,R3),0(R3) INIT NEXT ENTRY                           
*                                                                               
         AHI   R3,-1               ADJUST END OF TABLE                          
         ST    R3,EITBBXLE+8                                                    
*                                                                               
ADESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - GETESTID'                            
***********************************************************************         
*                                                                     *         
*        FIND ESTID IN TABLE                                          *         
*                                                                     *         
*NTRY    R2==> ESTID FOR TABLE                                        *         
*                                                                     *         
*        SINCE ESTID'S ARE SORTED WE CAN STOP SEARCH WHEN WE          *         
*        ARE PAST APPROPRIATE ENTRY IN TABLE                          *         
*                                                                     *         
*EXIT    NE CC MEANS NOT FOUND                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETESTID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R3,R5,EITBBXLE      LOAD BXLE REGISTERS FOR ESTID TABLE          
*                                                                               
         CR    R5,R3               CHECK IF TABLE IS EMPTY                      
         BL    GTESX               YES - GET OUT                                
*                                                                               
         CLC   0(L'IVDESTID,R2),0(R3)  MATCH TO TABLE ENTRY                     
         BE    GTESX               FOUND        - GET OUT                       
         BXLE  R3,R4,*-10                                                       
*                                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
GTESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - RECID'                               
***********************************************************************         
*                                                                     *         
*        RECORD ID - C'1' - INVOICE  RECORD                           *         
*                    C'2' - ESTIMATE RECORD                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IRECID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'1'          ASSUME PROCESSING BILLS                      
*                                                                               
         CLI   PBMODE,PBPROCBK     IF PROCESSING ESTIMATE BUCKETS               
         BNE   *+8                                                              
         MVI   0(R2),C'2'             USE DIFFERENT RECORD ID                   
*                                                                               
IRECIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ORECID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVRECID,0(R2)          SAVE RECORD ID                            
*                                                                               
ORECIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BLAGY'                               
***********************************************************************         
*                                                                     *         
*        BILLING AGENCY                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBLAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTBLAGY,R2),SPACES  INIT                                    
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IBLAGY10                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBBLAGY,R2),ETBBLAGY RETURN BILLING AGENCY CODE             
*                                                                               
         CLC   ETBBLAGY,SPACES     DONE IF FOUND                                
         BH    IBLAGY20                                                         
*                                                                               
IBLAGY10 DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IBLAGY20                                                         
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTBLAGY,R2),PRTBLAGY RETURN BILLING AGENCY CODE             
*                                                                               
IBLAGY20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTBLAGY,R2),SPACES  MAKE PRINTABLE                          
*                                                                               
         CLC   0(L'PRTBLAGY,R2),SPACES  IF BILLING AGENCY NOT FOUND             
         BH    *+12                                                             
         OI    ERRCD,ERREST          FLAG AS ERROR                              
         B     IBLAGYX                                                          
*                                                                               
         CLC   =C'EXCLUDE',0(R2)   DROP IF TO BE EXCLUDED                       
         BNE   *+8                                                              
         MVI   REJECT,C'Y'                                                      
*                                                                               
IBLAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBLAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OBLAGYTL                                                         
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OBLAGYIN                                                         
*                                                                               
         MVC   SPIBLAGY,0(R2)         SAVE FOR CONTROL RECORD                   
         OC    SPIBLAGY,SPACES        SPACE FILL                                
*                                                                               
         MVC   IVHBLAGY,SPIBLAGY      PUT IN INVOICE REOCRD                     
*                                                                               
         B     OBLAGYRX                                                         
*                                  ELSE                                         
OBLAGYIN DS    0H                                                               
*                                                                               
         MVC   SPEBLAGY,0(R2)         SAVE FOR CONTROL RECORD                   
         OC    SPEBLAGY,SPACES        SPACE FILL                                
*                                                                               
         MVC   ESHBLAGY,SPEBLAGY      PUT IN ESTIMATE RECORD                    
*                                                                               
OBLAGYRX DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(9,R3),=C'BILL AGY:'    PUT OUT TITLE                           
         MVC   10(8,R3),IVHBLAGY        PUT OUT BILLING AGENCY                  
*                                                                               
         B     OBLAGYX                                                          
*                                                                               
OBLAGYTL DS    0H                 TOTAL LINE                                    
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OBLAGTIN                                                         
*                                                                               
         MVC   IVCBLAGY,0(R2)         PUT IN INVOICE CONTROL RECORD             
         OC    IVCBLAGY,SPACES        SPACE FILL                                
*                                                                               
         B     OBLAGTIX                                                         
*                                                                               
OBLAGTIN DS    0H                 ELSE                                          
*                                                                               
         MVC   ESCBLAGY,0(R2)         PUT IN ESTIMATE CONTROL RECORD            
         OC    ESCBLAGY,SPACES        SPACE FILL                                
*                                                                               
OBLAGTIX DS    0H                                                               
*                                                                               
OBLAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BTID'                                
***********************************************************************         
*                                                                     *         
*        BATCH ID  -  CL10'BILLING AGENCY CODE'                       *         
*                     CL10'CREATION DATE'                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBTID    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PBMODE,PBPROCBL     IF PROCESSING BILL RECORDS                   
         BNE   IBTIDIVN                                                         
*                                                                               
         MVC   0(L'SPIVBTID,R2),SPIVBTID SET BATCH ID                           
*                                                                               
         B     IBTIDX                                                           
*                                                                               
IBTIDIVN DS    0H                 ELSE                                          
*                                                                               
         MVC   0(L'SPESBTID,R2),SPESBTID SET BATCH ID                           
*                                                                               
IBTIDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBTID    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OBTIDTL                                                          
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OBTIDIVN                                                         
*                                                                               
         MVC   IVHBTID,0(R2)          PUT IN INVOICE RECORD                     
         OC    IVHBTID,SPACES         SPACE FILL                                
*                                                                               
         B     OBTIDRX                                                          
*                                  ELSE                                         
OBTIDIVN DS    0H                                                               
*                                                                               
         MVC   ESHBTID,0(R2)          PUT IN ESTIMATE RECORD                    
         OC    ESHBTID,SPACES         SPACE FILL                                
*                                                                               
OBTIDRX  DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(9,R3),=C'BATCH ID:'    PRINT TITLE                             
         MVC   18(20,R3),0(R2)         PRINT BATCH ID                           
*                                                                               
         B     OBTIDX                                                           
*                                                                               
OBTIDTL  DS    0H                  TOTAL LINE                                   
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OBTIDTIN                                                         
*                                                                               
         MVC   IVCBTID,0(R2)          PUT IN INVOICE CONTROL RECORD             
         OC    IVCBTID,SPACES         SPACE FILL                                
*                                                                               
         B     OBTIDTLX                                                         
*                                                                               
OBTIDTIN DS    0H                 ELSE                                          
*                                                                               
         MVC   ESCBTID,0(R2)          PUT IN ESTIMATE CONTROL RECORD            
         OC    ESCBTID,SPACES         SPACE FILL                                
*                                                                               
OBTIDTLX DS    0H                                                               
*                                                                               
OBTIDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - MED'                                 
***********************************************************************         
*                                                                     *         
*        MEDIA                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OMED     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVMED,0(R2)         SAVE MEDIA CODE                              
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(9,R3),=CL9'MEDIA   :'   PRINT HEADLINE TITLE                   
         MVC   12(1,R3),0(R2)            PRINT MEDIA                            
         OC    0(16,R3),SPACES           FORCE TO UPPERCASE                     
*                                                                               
OMEDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - CLT'                                 
***********************************************************************         
*                                                                     *         
*        CLIENT                                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OCLT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVCLT,20(R2)        SAVE CLIENT CODE                             
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(9,R3),=CL9'CLIENT  :'   PRINT HEADLINE TITLE                   
         MVC   10(3,R3),19(R2)           PRINT CLIENT                           
         OC    0(16,R3),SPACES           FORCE TO UPPERCASE                     
*                                                                               
OCLTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PRD'                                 
***********************************************************************         
*                                                                     *         
*        PRODUCT                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPRD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVPRD,0(R2)         SAVE PRODUCT CODE                            
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(9,R3),=CL9'PRODUCT :'   PRINT HEADLINE TITLE                   
         MVC   10(3,R3),0(R2)            PRINT PRODUCT                          
*                                                                               
OPRDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - EST'                                 
***********************************************************************         
*                                                                     *         
*        ESTIMATE                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OEST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVEST,0(R2)         SAVE ESTIMATE CODE                           
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    OESTTX                                                           
*                                                                               
         MVC   0(9,R3),=CL9'ESTIMATE:'   PRINT HEADLINE TITLE                   
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         CVD   RF,DUB                                                           
         UNPK  10(3,R3),DUB              PRINT ESTIMATE                         
*                                                                               
OESTTX   DS    0H                                                               
*                                                                               
OESTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRITER - BUILD INVOICE NUMBER - OINV'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD INVOICE NUMBER                              *         
*                                                                     *         
*NTRY R2  -  INVOICE DATE YYMMDD - BINARY                             *         
*            INVOICE NUMBER  2 BYTES BINARY                           *         
*            MEDIA CODE                                               *         
*            CLIENT AREA                                              *         
*                                                                     *         
*EXIT    7 BYTE INVOICE NUMBER                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
OINV     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         OC    0(5,R2),0(R2)       MAKE SURE WE HAVE INPUT                      
         BZ    BLDINVX                                                          
*                                                                               
         LA    R4,5(R2)            POINT TO INPUT MEDIA/CLIENT                  
*                                                                               
*        READ B1 PROFILE                                                        
*                                                                               
         XC    WORK,WORK           PARAMETERS FOR GETPROF                       
*                                                                               
         MVC   WORK(4),=C'PB1 '    B1X PROFILE                                  
         MVC   WORK+4(2),PBQAGY    AGENCY                                       
         MVC   WORK+6(1),0(R4)     MEDIA                                        
         MVC   WORK+7(3),1(R4)     CLIENT                                       
*                                                                               
         CLI   PBQCLT,C'*'         OFFICE REQUESTED                             
         BNE   *+10                                                             
         MVC   WORK+10(2),PBQCLT                                                
*                                                                               
         CLC   BIVKEY,WORK         SKIP IF PROFILE ALREADY IN CORE              
         BE    BIVRDX                                                           
*                                                                               
         MVC   BIVKEY,WORK         SAVE PROFILE                                 
*                                                                               
         XC    BIVPRO,BIVPRO       SPECIAL BILLING PROFILE                      
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,BIVPRO,DATAMGR                                 
*                                                                               
BIVRDX   DS    0H                                                               
*                                                                               
*        READ B1X PROFILE                                                       
*                                                                               
         XC    WORK,WORK           PARAMETERS FOR GETPROF                       
*                                                                               
         MVC   WORK(4),=C'PB1X'    B1X PROFILE                                  
         NI    WORK,X'FF'-X'40'    LOWER CASE                                   
         MVC   WORK+4(2),PBQAGY    AGENCY                                       
         MVC   WORK+6(1),0(R4)     MEDIA                                        
         MVC   WORK+7(3),1(R4)     CLIENT                                       
*                                                                               
         CLI   PBQCLT,C'*'         OFFICE REQUESTED                             
         BNE   *+10                                                             
         MVC   WORK+10(2),PBQCLT                                                
*                                                                               
         CLC   BIVXKEY,WORK         SKIP IF PROFILE ALREADY IN CORE             
         BE    BIVREADX                                                         
*                                                                               
         MVC   BIVXKEY,WORK         SAVE PROFILE                                
*                                                                               
         XC    BIVPROX,BIVPROX      SPECIAL BILLING PROFILE                     
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,BIVPROX,DATAMGR                                
*                                                                               
*        BIVPROX+4   NOW HAS 'BASE' YEAR FOR INV MONTH DISPLAY                  
*        BIVPROX+5   ADD THIS MUNBER TO INVOICE MONTH                           
*                                                                               
BIVREADX DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),DUB  MAKE YYMMDD                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(20,WORK)  YYYYMMDD CHAR                   
*                                                                               
         GOTO1 =V(PPFMTINO),DMCB,DUB,(2,3(R2)),(0(R4),BIVPRO),         X        
               BIVPROX                                                          
*                                                                               
         L     R1,DMCB+4           POINT TO INVOICE NUMBER                      
*                                                                               
         MVI   IVHINVNO,C'P'       SET MEDIA                                    
         MVC   IVHINVNO+1(1),0(R4) SET SUB-MEDIA                                
         MVI   IVHINVNO+2,C'-'     DASH                                         
         MVC   IVHINVNO+3(4),WORK  YEAR                                         
         MVI   IVHINVNO+7,C'-'     DASH                                         
         MVC   IVHINVNO+8(7),0(R1)   SET INVOICE NUMBER                         
         MVC   0(15,R3),IVHINVNO    PRINT INVOICE NUMBER                        
*                                                                               
BLDINVX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
BIVPRO   DS    XL16                BILLING 1 PROFILE                            
BIVKEY   DS    CL12                BILLING 1 PROFILE KEY (AGY/MEDIA)            
BIVPROX  DS    XL16                BILLING 1X PROFILE                           
BIVXKEY  DS    CL12                BILLING 1X PROFILE KEY (AGY/MEDIA)           
*                                                                               
         TITLE 'PRWRITER - BUILD INVOICE NUMBER - OINVDT'                       
***********************************************************************         
*                                                                     *         
*        INVOICE DATE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OINVDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(23,IVHINVDT)  INVOICE DATE                
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVHINVDT,R3),IVHINVDT  PRINT INVOICE DATE                    
*                                                                               
OINVDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - SRCAG'                               
***********************************************************************         
*                                                                     *         
*        SOURCE AGENCY                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ISRCAG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTSRCAG,R2),SPACES   INIT                                   
*                                                                               
         CLI   PBMODE,PBPROCBL    REJECT IF NOT DOING BILLS                     
         BE    *+12                                                             
         MVI   REJECT,C'Y'                                                      
         B     ISRCAGX                                                          
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ISRCAG10                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBSRCAG,R2),ETBSRCAG RETURN SOURCE AGENCY CODE              
*                                                                               
         CLC   ETBSRCAG,SPACES     DONE IF FOUND                                
         BH    ISRCAG20                                                         
*                                                                               
ISRCAG10 DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ISRCAG20                                                         
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTSRCAG,R2),PRTSRCAG RETURN SOURCE AGENCY CODE              
*                                                                               
ISRCAG20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTSRCAG,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
ISRCAGX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OSRCAG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   IVHSRCAG,0(R2)      PUT IN INVOICE RECORD                        
         OC    IVHSRCAG,SPACES     SPACE FILL                                   
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVHSRCAG,R3),IVHSRCAG PRINT SOURCE AGENCY                    
*                                                                               
OSRCAGX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - REVRS'                               
***********************************************************************         
*                                                                     *         
*        REVERSAL INDICATOR                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IREVRS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'N'          ALWAYS C'N'                                  
*                                                                               
IREVRSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OREVRS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   IVHREVRS,0(R2)      PUT IN INVOICE RECORD                        
         OC    IVHREVRS,SPACES     SPACE FILL                                   
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   1(L'IVHREVRS,R3),IVHREVRS PRINT                                  
*                                                                               
OREVRSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BLSTA'                               
***********************************************************************         
*                                                                     *         
*        BILLING STATUS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBLSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'M'          ALWAYS C'M'                                  
*                                                                               
IBLSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBLSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE REPORT                             
         BNE   OBLSTAIN                                                         
*                                                                               
         MVC   IVHBLSTA,0(R2)        PUT IN INVOICE RECORD                      
         OC    IVHBLSTA,SPACES       SPACE FILL                                 
*                                                                               
         B     OBLSTARX                                                         
*                                 ELSE                                          
OBLSTAIN DS    0H                                                               
*                                                                               
         MVC   ESHBLSTA,0(R2)        PUT IN ESTIMATE RECORD                     
         OC    ESHBLSTA,SPACES       SPACE FILL                                 
*                                                                               
OBLSTARX DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   1(L'IVHBLSTA,R3),0(R2)     PRINT BILL STATUS                     
         OC    1(L'IVHBLSTA,R3),SPACES    SPACE FILL                            
*                                                                               
OBLSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - REQNM'                               
***********************************************************************         
*                                                                     *         
*        REQUIREMENT NUMBER                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IREQNM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        INPUT ROUTINE NOT USED ANY LONGER                                      
*                                                                               
         MVC   0(L'PRTREQNM,R2),SPACES  INIT                                    
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IREQNM10                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBREQNM,R2),ETBREQNM RETURN SOURCE REQ NUMBER               
*                                                                               
         CLC   ETBREQNM,SPACES     DONE IF FOUND                                
         BH    IREQNM20                                                         
*                                                                               
IREQNM10 DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IREQNM20                                                         
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTREQNM,R2),PRTREQNM RETURN SOURCE AGENCY CODE              
*                                                                               
IREQNM20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTREQNM,R2),SPACES  MAKE PRINTABLE                          
*                                                                               
IREQNMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OREQNM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,=A(ESTDATA)      POINT TO ESTIMATE DATA                       
*                                                                               
         MVC   ESHREQNM(L'ESTUDEF2),ESTUDEF2-ESTDATA(R1)                        
         OC    ESHREQNM,SPACES     SPACE FILL                                   
*                                                                               
*****    MVC   ESHREQNM,0(R2)      PUT IN ESTIMATE RECORD                       
*****    OC    ESHREQNM,SPACES     SPACE FILL                                   
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'ESHREQNM,R3),ESHREQNM PRINT REQUIREMENT NUMBER               
*                                                                               
OREQNMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - TGTMK'                               
***********************************************************************         
*                                                                     *         
*        TARGET MARKET CODE                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ITGTMK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTTGTMK,R2),SPACES  INIT                                    
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ITGTMK10                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBTGTMK,R2),ETBTGTMK RETURN TARGET MARKET CODE              
*                                                                               
         CLC   ETBTGTMK,SPACES     DONE IF FOUND                                
         BH    ITGTMK20                                                         
*                                                                               
ITGTMK10 DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ITGTMKX                                                          
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTTGTMK,R2),PRTTGTMK RETURN TARGET MARKET CODE              
*                                                                               
ITGTMK20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTTGTMK,R2),SPACES  MAKE PRINTABLE                          
*                                                                               
         CLC   0(L'PRTTGTMK,R2),SPACES  IF NOT FOUND                            
         BH    *+8                                                              
         OI    ERRCD,ERREST                FLAG AS ERROR                        
*                                                                               
ITGTMKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OTGTMK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ESHTGTMK,0(R2)      PUT IN ESTIMATE RECORD                       
         OC    ESHTGTMK,SPACES     SPACE FILL                                   
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   1(L'ESHTGTMK,R3),ESHTGTMK PRINT TARGET MARKET CODE               
*                                                                               
OTGTMKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - LINE#'                               
***********************************************************************         
*                                                                     *         
*        LINE NUMBER - ALWAYS ZERO                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ILINE#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(10,R2),=10C'0'        ALWAYS ZERO                              
*                                                                               
ILINE#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OLINE#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   IVDLINE#,0(R2)      PUT IN INVOICE RECORD                        
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVDLINE#,R3),IVDLINE#    PRINT LINE NUMBER                   
*                                                                               
OLINE#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - DESC'                                
***********************************************************************         
*                                                                     *         
*        DESCRIPTION                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IDESC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(50,R2),=CL50' '        ALWAYS SPACES                           
*                                                                               
IDESCX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ODESC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVRECID,C'1'       IF DOING INVOICE RECORD                       
         BNE   ODESCIVN                                                         
*                                                                               
         MVC   IVDDESC,0(R2)          PUT IN INVOICE RECORD                     
         OC    IVDDESC,SPACES         SPACE FILL                                
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(L'IVDDESC,R3),0(R2)      PRINT DESCRIPTION                     
         OC    0(L'IVDDESC,R3),0(R2)      SPACE FILL                            
*                                                                               
         B     ODESCRX                                                          
*                                                                               
ODESCIVN DS    0H                                                               
*                                 ELSE                                          
*                                 ESTIMATE NAME                                 
*                                                                               
         OC    0(2,R2),0(R2)       SKIP IF NO ESTIMATE DATA AVAILABLE           
         BZ    ODESCX                                                           
*                                                                               
         CLI   09(R2),0            IF SE NUMBER PASSED                          
         BE    *+8                                                              
         ICM   RF,15,PBUTLA        AND UTL ADDRESS KNOWN                        
         BZ    *+16                                                             
         MVC   PAOSESAV,4(RF)         SAVE SE NUMBER                            
         MVC   4(1,RF),09(R2)         SET NEEDED SE NUMBER                      
*                                                                               
         MVC   ESTDATA,0(R2)       YES-GET ESTIMATE RECORD                      
         XC    ESTNAME,ESTNAME     INIT ESTIMATE DATA AREA                      
         XC    ESTNAME2,ESTNAME2                                  L11           
         XC    ESTUDEF2,ESTUDEF2                                  L11           
         MVI   ESTBL1,C' '                                                      
         MVI   ESTBL2,C' '                                                      
         XC    ESTDATES,ESTDATES                                                
         MVC   ESTNAME(11),=C'**UNKNOWN**'                                      
         MVC   ESTDATES(11),=C'**UNKNOWN**'                                     
         MVC   ESTFILT,=C'***'                                                  
*                                                                               
         XC    KEY,KEY             ESTABLISH KEYAAREA AS ESTIMATE KEY           
*========              ========*                                                
         LA    R5,KEY                                                           
         USING PESTREC,R5                                                       
*========              ========*                                                
         MVC   PESTKAGY,10(R2)     AGENCY                                       
*                                                                               
         MVC   PESTKMED,PBQMED     MEDIA                                        
*                                                                               
         CLI   PBQMED,C'C'         IF MULTI MEDIA USE PASSED MEDIA              
         BE    *+8                                                              
         CLI   PBQMED,C'*'                                                      
         BNE   *+10                                                             
         MVC   PESTKMED,8(R2)                                   BUG04           
*                                                                               
         MVI   PESTKRCD,X'07'      RECORD ID                                    
         MVC   PESTKCLT,2(R2)      CLIENT                                       
         MVC   PESTKPRD,SVESTID+3  PRODUCT                                      
         MVC   PESTKEST,0(R2)      ESTIMATE                                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
OESTAGAN CLC   KEY(PESTKEST+2-PESTKEY),KEYSAVE  OKAY IF KEY FOUND               
         BE    OEST6                                                            
*                                                                               
         CLI   SVESTID+3,0         PRODUCT NOT IN KEY-- FIND PRODUCT            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      MUST MATCH CLIENT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ENSURE EST IS LOWER THAN REQUEST                                              
*                                                                               
         CLC   PESTKEST(2),0(R2)                                                
         BE    OEST6                                                            
*                                                                               
         GOTO1 SEQ                 READ NEXT KEY                                
*                                                                               
         B     OESTAGAN                                                         
*                                                                               
OEST6    L     R5,AIO1                                                          
         ST    R5,AIO                                                           
*                                                                               
         GOTO1 GETREC              READ IN ESTIMATE RECORD                      
*                                                                               
         MVC   ESTNAME,PESTNAME    SAVE ESTIMATE DATA                           
         MVC   ESTNAME2,PESTNAM2                                  L11           
         OC    ESTNAME2,SPACES                                    L11           
         MVC   ESTDATES,PESTST                                                  
         MVC   ESTFILT,PESTGRPS                                                 
         MVC   ESTRSCH,PESTRSCH    RETAIL SCHEME                                
*                                                                               
*        SAVE UDEF DATA                                                         
*                                                                               
         LA    R6,PESTELEM         POINT TO FIRST ELEMENT IN RECORD             
         SR    RF,RF                                                            
*                                                                               
ODSCUDLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE AT END OF RECORD                        
         BE    ODSCUDDN                                                         
*                                                                               
         CLI   0(R6),X'08'         LOOKING FOR UDEF ELEMENT                     
         BE    ODSCUDFD                                                         
*                                                                               
ODSCUDCN DS    0H                                                               
*                                                                               
         IC    RF,1(R6)            BUMP TO NEXT ELEMENT                         
         LA    R6,0(RF,R6)                                                      
         B     ODSCUDLP            CHECK NEXT ELEMENT                           
*                                                                               
ODSCUDFD DS    0H                  HAVE UDEF ELEMENT                            
*                                                                               
         USING PESTUDEF,R6         ESTABLSIH UDEF ELEMENT                       
*                                                                               
         MVC   ESTUDEF2,PEUSER2    SAVE UDEF 2                                  
*                                                                               
ODSCUDDN DS    0H                                                               
*                                                                               
         CLI   PAOSESAV,0          IF SE NUMBER WAS SAVED                       
         BE    *+18                                                             
         L     RF,PBUTLA           RESTORE IT                                   
         MVC   4(1,RF),PAOSESAV                                                 
         MVI   PAOSESAV,0          REST SAVEAREA                                
*                                                                               
OESTGETX DS    0H                                                               
*                                                                               
         LTR   R3,R3              IF PRINTING                                   
         BZ    *+10                                                             
         MVC   0(20,R3),ESTNAME       PRINT FIRST LINE OF NAME                  
*                                                                               
         MVC   ESHDESC(20),ESTNAME  PUT NAME IN HEADER                          
         MVC   ESHDESC+21(20),ESTNAME2                                          
*                                                                               
         L     RF,SQUASHER                                        L11           
*                                                                               
         GOTO1 (RF),DMCB,ESHDESC,42                                             
*                                                                               
         OC    ESHDESC,SPACES         SPACE FILL                                
*                                                                               
         B     ODESCX                                                           
*                                                                               
ODESCRX  DS    0H                                                               
*                                                                               
ODESCX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
ESTDATA  DS    CL12               ESTIMATE SAVEAREA                             
ESTNAME  DS    CL(L'PESTNAME)                                                   
ESTBL1   DS    CL1                                                              
ESTNAME2 DS    CL(L'PESTNAM2)                                                   
ESTBL2   DS    CL1                                                              
ESTDATES DS    0CL12                                                            
ESTST    DS    CL6                                                              
ESTND    DS    CL6                                                              
ESTFILT  DS    CL3                                                              
ESTRSCH  DS    CL2                 RETAIL SCHEME                                
ESTUDEF2 DS    CL16                UDEF 2                                       
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - EXPTP'                               
***********************************************************************         
*                                                                     *         
*        EXPENSE TYPE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IEXPTP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTEXPTP,R2),SPACES   INIT                                   
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IEXPTP10                                                         
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBEXPTP,R2),ETBEXPTP RETURN EXPENSE TYPE                    
*                                                                               
         CLC   ETBEXPTP,SPACES     DONE IF FOUND                                
         BH    IEXPTP20                                                         
*                                                                               
IEXPTP10 DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IEXPTP20                                                         
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTEXPTP,R2),PRTEXPTP RETURN SOURCE AGENCY CODE              
*                                                                               
IEXPTP20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTEXPTP,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
         CLC   0(L'PRTEXPTP,R2),SPACES   IF NOT FOUND                           
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
IEXPTPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OEXPTP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVRECID,C'1'       IF DOING INVOICE RECORD                       
         BNE   OEXPTPIN                                                         
*                                                                               
         MVC   IVDEXPTP,0(R2)         PUT IN INVOICE RECORD                     
         OC    IVDEXPTP,SPACES        SPACE FILL                                
*                                                                               
         B     OEXPTPRX                                                         
*                                                                               
OEXPTPIN DS    0H                                                               
*                                 ELSE                                          
         MVC   ESDEXPTP,0(R2)         PUT IN ESTIMATE RECORD                    
         OC    IVDEXPTP,SPACES        SPACE FILL                                
*                                                                               
OEXPTPRX DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVDEXPTP,R3),0(R2)       PRINT EXPENSE TYPE                  
*                                                                               
         OC    0(L'IVDEXPTP,R3),SPACES      SPACE FILL                          
*                                                                               
OEXPTPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PRDID'                               
***********************************************************************         
*                                                                     *         
*        PRODUCT ID                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IPRDID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTPRDID,R2),SPACES  INIT                                    
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IPRDID10            NO ENTRY EXISTS                              
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBPRDID,R2),ETBPRDID RETURN PRODUCT ID                      
*                                                                               
         CLC   ETBPRDID,SPACES     DONE IF FOUND                                
         BH    IPRDID20                                                         
*                                                                               
IPRDID10 DS    0H                                                               
*                                                                               
         ICM   R6,RF,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IPRDID20            NO ENTRY EXISTS                              
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTPRDID,R2),PRTPRDID RETURN PRODUCT ID CODE                 
*                                                                               
IPRDID20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTPRDID,R2),SPACES  MAKE PRINTABLE                          
*                                                                               
         CLC   0(L'PRTPRDID,R2),SPACES  IF PRODUCT ID NOT FOUND                 
         BH    *+8                                                              
         OI    ERRCD,ERRPRD          SET ERROR CODE                             
*                                                                               
IPRDIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OPRDID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVRECID,C'1'       IF DOING INVOICE RECORD                       
         BNE   OPRDIDIN                                                         
*                                                                               
         MVC   IVDPRDID,0(R2)        PUT IN INVOICE RECORD                      
         OC    IVDPRDID,SPACES       SPACE FILL                                 
*                                                                               
         B     OPRDIDRX                                                         
*                                                                               
OPRDIDIN DS    0H                 ELSE                                          
*                                                                               
         MVC   ESDPRDID,0(R2)        PUT IN ESTIMATE RECORD                     
         OC    ESDPRDID,SPACES       SPACE FILL                                 
*                                                                               
OPRDIDRX DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVDPRDID,R3),0(R2)        PRINT PRODUCT ID                   
*                                                                               
         OC    0(L'IVDPRDID,R3),SPACES       SPACE FILL                         
*                                                                               
OPRDIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTID'                               
***********************************************************************         
*                                                                     *         
*        ESTIMATE ID - CL3'CLT',CL3'PRD',CL3'EST'                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IESTID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R1,R2               COPY INPUT POINTER                           
*                                                                               
         MVC   0(L'PBCLT,R1),PBCLT  RETURN CLT                                  
         LA    R1,L'PBCLT(R1)      BUMP TO NEXT POSITION                        
*                                                                               
         MVC   0(L'PBPROD,R1),PBPROD  RETURN PRD                                
         LA    R1,L'PBPROD(R1)      BUMP TO NEXT POSITION                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PBEST          ESTIMATE NUMBER                              
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R1),DUB         PLACE IN RECORD                              
*                                                                               
         CLI   PBMODE,PBPROCBK     IF NOT PROCESSING ESTIMATE BUCKETS           
         BE    *+12                                                             
         BRAS  RE,ADDESTID            ADD ESTID TO TABLE                        
         B     IESTIDX                                                          
*                                  ELSE                                         
         BRAS  RE,GETESTID           CHECK IF ESTIMATE IN TABLE                 
         BE    *+8                      YES - KEEP RECORD                       
         MVI   REJECT,C'Y'              NO  - REJECT RECORD                     
*                                                                               
IESTIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OESTID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVESTID,0(R2)      SAVE ESTIMATE ID                              
*                                                                               
         CLI   SVRECID,C'1'       IF DOING INVOICE RECORD                       
         BNE   OESTIDIN                                                         
*                                                                               
         MVC   IVDESTID,0(R2)        PUT IN INVOICE RECORD                      
         OC    IVDESTID,SPACES       BLANK FILL                                 
*                                                                               
         B     OESTIDRX                                                         
*                                                                               
OESTIDIN DS    0H                 ELSE                                          
*                                                                               
         MVC   ESHESTID,0(R2)        PUT IN ESTIMATE RECORD                     
         OC    ESHESTID,SPACES       BLANK FILL                                 
*                                                                               
OESTIDRX DS    0H                 ELSE                                          
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVDESTID,R3),0(R2)    PRINT ESTIMATE ID                      
*                                                                               
         OC    0(L'IVDESTID,R3),SPACES   SPACE FILL                             
*                                                                               
OESTIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - CHGDT'                               
***********************************************************************         
*                                                                     *         
*        CHARGE DATE - FIRST DAY OF BILLABLE MONTH                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICHGDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PBMODE,PBPROCBL    IF PROCESSING BILLS                           
         BNE   ICHGDTIN                                                         
*                                                                               
         L     R5,AIO1             POINT TO BILL HEADER RECORD                  
         USING PBILLRCD,R5                                                      
*                                                                               
         MVC   0(2,R2),PBILKMOS    COPY YM OF MONTH OF SERVICE                  
*                                                                               
         MVI   2(R2),1             FORCE TO FIRST OF THE MONTH                  
         B     ICHGDTX                                                          
*                                                                               
*        CODE KEPT AROUND JUST IN CASE                                          
*                                                                               
         SR    RF,RF                                                            
         LA    R1,ICHGEOM-1        POINT TO TABLE OF MONTH LAST DAYS            
*                                                                               
         TM    PBILKMOS,X'03'      IF YEAR DIVISIBLE BY 4                       
         BNZ   *+8                                                              
         LA    R1,ICHGEOM1-1          USE LEAP YEAR TABLE                       
*                                                                               
         LA    R1,0(RF,R1)         POINT TO DAYS IN MONTH                       
*                                                                               
         MVC   2(1,R2),0(R1)       FILL IN LAST DAY IN MONTH                    
*                                                                               
         B     ICHGDTX                                                          
*                                                                               
ICHGDTIN DS    0H                 ASSUME PROCESSING ESTIMATE BUCKETS            
*                                                                               
         MVC   0(L'ESHCHGDT,R2),SPACES RETURN SPACES                            
*                                                                               
ICHGDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*              DAYS IN A MONTH                                                  
ICHGEOM  DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
ICHGEOM1 DC    AL1(31,29,31,30,31,30,31,31,30,31,30,31) - LEAP YEAR             
*                                                                               
         LTORG                                                                  
*                                                                               
OCHGDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WORK,SPACES        INIT TO BLANKS                                
*                                                                               
         OC    0(3,R2),0(R2)      SKIP IF DATE MISSING                          
         BZ    OCHGDT2                                                          
         CLC   0(3,R2),SPACES     SKIP IF DATE MISSING                          
         BE    OCHGDT2                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(23,WORK)  CHARGE DATE                     
*                                                                               
         OC    WORK,SPACES        MAKE PRINTABLE                                
*                                                                               
OCHGDT2  DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'1'       IF DOING INVOICE RECORD                       
         BNE   OCHGDT4                                                          
*                                                                               
         MVC   IVDCHGDT,WORK          PUT IN INVOICE DETAIL                     
         B     OCHGDT5                                                          
*                                 ELSE                                          
OCHGDT4  DS    0H                                                               
*                                                                               
         MVC   ESHCHGDT,SPACES       INIT CHARGE DATE                           
*                                                                               
         LAY   R1,ESTDATA          POINT TO SAVED ESTIMATE DATA                 
*                                                                               
         CLC   =C'**UNKNOWN**',ESTDATES-ESTDATA(R1) SKIP IF UNKNOWN             
         BE    OCHGDTX                                                          
*                                                                               
         MVC   WORK(4),ESTST-ESTDATA(R1)   EST START YYMM                       
         MVC   WORK+4,=C'01'          DD=01                                     
*                                                                               
         GOTO1 DATCON,DMCB,WORK,(23,ESHCHGDT)  PRINT CHARGE DATE                
*                                                                               
         B     OCHGDTX               NOT REQUIRED                               
*                                                                               
         MVC   ESHCHGDT,WORK         PUT IN ESTIMATE HEADER                     
*                                                                               
OCHGDT5  DS    0H                                                               
*                                                                               
         LTR   R3,R3              SKIP IF NOT PRINTING                          
         BZ    *+10                                                             
         MVC   0(10,R3),WORK      PRINT DATE                                    
*                                                                               
OCHGDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BKNET'                               
***********************************************************************         
*                                                                     *         
*        NET - FROM ESTIMATE BUCKETS                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBKNET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PBMODE,PBPROCBK     REJECT RECORD IF NOT ESTIMATE BUCKET         
         BE    *+12                                                             
         MVI   REJECT,C'Y'                                                      
         B     IBKNETX                                                          
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         ZAP   0(8,R2),BKONET      NET DRDERED                                  
*                                                                               
IBKNETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - NET'                                 
***********************************************************************         
*                                                                     *         
*        NET - BHNET                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ONET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    ONETTL                                                           
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   ONETIVN                                                          
*                                                                               
         EDIT  (P8,0(R2)),IVDNET,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   IVDNET+L'IVDNET-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   IVDNET+L'IVDNET-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         AP    SPIVNET,0(8,R2)     ACCUMULATE GRAND TOTAL                       
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     ONETX                                                            
*                                                                               
ONETIVN  DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'        IF ESTIMATE RECORD                           
         BNE   ONETESN                                                          
*                                                                               
         EDIT  (P8,0(R2)),ESDNET,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   ESDNET+L'ESDNET-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESDNET+L'ESDNET-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         AP    SPESNET,0(8,R2)     ACCUMULATE GRAND TOTAL                       
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     ONETX                                                            
*                                                                               
ONETESN  DS    0H                                                               
         B     ONETX               UNKNOWN RECORD ID                            
*                                                                               
*        PUT TOTALS TO CONTROL RECORD                                           
*                                                                               
ONETTL   DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE RECORD                             
         BNE   ONETTLIN                                                         
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     ONETX                                                            
*                                                                               
ONETTLIN DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'       IF ESTIMATE RECORD                            
         BNE   ONETTLEN                                                         
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     ONETX                                                            
*                                                                               
ONETTLEN DS    0H                 UNKNOWN RECORD ID                             
*                                                                               
ONETX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BKCOM'                               
***********************************************************************         
*                                                                     *         
*        COMMISSION - FROM ESTIMATE BUCKETS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBKCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PBMODE,PBPROCBK     SKIP IF NOT ESTIMATE BUCKET                  
         BNE   IBKCOMX                                                          
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         MVC   0(16,R2),SPACES     NOT REPORTING COMMISSION                     
*                                                                               
IBKCOMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BKCOM'                               
***********************************************************************         
*                                                                     *         
*        COMMISSION - FROM ESTIMATE BUCKETS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OBKCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ESDCOM,0(R2)       RETURN INPUT                                  
*                                                                               
         MVI   GLHOOK,GLEDIT       PRINT COMMISSION                             
*                                                                               
OBKCOMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - COM'                                 
***********************************************************************         
*                                                                     *         
*        COMMISSION                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OCOM     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'YN',TWAAGY       IF YNR                                       
         BNE   *+14                                                             
         MVC   IVDCOM,SPACES          FILL WITH SPACES                          
         B     OCOMX                                                            
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADA                                    
         BNE   *+14                                                             
         MVC   IVDCOM,SPACES          FILL WITH SPACES                          
         B     OCOMX                                                            
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OCOMTL                                                           
*                                                                               
         EDIT  (P8,0(R2)),IVDCOM,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   IVDCOM+L'IVDCOM-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE NO MONEY FOR LINE                    
         B     *+10                                                             
         MVC   IVDCOM+L'IVDCOM-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         AP    SPIVCOM,0(8,R2)     ACCUMULATE GRAND TOTAL                       
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OCOMX                                                            
*                                                                               
OCOMTL   DS    0H                                                               
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
OCOMX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - IBKADJ'                              
***********************************************************************         
*                                                                     *         
*        ADJUSTMENTS - FROM ESTIMATE BUCKETS                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBKADJ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         MVC   0(16,R2),SPACES     NOT REPORTING ADJUSTMENTS                    
*                                                                               
IBKADJX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - OBKADJ'                              
***********************************************************************         
*                                                                     *         
*        ADJUSTMENTS- FROM ESTIMATE BUCKETS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OBKADJ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ESDADJ,0(R2)        RETURN INPUT                                 
*                                                                               
         MVI   GLHOOK,GLEDIT       PRINT ADJUSTMENTS                            
*                                                                               
OBKADJX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ADJ'                                 
***********************************************************************         
*                                                                     *         
*        ADJUSTMENT = MINUS CD                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OADJ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVRECID,C'2'       SKIP IF NOT REPORTING DATA                    
         BE    OADJX                                                            
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+10                                                             
         ZAP   0(8,R2),=P'0'          NO CD REPORTED                            
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OADJTL                                                           
*                                                                               
         ZAP   DUB,=P'0'                                                        
         SP    DUB,0(8,R2)         REVERSES INPUTS SIGN                         
         ZAP   0(8,R2),DUB                                                      
*                                                                               
         EDIT  (P8,DUB),IVDADJ,2,FLOAT=-   PUT IN INVOICE RECORD                
*                                                                               
         CLC   IVDADJ+L'IVDADJ-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
         B     *+10                                                             
         MVC   IVDADJ+L'IVDADJ-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         AP    SPIVADJ,0(8,R2)     ACCUMULATE GRAND TOTAL                       
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OADJX                                                            
*                                                                               
OADJTL   DS    0H                                                               
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
OADJX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BKGRS'                               
***********************************************************************         
*                                                                     *         
*        GROSS - FROM ESTIMATE BUCKETS                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBKGRS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PBMODE,PBPROCBK     SKIP IF NOT ESTIMATE BUCKET                  
         BNE   IBKGRSX                                                          
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         ZAP   0(8,R2),BKOGRS      GROSS                                        
*                                                                               
IBKGRSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - GRS'                                 
***********************************************************************         
*                                                                     *         
*        GROSS                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OGRS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OGRSTL                                                           
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OGRSIVN                                                          
*                                                                               
         EDIT  (P8,0(R2)),IVDGRS,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   IVDGRS+L'IVDGRS-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
         B     *+10                                                             
         MVC   IVDGRS+L'IVDGRS-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         AP    SPIVGRS,0(8,R2)     ACCUMULATE GRAND TOTAL                       
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OGRSX                                                            
*                                                                               
OGRSIVN  DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'        IF ESTIMATE RECORD                           
         BNE   OGRSESN                                                          
*                                                                               
         EDIT  (P8,0(R2)),ESDGRS,2,FLOAT=-   PUT IN ESTIMATE RECORD             
*                                                                               
         CLC   ESDGRS+L'ESDGRS-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE NO MONEY FOR LINE                    
         B     *+10                                                             
         MVC   ESDGRS+L'ESDGRS-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         AP    SPESGRS,0(8,R2)     ACCUMULATE GRAND TOTAL                       
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OGRSX                                                            
*                                                                               
OGRSESN  DS    0H                                                               
         B     OGRSX              UNKNOWN RECORD ID                             
*                                                                               
*        PUT TOTALS TO CONTROL RECORD                                           
*                                                                               
OGRSTL   DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE RECORD                             
         BNE   OGRSTLIN                                                         
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OGRSX                                                            
*                                                                               
OGRSTLIN DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'       IF ESTIMATE RECORD                            
         BNE   OGRSTLEN                                                         
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OGRSX                                                            
*                                                                               
OGRSTLEN DS    0H                 UNKNOWN RECORD ID                             
*                                                                               
OGRSX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - CRAGY'                               
***********************************************************************         
*                                                                     *         
*        CREATIVE AGENCY                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICRAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTCRAGY,R2),SPACES   INIT                                   
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ICRAGY10                                                         
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBCRAGY,R2),ETBCRAGY RETURN CREATIVE AGENCY                 
*                                                                               
         CLC   ETBCRAGY,SPACES     DONE IF FOUND                                
         BH    ICRAGY20                                                         
*                                                                               
ICRAGY10 DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ICRAGY20                                                         
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTCRAGY,R2),PRTCRAGY RETURN CREATIVE AGENCY CODE            
*                                                                               
ICRAGY20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTCRAGY,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
         CLC   0(L'PRTCRAGY,R2),SPACES   IF NOT FOUND                           
         BH    *+8                                                              
         OI    ERRCD,ERRPRD                 FLAG AS ERROR                       
*                                                                               
ICRAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OCRAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   IVDCRAGY,0(R2)      PUT IN INVOICE RECORD                        
         OC    IVDCRAGY,SPACES     BLANK FILL                                   
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVDCRAGY,R3),IVDCRAGY PRINT ESTIMATE ID                      
*                                                                               
OCRAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - BKBLL'                               
***********************************************************************         
*                                                                     *         
*        BILLED AMOUNT - NOT PROVIDED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBKBLL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         MVC   0(16,R2),SPACES     NOT REPORTING BILLED AMOUNT                  
*                                                                               
IBKBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBKBLL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ESDBLL,0(R2)        RETURN INPUT                                 
*                                                                               
         MVI   GLHOOK,GLEDIT       PRINT BILLED AMOUNT                          
*                                                                               
OBKBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - DEAL#'                               
***********************************************************************         
*                                                                     *         
*        DEAL NUMBER   - NOT PROVIDED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IDEAL#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BKELEM,R6                                                        
*                                                                               
         MVC   0(10,R2),SPACES     NOT REPORTING BILLED AMOUNT                  
*                                                                               
IDEAL#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ODEAL#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ESDDEAL#,0(R2)      RETURN INPUT                                 
*                                                                               
         MVI   GLHOOK,GLEDIT       PRINT BILLED AMOUNT                          
*                                                                               
ODEAL#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - FILLER'                              
***********************************************************************         
*                                                                     *         
*        FILLER FIELD OF SPACES                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OFILLER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    OFILLERX                                                         
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         L     R1,GLADTENT         POINT TO OUTPUT CONTROL BLOCK                
*                                                                               
         ZIC   RE,DROLEN-DROD(R1)  GET OUTPUT LENGTH                            
*                                                                               
         BCTR  RE,0                DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      RETURN SPACES                                
*                                                                               
OFILLERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - OHST'                                
***********************************************************************         
*                                                                     *         
*        HST - BHHST                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OHST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OHSTX                                                            
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OHSTIVN                                                          
*                                                                               
         TP    0(8,R2)             SKIP IF NOT PACKED                           
         BNE   OHSTX                                                            
*                                                                               
         ZAP   KRHST,0(8,R2)       SAVE HST                                     
*                                                                               
OHSTIVN  DS    0H                                                               
*                                                                               
OHSTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - OGST'                                
***********************************************************************         
*                                                                     *         
*        GST - BHGST                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OGST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OGSTX                                                            
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OGSTIVN                                                          
*                                                                               
         MVC   IVGSTID,=CL20'886669233'  SET GST ID                             
*                                                                               
         TP    0(8,R2)             SKIP IF NOT PACKED                           
         BNE   OGSTX                                                            
*                                                                               
         AP    0(8,R2),KRHST       ADD IN HST                                   
*                                                                               
         EDIT  (P8,0(R2)),IVGST,2,FLOAT=-   PUT IN INVOICE RECORD               
*                                                                               
         CLC   IVGST+L'IVGST-4(4),=C' .00'   IF ZERO                            
         BNE   *+10                                                             
         MVC   IVGST+L'IVGST-4(4),=C'0.00'  ADD ZERO                            
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OGSTX                                                            
*                                                                               
OGSTIVN  DS    0H                                                               
*                                                                               
OGSTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - OPST'                                
***********************************************************************         
*                                                                     *         
*        HST - BHPST                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OPSTX                                                            
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OPSTIVN                                                          
*                                                                               
         MVC   IVQSTID,=CL20' '       SET QST ID                                
*                                                                               
         TP    0(8,R2)             SKIP IF NOT PACKED                           
         BNE   OPSTX                                                            
*                                                                               
         SP    0(8,R2),KRHST       SUBTRACT OUT HST                             
*                                                                               
         EDIT  (P8,0(R2)),IVQST,2,FLOAT=-   PUT IN INVOICE RECORD               
*                                                                               
         CLC   IVQST+L'IVQST-4(4),=C' .00'   IF ZERO                            
         BNE   *+10                                                             
         MVC   IVQST+L'IVQST-4(4),=C'0.00'  ADD ZERO                            
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OPSTX                                                            
*                                                                               
OPSTIVN  DS    0H                                                               
*                                                                               
OPSTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ERR'                                 
***********************************************************************         
*                                                                     *         
*        ERROR CODES                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IERR     NTR1  BASE=*,LABEL=*      INPUT OF ERROR CODE                          
*                                                                               
         MVC   0(1,R2),ERRCD       ERRORS                                       
*                                                                               
IERRX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OERR     NTR1  BASE=*,LABEL=*      PRINT ERROR CODE                             
*                                                                               
         LTR   R3,R3               SKIP IF NOT PRINTING                         
         BZ    OERRX                                                            
*                                                                               
         MVC   ERRCD,0(R2)         SAVE ERROR CODE                              
*                                                                               
         CLI   ERRCD,0             DONE IF NO ERRORS                            
         BE    OERRX                                                            
*                                                                               
         LR    R1,R3                                                            
*                                                                               
         TM    ERRCD,ERRAGY        CHECK FOR AGENCY IN ERROR                    
         BZ    *+14                                                             
         MVC   0(4,R1),=C'AGY,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    ERRCD,ERRPRD        CHECK FOR PRODUCT ERROR                      
         BZ    *+14                                                             
         MVC   0(4,R1),=C'PRD,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    ERRCD,ERREST        CHECK FOR ESTIMATE ERROR                     
         BZ    *+14                                                             
         MVC   0(3,R1),=C'EST'                                                  
         LA    R1,3(R1)                                                         
*                                                                               
         BCTR  R1,0                REMOVE TRAILING COMMA                        
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         B     OERRX                                                            
*                                                                               
OERRX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - LASTCOL'                             
***********************************************************************         
*                                                                     *         
*        LAST COLUMN - WRITE RECORD TO TAPE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LASTCOL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   LASTCOLX                                                         
*                                                                               
         CLI   REJECT,C'Y'         TEST RECORD REJECTED                         
         BE    LASTCOLX            YES                                          
*                                                                               
         CLI   ERRCD,0            SKIP RECORDS IN ERROR                         
         NOP   LASTCOLX                                                         
*                                                                               
         CLI   MONEY,C'Y'          TEST $0                                      
         NOP   LASTCOLX            YES                                          
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    LSTCLOPX                                                         
*                                                                               
         CLI   SPINVSW,C'Y'       SKIP IF TAPES ALREADY OPEN                    
         BE    LSTCLO10                                                         
*                                                                               
*        OPEN TAPES                                                             
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN FILE                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SPINVSW,C'Y'        INVOICE FILE OPENED                          
*                                                                               
LSTCLO10 DS    0H                                                               
*                                                                               
         CLI   SPESTSW,C'Y'       SKIP IF TAPES ALREADY OPEN                    
         BE    LSTCLO20                                                         
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN TEMP DISK FILE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SPESTSW,C'Y'        INVOICE FILE OPENED                          
*                                                                               
LSTCLO20 DS    0H                                                               
*                                                                               
LSTCLOPX DS    0H                                                               
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    LSTCLTL                                                          
*                                                                               
*        INVOICE RECORDS                                                        
*                                                                               
         CLI   SVRECID,C'1'       SKIP IF NOT INVOICE RECORD                    
         BNE   LSTCLIN                                                          
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCB                     
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         CLI   SPIHEAD,C'H'       IF HEADER TO BE PRINTED                       
         BNE   LSTCLIV1                                                         
*                                                                               
*        BUILD INVOICE BATCH HEADER AND CONTROL RECORD                          
*                                                                               
         MVI   IBHREC,C' '         INIT INVOICE BATCH HEADER RECORD             
         MVC   IBHREC+1(256),IBHREC                                             
         MVC   IBHREC+257(L'IBHREC-256-1),IBHREC                                
*                                                                               
         MVC   IBHRECID,=CL3'AI '  SET RECORD ID                                
         MVC   IBHSTART,=CL3'ADI'  SET SECONDARY RECORD ID                      
         MVC   IBHRECV,=CL10'NVOICE' SET REST OF RECORD ID                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(23,WORK)     SET YYYY-MM-DD                   
*                                  TRANSLATE TO MM/DD/YYYY                      
         MVI   WORK+10,C'/'        FOR TRANSLATE PURPOSES                       
         MVC   WORK+20(10),=X'05060A08090A00010203'                             
         TR    WORK+20(10),WORK    RE-ARRANGE DATE                              
         MVC   IBHCRDTE,WORK+20    SET CREATION DATE                            
*                                                                               
         TIME  DEC                 GET CURRENT TIME - PUS'HHMMSS.SS'            
*                                    RETURNED IN R0                             
         ZAP   FULL,=P'0'          INIT PACKED FIELD                            
         STCM  R0,14,FULL          SAVE P(HHMMSS0C)                             
*                                                                               
         ZAP   DUB,FULL            DOUBLE WORD FOR EDITING                      
*                                                                               
         AP    DUB,=P'600000'      ADD SIX HOURS                                
*                                                                               
         MVC   WORK(11),=X'40212020207A20207A2020' SET EDIT PATTERN             
         ED    WORK(11),DUB+3      PRINT C'  HH:MM:SS'                          
         MVC   IBHCRTIM,WORK+3     SET HH:MM:SS                                 
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    LSTCLIVA                                                         
*                                                                               
         LA    R6,IBHREC             WRITE BATCH HEADER REOCRD                  
         PUT   (R3),(R6)                                                        
*                                                                               
LSTCLIVA DS    0H                                                               
*                                                                               
         MVI   SPIHEAD,C'N'          TURN OFF SWITCH                            
*                                                                               
LSTCLIV1 DS    0H                                                               
*                                                                               
         LA    R6,IVREC            POINT TO OUTPUT INVOICE RECORD               
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    LSTCLIV2                                                         
*                                                                               
         CLI   KRCTRY,C'C'         IF CANADIAN AGENCY                           
         BNE   LSTCLI1A                                                         
*                                     BUILD CANADIAN RECORD                     
         LA    R6,KRCDNREC            POINT TO CANADIAN RECORD                  
*                                                                               
         MVC   KRCHDR,IVHDR              HEADER                                 
         MVC   KRCCDN,IVCDNFLS           CANADIAN FIELDS                        
         MVC   KRCDTL,IVDTL              DETAILS                                
*                                     BUILD CANADIAN RECORD                     
LSTCLI1A DS    0H                                                               
*                                                                               
         PUT   (R3),(R6)           PUT TO TEMP DISK FILE                        
*                                                                               
LSTCLIV2 DS    0H                                                               
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER                          
*                                                                               
         B     LASTCOLX                                                         
*                                                                               
*        ESTIMATE RECORDS                                                       
*                                                                               
LSTCLIN  DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'       SKIP IF NOT ESTIMATE RECORD                   
         BNE   LSTCLEN                                                          
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         CLI   SPEHEAD,C'H'       IF EST HEADER STILL TO BE PRINTED             
         BNE   LSTCLES1                                                         
*                                                                               
*        BUILD ESTIMATE BATCH HEADER AND CONTROL RECORD                         
*                                                                               
         MVI   EBHREC,C' '         INIT ESTIMATE BATCH HEADER RECORD            
         MVC   EBHREC+1(235),EBHREC                                             
*                                                                               
         ZAP   SPEBTCTR,=P'0'      INIT BATCH RECORD COUNTER                    
*                                                                               
         MVC   EBHRECID,=CL3'AE '  SET RECORD ID                                
         MVC   EBHSTART,=CL3'ADE'  SET SECONDARY RECORD ID                      
         MVC   EBHRECV,=CL10'STIMATE' SET REST OF RECORD ID                     
*                                                                               
         MVC   EBHCRDTE,IBHCRDTE   SET CREATION DATE                            
         MVC   EBHCRTIM,IBHCRTIM   SET HH:MM:SS                                 
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    LSTCLESA                                                         
*                                                                               
         LA    R6,EBHREC             WRITE BATCH HEADER RECORD                  
         PUT   (R3),(R6)                                                        
*                                                                               
LSTCLESA DS    0H                                                               
*                                                                               
         MVI   SPEHEAD,C'N'          TURN OFF SWITCH                            
*                                                                               
LSTCLES1 DS    0H                                                               
*                                                                               
         LA    R6,ESREC            POINT TO OUTPUT ESTIMATE RECORD              
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    LSTCLES2                                                         
*                                                                               
         PUT   (R3),(R6)           PUT TO TEMP DISK FILE                        
*                                                                               
LSTCLES2 DS    0H                                                               
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER                          
*                                                                               
         B     LASTCOLX                                                         
*                                                                               
*        UNKNOWN RECORD TYPE                                                    
*                                                                               
LSTCLEN  DS    0H                                                               
*                                                                               
         B     LASTCOLX                                                         
*                                                                               
LSTCLTL  DS    0H                                                               
*                                                                               
LASTCOLX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - WORKTSTG'                            
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKSTG  DS    0D                                                               
         DS    0F                                                               
ASPFUSER DS    A                                                                
AFORMTAB DS    A(FORMTAB)    FORMULA TABLE                                      
APRTENT  DS    A                   A(ENTRY IN PRODUCT  TABLE)                   
AETBENT  DS    A                   A(ENTRY IN ESTIMATE TABLE)                   
AESTZZT  DS    A(AESTZZT)    POL TABLE                                          
VDYNALLO DS    V                   V(DYNALLOC)                                  
TOTNET   DS    PL8                                                              
TOTCOM   DS    PL8                                                              
WKYEAR   DS    CL4                 YEAR                                         
*                                                                               
*        FIELD NAMES IN GFEST RECORD                                            
*                                                                               
QNAMETB  DS    0X                  TABLE OF GFEST FIELD NAMES/LENGTHS           
QDIVBR   DC    CL8'DIVBRNCD',AL1(L'PRTDIVBD)                                    
QPRDCD   DC    CL8'PROD CD ',AL1(L'PRTPRDCD)                                    
QGFNAT   DC    CL8'GF NTRL ',AL1(L'PRTGFNAT)                                    
QGFSUB   DC    CL8'GFSBNTRL',AL1(L'PRTGFSUB)                                    
QBLAGY   DC    CL8'BILL AGY',AL1(L'PRTBLAGY)                                    
QEXPTP   DC    CL8'EXP TYPE',AL1(L'PRTEXPTP)                                    
QPRDID   DC    CL8'PROD ID ',AL1(L'PRTPRDID)                                    
QCRTAG   DC    CL8'CRTV AGY',AL1(L'PRTCRAGY)                                    
QSRCAG   DC    CL8'SRCE AGY',AL1(L'PRTSRCAG)                                    
QREQNM   DC    CL8'REQ NM  ',AL1(L'PRTREQNM)                                    
QTGTMK   DC    CL8'TRGT MKT',AL1(L'PRTTGTMK)                                    
QDEAL    DC    CL8'DEAL# GV',AL1(L'PRTDEAL)                                     
QNAMENOQ EQU   (*-QNAMETB)/9       NUMBER OF ENTRIES IN TABLE                   
         DC    X'FF'               EOT                                          
*                                                                               
*        OLD FIELD NAMES IN GFEST RECORD                                        
*                                                                               
*                                                                               
COMPYDIV DS    CL2                 GF FIELDS                                    
PRODCODE DS    CL4                                                              
NATURAL  DS    CL3                                                              
SUBNATRL DS    CL3                                                              
ETEST    DS    CL1                TEST ESTIMATE BYTE X'80' = TEST               
EBFORM   DS    XL5                BILLING FORMULA                               
EBFDATE  DS    XL3                BILLING FORMUAL EFF DATE                      
AGENCYCD DS    CL1                                                              
*                                                                               
KRHST    DS    PL8                 HST SAVEAREA                                 
*                                                                               
SVRECID  DS    CL1                RECORD IDENTIFIER                             
SVESTID  DS    CL10               ESTIMATE ID SAVEAREA                          
SVMED    DS    CL1                                                              
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
SVBFEST  DS    XL2                                                              
REJECT   DS    CL1                                                              
MONEY    DS    CL1                                                              
BILLMON  DS    CL4                                                              
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRAGY   EQU   X'80'               AGENCY                                       
ERRPRD   EQU   X'40'               PRODUCT                                      
ERREST   EQU   X'08'               ESTIMATE                                     
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
XFF      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                                   
*                                                                               
MONTAB   DS    25XL6               MONTH TABLE                                  
*                                  ENTRIES ARE BINARY YYMM FOLLOWED             
*                                  BY CHARACTERS YYMM                           
*                                  EXAMPLE - X'5801' C'8801'                    
*                                                                               
         DC    25XL6'0'                                                         
         DC    25XL6'0'                                                         
SUPTAP   DS    CL1                                                              
*                                                                               
DESTID   DS    CL10                DESTINATION ID                               
*                                                                               
PARMLST1 CALL  ,(DDKRINV,SPFKRINV),MF=L  GET GENERATION                         
PARMLST2 CALL  ,(DDKREST,SPFKREST),MF=L  GET GENERATION                         
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
DDKRINV  DC    CL8'KRINVFL '                                                    
DDKREST  DC    CL8'KRESTFL '                                                    
DSNKRINV DC    CL20'PRTTAPE.PP0KRXX1'                                           
DSNKREST DC    CL20'PRTTAPE.PP0KRXX2'                                           
TMPALLOC DC    XL6'000003000003'                                                
MYWORK   DS    CL5                                                              
AKRINVFL DS    A                   A(KRINVFL  DCB IN SPFUSER)                   
AKRCINVF DS    A                   A(KRCINVFL DCB IN SPFUSER)                   
AKRESTFL DS    A                   A(KRESTFL  DCB IN SPFUSER)                   
*                                                                               
EITBBXLE DS    3A                  BXLE REGISTERS FOR ESTID TABLE               
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - SPFAREA'                             
***********************************************************************         
*                                                                     *         
*        SPFAREA -  VALUES SAVED IN SPFUSER BETWEEN REQUESTS          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPFAREA  DS    0D                                                               
*                                                                               
SPFAGYCD DS    CL1                                                              
SPFTITLE DS    CL63                                                             
SPFTTLSB DS    CL36                                                             
SPFSPLID DS    CL3                                                              
KRCTRY   DS    CL1                 C'C' - CANADIAN AGENCY                       
*                                                                               
SPFKRINV DC    CL44' '             GENERATION NAME                              
SPFKREST DC    CL44' '             GENERATION NAME                              
*                                                                               
SPFAPRDS DS    A                   A(PRODUCT TABLE)                             
APRDTAB  DS    A                   A(PRODUCT TABLE)                             
SPFAESTS DS    A                   A(ESTIMATE TABLE)                            
AESTTAB  DS    A                   A(ESTIMATE TABLE)                            
         DS    0D                  ALIGNMENT                                    
SPIBTCTR DS    PL8                 INVOICE  RECORD COUNTER                      
SPEBTCTR DS    PL8                 ESTIMATE RECORD COUNTER                      
*                                                                               
SPIBLAGY DS    CL8                 INVOICE  BILLING AGENCY                      
SPEBLAGY DS    CL8                 ESTIMATE BILLING AGENCY                      
*                                                                               
SPINVSW  DS    C                   INVOICE  FILE OPEN SWITCH                    
SPESTSW  DS    C                   ESTIMATE FILE OPEN SWITCH                    
*                                                                               
SPIHEAD  DS    XL1                C'H' - HEADER TO BE PRINTED                   
SPEHEAD  DS    XL1                C'H' - HEADER TO BE PRINTED                   
*                                                                               
SPIVBTID DS    CL20                BATCH ID                                     
SPESBTID DS    CL20                BATCH ID                                     
*                                                                               
         DS    0D                  ALIGNMENT                                    
SPIVNET  DS    PL8                 BATCH INVOICE NET                            
SPIVCOM  DS    PL8                 BATCH INVOICE COMMISSION                     
SPIVADJ  DS    PL8                 BATCH INVOICE ADJUSTMENT                     
SPIVGRS  DS    PL8                 BATCH INVOICE GROSS                          
*                                                                               
SPESNET  DS    PL8                 BATCH ESTIMATE NET                           
SPESCOM  DS    PL8                 BATCH ESTIMATE COMMISSION                    
SPESADJ  DS    PL8                 BATCH ESTIMATE ADJUSTMENT                    
SPESGRS  DS    PL8                 BATCH ESTIMATE GROSS                         
*                                                                               
SPFAREAL EQU   *-SPFAREA                                                        
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - DCBS'                                
***********************************************************************         
*                                                                     *         
*        DCBS                                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
KRINVFL  DCB   DDNAME=KRINVFL,DSORG=PS,LRECL=246,BLKSIZE=24600,        X        
               MACRF=PM,RECFM=FB                                                
*                                                                               
KRCINVFL DCB   DDNAME=KRCINVFL,DSORG=PS,LRECL=318,BLKSIZE=31800,       X        
               MACRF=PM,RECFM=FB                                                
*                                                                               
KRESTFL  DCB   DDNAME=KRESTFL,DSORG=PS,LRECL=236,BLKSIZE=23600,        X        
               MACRF=PM,RECFM=FB                                                
*                                                                               
SAVVALSL EQU   *-SPFAREA                                                        
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INVOICE RECORDS - IBHREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE BATCH HEADER RECORD                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
IBHREC   DS    0CL318          *** INVOICE BATCH HEADER RECORD                  
*                                                                               
IBHRECID DS    CL3'AI '            RECORD ID                                    
IBHSTART DS    CL3'ADI'            FURTHER RECORD ID                            
IBHRECV  DS    CL10'NVOICE'        MORE RECORD ID                               
IBHCRDTE DS    CL10                CREATION DATE                                
IBHCRTIM DS    CL8                 CREATION TIME                                
         DS    XL(L'IBHREC-(*-IBHREC)) SPARE                                    
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INVOICE RECORDS - IVREC'             
***********************************************************************         
*                                                                     *         
*        INVOICE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IVREC    DS    0D              *** INVOICE RECORD                               
*                                                                               
IVHDR    DS    0CL67               INVOICE HEADER                               
IVHBLAGY DS    CL8                 BILLING AGENCY CODE                          
IVHBTID  DS    CL20                BATCH ID                                     
IVHINVNO DS    CL20                INVOICE NUMBER                               
IVHINVDT DS    CL10                INVOICE DATE                                 
IVHSRCAG DS    CL8                 SOURCE AGENCY                                
IVHREVRS DS    CL1                 C'Y' - REVERSAL INVOICE                      
IVDTL    DS    0XL179              INVOICE DETAIL                               
IVHBLSTA DS    CL1                 BILLING STATUS                               
*                                  C'F' - FINAL                                 
*                                  C'P' - IN PROGRESS                           
*                                  C'O' - ORIGINAL                              
*                                  C'M' - MEDIA                                 
IVDLINE# DS    CL10                LINE NUMBER                                  
IVDDESC  DS    CL50                DESCRIPTION                                  
IVDEXPTP DS    CL6                 EXPENSE TYPE                                 
IVDPRDID DS    CL10                PRODUCT  ID                                  
IVDESTID DS    CL20                ESTIMATE ID                                  
IVDCHGDT DS    CL10                CHARGE DATE                                  
IVDNET   DS    CL16                NET                                          
IVDCOM   DS    CL16                COMMISSION                                   
IVDADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
IVDGRS   DS    CL16                GROSS                                        
IVDCRAGY DS    CL8                 CREATIVE AGENCY ID                           
*                                                                               
IVRECL   EQU   *-IVREC             RECORD LENGTH                                
*                                                                               
IVCDNFLS DS    0XL72               CANADIAN FIELDS                              
IVGST    DS    CL16                GST+HST                                      
IVQST    DS    CL16                QST                                          
IVGSTID  DS    CL20                GST-HST REGISTRATION CODE                    
IVQSTID  DS    CL20                QST     REGISTRATION CODE                    
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INVOICE RECORDS - IVCREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE CONTROL RECORD                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
IVCREC   DS    0CL318          *** INVOICE CONTROL RECORD                       
*                                                                               
IVCBLAGY DS    CL8                 BILLING AGENCY CODE                          
IVCBTID  DS    CL20                BATCH ID                                     
IVCRECCT DS    CL10                RECORD COUNT                                 
IVCNET   DS    CL16                NET                                          
IVCCOM   DS    CL16                COMMISSION                                   
IVCADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
IVCGRS   DS    CL16                GROSS                                        
IVCCRAGY DS    CL8                 CREATIVE AGENCY ID                           
*                                                                               
         DS    XL(L'IVCREC-(*-IVCREC)) SPARE                                    
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - INVOICE RECORDS - IBTREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE BATCH TRAILER RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBTREC   DS    0CL318          *** INVOICE BATCH TRAILER RECORD                 
*                                                                               
IBTRECID DS    CL3'AI '            RECORD ID                                    
IBTSTRT  DS    CL17'TTRL'          FURTHER RECORD ID                            
IBTRECCT DS    CL5                 RECORD COUNT                                 
*                                                                               
         DS    XL(L'IBTREC-(*-IBTREC)) SPARE                                    
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTIMATE RECORDS - EBHREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE BATCH HEADER RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
EBHREC   DS    0CL236          *** ESTIMATE BATCH HEADER RECORD                 
*                                                                               
EBHRECID DS    CL3'AE '            RECORD ID                                    
EBHSTART DS    CL3'ADE'            FURTHER RECORD ID                            
EBHRECV  DS    CL10'STIMATE'       MORE RECORD ID                               
EBHCRDTE DS    CL10                CREATION DATE                                
EBHCRTIM DS    CL8                 CREATION TIME                                
         DS    XL(L'EBHREC-(*-EBHREC)) SPARE                                    
         TITLE 'T40515 - KRTAPE CREATION - ESTIMATE RECORDS - ESREC'            
***********************************************************************         
*                                                                     *         
*        ESTIMATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESREC    DS    0D              *** ESTIMATE RECORD                              
*                                                                               
ESHDR    DS    0CL68               ESTIMATE HEADER                              
ESHBLAGY DS    CL8                 BILLING AGENCY CODE                          
ESHBTID  DS    CL20                BATCH ID                                     
ESHESTID DS    CL20                ESTIMATE NUMBER CLT/PRD/EST                  
ESHDESC  DS    CL50                ESTIMATE DESCRIPTION                         
ESHCHGDT DS    CL10                CHARGE DATE                                  
ESHBLSTA DS    CL1                 BILLING STATUS                               
*                                  C'F' - FINAL                                 
*                                  C'P' - IN PROGRESS                           
*                                  C'O' - ORIGINAL                              
*                                  C'M' - MEDIA                                 
ESHREQNM DS    CL20                REQUIREMENT NUMBER                           
ESHTGTMK DS    CL1                 TARGET MARKET CODE                           
*                                                                               
ESDTL    DS    0XL178              ESTIMATE DETAIL                              
ESDEXPTP DS    CL6                 EXPENSE TYPE                                 
ESDPRDID DS    CL10                PRODUCT  ID                                  
ESDNET   DS    CL16                NET                                          
ESDCOM   DS    CL16                COMMISSION                                   
ESDADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
ESDGRS   DS    CL16                GROSS                                        
ESDBLL   DS    CL16                BILLED SO FAR - NOT NEEDED                   
ESDDEAL# DS    CL10                DEAL NUMBER                                  
*                                                                               
ESRECL    EQU   *-ESREC              RECORD LENGTH                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTIMATE RECORDS - IVCREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE CONTROL RECORD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
ESCREC   DS    0CL236          *** ESTIMATE CONTROL RECORD                      
*                                                                               
ESCBLAGY DS    CL8                 BILLING AGENCY CODE                          
ESCBTID  DS    CL20                BATCH ID                                     
ESCRECCT DS    CL10                RECORD COUNT                                 
ESCNET   DS    CL16                NET                                          
ESCCOM   DS    CL16                COMMISSION                                   
ESCADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
ESCGRS   DS    CL16                GROSS                                        
ESCBLL   DS    CL16                BILLED                                       
*                                                                               
         DS    XL(L'ESCREC-(*-ESCREC)) SPARE                                    
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTIMATE RECORDS - ISTREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE BATCH TRAILER RECORD                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTREC   DS    0CL236          *** ESTIMATE BATCH TRAILER RECORD                
*                                                                               
ESTRECID DS    CL3'AE '            RECORD ID                                    
ESTSTRT  DS    CL17'TTRL'          FURTHER RECORD ID                            
ESTRECCT DS    CL5                 RECORD COUNT                                 
*                                                                               
         DS    XL(L'ESTREC-(*-ESTREC)) SPARE                                    
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - GFREC'                               
***********************************************************************         
*                                                                     *         
*        GF RECORD LAYOUTS                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GFREC    DS    0CL80           *** GF RECORD ***                                
*                                                                               
GFKEY    DS    0CL43                                                            
GFCOMDIV DS    CL2                                                              
GFPRODCD DS    CL4                                                              
GFAGY    DS    CL1                                                              
GFEST    DS    CL12                                                             
GFMON    DS    CL4                                                              
GFINV    DS    CL9                                                              
GFBLTYPE DS    CL1                                                              
GFNAT    DS    CL3                                                              
GFSUBNAT DS    CL3                                                              
GFMKT    DS    CL4                                                              
*                                                                               
GFNET    DS    CL10                                                             
GFCOM    DS    CL10                                                             
*                                                                               
GFDATE   DS    CL4                                                              
*                                                                               
GFFILLER DS    CL13                                                             
         ORG   GFFILLER                                                         
GFMED    DS    CL1                                                              
GFPNET   DS    PL6                                                              
GFPCOM   DS    PL6                                                              
         ORG                                                                    
         SPACE 2                                                                
SVKEY    DS    CL(GFMON-GFKEY+L'GFMON)    SAVED KEY                             
*                                                                               
KRCDNREC DS    0CL318              CANADIAN RECORD BUILD AREA                   
KRCHDR   DS    XL(L'IVHDR)           HEADER                                     
KRCCDN   DS    XL(L'IVCDNFLS)        CANADIAN FIELDS                            
KRCDTL   DS    XL(L'IVDTL)           DETAIL                                     
KRCDNRL  EQU   *-KRCDNREC          CANADIAN RECORD LENGTH                       
*                                                                               
ESTIDTB  DS    XL(1000*L'IVDESTID) ESTID TABLE                                  
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - DSECTS'                              
***********************************************************************         
*                                                                     *         
*        DSECTS                                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*PRWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*PPCLRST                                                                        
*PGENGRP                                                                        
*PPDDEQUS                                                                       
*CTGENFILE                                                                      
*DDMASTD                                                                        
*INCLUDE DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE PRWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE PPCLRST                                                        
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE PPDDEQUS                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         TITLE 'T40515 - KRTAPE CREATION - PPRDRECD'                            
***********************************************************************         
*                                                                     *         
*        PRODUCT RECORD DSECT                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PAGYRECD'                            
***********************************************************************         
*                                                                     *         
*        AGENCY  RECORD DSECT                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PAGYRECD DSECT                                                                  
PAGYKIDQ EQU   X'01'               AGENCY RECORD ID                             
       ++INCLUDE PAGYREC                                                        
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PESTRECD'                            
***********************************************************************         
*                                                                     *         
*        ESTIMATE RECORD DSECT                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PESTRECD DSECT                                                                  
       ++INCLUDE PESTREC                                                        
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PBILLRCD'                            
***********************************************************************         
*                                                                     *         
*        BILL HEADER RECORD DSECT                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PBILLRCD DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - GFESTRD'                             
***********************************************************************         
*                                                                     *         
*        GF ESTIMATE RECORD DSECT                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GFESTRD  DSECT                                                                  
       ++INCLUDE PGESTREC                                                       
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PBKRECD'                             
***********************************************************************         
*                                                                     *         
*        ESTIMATE BUCKET RECORD DSECT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
       ++INCLUDE DDBKELEM                                                       
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PRWRIE9D'                            
***********************************************************************         
*                                                                     *         
*        SCREEN LAYOUT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE PRWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRIE9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
*                                                                               
*GERFPIOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
*                                                                               
*PJOBREC                                                                        
         PRINT OFF                                                              
       ++INCLUDE PJOBREC                                                        
         PRINT ON                                                               
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTABDS'                             
***********************************************************************         
*                                                                     *         
*        ESTIMATE TABLE DSECT                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTABDS  DSECT                                                                  
*              ENTRIES INCLUDE NATURAL AND SUBNATURAL                           
ESTBNORM DS    CL3     NATURAL                                                  
ESTBSUBN DS    CL3     SUB-NATURAL                                              
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - PRDTABD'                             
***********************************************************************         
*                                                                     *         
*        PRODUCT TABLE DSECT                                         *          
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDTABD  DSECT                                                                  
*                                                                               
PRTENT   DS    0X                  ENTRY IN PRODUCT TABLE                       
*                                                                               
PRTPRD   DS    CL3                 PRODUCT CODE                                 
PRTFIRST DS    0X                  FIRST DATA FIELD                             
PRTDIVBD DS    CL2                 DIVISION BRAND CODE                          
PRTPRDCD DS    CL4                 PRODUCT CODE                                 
PRTGFNAT DS    CL3                 GF NATURAL                                   
PRTGFSUB DS    CL3                 GF SUB NATURAL                               
PRTBLAGY DS    CL8                 BILLING AGENCY                               
PRTEXPTP DS    CL6                 EXPENSE TYPE                                 
PRTPRDID DS    CL10                PRODUCT ID                                   
PRTCRAGY DS    CL8                 CREATIVE AGENCY                              
PRTSRCAG DS    CL8                 SOURCE  AGENCY                               
PRTREQNM DS    CL20                REQUIREMENT NUMBER                           
PRTTGTMK DS    CL1                 TARGET MARKETY                               
PRTDEAL  DS    CL10                DEAL NUMBER                                  
*                                                                               
PRTENTL  EQU   *-PRTENT            LENGTH OF ENTRY IN PRODUCT TABLE             
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTTABD'                             
***********************************************************************         
*                                                                     *         
*        ESTIMATE TABLE DSECT                                        *          
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTTABD  DSECT                                                                  
*                                                                               
ETBENT   DS    0X                  ENTRY IN ESTIMATE TABLE                      
*                                                                               
ETBEST   DS    CL2                 ESTIMATE CODE                                
         DS    XL1                 SPARE                                        
ETBFIRST DS    0X                  FIRST DATA FIELD                             
ETBDIVBD DS    CL2                 DIVISION BRAND CODE                          
ETBPRDCD DS    CL4                 ESTIMATE CODE                                
ETBGFNAT DS    CL3                 GF NATURAL                                   
ETBGFSUB DS    CL3                 GF SUB NATURAL                               
ETBBLAGY DS    CL8                 BILLING AGENCY                               
ETBEXPTP DS    CL6                 EXPENSE TYPE                                 
ETBPRDID DS    CL10                ESTIMATE ID                                  
ETBCRAGY DS    CL8                 CREATIVE AGENCY                              
ETBSRCAG DS    CL8                 SOURCE  AGENCY                               
ETBREQNM DS    CL20                REQUIREMENT NUMBER                           
ETBTGTMK DS    CL1                 TARGET MARKETY                               
ETBDEAL  DS    CL10                DEAL NUMBER                                  
*                                                                               
ETBENTL  EQU   *-ETBENT            LENGTH OF ENTRY IN ESTIMATE TABLE            
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - FORMTAB'                             
***********************************************************************         
*                                                                     *         
*        FORMULA TABLE DSECT                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FF       DSECT                                                    BUG01         
*******  DC    C'** PRWRI15 FORMULAE TABLE **'                    BUG01         
FORMTAB  DS    999XL(1+5+3)                                       BUG01         
         TITLE 'T40515 - KRTAPE CREATION - PRDTAB'                              
***********************************************************************         
*                                                                     *         
*        PRODUCT TABLE DSECT                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* PRODUCT AND ESTIMATE TABLES FOR GF CODES                                      
*                                                                               
         DC    C'** PRWRI15 PRD TABLE **'                                       
PRDTAB   DS    255XL(PRTENTL)                                                   
*                                                                               
         TITLE 'T40515 - KRTAPE CREATION - ESTTAB'                              
***********************************************************************         
*                                                                     *         
*        ESTIMATE TABLE DSECT                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DC    C'** PRWRI15 EST TABLE **'                                       
ESTTAB   DS    999XL(ETBENTL)                                                   
*                                                                               
*                                                                               
*              ENTRIES INCLUDE TEST,FORMULA,EFF DATE                            
*                                                                               
FORMTABD DSECT                                                                  
*              ENTRIES INCLUDE TEST,FORMULA,EFF DATE                            
FORMTST  DS    CL1     TEST ESTIMATE INDICATOR                                  
FORMFORM DS    CL5     INCLUDE TESTESTIMATE FORMULA                             
FORMEFFD DS    XL3     INCLUDE TESTEFFECTIVE DATE                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'232PRWRI15   11/08/12'                                      
         END                                                                    
