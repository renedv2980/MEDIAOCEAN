*          DATA SET SPWRI35    AT LEVEL 026 AS OF 11/08/12                      
*PHASE T20435A,*                                                                
*INCLUDE SPBVAL                                                                 
*        TITLE 'T20435 - KRTAPE CREATION'                                       
         TITLE 'T20435 - KRTAPE CREATION - PROGRAM CHANGE LOG'                  
*                                                                               
* BPLA  10/12    MODIFY EDICT HEADER FOR KRAFT SPLIT BILLING                    
*                                                                               
* BPLA  08/12    CHANGE FILE NAME BASED ON USER ID                              
*                                                                               
* BOBY  2/03/02  REVAMP FOR NEW KRAFT TAPE REQUIREMENTS'                        
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INIT'                                
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T20435   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20435,RR=RE                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         LAY   RA,WORKSTG          ESTABLISH WORKING STORAGE                    
         USING WORKSTG,RA                                                       
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   INITTABX                                                         
*                                                                               
*        GET A(DYNALLOC)                                                        
*                                                                               
         L     RF,TWADCONS               FROM DDGENTWA                          
         MVC   VDYNALLO,TDYNALLO-TWADCOND(RF)   V(DYNALLOC)                     
*                                                                               
*        ALTER FILE NAMES FOR AGENCIES                                          
*                                                                               
         MVC   DSNKRINV+13(2),TWAAGY                                            
         MVC   DSNKREST+13(2),TWAAGY                                            
*                                                                               
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
*        GET A(AREA) PRESERVED BETWEEN REQUESTS                                 
*                                                                               
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
*                                                                               
         L     RF,TSPFUSER                                                      
         LA    RF,256(RF)         SOMEONE IS USING 1ST 256 BYTES                
         ST    RF,ASPFUSER                                                      
*                                                                               
         DROP  R1                                                               
*                                                                               
         L     R1,ASPFUSER                                                      
*                                                                               
*        DETERMINE ADDRESSES OF DCBS STORED IN PRESERVED AREA                   
*                                                                               
         LA    RF,KRINVFL-SPFAREA(R1)                                           
         ST    RF,AKRINVFL          NEW DCB ADDRESS                             
         LA    RF,KRCINVFL-SPFAREA(R1)                                          
         ST    RF,AKRCINVF         NEW DCB ADDRESS                              
         LA    RF,KRESTFL-SPFAREA(R1)                                           
         ST    RF,AKRESTFL          NEW DCB ADDRESS                             
*                                                                               
INITTABX DS    0H                                                               
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - CKRPMODE'                            
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
         CLI   RPMODE,RPRNFRST     RUN FIRST                                    
         BE    FRST                                                             
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
         TITLE 'T20435 - KRTAPE CREATION - INIT'                                
***********************************************************************         
*                                                                     *         
*        REPORT INITIALIZATION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIT     DS    0H                                                               
*                                                                               
         MVI   LASTHEAD,9         SET NUMBER OF HEADLINES                       
         MVI   MYFIRSTH,9         SET NUMBER OF HEADLINES                       
*                                                                               
         CLI   TWAFIRST,0          IF FIRST REQUEST                             
         BNE   INIT1                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   INOCLEAR                                                         
*                                                                               
*        ON START OF RUN MOVE DCBS TO SPFUSER AREA                              
*                                                                               
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
*                                                                               
         MVC   SPFTITLE,TITLE                                                   
         MVC   SPFTTLSB,SUBTITLE                                                
         MVC   SPFSPLID,SPOOLID                                                 
*                                                                               
*        INVOICE BATCH                                                          
*                                                                               
         ZAP   SPIBTCTR,=P'0'      INIT INVOICE BATCH RECORD COUNTER            
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
         MVI   SPIVBTID+05,C' '     CLEAR EXTRA BYTE                            
*                                    RETURNED IN R0                             
         ZAP   DUB,=P'0'           INIT PACKED FIELD                            
         STCM  R0,15,DUB+3         SAVE P(HHMMSSSS0C)                           
*                                                                               
         AP    DUB,=P'60000000'   ADD SIX HOURS                                 
*                                                                               
         UNPK  SPIVBTID+5(9),DUB+3(5)   PRINT TIME                              
         MVI   SPIVBTID+13,C' '     CLEAR EXTRA BYTE                            
*                                                                               
         ZAP   SPIVNET,=P'0'       INIT MONEY ACCUMULATORS                      
         ZAP   SPIVCOM,=P'0'                                                    
         ZAP   SPIVADJ,=P'0'                                                    
         ZAP   SPIVGRS,=P'0'                                                    
*                                                                               
*        ESTIMATE BATCH                                                         
*                                                                               
         ZAP   SPEBTCTR,=P'0'      INIT BATCH RECORD COUNTER                    
         MVI   SPEHEAD,C'H'        INDICATE HEADER TO BE PRINTED                
         MVC   SPESBTID,SPACES     INIT BATCH ID                                
*                                                                               
*        CREATE ESTIMATE BATCH ID OF DATE AND TIME                              
*                                                                               
         TIME  DEC                 R0 - TIME PUS 0HHMMSS.SS                     
*                                  R1 - DATE  P   0CYYDDDF                      
         ST    R1,FULL             DATE                                         
         OI    FULL+3,X'0F'        FORCE SIGN                                   
         UNPK  SPESBTID(5),FULL+1(3)  DISPLAY DATE                              
         MVI   SPESBTID+05,C' '     CLEAR EXTRA BYTE                            
*                                    RETURNED IN R0                             
         ZAP   DUB,=P'0'           INIT PACKED FIELD                            
         STCM  R0,15,DUB+3         SAVE P(HHMMSSSS0C)                           
*                                                                               
         AP    DUB,=P'60000000'   ADD SIX HOURS                                 
         AP    DUB,=P'00000010'   ADD ONE TO FORCE NEW ID                       
*                                                                               
         UNPK  SPESBTID+5(9),DUB+3(5)   PRINT TIME                              
         MVI   SPESBTID+13,C' '     CLEAR EXTRA BYTE                            
*                                                                               
         ZAP   SPESNET,=P'0'       INIT MONEY ACCUMULATORS                      
         ZAP   SPESCOM,=P'0'                                                    
         ZAP   SPESADJ,=P'0'                                                    
         ZAP   SPESGRS,=P'0'                                                    
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
         BE    INITDYNX                                                         
*                                                                               
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
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
*        INITIALIZE TABLE AREAS                                                 
*                                                                               
*        OFF-LINE - GET BUFFER                                                  
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(((BUFFERL+7)/8)*8)  BUFFER FOR TABLES                        
*                                                                               
         OC    ABUFFER,ABUFFER     CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,ABUFFER          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     RF,ABUFFER                                                       
         USING BUFFERD,RF          ESTABLISH BUFFER                             
*                                                                               
         L     R1,=AL4(PRDTABC-BUFFERD)                                         
         LA    R1,BUFFERD(R1)                                                   
*                                                                               
         MVC   0(32,R1),=CL32'** SPWRI35  PRD TABLE    ***'                     
         LA    R1,32(R1)                                                        
         ST    R1,APRDTAB                                                       
*                                                                               
         L     R1,=AL4(ESTTABC-BUFFERD)       POINT TO ESTIMATE TABLE           
         LA    R1,BUFFERD(R1)                                                   
*                                                                               
         MVC   0(32,R1),=CL32'** SPWRI35  EST TABLE    ***'                     
         LA    R1,32(R1)                                                        
         ST    R1,AESTTAB                                                       
*                                                                               
         DROP  RF                                                               
*                                                                               
         BRAS  RE,INIESTID         INIT ESTIMATE ID TABLE                       
*                                                                               
         OI    SBQSKIP,SBQSKGL     SKIP READING GOALS                           
         OI    SBQSKIP,SBQSKBIL    SKIP READING STATION BILL RECORDS            
         OI    SBQREAD,SBQRDBH     FORCE BILL HEADER READS                      
         LHI   RF,SBQREAD2-SYSD                                                 
         LA    RF,SYSD(RF)                                                      
         OI    0(RF),SBQRD2BH      FORCE BILL HEADER READS EARLY                
*                                                                               
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         OI    SBQPER,SBQPMN       MONTHS                                       
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    SBQRDOPT,SBQROMAB   READ MANUAL BILLING                          
         OI    DATAIND2,DIEFFCST   GET EFFECTIVE BUY COST                       
         OI    DATAIND2,DIEST      ESTIMATE                                     
         XC    LEVELS,LEVELS                                                    
*                                                                               
         CLI   OFFLINE,C'Y'        IF ONLINE BYPASS                             
         BNE   INOCLEAR                                                         
*                                                                               
         XC    PROGPROX,PROGPROX   CLEAR PROFILE                                
*                                                                               
*        MUST READ SB1X PROFILE FOR                                             
*        INV MTH DISPLAY                                                        
*                                                                               
         XC    WORK,WORK                                          L05           
         MVC   WORK(4),=C'SB1X'                                   L05           
         NI    WORK,X'BF'                   LOWER CASE            L05           
         MVC   WORK+4(2),SBQAGY                                   L05           
         MVC   WORK+6(1),SBMED                                   L05            
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROGPROX,DATAMGR             L05               
*                                                                               
         XC    MONTABLE,MONTABLE   INITIALIZE MONTH TABLE                       
         XC    SVPRD,SVPRD                                                      
         XC    SVPCD,SVPCD                                                      
         XC    SVEST,SVEST                                                      
*                                                                               
         L     RE,APRDTAB          INIT  PRODUCT AND EST TABLES                 
         XC    0(PRTENTL,RE),0(RE)                                              
*                                                                               
         L     RE,AESTTAB                                                       
         XC    0(ETBENTL,RE),0(RE)                                              
*                                                                               
         MVI   IVREC,C' '          INIT INVOICE  RECORD                         
         MVC   IVREC+1(245),IVREC                                               
*                                                                               
         MVI   ESREC,C' '          INIT ESTIMATE RECORD                         
         MVC   ESREC+1(235),ESREC                                               
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
INITX    B     XIT                                                              
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - VALID'                               
***********************************************************************         
*                                                                     *         
*        REQUEST VALIDATION                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALID    DS    0H                                                               
*                                                                               
         CLI   SBQBPRD,X'FF'       TEST PRD=POL REQUEST                         
         BNE   *+12                NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
*                                                                               
         LA    R2,GFTMONH          VALIDATE BILLING MONTH                       
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
         CLC   =AL2(SP#RFPBD),RFPVSYME+1 MUST BE BMON SYMBOLIC                  
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
         GOTO1 DATCON,DMCB,DUB,(2,SBQBILST)    MONTH OF BILLING                 
*                                                                               
         MVC   DUB+4(2),=C'31'                                                  
*                                                                               
         GOTO1 (RF),(R1),DUB,(2,SBQBILEN)      START/END DATES                  
*                                                                               
         MVC   DUB+4(2),=C'01'                                                  
*                                                                               
         GOTO1 (RF),(R1),DUB,(X'20',WORK)      REQUEST CARD FORMAT              
*                                                                               
         MVC   BILLMON(4),WORK                                                  
*                                                                               
         GOTO1 DATCON,DMCB,DUB,(3,WORK) BINARY                                  
*                                                                               
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         IC    RF,DUB+1            YEAR AS DECIMAL                              
         IC    RE,WORK+1           MONTH                                        
         SLL   RF,4                YEAR TO HIGH ORDER NYBBLE                    
         OR    RF,RE               YM IN RF                                     
*                                                                               
         STC   RF,SBQYMST          BILL HEADER MONTH FILTERS                    
         STC   RF,SBQYMEND                                                      
*                                                                               
         MVC   SUBTITLE(12),=C'PERIOD FROM '                                    
         GOTO1 DATCON,DMCB,(3,SBQBILST),(6,SUBTITLE+12)                         
         MVC   SUBTITLE+19(3),=C'TO '                                           
         GOTO1 DATCON,DMCB,(3,SBQBILEN),(6,SUBTITLE+22)                         
*                                                                               
         MVI   LASTHEAD,9                                                       
         MVI   MYFIRSTH,9                                                       
*                                                                               
         OC    SUBTITLE,SPACES                                                  
         GOTO1 CENTER,DMCB,SUBTITLE,36                                          
*                                                                               
VBMONX   DS    0H                                                               
*                                                                               
         MVI   WIDTHOPT,C'W'      MAKE THIS A WIDE REPORT                       
*                                                                               
         MVI   SUPTAP,C'N'                                                      
*                                                                               
         LA    R2,GFTTAPH          OPTION TO SUPPRESS TAPE                      
         CLI   5(R2),0                                                          
         BE    VBTAP1                                                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   SUPTAP,WORK                                                      
*                                                                               
         CLI   SUPTAP,C'Y'                                                      
         BE    VBTAPX                                                           
*                                                                               
         CLI   SUPTAP,C'T'         TAPE BUT NO EDICT                            
         BE    VBTAP1                                                           
*                                                                               
         CLI   SUPTAP,C'N'                                                      
         BNE   ININVE                                                           
*                                                                               
VBTAP1   DS    0H                                                               
*                                                                               
         CLC   =C'SOON',CONWHEN      MUST NOT BE SOON                           
         BE    ININVE                                                           
*                                                                               
VBTAPX   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         LHI   RF,SBQREAD2-SYSD    POINT TO READ OPTIONS                        
         LA    RF,SYSD(RF)                                                      
         OI    0(RF),SBQRD2BL      READ BILLS BEFORE BUYS                       
*                                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ININVE'                              
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
         TITLE 'T20435 - KRTAPE CREATION - FRST'                                
***********************************************************************         
*                                                                     *         
*        FIRST OF RUN                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FRST     DS    0H                                                               
*                                                                               
*        RETURN NON-ZERO CC TO STOP CLEARING 256 BYTES OF SPFUSER               
*                                                                               
         LTR   RB,RB                                                            
*                                                                               
FRSTX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INPUT'                               
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
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BNE   INCLX                                                            
*                                                                               
         MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         MVI   GLFHEADL,9          SET FIRST LINE FOR HEADLINES                 
         MVI   GLLHEADL,14         SET LAST  LINE FOR HEADLINES                 
*                                                                               
         B     INCLX                                                            
*                                                                               
INCLX    DS    0H                                                               
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INBILL'                              
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
         CLI   SBMODE,SBPROCBH     IF PROCESSING BILL HEADER RECORDS            
         BNE   INBILLN                                                          
*                                                                               
         XC    BILLST,BILLST          SET FOR ALL BILL RECORDS                  
         MVC   BILLEND,=X'FFFF'                                                 
*                                                                               
         B     INMNTB              GO BUILD MONTH TABLE                         
*                                                                               
INBILLN  DS    0H                                                               
*                                                                               
*        HANDLE BUY RECORDS RECORDS                                             
*                                                                               
         CLI   SBMODE,SBPROCSP     BUY RECORDS                                  
         BNE   INPUTX                                                           
*                                                                               
         L     R5,SBACURCH         A(BUY RECORD CHUNK)                          
         USING SCHUNKD,R5                                                       
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INMNTB'                              
***********************************************************************         
*                                                                     *         
*        MONTH TABLE                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                  MONTABLE IS A TABLE OF BINARY                
*                                  YY/MM FROM REQ START TO REQ END              
*                                                                               
INMNTB   DS    0H                                                               
*                                                                               
         OC    MONTABLE,MONTABLE   SKIP IF MONTH TABLE SET                      
         BNZ   INMNTBX                                                          
*                                                                               
         L     R2,AMONTHS                                                       
         LA    R3,MONTABLE                                                      
         LHI   R0,24                                                            
*                                                                               
INMNTBLP DS    0H                                                               
*                                                                               
         OC    0(4,R2),0(R2)                                                    
         BZ    INMNTBDN                                                         
*                                                                               
         MVC   0(4,R3),0(R2)                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),DUB                                        
*                                                                               
         MVC   4(4,R3),DUB                                                      
*                                                                               
INMNTBCN DS    0H                                                               
*                                                                               
         LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,INMNTBLP                                                      
*                                                                               
INMNTBDN DS    0H                                                               
*                                                                               
         MVC   0(2,R3),=X'FFFF'    SET END OF TABLE                             
*                                                                               
INMNTBX  DS    0H                                                               
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INPRD'                               
***********************************************************************         
*                                                                     *         
*        CHANGE IN PRODUCT AND/OR ESTIMATE                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INPRD    DS    0H                                                               
*                                                                               
INPRDX   DS    0H                                                               
*                                                                               
INPUTX   B     XIT                                                              
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - DRHOOK'                              
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
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
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
         TITLE 'T20435 - KRTAPE CREATION - RESOLVE'                             
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
         TITLE 'T20435 - KRTAPE CREATION - DRVINIT'                             
***********************************************************************         
*                                                                     *         
*        DRIVER INITIALIZATION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         OI    GLINDS,GLPALTOT     PRINT ALL TOTALS                             
         MVI   GLOPTS+2,3          REPORT 3 = TAPE RECORD MAP                   
         B     XIT                                                              
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - EXEC'                                
***********************************************************************         
*                                                                     *         
*        DRIVER HOOK TO EXECUTE ROUTINES                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R5,SBACURCH         A(BUY RECORD CHUNK)                          
*                                                                               
         USING SCHUNKD,R5                                                       
*                                                                               
         CLI   REJECT,C'D'         RESET REJECTION SWITCHES                     
         BNE   *+8                                                              
         MVI   REJECT,C'N'                                                      
*                                                                               
         CLI   REJECTES,C'D'       RESET REJECTION SWITCHES                     
         BNE   *+8                                                              
         MVI   REJECTES,C'N'                                                    
*                                                                               
         CLI   MONEY,C'D'         RESET REJECTION SWITCHES                      
         BNE   *+8                                                              
         MVI   MONEY,C'N'                                                       
*                                                                               
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC2                                                            
*                                                                               
         MVI   INDATA,1           ALL DATA IS SIGNIFICANT                       
         L     R1,GLADTENT                                                      
*                                                                               
         CLI   DRINLEV-DRIND(R1),1 TEST LEVEL 1                                 
         BH    EXEC2                                                            
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
         TITLE 'T20435 - KRTAPE CREATION - PUTSRT'                              
***********************************************************************         
*                                                                     *         
*        DRIVER ABOUT TO PUT TO SORT                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUTSRT   DS    0H                                                               
*                                                                               
         CLI   MONEY,C'Y'          IF $0                                        
         BE    *+8                                                              
         CLI   MONEY,C'D'          IF MONEY SWITCH IS ON                        
         BNE   *+12                                                             
         MVI   GLHOOK,GLDONT          YES-TELL DRIVER TO REJECT                 
         MVI   MONEY,C'D'             INDICATE A DELETION DONE                  
*                                                                               
         CLI   REJECT,C'Y'         IF REJECT SWITCH IS ON                       
         BE    *+8                                                              
         CLI   REJECT,C'D'         IF REJECT SWITCH IS ON                       
         BNE   *+12                                                             
         MVI   GLHOOK,GLDONT          DON'T PUT RECORD TO SORT                  
         MVI   REJECT,C'D'            INDICATE A DELETION DONE                  
*                                                                               
         CLI   REJECTES,C'Y'       TEST RECORD REJECTED                         
         BE    *+8                                                              
         CLI   REJECTES,C'D'       IF REJECT SWITCH IS ON                       
         BNE   *+12                                                             
         MVI   GLHOOK,GLDONT           YES-TELL DRIVER TO REJECT                
         MVI   REJECTES,C'D'           INDICATE A DELETION DONE                 
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
         TITLE 'T20435 - KRTAPE CREATION - HEADHOOK'                            
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
         XC    70(26,R2),70(R2)                                                 
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
         TITLE 'T20435 - KRTAPE CREATION - PRINT'                               
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
         TITLE 'T20435 - KRTAPE CREATION - FINAL'                               
***********************************************************************         
*                                                                     *         
*        FINAL HOOK                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FINAL    DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   FINALX                                                           
*                                                                               
         L     R0,ASPFUSER         SAVE INTER-REQUEST VALUES                    
         LA    R1,SPFAREAL                                                      
         LA    RE,SPFAREA                                                       
         LR    RF,R1                                                            
*                                                                               
         MVC   SPFTITLE,TITLE                                                   
         MVC   SPFTTLSB,SUBTITLE                                                
         MVC   SPFSPLID,SPOOLID                                                 
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
FINAL1   DS    0H                                                               
*                                                                               
*        OFF-LINE - FREE BUFFER                                                 
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(((BUFFERL+7)/8)*8)  BUFFER FOR TABLES                        
*                                                                               
         LA    R4,ABUFFER          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    ABUFFER,ABUFFER     RESET ADDRESS                                
*                                                                               
FINALX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
PROGPROX DC    XL16'0'                                                          
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - GETPRD'                              
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
         CLI   PRTPCD,0            DONE AT END OF TABLE                         
         BE    GETPRDDN                                                         
*                                                                               
         CLC   PRTPCD,SVPCD        MATCH ON PRODUCT                             
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
         MVC   SAVEKEY,KEY        SAVE CUURENT KEY                              
*                                                                               
         XC    KEY,KEY             READ GF ESTIMATE RECORD                      
         LA    R5,KEY                 WITH ESTIMATE=0                           
         USING PGESTD,R5                                                        
*                                                                               
         MVI   PGKRID,PGKNDIRQ     SET RECID                                    
         MVI   PGKSID,PGKNDISQ     SET SUB ID                                   
         MVC   PGKAM,SBBAGYMD      SET AGENCY/MEDIA                             
         MVC   PGKCLT,SBBCLT       SET CLIENT                                   
         MVC   PGKPRD,SVPRD        SET PRODUCT                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PGKEST-PGKEY),KEYSAVE   SKIP IF NO RECORD                    
         BNE   GETPRDER                                                         
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT AIO                             
         L     R5,AIO2             POINT TO FOUND RECORD                        
         ST    R5,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT AIO                          
*                                                                               
         MVC   PRTPCD,SVPCD        SET PRODUCT CODE IN TABLE                    
*                                                                               
         LA    R5,PGKEDQ(R5)       SCAN ELEMENTS FOR                            
         SR    R0,R0               DIVISION/BRAND AND PRODUCT CODE              
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
         USING PGSTELMD,R5                                                      
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
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),SPACES      SPACE FILL                                   
*                                                                               
*        RIGHT JUSTIFY PRDID WITH LEADING ZEROS                                 
*                                                                               
         CLC   PGSTNAME,QPRDID    SKIP IF NOT PRODUCT ID                        
         BNE   GTPFLDCN                                                         
*                                                                               
*        FIELD ENDS AT FIRST NON-NUMERIC OR 10 BYTES IF FULL                    
*                                                                               
         IC    RF,8(R1)           NUMBER OF BYTES IN FIELD                      
         LA    RE,0(R4)           START OF DATA IN FIELD                        
*                                                                               
GTPPIDLP DS    0H                                                               
*                                                                               
         CLI   0(RE),C'0'         FIELD ENDS AT 1ST NON-NUMERIC                 
         BL    GTPPIDFD                                                         
*                                                                               
GTPPIDCN DS    0H                                                               
*                                                                               
         AHI   RE,1               NEXT BYTE                                     
         BCT   RF,GTPPIDLP                                                      
*                                                                               
GTPPIDDN DS    0H                                                               
*                                                                               
GTPPIDFD DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
         IC    RE,8(R1)           MAX FIELD LENGTH                              
         SR    RE,RF              TRUE FIELD LENGTH                             
         BNP   GETPRDER           ERROR IF NO DATA                              
*                                                                               
         BCTR  RE,0               DECREMENT FOR EXECUTE                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)        PACK NUMBER                                   
*                                                                               
         OI    DUB+7,X'0F'        FORCE SIGN                                    
*                                                                               
         IC    RE,8(R1)           MAX FIELD LENGTH                              
         BCTR  RE,0               DECREMENT FOR EXECUTE                         
         SLL   RE,4               MOVE TO LEFT NYBBLE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R4),DUB        ADD IN LEADING ZEROS                          
*                                                                               
GTPPIDX  DS    0H                                                               
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
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETPRDFD DS    0H                  PROD FOUND IN TABLE                          
*                                                                               
         ST    R6,APRTENT          SAVE TABLE ENTRY POINTER                     
*                                                                               
         CLC   PRTPRDID,SPACES    PRODUCT ID REQUIRED                           
         BNH   GETPRDER                                                         
*                                                                               
         CLC   PRTCRAGY,SPACES    CREATIVE AGENCY REQUIRED                      
         BNH   GETPRDER                                                         
*                                                                               
         B     GETPRDX                                                          
*                                                                               
GETPRDER DS    0H                                                               
*                                                                               
         OI    ERRCD,ERRPRD        INDICATE PRODUCT ERROR                       
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETPRDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - LST'                                 
***********************************************************************         
*                                                                     *         
*        RUNLAST                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LST      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   LSTX                                                             
*                                                                               
         L     RE,ASPFUSER         RESTORE SAVED VALUES                         
         LA    RF,SPFAREAL                                                      
         LA    R0,SPFAREA                                                       
*                                                                               
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING TAPE                      
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
         MVC   AGY2CH,2(R1)        SAVE AGENCY ID                               
*                                                                               
         CLI   KRINVSW,C'O'       SKIP IF FILE NOT OPEN                         
         BNE   LSTIVX                                                           
*                                                                               
*        PUT OUT BATCH CONTROL RECORD                                           
*                                                                               
         CLI   SPIHEAD,C'H'       SKIP IF HEADER STILL TO BE WRITTEN            
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
         CLC   =C'YN',TWAAGY       IF AGENCY IS YNR                             
         BNE   *+14                                                             
         MVC   IVCCOM,SPACES          PRINT SPACES                              
         B     LSTCOMX                SKIP                                      
*                                                                               
         EDIT  (P8,SPIVCOM),IVCCOM,2,FLOAT=-   PUT IN CONTROL RECORD            
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
         UNPK  IVCRECCT,SPIBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         LA    R6,IVCREC           POINT TO OUTPUT BATCH CONTROL REC            
*                                                                               
         PUT   (R3),(R6)           PUT TO INVOICE FILE                          
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
         UNPK  IBTRECCT,SPIBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         LA    R6,IBTREC           PUT TRAILER RECORD TO TAPE                   
*                                                                               
         PUT   (R3),(R6)                                                        
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
         L     R3,AKRINVFL                                                      
*                                                                               
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         CLOSE ((R3))              CLOSE INVOICE DISK FILE                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   GFTTAP,C'T'        SKIP IF NOT EDICT                             
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
         MVC   REMOTJID,=C'SKR'                                                 
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
         MVI   KRFTYPE,C' '        CLEAR KRAFT FILE TYPE                        
*                                  G= GROCERY, S= SNACK                         
         ICM   R2,15,TWAMASTC      POINT TO MASTC                               
         USING MASTD,R2                                                         
         CLC   MCORIGID,=H'10273'    SEE IF MVCTOK (KRAFT GROCERIES)            
         BNE   LSTIV5                                                           
         MVI   KRFTYPE,C'G'          GROCERY FILE                               
         MVC   15(7,R1),=C'MVWC009'  SPOT GROCERY INVOICE FILE                  
         B     LSTIV15                                                          
*                                                                               
LSTIV5   CLC   MCORIGID,=H'16377'    SEE IF MVCMON (KRAFT SNACKS)               
         BNE   LSTIV10                                                          
         MVC   15(7,R1),=C'MVWC003'  SPOT SNACK INVOICE FILE                    
         MVI   KRFTYPE,C'S'          SNACK FILE                                 
         B     LSTIV15                                                          
         DROP  R2                                                               
*                                                                               
LSTIV10  DS    0H                                                               
*                                                                               
         MVC   15(2,R1),AGY2CH     AGENCY ID                                    
         MVC   17(3,R1),=C'INV'    FILE TYPE                                    
         MVC   20(1,R1),=C'S'      MEDIA                                        
         MVC   21(2,R1),TWAAGY     AGENCY ALPHA                                 
*                                                                               
LSTIV15  MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   37(R1),C'D'         THIS IS A DATASET                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS SKR  TRN'                                      
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
         MVC   P+19(2),AGY2CH      AGENCY ID                                    
*                                                                               
         MVC   P+21(3),=C'INV'     FILE TYPE                                    
         MVC   P+24(1),=C'S'       MEDIA                                        
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
*        PUT OUT ESTIMATE CONTROL RECORD                                        
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
         EDIT  (P8,SPESCOM),ESCCOM,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   ESCCOM+L'ESCCOM-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESCCOM+L'ESCCOM-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (P8,SPESADJ),ESCADJ,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   ESCADJ+L'ESCADJ-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESCADJ+L'ESCADJ-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (P8,SPESGRS),ESCGRS,2,FLOAT=-   PUT IN CONTROL RECORD            
*                                                                               
         CLC   ESCGRS+L'ESCGRS-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESCGRS+L'ESCGRS-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         OI    SPEBTCTR+7,X'0F'    FORCE SIGN                                   
         UNPK  ESCRECCT,SPEBTCTR   PRINT WITH LEADING ZEROS                     
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCBS                    
         LA    R6,ESCREC           POINT TO OUTPUT BATCH CONTROL REC            
*                                                                               
         PUT   (R3),(R6)           PUT TO ESTIMATE FILE                         
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
         UNPK  ESTRECCT,SPEBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         LA    R6,ESTREC           PUT TRAILER RECORD TO TAPE                   
*                                                                               
         PUT   (R3),(R6)                                                        
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
         L     R3,AKRESTFL                                                      
         CLOSE ((R3))              CLOSE TEMP DISK FILE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   GFTTAP,C'T'        SKIP IF NOT EDICT                             
         BE    LSTESX                                                           
*                                                                               
*        GENERATE EDICT PQ ENTRY                                                
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
         MVC   15(7,R1),=C'MVWC010'  SPOT GROCERY ESTIMATE FILE                 
         B     LSTES15                                                          
*                                                                               
LSTES5   CLC   MCORIGID,=H'16377'    SEE IF MVCMON (KRAFT SNACKS)               
         BNE   LSTES10                                                          
         MVC   15(7,R1),=C'MVWC004'  SPOT SNACK ESTIMATE FILE                   
         B     LSTES15                                                          
         DROP  R2                                                               
*                                                                               
LSTES10  DS    0H                                                               
*                                                                               
*                                                                               
         MVC   15(2,R1),AGY2CH     AGENCY ID                                    
         MVC   17(3,R1),=C'EST'    FILE TYPE                                    
         MVC   20(1,R1),=C'S'      MEDIA                                        
         MVC   21(2,R1),TWAAGY     AGENCY ALPHA                                 
*                                                                               
LSTES15  MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   37(R1),C'D'         THIS IS A DATASET                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS SKR  TRN'                                      
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
         MVC   P+19(2),AGY2CH      AGENCY ID                                    
*                                                                               
         MVC   P+21(3),=C'EST'     FILE TYPE                                    
         MVC   P+24(1),=C'S'       MEDIA                                        
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
LSTESX   DS    0H                                                               
*                                                                               
LSTX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
KRFTYPE  DS    CL1                 KRAFT FILE TYPE                              
*                                  G=GROCERY, S= SNACK                          
AGY2CH   DC    CL2' '              AGENCY ID                                    
*                                                                               
*        AGENCY ID TABLE                                                        
*                                                                               
*        DS    C'2 CH ALPHA',CL2'AGY2CH'                                        
*                                                                               
AIDTAB   DS    0D                                                               
         DC    C'H9',C'MV'         MEDIAVEST                                    
AIDTABLQ EQU   *-AIDTAB            LENGTH OF TABLE ENTRY                        
         DC    C'O0',C'MV'         MEDIAVEST TORONTO                            
         DC    C'YN',C'BR'         BRAVO                                        
         DC    C'JT',C'JW'         JWT TORONTO                                  
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - GETEST'                              
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
         CLC   ETBEST,SBBEST       MATCH ON ESTIMATE                            
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
         MVC   SAVEKEY,KEY        SAVE CUURENT KEY                              
*                                                                               
         XC    KEY,KEY             READ GF ESTIMATE RECORD                      
         LA    R5,KEY                 WITH ESTIMATE=0                           
         USING PGESTD,R5                                                        
*                                                                               
         MVI   PGKRID,PGKNDIRQ     SET RECID                                    
         MVI   PGKSID,PGKNDISQ     SET SUB ID                                   
         MVC   PGKAM,SBBAGYMD      SET AGENCY/MEDIA                             
         MVC   PGKCLT,SBBCLT       SET CLIENT                                   
         MVC   PGKPRD,=C'POL'      SET PRODUCT TO POOL                          
         MVC   PGKEST,SBBEST       SET ESTIMATE                                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PGKEST-PGKEY+L'PGKEST),KEYSAVE SKIP IF NONE                  
         BNE   GETESTER                                                         
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT AIO                             
         L     R5,AIO2             POINT TO FOUND RECORD                        
         ST    R5,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT AIO                          
*                                                                               
         MVC   ETBEST,SBBEST       SET ESTIMATE CODE IN TABLE ENTRY             
*                                                                               
         LA    R5,PGKEDQ(R5)       POINT TO FIRST ENTRY IN RECORD               
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
         USING PGSTELMD,R5                                                      
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
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETESTFD DS    0H                  PROD FOUND IN TABLE                          
*                                                                               
         ST    R6,AETBENT          SAVE TABLE ENTRY POINTER                     
*                                                                               
         B     GETESTX                                                          
*                                                                               
GETESTER DS    0H                                                               
*                                                                               
         OI    ERRCD,ERREST        INDICATE ESTIMATE ERROR                      
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
GETESTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - RESOLVER'                            
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
         TITLE 'T20435 - KRTAPE CREATION - RTNLIST'                             
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
         DC    CL8'IRECID1 ',A(IRECID1)                                         
         DC    CL8'IRECID2 ',A(IRECID2)                                         
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
         DC    CL8'IINVDT  ',A(IINVDT)                                          
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
         DC    CL8'INET    ',A(INET)                                            
         DC    CL8'ONET    ',A(ONET)                                            
*                                                                               
         DC    CL8'ICOM    ',A(ICOM)                                            
         DC    CL8'OCOM    ',A(OCOM)                                            
*                                                                               
         DC    CL8'IADJ    ',A(IADJ)                                            
         DC    CL8'OADJ    ',A(OADJ)                                            
*                                                                               
         DC    CL8'IGRS    ',A(IGRS)                                            
         DC    CL8'OGRS    ',A(OGRS)                                            
*                                                                               
         DC    CL8'IBLLD   ',A(IBLLD)                                           
         DC    CL8'OBLLD   ',A(OBLLD)                                           
*                                                                               
         DC    CL8'ICRAGY  ',A(ICRAGY)                                          
         DC    CL8'OCRAGY  ',A(OCRAGY)                                          
*                                                                               
         DC    CL8'IDEAL#  ',A(IDEAL#)                                          
         DC    CL8'ODEAL#  ',A(ODEAL#)                                          
*                                                                               
         DC    CL8'OFILLER ',A(OFILLER)                                         
*                                                                               
         DC    CL8'IGST    ',A(IGST)                                            
         DC    CL8'OGST    ',A(OGST)                                            
*                                                                               
         DC    CL8'IPST    ',A(IPST)                                            
         DC    CL8'OPST    ',A(OPST)                                            
*                                                                               
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
*                                                                               
         DC    CL8'LASTCOL ',A(LASTCOL)                                         
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INIESTID'                            
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
         L     RF,ABUFFER          POINT TO BUFFER                              
         USING BUFFERD,RF          ESTABLISH BUFFER                             
*                                                                               
         L     R3,=AL4(ESTIDTB-BUFFERD)      POINT TO START OF TABLE            
         LA    R3,BUFFERD(R3)                                                   
*                                                                               
         LA    R4,L'IVDESTID       LENGTH OF ENTRY IN TABLE                     
         LR    R5,R3               SET TABLE AS EMPTY                           
         AHI   R5,-1                                                            
*                                                                               
         STM   R3,R5,EITBBXLE      SAVE STARTING BXLE REGISTERS                 
*                                                                               
         XC    0(L'IVDESTID,R3),0(R3) INIT FIRST ENTRY                          
*                                                                               
         DROP  RF                                                               
*                                                                               
INESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ADDESTID'                            
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
         CR    R5,R3               SKIP IF TABLE ENTRY                          
         BL    ADES10                                                           
*                                                                               
         CLC   0(L'IVDESTID,R3),0(R2) CHECK IF ALREADY IN TABLE                 
         BE    ADESX               YES                                          
         BXLE  R3,R4,*-10                                                       
*                                                                               
ADES10   DS    0H                                                               
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
         TITLE 'T20435 - KRTAPE CREATION - GETESTID'                            
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
         TITLE 'T20435 - KRTAPE CREATION - RECID1'                              
***********************************************************************         
*                                                                     *         
*        RECORD ID - C'1' - INVOICE  RECORD                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IRECID1  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'1'             SET RECORD ID                             
*                                                                               
         CLC   SBPRD,SVPRD        ON CHANGE IN PRODUCT                          
         BE    IRECPR1X                                                         
*                                                                               
         MVC   SVPRD,SBPRD        SAVE NEW PRODUCT                              
         MVC   SVPCD,SBBPRD       SAVE INTERNAL PRODUCT CODE                    
*                                                                               
*        BUILD ESTIMATE TABLES                                                  
*                                                                               
         L     RE,AESTTAB                                         L01           
         XC    0(ETBENTL,RE),0(RE)                                              
*                                                                               
         XC    SVBFEST,SVBFEST     FORCE CHANGE IN ESTIMATE                     
         XC    SVEST,SVEST         FORCE CHANGE IN ESTIMATE                     
*                                                                               
         BRAS  RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
*                                                                               
IRECPR1X DS    0H                                                               
*                                                                               
         BRAS  RE,GETEST           YES-GET GF CODES FOR ESTIMATE                
*                                                                               
         MVC   SVEST,SBBEST                                                     
*                                                                               
IRECID1X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - RECID2'                              
***********************************************************************         
*                                                                     *         
*        RECORD ID - C'2'  - ESTIMATE RECORD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IRECID2  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'2'             USE DIFFERENT RECORD ID                   
*                                                                               
         CLI   SBMODE,SBPROCSP     IF PROCESSING BUYS                           
         BNE   IRECID2X                                                         
*                                                                               
         ICM   R5,15,SBACURCH     A(BUY RECORD CHUNK)                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SCHUNKD,R5                                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,SCPRD1        GET PRODUCT INTERNAL CODE                     
         BZ    IRECID2X            UNALLOCATED                                  
         BCTR  RE,0               DECREMENT FOR INDEXING                        
         MHI   RE,PRDBUFFL        DISPLACEMENT TO PRD'S ENTRY IN BUFF           
         ICM   R1,15,SBAPRDBF     A(PRODUCT BUFFER)                             
         BZ    IRECPR2X           NO ENTRY IN BUFFER                            
*                                                                               
         AR    R1,RE              POINT TO PRD'S ENTRY IN BUFFER                
         USING PRDBUFFD,R1        ESTABLISH PRD BUFFER ENTRY                    
*                                                                               
         MVC   SVPRD,PBALPH       SAVE PRODUCT ALPHA CODE                       
         MVC   SVPCD,SCPRD1       SAVE CURRENT PRODUCT                          
*                                                                               
*        BUILD ESTIMATE TABLES                                                  
*                                                                               
         L     RE,AESTTAB                                         L01           
         XC    0(ETBENTL,RE),0(RE)                                              
*                                                                               
         XC    SVBFEST,SVBFEST     FORCE CHANGE IN ESTIMATE                     
         XC    SVEST,SVEST         FORCE CHANGE IN ESTIMATE                     
*                                                                               
         BRAS  RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
*                                                                               
IRECPR2X DS    0H                                                               
*                                                                               
         BRAS  RE,GETEST           YES-GET GF CODES FOR ESTIMATE                
*                                                                               
         MVC   SVEST,SBBEST                                                     
*                                                                               
IRECID2X DS    0H                                                               
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
         TITLE 'T20435 - KRTAPE CREATION - BLAGY'                               
***********************************************************************         
*                                                                     *         
*        BILLING AGENCY                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBLAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'ETBBLAGY,R2),SPACES   INIT                                   
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IBLAGYEX                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBBLAGY,R2),ETBBLAGY RETURN BILLING AGENCY CODE             
*                                                                               
         CLC   ETBBLAGY,SPACES     DONE IF FOUND                                
         BH    IBLAGYX                                                          
*                                                                               
IBLAGYEX DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IBLAGYXF                                                         
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTBLAGY,R2),PRTBLAGY RETURN BILLING AGENCY CODE             
*                                                                               
IBLAGYX  DS    0H                                                               
*                                                                               
         CLC   =C'EXCLUDE',0(R2)   DROP IF TO BE EXCLUDED                       
         BNE   IBLAGYXF                                                         
*                                                                               
         CLI   SBMODE,SBPROCBH    IF PROCESSING BILLS                           
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
         BNE   IBLAGYXA                                                         
*                                                                               
         MVI   REJECT,C'Y'         DROP FROM REPORT                             
*                                                                               
         B     IBLAGYXX                                                         
*                                                                               
IBLAGYXA DS    0H                                                               
*                                                                               
         MVI   REJECTES,C'Y'      REJECT ESTIMATE RECORD                        
*                                                                               
         B     IBLAGYXX                                                         
*                                                                               
IBLAGYXF DS    0H                                                               
*                                                                               
         CLC   0(L'PRTBLAGY,R2),SPACES   MUST HAVE BILLING AGENCY               
         BH    IBLAGYXX                                                         
*                                                                               
         OI    ERRCD,ERREST        INDICATE ESTIMATE ERROR                      
*                                                                               
         CLI   SBMODE,SBPROCBH    IF PROCESSING BILLS                           
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
         BNE   IBLAGYX1                                                         
*                                                                               
******   MVI   REJECT,C'Y'         DROP FROM REPORT                             
*                                                                               
         B     IBLAGYXX                                                         
*                                                                               
IBLAGYX1 DS    0H                                                               
*                                                                               
******   MVI   REJECTES,C'Y'      REJECT ESTIMATE RECORD                        
*                                                                               
IBLAGYXX DS    0H                                                               
*                                                                               
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
         MVC   IVHBLAGY,SPIBLAGY   PUT IN INVOICE RECORD                        
*                                                                               
         B     OBLAGYRX                                                         
*                                  ELSE                                         
OBLAGYIN DS    0H                                                               
*                                                                               
         MVC   SPEBLAGY,0(R2)         SAVE FOR CONTROL RECORD                   
         OC    SPEBLAGY,SPACES        SPACE FILL                                
*                                                                               
         MVC   ESHBLAGY,SPEBLAGY   PUT IN ESTIMATE RECORD                       
*                                                                               
OBLAGYRX DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(8,R3),0(R2)            PUT OUT BILLING AGENCY                  
         OC    0(8,R3),SPACES           SPACE FILL                              
*                                                                               
         B     OBLAGYX                                                          
*                                                                               
OBLAGYTL DS    0H                 TOTAL LINE                                    
*                                                                               
OBLAGTIX DS    0H                                                               
*                                                                               
OBLAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - BTID'                                
***********************************************************************         
*                                                                     *         
*        BATCH ID  -  CL10'BILLING AGENCY CODE'                       *         
*                     CL10'CREATION DATE'                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBTID    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SBMODE,SBPROCBH     IF PROCESSING BILL RECORDS                   
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
         BNE   IBTIDIVN                                                         
*                                                                               
         MVC   0(L'IVHBTID,R2),SPIVBTID   SET BATCH ID                          
*                                                                               
         B     IBTIDX                                                           
*                                                                               
IBTIDIVN DS    0H                 ELSE                                          
*                                                                               
         MVC   0(L'ESHBTID,R2),SPESBTID   SET BATCH ID                          
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
         CLC   IVHBTID,SPESBTID    IF ESTIMATE BATCH ID                         
         BNE   *+8                                                              
         MVI   MONEY,C'Y'             DROP RECORD - REPORT IS CONFUSED          
*                                                                               
         B     OBTIDRX                                                          
*                                  ELSE                                         
OBTIDIVN DS    0H                                                               
*                                                                               
         MVC   ESHBTID,0(R2)          PUT IN ESTIMATE RECORD                    
         OC    ESHBTID,SPACES         SPACE FILL                                
*                                                                               
         CLC   ESHBTID,SPIVBTID    IF INVOICE  BATCH ID                         
         BNE   *+8                                                              
         MVI   MONEY,C'Y'             DROP RECORD - REPORT IS CONFUSED          
*                                                                               
OBTIDRX  DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(9,R3),=C'BATCH ID:'    PRINT TITLE                             
         MVC   18(20,R3),0(R2)          PRINT BATCH ID                          
         OC    18(20,R3),SPACES         SPACE FILL                              
*                                                                               
         B     OBTIDX                                                           
*                                                                               
OBTIDTL  DS    0H                  TOTAL LINE                                   
*                                                                               
OBTIDTLX DS    0H                                                               
*                                                                               
OBTIDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - MED'                                 
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
         TITLE 'T20435 - KRTAPE CREATION - CLT'                                 
***********************************************************************         
*                                                                     *         
*        CLIENT                                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OCLT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVCLT,0(R2)         SAVE CLIENT CODE                             
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+22                                                             
         MVC   0(9,R3),=CL9'CLIENT  :'   PRINT HEADLINE TITLE                   
         MVC   10(3,R3),0(R2)            PRINT CLIENT                           
         OC    0(16,R3),SPACES           FORCE TO UPPERCASE                     
*                                                                               
OCLTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - PRD'                                 
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
         TITLE 'T20435 - KRTAPE CREATION - EST'                                 
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
         MVI   IVHINVNO,C'S'       SET MEDIA CODE TO SPOT                       
         MVC   IVHINVNO+1(1),SBMED SET SUB-MEDIA CODE                           
         MVI   IVHINVNO+2,C'-'     SET DASH                                     
*                                  YEAR WILL BE FILLED IN LATER                 
         MVI   IVHINVNO+7,C'-'     SET DASH                                     
         MVC   IVHINVNO+8(2),0(R2)   SET INVOICE NUMBER                         
         MVI   IVHINVNO+10,C'-'     SET DASH                                    
         MVC   IVHINVNO+11(4),2(R2)   SET INVOICE NUMBER                        
*                                                                               
         LTR   R3,R3              IF PRINTING                                   
         BZ    OINVX                                                            
*                                                                               
         MVC   0(15,R3),IVHINVNO     PRINT INVOICE NUMBER                       
*                                                                               
         CLC   8(07,R3),=7C' '                                                  
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,APINVNO          SAVE PRINT POSITION                          
*                                                                               
OINVX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRITER - BUILD INVOICE NUMBER - INVDT'                        
***********************************************************************         
*                                                                     *         
*        INVOICE DATE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IINVDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING STABELEM,R6                                                      
*                                                                               
         MVC   0(2,R2),STABBDT    RETURN DATE                                   
*                                                                               
IINVDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
OINVDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   IVHINVDT,SPACES    INIT TO SPACES                                
*                                                                               
         OC    0(6,R2),0(R2)      IF WE HAVE A DATE                             
         BZ    OINVDTX                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,0(R2)),(23,IVHINVDT)  INVOICE DATE                
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'IVHINVDT,R3),IVHINVDT  PRINT INVOICE DATE                    
*                                                                               
         MVC   IVHINVNO+3(4),IVHINVDT FILL YEAR IN INVNO                        
*                                                                               
         ICM   RF,15,APINVNO       IF PRINTING INVOICE NUMBER                   
         BZ    *+10                                                             
         MVC   3(4,RF),IVHINVDT       ADD YEAR TO INVOICE NUMBER                
*                                                                               
OINVDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - SRCAG'                               
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
         CLI   SBMODE,SBPROCBH     SKIP IF NOT DOING BILLS                      
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
         BNE   ISRCAGX                                                          
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ISRCAGEX                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBSRCAG,R2),ETBSRCAG RETURN SOURCE AGENCY CODE              
*                                                                               
         CLC   ETBSRCAG,SPACES     DONE IF FOUND                                
         BH    ISRCAGX                                                          
*                                                                               
ISRCAGEX DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ISRCAGX                                                          
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTSRCAG,R2),PRTSRCAG RETURN SOURCE AGENCY CODE              
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
         BZ    *+16                                                             
         MVC   0(L'IVHSRCAG,R3),0(R2)    PRINT SOURCE AGENCY                    
         OC    0(L'IVHSRCAG,R3),SPACES   SPACE FILL                             
*                                                                               
OSRCAGX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - REVRS'                               
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
         BZ    *+16                                                             
         MVC   1(L'IVHREVRS,R3),0(R2)    PRINT                                  
         OC    1(L'IVHREVRS,R3),SPACES   SPACE FILL                             
*                                                                               
OREVRSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - BLSTA'                               
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
         MVC   1(L'IVHBLSTA,R3),0(R2)       PRINT BILL STATUS                   
         OC    1(L'IVHBLSTA,R3),SPACES      PRINT BILL STATUS                   
*                                                                               
OBLSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - REQNM'                               
***********************************************************************         
*                                                                     *         
*        REQUIREMENT NUMBER                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IREQNM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        PASS ESTIMATE KEY FIELDS TO OUTPUT                                     
*                                                                               
         MVC   0(1,R2),SBBAGYMD    AGENCY/MEDIA                                 
         MVC   1(2,R2),SBBCLT      CLIENT                                       
         MVC   3(3,R2),SBPRD       PRODUCT                                      
         MVC   6(1,R2),SBBEST      ESTIMATE                                     
*                                                                               
IREQNMX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OREQNM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
*        READ ESTIMATE RECORD FOR UDEF DATA                                     
*                                                                               
         MVC   SAVEKEY,KEY        SAVE CUURENT KEY                              
         MVC   ESHREQNM,SPACES    INIT                                          
*                                                                               
         XC    KEY,KEY             READ ESTIMATE RECORD                         
         LA    R1,KEY                                                           
         USING ESTHDR,R1           ESTABLISH ESTIMATE HEADER KEY                
*                                                                               
         MVI   EKEYTYPE,EKEYTYPQ   SET RECID                                    
         MVC   EKEYAM,0(R2)        SET AGENCY/MEDIA                             
         MVC   EKEYCLT,1(R2)       SET CLIENT                                   
         MVC   EKEYPRD,3(R2)       SET PRODUCT                                  
         MVC   EKEYEST,6(R2)       SET ESTIMATE                                 
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   EKEY,KEYSAVE         SKIP IF NO RECORD                           
         BNE   OREQNMX                                                          
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT AIO                             
         L     R1,AIO2             POINT TO FOUND RECORD                        
         ST    R1,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT AIO                          
*                                                                               
         L     R1,AIO2             POINT TO FOUND RECORD                        
         USING ESTHDR,R1           ESTABLISH FOUND RECORD                       
*                                                                               
         MVC   ESHREQNM(16),EUSER1 GET UDEF FIELD                               
         OC    ESHREQNM,SPACES     SPACE FILL                                   
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+16                                                             
         MVC   0(16,R3),EUSER1        PRINT REQUIREMENT NUMBER                  
         OC    0(16,R3),SPACES        SPACE FILL                                
*                                                                               
OREQNMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R1                                                               
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - TGTMK'                               
***********************************************************************         
*                                                                     *         
*        TARGET MARKET CODE                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ITGTMK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTTGTMK,R2),SPACES     INIT                                 
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ITGTMKEX                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBTGTMK,R2),ETBTGTMK RETURN TARGET NMKT CODE                
*                                                                               
         CLC   ETBTGTMK,SPACES     DONE IF FOUND                                
         BH    ITGTMKX                                                          
*                                                                               
ITGTMKEX DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ITGTMKX                                                          
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTTGTMK,R2),PRTTGTMK RETURN TARGET MKT CODE                 
*                                                                               
ITGTMKX  DS    0H                                                               
*                                                                               
         CLC   0(L'PRTTGTMK,R2),SPACES ERROR IF NOT PRESENT                     
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
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
         BZ    *+16                                                             
         MVC   1(L'ESHTGTMK,R3),0(R2)    PRINT REQUIREMENT NUMBER               
         OC    1(L'ESHTGTMK,R3),SPACES   SPACE FILL                             
*                                                                               
OTGTMKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - LINE#'                               
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
         BZ    *+16                                                             
         MVC   0(L'IVDLINE#,R3),0(R2)       PRINT LINE NUMBER                   
         OC    0(L'IVDLINE#,R3),SPACES      SPACE FILL                          
*                                                                               
OLINE#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - DESC'                                
***********************************************************************         
*                                                                     *         
*        DESCRIPTION                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IDESC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 GETESTNM           GET ESTIMATE'S NAME                           
*                                                                               
         MVC   0(50,R2),=CL50' '   INIT WITH SPACES                             
*                                                                               
         MVC   0(L'SBESTNM,R2),SBESTNM   USE ESTIMATE NAME                      
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
         MVC   IVDDESC(L'SBESTNM),0(R2)  PUT IN INVOICE RECORD                  
         OC    IVDDESC,SPACES         SPACE FILL                                
*                                                                               
         B     ODESCRX                                                          
*                                                                               
ODESCIVN DS    0H                                                               
*                                 ELSE                                          
         MVC   SBBEST,9(R2)       SET ESTIMATE NUMBER                           
         MVC   SBBPRD,10(R2)      SET PRODUCT CODE                              
*                                                                               
         GOTO1 GETESTNM           GET ESTIMATE'S NAME                           
*                                                                               
         MVC   ESHDESC(L'SBESTNM),SBESTNM   PUT IN ESTIMATE RECORD              
         OC    ESHDESC,SPACES         SPACE FILL                                
*                                                                               
ODESCRX  DS    0H                                                               
*                                                                               
         LTR   R3,R3               IF PRINTING                                  
         BZ    *+10                                                             
         MVC   0(L'SBESTNM,R3),SBESTNM    PRINT DESCRIPTION                     
*                                                                               
ODESCX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - EXPTP'                               
***********************************************************************         
*                                                                     *         
*        EXPENSE TYPE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IEXPTP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTEXPTP,R2),SPACES    INIT                                  
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IEXPTPEX                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBEXPTP,R2),ETBEXPTP RETURN EXPENSE TYPE                    
*                                                                               
         CLC   ETBEXPTP,SPACES     DONE IF FOUND                                
         BH    IEXPTPX                                                          
*                                                                               
IEXPTPEX DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IEXPTPX                                                          
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTEXPTP,R2),PRTEXPTP RETURN EXPENSE TYPE                    
*                                                                               
IEXPTPX  DS    0H                                                               
*                                                                               
         CLC   0(L'PRTEXPTP,R2),SPACES ERROR IF NOT PRESENT                     
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
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
         BZ    *+16                                                             
         MVC   0(L'IVDEXPTP,R3),0(R2)       PRINT EXPENSE TYPE                  
         OC    0(L'IVDEXPTP,R3),SPACES      SPACE FILL                          
*                                                                               
OEXPTPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - PRDID'                               
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
         BZ    IPRDID1                                                          
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBPRDID,R2),ETBPRDID RETURN PRODUCT ID                      
*                                                                               
         CLC   ETBPRDID,SPACES     DONE IF FOUND                                
         BH    IPRDIDX                                                          
*                                                                               
IPRDID1  DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IPRDIDX                NONE AVAILABLE                            
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTPRDID,R2),PRTPRDID    RETURN PRODUCT ID                   
*                                                                               
IPRDIDX  DS    0H                                                               
*                                                                               
         OC    0(L'PRTPRDID,R2),SPACES                                          
*                                                                               
         CLC   0(L'PRTPRDID,R2),SPACES ERROR IF NOT PRESENT                     
         BH    *+8                                                              
         OI    ERRCD,ERRPRD                                                     
*                                                                               
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
         BZ    *+16                                                             
         MVC   0(L'IVDPRDID,R3),0(R2)        PRINT PRODUCT ID                   
         OC    0(L'IVDPRDID,R3),SPACES       SPACE FILL                         
*                                                                               
OPRDIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ESTID'                               
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
         MVC   0(L'SBCLT,R1),SBCLT  RETURN CLT                                  
         LA    R1,L'SBCLT(R1)      BUMP TO NEXT POSITION                        
*                                                                               
         MVC   0(L'SBPRD,R1),SBPRD   RETURN PRD                                 
         LA    R1,L'SBPRD(R1)      BUMP TO NEXT POSITION                        
*                                                                               
         MVC   0(L'SBEST,R1),SBEST   ESTIMATE NUMBER                            
*                                                                               
         CLI   SBMODE,SBPROCBH     IF PROCESSING BILL RECORDS                   
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
         BNE   *+12                                                             
         BRAS  RE,ADDESTID            ADD ESTID TO TABLE                        
         B     IESTIDX                                                          
*                                                                               
         CLC   SBPRD,=X'FEFEFE'    DROP IF UNALLOCATED                          
         BNE   *+12                                                             
         MVI   REJECTES,C'Y'             NO  - REJECT RECORD                    
         B     IESTIDX                                                          
*                                                                               
         BRAS  RE,GETESTID            CHECK IF ESTIMATE IN TABLE                
*        B     *+8                       YES - KEEP RECORD                      
         BE    *+8                       YES - KEEP RECORD                      
         MVI   REJECTES,C'Y'             NO  - REJECT RECORD                    
*                                                                               
IESTIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OESTID   NTR1  BASE=*,LABEL=*                                                   
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
         BZ    *+16                                                             
         MVC   0(L'IVDESTID,R3),0(R2)    PRINT ESTIMATE ID                      
         OC    0(L'IVDESTID,R3),SPACES   SPACE FILL                             
*                                                                               
OESTIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - CHGDT'                               
***********************************************************************         
*                                                                     *         
*        CHARGE DATE - FIRST DAY OF BILLABLE MONTH                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICHGDT   NTR1  BASE=*,LABEL=*                                                   
******                                                                          
******   CLI   SBMODE,SBPROCBH    IF PROCESSING BILLS                           
******   BE    *+8                                                              
******   CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
******   BNE   ICHGDTIN                                                         
******                                                                          
******   GOTO1 DATCON,DMCB,(2,SBQBILST),(3,0(R2)) BILL PERIOD START             
*****    MVC   0(2,R2),STABPER     COPY YM OF MONTH OF SERVICE                  
*                                                                               
         MVC   0(4,R2),SPACES                                                   
*                                                                               
ICHGDTIN DS    0H                                                               
*                                                                               
ICHGDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OCHGDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WORK,SPACES        INIT OUTPUT                                   
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE RECORD                             
         BNE   OCHGDT5                                                          
*                                                                               
         CLC   0(4,R2),SPACES     IF DATE PRESENT                               
         BNH   OCHGDT3                                                          
*                                                                               
         MVC   WORK+16(4),0(R2)       YYMM CHARACTER                            
         MVC   WORK+20(2),=C'01'      FIRST OF MONTH                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+16),(23,WORK)  CHARGE DATE                   
*                                                                               
OCHGDT3  DS    0H                                                               
*                                                                               
         MVC   IVDCHGDT,WORK         SET CHARGE DATE                            
         OC    IVDCHGDT,SPACES       SPACE FILL                                 
*                                                                               
         B     OCHGDT7                                                          
*                                 ELSE                                          
OCHGDT5  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,SBESTSTP),(0,WORK+16)  EST ST DTE                 
*                                                                               
         MVC   WORK+16+4(2),=C'01'    FIRST OF MONTH                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+16),(23,WORK)  CHARGE DATE                   
*                                                                               
         MVC   ESHCHGDT,WORK         SET IN ESTIMATE HEADER                     
         OC    ESHCHGDT,SPACES       SPACE FILL                                 
*                                                                               
OCHGDT7  DS    0H                                                               
*                                                                               
         LTR   R3,R3              IF PRINTING                                   
         BZ    *+10                                                             
         MVC   0(10,R3),WORK       PRINT DATE                                   
*                                                                               
OCHGDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - NET'                                 
***********************************************************************         
*                                                                     *         
*        NET - BHNET/BYNET                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(4,R2),0(R2)       INIT VALUE                                   
*                                                                               
         CLI   SBMODE,SBPROCBH     SKIP IF BILL DATA                            
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA     OR  PROCESSING CASH APPLIED                  
         BE    INETX                                                            
*                                                                               
         CLI   SBMODE,SBPROCSP     SKIP IF NOT PROCESSING BUYS                  
         BNE   INETX                                                            
*                                                                               
         L     R5,SBACURCH         A(BUY RECORD CHUNK)                          
         USING SCHUNKD,R5                                                       
*                                                                               
         MVC   0(4,R2),SCNET       GET NET AS BINARY                            
*                                                                               
INETX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ONET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    ONETTL                                                           
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   ONETIVN                                                          
*                                                                               
         ICM   RF,15,0(R2)         ACCUMULATE BATCH TOTAL                       
         CVD   RF,DUB                                                           
         AP    SPIVNET,DUB                                                      
*                                                                               
         MVC   SVNET,0(R2)         SAVE NET VALUE                               
*                                                                               
         EDIT  (B4,0(R2)),IVDNET,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   IVDNET+L'IVDNET-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   IVDNET+L'IVDNET-4(4),=C'0.00'  ADD ZERO                          
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
         EDIT  (B4,0(R2)),ESDNET,2,FLOAT=-   PUT IN ESTIMATE RECORD             
*                                                                               
         CLC   ESDNET+L'ESDNET-4(4),=C' .00'   IF ZERO                          
         BNE   *+10                                                             
         MVC   ESDNET+L'ESDNET-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (B4,0(R2)),(14,0(R3)),2,FLOAT=-   PRINT                          
*                                                                               
         ICM   RF,15,0(R2)                                                      
         CVD   RF,DUB                                                           
         AP    SPESNET,DUB         ACCUMULATE BATCH TOTAL                       
*                                                                               
         B     ONETX                                                            
*                                                                               
ONETESN  DS    0H                                                               
         B     ONETX               UNKNOWN RECORD ID                            
*                                                                               
*        PRINT TOTALS                                                           
*                                                                               
ONETTL   DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE RECORD                             
         BNE   ONETTLIN                                                         
*                                                                               
         MVC   SVNET,0(R2)         SAVE NET VALUE                               
*                                                                               
         OC    SVNET,SVNET         CHECK FOR DATA                               
         BNZ   *+8                                                              
         MVI   MONEY,C'Y'                                                       
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
         OC    0(4,R2),0(R2)       CHECK FOR NO MONEY                           
         BNZ   *+8                                                              
         MVI   MONEY,C'Y'                                                       
*                                                                               
         EDIT  (B4,0(R2)),(14,0(R3)),2,FLOAT=-   PRINT                          
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
         TITLE 'T20435 - KRTAPE CREATION - COM'                                 
***********************************************************************         
*                                                                     *         
*        COMPENSATION                                                 *         
*                                                                     *         
*NTRY    R2 ==> BILLCST                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICOM     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'      NO COMPENSATION FOR ESTIMATE                  
*                                                                               
ICOMX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OCOM     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   AGYPROF+7-AGYHDR(R1),C'C'   NO COMPENSATION FOR CDN AGY          
         BNE   *+10                                                             
         SR    RF,RF                  COMPENSATION AMOUNT                       
         B     OCOM20                                                           
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OCOMTL                                                           
*                                                                               
         CLI   SVRECID,C'2'       IF ESTIMATE RECORD                            
         BNE   *+14                                                             
         MVC   ESDCOM,SPACES         SPACE FILL                                 
         B     OCOMX                                                            
*                                                                               
         CLC   =C'YN',TWAAGY       IF AGENCY IS YNR                             
         BNE   *+14                                                             
         MVC   IVDCOM,SPACES         SPACE FILL                                 
         B     OCOMX                                                            
*                                                                               
         ICM   RF,15,0(R2)        COMPENSATION = AGENCY COMMISSION              
******   S     RF,SVNET                                                         
*                                                                               
OCOM20   DS    0H                                                               
*                                                                               
         STCM  RF,15,0(R2)         SET COMPENSATION                             
*                                                                               
         CVD   RF,DUB                                                           
*                                                                               
         AP    SPIVCOM,DUB         ACCUMULATE BATCH TOTALS                      
*                                                                               
         EDIT  (B4,0(R2)),IVDCOM,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   IVDCOM+L'IVDCOM-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
         B     *+10                                                             
         MVC   IVDCOM+L'IVDCOM-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OCOMX                                                            
*                                                                               
*        PRINT TOTALS                                                           
*                                                                               
OCOMTL   DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'       SKIP IF ESTIMATE RECORD                       
         BE    OCOMX                                                            
*                                                                               
         CLC   =C'YN',TWAAGY       IF AGENCY IS YNR                             
         BNE   *+14                                                             
         MVC   IVDCOM,SPACES         SPACE FILL                                 
         B     OCOMX                                                            
*                                                                               
         ICM   RF,15,0(R2)        COMPENSATION = COST-NET                       
         S     RF,SVNET                                                         
         BZ    *+8                                                              
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
*                                                                               
         STCM  RF,15,0(R2)                                                      
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
OCOMX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ADJ'                                 
***********************************************************************         
*                                                                     *         
*        ADJUSTMENT = ZERO                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IADJ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'      NO CASH DISCOUNT                              
*                                                                               
IADJX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ADJ'                                 
***********************************************************************         
*                                                                     *         
*        ADJUSTMENT = MINUS CD                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OADJ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    0(8,R2),0(R2)      IF NO DATA                                    
         BNZ   *+10                                                             
         ZAP   0(8,R2),=P'0'         FORCE ZERO                                 
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OADJTL                                                           
*                                                                               
         CLI   SVRECID,C'2'       IF ESTIMATE RECORD                            
         BNE   *+14                                                             
         MVC   ESDADJ,SPACES         SPACE FILL                                 
         B     OADJX                                                            
*                                                                               
         ZAP   DUB,=P'0'                                                        
         SP    DUB,0(8,R2)         REVERSES INPUTS SIGN                         
         EDIT  (P8,DUB),IVDADJ,2,FLOAT=-   PUT IN INVOICE RECORD                
*                                                                               
         AP    SPIVADJ,DUB         ACCUMULATE BATCH TOTAL                       
*                                                                               
         CLC   IVDADJ+L'IVDADJ-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
         B     *+10                                                             
         MVC   IVDADJ+L'IVDADJ-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
         B     OADJX                                                            
*                                                                               
*        PRINT TOTALS                                                           
*                                                                               
OADJTL   DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'2'       SKIP IF ESTIMATE RECORD                       
         BE    OADJX                                                            
*                                                                               
         CP    0(8,R2),=P'0'       IF WE HAVE DOLLARS                           
         BE    *+8                                                              
         MVI   MONEY,C'N'             INDICATE MONEY FOR LINE                   
*                                                                               
         MVI   GLHOOK,GLEDIT       HAVE  DRIVER PRINT IT                        
*                                                                               
OADJX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - GRS'                                 
***********************************************************************         
*                                                                     *         
*        GROSS                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IGRS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SBMODE,SBPROCBH     SKIP IF INVOICE DATA                         
         BE    *+8                                                              
         CLI   SBMODE,SBPROCCA    IF PROCESSING CASH APPLIED                    
         BE    IGRSX                                                            
*                                                                               
         L     R5,SBACURCH         A(BUY RECORD CHUNK)                          
         USING SCHUNKD,R5                                                       
*                                                                               
         CLI   SBMODE,SBPROCSP    SKIP IF NOT PROCESSING BUYS                   
         BNE   IGRSX                                                            
*                                                                               
         MVC   0(4,R2),SCGROSS    RETURN GROSS                                  
*                                                                               
IGRSX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
OGRS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OGRSTL                                                           
*                                                                               
         CLI   SVRECID,C'1'        IF INVOICE RECORD                            
         BNE   OGRSIVN                                                          
*                                                                               
         ICM   RF,15,0(R2)         ACCUMULATE BATCH TOTALS                      
         CVD   RF,DUB                                                           
         AP    SPIVGRS,DUB                                                      
*                                                                               
         EDIT  (B4,0(R2)),IVDGRS,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   IVDGRS+L'IVDGRS-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
         B     *+10                                                             
         MVC   IVDGRS+L'IVDGRS-4(4),=C'0.00'  ADD ZERO                          
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
         ICM   RF,15,0(R2)                                                      
         CVD   RF,DUB                                                           
         AP    SPESGRS,DUB         ACCUMULATE BATCH TOTALS                      
*                                                                               
         EDIT  (B4,0(R2)),ESDGRS,2,FLOAT=-   PUT IN INVOICE RECORD              
*                                                                               
         CLC   ESDGRS+L'ESDGRS-4(4),=C' .00'   IF ZERO                          
         BE    *+12                                                             
         MVI   MONEY,C'N'         INDICATE MONEY FOR LINE                       
         B     *+10                                                             
         MVC   ESDGRS+L'ESDGRS-4(4),=C'0.00'  ADD ZERO                          
*                                                                               
         EDIT  (B4,0(R2)),(14,0(R3)),2,FLOAT=-   PRINT                          
*                                                                               
         B     OGRSX                                                            
*                                                                               
OGRSESN  DS    0H                                                               
         B     OGRSX              UNKNOWN RECORD ID                             
*                                                                               
*        PRINT TOTALS                                                           
*                                                                               
OGRSTL   DS    0H                                                               
*                                                                               
         CLI   SVRECID,C'1'       IF INVOICE RECORD                             
         BNE   OGRSTLIN                                                         
*                                                                               
         OC    0(4,R2),0(R2)       IF WE HAVE DOLLARS                           
         BE    *+8                                                              
         MVI   MONEY,C'N'             INDICATE MONEY FOR LINE                   
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
         OC    0(4,R2),0(R2)       IF WE HAVE DOLLARS                           
         BZ    *+8                                                              
         MVI   MONEY,C'N'             INDICATE MONEY FOR LINE                   
*                                                                               
         EDIT  (B4,0(R2)),(14,0(R3)),2,FLOAT=-   PRINT                          
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
         TITLE 'T20435 - KRTAPE CREATION - BLLD'                                
***********************************************************************         
*                                                                     *         
*        BILLED                                                       *         
*                                                                     *         
*NTRY    R2 ==> BILLCST                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBLLD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'      NO BILLED FOR ESTIMATE                        
*                                                                               
IBLLDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBLLD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BO    OBLLDTL                                                          
*                                                                               
         MVC   ESDBLLD,SPACES        SPACE FILL                                 
         B     OBLLDX                                                           
*                                                                               
*        TOTALS                                                                 
*                                                                               
OBLLDTL  DS    0H                                                               
*                                                                               
OBLLDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - CRAGY'                               
***********************************************************************         
*                                                                     *         
*        CREATIVE AGENCY                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICRAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTCRAGY,R2),SPACES    INIT                                  
*                                                                               
         CLI   SBMODE,SBPROCSP    SKIP IF PROCESSING BUYS                       
         BE    ICRAGYX                                                          
*                                                                               
         ICM   R6,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ICRAGYEX                                                         
*                                                                               
         USING ESTTABD,R6          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBCRAGY,R2),ETBCRAGY RETURN CREATIVE AGENCY                 
*                                                                               
         CLC   ETBCRAGY,SPACES     DONE IF FOUND                                
         BH    ICRAGYX                                                          
*                                                                               
ICRAGYEX DS    0H                                                               
*                                                                               
         ICM   R6,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ICRAGYX                                                          
*                                                                               
         USING PRDTABD,R6          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTCRAGY,R2),PRTCRAGY RETURN CREATIVE AGENCY CODE            
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
         BZ    *+16                                                             
         MVC   0(L'IVDCRAGY,R3),0(R2)    PRINT ESTIMATE ID                      
         OC    0(L'IVDCRAGY,R3),SPACES   PRINT ESTIMATE ID                      
*                                                                               
OCRAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - DEAL#'                               
***********************************************************************         
*                                                                     *         
*        DEAL NUMBER   - NOT PROVIDED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IDEAL#   NTR1  BASE=*,LABEL=*                                                   
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
         TITLE 'T20435 - KRTAPE CREATION - FILLER'                              
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
         LLC   RE,DROLEN-DROD(R1)  GET OUTPUT LENGTH                            
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
         TITLE 'T20435 - KRTAPE CREATION - IGST'                                
***********************************************************************         
*                                                                     *         
*        GST                                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IGST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SBMODE,SBPROCBH     SKIP IF NOT BILL HEADER                      
         BNE   IGSTX                                                            
*                                                                               
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   IGSTX                                                            
*                                                                               
*                                                                               
*        BREAKOUT VALUES IN BILL HEADER RECORD                                  
*              BILL HEADER IN IOAREA1                                           
*                                                                               
         GOTO1 =V(SPBVAL),DMCB,(C'B',AIO1),(0,SPBVALC)                          
*                                                                               
         LA    R6,SPBVALC          ESTABLISH SPBVAL OUTPUT                      
         USING SPBVALD,R6                                                       
*                                                                               
         L     RF,SPBVGST          COMBINE GST AND HST                          
         A     RF,SPBVHST                                                       
*                                                                               
         STCM  RF,15,0(R2)         RETURN INPUT TO DRIVER                       
*                                                                               
IGSTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - OGST'                                
***********************************************************************         
*                                                                     *         
*        GST                                                          *         
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
         EDIT  (B4,0(R2)),IVGST,2,FLOAT=-   PUT IN INVOICE RECORD               
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
         TITLE 'T20435 - KRTAPE CREATION - IPST'                                
***********************************************************************         
*                                                                     *         
*        PST                                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IPST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,SPBVALC          ESTABLISH SPBVAL OUTPUT                      
         USING SPBVALD,R6                                                       
*                                                                               
         L     RF,SPBVPST          GET PST                                      
         S     RF,SPBVHST          SUBTRACT ANY HST                             
         STCM  RF,15,0(R2)         RETURN INPUT TO DRIVER                       
*                                                                               
*                                                                               
IPSTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - OPST'                                
***********************************************************************         
*                                                                     *         
*        PST                                                          *         
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
         EDIT  (B4,0(R2)),IVQST,2,FLOAT=-   PUT IN INVOICE RECORD               
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
         TITLE 'T20435 - KRTAPE CREATION - ERR'                                 
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
         MVI   ERRCD,0            RESET ERROR SWITCH                            
*                                                                               
******   MVI   REJECT,C'R'        DON'T WRITE RECORD                            
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
         TITLE 'T20435 - KRTAPE CREATION - LASTCOL'                             
***********************************************************************         
*                                                                     *         
*        LAST COLUMN - WRITE RECORD TO TAPE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LASTCOL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'       SKIP IF ON-LINE                               
         BNE   LASTCOLX                                                         
*                                                                               
         CLI   GFTTAP,C'Y'        SKIP IF SUPPRESSING THE TAPE                  
         BE    LASTCOLX                                                         
*                                                                               
         CLI   REJECT,C'Y'         TEST RECORD REJECTED                         
         BE    *+8                                                              
         CLI   REJECT,C'R'         TEST RECORD REJECTED                         
         NOP   LASTCOLX            YES                                          
*                                                                               
         CLI   ERRCD,0            SKIP RECORDS IN ERROR                         
         NOP   LASTCOLX                                                         
*                                                                               
         CLI   MONEY,C'Y'          TEST $0                                      
         BE    LASTCOLX            YES                                          
*                                                                               
         CLI   KRINVSW,C'O'       SKIP IF TAPE OPEN                             
         BE    LSTCLO10                                                         
*                                                                               
*        OPEN INVOICE TAPE IF NEEDED                                            
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN INVOICE FILE                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   KRINVSW,C'O'       INDICATE FILE WAS OPENED                      
*                                                                               
LSTCLO10 DS    0H                                                               
*                                                                               
         CLI   KRESTSW,C'O'       SKIP IF TAPE OPEN                             
         BE    LSTCLO20                                                         
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCBS                    
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN ESTIMATE FILE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   KRESTSW,C'O'       INDICATE FILE WAS OPENED                      
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
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+8                                                              
         L     R3,AKRCINVF            POINT TO OUTPUT TAPE DCBS                 
*                                                                               
         CLI   SPIHEAD,C'H'       IF HEADER TO BE PRINTED                       
         BNE   LSTCLIV1                                                         
*                                                                               
*        BUILD INVOICE BATCH HEADER                                             
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
         MVC   IBHCRDTE,WORK+20                                                 
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
         LA    R6,IBHREC             WRITE RECORD TO TEMP FILE                  
         PUT   (R3),(R6)                                                        
*                                                                               
         MVI   SPIHEAD,C'N'          TURN OFF SWITCH                            
*                                                                               
LSTCLIV1 DS    0H                                                               
*                                                                               
         LA    R6,IVREC            POINT TO OUTPUT INVOICE RECORD               
*                                                                               
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
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
         CLI   SPEHEAD,C'H'       IF HEADER STILL TO BE PRINTED                 
         BNE   LSTCLES1                                                         
*                                                                               
*        BUILD ESTIMATE BATCH HEADER                                            
*                                                                               
         MVI   EBHREC,C' '         INIT ESTIMATE BATCH HEADER RECORD            
         MVC   EBHREC+1(235),EBHREC                                             
*                                                                               
         MVC   EBHRECID,=CL3'AE '  SET RECORD ID                                
         MVC   EBHSTART,=CL3'ADE'  SET SECONDARY RECORD ID                      
         MVC   EBHRECV,=CL10'STIMATE' SET REST OF RECORD ID                     
*                                                                               
         MVC   EBHCRDTE,IBHCRDTE  SET CREATION DATE                             
         MVC   EBHCRTIM,IBHCRTIM  SET HH:MM:SS                                  
*                                                                               
         LA    R6,EBHREC             WRITE RECORD TO TEMP FILE                  
         PUT   (R3),(R6)                                                        
*                                                                               
         MVI   SPEHEAD,C'N'          TURN OFF SWITCH                            
*                                                                               
LSTCLES1 DS    0H                                                               
*                                                                               
         LA    R6,ESREC            POINT TO OUTPUT ESTIMATE RECORD              
*                                                                               
         PUT   (R3),(R6)           PUT TO TEMP DISK FILE                        
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
         MVI   ERRCD,0                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - WORKTSTG'                            
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKSTG  DS    0D                                                               
         DS    0F                                                               
ASPFUSER DS    A                                                                
ABUFFER  DC    A(0)                A(GETMAIN AREA)                              
APRDTAB  DS    A(APRDTAB)    PRODUCT TABLE                                      
APRTENT  DS    A                   A(ENTRY IN PRODUCT  TABLE)                   
AETBENT  DS    A                   A(ENTRY IN ESTIMATE TABLE)                   
AESTTAB  DS    A(AESTTAB)    ESTIMATE TABLE                                     
AESTZZT  DS    A(AESTZZT)    POL TABLE                                          
VDYNALLO DS    V                   V(DYNALLOC)                                  
APINVNO  DS    A                   A(INVOICE NUMBER PRINT POSITION)             
*                                                                               
TOTNET   DS    PL8                                                              
TOTCOM   DS    PL8                                                              
*                                                                               
*        FIELD NAMES IN GFEST RECORD                                            
*                                                                               
QNAMETB  DS    0X                  TABLE OF GFEST FIELD NAMES/LENGTHS           
QDIVBD   DC    CL8'DIVBRNCD',AL1(L'PRTDIVBD)   DIVISION BRAND CODE              
QPRDCD   DC    CL8'PROD CD ',AL1(L'PRTPRDCD)   PRODUCT CODE                     
QGFNAT   DC    CL8'GF NTRL ',AL1(L'PRTGFNAT)   GF NATURAL                       
QGFSUB   DC    CL8'GFSBNTRL',AL1(L'PRTGFSUB)   GF SUB NATURAL                   
QBLAGY   DC    CL8'GFBILAGY',AL1(L'PRTBLAGY)                                    
QEXPTP   DC    CL8'GFEXPTYP',AL1(L'PRTEXPTP)                                    
QPRDID   DC    CL8'GFPRODID',AL1(L'PRTPRDID)                                    
QCRTAG   DC    CL8'GFCRTVAG',AL1(L'PRTCRAGY)                                    
QSRCAG   DC    CL8'GFSRCAGY',AL1(L'PRTSRCAG)                                    
QREQNM   DC    CL8'GFREQNUM',AL1(L'PRTREQNM)                                    
QTGTMK   DC    CL8'GFTGRMKT',AL1(L'PRTTGTMK)                                    
QDEAL    DC    CL8'GFDEALNO',AL1(L'PRTDEAL)                                     
QNAMENOQ EQU   (*-QNAMETB)/9       NUMBER OF ENTRIES IN TABLE                   
         DC    X'FF'               EOT                                          
*                                                                               
*        OLD FIELD NAMES IN GFEST RECORD                                        
*                                                                               
*                                                                               
ETEST    DS    CL1                TEST ESTIMATE BYTE X'80' = TEST               
EBFORM   DS    XL5                BILLING FORMULA                               
EBFDATE  DS    XL3                BILLING FORMUAL EFF DATE                      
*                                                                               
SVRECID  DS    CL1                RECORD IDENTIFIER                             
SVMED    DS    CL1                                                              
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVPCD    DS    CL1                                                              
SVEST    DS    XL1                                                              
SVBFEST  DS    XL1                                                              
SVNET    DS    F                                                                
REJECT   DS    CL1                                                              
REJECTES DS    CL1                                                              
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
KRPSTTYP DS    CL1                 C'H' - PST IS HST                            
*                                                                               
MONTABLE DS    25XL6               MONTH TABLE                                  
*                                  ENTRIES ARE BINARY YYMM FOLLOWED             
*                                  BY CHARACTERS YYMM                           
*                                  EXAMPLE - X'5801' C'8801'                    
*                                                                               
         DC    25XL6'0'                                                         
         DC    25XL6'0'                                                         
SUPTAP   DS    CL1                                                              
*                                                                               
PARMLST1 CALL  ,(DDKRINV,SPFKRINV),MF=L  GET GENERATION                         
PARMLST2 CALL  ,(DDKREST,SPFKREST),MF=L  GET GENERATION                         
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
DDKRINV  DC    CL8'KRINVFL '                                                    
DDKREST  DC    CL8'KRESTFL '                                                    
DSNKRINV DC    CL20'SPTTAPE.SP0KRXX1'                                           
DSNKREST DC    CL20'SPTTAPE.SP0KRXX2'                                           
TMPALLOC DC    XL6'000003000003'                                                
MYWORK   DS    CL5                                                              
AKRINVFL DS    A                   A(KRINVFL DCB IN SPFUSER)                    
AKRCINVF DS    A                   A(KRINVFL DCB IN SPFUSER)                    
AKRESTFL DS    A                   A(KRESTFL DCB IN SPFUSER)                    
*                                                                               
EITBBXLE DS    3A                  BXLE REGISTERS FOR ESTID TABLE               
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - SPFAREA'                             
***********************************************************************         
*                                                                     *         
*        SPFAREA -  VALUES SAVED IN SPFUSER BETWEEN REQUESTS          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPFAREA  DS    0X                                                               
*                                                                               
SPFAGYCD DS    CL1                                                              
SPFTITLE DS    CL63                                                             
SPFTTLSB DS    CL36                                                             
SPFSPLID DS    CL3                                                              
*                                                                               
SPFKRINV DC    CL44' '             GENERATION NAME                              
SPFKREST DC    CL44' '             GENERATION NAME                              
*                                                                               
KRINVSW  DC    C' '               C'O' - KRINVFL TAPE OPENED                    
KRESTSW  DC    C' '               C'O' - KRESTFL TAPE OPENED                    
*                                                                               
SPIBTCTR DS    PL8                 INVOICE  RECORD COUNTER                      
SPEBTCTR DS    PL8                 ESTIMATE RECORD COUNTER                      
*                                                                               
SPIBLAGY DS    CL8                 INVOICE  BILLING AGENCY                      
SPEBLAGY DS    CL8                 ESTIMATE BILLING AGENCY                      
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
         TITLE 'T20435 - KRTAPE CREATION - DCBS'                                
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
         DC    CL32'END OF SPWRI35 SPFUSER'                                     
*                                                                               
SAVVALSL EQU   *-SPFAREA                                                        
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INVOICE RECORDS - IBHREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE BATCH HEADER RECORD                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBHREC   DS    0CL318          *** INVOICE BATCH HEADER RECORD                  
*                                                                               
IBHRECID DS    CL3'AI '            RECORD ID                                    
IBHSTART DS    CL3'ADI'            FURTHER RECORD ID                            
IBHRECV  DS    CL10'NVOICE'        MORE RECORD ID                               
IBHCRDTE DS    CL10                CREATION DATE                                
IBHCRTIM DS    CL8                 CREATION TIME                                
         DS    XL(L'IBHREC-(*-IBHREC)) SPARE                                    
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INVOICE RECORDS - IVREC'             
***********************************************************************         
*                                                                     *         
*        INVOICE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IVREC    DS    0CL318          *** INVOICE RECORD                               
*                                                                               
IVHDR    DS    0CL67               INVOICE HEADER                               
IVHBLAGY DS    CL8                 BILLING AGENCY CODE                          
IVHBTID  DS    CL20                BATCH ID                                     
IVHINVNO DS    CL20                INVOICE DATE                                 
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
*                                                                               
IVRECL   EQU   *-IVREC             RECORD LENGTH                                
*                                                                               
IVCDNFLS DS    0XL72               CANADIAN FIELDS                              
IVGST    DS    CL16                GST+HST                                      
IVQST    DS    CL16                QST                                          
IVGSTID  DS    CL20                GST-HST REGISTRATION CODE                    
IVQSTID  DS    CL20                QST     REGISTRATION CODE                    
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - INVOICE RECORDS - IVCREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE CONTROL RECORD                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
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
         TITLE 'T20435 - KRTAPE CREATION - INVOICE RECORDS - IBTREC'            
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
         TITLE 'T20435 - KRTAPE CREATION - ESTIMATE RECORDS - EBHREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE BATCH HEADER RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EBHREC   DS    0CL236          *** ESTIMATE BATCH HEADER RECORD                 
*                                                                               
EBHRECID DS    CL3'AE '            RECORD ID                                    
EBHSTART DS    CL3'ADE'            FURTHER RECORD ID                            
EBHRECV  DS    CL10'STIMATE'       MORE RECORD ID                               
EBHCRDTE DS    CL10                CREATION DATE                                
EBHCRTIM DS    CL8                 CREATION TIME                                
         DS    XL(L'EBHREC-(*-EBHREC)) SPARE                                    
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ESTIMATE RECORDS - ESREC'            
***********************************************************************         
*                                                                     *         
*        ESTIMATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESREC    DS    0CL236          *** ESTIMATE RECORD                              
*                                                                               
ESHDR    DS    0CL68               ESTIMATE HEADER                              
ESHBLAGY DS    CL8                 BILLING AGENCY CODE                          
ESHBTID  DS    CL20                BATCH ID                                     
ESHESTID DS    CL20                ESTIMATE NUMBER CLT/PRD/EST                  
ESHDESC  DS    CL50                ESTIMATE DESCRIPTION                         
ESHCHGDT DS    CL10                CHARGE DATE - NOT NEEDED                     
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
ESDBLLD  DS    CL16                BILLED SO FAR - NOT NEEDED                   
ESDDEAL# DS    CL10                DEAL NUMBER                                  
*                                                                               
SAVEKEY  DS    XL48                                                             
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ESTIMATE RECORDS - ESCREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE CONTROL RECORD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESCREC   DS    0CL236          *** ESTIMATE CONTROL RECORD                      
*                                                                               
ESCBLAGY DS    CL8                 BILLING AGENCY CODE                          
ESCBTID  DS    CL20                BATCH ID                                     
ESCRECCT DS    CL10                RECORD COUNT                                 
ESCNET   DS    CL16                NET                                          
ESCCOM   DS    CL16                COMMISSION                                   
ESCADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
ESCGRS   DS    CL16                GROSS                                        
ESCBLLD  DS    CL16                BILLED                                       
*                                                                               
         DS    XL(L'ESCREC-(*-ESCREC)) SPARE                                    
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - ESTIMATE RECORDS - ESTREC'           
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
*                                                                               
KRCDNREC DS    0CL318              CANADIAN RECORD BUILD AREA                   
KRCHDR   DS    XL(L'IVHDR)           HEADER                                     
KRCCDN   DS    XL(L'IVCDNFLS)        CANADIAN FIELDS                            
KRCDTL   DS    XL(L'IVDTL)           DETAIL                                     
KRCDNRL  EQU   *-KRCDNREC          CANADIAN RECORD LENGTH                       
*                                                                               
SPBVALC  DS    XL(SPBVALDL)        SPBVAL OUTPUTAREA                            
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - BUFFERD'                             
***********************************************************************         
*                                                                     *         
*        BUFFER DSECT                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
BUFFERD  DSECT                    BUFFER DSECT                                  
*                                                                               
         DS    0D                 ALIGNMENT                                     
PRDTABC  DS    XL(32+255*PRTENTL) PRODUCT  CODE TABLE                           
         DS    0D                 ALIGNMENT                                     
ESTTABC  DS    XL(32+255*ETBENTL) ESTIMATE CODE TABLE                           
*                                                                               
ESTIDTB  DS    XL(1000*L'IVDESTID) ESTID TABLE                                  
*                                                                               
BUFFERL  EQU   *-BUFFERD           LENGTH OF DSECT                              
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - DSECTS'                              
***********************************************************************         
*                                                                     *         
*        DSECTS                                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENSTAB                                                                      
*SPGENEST                                                                       
*SPDDEQUS                                                                       
*DDMASTD                                                                        
*DDREMOTED                                                                      
*SPGENAGY                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         TITLE 'T20435 - KRTAPE CREATION - PGESTD'                              
***********************************************************************         
*                                                                     *         
*        GF ESTIMATE RECORD DSECT                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE SPGENPGEST                                                     
*                                                                               
         TITLE 'T20435 - KRTAPE CREATION - PRWRIE9D'                            
***********************************************************************         
*                                                                     *         
*        SCREEN LAYOUT                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE9D                                                       
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
         TITLE 'T20435 - KRTAPE CREATION - PRDTABD'                             
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
PRTPCD   DS    XL1                 PRODUCT CODE                                 
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
         TITLE 'T20435 - KRTAPE CREATION - ESTTABD'                             
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
ETBEST   DS    CL1                 ESTIMATE CODE                                
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPWRI35   11/08/12'                                      
         END                                                                    
