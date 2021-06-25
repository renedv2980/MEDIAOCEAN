*          DATA SET PPPAY04    AT LEVEL 035 AS OF 01/30/12                      
*PHASE T40304B                                                                  
*        TITLE 'PPPAY04 - CASH RECEIVED FOR BILL -INIT'                         
***********************************************************************         
*                                                                     *         
*        PPPAY04 - CASH RECEIVED FOR BILL                             *         
*              GIVEN A BILL RECORD THIS MODULE CHECKS ACC             *         
*              TO SEE IF THE CASH FOR IT HAS BEEN RECEIVED            *         
*                                                                     *         
*NTRY    PARM0    A(CSHRCVC - CONTROL BLOCK)                          *         
*        PARM1    A(BILLREC)                                          *         
*                                                                     *         
*                                                                     *         
*EXIT    RETURN CODE SET TO INDICATE WHETHER CASH RECEIVED OR NOT     *         
*        RETURN CODE IN CONTROL BLOCK                                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* 11/27/02 BOBY - USE PBILIMO FOR INVOICE MONTH IF PRESENT                      
*                                                                               
* 12/27/01 SMYE - CHANGE PARAMETER 1 IN DATAMGR CALLS                           
*                 FROM =CL8'XXXXXX' TO (0,=C'XXXXXX')                           
*                                                                               
         SPACE 2                                                                
*        REGISTER USAGE                                                         
*                                                                               
COMFACRG EQU   R2                  COMFACSD REGISTER                            
CT5RECRG EQU   R2                  CT5REC   REGISTER                            
CTSYSDRG EQU   R3                  CTSYSD   REGISTER                            
*                                                                               
MPDRECRG EQU   R2                  MPDREC   REGISTER                            
MBTELDRG EQU   R3                  MBTELD   REGISTER                            
*                                                                               
TRNRECRG EQU   R4                  TRNRECD  REGISTER                            
ACTRECRG EQU   R4                  ACTRECD  REGISTER                            
PMDRECRG EQU   R4                  PMDRECD  REGISTER                            
*                                                                               
GDAELDRG EQU   R5                  ODAELD   REGISTER                            
OTHELDRG EQU   R5                  OTHELD   REGISTER                            
TRNELDRG EQU   R5                  TRNELD   REGISTER                            
MBIELDRG EQU   R5                  MBIELD   REGISTER                            
MDTELDRG EQU   R5                  MDTELD   REGISTER                            
BNDELDRG EQU   R5                  BNDELD   REGISTER                            
PPRELDRG EQU   R5                  PPRELD   REGISTER                            
PMDELDRG EQU   R5                  PMDELD   REGISTER                            
PTAELDRG EQU   R5                  PTAELD   REGISTER                            
*                                                                               
PBILRCRG EQU   R9                  PBILLREC REGISTER                            
*                                                                               
CRCBLKRG EQU   RA                  CSHRCVD  REGISTER                            
CRWRKRG  EQU   RC                  CSHRCVWD REGISTER                            
*                                                                               
         EJECT                                                                  
T40304   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 CSHRCVWL,T40304                                                  
*                                                                               
         USING CSHRCVWD,CRWRKRG   LOCAL WORKING STORAGE                         
*                                                                               
         MVC   CRWPARMS(CRWPARML),0(R1)  SAVE PARAMETER LIST                    
*                                                                               
         L     CRCBLKRG,CRWCBLKA                                                
         USING CSHRCVD,CRCBLKRG   CASH RECEIVED CONTROL BLOCK                   
*                                                                               
         L     PBILRCRG,CRWBRECA  A(PBILLREC)                                   
         USING PBILLREC,PBILRCRG                                                
*                                                                               
*        GET REQUIRED ADDRESSES                                                 
*                                                                               
         L     COMFACRG,CRCOMA     A(COMFACS)                                   
         USING COMFACSD,COMFACRG                                                
*                                                                               
         MVC   VDATAMGR,CDATAMGR   A(DATAMGR)                                   
         MVC   VDATCON,CDATCON     A(DATCON)                                    
         MVC   VSWITCH,CSWITCH     A(SWITCH)                                    
         MVC   VGETPROF,CGETPROF   A(GETPROF)                                   
*                                                                               
         DROP  COMFACRG                                                         
*                                                                               
         LH    RF,=Y(IOAREA1-CSHRCVWD) SET IO AREAS ADDRESSES                   
         LA    RF,CSHRCVWD(RF)                                                  
         ST    RF,AIO1                                                          
*                                                                               
         LH    RF,=Y(IOAREA2-CSHRCVWD) SET IO AREAS ADDRESSES                   
         LA    RF,CSHRCVWD(RF)                                                  
         ST    RF,AIO2                                                          
*                                                                               
         LH    RF,=Y(IOAREA3-CSHRCVWD) SET IO AREAS ADDRESSES                   
         LA    RF,CSHRCVWD(RF)                                                  
         ST    RF,AIO3                                                          
*                                                                               
         ZAP   ACCBAL,=P'0'        INIT ACCOUNT BALANCE                         
         MVI   ACTVSW,0            INIT ACTIVITY SWITCH                         
         CLI   CRIND,CRI1STQ       SKIP INIT IF NOT FIRST TIME                  
         BNE   INIT10                                                           
*                                                                               
***********************************************************************         
*                                                                     *         
*        GET ACCPAK SYSTEM NUMBER AND COMPANY CODE                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CTAUTH   DS    0H                                                               
*                                                                               
         MVI   CRIND,0             RESET INDICATOR                              
*                                                                               
         XC    CRSAVE,CRSAVE       INIT SAVED WORK AREA                         
*                                                                               
         GOTO1 VSWITCH,DMCB,(X'0A',0),0 SWITCH TO CONTROL SYSTEM                
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    CT5RECRG,KEY        ESTABLISH CT5REC (SYSTEM ACCESS REC)         
         USING CT5REC,CT5RECRG                                                  
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ    RECORD ID                                    
         MVC   CT5KALPH,PBILKAGY   SET AGENCY CODE                              
*                                                                               
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE',KEY,AIO1,0 READ          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=CL8'CTFILE',KEY,AIO1,0             
         CLI   8(R1),0             NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     CT5RECRG,AIO1       POINT TO FOUND RECORD                        
*                                                                               
         LA    CTSYSDRG,CT5DATA    POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
CTAUTHLP DS    0H                                                               
*                                                                               
         USING CTSYSD,CTSYSDRG     ESTABLISH SYS AUTH ELEMENT                   
*                                                                               
         CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   CTAUTHCN                                                         
*                                                                               
         CLI   CTSYSNUM,6          MUST BE FOR ACC SYSTEM                       
         BNE   CTAUTHCN                                                         
*                                                                               
         MVC   CRSACCSE,CTSYSSE    SAVE ACC SE NUMBER                           
         MVC   CRSACCCD,CTSYSAGB   SAVE COMPANY CODE                            
         XC    CRSALPH,CRSALPH     INIT AGENCY ALPH CD FOR SPLIT FILES          
*                                                                               
         B     CTAUTHDN                                                         
*                                                                               
CTAUTHCN DS    0H                                                               
*                                                                               
         IC    R0,CTSYSLEN         BUMP TO NEXT ELEMENT                         
         AR    CTSYSDRG,R0                                                      
         B     CTAUTHLP                                                         
*                                                                               
CTAUTHDN DS    0H                                                               
*                                                                               
         GOTO1 VSWITCH,DMCB,=C'PRINT',0   SWITCH TO BACK TO PRINT               
*                                                                               
CTAUTHX  DS    0H                                                               
*                                                                               
         TITLE 'READ B1X PROFILE FOR AGENCY AND MEDIA - RDB1X'                  
***********************************************************************         
*                                                                     *         
*        READ B1X PROFILE FOR AGENCY AND MEDIA                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDB1X    DS    0H                                                               
*                                                                               
*        READ B1X PROFILE                                                       
*                                                                               
         XC    CRSB1XPR,CRSB1XPR  READ B1X PROF                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          FORCE LOWERCASE                              
         MVC   WORK+4(2),PBILKAGY  AGENCY                                       
         MVC   WORK+6(1),PBILKMED  MEDIA                                        
*                                                                               
         GOTO1 VGETPROF,DMCB,WORK,CRSB1XPR,VDATAMGR                             
*                                                                               
RDB1XX   DS    0H                                                               
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
         TITLE 'FORMAT TRUE INVOICE NUMBER - SETINV'                            
***********************************************************************         
*                                                                     *         
*        FORMAT TRUE INVOICE NUMBER                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETINV   DS    0H                                                               
*                                                                               
         CLC   PBILIMO,=CL16' '    IF INVOICE MONTH AVAILABLE                   
         BNH   *+14                                                             
         MVC   WORK(2),PBILIMO        USE IT                                    
         B     SETINV30                                                         
*                                                                               
         MVC   WORK(2),PBILLDAT+2  BILL MONTH                                   
*                                                                               
         CLI   CRSB1XPR+4,0                                                     
         BE    SETINV20                                                         
*                                                                               
         PACK  DUB,PBILLDAT(2)    YEAR OF BILL                                  
         CVB   R0,DUB                                                           
         ZIC   RF,CRSB1XPR+4      INVOICE BASE YEAR                             
         SR    R0,RF              DIFFERENCE                                    
         BNP   SETINV20                                                         
*                                                                               
         MH    R0,=H'12'                                                        
         PACK  DUB,PBILLDAT+2(2)  BILL MONTH                                    
         CVB   RF,DUB                                                           
         AR    R0,RF                                                            
*                                                                               
SETINV10 CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB    SET NEW INVOICE MONTH                             
         B     SETINV30                                                         
*                                                                               
SETINV20 CLI   CRSB1XPR+5,0       SEE IF BUMPING INV MONTH                      
         BE    SETINV30                                                         
*                                                                               
         PACK  DUB,PBILLDAT+2(2)  MONTH                                         
         CVB   RF,DUB                                                           
         ZIC   R0,CRSB1XPR+5                                                    
         AR    R0,RF                                                            
*                                                                               
         CH    R0,=H'12'                                                        
         BNH   SETINV10                                                         
*                                                                               
         SH    R0,=H'12'                                                        
         B     SETINV10                                                         
*                                                                               
SETINV30 MVC   HALF,PBILKBNO        BILL NUMBER                                 
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(4),DUB                                                    
*                                                                               
         MVC   BINVOICE,WORK       SAVE ACTUAL INVOICE NUMBER                   
*                                                                               
SETINVX  DS    0H                                                               
*                                                                               
         TITLE 'SWITCH TO ACC SYSTEM - ACCSWT'                                  
***********************************************************************         
*                                                                     *         
*        SWITCH TO ACC SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ACCSWT   DS    0H                                                               
*                                                                               
         GOTO1 VSWITCH,DMCB,(CRSACCSE,0),0  SWITCH TO ACC SYSTEM                
*                                                                               
ACCSWTX  DS    0H                                                               
*                                                                               
         TITLE 'READ MEDIA POSTING RECORD(S) FOR GIVEN BILL - RDMPD'            
***********************************************************************         
*                                                                     *         
*        READ MEDIA POSTING RECORD(S) FOR GIVEN BILL                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDMPD    DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    MPDRECRG,KEY       ESTABLISH KEY AS MEDIAPOST DTL REC            
         USING MPDRECD,MPDRECRG                                                 
*                                                                               
         MVI   MPDKTYP,MPDKTYPQ   X'2F' - RECORD TYPE                           
         MVI   MPDKSUB,MPDKSUBQ   X'00' - RECORD SUB-TYPE                       
         MVC   MPDKCPY,CRSACCCD   NATIVE COMPANY CODE                           
         MVC   MPDKALPH,CRSALPH   AGY ALPHA FOR SPLIT MEDIA FILES               
         MVI   MPDKSYS,C'P'       SYSTEM                                        
         MVC   MPDKMED,PBILKMED   MEDIA                                         
         MVC   MPDKCLI,PBILKCLT   CLIENT                                        
         MVC   MPDKPRD,PBILKPRD   PRODUCT                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PBILKEST                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MPDKEST,DUB        ESTIMATE #                                    
*                                                                               
         MVC   FULL(2),PBILKBMN   (YM) BILLING MONTH                            
         MVI   FULL+3,1                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,FULL),(0,WORK)                                   
*                                                                               
         MVC   MPDKPER,WORK       SET BILLING MONTH (YYMM)                      
*                                                                               
         MVC   FULL,PBILKMOS      YM OF SERVICE                                 
         MVI   FULL+3,1                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,FULL),(0,WORK)                                   
*                                                                               
         MVC   MPDKMOS,WORK       MONTH OF SERVICE (YYMM)                       
*                                                                               
         EDIT  PBILKBNO,(5,MPDKINV),FILL=0                                      
*                                                                               
         MVC   MPDKEYSV,KEY        SAVE START KEY                               
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',KEY,KEY                  
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',KEY,KEY                
*                                                                               
RDMPDLP  DS    0H                                                               
*                                                                               
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BNE   CSHRCVER            MUST FIND A DETAIL RECORD                    
*                                                                               
         CLC   KEY(MPDKSEQ-MPDRECD),MPDKEYSV CHECK FOR END OF DETAILS           
         BNE   RDMPDDN                                                          
*                                                                               
         MVC   MPDKEYSV,KEY        SAVE FOUND KEY                               
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'ACCMST',KEY+50,AIO1,             
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=CL8'ACCMST',KEY+50,AIO1,  X        
               DMWRK                                                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST FIND RECORD                             
*                                                                               
         L     MPDRECRG,AIO1       NOW POINT TO RECORD                          
*                                                                               
         TITLE 'READ MEDIA POSTING RECORD(S) FOR GIVEN BILL - MBT'              
***********************************************************************         
*                                                                     *         
*        FIND BILL TRANSFER ELEMENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MBT      DS    0H                                                               
*                                                                               
         LA    MBTELDRG,MPDRFST    POINT TO FIRST ELEMENT                       
         USING MBTELD,MBTELDRG     ESTABLISH AS MEDIA BILL TRANS ELM            
*                                                                               
MBTLOOP  DS    0H                                                               
*                                                                               
         CLI   MBTEL,0             CHECK FOR END OF RECORD                      
         BE    MBTDONE                                                          
*                                                                               
         CLI   MBTEL,MBTELQ        IGNORE ALL BUT TRANSFER ELMS                 
         BNE   MBTCONT                                                          
         CLI   MBTTYP,MBTTRCV      IGNORE ALL BUT RECEIVABLES                   
         BNE   MBTCONT                                                          
*                                                                               
*                                                                               
         CP    MBTPOST,=P'0'       IGNORE ZERO POSTINGS                         
         BNE   *+12                                                             
         MVI   ACTVSW,X'FF'           INDICATE ONE TRANS ELM FOUND              
         B     MBTCONT                                                          
*                                                                               
*        IF BILLED VIA PRODUCTION MORE WORK IS NEEDED                           
*                                                                               
MBTPRD   DS    0H                                                               
*                                                                               
         CLI   MBTLDGR,C'J'        IF PRODUCTION BILLING SITUATION              
         BNE   MBTPRDN                                                          
*                                                                               
         BAS   RE,PRDBLL              MORE WORK NEEDED                          
         BNE   CSHRCVER               NOT PRODUCTION BILLED                     
         B     MBTCONT                                                          
*                                                                               
MBTPRDN  DS    0H                                                               
*                                                                               
*        BUILD TRANSACTION RECORD KEY                                           
*                                                                               
         XC    AKEY,AKEY                                                        
         LA    TRNRECRG,AKEY       ESTABLISH TRANSACTION RECORD KEY             
         USING TRNRECD,TRNRECRG                                                 
*                                                                               
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,CRSACCCD      COMPANY CODE                               
         MVC   TRNKULA,MBTULA        ACCOUNT                                    
         MVC   TRNKULC,MBTCNTRA      CONTRA ACCOUNT                             
         GOTO1 VDATCON,DMCB,(3,PBILINVD),(1,TRNKDATE)                           
         MVC   TRNKREF,BINVOICE      BILL REF #                                 
         MVI   TRNKSBR,0             SUB ZERO                                   
*                                                                               
         BAS   RE,RDTRA            READ TRANSACTION RECORDS                     
*                                                                               
         B     MBTCONT                                                          
*                                                                               
         TITLE 'CONTINUATION OF FINDING TRANSFER ELEMENTS - MBTCONT'            
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FINDING TRANSFER ELEMENTS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MBTCONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,MBTLLN           BUMP TO NEXT ELEMENT                         
         LA    MBTELDRG,MBTELD(RF)                                              
*                                                                               
         B     MBTLOOP                                                          
*                                                                               
MBTDONE  DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF READING TRANSFER RECORDS - RDMPDCN'             
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF READING TRANSFER RECORDS                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDMPDCN  DS    0H                                                               
*                                                                               
         LA    MPDRECRG,KEY        POINT TO KEY AREA                            
*                                                                               
         MVC   KEY,MPDKEYSV        RESTORE MEDIA POSTING RECORD                 
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',KEY,KEY RESET            
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',KEY,KEY RESET          
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',KEY,KEY NEXT DTL         
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRSEQ'),=CL8'ACCDIR',KEY,KEY                
*                                                                               
         B     RDMPDLP                                                          
*                                                                               
RDMPDDN  DS    0H                                                               
*                                                                               
         TITLE 'DETERMINE IF CASH HAS BEEN RECEIVED '                           
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FINDING TRANSFER ELEMENTS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   ACTVSW,0            ERROR IF NO TRANS ELMS FOUND                 
         BE    CSHRCVER                                                         
*                                                                               
         CP    ACCBAL,=P'0'        NOT ALL CASH RECEIVED IF NEGATIVE            
         BL    CSHRCVER                                                         
*                                                                               
         B     CSHRCVX             DONE                                         
*                                                                               
         TITLE 'ERROR ROUTINES '                                                
***********************************************************************         
*                                                                     *         
*        ERROR ROUTINES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHRCVER DS    0H                  CASH FOR BILL NOT RECEIVED                   
*                                                                               
         MVI   CRIND,CRIERRQ       INDICATE ERROR ON RETURN                     
         MVC   CRINV,BINVOICE      RETURN INVOICE NUMBER                        
*                                                                               
         TITLE 'EXIT ROUTINES   '                                               
***********************************************************************         
*                                                                     *         
*        ERROR ROUTINES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHRCVX  DS    0H                                                               
*                                                                               
         GOTO1 VSWITCH,DMCB,=C'PRINT',0   SWITCH BACK TO PRINT                  
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'READ TRANSACTION RECORD - RDTRA'                                
***********************************************************************         
*                                                                     *         
*        READ TRANSACTION RECORD(S) FOR GIVEN BILL                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDTRA    NTR1                                                                   
*                                                                               
         MVC   TRNKEYSV,AKEY         SAVE TRANSACTIION KEY BUILD                
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
RDTRALP  DS    0H                                                               
*                                                                               
         CLC   AKEY(TRNKSBR-TRNKEY),TRNKEYSV  DONE IF RECORD NOT FOUND          
         BNE   RDTRADN                                                          
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'ACCMST',AKEY+50,AIO2,            
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=CL8'ACCMST',AKEY+50,AIO2, X        
               DMWRK                                                            
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     TRNRECRG,AIO2       POINT TO FOUND RECORD                        
*                                                                               
         TITLE 'READ TRANSACTION ELEMENTS -TRN'                                 
***********************************************************************         
*                                                                     *         
*        READ TRANSACTION ELEMENTS AND ACCUMULATE POSTED AMOUNTS      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRN      DS    0H                                                               
*                                                                               
         LA    TRNELDRG,TRNRFST    POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
TRNLOOP  DS    0H                                                               
*                                                                               
         USING TRNELD,TRNELDRG     ESTABLISH AS OTHER TYPE ELEMENT              
*                                                                               
         CLI   TRNEL,0            END OF RECORD?                                
         BE    TRNDONE                                                          
*                                                                               
         CLI   TRNEL,TRNELQ       LOOK FOR TRANSACTION ELEMENT                  
         BNE   TRNCONT                                                          
*                                                                               
         MVI   ACTVSW,X'FF'       INDICATE ONE TRANSACTION ELM FOUND            
*                                                                               
         TM    TRNSTAT,TRNSDR     IF DEBIT                                      
         BNO   *+14                                                             
         SP    ACCBAL,TRNAMNT          SUBTRACT FROM ACCOUNT BALANCE            
         B     *+10                                                             
         AP    ACCBAL,TRNAMNT     ELSE ADD      TO   ACCOUNT BALANCE            
*                                                                               
TRNCONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,TRNLN            BUMP TO NEXT ELEMENT                         
         LA    TRNELDRG,TRNELD(RF)                                              
*                                                                               
         B     TRNLOOP                                                          
*                                                                               
TRNDONE  DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF READING TRANSACTION RECORDS - RDTRACN'          
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF READING TRANSACTION RECORDS                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDTRACN  DS    0H                                                               
*                                                                               
         LA    TRNRECRG,AKEY       POINT TO KEY AREA                            
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRSEQ'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
         B     RDTRALP                                                          
*                                                                               
RDTRADN  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PRODUCTION BILLING -PRDBLL'                                     
***********************************************************************         
*                                                                     *         
*        PRODUCTION BILLING                                           *         
*                                                                     *         
*        FIND PRODUCTION RECEIVABLE AND CONTRA ACCOUNTS               *         
*                                                                     *         
*NTRY    MBTELDRG  ==> MBTELD - MEDIA TRANSFER ELEMENT                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDBLL   NTR1                                                                   
*                                                                               
***********************************************************************         
*                                                                     *         
*        FIND PRODUCTION RECEIVABLE ACCOUNT                           *         
*                                                                     *         
*        READ ACCOUNT RECORD TO FIND RECEIVABLE ACCOUNT CODE          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         XC    AKEY,AKEY                                                        
         LA    ACTRECRG,AKEY       ESTABLISH ACCOUNT RECORD KEY                 
         USING ACTRECD,ACTRECRG                                                 
*                                                                               
         MVC   ACTKEY,SPACES         INIT KEY                                   
         MVC   ACTKCPY,CRSACCCD      COMPANY CODE                               
         MVC   ACTKULA,MBTULA        ACCOUNT                                    
*                                                                               
         CLC   ACTKEYSV,AKEY       IF ORIGINAL ACCOUNT KEY UNCHANGED            
         BE    RDACTDN                SKIP READING RECORD                       
*                                                                               
         MVC   ACTKEYSV,AKEY       SAVE ORIGINAL ACCOUNT KEY BUILD              
*                                                                               
RDACTLP  DS    0H                                                               
*                                                                               
         MVC   AKEYSAVE,AKEY         SAVE ACCOUNT KEY BUILD                     
*                                    READ ACCOUNT RECORD                        
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
         CLC   AKEY(L'ACTKEY),AKEYSAVE  SKIP IF RECORD NOT FOUND                
         BNE   RDACTCN                                                          
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'ACCMST',AKEY+50,AIO3,            
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=CL8'ACCMST',AKEY+50,AIO3, X        
               DMWRK                                                            
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     ACTRECRG,AIO3       POINT TO FOUND RECORD                        
*                                                                               
         TITLE 'READ PRODUCTION PROFILE ELEMENT - PPR'                          
***********************************************************************         
*                                                                     *         
*        READ PRODUCTION PROFILE ELEMENT                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PPR      DS    0H                                                               
*                                                                               
         LA    PPRELDRG,ACTRFST    POINT TO FIRST ELM IN ACCOUNT RECORD         
*                                                                               
PPRLOOP  DS    0H                                                               
*                                                                               
         USING PPRELD,PPRELDRG    ESTABLISH AS PROD PROFILE ELEMENT             
*                                                                               
         CLI   PPREL,0            END OF RECORD?                                
         BE    PPRDONE                                                          
*                                                                               
         CLI   PPREL,PPRELQ       LOOK FOR PROD PROFILE ELEMENT                 
         BNE   PPRCONT                                                          
*                                                                               
         CLC   PPRRECV,SPACES     IF RECEIVABLE ACCOUNT PRESENT                 
         BNH   PPRCONT                                                          
*                                                                               
         MVC   PPRELM,PPRELD         SAVE ELEMENT                               
         B     RDACTDN               ACCOUNT RECORD PROCESSING DONE             
*                                                                               
PPRCONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PPRLN            BUMP TO NEXT ELEMENT                         
         LA    PPRELDRG,PPRELD(RF)                                              
*                                                                               
         B     PPRLOOP                                                          
*                                                                               
PPRDONE  DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF READING ACCOUNT RECORDS - RDACTCN'              
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF READING ACCOUNT RECORDS                      *         
*              ACCOUNT CODE IS CONSTRUCTED AS FOLLOWS                 *         
*              CL3'CLIENT CODE'                                       *         
*              CL3'PRODUCT CODE'                                      *         
*              CL6'JOB CODE'                                          *         
*              IF NO PRODUCTION PROFILE RECORD IS FOUND AT ONE        *         
*                 LEVEL THEN ERASE KEY COMPONENT AND TRY AT NEXT      *         
*                 HIGHER LEVEL                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDACTCN  DS    0H                                                               
*                                                                               
         MVC   AKEY,AKEYSAVE       RESTORE ASKED FOR KEY                        
         LA    ACTRECRG,AKEY       POINT TO KEY AREA                            
*                                                                               
         CLC   ACTKACT+6(6),SPACES IF LAST KEY WAS FOR JOB LEVEL                
         BNH   *+14                                                             
         MVC   ACTKACT+6(6),SPACES    ERASE JOB CODE                            
         B     RDACTLP                AND GO READ AT PRODUCT LEVEL              
*                                                                               
         CLC   ACTKACT+3(3),SPACES IF LAST KEY WAS FOR PRODUCT LEVEL            
         BNH   *+14                                                             
         MVC   ACTKACT+3(3),SPACES    ERASE PRODUCT CODE                        
         B     RDACTLP                AND GO READ AT CLIENT LEVEL               
*                                                                               
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
RDACTDN  DS    0H                                                               
*                                                                               
         TITLE 'READ PRODUCTION MEDIA RECORD - PMD'                             
***********************************************************************         
*                                                                     *         
*        READ PRODUCTION MEDIA RECORD TO FIND CONTRA ACCOUNT CODE     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDPMD    DS    0H                                                               
*                                                                               
         XC    AKEY,AKEY                                                        
         LA    PMDRECRG,AKEY       ESTABLISH PROD MEDIA RECORD KEY              
         USING PMDRECD,PMDRECRG                                                 
*                                                                               
         MVC   PMDKEY,SPACES         INIT KEY                                   
         MVI   PMDKTYP,PMDKTYPQ      PRODUCTION MEDIA RECORD TYPE               
         MVC   PMDKCPY,CRSACCCD      COMPANY CODE                               
         MVC   PMDKMED,MBTACCT+6     MEDIA CODE IS FIRST CH OF JOB CODE         
*                                                                               
         CLC   PMDKEYSV,AKEY       IF ORIGINAL PROD MEDIA KEY UNCHANGED         
         BE    RDPMDDN                SKIP READING RECORD                       
*                                                                               
         MVC   PMDKEYSV,AKEY       SAVE ORIGINAL ACCOUNT KEY BUILD              
*                                                                               
*                                    READ PROD MEDIA RECORD                     
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
         CLC   AKEY(L'PMDKEY),PMDKEYSV  MUST FIND KEY                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'ACCMST',AKEY+50,AIO3,            
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=CL8'ACCMST',AKEY+50,AIO3, X        
               DMWRK                                                            
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     PMDRECRG,AIO3       POINT TO FOUND RECORD                        
*                                                                               
         TITLE 'READ PRODUCTION MEDIA ELEMENT - PMD'                            
***********************************************************************         
*                                                                     *         
*        READ PRODUCTION MEDIA ELEMENT                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PMD      DS    0H                                                               
*                                                                               
         LA    PMDELDRG,PMDRFST    POINT TO FIRST ELM IN PROD MEDIA REC         
*                                                                               
PMDLOOP  DS    0H                                                               
*                                                                               
         USING PMDELD,PMDELDRG     ESTABLISH AS PROD MEDIA ELEMENT              
*                                                                               
         CLI   PMDEL,0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
         CLI   PMDEL,PMDELQ       LOOK FOR PROD MEDIA ELEMENT                   
         BNE   PMDCONT                                                          
*                                                                               
         MVC   PMDELM,PMDELD         SAVE ELEMENT                               
*                                                                               
         B     RDPMDDN               PROD MEDIA RECORD PROCESSING DONE          
*                                                                               
PMDCONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PMDLN            BUMP TO NEXT ELEMENT                         
         LA    PMDELDRG,PMDELD(RF)                                              
*                                                                               
         B     PMDLOOP                                                          
*                                                                               
PMDDONE  DS    0H                                                               
*                                                                               
RDPMDDN  DS    0H                                                               
*                                                                               
         TITLE 'FIND PRODUCTION REFERNCE NUMBER - PRDREF'                       
***********************************************************************         
*                                                                     *         
*        FIND PRODUCTION POSTING INFORMATION                          *         
*                                                                     *         
*        READ TRANSACTION RECORD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDPST   DS    0H                                                               
*                                                                               
         XC    AKEY,AKEY                                                        
         LA    TRNRECRG,AKEY       ESTABLISH TRANSACTION RECORD KEY             
         USING TRNRECD,TRNRECRG                                                 
*                                                                               
         MVC   TRNKEY,SPACES         INIT KEY                                   
         MVC   TRNKCPY,CRSACCCD      COMPANY CODE                               
         MVC   TRNKULA,MBTULA        ACCOUNT                                    
         MVC   TRNKWORK,MBTOFFC      WORK CODE                                  
         MVC   TRNKCCPY,CRSACCCD     CONTRA COMPANY CODE                        
         MVC   TRNKULC,MBTCNTRA      CONTRA ACCOUNT                             
         GOTO1 VDATCON,DMCB,(3,PBILINVD),(1,TRNKDATE)                           
         MVC   TRNKREF,BINVOICE      BILL REF #                                 
         MVI   TRNKSBR,0             SUB ZERO                                   
*                                                                               
         MVC   PSTKEYSV,AKEY         SAVE POSTING KEY BUILD                     
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
*                                    READ ACCOUNT RECORD                        
PRDPSTLP DS    0H                                                               
*                                                                               
         CLC   AKEY(TRNKSBR-TRNKEY),PSTKEYSV  MUST FIND A RECORD                
         BNE   PRDPSTDN                                                         
*                                                                               
         MVC   PSTKEYSV,AKEY         SAVE POSTING KEY BUILD                     
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'ACCMST',AKEY+50,AIO3,            
         GOTO1 VDATAMGR,DMCB,(0,=C'GETREC'),=CL8'ACCMST',AKEY+50,AIO3, X        
               DMWRK                                                            
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     TRNRECRG,AIO3       POINT TO FOUND RECORD                        
*                                                                               
         TITLE 'FIND BILL NUMBER ELEMENT - BND'                                 
***********************************************************************         
*                                                                     *         
*        FIND BILL NUMBER ELEMENT                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BND      DS    0H                                                               
*                                                                               
         LA    BNDELDRG,TRNRFST    POINT TO FIRST ELM IN TRANS RECORD           
*                                                                               
BNDLOOP  DS    0H                                                               
*                                                                               
         USING BNDELD,BNDELDRG     ESTABLISH AS BILL NUMBER/DATE ELM            
*                                                                               
         CLI   BNDEL,0             END OF RECORD?                               
         BE    BNDDONE                                                          
*                                                                               
         CLI   BNDEL,BNDELQ        LOOK FOR BILL NUMBER ELEMENT                 
         BNE   BNDBNDN                                                          
*                                                                               
         OC    BNDAMNT,BNDAMNT     SKIP IF AMOUNT IS ZERO                       
         BZ    BNDCONT                                                          
*                                                                               
         CLC   BNDBNO,SPACES       NOT BILLED IF NO NUMBER                      
         BNH   PRDBLLER                                                         
*                                                                               
         MVC   SVBNO,BNDBNO          SAVE BILL NUMBER                           
         MVC   SVBDTE,BNDDTE         SAVE BILL DATE                             
*                                                                               
         B     BNDFND                                                           
*                                                                               
BNDBNDN  DS    0H                                                               
*                                                                               
         USING PTAELD,PTAELDRG     ESTABLISH PROD TRAN ACTIVITY ELM             
*                                                                               
         CLI   PTAEL,PTAELQ        PRODUCTION TRANSACTION ACTIVITY              
         BNE   BNDCONT                                                          
*                                                                               
         CLI   PTATYPE,PTATRAL     MUST BE ALLOCATION TO BILL                   
         BNE   BNDCONT                                                          
*                                                                               
         MVC   SVBNO,PTARBLNO        SAVE BILL NUMBER                           
         MVC   SVBDTE,PTARBLDT       SAVE BILL DATE                             
*                                                                               
BNDFND   DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        READ TRANSACTION RECORD(S) FOR GIVEN BILL                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDTRA   DS    0H                                                               
*                                                                               
         XC    AKEY,AKEY                                                        
         LA    TRNRECRG,AKEY       ESTABLISH TRANSACTION RECORD KEY             
         USING TRNRECD,TRNRECRG                                                 
*                                                                               
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY(15),PPRRECV-PPRELD+PPRELM RECEIVABLE ACCOUNT             
         MVC   TRNKCACT,PMDDESC-PMDELD+PMDELM   CONTRA ACCOUNT                  
         LA    RF,SVBDTE                        PROD BILLING DATE               
         GOTO1 VDATCON,DMCB,(2,0(RF)),(1,TRNKDATE)                              
         MVC   TRNKREF,SVBNO                    BILL REF #                      
         MVI   TRNKSBR,0                        SUB ZERO                        
*                                                                               
         BAS   RE,RDTRA            READ TRANSACTION RECORDS                     
*                                                                               
PRDTRAX  DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF READING BILL NUMBER ELMS - BNDCONT'             
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF BILL NUMBER ELEMENTS                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
BNDCONT  DS    0H                                                               
*                                                                               
         USING BNDELD,BNDELDRG     RE-ESTABLISH BILL NUMBER ELM                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BNDLN            BUMP TO NEXT ELEMENT                         
         LA    BNDELDRG,BNDELD(RF)                                              
*                                                                               
         B     BNDLOOP                                                          
*                                                                               
BNDDONE  DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF READING POSTING RECORDS - PRDPSTDN'             
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF READING TRANSACTION RECORDS FOR POSTING      *         
*              INFORMATION                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDPSTCN DS    0H                                                               
*                                                                               
         MVC   AKEY,PSTKEYSV       RESTORE LAST KEY READ                        
*                                  RE-POINT FILE                                
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
         LA    TRNRECRG,AKEY       POINT TO KEY AREA                            
*                                                                               
*NOP*    GOTO1 VDATAMGR,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRSEQ'),=CL8'ACCDIR',AKEY,AKEY,    X        
               DMWRK                                                            
*                                                                               
         B     PRDPSTLP                                                         
*                                                                               
PRDPSTDN DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     PRDBLLX                                                          
*                                                                               
         TITLE 'EXIT ROUTINES - PRDBLLX'                                        
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDBLLER DS    0H                  INVOICE NOT PRODUCTION BILLED                
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
         B     PRDBLLX                                                          
*                                                                               
PRDBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'CONSTANTS AND LITERAL POOL'                                     
***********************************************************************         
*                                                                     *         
*        CONSTANTS AND LITERAL POOL                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPACES   DC    CL80' '             SPACES                                       
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'CASH RECEIVED CONTROL BLOCK - CSHRCVD'                          
       ++INCLUDE PPCSHRCVD                                                      
*                                                                               
         TITLE 'CASH RECEIVED WORKING STORAGE - CSHRCVWD'                       
***********************************************************************         
*                                                                     *         
*        CASH RECEIVED WORKING STORAGE - CSHRCVWD                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHRCVWD DSECT                                                                  
DUB      DS    D                   WORK AREAS                                   
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
WORK     DS    CL80                                                             
*                                                                               
DMCB     DS    6A                                                               
*                                                                               
DMWRK    DS    XL512               DATAMGR WORKAREA                             
*                                                                               
KEY      DS    CL64                KEY BUILD AREA                               
KEYSAVE  DS    CL64                KEY BUILD SAVEAREA                           
*                                                                               
AKEY     DS    CL64                KEY BUILD AREA                               
AKEYSAVE DS    CL64                KEY BUILD SAVEAREA                           
*                                                                               
CRWPARMS DS    0A                  PARAMETER LIST                               
CRWCBLKA DS    A                   A(CSHRCV CONTROL BLOCK)                      
CRWBRECA DS    A                   A(PRINT BILL RECORD)                         
CRWPARML EQU   *-CRWPARMS          LENGTH OF PARAMETER LIST                     
*                                                                               
VDATAMGR DS    V                   V(DATAMGR)                                   
VDATCON  DS    V                   V(DATCON)                                    
VSWITCH  DS    V                   V(SWITCH)                                    
VGETPROF DS    V                   V(GETPROF)                                   
*                                                                               
AIO1     DS    A                   A(IOAREA1)                                   
AIO2     DS    A                   A(IOAREA2)                                   
AIO3     DS    A                   A(IOAREA3)                                   
*                                                                               
ACCBAL   DS    PL8                 ACCOUNT BALANCE                              
ACTVSW   DS    X                   NON-NULLS MEANS TRANS ELM FOUND              
*                                                                               
ACCCD    DS    CL1                 COMPANY CODE                                 
ACCSE    DS    XL1                 ACC POWER CODE                               
ACCSALPH DS    CL2                 NOT USED                                     
*                                                                               
BINVOICE DS    CL6                 TRUE INVOICE NUMBER                          
*                                                                               
PSTKEYSV DS    XL(L'TRNKEY)        POSTING     KEY        SAVEAREA              
TRNKEYSV DS    XL(L'TRNKEY)        TRANSACTION KEY        SAVEAREA              
MPDKEYSV DS    XL(L'MPDKEY)        MEDIA POSTING KEY      SAVEAREA              
BNDELM   DS    XL(BNDLN2Q)         BILL NUMBER/DATE   ELM SAVEAREA              
PMDKEYSV DS    XL(L'PMDKEY)        PRODUCTION KEY         SAVEAREA              
PMDELM   DS    XL(PMDLN2Q)         PRODUCTION MEDIA   ELM SAVEAREA              
ACTKEYSV DS    XL(L'ACTKEY)        ACCOUNT    KEY         SAVEAREA              
PPRELM   DS    XL(PMDLN2Q)         PRODUCTION PROFILE ELM SAVEAREA              
*                                                                               
SVBNO    DS    CL(L'BNDBNO)        BILL NUMBER SAVEAREA                         
SVBDTE   DS    XL(L'BNDDTE)        BILL DATE SAVEAREA                           
*                                                                               
IOAREA1  DS    CL4000              IOAREA1                                      
IOAREA2  DS    CL4000              IOAREA2                                      
IOAREA3  DS    CL4000              IOAREA3                                      
*                                                                               
CSHRCVWL EQU   *-CSHRCVWD          LENGTH OF WORKING STORAGE                    
*                                                                               
         TITLE 'PRINT BILLING RECORD - PBILLREC'                                
***********************************************************************         
*                                                                     *         
*        PRINT BILLING RECORD - PBILLREC                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PBILLRCD DSECT BILLING RECORD                                                   
PBILKIDQ EQU   C'08'               BILLING RECORD ID                            
       ++INCLUDE PBILLREC                                                       
*                                                                               
         TITLE 'OTHER INCLUDED DSECTS'                                          
***********************************************************************         
*                                                                     *         
*        OTHER INCLUDED DSECTS                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  CTGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*  COMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPPAY04   01/30/12'                                      
         END                                                                    
