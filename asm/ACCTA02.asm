*          DATA SET ACCTA02    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T61E02A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCTA02 -- CTAGS DIS/CHA                                       
*                                                                     *         
*  COMMENTS:     MAINTAINS MADIA CONTRACT RECORDS                     *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61E00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCTAF2 (MAINT)                              *         
*                                                                     *         
*  OUTPUTS:      UPDATED CONTRACT RECORDS                             *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T61E02 - CTAGS DIS/CHA'                                         
T61E02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1E02**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61EFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         BAS   RE,SETUP            ANY INITIALIZING                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         MVI   STATBYTE,STKEYCHG   ONLY HERE IF KEY CHANGES                     
*                                                                               
         LA    R2,CTAMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    ERRPLS                                                           
         CLI   5(R2),2             MUST BE TWO CHARACTERS                       
         BNE   ERRMEDIA            INVALID MEDIA                                
         MVC   ACCNT,SPACES        VALIDATE ACCOUNT RECS EXIST                  
         MVC   ACCNT(2),8(R2)                                                   
         GOTO1 GTLEVNM,DMCB,C'S1',ACCNT,0                                       
         BNE   ERRNOS1                                                          
         GOTO1 GTLEVNM,DMCB,C'S2',ACCNT,0                                       
         BNE   ERRNOS2                                                          
         CLI   8(R2),C'S'          1ST CHAR IS SYSTEM                           
         BNE   ERRMEDIA                                                         
         MVC   QSYS,8(R2)                                                       
         MVC   QMED,9(R2)          2ND CHAR IS MEDIA                            
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VALMED              VALIDATE MEDIA                               
         BNE   ERRMEDIA                                                         
*                                                                               
         LA    R2,CTACONTH         VALIDATE SPOT CONTRACT NUMBER                
         CLI   5(R2),0                                                          
         BE    ERRPLS                                                           
         MVC   AIO,AIO2            READ SPOT CONTRACT RECORD INTO AIO2          
         GOTO1 VALCON#                                                          
         BNE   ERRNOCON            MEDIA CONTRACT DOESN'T EXIST                 
         L     R6,AIO                                                           
         MVC   SPOTKEY,0(R6)       SAVE SPOT CONTRACT KEY                       
         GOTO1 VALCNTR             GET CONTRACTOR NAME                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   CTACNTR,CONTRCTR    CONTRACTOR                                   
         OI    CTACNTRH+6,X'80'                                                 
         MVC   CTASTNS,CNTRNAME    CONTRACTOR NAME? (OR STATIONS)               
         OI    CTASTNSH+6,X'80'                                                 
         EDIT  (P6,GCIAMT),(11,CTATGCI),2,MINUS=YES     GCI AND NCI             
         OI    CTATGCIH+6,X'80'    XMIT                                         
         EDIT  (P6,NCIAMT),(11,CTATNCI),2,MINUS=YES                             
         OI    CTATNCIH+6,X'80'    XMIT                                         
         EDIT  (P4,NCIPCT),(6,CTACOMM),2,MINUS=YES                              
         OI    CTACOMMH+6,X'80'    XMIT                                         
*                                                                               
         MVC   CTASTAT,SPACES      DISPLAY STATUS                               
         MVC   CTASTAT(4),=C'OPEN'                                              
         OI    CTASTATH+6,X'80'                                                 
         TM    CONSTAT,CSLOCK      IS CONTRACT LOCKED                           
         BNO   *+10                                                             
         MVC   CTASTAT(6),=C'LOCKED'                                            
         TM    CONSTAT,CSCLOSE     IS CONTRACT CLOSED                           
         BNO   *+10                                                             
         MVC   CTASTAT(6),=C'CLOSED'                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR ACC CONTRACT RECORD                                *         
***********************************************************************         
*                                                                               
         GOTO1 ACCSYS              SWITCH BACK TO ACC SYSTEM                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTRECD,R6          BUILD CONTRACT ACCOUNT REC                   
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'        UL= S2                                       
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED          SYSTEM AND MEDIA             
         MVC   ACTKACT+L'QSYSMED(L'CON#),CON#      CONTRACT NUMBER              
         MVC   SAVEKEY,BIGKEY                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    VKX                                                              
         OI    STATBYTE,STADDREC   WILL NEED TO ADD RECORD                      
         CLI   ACTEQU,ACTDIS                                                    
         BE    ERECNF              RECORD NOT FOUND                             
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALREC - GET RECORD IF ONE EXISTS                            *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         CLI   ACTEQU,ACTDIS       DISPLAY                                      
         BE    VR01                                                             
         TM    CONSTAT,CSCLOSE     CAN'T CHANGE CLOSED CONTRACT                 
         BO    ERRNOCHG                                                         
         BAS   RE,VALSTAT          VALIDATE STATUS BEFORE READING REC           
*                                                                               
VR01     L     RE,AIO              CLEAR IO AREA                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),SAVEKEY   PRESET IN CASE OF ADD                   
         TM    STATBYTE,STADDREC        DOES A RECORD EXIST                     
         BO    VR10                     NO                                      
*                                                                               
         XC    BIGKEY,BIGKEY            MAKE SURE HAVE RIGHT REC                
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         CLI   ACTEQU,ACTDIS                                                    
         BE    *+8                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         CLI   ACTEQU,ACTDIS       DISPLAY                                      
         BE    DR                                                               
         TM    STATBYTE,STKEYCHG   KEY CHANGE                                   
         BO    DR                                                               
         EJECT                                                                  
***********************************************************************         
*        VALREC - VALIDATE CATEGORY LINES                             *         
***********************************************************************         
*                                                                               
VR10     ZAP   ACCGCI,=P'0'        INIT TOTALS                                  
         ZAP   ACCNCI,=P'0'                                                     
         ZAP   ACCNPAY,=P'0'                                                    
*                                                                               
         BAS   RE,VALCOMM          VALIDATE COMMISSION                          
*                                                                               
*        TM    STATBYTE,STADDREC   DOES A RECORD EXIST                          
*        BNO   VR12                                                             
*        LA    R2,CTAFRSTH                                                      
*        CLI   5(R2),0                                                          
*        BE    ERRPLS              PLEASE ENTER                                 
*                                                                               
         USING CTABD,R3                                                         
VR12     LA    R3,CATTAB           CATEGORY TABLE                               
VR15     ZICM  R4,CTLINE,2         DISP TO CATEGORY'S SCREEN LINE               
         AR    R4,RA               FROM RA                                      
         MVC   CATWC,CTWORKCD      CATEGORY'S WORK CODE                         
         BAS   RE,VALLINE          VALIDATE LINE - UPDATES X'93' ELEMS          
         LA    R3,CTLENQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   VR15                                                             
         CP    ACCGCI,MGBUY                                                     
         BL    ERRLGCI             GCI LESS THAN MEDIA BOUGHT                   
*                                                                               
         CP    ACCGCI,=P'0'        ANY DATA                                     
         BH    VR20                                                             
         LA    R2,CTAFRSTH                                                      
         B     ERRPLS              PLEASE ENTER                                 
*                                                                               
VR20     BAS   RE,ELEM92           UPDATE CONTRACT ELEM = X'92'                 
         BAS   RE,ELEM3E           UPDATE COMMENT ELEMS = X'3E'                 
*                                                                               
         MVC   BYTE,CNTRNMLN       USING CONTRACTOR NAME FOR S2 AND S1          
         MVC   BLOCK(L'CNTRNAME),CNTRNAME                                       
         BAS   RE,ELEM20           UPDATE NAME ELEM = X'20'                     
*                                                                               
         BAS   RE,ELEM30           ADD REC STATUS ELEM  = X'30'                 
         BAS   RE,ELEM31           ADD ACCOUNT STATUS ELEM  = X'31'             
         BAS   RE,ELEM32           ADD BALANCE ELEM  = X'32'                    
         BAS   RE,ELEM33           ADD PEEL ELEM  = X'33'                       
         TM    CONSTAT,CSCLOSE     CLOSE ACCOUNT                                
         BNO   *+12                                                             
         MVI   EL95STAT,EL95FROM   TRANSFER IS FROM THIS CONTRACT               
         BAS   RE,ELEM95           ADD TRANSFER ELEM ON CLOSE                   
*                                                                               
         USING ACTRECD,R6                                                       
         L     R6,AIO                                                           
         NI    ACTRSTAT,X'FF'-(ACTSLOCK+ACTSCLOS)                               
         CLC   ACTKUNT(2),=C'S1'                                                
         BE    VR30                                                             
         TM    CONSTAT,CSLOCK      LOCK ACCOUNT                                 
         BNO   *+8                                                              
         OI    ACTRSTAT,ACTSLOCK                                                
VR30     TM    CONSTAT,CSCLOSE     CLOSE ACCOUNT                                
         BNO   *+8                                                              
         OI    ACTRSTAT,ACTSCLOS                                                
         LA    R6,BIGKEY                                                        
         NI    ACTKSTAT,X'FF'-(ACTSLOCK+ACTSCLOS)                               
         CLC   ACTKUNT(2),=C'S1'                                                
         BE    VR40                                                             
         TM    CONSTAT,CSLOCK      LOCK ACCOUNT                                 
         BNO   *+8                                                              
         OI    ACTKSTAT,ACTSLOCK                                                
VR40     TM    CONSTAT,CSCLOSE     CLOSE ACCOUNT                                
         BNO   *+8                                                              
         OI    ACTKSTAT,ACTSCLOS                                                
         DROP  R6                                                               
*                                                                               
         TM    STATBYTE,STADDREC                                                
         BNO   VRWRITE                                                          
         GOTO1 ADDREC                                                           
         NI    STATBYTE,X'FF'-STADDREC                                          
         B     VR100                                                            
VRWRITE  GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
*                                                                               
VR100    MVC   AIO,AIO2                                                         
         BAS   RE,CONTRUPD         UPDATE CONTRACTOR ACCOUNT REC                
         BAS   RE,CONHEADR         UPDATE CONTRA ACCOUNT HEADER                 
         TM    CONSTAT,CSCLOSE     CLOSE ACCOUNT                                
         BNO   *+8                                                              
         BAS   RE,TRANSUPD         UPDATE 'TO' CONTRACT FOR TRANSFER            
         BAS   RE,SPOTCON          UPDATE SPOT CONTRACT RECORD                  
         MVC   AIO,AIO1                                                         
*                                                                               
VRX      B     DR                  DISPLAY REC CHANGES                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE COMMISSION PERCENT                                            
***********************************************************************         
*                                                                               
VALCOMM  NTR1                                                                   
         TM    CTACOMMH+4,X'80'    INPUT THIS TIME                              
         BNO   VCX                                                              
         LA    R2,CTACOMMH                                                      
         ZICM  R5,CTACOMMH+5,1                                                  
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',CTACOMM),(R5)                                
         CLI   DMCB,X'FF'          VALID                                        
         BE    ERRCOMM             INVALID COMMISSION %                         
         ZAP   NCIPCT,DMCB+4(8)                                                 
         CP    NCIPCT,=P'0'                                                     
         BE    ERRCOMM             INVALID COMMISSION %                         
VCX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE STATUS                                                        
***********************************************************************         
*                                                                               
VALSTAT  NTR1                                                                   
         TM    CTASTATH+4,X'80'    INPUT THIS TIME                              
         BNO   VSX                                                              
         LA    R2,CTASTATH                                                      
         ZICM  R5,CTASTATH+5,1                                                  
         BZ    ERRMISS                                                          
         SH    R5,=H'1'                                                         
         BM    ERRMISS                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   CTASTAT(0),=C'OPEN'                                              
         BE    VS20                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   CTASTAT(0),=C'LOCKED'                                            
         BE    VS30                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   CTASTAT(0),=C'CLOSED'                                            
         BE    VS40                                                             
         B     ERRINV                  INVALID STATUS                           
*                                                                               
VS20     TM    CONSTAT,CSCLOSE         CAN'T OPEN IF CLOSED                     
         BO    ERRNOCHG                                                         
         NI    CONSTAT,X'FF'-CSLOCK    UNLOCK IF LOCKED                         
         B     VSX                                                              
*                                                                               
VS30     TM    CONSTAT,CSCLOSE         CAN'T LOCK IF CLOSED                     
         BO    ERRNOCHG                                                         
         OI    CONSTAT,CSLOCK          LOCK CONTRACT                            
         B     VSX                                                              
*                                                                               
VS40     OI    CONSTAT,CSCLOSE     CLOSE CONTRACT                               
         BAS   RE,VALSPCLS         MEDIA ELIGIBILITY FOR CLOSE                  
         BAS   RE,VALACCLS         SERVICES ELIGIBILITY FOR CLOSE               
         B     VSX                                                              
*                                                                               
VSX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MEDIA CONTRACT ELIGIBLE FOR CLOSING                           
***********************************************************************         
*                                                                               
VALSPCLS NTR1                                                                   
         CP    MGBUY,MGPAID        MEDIA BOUGHT MUST = MEDIA PAID               
         BNE   ERRALLPD                                                         
*                                                                               
         ZAP   MXFRAMT,GCIAMT                                                   
         SP    MXFRAMT,MGPAID       AMOUNT TO TRANSFER                          
*                                                                               
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTARECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       LOOK FOR NEXT CONTRACT                       
         MVI   CTCPTYP,CTCPTYPQ    X'0D'                                        
         MVI   CTCPSUB,CTCPSUBQ    X'FD'                                        
         MVC   CTCPAGMD,SBAGYMD    AGENCY MEDIA                                 
         MVC   CTCPCNTR,CONTRCTR   CONTRACTOR                                   
         MVC   WORK1+1(L'PCON#),PCON#                                           
         MVI   WORK1,X'00'                                                      
         L     R1,NINES            GET NEXT CONTRACT                            
         S     R1,WORK1                                                         
         STCM  R1,7,CTCPCNUM                                                    
VSCLS10  LA    R6,BIGKEY                                                        
         MVI   CTCPCNUM+3,X'FF'                                                 
         GOTO1 HIGH                SPOT READ                                    
         CLC   BIGKEY(CTCPCNUM-CTCPASS),KEYSAVE                                 
         BNE   ERRNOXFR            NO MATCH FOUND                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   SPXFRKEY,0(R6)                                                   
         TM    CTARCNTL,CTARCLOC+CTARCCLS     LOCKED OR CLOSED                  
         BNZ   VSCLS10                                                          
         MVC   MXFRCON#,CTAKCNUM              TO CONTRACT NUMBER                
         ZAP   WORK1,=P'999999'                                                 
         ZAP   WORK2(4),=P'0'                                                   
         MVO   WORK2(4),CTAKCNUM              UNPACK                            
         SP    WORK1,WORK2(4)                                                   
         EDIT  (P4,WORK1),(5,SXFRCON#)     TO CONTRACT NUMBER                   
*                                                                               
         USING CTAUSELD,R6                                                      
         ZAP   WORK(6),=P'0'                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,CTAUSELQ     X'06' USAGE ELEMENT                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VSCLS20  BAS   RE,NEXTEL                                                        
         BNE   VSCLS30                                                          
         ZICM  R1,CTAUSPGR,4                                                    
         CVD   R1,DUB                                                           
         AP    WORK(6),DUB(8)      MEDIA GROSS PAID TOTAL                       
         B     VSCLS20                                                          
VSCLS30  CP    MXFRAMT,WORK(6)                                                  
         BH    VSCLS10             CAN'T XFR MORE THAN NEW CON PAID             
*                                                                               
VSCLSX   GOTO1 ACCSYS              SWITCH BACK TO ACC                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ACCOUNT CONTRACT ELIGIBLE FOR CLOSING                         
***********************************************************************         
*                                                                               
VALACCLS NTR1                                                                   
         TM    STATBYTE,STADDREC                                                
         BO    ERRNOXFR            CAN'T CLOSE NOTHING                          
         XC    BIGKEY,BIGKEY       READ CONTRACT                                
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         USING CNTELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    CNTOPNPO,=P'0'      NO OPEN ORDERS                               
         BNE   ERROPORD                                                         
         ZAP   SXFRAMT,CNTNPAY                                                  
         SP    SXFRAMT,CNTINVPO    AMOUNT OF SERVICES TO TRANSFER               
         CLI   CNTLN,CNTLN2Q                                                    
         BL    VACLS05                                                          
         SP    SXFRAMT,CNTBINV     ANY INVOICED BAL B/F                         
*                                                                               
VACLS05  LA    R4,XFRCTGS                                                       
         XC    XFRCTGS,XFRCTGS     VALID CATEGORIES                             
         USING CTGELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VACLS10  BAS   RE,NEXTEL                                                        
         BNE   VACLS15                                                          
         MVC   0(L'CTGCTGY,R4),CTGCTGY                                          
         LA    R4,L'CTGCTGY(R4)                                                 
         B     VACLS10                                                          
*                                                                               
         USING ACTRECD,R6          GET TO CONTRACT                              
VACLS15  LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'        UL= S2                                       
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED          SYSTEM AND MEDIA             
         MVC   ACTKACT+L'QSYSMED(L'CON#),SXFRCON#  CONTRACT NUMBER              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ERRNOXFR                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING CNTELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   WORK(6),CNTINVPO                                                 
         CLI   CNTLN,CNTLN2Q                                                    
         BL    *+10                                                             
         AP    WORK(6),CNTBINV     ADD IN INV'D BAL B/F                         
         CP    SXFRAMT,WORK(6)     XFR AMT LESS THAN INVOICED                   
         BH    ERRNOXFR            NO CONTRACT TO TRANSFER TO                   
*                                                                               
         USING CTGELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VACLS20  BAS   RE,NEXTEL                                                        
         BNE   ERRNOXFR            NO MATCHING CATEGORY                         
         LA    R4,XFRCTGS                                                       
VACLS30  CLC   0(L'CTGCTGY,R4),CTGCTGY                                          
         BE    VACLSX                                                           
         LA    R4,L'CTGCTGY(R4)                                                 
         CLI   0(R4),0                                                          
         BE    VACLS20                                                          
         B     VACLS30                                                          
*                                                                               
VACLSX   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CATEGORY SCREEN LINE AT R4                                    
***********************************************************************         
*                                                                               
         USING CLINED,R4                                                        
         USING CTGELD,R6                                                        
VALLINE  NTR1                                                                   
         ZAP   CATGCI,=P'0'                                                     
         BAS   RE,ELEM93           POINTS R6 TO CATEGORY ELEM                   
*                                                                               
         LA    R2,CLGCIH                                                        
         ZICM  R5,CLGCIH+5,1       NO GCI                                       
         BNZ   VL10                                                             
*                                                                               
         TM    STATBYTE,STADDEL    IS ELEM ON RECORD                            
         BO    VLX                 NO THEN DONE                                 
         CP    CTGOPNPO,=P'0'      ANY OPEN PO'S                                
         BNE   ERRNODEL            CANNOT DELETE - ORDERS EXIST                 
         CP    CTGINVPO,=P'0'      ANY INVOICED PO'S                            
         BNE   ERRNODEL            CANNOT DELETE - ORDERS EXIST                 
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE ELEMENT                               
         B     VLX                                                              
*                                                                               
VL10     GOTO1 CASHVAL,DMCB,(X'82',CLGCI),(R5)                                  
         CLI   DMCB,X'FF'          VALID                                        
         BE    ERRAMNT             INVALID AMOUNT                               
         ZAP   CATGCI,DMCB+4(8)                                                 
         AP    ACCGCI,CATGCI       UPDATE TOTALS                                
*                                                                               
         ZAP   WORK(10),CATGCI     NCI=GCI*NCI%                                 
         MP    WORK(10),NCIPCT                                                  
         SRP   WORK(10),64-4,5                                                  
         ZAP   CATNCI,WORK(10)                                                  
         AP    ACCNCI,CATNCI       UPDATE TOTALS                                
*                                                                               
         ZAP   WORK(10),CTGOPNPO   OPEN PO'S                                    
         AP    WORK(10),CTGINVPO   PLUS INVOICED PO'S                           
         CP    WORK(10),CATNCI                                                  
         BH    ERRLNCI             NCI LESS THAN ORDERED                        
*                                                                               
         LA    R2,CLNPAYH          VALIDATE NET PAYABLE                         
         ZICM  R5,CLNPAYH+5                                                     
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',CLNPAY),(R5)                                 
         CLI   DMCB,X'FF'          VALID                                        
         BE    ERRAMNT             INVALID AMOUNT                               
         ZAP   CATNPAY,DMCB+4(8)                                                
         AP    ACCNPAY,CATNPAY                                                  
*                                                                               
         LA    R2,CLCPCTH          VALIDATE EST COST PERCENT                    
         ZICM  R5,CLCPCTH+5,1                                                   
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',CLCPCT),(R5)                                 
         CLI   DMCB,X'FF'          VALID                                        
         BE    ERRAMNT             INVALID AMOUNT                               
         ZAP   CATCPCT,DMCB+4(8)                                                
*                                                                               
         ZAP   CTGGCI,CATGCI       UPDATE VALUES IN ELEM                        
         ZAP   CTGNCI,CATNCI                                                    
         ZAP   CTGNPAY,CATNPAY                                                  
         ZAP   CTGCPCT,CATCPCT                                                  
         TM    STATBYTE,STADDEL    ADD ELEM IF NEEDED                           
         BNO   VLX                                                              
         GOTO1 ADDELEM                                                          
*                                                                               
VLX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        FIND X'93' CATEGORY ELEMENT IN AIO OR MAKE NEW ONE IN ELEM             
*        RETURNS R6                                                             
***********************************************************************         
*                                                                               
         USING CTGELD,R6                                                        
ELEM93   NTR1                                                                   
         NI    STATBYTE,X'FF'-STADDEL                                           
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
EL93NX   BAS   RE,NEXTEL                                                        
         BNE   EL93ADD                                                          
         CLC   CATWC,CTGCTGY       MATCH ON CATEGORY=WORK CODE                  
         BNE   EL93NX                                                           
         B     EL93X                                                            
*                                                                               
EL93ADD  LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CTGEL,CTGELQ        X'93'                                        
         MVI   CTGLN,CTGLNQ                                                     
         MVC   CTGCTGY,CATWC       WORK CODE                                    
         ZAP   CTGOPNPO,=P'0'                                                   
         ZAP   CTGINVPO,=P'0'                                                   
         ZAP   CTGCOST,=P'0'                                                    
         OI    STATBYTE,STADDEL    FLAG TO ADD ELEMENT                          
*                                                                               
EL93X    XIT1  REGS=(R6)           RETURN 93 ELEM IN R6                         
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'92' CONTRACT TOTALS ELEMENT                                   
***********************************************************************         
*                                                                               
         USING CNTELD,R6                                                        
ELEM92   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92'                                        
         BAS   RE,GETEL                                                         
         BNE   EL92ADD                                                          
         ZAP   CNTGCI,ACCGCI                                                    
         ZAP   CNTNCI,ACCNCI                                                    
         ZAP   CNTNPAY,ACCNPAY                                                  
         B     EL92X                                                            
*                                                                               
EL92ADD  LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CNTEL,CNTELQ        X'92'                                        
         MVI   CNTLN,CNTLNQ                                                     
         MVC   CNTCONTR,CONTRCTR                                                
         ZAP   CNTGCI,ACCGCI                                                    
         ZAP   CNTNCI,ACCNCI                                                    
         ZAP   CNTNPAY,ACCNPAY                                                  
         ZAP   CNTOPNPO,=P'0'                                                   
         ZAP   CNTINVPO,=P'0'                                                   
         ZAP   CNTCOST,=P'0'                                                    
         GOTO1 ADDELEM                                                          
EL92X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'95' CONTRACT TRANSFER ELEMENT                                 
***********************************************************************         
*                                                                               
         USING CXFELD,R6                                                        
ELEM95   NTR1                                                                   
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CXFEL,CXFELQ        X'95'                                        
         MVI   CXFLN,CXFLNQ                                                     
         MVC   CXFCON,SXFRCON#     REFERENCE TRANSFER CONTRACT                  
         MVI   CXFSTAT,CXFSPAID    DOLLARS ARE PAID                             
         CLI   EL95STAT,EL95FROM   CREATE FROM ELEM?                            
         BE    EL95A                                                            
         MVC   CXFCON,CON#         REFERNCE TRANSFER CONTRACT                   
         MVI   CXFSTAT,CXFSAVL     DOLLARS ARE AVAILABLE                        
EL95A    ZAP   CXFAMNT,SXFRAMT     TRANSFER AMOUNT                              
         GOTO1 ADDELEM                                                          
EL95X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'3E' STANDARD COMMENT ELEMENTS                                 
***********************************************************************         
*                                                                               
ELEM3E   NTR1                                                                   
         MVI   ELCODE,SCMELQ       X'3E'                                        
         GOTO1 REMELEM             REMOVE                                       
*                                                                               
         LA    R3,COMMTAB                                                       
EL3E10   ZICM  R2,1(R3),2          DISP TO COMMENT LINE                         
         AR    R2,RA                                                            
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'1'            SET R1 FOR MOVE                              
         BM    EL3ENX                                                           
*                                                                               
         USING SCMELD,R6                                                        
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SCMEL,SCMELQ        X'3E'                                        
         MVC   SCMSEQ,0(R3)        SEQUENCE NUMBER                              
         EX    R1,*+4                                                           
         MVC   SCMNARR(0),8(R2)                                                 
         LA    R1,SCMLN1Q+1(R1)                                                 
         STC   R1,SCMLN                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
EL3ENX   LA    R3,3(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   EL3E10                                                           
EL3EX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'20' - NAME ELEMENT                                            
*                       USE NAME IN BLOCK FOR LENGTH IN BYTE                    
***********************************************************************         
*                                                                               
         USING NAMELD,R6                                                        
ELEM20   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,NAMELQ       X'20'                                        
         GOTO1 REMELEM             REMOVE                                       
*                                                                               
         LA    R6,ELEM             ADD                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ        X'20'                                        
         ZIC   R1,BYTE                                                          
         CH    R1,=H'36'           MAX OF CL36                                  
         BNH   *+8                                                              
         LA    R1,36                                                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   NAMEREC(0),BLOCK                                                 
         LA    R1,NAMLN1Q+1(R1)                                                 
         STC   R1,NAMLN                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
EL20X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'22' - ADDRESS ELEMENT                                         
*                       CONTRACTOR ADDRESS ELEM IN CNTRADR                      
***********************************************************************         
*                                                                               
         USING ADRELD,R6                                                        
ELEM22   NTR1                                                                   
         CLI   CNTRADR,ADRELQ      MAKE SURE HAVE X'22' ELEM                    
         BNE   EL22X                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,ADRELQ       X'22'                                        
         GOTO1 REMELEM             REMOVE                                       
*                                                                               
         XC    ELEM,ELEM           ADD NEW ELEM                                 
         ZIC   R1,CNTRADR+1        ELEM 22 LEN                                  
         SH    R1,=H'1'                                                         
         BM    EL22X                                                            
         EX    R1,*+4                                                           
         MVC   ELEM(0),CNTRADR                                                  
         GOTO1 ADDELEM                                                          
EL22X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD X'30' -  RECORD STATUS ELEMENT IF NOT THERE                        
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING ACTRECD,RF                                                       
ELEM30   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,RSTELQ       X'30'                                        
         BAS   RE,GETEL                                                         
         BNE   EL30B                                                            
         NI    RSTSTAT1,X'FF'-RSTSACIL                                          
         L     RF,AIO                                                           
         CLC   ACTKUNT(2),=C'S1'           DON'T LOCK CONTRACTOR                
         BE    EL30X                                                            
         TM    CONSTAT,CSLOCK                                                   
         BNO   *+8                                                              
         OI    RSTSTAT1,RSTSACIL   ACCOUNT IS LOCKED                            
         B     EL30X                                                            
*                                                                               
EL30B    LA    R6,ELEM             ADD IF NOT THERE                             
         XC    ELEM,ELEM                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
         L     RF,AIO                                                           
         CLC   ACTKUNT(2),=C'S1'           DON'T LOCK CONTRACTOR                
         BE    EL30D                                                            
         TM    CONSTAT,CSLOCK                                                   
         BNO   *+8                                                              
         OI    RSTSTAT1,RSTSACIL   ACCOUNT IS LOCKED                            
*                                                                               
EL30D    MVC   RSTFILT1,SPACES     UPDATE FILTERS                               
         MVC   RSTFILT2,SPACES                                                  
         MVC   RSTFILT3,SPACES                                                  
         MVC   RSTFILT4,SPACES                                                  
         MVC   RSTFILT5,SPACES                                                  
*                                                                               
         GOTO1 ADDELEM                                                          
EL30X    B     XIT                                                              
         DROP  R6,RF                                                            
         EJECT                                                                  
***********************************************************************         
*        ADD X'31' ACCOUNT STATUS ELEM IF ONE DOESN'T EXIST ON REC              
***********************************************************************         
*                                                                               
         USING ASTELD,R6                                                        
ELEM31   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,ASTELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'31' EXISTS                   
         BE    EL31X                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ASTEL,ASTELQ        X'31'                                        
         MVI   ASTLN,ASTLN1Q                                                    
         GOTO1 ADDELEM                                                          
EL31X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD X'32' ACCOUNT BALANCE ELEM IF ONE DOESN'T EXIST ON REC             
***********************************************************************         
*                                                                               
         USING ABLELD,R6                                                        
ELEM32   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'32' EXISTS                   
         BE    EL32X                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ABLEL,ABLELQ        X'32'                                        
         MVI   ABLLN,ABLLN2Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         GOTO1 ADDELEM                                                          
EL32X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD X'33' ACCOUNT PEEL-OFF ELEM IF ONE DOESN'T EXIST ON REC            
***********************************************************************         
*                                                                               
         USING APOELD,R6                                                        
ELEM33   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,APOELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'33' EXISTS                   
         BE    EL33X                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   APOEL,APOELQ        X'33'                                        
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         GOTO1 ADDELEM                                                          
EL33X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE CONTRACTOR ACCOUNT RECORD    UL=S1                              
***********************************************************************         
*                                                                               
         USING ACTRECD,R6          BUILD CONTRACTOR ACCOUNT REC                 
CONTRUPD NTR1                                                                   
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'        UL= S1                                       
         MVI   ACTKLDG,C'1'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED                SYSTEM/MEDIA           
         MVC   ACTKACT+L'QSYSMED(L'CONTRCTR),CONTRCTR    CONTRACTOR             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   CU20                GO ADD                                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         B     CU50                UPDATE NAME ELEM                             
*                                                                               
CU20     L     RE,AIO              CLEAR IO AREA                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),KEYSAVE                                           
         OI    STATBYTE,STADDREC        NEED TO ADD                             
*                                                                               
CU50     MVC   BYTE,CNTRNMLN       CONTRACTOR NAME                              
         MVC   BLOCK(L'CNTRNAME),CNTRNAME                                       
         BAS   RE,ELEM20           UPDATE NAME ELEM = X'20'                     
         BAS   RE,ELEM22           UPDATE ADR ELEM = X'22'                      
*                                                                               
         BAS   RE,ELEM30           ADD REC STATUS ELEM  = X'30'                 
         BAS   RE,ELEM31           ADD ACCOUNT STATUS ELEM  = X'31'             
         BAS   RE,ELEM32           ADD BALANCE ELEM  = X'32'                    
         BAS   RE,ELEM33           ADD PEEL ELEM  = X'33'                       
         TM    STATBYTE,STADDREC                                                
         BNO   CUWRITE                                                          
         GOTO1 ADDREC                                                           
         NI    STATBYTE,X'FF'-STADDREC                                          
         B     CUX                                                              
CUWRITE  GOTO1 PUTREC                                                           
CUX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE CONTRA ACCOUNT HEADER RECORD  S1-S2                             
***********************************************************************         
*                                                                               
         USING CHDRECD,R6          BUILD CONTRACTOR ACCOUNT REC                 
CONHEADR NTR1                                                                   
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   CHDKCPY,CMPY                                                     
         MVI   CHDKUNT,C'S'        UL= S1                                       
         MVI   CHDKLDG,C'1'                                                     
         MVC   CHDKACT(L'QSYSMED),QSYSMED                SYSTEM/MEDIA           
         MVC   CHDKACT+L'QSYSMED(L'CONTRCTR),CONTRCTR    CONTRACTOR             
         MVC   CHDKCCPY,CMPY                                                    
         MVI   CHDKCUNT,C'S'       UL= S2                                       
         MVI   CHDKCLDG,C'2'                                                    
         MVC   CHDKCACT(L'QSYSMED),QSYSMED               SYSTEM/MEDIA           
         MVC   CHDKCACT+L'QSYSMED(L'CON#),CON#           CONTRACT               
         XC    CHDKNULL,CHDKNULL                                                
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   CH20                GO ADD                                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         B     CH50                UPDATE NAME                                  
*                                                                               
CH20     L     RE,AIO              CLEAR IO AREA                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),KEYSAVE                                           
         OI    STATBYTE,STADDREC        NEED TO ADD                             
*                                                                               
CH50     L     R6,AIO                                                           
         MVI   ELCODE,CACELQ       X'43' - CONTRA ACCNT HEADER ELEM             
         GOTO1 REMELEM                                                          
*                                                                               
         USING CACELD,R4                                                        
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CACEL,CACELQ        X'43'                                        
         MVC   CACCNT,CHDKCULC     S2 CONTRACT ACCOUNT                          
*                                                                               
         ZIC   R1,CONTNMLN         LENGTH OF CONTRACT NAME                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   CACNAME(0),CONTNAME                                              
         LA    R1,CACLN1Q+1(R1)                                                 
         STC   R1,CACLN                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
         TM    STATBYTE,STADDREC                                                
         BNO   CHWRITE                                                          
         GOTO1 ADDREC                                                           
         NI    STATBYTE,X'FF'-STADDREC                                          
         B     CHX                                                              
CHWRITE  GOTO1 PUTREC                                                           
CHX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        UPDATE 'TO' CONTRACT ON TRANSFER                                       
***********************************************************************         
*                                                                               
         USING ACTRECD,R6          GET TO CONTRACT                              
TRANSUPD NTR1                                                                   
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'        UL= S2                                       
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED          SYSTEM AND MEDIA             
         MVC   ACTKACT+L'QSYSMED(L'CON#),SXFRCON#  CONTRACT NUMBER              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVI   EL95STAT,EL95TO     TRANSFER IS TO THIS CONTRACT                 
         BAS   RE,ELEM95           ADD TRANSFER ELEM ON CLOSE                   
         GOTO1 PUTREC                                                           
TUX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE SPOT CONTRACT RECORD                                            
***********************************************************************         
*                                                                               
SPOTCON  NTR1                                                                   
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZAP   GCIAMT,ACCGCI       UPDATE MEDIA AMOUNTS                         
         ZAP   DUB,ACCGCI                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,NEWGCI                                                     
         ZAP   NCIAMT,ACCNCI                                                    
         ZAP   DUB,ACCNCI                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,NEWNCI                                                     
         XC    NEWPCT,NEWPCT                                                    
         CP    NCIPCT,=P'10000'                                                 
         BE    SC10                                                             
         ZAP   DUB,NCIPCT                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,NEWPCT                                                     
*                                                                               
SC10     MVC   AIO,AIO2            MARK SPOT CONTRACT RECORD                    
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SPOTKEY),SPOTKEY                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'SPOTKEY),KEYSAVE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING CTARECD,R6                                                       
         L     R6,AIO2                                                          
         OI    CTARCNTL,CTARCACC   ACCOUNT CONTRACT REC EXISTS                  
         NI    CTARCNTL,X'FF'-CTARCLOC                                          
         TM    CONSTAT,CSLOCK      CONTRACT LOCKED                              
         BNO   *+8                                                              
         OI    CTARCNTL,CTARCLOC                                                
         TM    CONSTAT,CSCLOSE     CONTRACT CLOSED                              
         BNO   *+8                                                              
         OI    CTARCNTL,CTARCCLS                                                
         TM    CONSTAT,CSDOLL      PARTICIPANTS BY DOLLARS                      
         BNO   SC20                                                             
         CLC   NEWGCI,MEDGCI       HAS GCI CHANGED                              
         BE    *+8                                                              
         OI    CTARCNTL,CTARCBAL   PARTICIPANTS OUT OF BALANCE                  
*                                                                               
         USING CTDSCEL,R6                                                       
SC20     MVI   ELCODE,CTDSCELQ     X'01'DESCRIPTION ELEM                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CTDSCGCI,NEWGCI     UPDATE GCI                                   
         MVC   CTDSCNCI,NEWNCI     AND NCI                                      
         MVC   CTDSCCOM,NEWPCT     AND NCI PCT                                  
*                                                                               
         TM    CONSTAT,CSPCT       PARTICIPANTS BY PERCENT                      
         BNO   SC40                                                             
*                                                                               
         USING CTPARD,R6                                                        
         MVI   ELCODE,CTPARELQ     X'05' PARTICIPANTS ELEM                      
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
SC30     BAS   RE,NEXTEL                                                        
         BNE   SC40                                                             
         ICM   RF,15,NEWGCI                                                     
         ICM   R1,15,CTPARPCT                                                   
         MR    RE,R1               GCI X PARICIPANT %                           
         D     RE,=F'10000'        GET RID OF FOUR DECIMAL PLACES               
         STCM  RF,15,CTPARAMT                                                   
         B     SC30                                                             
*                                                                               
SC40     TM    CONSTAT,CSCLOSE                                                  
         BNO   SC50                                                             
         USING CTAXFELD,R6                                                      
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CTAXFEL,CTAXFELQ    X'07'                                        
         MVI   CTAXFELN,CTAXFLNQ                                                
         MVC   CTAXFCON,MXFRCON#   REFERENCE TRANSFER CONTRACT                  
         OI    CTAXSTAT,CTAXSTPD   DOLLARS ARE PAID                             
         ZAP   DUB,MXFRAMT                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,CTAXFOGR      TRANSFER AMOUNT                              
         GOTO1 ADDELEM                                                          
*                                                                               
SC50     GOTO1 PUTREC                                                           
*                                                                               
         USING CTARECD,R6                                                       
         LA    R6,BIGKEY                                                        
         OI    CTAKCNTL,CTARCACC   ACCOUNT CONTRACT REC EXISTS                  
         NI    CTAKCNTL,X'FF'-CTAKCLOC                                          
         TM    CONSTAT,CSLOCK      CONTRACT LOCKED                              
         BNO   *+8                                                              
         OI    CTAKCNTL,CTAKCLOC                                                
         TM    CONSTAT,CSCLOSE     CONTRACT CLOSED                              
         BNO   *+8                                                              
         OI    CTAKCNTL,CTAKCCLS                                                
         TM    CONSTAT,CSDOLL      PARTICIPANTS BY DOLLARS                      
         BNO   SC80                                                             
         CLC   NEWGCI,MEDGCI       HAS GCI CHANGED                              
         BE    *+8                                                              
         OI    CTAKCNTL,CTAKCBAL   PARTICIPANTS OUT OF BALANCE                  
SC80     GOTO1 WRITE                                                            
*                                                                               
         TM    CONSTAT,CSCLOSE                                                  
         BNO   SC100                                                            
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SPXFRKEY),SPXFRKEY                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'SPXFRKEY),KEYSAVE                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING CTAXFELD,R6                                                      
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CTAXFEL,CTAXFELQ    X'07'                                        
         MVI   CTAXFELN,CTAXFLNQ                                                
         MVC   CTAXFCON,PCON#      REFERENCE TRANSFER CONTRACT                  
         OI    CTAXSTAT,CTAXSTAV   DOLLARS ARE AVAILABLE                        
         ZAP   DUB,MXFRAMT                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,CTAXFOGR      TRANSFER AMOUNT                              
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
*                                                                               
SC100    MVC   AIO,AIO1                                                         
         GOTO1 ACCSYS              SWITCH BACK TO ACC SYSTEM                    
         BE    *+6                                                              
         DC    H'0'                                                             
SCX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPREC - DISPLAY CATEGORY LINES                             *         
***********************************************************************         
*                                                                               
         USING CTABD,R3                                                         
DR       DS    0H                                                               
         NI    STATBYTE,X'FF'-STKEYCHG   RESET KEY CHANGE                       
         ZAP   ACCGCI,=P'0'        INIT TOTALS                                  
         ZAP   ACCNCI,=P'0'                                                     
         ZAP   ACCNPAY,=P'0'                                                    
         ZAP   ACCDOLL,=P'0'                                                    
*                                                                               
         LA    R3,CATTAB           CATEGORY TABLE                               
DR15     ZICM  R4,CTLINE,2         DISP TO CATEGORY'S SCREEN LINE               
         AR    R4,RA               FROM RA                                      
         MVC   CATWC,CTWORKCD      CATEGORY'S WORK CODE                         
         BAS   RE,DISLINE          DISPLAY  LINE                                
         LA    R3,CTLENQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   DR15                                                             
         BAS   RE,DISP92           DISPLAY TOTALS  ELEM = X'92'                 
         BAS   RE,DISPCOMS         DISPLAY COMMENTS                             
DRX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY CATEGORY SCREEN LINE AT R4                                     
***********************************************************************         
*                                                                               
         USING CLINED,R4                                                        
DISLINE  NTR1                                                                   
         LR    R2,R4               CLEAR LINE                                   
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               SKIP FIRST FIELD                             
         LA    R3,FLDSLINE         #FIELDS PER LINE                             
DL10     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         LA    R2,9(R1,R2)                                                      
         BCT   R3,DL10                                                          
*                                                                               
         USING CTGELD,R6                                                        
         BAS   RE,ELEM93           RETURNS CAT ELEM AT R6                       
         TM    STATBYTE,STADDEL    IS THERE AN ELEM                             
         BO    DLX                 NO                                           
*                                                                               
         EDIT  (P6,CTGGCI),(11,CLGCI),2,MINUS=YES GCI                           
         AP    ACCGCI,CTGGCI                                                    
*                                                                               
         EDIT  (P6,CTGNCI),(11,CLNCI),2,MINUS=YES NCI                           
         AP    ACCNCI,CTGNCI                                                    
*                                                                               
         OC    CTGNPAY,CTGNPAY                                                  
         BNZ   DL11                                                             
         ZAP   WORK(10),CTGNCI                                                  
         MP    WORK(10),=P'100'                                                 
         DP    WORK(10),CTGRATIO                                                
         ZAP   CTGNPAY,WORK(6)                                                  
         B     DL12                                                             
DL11     EDIT  (P6,CTGNPAY),(11,CLNPAY),2,MINUS=YES NET PAYABLE                 
         AP    ACCNPAY,CTGNPAY                                                  
*                                                                               
DL12     EDIT  (P4,CTGCPCT),(6,CLCPCT),2    COST %                              
*                                                                               
         ZAP   WORK2(16),CTGNPAY            EST DOLLARS = NET PAY*COST%         
         MP    WORK2(16),CTGCPCT                                                
         SRP   WORK2(16),64-4,5                                                 
         EDIT  (P16,WORK2),(11,CLDOLL),2,MINUS=YES                              
         AP    ACCDOLL,WORK2(16)                                                
*                                                                               
         MP    WORK2(16),=P'10000'                                              
         DP    WORK2(16),CTGGCI             EST GCI% = EST DOLLARS/GCI          
         EDIT  (P10,WORK2),(6,CLGCIP),2,MINUS=YES                               
*                                                                               
DLX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY X'92' CONTRACT TOTALS ELEMENT                                  
***********************************************************************         
*                                                                               
         USING CLINED,R4                                                        
         USING CNTELD,R6                                                        
DISP92   NTR1                                                                   
         LA    R4,CTATOTSH                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    ACCGCI,CNTGCI       DOUBLE CHECK TOTALS                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    ACCNCI,CNTNCI                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EDIT  (P6,ACCGCI),(11,CLGCI),2,MINUS=YES                               
         OI    CLGCIH+6,X'80'                                                   
         EDIT  (P6,ACCNCI),(11,CLNCI),2,MINUS=YES                               
         OI    CLNCIH+6,X'80'                                                   
         EDIT  (P6,ACCNPAY),(11,CLNPAY),2,MINUS=YES                             
         OI    CLNPAYH+6,X'80'                                                  
         EDIT  (P6,ACCDOLL),(11,CLDOLL),2,MINUS=YES                             
         OI    CLDOLLH+6,X'80'                                                  
         ZAP   WORK2(10),ACCDOLL                                                
         MP    WORK2(10),=P'10000'                                              
         DP    WORK2(10),ACCGCI             EST GCI% = EST DOLLARS/GCI          
         EDIT  (P4,WORK2),(6,CLGCIP),2,MINUS=YES                                
         OI    CLGCIPH+6,X'80'                                                  
*                                                                               
         EDIT  (P6,GCIAMT),(11,CTATGCI),2,MINUS=YES   HEADLINE GCI              
         OI    CTATGCIH+6,X'80'    XMIT                                         
         EDIT  (P6,NCIAMT),(11,CTATNCI),2,MINUS=YES                             
         OI    CTATNCIH+6,X'80'    XMIT                                         
         EDIT  (P4,NCIPCT),(6,CTACOMM),2                                        
         OI    CTACOMMH+6,X'80'    XMIT                                         
D92X     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY X'3E' COMMENT ELEMENTS                                         
***********************************************************************         
*                                                                               
DISPCOMS NTR1                                                                   
         LA    R3,COMMTAB                                                       
DC10     ZICM  R2,1(R3),2          DISP TO COMMENT LINE                         
         AR    R2,RA               FROM RA                                      
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         BM    DC20NX                                                           
         EX    R1,*+4                                                           
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         USING SCMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,SCMELQ       X'3E' STANDARD COMMENTS                      
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
DC10NX   BAS   RE,NEXTEL                                                        
         BNE   DC20NX                                                           
         CLC   SCMSEQ,0(R3)        MATCH ON COMMENT SEQ NUMBER                  
         BNE   DC10NX                                                           
         ZIC   R4,SCMLN                                                         
         SH    R4,=Y(SCMLN1Q+1)                                                 
         BM    DC20NX                                                           
         CR    R4,R1               IS COMMENT LONGER THAN FIELD                 
         BNH   *+6                                                              
         LR    R4,R1               THEN USE MAX FIELD LEN                       
         EX    R4,*+4                                                           
         MVC   8(0,R2),SCMNARR                                                  
*                                                                               
DC20NX   LA    R3,3(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   DC10                                                             
DCX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    SU10                                                             
         ST    RF,VGLOBBER                                                      
*                                                                               
         TM    PRGSTAT,GLOBCALL    CALLED WITH GLOBBER                          
         BNO   SU10                                                             
         TM    PRGSTAT,GLOBELEM    ALREADY GOT ELEMS                            
         BO    SU10                                                             
         OI    PRGSTAT,GLOBELEM    SET READ ELEMS                               
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,1,GLVSPMD                                
         CLI   8(R1),0                                                          
         BNE   SU10                LEAVE ELEMS FOR RETURN                       
         MVC   CTAMED(1),GLOBFRSY                                               
         MVC   CTAMED+1(1),ELEM                                                 
         MVI   CTAMEDH+5,2                                                      
         OI    CTAMEDH+6,X'80'                                                  
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',CTACONTH,5,GLVSPCON                           
         CLI   8(R1),0                                                          
         BNE   SU10                LEAVE ELEMS FOR RETURN                       
*                                                                               
SU10     LA    R3,CTAPFKYH                                                      
         LA    R2,PFTABLE                                                       
         GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
SUX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ERRORS AND RANDOM STUFF                                      *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
ERECNF   MVC   GERROR,=AL2(ACERECNF)   RECORD NOT FOUND                         
         B     ACCERRX                                                          
ERRAMNT  MVC   GERROR,=AL2(ACEAMNT)    INVALID AMOUNT                           
         B     ACCERRX                                                          
ERRMEDIA MVC   GERROR,=AL2(ACEINVMD)   INVALID MEDIA                            
         B     ACCERRX                                                          
ERRNOS1  MVC   GERROR,=AL2(ACENOS1)    S1 ACCOUNT DOESN'T EXIST                 
         B     ACCERRX                                                          
ERRNOS2  MVC   GERROR,=AL2(ACENOS2)    S2 ACCOUNT DOESN'T EXIST                 
         B     ACCERRX                                                          
ERRNOCON MVC   GERROR,=AL2(ACENOCON)   MEDIA CONTRACT DOESN'T EXIST             
         B     ACCERRX                                                          
ERRLGCI  MVC   GERROR,=AL2(ACELGCI)    GCI LESS THAN MEDIA BOUGHT               
         B     ACCERRX                                                          
ERRLNCI  MVC   GERROR,=AL2(ACELNCI)    NCI LESS THAN ORDERED                    
         B     ACCERRX                                                          
ERRNODEL MVC   GERROR,=AL2(ACENODEL)   CANNOT DELETE - ORDERS EXIST             
         B     ACCERRX                                                          
ERRCOMM  MVC   GERROR,=AL2(ACEINVCM)   INVALID COMMISSION %                     
         B     ACCERRX                                                          
ERRNOXFR MVC   GERROR,=AL2(ACENOXFR)   NO CONTRACT TO TRANS TO                  
         B     ACCERRX                                                          
ERRNOCHG MVC   GERROR,=AL2(ACECLOSE)   CANNOT CHANGE CLOSED CONTRACT            
         B     ACCERRX                                                          
ERROPORD MVC   GERROR,=AL2(ACEOPORD)   CONTRACT HAS OPEN ORDERS                 
         B     ACCERRX                                                          
ERRALLPD MVC   GERROR,=AL2(ACEALLPD)   MEDIA BOUGHT /= PAID                     
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         DS    F                                                                
NINES    DC    X'00999999'                                                      
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CATEGORY TABLE                                                         
***********************************************************************         
*                                                                               
CATTAB   DC    CL2'MD',AL2(CTAMDSEH-T61EFFD)                                    
         DC    CL2'AX',AL2(CTAAMEXH-T61EFFD)                                    
         DC    CL2'CA',AL2(CTACASHH-T61EFFD)                                    
         DC    CL2'HL',AL2(CTAHOTLH-T61EFFD)                                    
         DC    CL2'SC',AL2(CTASCPTH-T61EFFD)                                    
         DC    CL2'OR',AL2(CTAOTHRH-T61EFFD)                                    
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        COMMENTS TABLE                                                         
***********************************************************************         
*                                                                               
COMMTAB  DC    XL1'01',AL2(CTACOM1H-T61EFFD)                                    
         DC    XL1'02',AL2(CTACOM2H-T61EFFD)                                    
         DC    XL1'03',AL2(CTACOM3H-T61EFFD)                                    
         DC    XL1'04',AL2(CTACOM4H-T61EFFD)                                    
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE DEFINITIONS                                                
***********************************************************************         
*                                                                               
PFTABLE  DS    0C                                                               
         DC    AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
PF03     DC    AL1(KEYTYTWA,L'CTAMED-1),AL2(CTAMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'CTACNTR-1),AL2(CTACNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'CTACONT-1),AL2(CTACONT-T61EFFD)                   
PF03X    EQU   *                                                                
*                                                                               
         DC    AL1(PF04X-*,04,PFTCPROG,(PF04X-PF04)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'DISPLAY '                               
PF04     DC    AL1(KEYTYTWA,L'CTAMED-1),AL2(CTAMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'CTACNTR-1),AL2(CTACNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'CTACONT-1),AL2(CTACONT-T61EFFD)                   
PF04X    EQU   *                                                                
*                                                                               
         DC    AL1(PF05X-*,05,PFTCPROG,(PF05X-PF05)/KEYLNQ,0)                   
         DC    CL3' ',CL8'ORDER   ',CL8'ADD     '                               
PF05     DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'CTAMED-1),AL2(CTAMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'CTACONT-1),AL2(CTACONT-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'CTACNTR-1),AL2(CTACNTR-T61EFFD)                   
PF05X    EQU   *                                                                
*                                                                               
         DC    AL1(PF09X-*,09,PFTCPROG,(PF09X-PF09)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'BALANCE '                               
PF09     DC    AL1(KEYTYTWA,L'CTAMED-1),AL2(CTAMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'CTACNTR-1),AL2(CTACNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'CTACONT-1),AL2(CTACONT-T61EFFD)                   
PF09X    EQU   *                                                                
*                                                                               
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SCREEENS                                                     *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCTAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF2D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                          *         
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SAVERC   DS    F                   SAVED RC                                     
STADDR   DS    F                   START ADDRESS                                
WORK2    DS    4F                                                               
*                                                                               
STATBYTE DS    XL1                 STATUS BYTE                                  
STADDREC EQU   X'80'               NEED TO ADD ACC CONTRACT REC                 
STKEYCHG EQU   X'40'               KEY CHANGE - REDISPLAY                       
STADDEL  EQU   X'20'               ADD ELEMENT                                  
*                                                                               
NEWGCI   DS    XL4                 NEW GCI FOR MEDIA REC                        
NEWNCI   DS    XL4                 NEW NCI FOR MEDIA REC                        
NEWPCT   DS    XL4                 NEW NCI PCT FOR MEDIA REC                    
*                                                                               
ACCGCI   DS    PL6                 TOTAL GCI FOR ACC                            
ACCNCI   DS    PL6                 TOTAL NCI FOR ACC                            
ACCNPAY  DS    PL6                 TOTAL NET PAYABLE                            
ACCDOLL  DS    PL6                 TOTAL ESTIMATED DOLLARS                      
*                                                                               
CATWC    DS    CL2                 CATEGORY WORK CODE                           
CATGCI   DS    PL6                 CATEGORY GCI FOR ACC                         
CATNCI   DS    PL6                 CATEGORY NCI FOR ACC                         
CATRATIO DS    PL4                 CATEGORY RATIO FOR NET PAYABLE               
CATNPAY  DS    PL6                 CATEGORY NET PAYABLE                         
CATCPCT  DS    PL4                 CATEGORY ESTIMATED COST %                    
*                                                                               
MXFRAMT  DS    PL6                 MEDIA AMOUNT TO TRANSFER ON CLOSE            
MXFRCON# DS    PL3                 MEDIA CONTRACT TO TRANSFER TO                
SXFRAMT  DS    PL6                 SERVICES AMOUNT TO TRANSFER ON CLOSE         
SXFRCON# DS    CL5                 SERVICES CONTRACT TO TRANSFER TO             
XFRCTGS  DS    8CL2                VALID TRANSFER CATEGORIES                    
EL95STAT DS    XL1                                                              
EL95FROM EQU   1                   ELEM IS FROM                                 
EL95TO   EQU   2                   ELEM IS TO                                   
WORK1    DS    F                                                                
*                                                                               
ACCNT    DS    CL12                                                             
SAVEKEY  DS    CL(L'ACCKEY)                                                     
SPOTKEY  DS    CL(L'CTAKEY)                                                     
SPXFRKEY DS    CL(L'CTAKEY)                                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*        CATEGORY TABLE DSECT                                                   
***********************************************************************         
*                                                                               
CTABD    DSECT                                                                  
CTWORKCD DS    CL2                 CATEGORY WORK CODE                           
CTLINE   DS    XL2                 DISP TO CATEGORY SCREEN LINE FROM RA         
CTLENQ   EQU   *-CTABD                                                          
*                                                                               
***********************************************************************         
*        CATEGORY LINE DSECT (SCREEN)                                           
***********************************************************************         
*                                                                               
CLINED   DSECT                                                                  
CLNAMEH  DS    XL8                 NAME OF CATEGORY                             
CLNAME   DS    CL8                                                              
CLGCIH   DS    XL8                 GCI                                          
CLGCI    DS    CL11                                                             
CLNCIH   DS    XL8                 NCI                                          
CLNCI    DS    CL11                                                             
CLNPAYH  DS    XL8                 NET PAYABLE                                  
CLNPAY   DS    CL11                                                             
CLCPCTH  DS    XL8                 COST PERCENT                                 
CLCPCT   DS    CL6                                                              
CLDOLLH  DS    XL8                 ESTIMATED DOLLARS = NET PAY*COST%            
CLDOLL   DS    CL11                                                             
CLGCIPH  DS    XL8                 GCI% = EST DOLLARS/GCI DOLLARS               
CLGCIP   DS    CL6                                                              
CLLENQ   EQU   *-CLINED                                                         
FLDSLINE EQU   6                   FIELDS TO BE CLEARED PER LINE                
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                     *         
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        DDGLOBEQUS                                                             
*        DDGLVXCTLD                                                             
*        DDCOMFACS                                                              
*        ACCTAWORKD                                                             
*        ACCTADSECT                                                             
*        ACGENFILE                                                              
*        SPGENCTA                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACCTAWORKD                                                     
       ++INCLUDE ACCTADSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE SPGENCTA                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACCTA02   05/01/02'                                      
         END                                                                    
