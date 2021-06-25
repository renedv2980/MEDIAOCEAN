*          DATA SET SPLNK1B    AT LEVEL 060 AS OF 02/23/21                      
*PHASE T21E1BC                                                                  
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* HWON  24FEB21 060 SPEC-49771|PREVENT NEW LINES WITH PAID DATA                 
* HWON  24FEB21 060 SPEC-54400|FOR PRISMA, POPULATE BUYVAL ERROR                
* HWON  24FEB21 060 SPEC-54400|FOR PRISMA, CONTINUE PROCESS ON ERROR            
* HWON  20NOV19 059 DS 19.3                                                     
*               059 -SPEC-25154|PREVENT MEDIA R 2-DEC DEMOS VALUES              
*               059 -SPEC-12791|PRISMA CONVERGENCE                              
*               059 -SPEC-29367|SUPPORT 2-DEC PREC IMPRESSIONS                  
* HWON  X19.1XX 058 SPEC-23940|2-BYTE BUYLINES                                  
* HWON  28NOV18 057 SPEC-29725|RELAX PAID CHECKSUM VALIDATION FOR BUYID         
* HWON  30MAY18 056 SPEC-19442|FIX DUMP CAUSED BY INVALID SPILL                 
* HWON  18APR18 055 SPEC-19994|PRODUCT CODE AAA NOT ALLOWED                     
* HWON  28FEB18 054 SPEC-13444|FIX PAID BUYLINE VALIDATION                      
* HWON  12DEC17 053 SPEC-18844-FIX XFER DUMPS CAUSED BY INVLD ELEM LEN          
* HWON  08NOV17 052 SPEC-17211-FIX BUYS UPDATED WHEN DARE ORDER LOCKED          
* HWON  12SEP17 051 SPEC-13049-RELINK FOR LARGER IOAREAS                        
* HWON  16JUN17 050 SPEC-12371- FIX ORIG&PB-ORIG, & SPILL&PB-SPL DEMO           
*               050  ELEMENTS TO FOLLOW POL EST-DEMOS SEQUENCE                  
*               050 SPEC-13761-PRESERVE COMSCORE LKUP FLAGS IN THE CASE         
*               050  WHERE SBTK DOES NOT UPLOAD                                 
* HWON  12JUN17 049 2017.2 RELEASE                                              
*               049 -SPEC-7781 - AUTOMATED AVAIL UUID SUPPORT                   
* HWON  01NOV16 047 COMSCORE SUPPORT                                            
* HWON  22JUN16 046 LOCK RADIO CM BAND BUYS                                     
* HWON  31MAY16 045 FIX SVACTION & DON'T ALLOW NEW BYLN W/O SPOTS               
* HWON  27JAN16 044 FIX DARE BATCH FOR FLIGHTS                                  
* HWON  27JAN16 043 CHANGE QEST TO QBEST DUE TO DUPLICATE LABEL                 
* HWON  21OCT15 042 PASS CLIENT CPROF+0 TO BUYVAL                               
* HWON  12AUG14 041 SUPPORT NEW SPBLDMGN/SPMGADN                                
* HWON  04DEC13 040 SUPPORT 240 MAKEGOODS                                       
*               040 SEND LOCK ORDER ERROR FOR RADIO STREAM                      
* HWON  13AUG12 039 SUPPORT MEDIA X (NETWORK RADIO)                             
* HWON  10DEC10 035 FIX COPY REVLINE/PASTE AVAIL BUG WHICH ALLOCATED            
*                   BRANDS ON THE SPOTS                                         
* HWON  01FEB11 034 REMOVE ORG AFTER QBDELEM, UPTEXT WAS CLOBBERING IT          
*               034 CAUSING BDXFRAGY AND BDMASMRD+1 CORRUPTION                  
* HWON  28JAN11 033 SKIP PROJ SPILL BOOKS DUE TO DESKTOP BUG                    
* HWON  06JAN11 032 DON'T ADD BATCH REC IF ORDER REC ALREADY PRESENT            
* HWON  10DEC10 031 CHANGE VERSION CHECK TO NOT SKIP P RATE BUG FIX             
* HWON  08DEC10 030 FIX DUMP WHEN SENDING CKSM VALIDATION ERROR                 
* WHOA  29NOV10 029 STATION VENDOR LOCK                                         
* HWON  17NOV10 028 FIX ADDING DARE BATCH PASSIVE ON A CHANGE                   
* HWON  16NOV10 027 FIX SAVING AVG/PAVG W/BT                                    
* HWON  27SEP10 026 ADDING NEW DARE BATCH PASSIVE KEYS                          
*               026 FIX QACTION/SVACTION                                        
*               026 TEMPORARILY REMOVE REUSE DELETED BUYLINES                   
* HWON  11MAR10 025 FIX SAVING UPGRADE FORMULA, CHG LEN OF SCANNER BLK          
*               025 SKIP SPILL MARKET IF HOME MARKET                            
* HWON  10NOV09 022 ALWAYS OC PROGRAM NAME TO SPACES                            
* MHER  29OCT09 021 CLEAR STATUS EXCEPT FOR COST OVERRIDE                       
* HWON  03SEP09 019 SAVE THE RATING SERVICE MARKET IF NOT SENT BY DTOP          
* HWON  28JUL09 018 FIX BOOK NOT SAVED WHEN UPGRADES ARE ALSO SAVED             
*               018 SEND ERROR IF BUY DELETED WHEN VALIDATING CHKSUM            
* WHOA  26JUN09 017 FIX PRODUCT ALLOCATION BUG                                  
* HWON  04MAY09 016 FIX LOCKED ORDER CHECK FOR FAX CANCELLED STATUS             
* WHOA  16APR09 015 SUPPORT FOR C2 LOCK                                         
* HWON          015 SAVE RADIO FREQUENCIES TO BUY                               
*               015 TRANSFERRING AVERAGE BOOKS                                  
* HWON  16DEC08 014 POST BUY DEMOS FIX                                          
*               014 DELETE ALL SPILL FIRST TIME IN                              
*               014 ALLOW MAKEGOOD MISSED SPOTS TO BE SENT UP FIRST             
*               014 INITIALIZE BOOKTYPE TO NOT COPY TO NEXT BUYLINE             
* HWON  24NOV08 013 ADD FINDHOLE OPTION WHEN 255 BUYLINES                       
* HWON  18NOV08 012 DON'T ALLOW TRANSFER WHEN 255 BUYLINES                      
* HWON  03NOV08 011 REMOVED MAX RECORD LENGTH VALIDATION                        
*               011 FIX PACKAGE SAVING                                          
* WHOA  25AUG08 010 FIX: IMPRESSIONS SHOULD BE SAVED AS 1-DEC PRECISION         
* WHOA  21AUG08 009 SUPERDESK SUPPORT CODE                                      
* HWON  13MAY08 008 SUPPORT FOR NEW MAKEGOODS                                   
* EJOR  25APR08 007 ADD DESKTOP TRANSFER ELEM TO BUY ON ADD!                    
*               --- CALL BUYVAL ON ADD!                                         
* EJOR  26FEB08 006 ADD DESKTOP TRANSFER ELEM TO BUY                            
* EJOR  01FEB08 005 RE-USE DELETED BUYLINES (NOP'D UNTIL TESTING)               
*               --- REMOVE UNNECESSARY MAP CODE                                 
*               --- ADD NEW FIELDS FOR PAID CHECKSUM                            
*               --- SEND TOKEN WHEN RUNNING A BUY DOWNLOAD                      
*               --- EXTRACT VERSION NUMBER (OLD CHECKS WERE WRONG)              
*               --- MOVE BUYVAL CALL TO JUST PRIOR TO PUTREC                    
*               --- FIX DATA TYPE IN FILM CODE MAPS                             
* HWON  27NOV07 004 FIX BUG THAT WAS CLOBBERING RLXACT                          
*               004 DON'T DELETE AVAIL/REVLINES ON D/E ACTION, INSTEAD          
*               004 MARK AVAIL/REVLINES HISTORY                                 
* HWON  08NOV07 003 DOWNLOAD ROUTES AFTER TRANSFER                              
*               003 CHANGED ERROR RESPONSE MAP CODE FOR VER >= 3.0.0.88         
* HWON  17SEP07 002 TRANSFER 2 CHARACTER BOOK TYPES FOR DEMOS/UPGRADES          
*               002 TRANSFER ACTIVITY NOT SAVED CORRECTLY                       
*               002 CLEAR FIELDS OF GARBAGE                                     
*               002 SUPPORT DELETE BUY AND AVAIL/REVLINE ACTION                 
*               002 MOVE CALL TO CHGBYR AFTER SV FIELDS SET                     
*                                                                               
*=====================================================================*         
SPLNK1B  TITLE 'SPOT DESKTOP BUY UPLOAD'                                        
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=CODE,REQUEST=*,SYSTEM=SPTSYSQ,LINKIO=Y,     X        
               WORKERKEY=SPRT,RLEN=512,                                X        
               BLOCKS=(B#WORKD,WORKD,                                  X        
               B#SAVED,SAVED,                                          X        
               B#LP_D,LP_D,                                            X        
               B#TWAD,TWAD)                                                     
*                                                                               
SE#MAXLN EQU   1276                                                             
SE#CKSUM EQU   1277                                                             
SE#MGMAX EQU   1301                                                             
SE#C2LKD EQU   0902                BUY DATA IS LOCKED BY C2 PROGRAM             
SE#STLCK EQU   1372                STATION IS LOCKED                            
SE#NOSPT EQU   0118                NO SPOTS IN BUY PERIOD                       
*                                                                               
CODE     NMOD1 0,**SL1B**,RR=RE                                                 
         LR    R6,R1                                                            
         USING LP_D,R6             R6=A(DDLINK PARAMETER BLOCK)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         L     RF,LP_ALPXD                                                      
         MVC   SVTXPNUM,(LP_XPINF-LP_XD)+(TXPNUM-TXPINFO)(RF)                   
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BE    FIRST                                                            
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         J     EXITY                                                            
         DROP  R7                                                               
*                                                                               
*==========================================================                     
* RUN-START - 'FIRST' MODE                                                      
*==========================================================                     
*                                                                               
FIRST    DS    0H                                                               
         CLC   STAMP,STAMPLIT      TEST CORRECT SAVE STORAGE STAMP              
         BE    FIRST10                                                          
         MVC   DUB(4),SVMED        SAVE MEDIA/CLIENT ALPHA                      
*                                                                               
         LA    R0,SAVED            NO - CLEAR SAVED STORAGE                     
         LHI   R1,SAVEDX-SAVED                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVMED(4),DUB        RESTORE MEDIA/CLIENT ALPHA                   
         MVC   STAMP,STAMPLIT                                                   
*                                                                               
FIRST10  MVC   LP_BLKS+((B#CLTREC-1)*L'LP_BLKS)(AIOLAST-AIO2),AIO2              
*                                                                               
         J     EXITY                                                            
*                                                                               
*==========================================================                     
* INIT - NOTE FIRST REC PROCESSED *BEFORE* INIT CALL!                           
*==========================================================                     
*                                                                               
INIT     DS    0H                                                               
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   RECUP,CRECUP-COMFACSD(RF)                                        
         XC    SVMGTKNS(SVMGTKNX-SVMGTKNS),SVMGTKNS                             
         MVI   SVMGCDTF,L'SVMGCDTB/256                                          
*                                                                               
         LA    R0,4                                                             
         LA    RF,SVMGCDTB                                                      
         XC    0(256,RF),0(RF)                                                  
         LA    RF,256(RF)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVI   ERRORFLG,0                                                       
*                                                                               
         L     RF,LP_AWMP           SAVE DARE TRACE/LINE # TABLE                
         ST    RF,AC2LKTBL                                                      
         AHI   RF,C2LKTABQ          RESERVE SPACE FOR C2 LOCK TABLE             
         ST    RF,LP_AWMP                                                       
*                                                                               
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         L     RF,ATWA                                                          
         MVC   GBYAGY,TWAAGY-TWAD(RF)                                           
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         MVC   SAVE1OR2,GBY1OR2                                                 
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
*==========================================================                     
* PROCESS INPUT OF AN UPLOAD RECORD                                             
*==========================================================                     
INPUT    LA    RE,RECTAB                                                        
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   0(2,RE),LP_QMAPN   LOOK UP RECORD MAP CODE IN TABLE              
         BE    INPUT20                                                          
         AHI   RE,L'RECTAB                                                      
         BCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  DS    0H                                                               
         CLI   ERRORFLG,0          HAVE ERROR?                                  
         BE    INPUT50              NO, GO PROCESS THE MAP                      
*                                                                               
         CLI   5(RE),X'FF'         PROCESSING HIGH LEVEL MAP?                   
         BNE   INPUT30              NO                                          
*                                                                               
         ICM   RF,15,AMAPNUM       HAVE PREVIOUS MAP?                           
         BZ    INPUT40              NO, OK TO CLEAR ERRORFLG                    
         CLI   5(RF),X'FF'         WAS PREVIOUS MAP HIGH LEVEL?                 
         BNE   INPUT40              NO, OK TO CLEAR ERRORFLG                    
         B     INPUT50              OTHERWISE, DON'T CLEAR IT                   
*                                                                               
INPUT30  TM    5(RE),X'80'         PROCESSING LOW LEVEL MAP?                    
         BZ    INPUT50              NO                                          
         CLI   ERRORFLG,FATALERR    FATAL (CRITICAL) ERROR?                     
         BNE   INPUT40               NO, GO CLEAR NON-CRITICAL ERROR            
*                                                                               
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE?                
         BNE   INPUT50              NO, DO NOT CLEAR FATAL ERROR                
*                                   YES, CLR FATAL ERR & CONT. PROC'G           
INPUT40  MVI   ERRORFLG,0                                                       
*                                                                               
INPUT50  ST    RE,AMAPNUM          SAVE ADDRESS OF LP_QMAPN ENTRY               
         CLI   ERRORFLG,0          HAVE ANY ERRORS?                             
         JNE   EXITY               YES                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,2(RE)                                                       
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR          SET A(PROCESSING ROUTINE)                    
*                                                                               
         MVC   XTRATEXT,SPACES     INITIALIZE EXTRA MESSAGE TEXT                
         GOTOR RECADDR                                                          
         J     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
*================================================================               
* ROUTINE TO VALIDATE AND BUILD BUY RECORD                                      
*================================================================               
                                                                                
VALBHD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PROCFLAG,0          RESET PROC FLAG                              
         XC    SVSPLMKS,SVSPLMKS                                                
         XC    SVNTELEM,SVNTELEM   CLEAR IT                                     
         XC    SVUPBTYP,SVUPBTYP                                                
         XC    SVUPBOOK,SVUPBOOK                                                
         XC    IOBRDLST,IOBRDLST   CLEAR PRODUCT LIST                           
         XC    XPRDLIST,XPRDLIST   CLEAR EXISTING PRODUCT LIST                  
         XC    QACOMM1(20),QACOMM1 CLEAR COMMENT ADDRESSES                      
         XC    CKSUMBFR,CKSUMBFR                                                
*                                                                               
         CLI   QACTION,0           TEST ACTION GIVEN                            
         BE    *+10                                                             
         MVC   SVACTION,QACTION                                                 
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         JE    *+2                  NO                                          
*                                                                               
         CLI   QMEDX,0                                                          
         BE    VALBH1                                                           
         MVC   SVBAGYMD,QMEDX                                                   
         MVC   SVMED,QMEDA                                                      
*                                                                               
VALBH1   OC    QCLTX,QCLTX                                                      
         BZ    *+10                                                             
         MVC   SVBCLT,QCLTX                                                     
*                                                                               
         MVC   SV1OR2,SAVE1OR2                                                  
         CLC   LP_AGY,=C'SJ'       IF AGENCY SJ AND                             
         JNE   VALBH1B                                                          
         CLC   QCLTA,=C'TBL'       CLIENT TBL?                                  
         JE    *+10                                                             
         CLC   QCLTA,=C'PG0'       CLIENT PG0?                                  
         JE    *+10                                                             
         CLC   QCLTA,=C'PG1'       CLIENT PG1?                                  
         JNE   VALBH1B                                                          
         MVI   SV1OR2,2            SET 2-BYTE BUYLINE OVERRIDE                  
*                                                                               
VALBH1B  CLI   SVBAGYMD,0          TEST MEDIA KNOWN                             
         BE    VALERR02                                                         
         OC    SVBCLT,SVBCLT       TEST CLIENT KNOWN                            
         BZ    VALERR04                                                         
*                                                                               
         L     R1,ACLTREC          TEST CLIENT RECORD IS AROUND                 
         CLC   SVBAGYMD(3),CKEYAM-CLTHDR(R1)                                    
         BE    VALBH1A                                                          
*                                                                               
         MVC   QMEDX,SVBAGYMD      NO - GET CLIENT RECORD                       
         MVC   QCLTX,SVBCLT                                                     
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   VALERR03                                                         
*                                                                               
VALBH1A  DS    0H                                                               
         CLI   QBEST,0             TEST ESTIMATE NUMBER GIVEN                   
         BE    *+10                                                             
         MVC   SVBEST,QBEST                                                     
*                                                                               
         MVC   FULL(3),=C'POL'     READ THE POL ESTIMATE                        
         BRAS  RE,GTESTREC         INTO AESTREC                                 
*                                                                               
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE?                
         BE    VALBH2               YES, SKIP COMSCORE SUPPORT TEST             
         OC    LP_VRSN,LP_VRSN     NO PC VERSION?                               
         JZ    VALBH2               YES, SKIP COMSCORE SUPPORT TEST             
         CLC   LP_VRSN,=AL1(4,6,0,50)  VERSION# >= V4.6.0.50?                   
         JNL   VALBH2                   YES, SKIP COMSCORE TEST                 
*                                                                               
* SBTK VERSION <4.6.0.50 DOES NOT SUPPORT COMSCORE DEMOS                        
* SEND ERROR IF PRESENSE OF COMSCOE DEMOS ON POL ESTIMATE                       
*                                                                               
         L     R3,AESTREC                                                       
         USING ESTHDR,R3           IF ESTIMATE HAS COMSCORE DEMOS               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   VALBH2                                                           
         OC    ENONTDMS(L'ENONTDMS*20),ENONTDMS                                 
         JNZ   VALERR18            SEND AN ERROR                                
         DROP  R3                                                               
*                                                                               
NEW      USING BUYKEY,SVKEY        BUILD NEW BUY KEY FROM INPUT DATA            
*                                                                               
VALBH2   XC    NEW.BUYKEY,NEW.BUYKEY                                            
*                                                                               
         MVC   NEW.BUYKAM,SVBAGYMD                                              
         MVC   NEW.BUYKCLT,SVBCLT                                               
         MVI   NEW.BUYKPRD,X'FF'   SET PRD=POL                                  
*                                                                               
         OC    QMKT,QMKT           TEST MARKET GIVEN                            
         BNZ   *+10                                                             
         MVC   QMKT,SVBMKT                                                      
*                                                                               
         MVC   SVBMKT,QMKT                                                      
         MVC   NEW.BUYKMKT,QMKT                                                 
         OC    NEW.BUYKMKT,NEW.BUYKMKT                                          
         BZ    VALERR05                                                         
*                                                                               
VALBH4   OC    QSTA,QSTA           TEST STATION GIVEN                           
         BZ    VALBH6                                                           
*                                                                               
         MVC   SVSTA,QSTA                                                       
*                            ALSO WANT SFLAG1 FROM STATION REC                  
         MVI   SVSFLAG1,0                                                       
         MVI   LP_VPARM,$VALFLG1                                                
         GOTOR (#VALSTA,AVALSTA),DMCB,SVSTA,L'SVSTA,WORK                        
         BNE   VALERR06                                                         
         MVC   SVBSTA,STAPSTA-STAPACKD+WORK                                     
         MVC   SVSFLAG1,WORK+STAPACKL                                           
*                                                                               
VALBH6   DS    0H                                                               
         CLI   SVACTION,QACTADD     ADDING A NEW RECORD?                        
         BNE   VALBH8               NO                                          
         TM    SVSFLAG1,SLOCK       X'04' - STATION LOCKED?                     
         BNZ   VALERR13                                                         
VALBH8   MVC   NEW.BUYKSTA,SVBSTA                                               
         OC    NEW.BUYKSTA,NEW.BUYKSTA                                          
         BZ    VALERR07                                                         
*                                                                               
         MVC   NEW.BUYKEST,SVBEST                                               
         CLI   NEW.BUYKEST,0                                                    
         BE    VALERR08                                                         
*                                                                               
         BRAS  RE,TSTLOCK          TEST SOON LOCKS                              
         JNE   *+2                 PER MHER, EASIER TO DIE & ROLLBACK           
*        BNE   VALERR21                      -HWON 6/26/2019 SPEC-36655         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLIN           TEST LINE NUMBER                             
         BNZ   VALBH10                                                          
         CLI   SVACTION,QACTADD     TEST ADDING A NEW RECORD                    
         JNE   *+2                 NO - LINE NUMBER MUST BE GIVEN               
*                                                                               
VALBH10  STCM  R0,3,SVBLIN         SAVE LINE NUMBER                             
         MVC   NEW.BUYKBUY+1(2),SVBLIN                                          
         DROP  NEW                                                              
*                                                                               
         BRAS  RE,CHGBYR           CHANGE BUYER CODE IF PASSED                  
*                                                                               
         CLI   SVACTION,QACTADD    TEST ADDING A NEW RECORD                     
         BNE   VALBH20                                                          
*                                                                               
         MVC   IOKEY(L'BUYKEY),SVKEY                                            
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#BUYREC'                   
*                                                                               
VALBH12  CLC   IOKEY(10),IOKEYSAV  A-M/CLT/PRD/MKT/STA/EST                      
         BNE   VALBH14                                                          
         MVC   IOKEYSAV,IOKEY      SAVE LAST KEY READ                           
         MVI   GBYACT,GBYSEQ                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBSQD+IOSPTDIR+B#BUYREC'                     
         B     VALBH12                                                          
*                                                                               
VALBH14  MVC   SVKEY,IOKEYSAV      RESTORE LAST KEY FOUND                       
         CLI   SV1OR2,2                                                         
         BNE   *+14                                                             
         CLC   SVKEY+11(2),=H'499' 499 MAX LINES FOR 2BYTES                     
         B     *+10                                                             
         CLC   SVKEY+11(2),=H'255' 255 MAX LINES FOR 1BYTE                      
         BNE   VALBH16              NO                                          
*                                                                               
         BRAS  RE,FINDHOLE         LOOK FOR OPEN/DELETED LINES                  
         BE    VALBH18             SHOULD HAVE NOOP THIS TOO                    
         B     MAXLNERR                                                         
*                                                                               
VALBH16  LLH   R0,SVKEY+11         GET 2 BYTE BUYLINE                           
         AHI   R0,1                INC 1                                        
         STCM  R0,3,SVKEY+11       PUT IT BACK                                  
*                                                                               
VALBH18  L     R0,ABUYREC          INITIALIZE NEW BUY RECORD                    
         LHI   R1,IO5LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ABUYREC                                                       
         USING BUYRECD,RF                                                       
         MVC   BUYKEY(13),SVKEY    SET BUYREC                                   
         MVC   BUYKBUY(2),SVKEY+11 SET 2 BYTE BUYLINE AT BUYREC+10              
         MVI   BUYKBUY+2,0         NEED TO CLEAR BUYREC+12                      
*                                                                               
         LHI   R0,BDELEMX-BUYRECD                                               
         STCM  R0,3,BUYRLEN                                                     
         MVC   BUYALPHA,LP_AGY                                                  
         MVI   BDCODE,BDCODEQ      X'01' PRIMARY                                
         MVI   BDLEN,BDELEMX-BDELEM                                             
         J     VALBHX                                                           
         DROP  RF                                                               
*                                                                               
VALBH20  MVC   IOKEY(L'BUYKEY),SVKEY                                            
*                                                                               
         L     R1,=A(IOBRDUP+IOSPTDIR+B#BUYREC)  READ FOR UPDATE (LOCK)         
         CLI   SVACTION,QACTDEL    TEST DELETE                                  
         JE    VALBH25                                                          
         CLI   SVACTION,QACTDELA   TEST DELETE BUY AND AVAIL                    
         JE    VALBH25                                                          
         L     R1,=A(IOBRD+IOSPTDIR+B#BUYREC)    JUST READ (NO LOCK)            
*                                                                               
VALBH25  MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BNE   VALERR10                                                         
*                                                                               
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#BUYREC'                   
         BNE   VALERR11                                                         
* COMPUTE CHECKSUM FOR RECORD                                                   
         OC    QCHKSUM,QCHKSUM     IF NO CHECKSUM PASSED, SKIP TEST             
         BZ    VALBH30                                                          
         L     RE,ABUYREC                                                       
         SR    RF,RF                                                            
         ICM   RF,3,BUYRLEN-BUYREC(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,FULL                                                       
         CLC   FULL,QCHKSUM                                                     
         JNE   VALERR15                                                         
*                                                                               
VALBH30  BRAS  RE,PDCHKSUM         GET CHKSUM OF PAID SPOTS                     
         MVC   CKSUMBFR,FULL       SAVE CHKSUM BEFORE CHANGES                   
*                                                                               
         BRAS  RE,BLDXPRD          BUILD LIST OF EXISTING PRODUCTS              
*                                                                               
         CLI   SVACTION,QACTDEL    TEST DELETE                                  
         JE    VALBH40                                                          
         CLI   SVACTION,QACTDELA   TEST DELETE BUY AND AVAIL                    
         JE    VALBH40                                                          
*                                                                               
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,NTDELCDQ     X'50'-NON-TRAD DEMO ELEM                     
         MVI   ELCDHI,NTDELCDQ                                                  
         BRAS  RE,NEXTEL           DID WE FIND A X'50' NON TRAD ELEM?           
         JNE   VALBHX               NO, DONE                                    
         LLC   RE,1(R3)             YES, LETS SAVE IT OFF                       
         LAY   RE,-3(RE)           DEC-3 (2 FOR CODE+LEN & 1 FOR EX)            
         MVC   SVNTELEM+2(0),2(R3)                                              
         EX    RE,*-6                                                           
         J     VALBHX                                                           
*                                                                               
VALBH40  OI    IOKEY+13,X'80'      SET DELETED FLAG                             
         MVI   GBYACT,GBYWRT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOSPTDIR+B#BUYREC'                     
         JNE   *+2                                                              
*                                                                               
         L     R3,ABUYREC                                                       
         OI    15(R3),X'80'        SET RECORD DELETED                           
         MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#BUYREC'                     
         JNE   *+2                                                              
* SEND DISK ADDRESS OF DELETED BUY                                              
         LA    RE,I#SDXCNF                                                      
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE                 
         JNE   *+8                  YES, SEND I#SDXCNF                          
         LA    RE,I#SDXDEL                                                      
         GOTOR UPLRSP,DMCB,(RE)      SEND DEL UPLOAD RESPONSE                   
*                                                                               
VALBHX   J     EXITY                                                            
         EJECT                                                                  
*================================================                               
* BUY DESCRIPTION DATA                                                          
*================================================                               
                                                                                
VALBDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         LHI   RE,BDELEMX-BDELEM                                                
         AHI   RE,-3               -ELCODE/LEN                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BDELEM+2(0),QBDELEM                                              
*                                                                               
         MVI   BDTIME,0                                                         
         MVI   BDCOSTP,0                                                        
         MVI   BDPURP,0                                                         
         MVI   BDCANAD,0                                                        
*                                                                               
         MVC   BDCOST,QBDCOST+1    MOVE 3 BYTES OF COST                         
         CLI   QBDCOST,0           TEST FITS IN 3 BYTES                         
         BE    VALBD2                                                           
         ICM   R1,15,QBDCOST                                                    
         M     R0,=F'2'                                                         
         D     R0,=F'100'          CONVERT TO PENNIES                           
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,7,BDCOST                                                      
         OI    BDCIND2,BDCNBRDQ    X'10' ELSE SET COST IN DOLLARS               
*                                                                               
VALBD2   DS    0H                                                               
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE                 
         JE    VALBD3               YES, SKIP P RATE FIX                        
         OC    LP_VRSN,LP_VRSN     NO PC VERSION?                               
         BZ    VALBD3               YES, SKIP P RATE FIX                        
         CLC   LP_VRSN,=AL1(5,0,0,0) STILL WANT P RATE FIX UNTIL V5             
         BNL   VALBD3                                                           
*                                                                               
         CLC   =C'DF',LP_AGY       AGENCY  DF  USES P-RATE TYPE                 
         BE    VALBD3                                                           
*                                                                               
         TM    BDCIND2,BDCCOMOQ    X'80' COMMISSION ONLY??                      
         BNZ   VALBD3              YES, THEN WE DON'T CARE ABOUT P RATE         
*                                                                               
         CLI   BDCIND,BDCMINSQ     X'01' MINUS 'P' RATE                         
         BE    *+12                                                             
         CLI   BDCIND,BDCNTPQ      X'00' IF RATE TYPE OF 'P'                    
         BNE   VALBD3                                                           
         OI    BDCIND,BDCGROSQ     TURN ON GROSS INSTEAD                        
*                                                                               
VALBD3   CLC   BUMASPR1,=C'AAA'    CHECK IF PRODUCT IS AAA                      
         JE    VALERR19             SEND ERROR, AAA IS NOT ALLOWED              
         LA    R1,BUMASPR1         POINT TO EBCDIC PRD                          
         BRAS  RE,FINDPRD                                                       
         BL    VALBD6                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDMASPRD(1),0(R1)                                                
*                                                                               
VALBD6   CLC   BUMASPR2,=C'AAA'    CHECK IF PRODUCT IS AAA                      
         JE    VALERR19             SEND ERROR, AAA IS NOT ALLOWED              
         LA    R1,BUMASPR2         POINT TO EBCDIC PRD                          
         BRAS  RE,FINDPRD                                                       
         BL    VALBD10                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDMASPRD+1(1),0(R1)                                              
*                                                                               
VALBD10  GOTO1 VDATCON,DMCB,(5,0),(3,BDCHG)  SET LAST CHANGED DATE              
*                                                                               
         CLI   BDMGDATE,C'$'       TEST MGCODE IS A TOKEN                       
         BNE   VALBD20                                                          
         LA    R4,SVMGTKNS         POINT TO LIST OF TOKENS DONE                 
         LHI   R5,SVNWMGMX         15 MAX NEW MAKEGOODS                         
         USING MGTOKEND,R4                                                      
*                                                                               
VALBD12  CLI   MGTOKEN,C'$'        ANY MORE TOKENS                              
         BNE   VALBD14                                                          
         CLC   MGSTATN,SVBSTA                                                   
         BNE   VALBD13                                                          
         CLC   BDMGDATE,MGTOKEN                                                 
         BE    VALBD16                                                          
VALBD13  AHI   R4,L'SVMGTKNS                                                    
         BCT   R5,VALBD12                                                       
         DC    H'0'                TOO MANY TOKENS                              
*                                                                               
VALBD14  MVC   MGSTATN,SVBSTA      SAVE STATION AND                             
         MVC   MGTOKEN,BDMGDATE    SAVE NEW TOKEN IN TABLE                      
         GOTOR GETMGCD,(R4)        GET NEW ALPHA/BINARY CODES                   
*                                                                               
VALBD16  MVC   BDMGDATE,MGCDALPH   REPLACE TOKEN IN BUY RECORD                  
         DROP  R4                                                               
                                                                                
*==================================================================             
* CHECK FOR -S AND SET LAST BYTE OF BDPROGRM TO X'00'                           
*==================================================================             
                                                                                
VALBD20  OC    BDPROGRM,SPACES                                                  
         LA    RE,BDPROGRM+17                                                   
         LHI   RF,16                                                            
*                                                                               
VALBD22  CLI   0(RE),C' '                                                       
         BH    VALBD24                                                          
         BCTR  RE,0                                                             
         BCT   RF,VALBD22                                                       
         B     VALBD26                                                          
*                                                                               
VALBD24  BCTR  RE,0                BACK UP ONE MORE                             
         CLC   0(2,RE),=C'-S'                                                   
         BNE   VALBD26                                                          
         MVI   BDPROGRM+17,X'00'                                                
                                                                                
*=================================================================              
* NOW REPLACE QBDELEM SO VALUES ARE CORRECT IF IT IS USED AGAIN                 
*=================================================================              
                                                                                
VALBD26  LHI   RE,BDELEMX-BDELEM                                                
         AHI   RE,-3               -ELCODE/LEN                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   QBDELEM(0),BDELEM+2                                              
                                                                                
*==========================================================                     
* ON ADD INSERT DUMMY 02 DEMO ELEMENT SO CAN REPLACE LATER                      
*==========================================================                     
                                                                                
         LA    R3,BDELEM                                                        
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),NDCORGQ       X'02' ORIGINAL DEMO                          
         JE    EXIT                                                             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NDCORGQ        X'02' ORIGINAL DEMO                          
         MVI   ELEM+1,24                                                        
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================================================                               
* DEMOS - NOTE: PROCESS UPGRADES FIRST!                                         
*================================================                               
                                                                                
VALDEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
E        USING NDELEM,ELEM                                                      
         XC    ELEM,ELEM                                                        
         MVI   E.NDCODE,NDCORGQ    SET ORIGINATING DEMOS                        
         MVC   E.NDBOOK,BUBOOK                                                  
         OC    SVUPBOOK,SVUPBOOK   ANY UPGRADE BOOK?                            
         BZ    *+10                                                             
         MVC   E.NDBOOK,SVUPBOOK    YES - USE IT                                
         MVC   E.NDPROG,BUPROG                                                  
*                                                                               
         BRAS  RE,BLDDEM           BUILD DEMO CODES/VALUES                      
         JNE   EXITN                                                            
*                                                                               
D        USING DLUELEM,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVI   D.DLUCODE,DLUCODEQ                                               
         MVI   D.DLULEN,DLULENQ                                                 
         MVC   D.DLUBKTYP,BUBKTYPE                                              
         OC    SVUPBTYP,SVUPBTYP   ANY UPGRADE BOOK TYPE?                       
         BZ    *+10                                                             
         MVC   D.DLUBKTYP,SVUPBTYP  YES - USE IT                                
*  FOLLOWING FIELDS CANADA ONLY                                                 
*&&DO                                                                           
         MVC   D.DLUBAMKT,DEMQMKT                                               
         MVC   D.DLUBSTOV,DEMQSTA                                               
*                                                                               
         LA    RE,X'01'            SET FOR BBM                                  
         CLI   DEMQRSV,C'N'        TEST NSI LOOKUP                              
         BNE   *+8                                                              
         LA    RE,X'02'                                                         
         EX    RE,*+8                                                           
         B     *+8                                                              
         OI    D.DLUBFLGS,0                                                     
*&&                                                                             
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,DLUCODEQ     X'24'-DEMO LOOK-UP OVERRIDE ELEM             
         MVI   ELCDHI,DLUCODEQ                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         CLI   D.DLUBKTYP,0        DON'T ADD EMPTY ELEM                         
         BNH   *+8                                                              
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  D,R2,E                                                           
*                                                                               
*================================================                               
* SPILL DEMOS - NOTE: PROCESS UPGRADES FIRST!                                   
*================================================                               
VALSPL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ABUYREC                                                       
         CLC   BUSPLAMK,BUYKMKTN-BUYREC(R2)  SPILL MKT=ACTUAL MKT               
         BE    VALSPLX                        YES - SKIP IT                     
*                                                                               
         XC    BUBOOK,BUBOOK       INIT BOOK                                    
*                                                                               
         USING LW_D,RE                                                          
         ICM   RE,15,QABOOK        WAS BOOK SENT?                               
         BZ    VALSPL40             NO                                          
         CLC   LW_DATA1(3),=C'AVG' SBTK SENT AVG AS BOOK?                       
         BE    VALSPL40             YES, ITS A DEFECT, IGNORE IT                
         GOTOR VPERVAL,DMCB,(8,LW_DATA1),PERVALST                               
         MVC   BUBOOK,PERVALST+PVALBSTA-PERVALD                                 
         DROP  RE                                                               
*                                                                               
E        USING NDELEM,ELEM                                                      
VALSPL40 XC    ELEM,ELEM                                                        
         MVI   E.NDCODE,NDCSPLQ    SET X'03' SPILL DEMOS                        
         MVC   E.NDBOOK,BUBOOK                                                  
*                                                                               
* SETUP BUY SPILL ELEMENT                                                       
*                                                                               
         MVC   E.NDAGYMKT,BUSPLAMK AGENCY MARKET CODE                           
**NOP    MVC   E.NDSTA,DEMQSTA     CANADA ONLY                                  
         MVC   E.NDMKTALF,DEMQMKT                                               
         MVC   E.NDBKTYPE,BUBKTYPE                                              
         MVI   E.NDRTGSVC,C'0'                                                  
         CLI   DEMQRSV,C'N'                                                     
         BE    *+8                                                              
         MVI   E.NDRTGSVC,C'1'                                                  
*                                                                               
         OC    E.NDRSMKT,DEMRSMKT  DT SEND US RTG SVC MKT?                      
         BNZ   VALSPL50             YES!                                        
         BRAS  RE,GETSPL           READ SPLDEF AND VALIDATE SPILL               
         BNE   VALSPLX             SPILL MKT DOESN'T EXIST, SKIP                
*                                                                               
* KEEP TRACK WHICH SPILL MARKETS WERE UPDATED                                   
*                                                                               
VALSPL50 LA    RE,SVSPLMKS         UPDATE SPILL LIST                            
         LA    RF,SVSPLMKS+L'SVSPLMKS-1                                         
VALSPL55 CR    RE,RF               NEED A BIGGER LIST?                          
         JH    *+2                  YES, DIE                                    
         OC    0(L'BUSPLAMK,RE),0(RE)  OPEN SLOT?                               
         JZ    *+12                      YES                                    
         LA    RE,L'BUSPLAMK(RE)                                                
         J     VALSPL55                                                         
         MVC   0(2,RE),BUSPLAMK    UPDATE SPILL LIST                            
*                                                                               
         OI    PROCFLAG,PROCSPIL   SET FLAG - PROCESSED SPILL                   
*                                                                               
         BRAS  RE,BLDDEM                                                        
VALSPLX  J     EXIT                                                             
         DROP  E                                                                
*=================================================================              
* GET SPILL RECORD                                                              
*=================================================================              
SP       USING STAPACKD,WORK                                                    
E        USING NDELEM,ELEM                                                      
GETSPL   NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVI   SP.STAPACT,C'U'                                                  
         MVC   SP.STAPAGY,LP_AGY                                                
         L     RF,AAGYREC                                                       
         MVC   SP.STAPCTRY,AGYPCNDA-AGYHDR(RF)                                  
         MVC   SP.STAPMED,SVMED                                                 
         MVC   SP.STAPACOM,ACOMFACS                                             
         XC    SP.STAPQMKT,SP.STAPQMKT                                          
         MVC   SP.STAPSTA,SVBSTA                                                
         GOTOR VSTAPACK,SP.STAPACKD                                             
*                                                                               
         XC    IOKEY,IOKEY         BUILD KEY FOR SPILL DEFINITION REC           
         MVC   IOKEY(2),=X'0D13'                                                
         MVC   IOKEY+2(2),LP_AGY                                                
         L     R3,ACLTREC                                                       
         USING CLTRECD,R3          R3=A(CLIENT RECORD)                          
         MVC   IOKEY+4(1),CPROF+3  AGENCY RATING SERVICE                        
         MVC   IOKEY+5(5),SP.STAPQSTA STATION CALL LETTERS                      
         CLI   SVMED,C'R'                                                       
         BE    GETSPL10            MEDIA T/N HAVE 0 IN KEY+9 !!!                
         CLI   SVMED,C'X'                                                       
         BE    GETSPL10            MEDIA T/N HAVE 0 IN KEY+9 !!!                
         MVI   IOKEY+9,0                                                        
GETSPL10 MVC   IOKEY+10(2),SVBCLT  CHECK FOR CLT EXCEPTION REC                  
         DROP  R3,SP                                                            
*                                                                               
         L     R1,ASDFREC                                                       
         CLC   IOKEY(10),0(R1)     DID WE ALREADY READ IT?                      
         BE    GETSPL30             YES!!                                       
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IODIR+B#SDFREC'                         
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV  AND USE IT IF FOUND                          
         BE    GETSPL20                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(10),IOKEYSAV  RESTORE WITHOUT CLIENT                       
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IODIR+B#SDFREC'                         
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV  READ FOR DEFAULT SPILL RECORD                
         JNE   EXITY               EXIT WITH CC NOT =                           
*                                                                               
GETSPL20 GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#SDFREC'                        
         JNE   *+2                                                              
*                                                                               
GETSPL30 L     R3,ASDFREC                                                       
         USING SDEFRECD,R3                                                      
         CLC   13(2,R3),=H'256'                                                 
         JH    *+2                                                              
         LA    R3,SDEFEL                                                        
         DROP  R3                                                               
         USING SDEFEL05,R3                                                      
GETSPL40 CLI   0(R3),0                                                          
         BE    GETSPLXN                                                         
*                                                                               
         CLC   SDEFAMKT,BUSPLAMK                                                
         BE    GETSPL60                                                         
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETSPL40                                                         
*                                                                               
GETSPL60 MVC   E.NDRSMKT,SDEFRMKT                                               
         J     EXITY                                                            
*                                                                               
GETSPLXN J     EXITN                                                            
         DROP  E                                                                
         EJECT                                                                  
*================================================                               
* VALIDATE THE DEMO CATEGORY AND BUILD BUY ORIG-DEMO OR SPILL ELEMENT,          
* CALL SPDEMEXT TO HELP BUILD ELEMS TO FOLLOW THE POL DEMO SEQUENCE,            
* CONVERTS 2-DECIMAL DEMO VALES IF NOT RATING/EXTENDED, AND                     
* THEN PUT THE ELEMENT TO THE BUY RECORD.                                       
*================================================                               
E        USING NDELEM,ELEM                                                      
         USING BDMWORKD,RC                                                      
DX       USING DEMEXTD,BDMDMEXT                                                 
BLDDEM   NTR1  BASE=*,LABEL=*,WORK=(RC,BDMWORKL)                                
*                                                                               
         XC    BDMDMEXT,BDMDMEXT                                                
*                                                                               
         BRAS  RE,FNDBDEMO         FIND TARGET BUY 02/03 DEMO ELEM?             
         ST    R3,BDABYDEM         SAVE ADDRESS OR INSERTION ADDRESS            
         JE    *+8                 YES, USE ADDR OF 02/03 DEMO ELEM             
         LA    R3,ELEM             NO, LETS USE ELEM ADDRESS                    
         ST    R3,DX.DXDEMEL        AND SET DEMEL ADDRESS                       
*                                                                               
         MVC   FULL(3),=C'POL'     READ THE POL ESTIMATE                        
         BRAS  RE,GTESTREC         INTO AESTREC                                 
*                                                                               
* CALL SPDEMEXT TO BUILD TABLE OF DEMOS IN POL ORDER                            
*                                                                               
         L     RE,AESTREC                                                       
         USING ESTHDR,RE                                                        
         LA    RF,EDEMLST                                                       
         ST    RF,DX.DXESTDEM      SET EST DEMO LIST                            
         LA    RF,EUSRNMS                                                       
         ST    RF,DX.DXUSRNMS      SET EST DEMO USER NAMES                      
* DOES BUY HAVE NON-TRAD DEMO ELEMENT?                                          
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   *+12                                                             
         LA    RF,ENONTDMS                                                      
         ST    RF,DX.DXNTNMS       SET EST DEMO USER NAMES                      
         DROP  RE                                                               
*                                                                               
         GOTOR VCALLOV,DMCB,0,X'D9000AC3'  GET ADDRESS OF SPDEMEXT              
         L     RF,0(R1)                                                         
         GOTOR (RF),BDMDMEXT                                                    
*                                                                               
         ICM   R5,15,DX.DXDEMTAB   GET A(DEMO TABLE ON RETURN)                  
         JZ    *+2                                                              
         USING DXDEMTABD,R5                                                     
*                                                                               
         OC    DXDEMCD,DXDEMCD     TEST ANY DEMOS IN TABLE                      
         JZ    BLDDEM50               NO                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DX.DXDEMCNT    GET # OF DEMOS                               
         JZ    *+2                  HAVE ZERO DEMOS, SOMETHING WRONG            
         SLL   R0,3                X 8                                          
         AHI   R0,NDEMNO-NDELEM                                                 
         STC   R0,E.NDLEN          SET NEW ELEMENT LENGTH                       
         LA    R4,E.NDEMNO                                                      
         USING NDEMNO,R4                                                        
*                                                                               
         XC    ELEM2,ELEM2         BUILD NEW POST BUY IN ELEM2                  
         ICM   RF,15,DX.DXPBDEL    GET A(POST BUY DEMEL) IF ANY                 
         JZ    BLDDEM30                                                         
*                                                                               
         LA    R3,ELEM2                                                         
         LLC   R0,DX.DXDEMCNT                                                   
         MHI   R0,3                X 3                                          
         CLI   ELEM,NDCORGQ        TEST ORIGINATING X'02'                       
         BNE   BLDDEM20                                                         
*                                                                               
PD       USING PDELEM,ELEM2        SETUP POST BUY ELEM                          
         MVI   PD.PDCODE,X'22'                                                  
         AHI   R0,PDEMO-PDELEM     R0 = TOTAL L'PDELEM                          
         STC   R0,PD.PDLEN         SET LENGTH OF PDELEM                         
         LA    R3,PD.PDEMO         R3=A(FIRST DEMO POSN IN PDELEM)              
         B     BLDDEM30                                                         
         DROP  PD                                                               
*                                                                               
SD       USING SDELEM,ELEM2        SETUP POST BUY SPILL ELEM                    
BLDDEM20 MVC   SD.SDCODE(SDEMO-SDELEM),0(RF)                                    
         AHI   R0,SDEMO-SDELEM     R0 = TOTAL L'SDELEM                          
         STC   R0,SD.SDLEN         SET LENGTH OF SDELEM                         
         LA    R3,SD.SDEMO         R3=A(FIRST DEMO POSN IN SDELEM)              
         DROP  SD                                                               
*                                                                               
BLDDEM30 MVC   NDEMNO,DXDEMCD      TABLE DEMO CODE                              
*                                                                               
         L     R0,DXDEMVAL         GET VALUE FROM TABLE                         
         SR    RF,RF                                                            
         ICM   RF,12,DXDEMFLG                                                   
         N     RF,=X'C0000000'     DROP ALL BUT OVRD/2 DEC                      
         OR    R0,RF                                                            
         STCM  R0,15,NDEMRAW       SET VALUE IN ELEMENT                         
         MVI   NDSVI,100           SET SVI                                      
*                                                                               
         OC    DX.DXPBDEL,DX.DXPBDEL HAVE POST-BUY DEMEL                        
         JZ    BLDDEM40               NO                                        
         L     R0,DXPOSTVAL                                                     
         STCM  R0,7,0(R3)          SET VALUE IN ELEMENT                         
         TM    DXPOSTFLG,X'80'      OVERRIDE?                                   
         JZ    *+8                                                              
         OI    0(R3),X'80'         YES                                          
*                                                                               
BLDDEM40 LA    R4,NDEMLNQ(R4)      NEXT DEMO SLOT IN 02/03 ELEM                 
         LA    R3,L'PDEMO(R3)      NEXT DEMO SLOT IN 22/23 ELEM                 
*                                                                               
         AHI   R5,DXDEMTABL        NEXT DEMO IN DEMEXT TABLE                    
         OC    DXDEMCD,DXDEMCD     TEST MORE DEMOS IN TABLE                     
         JNZ   BLDDEM30                                                         
         DROP  R5,R4                                                            
*                                                                               
BLDDEM50 DS    0H                                                               
         ICM   R3,15,DX.DXPBDEL    DID WE FIND A 22/23 ELEM?                    
         JZ    BLDDEM55            NO                                           
         BRAS  RE,DELEL                                                         
         MVC   BDMELEM,ELEM        SAVE ELEM                                    
         MVC   ELEM,ELEM2                                                       
         BRAS  RE,ADDEL                                                         
         MVC   ELEM,BDMELEM         RESTORE ELEM                                
*                                                                               
BLDDEM55 L     R3,DX.DXDEMEL                                                    
         LA    RF,ELEM                                                          
         CR    R3,RF               DID WE FIND A 02/03 ELEM?                    
         BE    *+12                NO                                           
         BRAS  RE,DELEL            YES, DELETE IT                               
         B     *+8                                                              
         L     R3,BDABYDEM         LOAD INSERTION ADDRESS IF NOT FOUND          
         BRAS  RE,ADDEL            AND ADD NEW ELEM BACK                        
*                                                                               
* SEED INPUT VALUES INTO DEMO ELEM                                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QADEMO        MUST HAVE INPUT DEMO VALUES                  
         JZ    *+2                                                              
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         JZ    *+2                  ZERO ENTRIES, SOMETHING WRONG               
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
*                                                                               
         USING $QADEMO,R2                                                       
BLDDEM60 DS    0H                                                               
         XC    BDDEMNO,BDDEMNO                                                  
         GOTOR VALDCD,DMCB,$QDEMONM,(0,L'$QDEMONM),BDDEMNO                      
         JNE   BLDDEMNO                                                         
*                                                                               
BLDDEM65 LLC   RF,E.NDLEN          GET ELEMENT LENGTH                           
         SHI   RF,NDEMNO-NDELEM    SUB OVERHEAD                                 
         JNP   *+2                                                              
         SRL   RF,3                / 8, GETS ME # OF DEMOS                      
*                                                                               
         L     R4,BDABYDEM         GET ADDRESS OF DEMO ELEM                     
         MVC   ELEM,0(R4)                                                       
         LA    R4,NDEMNO-NDELEM(R4) AND POINT TO DEMO ARRAY                     
         USING NDEMNO,R4                                                        
*                                                                               
BLDDEM70 CLC   NDEMNO,BDDEMNO      MATCH ON DEMO?                               
         BE    BLDDEM80             YES                                         
         LA    R4,NDEMLNQ(R4)       NO, BUMP TO NEXT IN ELEM                    
         JCT   RF,BLDDEM70                                                      
         B     BLDDEM90                                                         
*                                  MATCH FOUND                                  
BLDDEM80 BRAS  RE,SDDEMVAL         SEED INPUT VALUES TO ELEM                    
         JNE   EXITN                                                            
*                                                                               
BLDDEM90 CLI   BDDEMNO+2,0         NON-TRADITIONAL?                             
         JNE   BLDDEM99            NO                                           
         BRAS  RE,UPDNTDEM         YES, UPDATE NT-DEMO ELEMENT                  
*                                                                               
BLDDEM99 AHI   R2,$QADEMOL         NEXT ARRAY ENTRY                             
         JCT   R0,BLDDEM60                                                      
         DROP  R2                                                               
*                                                                               
         J     EXITY                                                            
*                                                                               
BLDDEMNO J     EXITN                                                            
         DROP  DX                                                               
*                                                                               
BDMWORKD DSECT                     ** BLDDEM LOCAL W/S **                       
BDABYDEM DS    A                   A(DEMO ELEM) OR INSERTION ADDRESS            
*                                                                               
         DS    0A                                                               
BDMDMEXT DS    XL(DEMEXTL)         DEMEXT BLOCK                                 
BDMELEM  DS    XL256                                                            
*                                                                               
BDDEMNO  DS    XL3                                                              
                                                                                
BDMWORKL EQU   *-BDMWORKD                                                       
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*                                                                               
*=========================================                                      
* SEED THE DEMO VALUE                                                           
*=========================================                                      
         USING $QADEMO,R2                                                       
         USING NDEMNO,R4                                                        
SDDEMVAL NTR1                                                                   
*                                                                               
         CLI   SVMED,C'X'          FOR MEDIA X, LEAVE ALONE                     
         JE    SDDMVL50                                                         
*                                                                               
         CLI   SVMED,C'R'          MEDIA R, ONLY ALLOW 1-DEC                    
         JE    SDDMVL30                                                         
*                                                                               
***************                                                                 
* PROTECTION CODE AGAINST SBTK SENDING 2-DEC FOR NON E & R                      
***************                                                                 
SDDMVL10 LA    RF,NDEMNO+1         RF=A(NSI DEMO TYPE)                          
         CLI   NDEMNO+2,0          NON-TRADITIONAL?                             
         JNE   *+8                  NO                                          
         LA    RF,$QDEMONM         RF=A(CS DEMO TYPE)                           
*                                                                               
         L     R1,ATWA                                                          
         USING TWAD,R1                                                          
         LA    RE,SVS002DP         RE=(2-DEC RTG PROFILE)                       
         CLI   0(RF),C'R'          PROC A RATING?                               
         JE    SDDMVL20             YES                                         
         CLI   0(RF),C'E'          OR EXTENDED?                                 
         JE    SDDMVL20             YES                                         
         LA    RE,SVS00A2DI        RE=(2-DEC IMP PROFILE)                       
         DROP  R1                                                               
*                                                                               
SDDMVL20 CLI   0(RE),C'Y'          AGY TURNED ON TO USE 2-DEC RTG/IMP?          
         JE    SDDMVL50                                                         
*                                                                               
* CANNOT USE 2-DECIMAL PRECISION VALUE                                          
* CHECK IF WE NEED TO ROUND TO 1-DEC                                            
*                                                                               
SDDMVL30 TM    $QDEMFLG,NDEM2DEC   TWO DECIMAL PRECISION?                       
         JZ    SDDMVL50             NO, LEAVE ALONE                             
         NI    $QDEMFLG,X'FF'-X'40'  TURN FLAG OFF                              
*                                                                               
         L     RE,$QDEMRAW         ADJUST TO 1 DECIMAL PRECISION                
         CLI   SVMED,C'R'          MEDIA R?                                     
         JNE   SDDMVL40             NO, SKIP TEST INVALID VALUE                 
         CVD   RE,MYDUB                                                         
         CLI   MYDUB+L'MYDUB-1,X'0C' 2-DEC.PREC VALUE?                          
         JNE   NO2DECRT               YES, SEND ERROR                           
*                                                                               
SDDMVL40 SRDA  RE,31               MULT*2                                       
         LHI   R1,10                                                            
         DR    RE,R1               DIV BY 10                                    
         AHI   RF,1                                                             
         SRA   RF,1                DIV BY 2                                     
         ST    RF,$QDEMRAW                                                      
*                                                                               
SDDMVL50 CLC   $QDEMRAW,=F'10000000' CHECK REASONABLE VALUE                     
         JNL   *+2                                                              
         MVI   NDSVI,100                                                        
         MVC   NDEMRAW,$QDEMRAW                                                 
         OC    NDEMRAW(1),$QDEMFLG 'OR' IN FLAGS                                
SDDMVLX  J     EXITY                                                            
         DROP  R2,R4                                                            
*                                                                               
*=========================================                                      
* UPDATE NON-TRAD DEMO ELEM                                                     
*                                                                               
* ON ENTRY : R2        A(DEMO ENTRY IN INPUT DEMO ARRAY)                        
*            R4        A(BUY NDEMNO)                                            
*                                                                               
* ON EXIT  : SVNTELEM  UPDATED                                                  
*=========================================                                      
         USING $QADEMO,R2                                                       
UPDNTDEM NTR1                                                                   
         SR    RF,RF                                                            
         ICM   RF,1,BDDEMNO+1      GET NT INDEX  00XX00                         
         JZ    *+2                 IF 0, SOMETHING IS VERY WRONG                
         MHI   RF,NTDDLEN          MULT BY L'ENTRY                              
         LA    RF,NTDOVHDQ(RF)     ADD OVERHEAD                                 
         CLM   RF,1,SVNTELEM+1     IS THIS LENGTH LARGER?                       
         JL    *+8                 NO                                           
         STC   RF,SVNTELEM+1       YES, SET AS NEW ELEM LENGTH                  
         LAY   RF,SVNTELEM-NTDDLEN(RF)  GO TO OFFSET                            
*                                                                               
         CLC   0(L'NTDDMONM,RF),$QDEMONM  SAME NAME?                            
         BE    UPDNTD10                    YES                                  
         MVC   0(L'NTDDMONM,RF),$QDEMONM   NO, SAVE THE NEW NAME                
         MVI   L'NTDDMONM(RF),0             AND CLEAR FLAGS                     
*                                                                               
UPDNTD10 MVI   BYTE,NTDDFDLK+NTDDLKNA    X'80' + X'20'                          
         CLI   ELEM,NDCORGQ        X'02' ORIGINAL?                              
         JE    UPDNTD20                                                         
         MVI   BYTE,NTDDFSDL             X'40'                                  
         CLI   ELEM,NDCSPLQ        X'03' SPILL?                                 
         JNE   *+2                                                              
UPDNTD20 DS    0H                                                               
*                                                                               
         NC    $QDEMNTF,BYTE       CLEAR BITS EXCEPT OUR CHANGE BIT             
         XI    BYTE,X'FF'          FLIP BITS SO WE CAN CLEAR THE                
         NC    L'NTDDMONM(1,RF),BYTE  REC BIT AND LEAVE REST ALONE              
         OC    L'NTDDMONM(1,RF),$QDEMNTF LASTLY, SET THE BIT                    
UPDNTDX  J     EXITY                                                            
         DROP  R2                                                               
*                                                                               
$QADEMO  DSECT                                                                  
$QDEMONM DS    XL8                                                              
$QDEMRAW DS    CL(L'NDEMRAW)                                                    
$QDEMFLG DS    XL1                                                              
$QDEMNTF DS    XL1                                                              
$QADEMOL EQU   *-$QADEMO                                                        
                                                                                
SVRDEF   CSECT                                                                  
*                                                                               
*==========================================                                     
* FIND BUY X'02' OR X'03' DEMO ELEMENT                                          
*                                                                               
* ON ENTRY: ELEM    SETUP AS 02 OR 03 DEMO                                      
*                                                                               
* ON EXIT : CC=EQ   DEMO ELEMENT FOUND                                          
*                   R3=A(BUY DEMO ELEM)                                         
*                                                                               
*           CC=NEQ  DEMO ELEMENT NOT FOUND                                      
*                   R3=A(INSERTION)                                             
*==========================================                                     
FNDBDEMO NTR1                                                                   
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
*                                                                               
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             IF REACH EOR, INSERT NOW                     
         JE    FNDBDMN                                                          
         CLI   ELEM,NDCORGQ        TEST DOING X'02' ORIG DEMOS                  
         JNE   FNDBDM10            NO, ASSUME X'03' SPILL DEMOS                 
* ORIG DEMOS                                                                    
         CLI   0(R3),NDCORGQ       THIS A X'02' ORIG DEMO                       
         JE    FNDBDMY                                                          
         DC    H'0'                                                             
                                                                                
* SPILL DEMOS                                                                   
                                                                                
         USING NDELEM,R3                                                        
FNDBDM10 CLI   0(R3),NDCSPLQ       THIS A X'03' SPILL DEMO ELEM                 
         JNE   FNDBDM20                                                         
         CLC   NDAGYMKT,ELEM+(NDAGYMKT-NDELEM)  MATCH AGY MKT NUM               
         JE    FNDBDMY                                                          
*                                                                               
FNDBDM20 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         JNE   FNDBDM10                                                         
*                                                                               
FNDBDM30 LA    R3,BDELEM           FIND INSERTION POINT                         
         MVI   ELCDLO,NDCORGQ      X'02' ORIGINAL                               
         MVI   ELCDHI,NDCSPLQ      X'03' SPILL                                  
         DROP  R2                                                               
*                                                                               
FNDBDM40 BRAS  RE,NEXTEL                                                        
         JNE   FNDBDM50                                                         
         LR    R0,R3               SAVE LAST 02/03 ADDRESS                      
         J     FNDBDM40                                                         
*                                                                               
FNDBDM50 LR    R3,R0               POINT TO LAST 02/03                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R3),X'22'         TEST 02/03 FOLLOWED BY 22/23                 
         JL    FNDBDMN                                                          
         CLI   0(R3),X'23'                                                      
         JH    FNDBDMN                                                          
         IC    R0,1(R3)            MAKE SURE TO ADD AFTER 22/23                 
         AR    R3,R0                                                            
FNDBDMN  SR    RE,RE                                                            
         J     FNDBDMX                                                          
*                                                                               
FNDBDMY  LHI   RE,1                                                             
FNDBDMX  CHI   RE,1                                                             
         XIT1  REGS=(R3)           RETURN R3 TO USER                            
*                                                                               
*=================================================================              
* VALIDATE POSTBUY DEMOS FOR ORIGINATING AND SPILL                              
*=================================================================              
                                                                                
VALPBD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
*                                                                               
         USING PDELEM,R3                                                        
         MVI   PDCODE,PDCODELQ     X'22'                                        
         LA    R3,PDEMO            POINT TO FIRST DEMO VALUE                    
         DROP  R3                                                               
*                                                                               
         CLC   LP_QMAPN,=AL2(I#CDPSPL) TEST THIS IS SPILL                       
         BNE   VALPBD10                                                         
*                                                                               
         LA    R3,ELEM                                                          
         USING SDELEM,R3                                                        
         MVI   SDCODE,SDCODELQ     X'23'                                        
         MVC   SDAGYMKT,BUSPLAMK                                                
         MVC   SDRSVMKT,BUSPLRMK                                                
         LA    R3,SDEMO            POINT TO FIRST DEMO VALUE                    
         DROP  R3                                                               
*                                                                               
VALPBD10 MVC   PBELCD,ELEM                                                      
         SR    R2,R2                                                            
         ICM   R2,15,QAPBDEMO      NOTE - DON'T USE DSECT $QADEMO HERE!         
         BZ    VALPBD30            OK, SO WE'RE DELETING                        
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
*                                                                               
VALPBD20 MVC   0(3,R3),1(R2)       MOVE 3 BYTES FROM FULLWORD                   
         CLC   0(3,R3),=X'3FFFFF'  CHECK FOR REASONABLE VALUE                   
         JNL   *+2                                                              
         OC    0(1,R3),4(R2)       'OR' IN FLAGS                                
*                                                                               
         AHI   R2,5                NEXT ARRAY ENTRY                             
         AHI   R3,3                NEXT DEMO IN ELEMENT                         
         BCT   R0,VALPBD20                                                      
*                                                                               
VALPBD30 LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         STC   R3,ELEM+1                                                        
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
         DROP  R2                                                               
*                                                                               
VALPBD40 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   0(R3),0             END OF RECORD????                            
         JE    *+2                 THIS IS STRANGE?!  DIE!                      
*                                                                               
         CLI   ELEM,PDCODELQ       TEST DOING ORIG DEMOS X'22'                  
         BNE   VALPBD70            NO                                           
* ORIG DEMOS                                                                    
         CLI   0(R3),NDCORGQ       THIS A X'02' DEMO ELEM                       
         BE    VALPBD90                                                         
         DC    H'0'                                                             
                                                                                
* SPILL DEMOS                                                                   
                                                                                
         USING NDELEM,R3                                                        
VALPBD70 CLI   0(R3),NDCSPLQ       THIS A X'03' SPILL DEMO ELEM                 
         BNE   VALPBD40                                                         
         CLC   NDAGYMKT,ELEM+(SDAGYMKT-SDELEM)  MATCH AGY MKT NUM               
         BNE   VALPBD40                                                         
*                                                                               
VALPBD90 LLC   R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
*                                                                               
         CLC   PBELCD,0(R3)                                                     
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         OC    QAPBDEMO,QAPBDEMO    DELETING?                                   
         BZ    VALPBDX              YUP                                         
         BRAS  RE,ADDEL                                                         
VALPBDX  J     EXIT                                                             
PBELCD   DS    X                                                                
PDCODELQ EQU   X'22'                                                            
SDCODELQ EQU   X'23'                                                            
         DROP  R3                                                               
         EJECT                                                                  
*============================================================                   
* VALSPT  -  PROCESS SPOT ELEMENTS                                              
*            DELETE SPOTS FOR ONE (OR ALL WEEKS)                                
*            THEN PROCESS ALL SPOTS FOR A WEEK                                  
*============================================================                   
                                                                                
VALSPT   NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,RCPOLOQ                                                   
         MVI   ELCDHI,RCPOTOQ                                                   
         DROP  R2                                                               
*                                                                               
         MVC   BUWEEKX,EFFS                                                     
         OC    BUWEEK,BUWEEK       TEST HAVE WEEK START DATE                    
         BZ    VALSPT02            NO - DELETE ALL REGELS                       
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUWEEK),WORK   GET END DATE OF WEEK              
         LHI   R0,6                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,BUWEEKX)                                  
*                                                                               
VALSPT02 BRAS  RE,NEXTEL                                                        
         BNE   VALSPT12                                                         
         B     VALSPT06                                                         
*                                                                               
VALSPT04 BRAS  RE,NEXTEL2                                                       
         BNE   VALSPT12                                                         
*                                                                               
         USING REGELEM,R3                                                       
VALSPT06 CLC   RDATE,BUWEEK        TEST PRIOR TO WEEK START                     
         BL    VALSPT02            YES - CONTINUE                               
         CLC   RDATE,BUWEEKX       TEST AFTER WEEK END                          
         BH    VALSPT12            YES - DONE                                   
         DROP  R3                                                               
*                                                                               
VALSPT10 BRAS  RE,DELEL                                                         
         CLI   0(R3),X'10'         TEST ASSOCIATED ELEM                         
         BL    VALSPT04            NO                                           
         CLI   0(R3),X'1F'                                                      
         BNH   VALSPT10            DELETE ELEMS IN RANGE 10-1F                  
         B     VALSPT04                                                         
*                                                                               
VALSPT12 SR    R4,R4                                                            
         ICM   R4,7,QASPOT+1                                                    
         BZ    VALSPT20                                                         
         USING LW_D,R4                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         BZ    VALSPT20                                                         
*                                                                               
         LA    R4,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R4                                                               
         USING $SDD,R4                                                          
         MVC   SVSPDATE,BUWEEK     INITIALIZE SPOT DATE (IF GIVEN)              
*                                                                               
VALSPT14 BRAS  RE,BLDREG           BUILD AND ADD NEW SPOT ELEMENTS              
*                                                                               
         AHI   R4,$SDDX-$SDD       BUMP TO NEXT ARRAY ENTRY                     
         BCT   R0,VALSPT14         DO FOR NUMBER OF SPOTS                       
*                                                                               
VALSPT20 DS    0H                                                               
         B     VALSPTX                                                          
*                                                                               
VALSPTX  J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*========================================================                       
* BUILD REGEL AND ASSOCIATED ELEMENTS USING $SDD                                
*========================================================                       
                                                                                
         USING $SDD,R4                                                          
BLDREG   NTR1  ,                                                                
                                                                                
         XC    ELEM,ELEM                                                        
EL       USING REGELEM,ELEM                                                     
*                                                                               
         MVI   EL.RCODE,RCPOLOQ    X'0B' - POOL ORIGINAL                        
         TM    $SDSTAT3,X'06'      TEST OTO                                     
         BZ    *+12                                                             
         MVI   EL.RCODE,RCPOTOQ    X'0C' - POOL OTO                             
                                                                                
         TM    $SDSTAT3,X'02'      TEST MINUS OTO                               
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSMINUSQ SET X'80' MINUS FLAG                         
*                                                                               
         TM    $SDSTAT3,X'10'      TEST MAKEGOOD ON NEW LINE                    
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSMGONLQ SET X'02' MAKEGOOD ON NEW LINE               
*                                                                               
         MVI   EL.RLEN,10          SET UNALLOCATED ELEM LENGTH                  
                                                                                
         OC    $SDDATE,$SDDATE     TEST REQ SPOT DATE PRESENT                   
         BNZ   *+10                 YES                                         
         MVC   $SDDATE,SVSPDATE     NO, COPY FROM PREVIOUS SPOT DATE            
*                                                                               
         OC    EL.RDATE,$SDDATE    TEST SPOT DATE PRESENT                       
         JZ    *+2                                                              
         MVC   SVSPDATE,$SDDATE                                                 
*                                                                               
         MVC   EL.RPAY,$SDCLRDT                                                 
         TM    $SDSTAT3,X'01'      TEST PAID                                    
         BNZ   BLDREG02            YES                                          
         OC    $SDCLRDT,$SDCLRDT   TEST CLEARANCE DATE                          
         BZ    BLDREG04            NO                                           
         DC    H'0'                MUST NOT BE THERE!                           
*                                                                               
BLDREG02 OC    $SDCLRDT,$SDCLRDT   TEST CLEARANCE DATE                          
         JZ    *+2                                                              
*                                                                               
BLDREG04 TM    $SDSTAT3,X'80'      TEST HIATUS                                  
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSHIATSQ SET X'04' HIATUS                             
*                                                                               
         TM    $SDSTAT3,X'08'      TEST MISSED                                  
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSMINSDQ SET X'40' SPOT HAS BEEN MINUSED              
*                                                                               
         TM    $SDSTAT3,X'20'      TEST MAKEGOOD PENDING                        
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSMKGDPQ SET X'10' MAKEGOOD PENDING                   
*                                                                               
         TM    $SDSTAT4,X'80'      TEST PRD1 PAYS ALL                           
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSB1PALQ SET X'08' BRAND 1 PAYS ALL                   
*                                                                               
         TM    $SDSTAT4,X'04'      TEST PRE-ALLOCATED                           
         BZ    *+8                                                              
         OI    EL.RSTATUS,RSNOALLQ SET X'01' PRE-ALLOCATED                      
*                                                                               
         TM    $SDSTAT4,X'02'      TEST FOR COST OVERRIDE                       
         BZ    BLDREG06                                                         
         OI    EL.RSTATUS,RSRATOVQ SET X'20' COST OVERRIDE FLAG                 
         MVC   EL.RPCOST,$SDCOST   AND AMOUNT                                   
*                                                                               
BLDREG06 L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
         CLI   BDMASPRD,0          TPOL BUY AND NO PROD ALLOC??                 
         BNE   BLDREG08                                                         
         DROP  R2                                                               
         CLI   XFRAVAIL,C'Y'       AVAIL TRANSFER?                              
         BE    BLDREG10             YES, DON'T SET BRAND ALLOCS                 
*                                   ** DUE TO COPY/PASTE BUG FOUND **           
*                                   ** IN DESKTOP V4.0.0.30 & PRIOR**           
BLDREG08 LA    R1,$SDPRD1                                                       
         BRAS  RE,FINDPRD                                                       
         BL    BLDREG10            NOT ALLOCATED                                
         BH    VALERR20            INVALID                                      
*                                                                               
         MVI   EL.RLEN,RLPOL1LQ    UDPATE ELEM LENGTH                           
         MVC   EL.RPPRD,0(R1)                                                   
         MVC   EL.RPTIME,$SDLEN1                                                
         MVC   EL.RPPAYSEQ,$SDCLRSQ                                             
*                                                                               
         BRAS  RE,ADDPRD           ADD PRODUCT TO IOBRDLST                      
*                                                                               
         LA    R1,$SDPRD2          TEST FOR SECOND PRD                          
         BRAS  RE,FINDPRD                                                       
         BL    BLDREG10            NOT ALLOCATED                                
         BH    VALERR20            INVALID                                      
*                                                                               
         MVI   EL.RLEN,RLPOL2LQ                                                 
         MVC   EL.RPPRD+4(L'RPPRD),0(R1)                                        
         MVC   EL.RPTIME+4(L'RPTIME),$SDLEN2                                    
         BRAS  RE,ADDPRD           ADD PRODUCT TO IOBRDLST                      
*                                                                               
BLDREG10 LA    R1,$SDMGCD                                                       
         CLI   0(R1),C' '          TEST NOT THERE                               
         BNH   BLDREG20                                                         
         CLI   0(R1),C'$'          TEST TOKEN                                   
         BE    BLDREG12            YES                                          
         BRAS  RE,GETMGBIN         ELSE CONVERT 2 CHAR TO BINARY                
         CLI   HALF,0              2-BYTE BINARY MG CODE?                       
         JNE   *+2                 DIE, DON'T SUPPORT THIS YET                  
         MVC   EL.RPSTAT2,HALF+1                                                
         B     BLDREG20                                                         
*                                                                               
BLDREG12 LA    R1,SVMGTKNS         TRANSLATE MG TOKEN                           
         LHI   R0,SVNWMGMX                                                      
         USING MGTOKEND,R1                                                      
*                                                                               
BLDREG14 CLI   MGTOKEN,C'$'        ANY MORE TOKENS?                             
         BNE   BLDREG16                                                         
         CLC   MGSTATN,SVBSTA      SAME STATION?                                
         BNE   BLDREG15                                                         
         CLC   MGTOKEN,$SDMGCD     SAME CODE?                                   
         BNE   BLDREG15                                                         
*                                                                               
* FIX SBT BUG THAT SEEDS THE MG-BIN CODE ON SPOTS FOR A MAKEGOOD LINE           
*                                  - HWON 12/10/2013                            
         L     RF,ABUYREC                                                       
         LA    RF,BDELEM-BUYRECD(RF)                                            
         CLC   BDMGDATE-BDELEM(L'BDMGDATE,RF),MGCDALPH                          
         BE    BLDREG20              SAME CODE, DON'T PUT                       
         B     BLDREG18                                                         
*                                                                               
BLDREG15 AHI   R1,L'SVMGTKNS                                                    
         BCT   R0,BLDREG14                                                      
         DC    H'0'                                                             
*                                                                               
BLDREG16 MVC   MGSTATN,SVBSTA      SAVE STATION AND                             
         MVC   MGTOKEN,$SDMGCD     SAVE NEW TOKEN IN TABLE                      
         GOTOR GETMGCD,(R1)        GET NEW ALPHA/BINARY CODES                   
*                                                                               
BLDREG18 MVC   EL.RPSTAT2,MGCDBIN                                               
         DROP  R1                                                               
*                                                                               
BLDREG20 L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM           FIND INSERTION POINT                         
         USING REGELEM,R3                                                       
         MVI   ELCDLO,RCPOLOQ      X'0B'-POOL ORIGINAL                          
         MVI   ELCDHI,RCPOTOQ      X'0C' - POOL OTO                             
         SR    R7,R7                                                            
         DROP  R2                                                               
*                                                                               
BLDREG22 BRAS  RE,NEXTEL                                                        
         BNE   BLDREG24                                                         
         CLC   EL.RDATE,RDATE      COMPARE DATES                                
         BL    BLDREG30            IF LOW, INSERT NOW                           
         LR    R7,R3               SAVE ADDRESS OF LAST 0B/0C                   
         B     BLDREG22                                                         
         DROP  EL                                                               
*                                                                               
BLDREG24 LTR   R7,R7               INSERT NEW AFTER LAST 0B/0C                  
         BZ    BLDREG30            UNLESS THERE WASN'T ONE                      
         LR    R3,R7                                                            
         SR    R0,R0                                                            
*                                                                               
BLDREG26 IC    R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEM                           
         CLI   0(R3),0                                                          
         BE    BLDREG30                                                         
         CLI   0(R3),X'20'         WATCH OUT FOR AUX ELEMS                      
         BL    BLDREG26                                                         
*                                                                               
BLDREG30 XR    RF,RF                                                            
         ICM   RF,1,$SDSPOTS       GET NUMBER OF OCCURRENCES                    
         BNZ   *+8                                                              
         LHI   RF,1                                                             
*                                                                               
         CHI   RF,100                                                           
         JNL   *+2                 NO MORE THAN 100                             
*                                                                               
         BRAS  RE,ADDEL                                                         
         BCT   RF,*-4                                                           
*                                                                               
         OC    $SDADATE,$SDADATE   TEST FOR AFFID DATA                          
         BZ    BLDREG36                                                         
*                                                                               
         XC    ELEM,ELEM           ADD AFFIADVIT ELEMENT                        
EL       USING AFFELEM,ELEM                                                     
         MVI   EL.ACODE,ACCODEQ                                                 
         MVI   EL.ALEN,6                                                        
         MVC   EL.ADATE,$SDADATE                                                
         MVC   EL.ATIME,$SDATIME                                                
         BRAS  RE,BUMPADD                                                       
         DROP  EL                                                               
*                                                                               
         OC    $SDAFLM1,$SDAFLM1   TEST FOR FILMS                               
         BZ    BLDREG36                                                         
         XC    ELEM,ELEM                                                        
EL       USING FLMELEM,ELEM                                                     
         MVI   EL.FLMCODE,FLMCODEQ                                              
         MVI   EL.FLMLEN,5                                                      
         MVC   EL.FLMDAY,$SDADAY                                                
         MVC   EL.FLMNUM,$SDAFLM1                                               
         OC    $SDAFLM2,$SDAFLM2                                                
         BZ    *+14                                                             
         MVI   EL.FLMLEN,7         UPDATE ELEMENT LENGTH                        
         MVC   EL.FLMNUM+2(2),$SDAFLM2                                          
         BRAS  RE,BUMPADD                                                       
         DROP  EL                                                               
*                                                                               
BLDREG36 OC    $SDDFLM,$SDDFLM     TEST DEALER TAG DATA                         
         BZ    BLDREG38                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
EL       USING DLTGID,ELEM                                                      
         MVI   EL.DLTGID,DLTGIDQ                                                
         MVI   EL.DLTGLN,DLTLENQ                                                
         MVC   EL.DLTGCSQ+1(2),$SDDFLM                                          
         MVC   EL.DLTGTAG,$SDDTAG                                               
         MVC   EL.DLTGDTE,$SDDDATE                                              
         BRAS  RE,BUMPADD                                                       
*                                                                               
BLDREG38 OC    $SDTFLM1,$SDTFLM1   TEST TRAFFIC CMML ASSIGN                     
         BZ    BLDREGX                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
EL       USING TRACID,ELEM                                                      
         MVI   EL.TRACID,TRACIDQ                                                
         MVI   EL.TRACLN,TRACLEN                                                
         MVC   EL.TRACCSQ,$SDTFLM1                                              
         MVC   EL.TRACCSQ2,$SDTFLM2                                             
         MVC   EL.TRACREF,$SDTPATT                                              
         BRAS  RE,BUMPADD                                                       
         DROP  EL                                                               
*                                                                               
BLDREGX  J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
$SDD     DSECT                                                                  
                                                                                
$SDDATE  DS    XL(L'RDATE)         SPOT DATE                                    
                                                                                
$SDSTAT  DS    0XL4                SPOT STATUS BYTES                            
$SDSTAT1 DS    X                   ** STATUS BYTE 1 **                          
$SDSTAT2 DS    X                   ** STATUS BYTE 2 **                          
$SDSTAT3 DS    X                                                                
*              X'80'  HIATUS                                                    
*              X'40'  MINUSED SPOT PAID                                         
*              X'20'  MAKEGOOD PENDING                                          
*              X'10'  MAKEGOOD ON NEW LINE                                      
*              X'08'  MISSED                                                    
*              X'04'  +OTO                                                      
*              X'02'  -OTO                                                      
*              X'01'  PAID                                                      
$SDSTAT4 DS    X                                                                
*              X'80'  PRODUCT 1 PAYS ALL (PIGGIES)                              
*              X'40'  MAKEGOOD CODE PRESENT                                     
*              X'20'  FILM DATA PRESENT                                         
*              X'10'  AFFIDAVIT DATA AVAILABLE                                  
*              X'08'  ALLOCATED                                                 
*              X'04'  PRE-ALLOCATED                                             
*              X'02'  COST OVERRIDE                                             
*              X'01'  SPOT IN NEXT WEEK                                         
                                                                                
$SDAV1   DS    0X                  ** ALLOCATION (1) VALUES **                  
$SDPRD1  DS    CL(L'EKEYPRD)       ALLOCATED PRODUCT 1 CODE                     
$SDLEN1  DS    XL(L'RPTIME)        PRODUCT 1 SECONDS LENGTH                     
$SDPRD2  DS    CL(L'EKEYPRD)       ALLOCATED PRODUCT 2 CODE                     
$SDLEN2  DS    XL(L'RPTIME)        PRODUCT 2 SECONDS LENGTH                     
$SDAV1L  EQU   *-$SDAV1                                                         
                                                                                
$SDCOST  DS    XL(L'RPCOST)        COST OVERRIDE                                
$SDMGCD  DS    CL2                 MAKEGOOD CODE                                
                                                                                
$SDCLRV  DS    0X                  ** CLEARANCE VALUES **                       
$SDCLRDT DS    XL(L'RPAY)          CLEARANCE DATE                               
$SDCLRSQ DS    XL(L'RPPAYSEQ)      CLEARANCE SEQUENCE                           
$SDCLRL  EQU   *-$SDCLRV                                                        
                                                                                
$SDADATE DS    XL(L'ADATE)         AFFIDAVIT DATE                               
$SDATIME DS    XL(L'ATIME)         AFFIDAVIT TIME                               
                                                                                
$SDADAY  DS    XL(L'FLMDAY)        FILM ROTATION                                
$SDAFLM1 DS    XL(L'FLMNUM)        FILM CODE 1 NUMBER                           
$SDAFLM2 DS    XL(L'FLMNUM)        FILM CODE 2 NUMBER                           
                                                                                
$SDDLRV  DS    0X                  ** DEALER TAG VALUES **                      
$SDDFLM  DS    XL(L'DLTGCSQ-1)     DEALER TAG SEQUENCE                          
$SDDTAG  DS    XL(L'DLTGTAG)       DEALER TAG NUMBER                            
$SDDDATE DS    XL(L'DLTGDTE)       DEALER INSTRUCTION DATE                      
$SDDLRL  EQU   *-$SDDLRV                                                        
                                                                                
$SDTRFV  DS    0X                  ** TRAFFIC DATA **                           
$SDTFLM1 DS    XL(L'TRACCSQ)       TRAFFIC FILM 1 NUMBER                        
$SDTFLM2 DS    XL(L'TRACCSQ2)      TRAFFIC FILM 2 NUMBER                        
$SDTPATT DS    XL(L'TRACREF)       TRAFFIC PATTERN REFERENCE                    
$SDTRFL  EQU   *-$SDTRFV                                                        
                                                                                
$SDSPOTS DS    XL1                 SPOT REPETITION FACTOR                       
                                                                                
$SDDUL   EQU   *-$SDD                                                           
$SDDX    EQU   *                                                                
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
                                                                                
*=================================================================              
* BUILD COMMENT ELEMENTS                                                        
*=================================================================              
                                                                                
VALCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,CMCODEQ      REMOVE ALL X'66' COMMENTS FROM BUY           
         MVI   ELCDHI,CMCODEQ                                                   
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   VALCOM4                                                          
         DROP  R2                                                               
*                                                                               
VALCOM2  BRAS  RE,DELEL                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    VALCOM2                                                          
*                                                                               
VALCOM4  LA    R4,QACOMM1                                                       
*                                                                               
VALCOM10 XC    ELEM,ELEM                                                        
         MVI   ELEM,CMCODEQ        X'66' COMMENT ELEMENT                        
*                                                                               
         L     R2,0(R4)            POINT TO INPUT MAP                           
         USING LW_D,R2                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-8               DATA LENGTH                                  
         BNP   VALCOM12                                                         
         LA    RF,3(RE)            ELEM LEN IS 3 BYTES MORE                     
         STC   RF,ELEM+1                                                        
*                                                                               
         MVC   ELEM+2(1),LW_CODE+1 SET COMMENT NUMBER                           
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+3(0),LW_DATA1                                               
         BRAS  RE,BUMPADD                                                       
*                                                                               
VALCOM12 LA    R4,4(R4)            NEXT COMMENT AREA                            
         LA    R0,QACOMM5                                                       
         CR    R4,R0                                                            
         BNH   VALCOM10                                                         
         J     EXIT                                                             
         DROP  R2                                                               
*=================================================================              
* BUILD CONTRACT (ID OR BUYID) ELEMENT                                          
*=================================================================              
                                                                                
VALCON   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ABUYREC          GET BUYREC ADDRESS                           
         USING BUYRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,IDELCODQ     NOW FIND THE OLD X'70' ID ELEMENT            
         MVI   ELCDHI,IDELCODQ                                                  
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         DROP  R2                                                               
*                                                                               
         CLI   QCONTRCT,C' '                                                    
         JNH   EXIT                                                             
         XC    ELEM,ELEM                                                        
ID       USING IDELEM,ELEM                                                      
         MVI   ID.IDELCOD,IDELCODQ       X'70' ID ELEMENT                       
         MVI   ID.IDELLEN,IDELLENQ                                              
         MVC   ID.IDCONNO,QCONTRCT                                              
         DROP  ID                                                               
*                                                                               
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXIT                                                             
         EJECT                                                                  
*================================================                               
* VALIDATE CHECKSUMS FOR ALL RECS                                               
*================================================                               
                                                                                
VALCKSM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R2,15,QACKSM        TEST CHECKSUM PRESENT                        
         JZ    *+2                                                              
         USING LW_D,R2                                                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
         USING $CKSM,R2                                                         
*                                                                               
VCKSM20  MVC   QTOKEN,$CKSMTOK     SET TOKEN                                    
         MVC   IODAOVER,$CKSMDA    SET D/A                                      
         OC    IODAOVER,IODAOVER                                                
         BNZ   *+6                                                              
         DCHO                                                                   
                                                                                
         LHI   R1,IOBGETUP+IOSPTFIL+B#CKSMREC                                   
         CLI   $CKSMFIL,C'X'                                                    
         BNE   *+8                                                              
         LHI   R1,IOBGETUP+IOXSPFIL+B#CKSMREC                                   
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    VCKSM30                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         JNZ   VALERR11             YES, DON'T DIE, JUST SEND AN ERROR          
         DCHO                                                                   
*                                                                               
VCKSM30  L     RE,ACKSMREC                                                      
         CLI   $CKSMFIL,C'X'                                                    
         BE    VCKSM40                                                          
                                                                                
         CLI   0(RE),X'10'         BUY RECORD?                                  
         BNH   VCKSM50              NO                                          
*                                                                               
         MVC   SV1OR2,SAVE1OR2                                                  
         MVC   GBY1OR2,SAVE1OR2                                                 
*                                                                               
         CLC   LP_AGY,=C'SJ'       IF AGENCY SJ AND                             
         JNE   VCKSM35                                                          
         CLC   1(2,RE),=X'CC2B'    CLIENT TBL?                                  
         JE    *+10                                                             
         CLC   1(2,RE),=X'BCC9'    CLIENT PG0?                                  
         JE    *+10                                                             
         CLC   1(2,RE),=X'BCDA'    CLIENT PG1?                                  
         JNE   VCKSM35                                                          
         MVI   GBY1OR2,2           SET 2-BYTE BUYLINE OVERRIDE                  
         MVI   SV1OR2,2                                                         
*                                                                               
VCKSM35  MVC   GBYIOA,ACKSMREC     CONVERT BUY RECORD TO 2 BYTE BUYLINE         
         MVI   GBYACT,GBYCONV                                                   
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         B     VCKSM50                                                          
*                                                                               
VCKSM40  CLI   DRVKTYP-DRVKEY(RE),DRVKTYPQ                                      
         BNE   VCKSM50                                                          
         CLI   DRVKSUB-DRVKEY(RE),DWKKSUBQ  WORKSHEET/AVAIL RECORD?             
         BNE   VCKSM50                                                          
         MVI   XFRAVAIL,C'Y'                                                    
*                                                                               
VCKSM50  XR    RF,RF                                                            
         L     RE,ACKSMREC                                                      
         ICM   RF,3,BUYRLEN-BUYREC(RE)   SPTFIL LENGTH                          
         CLI   $CKSMFIL,C'X'                                                    
         BNE   VCKSM60                                                          
         ICM   RF,3,DRVRLEN-DRVKEY(RE)   XSPFIL LENGTH                          
*                                                                               
VCKSM60  XR    R1,R1                                                            
         CKSM  R1,RE                                                            
         JO    *-4                                                              
         CLM   R1,15,$CKSMVAL      TEST COMPUTED CKSM = PASSED CKSM             
         JNE   VALERR17            FATAL ERROR                                  
*                                                                               
         AHI   R2,$CKSML           NEXT ARRAY ENTRY                             
         BCT   R0,VCKSM20                                                       
         DROP  R2                                                               
*                                                                               
         J     EXIT                                                             
*                                                                               
$CKSM    DSECT                                                                  
$CKSMTOK DS    CL(L'QTOKEN)                                                     
$CKSMFIL DS    CL1                 FILE - DEFAULT SPTFIL, X=XSPFIL              
$CKSMDA  DS    XL4                 DISK ADDR                                    
$CKSMVAL DS    XL4                 CKSM TO VALIDATE                             
$CKSML   EQU   *-$CKSM                                                          
                                                                                
SVRDEF   CSECT                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DARE LOCKED ORDERS                                                  
*                                                                               
* IO3 USED FOR READING ORDERS                                                   
* IO5 USED FOR BUILDING OUR LIST TO SEND TO SPLNK12                             
***********************************************************************         
                                                                                
CKDLCK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AIO5             INITIALIZE LIST                              
         LHI   R1,IO5LQ            1ST 2 BYTES IS N'ENTRIES                     
         SR    RE,RE                 FOLLOWED BY THE ACTUAL ENTRIES             
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ICM   R2,15,QALOCK        TEST A(LOCKED ORDERS) PRESENT                
         JZ    *+2                                                              
         USING LW_D,R2                                                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
         USING $LOCK,R2                                                         
*                                                                               
VCDLK010 MVC   BYTE,$LCKBAMD       LET'S ISOLATE THE MEDIA                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'02'          ARE WE RADIO?                                
         BNE   VCDLK012             NO, THEN CHECK LOCK                         
*                                                                               
         MVC   BYTE,$LCKBSTA+2     ISOLATE BAND IN STATION                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'06'          ONLY CHECK DARE LOCK FOR RADIO               
         BE    VCDLK011             IF NOT STREAM -SM,                          
         CLI   BYTE,X'07'            OR                                         
         BNE   VCDLK040             IF NOT IHEART -CM, SKIP LOCK                
*                                                                               
VCDLK011 XC    IOKEY,IOKEY                                                      
VCDLK012 LA    R3,IOKEY                                                         
         USING DOKEY,R3                                                         
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,$LCKBAMD                                                 
         MVC   DCKCLT,$LCKBCLT                                                  
         MVC   DCKPRD,$LCKBPRD                                                  
         MVC   DCKEST,$LCKBEST+1   BINARY EST IS STILL ONLY 1 BYTE              
         MVC   DCKSTA,$LCKBSTA                                                  
         MVC   DCKPRD2,$LCKBPR2                                                 
         MVC   DCKFLTNM,$LCKBFLT                                                
         MVI   DCKFLAG,0                                                        
         CLI   $LCKCORT,C'T'       LOOKING FOR TRADE?                           
         JNE   *+8                                                              
         OI    DCKFLAG,DCKFTRDE    YES                                          
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOSPTDIR+B#ORDREC'                      
         JNE   VCDLK040                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ORDREC'                        
         JNE   VCDLK040                                                         
*                                                                               
         L     R3,AORDREC          LET'S SEE IF ORDER WAS EXPORTED              
         USING DOKEY,R3                                                         
         LA    R3,DORFRST                                                       
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,NEXTEL                                                        
         JNE   VCDLK015                                                         
         CLI   DOSPIMTH-DOSPELD(R3),C'E'                                        
         BE    VCDLK040            SKIP LOCK CHECKING IF EXPORTED               
         CLI   DOSPIMTH-DOSPELD(R3),C'F'                                        
         BE    VCDLK040            SKIP LOCK CHECKING IF FAXED                  
*                                                                               
VCDLK015 L     R3,AORDREC                                                       
         USING DOKEY,R3                                                         
         LA    R3,DORFRST                                                       
         DROP  R3                                                               
         MVI   ELCDLO,DOSTELQ                                                   
         MVI   ELCDHI,DOSTELQ                                                   
*                                                                               
VCDLK020 BRAS  RE,NEXTEL                                                        
         JNE   VCDLK040                                                         
         USING DOSTELD,R3                                                       
         CLI   DOSTSTAT,DDLVRD     A DELIVERED STATUS?                          
         JE    VCDLK020                                                         
         CLI   DOSTSTAT,DFXDLVD                                                 
         JE    VCDLK020                                                         
         CLI   DOSTSTAT,DEMDLVD                                                 
         JE    VCDLK020            YES, GET NEXT ELEMENT INSTEAD                
*                                                                               
         LA    RE,LST_STAT                                                      
VCDLK030 CLI   0(RE),X'FF'         STATUS IS NOT IN THE TABLE?                  
         JE    VCDLK060            ADD ORDER'S DATA TO LIST IN AIO5             
*                                                                               
         CLC   DOSTSTAT,0(RE)      MATCH ON STATUS                              
         JE    VCDLK040            MEANS ORDER IS NOT LOCKED                    
         LA    RE,1(RE)                                                         
         J     VCDLK030                                                         
                                                                                
VCDLK040 AHI   R2,$LOCKL           NEXT ARRAY ENTRY                             
         JCT   R0,VCDLK010                                                      
*                                                                               
         J     VCDLK090            PROCESSED ALL REQUESTED DATA                 
         DROP  R3                                                               
***********************************                                             
* IO5    BYTES 0-1                 NUMBER OF ENTRIES IN LIST                    
*        BYTES 2-???               ENTRIES THAT ARE POINTED BY $LCKLSTD         
***********************************                                             
VCDLK060 L     RE,AIO5                                                          
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         MHI   RF,$LCKLSTL                                                      
         LA    RE,2(RE)            POINT TO THE FIRST ENTRY                     
         AR    RE,RF                                                            
*                                                                               
         USING $LCKLSTD,RE                                                      
         MVC   $LCKLSDA,IODA                                                    
         MVC   $LCKLSPK,$LCKPCKY                                                
         DROP  RE                                                               
*                                                                               
         L     RE,AIO5             BUMP UP N'ENTRIES IN OUR LIST                
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         AHI   RF,1                                                             
         STCM  RF,3,0(RE)                                                       
         J     VCDLK040            ANY MORE LOCKED ORDERS TO FIND?              
         DROP  R2                                                               
***********************************                                             
* LET'S SEE IF WE HAVE ANYTHING TO PASS TO SPLNK12                              
***********************************                                             
VCDLK090 L     RE,AIO5                                                          
         OC    0(2,RE),0(RE)       ANY LOCKED ENTRIES?                          
         BZ    VCDLKX              NONE                                         
         MVI   ERRORFLG,FATALERR   SET TO IGNORE ALL FURTHER INPUT              
*                                                                               
         L     R4,ALIOB                                                         
         USING LIOBD,R4                                                         
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTSRU',I#SDORDL)               
*                                                                               
         L     RE,AIO5             CALC LENGTH OF ENTIRE LIST INCL              
         SR    RF,RF                  THE 2 BYTES FOR THE N'ENTRIES             
         ICM   RF,3,0(RE)                                                       
         MHI   RF,$LCKLSTL                                                      
         AHI   RF,2                                                             
         ST    RF,DMCB+12                                                       
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),                          +        
               ('LIOTLRQ',1),('LQ_TLSTQ',AIO5),,0                               
*                                                                               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                    
*                                                                               
VCDLKX   J     EXIT                                                             
         DROP  R4                                                               
***************                                                                 
* THESE STATI ARE CONSIDERED UNLOCKED AS PER SPBUY01                            
***************                                                                 
LST_STAT DC    AL1(QRJCT)          REJECTED                                     
         DC    AL1(QCFMD)          CONFIRMED                                    
         DC    AL1(QUNDARE)        UNDARED                                      
         DC    AL1(QNODARE)        NOT DARE ANY MORE                            
         DC    AL1(QERRORED)       SF ERROR                                     
         DC    AL1(QRECALL)        RECALLING IS OKAY FOR AUTO-RECALL            
         DC    AL1(QRCLAPPR)       RECALL OF APPROVED ORDER                     
         DC    AL1(QRCLDELN)       RECALL OF DELIVERED ORDERE                   
         DC    AL1(QRCLTRNS)       RECALL AND TRANSMITTED TO STA                
         DC    AL1(QRCLWIP)        RECALL AND WORK IN PROGRESS                  
         DC    AL1(QEMPTY)         NO BUYS WHEN THIS ORDER WAS SENT             
         DC    AL1(QBYRCNFM)       BUYER CONFIRMED                              
         DC    AL1(QSNTXCNF)       SENT CANCELLED, RECALL CONF W/COM            
         DC    AL1(QSNTXREJ)       SENT CANCELLED, RECALLED REJECTED            
         DC    AL1(DFXSENT)        FAX SENT                                     
         DC    AL1(DFXRSNT)        FAX RE-SENT                                  
         DC    X'FF'               END OF TABLE                                 
***************                                                                 
*                                                                               
$LOCK    DSECT                                                                  
$LCKBAMD DS    XL1                 AGENCY/MEDIA                                 
$LCKBCLT DS    XL2                 CLIENT                                       
$LCKBPRD DS    XL1                 PRODUCT ALLOCATION                           
$LCKBPR2 DS    XL1                 PIGGYBK ALLOCATION                           
$LCKBEST DS    XL2                 ESTIMATE                                     
$LCKBSTA DS    XL3                 STATION                                      
$LCKBFLT DS    X                   FLIGHT NUMBER                                
$LCKCORT DS    CL1                 CASH/TRADE FLAG                              
$LCKPCKY DS    CL12                PCKEY                                        
$LOCKL   EQU   *-$LOCK                                                          
*                                                                               
$LCKLSTD DSECT                                                                  
$LCKLSDA DS    XL4                 DISK ADDRESS                                 
$LCKLSPK DS    CL12                PCKEY                                        
$LCKLSTL EQU   *-$LCKLSTD                                                       
                                                                                
SVRDEF   CSECT                                                                  
*                                                                               
         EJECT                                                                  
*================================================                               
* ORBITS                                                                        
*================================================                               
                                                                                
         USING VOWORKD,RC                                                       
VALORB   NTR1  BASE=*,LABEL=*,WORK=(RC,VOWORKL)                                 
*                                                                               
         XC    ELEM,ELEM           INIT                                         
*                                                                               
         ICM   R2,15,QAORBIT                                                    
         BZ    VORB130             DELETING ALL ORBITS                          
         USING LW_D,R2                                                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         BZ    VORB130              NONE, MUST BE DELETING ORBITS               
*                                                                               
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
         USING $ORBIT,R2                                                        
*                                                                               
* READ POL ESTIMATE RECORD AND SAVE PRIMARY DEMO TYPE                           
*                                                                               
         MVC   FULL(3),=C'POL'     READ THE POL ESTIMATE                        
         BRAS  RE,GTESTREC         INTO AESTREC                                 
*                                                                               
         L     R5,AESTREC                                                       
         USING ESTHDR,R5                                                        
         MVC   VODEMTYP,EDEMLTYP   SAVE NSI PRIMARY DEMO TYPE                   
         LLC   RF,EDEMLNU1                                                      
         ICM   RF,B'0010',EDEMLIS1 BYTE1&3 ZERO, NON-TRADITIONAL?               
         LTR   RF,RF               HAVE NON-TRADITIONAL DEMO CAT?               
         JNZ   VORB010              NO, HAVE TRADITIONAL                        
*                                                                               
         LLC   RF,EDEMLTYP         GET INDEX                                    
         SHI   RF,1                SUB 1 FROM INDEX                             
         MHI   RF,L'ENONTDMS       MULTIPLE * L'ENTRY = OFFSET                  
         LA    RE,ENONTDMS         RE=A(START OF NONTRAD DEMO LIST)             
         LA    RF,0(RF,RE)         RF=A(OFFSET INTO NONTRAD DEMO LIST)          
*                                                                               
         LA    RE,20*L'ENONTDMS(RE) RE=A(END OF NONTRAD DEMO LIST)              
         CR    RF,RE               BEYOND NONTRAD DEMO LIST?                    
         JNL   VORB010              YES, SKIP IT                                
         MVC   VODEMTYP,0(RF)      SAVE CS PRIMARY DEMO TYPE                    
         DROP  R5                                                               
*                                                                               
VORB010  MVI   ELEM,X'67'          SETUP BUY ORBIT (X'67') ELEM                 
         LR    RF,R0               COMPUTE ELEM LENGTH (4+16*N'SHOWS)           
         SLL   RF,4                                                             
         AHI   RF,4                                                             
         STC   RF,ELEM+1                                                        
         LA    R3,ELEM+4                                                        
*                                                                               
         USING ORBDAY,R3                                                        
VORB100  MVC   ORBDAY,$ORBDAY                                                   
         MVC   ORBTIME(2),$ORBTIMS                                              
         MVC   ORBTIME+2(2),$ORBTIME                                            
         MVC   ORBDESC,$ORBPRG     SET ORBIT PGM                                
         MVC   ORBDEM,$ORBDEMV     SET ORBIT DEMO VALUE                         
*                                                                               
         CLI   VODEMTYP,C'R'       PRIM DEMO TYPE IS RATING?                    
         JE    VORB110                                                          
         CLI   VODEMTYP,C'E'       OR EXTENDED                                  
         JE    VORB110                                                          
*                                  CONFIRMED WE HAVE IMPRESSION                 
         TM    $ORBDEMF,X'40'      IMP HAS 2-DEC FLAG?                          
         JZ    VORB110             NO                                           
         CLC   ORBDEM,=X'3FFF'     IS IMP VALUE TO LARGE TO STORE?              
         JNH   VORB110             NO                                           
         NI    $ORBDEMF,X'BF'      FIRST CLEAR 2-DEC FLAGS                      
         LLH   RE,ORBDEM           THEN ROUND 2-DEC TO 1-DEC                    
         SRDA  RE,31                                                            
         LHI   R1,10                                                            
         DR    RE,R1                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         STCM  RF,3,ORBDEM                                                      
*                                                                               
VORB110  OC    ORBDEM(1),$ORBDEMF  SET ORBIT DEMO FLAGS                         
*                                                                               
         LA    R3,16(R3)           NEXT ARRAY IN 67 ELEM                        
         AHI   R2,$ORBITL          NEXT ARRAY ENTRY                             
         BCT   R0,VORB100                                                       
         DROP  R2,R3                                                            
*                                                                               
VORB130  MVI   ELCDLO,X'67'        FIND AND DELETE OLD ORBIT                    
         MVI   ELCDHI,X'67'                                                     
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         JNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         OC    ELEM,ELEM           DELETING ORBIT?                              
         JZ    VORBX               YES                                          
         BRAS  RE,ADDEL                                                         
*                                                                               
VORBX    J     EXIT                                                             
         DROP  RC                                                               
*                                                                               
VOWORKD  DSECT                                                                  
VODEMTYP DS    C                   POL ESTIMATE PRIMARY DEMO TYPE               
VOWORKL  EQU   *-VOWORKD                                                        
*                                                                               
$ORBIT   DSECT                                                                  
$ORBDAY  DS    X                   ORBIT DAY                                    
$ORBTIMS DS    CL2                 ORBIT TIME START                             
$ORBTIME DS    CL2                 ORBIT TIME END                               
$ORBPRG  DS    CL7                 ORBIT DESCRIPTION                            
$ORBDEMV DS    CL2                 ORBIT DEMO VALUE                             
$ORBDEMF DS    CL1                 ORBIT DEMO FLAGS                             
*                                  -X'80'-OVERRIDE, X'40'-2DECIMAL              
$ORBITL  EQU   *-$ORBIT                                                         
                                                                                
SVRDEF   CSECT                                                                  
*                                                                               
         EJECT                                                                  
*================================================                               
* PACKAGES                                                                      
*================================================                               
                                                                                
VALPKG   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM             BUILD NEW PKG ELEMENT                        
         USING PKGELEM,RF                                                       
         MVI   PKGCODE,PKGCODEQ    PACKAGE (X'05')                              
                                                                                
         CLI   QPKGTYP,0           DID WE GET A PKG TYPE?                       
         BE    VALPKG10             NO - JUST DELETE PKG AND EXIT               
*                                                                               
         ICM   R2,15,QAPKGLIN                                                   
         JZ    *+2                 MUST BE REF LINES IF PKG TYPE                
         USING LW_D,R2                                                          
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         BZ    VALPKGX                                                          
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
*                                                                               
         LR    RE,R1               SET PKG ELEM LENGTH                          
         AHI   RE,3                                                             
         STC   RE,PKGLEN                                                        
*                                                                               
         MVC   PKGIND,QPKGTYP      SET PKG TYPE                                 
         OI    PKGIND,X'10'        SET 2-BYTE PKG LINE NUMBERS                  
         LA    RE,PKGLINES                                                      
*                                                                               
         BCTR  R1,0                MOVE ALL LINES IN ARRAY TO PKG ELEM          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R2)                                                    
         DROP  RF                                                               
*                                                                               
         MVC   SV1OR2,SAVE1OR2                                                  
         CLC   LP_AGY,=C'SJ'       IF AGENCY SJ AND                             
         JNE   VALPKG06                                                         
         CLC   QCLTA,=C'TBL'       CLIENT TBL?                                  
         JE    *+10                                                             
         CLC   QCLTA,=C'PG0'       CLIENT PG0?                                  
         JE    *+10                                                             
         CLC   QCLTA,=C'PG1'       CLIENT PG1?                                  
         JNE   VALPKG06                                                         
         MVI   SV1OR2,2            SET 2-BYTE BUYLINE OVERRIDE                  
*                                                                               
VALPKG06 CLI   SV1OR2,2            2 BYTE BUYLINES?                             
         JE    VALPKG10             YES, DO NOTHING                             
***                                                                             
* CODE BELOW CONVERTS PACKAGE 2-BYTE BUYLINE BACK TO 1 BYTE                     
***                                                                             
         NI    ELEM+PKGIND-PKGELEM,X'FF'-X'10'  UNSET 2-BYTE FLAG               
         LLC   R0,ELEM+PKGLEN-PKGELEM                                           
         AHI   R0,-3                                                            
         SRL   R0,1                SET R0 FOR NUMBER OF LINES                   
         LA    RE,ELEM+PKGLINES-PKGELEM                                         
         LR    RF,RE                                                            
                                                                                
VALPKG07 MVC   0(1,RF),1(RE)       MOVE 2-BYTE LINE TO 1-BYTE FIELD             
         LA    RF,1(RF)                                                         
         LA    RE,2(RE)                                                         
         BRCT  R0,VALPKG07                                                      
                                                                                
         LA    R0,ELEM                                                          
         SR    RF,R0               GET NEW ELEMENT LENGTH                       
         STC   RF,ELEM+PKGLEN-PKGELEM                                           
***                                                                             
* CODE ABOVE CONVERTS PACKAGE 2-BYTE BUYLINE BACK TO 1 BYTE                     
***                                                                             
*                                                                               
VALPKG10 MVC   ELCDLO,ELEM                                                      
         MVC   ELCDHI,ELEM                                                      
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         CLI   QPKGTYP,0           DID WE GET A PKG TYPE?                       
         BE    *+8                  NO - JUST EXIT                              
         BRAS  RE,ADDEL                                                         
                                                                                
VALPKGX  J     EXIT                                                             
                                                                                
         EJECT                                                                  
*================================================                               
* SECOND COST                                                                   
*================================================                               
                                                                                
VALCOS2  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'71'                                                       
         CLI   MYBYTE,C'F'         C2 FACTOR (NOT DOLLAR)?                      
         BNE   *+8                                                              
         MVI   ELEM,X'73'          FACTOR IS X'73'                              
*                                                                               
         MVI   ELEM+1,COS2LENQ                                                  
         TP    MYDUB               TEST VALID PACKED                            
         BNZ   VALCOS20            NOT VALID, THIS WILL DELETE COS2             
*                                                                               
         L     R3,ABUYREC                                                       
         CLI   MYBYTE,C'F'         C2 FACTOR (NOT DOLLAR)?                      
         BE    VALCOS10            YES, DON'T CARE ABOUT $ NOT CENTS            
*                                                                               
         OI    MYDUB+7,X'0F'       MAKE COS2 POSITIVE                           
         TM    BDCIND2-BUYREC(R3),BDCNBRDQ  X'10' - BUY IN DOLLARS?             
         BNZ   VALCOS40            YES, ADJUST COS2 TO BE IN DOLLARS            
         CP    MYDUB,=P'16777215'  COS2 > X'FFFFFF'?                            
         BH    VALCOS30                                                         
*                                                                               
VALCOS10 CVB   RF,MYDUB                                                         
         STCM  RF,7,ELEM+3                                                      
*                                                                               
VALCOS20 MVC   ELCDLO,ELEM                                                      
         MVC   ELCDHI,ELEM                                                      
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         TP    MYDUB               DON'T ADD ELEM IF NO VALUE                   
         BNZ   *+8                                                              
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
VALCOS30 XR    RE,RE               SINCE COST WASN'T ORIGINALLY IN $            
         ICM   RE,7,BDCOST-BUYREC(R3)                                           
         CVD   RE,MYDUB2           WE HAVE TO MODIFY AS SUCH BECAUSE            
         SRP   MYDUB2,64-2,0       COS2 IS IN DOLLARS                           
         CVB   RE,MYDUB2                                                        
         STCM  RE,7,BDCOST-BUYREC(R3)                                           
*                                                                               
VALCOS40 SRP   MYDUB,64-2,0        DIVIDE BY 100 AND THEN STORE IT              
         L     R3,ABUYREC                                                       
         OI    BDCIND2-BUYREC(R3),BDCNBRDQ  SET X'10' - BUY IN DOLLARS          
         B     VALCOS10                                                         
*                                                                               
         EJECT                                                                  
*================================================                               
* PURPOSE CODES                                                                 
*================================================                               
                                                                                
VALPUR   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING IDELEM,RF                                                        
         MVI   IDELCOD,IDELCODQ                                                 
         MVI   IDELLEN,IDELLENQ                                                 
         MVC   IDCONNO,PURCODE                                                  
         DROP  RF                                                               
*                                                                               
         MVC   ELCDLO,ELEM                                                      
         MVC   ELCDHI,ELEM                                                      
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         CLC   ELEM+IDCONNO-IDELEM(L'IDCONNO),SPACES  DON'T ADD BLANK           
         BNH   *+8                                                              
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
                                                                                
         EJECT                                                                  
*================================================                               
* UPGRADE FORUMLA - NOTE: MUST COME BEFORE DEMO VALUES!                         
*================================================                               
                                                                                
         USING VUWORKD,RC                                                       
VALUPG   NTR1  BASE=*,LABEL=*,WORK=(RC,VUWORKL)                                 
                                                                                
         LR    R0,RC               CLEAR SAVED STORAGE                          
         LHI   R1,VUWORKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         BNO   *+6                 DESTRUCTIVE MOVE?                            
         DCHO                                                                   
                                                                                
         MVI   BYTE,0                                                           
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING UPELEM,R2                                                        
         MVI   0(R2),X'62'         UPGRADE ELEMENT                              
         MVI   1(R2),49                                                         
         MVI   UPFILE,C'T'                                                      
         MVI   UPSRC,C'N'                                                       
                                                                                
         LA    R1,UPTEXT           GET L'INPUT STRING                           
         LR    R0,R1                                                            
         BCTR  R0,0                                                             
         AHI   R1,L'UPTEXT-1                                                    
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,R0                                                            
                                                                                
         STC   R1,VUCARDH+5        PARSE FORMULA FOR PARMS                      
         AHI   R1,8                                                             
         STC   R1,VUCARDH                                                       
         MVC   VUCARD(L'UPTEXT),UPTEXT                                          
         OC    VUCARD,SPACES                                                    
         GOTOR VSCANNER,DMCB,('VULENQ',VUCARDH),(6,VUSCAN),C',=,='              
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VU100                                                            
         LA    R3,VUSCAN                                                        
         USING SCANBLKD,R3                                                      
                                                                                
VU10     CLC   =C'UPT',SC1STFLD                                                 
         BE    VU20                                                             
         CLC   =C'BK',SC1STFLD                                                  
         BE    VU30                                                             
         CLC   =C'DT',SC1STFLD                                                  
         BE    VU40                                                             
         CLC   =C'PUT',SC1STFLD                                                 
         BE    VU50                                                             
         CLC   =C'RP',SC1STFLD                                                  
         BE    VU50                                                             
         CLC   =C'SHR',SC1STFLD                                                 
         BE    VU60                                                             
         CLC   =C'RTG',SC1STFLD                                                 
         BE    VU60                                                             
         B     VU90                                                             
                                                                                
VU20     MVC   UPINPUT,SC2NDFLD                                                 
                                                                                
         XC    VUCARD,VUCARD       FORMAT FOR PUT/MAY06                         
         LLC   R1,SC2NDLEN                                                      
         STC   R1,VUCARDH+5                                                     
         AHI   R1,8                                                             
         STC   R1,VUCARDH                                                       
         AHI   R1,-9                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VUCARD(0),SC2NDFLD                                               
         OC    VUCARD,SPACES                                                    
         GOTOR VUPVAL,DMCB,(1,VUCARDH),WORK,(C'/',ACOMFACS)                     
         CLI   0(R1),0             TEST VALID UPGRADE EXPRESSION                
         BNE   *+6                                                              
         DCHO                                                                   
         MVC   UPTYPE,WORK+4                                                    
         B     VU90                                                             
                                                                                
VU30     DS    0H                  PARSE BK=                                    
         XC    VUCARD,VUCARD                                                    
         MVC   VUCARD(20),SC2NDFLD                                              
         OC    VUCARD,SPACES                                                    
         GOTOR VSCANNER,DMCB,(C'C',VUCARD),(1,VUSCAN2),C',=,('                  
         CLI   4(R1),1                                                          
         BE    *+6                                                              
         DCHO                                                                   
* CREATE FLDHDR FOR BOOKVAL *                                                   
         XC    VUCARD,VUCARD                                                    
         LLC   RF,VUSCAN2                                                       
         STC   RF,VUCARDH+5                                                     
         AHI   RF,8                                                             
         STC   RF,VUCARDH                                                       
         AHI   RF,-9                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VUCARD(0),VUSCAN2+12                                             
         OC    VUCARD,SPACES                                                    
*                                                                               
* NOTE: CODE DOES NOT SUPPORT WEEK NUMBER                                       
* WORKING CODE CAN BE FOUND IN DDUPVAL AT LABEL "WHAT22"                        
*                                                                               
         GOTOR VBOOKVAL,DMCB,(C'N',VUCARDH),(1,MYDUB),VSCANNER                  
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DCHO                                                                   
         TM    MYDUB,X'BF'         TEST ANY GARBAGE OPTIONS SPECIFIED           
         BZ    *+6                                                              
         DCHO                                                                   
         MVC   UPFBK,MYDUB+1                                                    
         MVC   SVUPBOOK,UPFBK                                                   
                                                                                
         MVI   SVUPBTYP,0          CLEAR BOOKTYPE                               
         CLI   VUSCAN2+1,0         TEST FOR BOOKTYPE                            
         BE    VU90                                                             
         CLI   VUSCAN2+22+1,C')'   IS IT ONE CHARACTER?                         
         BNE   *+8                                                              
         MVI   VUSCAN2+22+1,C' '   YES, MOVE IN A SPACE                         
*                                                                               
         GOTOR (#TRNSBT,ATRNSBT),DMCB,VUSCAN2+22,2,SVUPBTYP,0                   
         B     VU90                                                             
                                                                                
VU40     MVC   VUCARD,SPACES       PARSE DT=                                    
         MVC   VUCARD(20),SC2NDFLD                                              
         GOTOR VSCANNER,DMCB,(C'C',VUCARD),(1,VUSCAN2),C',=,/'                  
         CLI   4(R1),1                                                          
         BE    *+6                                                              
         DCHO                                                                   
         GOTOR VCALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         LLC   R4,VUSCAN2+0                                                     
         GOTOR (RF),(R1),((R4),VUSCAN2+12),MYBYTE,MYBYTE2                       
         CLI   MYBYTE,0                                                         
         BNE   *+6                                                              
         DCHO                                                                   
         MVC   UPDAYTIM(1),MYBYTE                                               
* EDIT TIME *                                                                   
         LLC   R4,VUSCAN2+1                                                     
         GOTOR VTIMVAL,DMCB,((R4),VUSCAN2+22),MYFULL                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DCHO                                                                   
         MVC   UPDAYTIM+1(4),MYFULL                                             
         B     VU90                                                             
                                                                                
VU50     OI    MYBYTE,X'80'        PUT=/RP=                                     
         CLC   =C'RP',SC1STFLD                                                  
         BNE   *+8                                                              
                                                                                
VU60     OI    BYTE,X'01'          SHR=/RTG=                                    
         CLC   =C'1',SC2NDFLD                                                   
         BE    VU90                                                             
         CLC   =C'2',SC2NDFLD                                                   
         BE    *+6                                                              
         DCHO                                                                   
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   UP2YRP,C'Y'                                                      
         TM    BYTE,X'01'                                                       
         BZ    *+8                                                              
         MVI   UP2YRS,C'Y'                                                      
         B     VU90                                                             
                                                                                
VU90     AHI   R3,SC2NDFLD-SCANBLKD+VULENQ                                      
         BCT   R0,VU10                                                          
                                                                                
         DROP  R2,R3                                                            
*                                                                               
VU100    MVC   ELCDLO,ELEM                                                      
         MVC   ELCDHI,ELEM                                                      
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         CLC   ELEM+UPINPUT-UPELEM(L'UPINPUT),SPACES  DON'T ADD BLANK!          
         BNH   *+8                                                              
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  RC                                                               
                                                                                
VUWORKD  DSECT                     ** VALUPG LOCAL W/S **                       
VUCARDH  DS    XL8                                                              
VUCARD   DS    CL80                SCANNER INPUT                                
VUSCAN   DS    6XL(SCBLKLQ)        SCANNER OUTPUT                               
VUSCAN2  DS    XL(SCBLKLQ)                                                      
                                                                                
VUWORKL  EQU   *-VUWORKD                                                        
VULENQ   EQU   25                  LENGTH OVERRIDE FOR SECOND SCAN FIELD        
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* AVERAGE BOOK FORUMLA                                                          
*================================================                               
                                                                                
VALAVG   NTR1  BASE=*,LABEL=*                                                   
         XC    UPTEXT,UPTEXT                                                    
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING AVGBKLEM,RF                                                      
         MVI   AVGBKEL,AVGBKELQ                                                 
                                                                                
         ICM   R2,15,QAAVGBK                                                    
         BZ    VALAVG10                                                         
         USING LW_D,R2                                                          
                                                                                
         XR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-8               DATA LENGTH                                  
         BNP   VALAVG10                                                         
         LA    R0,AVGBKLNQ(RE)     UPDATE ELEM LENGTH                           
         STC   R0,AVGBKLN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   AVGBOOK(0),LW_DATA1                                              
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   UPTEXT(0),LW_DATA1            SAVE IN UPTEXT                     
         CLC   LW_DATA1(3),=C'AVG'                                              
         BE    VALAVG10                                                         
         MVI   ELEM+1,AVGBKLNQ                                                  
         DROP  R2,RF                                                            
                                                                                
VALAVG10 MVC   ELCDLO,ELEM                                                      
         MVC   ELCDHI,ELEM                                                      
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         CLI   ELEM+1,AVGBKLNQ                                                  
         BL    *+8                                                              
         BRAS  RE,ADDEL                                                         
*                                                                               
         OC    UPTEXT,UPTEXT                                                    
         BZ    VALAVG15                                                         
         CLC   UPTEXT(3),=C'AVG'                                                
         JNE   VALUPG                                                           
*                                                                               
VALAVG15 LLC   R1,ELEM+1                                                        
         LA    RE,ELEM(R1)                                                      
         LA    RF,ELEM+AVGBOOK-AVGBKLEM                                         
VALAVG20 CR    RE,RF                                                            
         BNH   VALAVGX                                                          
         CLI   0(RE),C'('                                                       
         BE    VALAVG30                                                         
         BCT   RE,VALAVG20                                                      
         DC    H'0'                                                             
*                                                                               
VALAVG30 CLI   2(RE),C')'                                                       
         BNE   VALAVGX                                                          
         MVC   SVUPBTYP,1(RE)                                                   
         B     VALAVGX                                                          
*                                                                               
VALAVG40 CLI   3(RE),C')'                                                       
         BNE   VALAVGX                                                          
         MVC   HALF,1(RE)                                                       
         GOTOR (#TRNSBT,ATRNSBT),DMCB,HALF,2,SVUPBTYP,0                         
*                                                                               
VALAVGX  J     EXIT                                                             
*================================================                               
* REASON CODES                                                                  
*================================================                               
                                                                                
VALRSN   NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING RCELEM,RF                                                        
         MVI   RCELCOD,RCELCODQ                                                 
         MVI   RCELLEN,RCELLENQ                                                 
         MVC   RCELRC,MYDUB                                                     
         MVC   RCFLDID,MYBYTE                                                   
                                                                                
         ICM   R2,15,QACOMM1       ADD REASON TEXT TO ELEM                      
         BZ    VALRSN10                                                         
         USING LW_D,R2                                                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-8               DATA LENGTH                                  
         BNP   VALRSN10                                                         
         LA    R0,RCELLENQ(RE)     UPDATE ELEM LENGTH                           
         STC   R0,RCELLEN                                                       
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RCELTXT(0),LW_DATA1                                              
         DROP  R2,RF                                                            
*                                                                               
VALRSN10 MVC   ELCDLO,ELEM                                                      
         MVC   ELCDHI,ELEM                                                      
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
         CLC   ELEM+RCELRC-RCELEM(L'RCELRC),SPACES                              
         BNH   *+8                                                              
         BRAS  RE,ADDEL                                                         
                                                                                
VALRSNX  J     EXIT                                                             
                                                                                
         EJECT                                                                  
*=================================================================              
* BUILD AUTOMATED AVAIL UUID ELEMENT                                            
*=================================================================              
                                                                                
VALAAU   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,AAVCODQ      CHECK IF ALREADY PRESENT                     
         MVI   ELCDHI,AAVCODQ                                                   
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JE    HAVEUUID            YES, ALREADY PRESENT, SEND ERROR             
         DROP  R2                                                               
*                                                                               
         ICM   R2,15,QAAAUID       DID WE GET UUID?                             
         JZ    MISSUUID            NO, SEND ERROR                               
         USING LW_D,R2                                                          
*                                                                               
AAV      USING AAVELEM,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVI   ELEM,AAVCODQ        X'7A' COMMENT ELEMENT                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LW_LN          ENTRY LENGTH                                 
         AHI   RE,-8               GET DATA LENGTH                              
         JNP   MISSUUID             SEND ERROR, HAVE EMPTY STRING               
         LA    RF,AAVLENQ(RE)      PLUS ELEM OVERHEAD                           
         STC   RF,AAV.AAVLEN                                                    
*                                                                               
         BCTR  RE,0                                                             
         MVC   AAV.AAVUUID(0),LW_DATA1                                          
         EX    RE,*-6                                                           
         BRAS  RE,ADDEL                                                         
*                                                                               
         J     EXITY                                                            
         DROP  R2,AAV                                                           
*===============================================================                
* WRITE BUY RECORD TO FILE                                                      
*===============================================================                
                                                                                
ENDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVACTION,QACTADD    ADDING A NEW BUYLINE?                        
         BE    *+12                                                             
         CLI   SVACTION,QACTOVER                                                
         BNE   ENDBUY05                                                         
         L     R3,ABUYREC          MAKE SURE THERE ARE SPOTS                    
         AHI   R3,BDELEM-BUYRECD                                                
         MVI   ELCDLO,RCPOLOQ      X'0B'-POOL ORIGINAL                          
         MVI   ELCDHI,RCPOTOQ      X'0C'-POOL OTO                               
         BRAS  RE,NEXTEL                                                        
         BNE   BUYNOSPT            NO SPOTS, DON'T ALLOW IT                     
*                                                                               
ENDBUY05 L     RF,AC2LKTBL                                                      
         CLI   0(RF),C'Y'                                                       
         BE    ENDBUY10                                                         
         BRAS  RE,RDC2RECS                                                      
*                                                                               
ENDBUY10 L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
         CLI   BDMASPRD,0          TPOL BUY AND NO PROD ALLOC??                 
         BNE   ENDBUY18                                                         
*                                                                               
         LA    R3,BDELEM                                                        
ENDBUY12 CLI   0(R3),0             ANY MORE BRANDS?                             
         BE    ENDBUY20            NO MORE BRANDS                               
         CLI   0(R3),RCPOLOQ       X'0B'-POOL ORIGINAL                          
         BE    ENDBUY16                                                         
         CLI   0(R3),RCPOTOQ       X'0C' - POOL OTO                             
         BE    ENDBUY16                                                         
ENDBUY14 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ENDBUY12                                                         
*                                                                               
ENDBUY16 L     RF,AC2LKTBL                                                      
         CLI   RLEN-REGELEM(R3),RPALLOC-REGELEM   ANY ALLOCATION??              
         BNH   ENDBUY14                           NONE, NEXT SPOT               
         LLC   RE,RPALLOC-REGELEM(R3)  IS BRAND C2 LOCKED?                      
         AR    RE,RF                                                            
         CLI   0(RE),C'Y'                                                       
         BE    BUYC2LKD                                                         
         CLI   RLEN-REGELEM(R3),RLPOL1LQ          MORE THAN 1 BRAND?            
         BNH   ENDBUY14                           NO, NEXT SPOT                 
         LLC   RE,RPALLOC2-REGELEM(R3)  IS PIGGY C2 LOCKED?                     
         AR    RE,RF                                                            
         CLI   0(RE),C'Y'                                                       
         BE    BUYC2LKD                                                         
         B     ENDBUY14            CHECK ALL THE SPOTS                          
*                                                                               
ENDBUY18 L     RF,AC2LKTBL                                                      
         LLC   RE,BDMASPRD         LET'S SEE IF THIS PROD IS C2 LOCKED          
         AR    RE,RF                                                            
         CLI   0(RE),C'Y'                                                       
         BE    BUYC2LKD            WHAT DO YOU KNOW, IT IS                      
         CLI   BDMASPRD+1,0        DO WE HAVE A PIGGYBACK PRD?                  
         BE    ENDBUY20                                                         
         LLC   RE,BDMASPRD+1       PIGGYBACK PROD IS C2 LOCKED?                 
         AR    RE,RF                                                            
         CLI   0(RE),C'Y'                                                       
         BE    BUYC2LKD            IT IS                                        
*                                                                               
ENDBUY20 L     RF,ACLTREC                                                       
         TM    COPT4-CLTHDR(RF),COP4MIDS  X'20' MIDAS CLIENT?                   
         BZ    ENDBUY25                                                         
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,X'71'        COST2                                        
         MVI   ELCDHI,X'71'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    ENDBUY25                                                         
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,X'73'        COST2 FACTOR?                                
         MVI   ELCDHI,X'73'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    ENDBUY25                                                         
*                                                                               
         MVI   MYBYTE,0                                                         
         ZAP   MYDUB,=P'0'                                                      
         BRAS  RE,VALCOS2                                                       
*                                                                               
* READ AVAIL/REVLINE EARLY SO HAVE IT FOR ADDDTXEL                              
ENDBUY25 OC    QLINADDR,QLINADDR   IF NO AVAIL PASSED, EXIT                     
         BZ    ENDBUY30                                                         
         MVC   IODAOVER,QLINADDR   SET D/A OF AVAIL/REVLINE                     
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOXSPFIL+B#REVREC'                   
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
ENDBUY30 DS    0H                                                               
*                                                                               
* IF SPILL WAS UPDATED, DELETE SPILL&PBSPILL NOT UPDATED                        
*                                                                               
         TM    PROCFLAG,PROCSPIL   WAS SPILL UPDATED?                           
         JZ    *+8                  NO                                          
         BRAS  RE,UPDSPILL          YES, GO CLEAN UP SPILL IN BUY               
*                                                                               
* FIND AND UPDATE X'50' NTDELEM                                                 
*                                                                               
         CLI   SVNTELEM+1,0        HAVE NON-TRAD ELEM TO ADD/UPDATE?            
         JE    *+8                  NO                                          
         BRAS  RE,ADDNTEL                                                       
*                                                                               
* THIS SECTION WILL ADD OR UPDATE BUY ACTIVITY ELEMENT                          
*                                                                               
         CLI   SVACTION,QACTADD    IS ACTION AN ADD OR                          
         BE    ENDBUY31             YES                                         
         CLI   SVACTION,QACTOVER   OVERWRITE BUY?                               
         BNE   ENDBUY35             NO                                          
ENDBUY31 OI    BDSTAT3,BDST3_DSKADD  SET ADDED BY DESKTOP FOR DMDAPTRS          
         BRAS  RE,ADDACTEL                                                      
*                                                                               
         CLI   SVACTION,QACTOVER   DON'T ADDREC DELETED REC!                    
         BE    ENDBUY40                                                         
*                                                                               
         BRAS  RE,ADDDTXEL                                                      
         BRAS  RE,BUYVAL                                                        
         MVI   GBYACT,GBYADD                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTFIL+B#BUYREC'                     
         JNE   *+2                                                              
*                                                                               
* PREVENT SBTK FROM ADDING NEW LINE WITH PAID DATA   SPEC-49771                 
*                                                                               
         BRAS  RE,PDCHKSUM                                                      
         OC    FULL,FULL           HAVE PAID DATA?                              
         BZ    ENDBUY45             NO                                          
         DC    H'0'                 YES, DON'T ALLOW                            
*                                                                               
* FOR ACTION OTHER THAN ADD/OVERWRITE, MUST COMPARE CHECKSUM                    
*                                                                               
ENDBUY35 BRAS  RE,PDCHKSUM         GET PAID ELEMENT CHKSUM                      
         CLC   FULL,CKSUMBFR                                                    
         JNE   *+2                                                              
         OI    BDSTAT3,BDST3_DSKCHG  SET CHANGED BY DESKTOP                     
*                                                                               
* FIND AND UPDATE ACTIVITY ELEMENT                                              
*                                                                               
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,X'99'                                                     
         MVI   ELCDHI,X'99'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+12                                                             
         BRAS  RE,ADDACTEL                                                      
         B     ENDBUY40                                                         
*                                                                               
         USING ACTVELEM,R3                                                      
         L     R1,LP_ASECD                                                      
         MVC   ACTVCHG,SECOPASS-SECD(R1)                                        
         GOTO1 VDATCON,DMCB,(5,0),(3,ACTVCHG+2)                                 
         DROP  R3                                                               
*                                                                               
ENDBUY40 BRAS  RE,ADDDTXEL         UPDATE DESKTOP XFER ELEM                     
         BRAS  RE,BUYVAL                                                        
*                                                                               
         MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#BUYREC'                     
         JNE   *+2                                                              
*                                                                               
* RUN SINGLE BUY DOWNLOAD                                                       
*                                                                               
ENDBUY45 BRAS  RE,CHKAUTH          ADD SUPERDESK AUTH RECORDS                   
*                                                                               
         CLI   SVACTION,QACTDEL                                                 
         BE    ENDBUY60            DON'T SEND DELETED BUY                       
         CLI   SVACTION,QACTDELA                                                
         BE    ENDBUY60         DON'T SEND DELETED BUY OR AVAIL/REVLINE         
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRUN',I#SDBUYR)               
*                                                                               
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE                 
         BE    ENDBUY47             YES, SEND PCKEY                             
         OC    LP_VRSN,LP_VRSN     NO PC VERSION?                               
         BZ    ENDBUY47             YES, SEND PCKEY                             
* SEND PC KEY AFTER VERSION 3.00.00.120                                         
         CLC   LP_VRSN,=AL1(3,0,0,120)  03.00.00.120                            
         BL    ENDBUY50                                                         
ENDBUY47 GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',D#PCKEY),        X        
               ('LD_VSTRQ',QTOKEN),(L'QTOKEN,0)                                 
*                                                                               
ENDBUY50 GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',D#DA),           X        
               ('LD_HEXDQ',IODA),(L'IODA,0)                                     
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERU',0),0,0                    
*                                                                               
* RETURN TOKEN, D/A, AND ACTION ON ADD                                          
         GOTOR UPLRSP,DMCB,I#SDXCNF  SEND ADD/CHG UPLOAD RESPONSE               
*                                                                               
         CLI   QROUTE,C'Y'                                                      
         BNE   ENDBUY55                                                         
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTSRU',I#SDRD4D)               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_HEXDQ',BUYKAM),(L'BUYKAM,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',2),              X        
               ('LD_HEXDQ',BUYKCLT),(L'BUYKCLT,0)                               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',3),              X        
               ('LD_HEXDQ',BUYKSTA),(L'BUYKSTA,0)                               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',7),              X        
               ('LD_VSTRQ',QSTA),(L'QSTA,0)                                     
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERU',0),0,0                    
*                                                                               
ENDBUY55 CLI   SVACTION,QACTADD                                                 
         BE    *+12                                                             
         CLI   SVACTION,QACTOVER                                                
         BNE   ENDBUY60                                                         
         BRAS  RE,DARBATCH                                                      
                                                                                
* UPDATE AVAIL/REVLINE ACTIVITY ELEM                                            
                                                                                
ENDBUY60 OC    QLINADDR,QLINADDR   IF NO AVAIL PASSED, EXIT                     
         BZ    ENDBUYX                                                          
                                                                                
         USING DRVRECD,R4                                                       
         L     R4,AREVREC                                                       
*                                                                               
* IF WE HAVEN'T ALREADY, RUN A AVAIL/REV LINE DOWNLOAD                          
*                                                                               
         CLI   SVACTION,QACTDELA   DON'T SEND DELETED AVAIL/REVLINE             
         BE    ENDBUY75                                                         
         CLC   SLINADDR,QLINADDR                                                
         BE    ENDBUY75                                                         
         MVC   SLINADDR,QLINADDR                                                
         LHI   R0,I#SDRLDR                                                      
         CLI   DRVKSUB,DRVKSUBQ    X'10' - REVISION SUBTYPE?                    
         BE    ENDBUY70                                                         
         LHI   R0,I#SDALDR                                                      
         CLI   DRVKSUB,DWKKSUBQ    X'11' - AVAIL SUBTYPE?                       
         JNE   *+2                  UKNOWN SUBTYPE                              
*                                                                               
* NOTE LIOTSRU COMMAND BELOW.  THIS WILL PREVENT LINKIO FROM                    
* OVERWRITING THE LAST MAP ELEM PUT TO THE OUTPUT RECORD.  THIS IS              
* NEEDED HERE BECAUSE THERE ARE 2 RUN COMMANDS, WITH DATA INBETWEEN             
                                                                                
ENDBUY70 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTSRU',(R0))                   
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',D#DA),           *        
               ('LD_HEXDQ',QLINADDR),(L'IODA,0)                                 
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERU',0),0,0                    
*                                                                               
ENDBUY75 TM    DRVRST1,DRVKSXFR    PREVIOUSLY TRANSFERRED/UPDATED?              
         BNZ   ENDBUY85            YES, UPDATE AVAIL/REVLINE WITH               
*                                     BUY DETAILS                               
         LA    R3,DRVEL            UPDATE LINE DESCRIPTION ELEMENT              
         CLI   0(R3),RLDELQ                                                     
         BE    ENDBUY80                                                         
         MVI   ELCDLO,RLDELQ                                                    
         MVI   ELCDHI,RLDELQ                                                    
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DCHO                                                                   
         USING RLDELD,R3                                                        
ENDBUY80 MVC   RLDBLINE,BUYKBUY                                                 
         DROP  R3                                                               
*                                                                               
         LA    R3,DRVEL            UPDATE TRANSFER DATA IN ACTIVITY EL          
         MVI   ELCDLO,RLAELQ                                                    
         MVI   ELCDHI,RLAELQ                                                    
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DCHO                                                                   
         USING RLAELD,R3                                                        
                                                                                
         L     R1,LP_ASECD                                                      
         MVC   RLAXPID,SECOPASS-SECD(R1)                                        
         BRAS  RE,GETDTTM                                                       
***HW -  REMOVED, AS THIS WOULD CAUSE MISSING BUYLINES IF TRANSFER              
***HW -  HAPPENED AT THE CUSP/END OF THE MINUTE                                 
***HW    CLC   RLAXFR(5),WORK      IF XFER DATE/TIME NOT NOW, DEL ...           
***HW    BE    ENDBUY85             XFER ELEM AND SET DATE/TIME                 
         MVC   RLAXFR,WORK                                                      
         MVC   RLAXTIM,WORK+2                                                   
         DROP  R3                                                               
*                                                                               
         GOTOR VHELLO,DMCB,(C'D',=C'XSPFIL'),('RLXELQ',AREVREC),0,0             
                                                                                
* ADD NEW AVAIL/REVLINE TRANSFER ELEM                                           
                                                                                
         USING RLXELD,R3                                                        
ENDBUY85 XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         MVI   RLXEL,RLXELQ                                                     
         MVI   RLXLEN,RLXENQ                                                    
         MVC   RLXBUYLN,BUYKBUY                                                 
         MVC   RLXACT,SVACTION                                                  
         CLI   SVACTION,QACTDELA   IF THE BUY WAS DELETED,                      
         BE    ENDBUY90             THE ELEM WILL ONLY HAVE THE LINE #          
         CLI   SVACTION,QACTDEL    IF THE BUY WAS DELETED,                      
         BE    ENDBUY90             THE ELEM WILL ONLY HAVE THE LINE #          
         MVC   RLXFLTST,BDSTART                                                 
         MVC   RLXFLTEN,BDEND                                                   
         MVC   RLXDAYS,BDDAY                                                    
         MVC   RLXRATE+1(3),BDCOST                                              
         TM    BDCIND2,BDCNBRDQ    TEST X'10' COST IN DOLLARS                   
         BZ    ENDBUY90                                                         
         XR    RF,RF                                                            
         ICM   RF,7,BDCOST                                                      
         MHI   RF,100              CONVERT TO PENNIES                           
         STCM  RF,15,RLXRATE                                                    
                                                                                
ENDBUY90 GOTOR VHELLO,DMCB,(C'P',=C'XSPFIL'),AREVREC,ELEM,0                     
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
                                                                                
* UPDATE STATUS IN RECORD AND KEY                                               
                                                                                
         OI    DRVRST1,DRVKSXFR    X'40' LINE TRANSFERRED                       
         CLI   SVACTION,QACTDELA   MARK AVAIL/REVLINE HISTORY?                  
         BNE   *+8                                                              
         OI    DRVRST1,DRVKSRPL    X'80' LINE REPLACED                          
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOXSPFIL+B#REVREC'                     
                                                                                
K        USING DRVKEY,IOKEY                                                     
         MVC   IOKEY(L'DRVKEY),DRVKEY                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOXSPDIR'   NOTE: NO I/O AREA          
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         OI    K.DRVKST1,DRVKSXFR    X'40' LINE TRANSFERRED                     
         CLI   SVACTION,QACTDELA   MARK AVAIL/REVLINE HISTORY?                  
         BNE   *+8                                                              
         OI    K.DRVKST1,DRVKSRPL    X'80' LINE REPLACED                        
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR'                               
         JNE   *+2                                                              
         DROP  K,R2                                                             
                                                                                
ENDBUYX  J     EXIT                                                             
         EJECT                                                                  
*================================================                               
* CLEAN UP SPILL AND POST BUY SPILL                                             
*  IF SPILL WAS NOT UPDATED, THEN DELETE IT AND POST BUY SPILL                  
*================================================                               
UPDSPILL NTR1  BASE=*,LABEL=*                                                   
         L     R2,ABUYREC                                                       
         MVI   ELCDLO,NDCSPLQ      READ ALL X'03' SPILL FROM BUY                
         MVI   ELCDHI,NDCSPLQ                                                   
         LA    R3,BDELEM-BUYRECD(R2)                                            
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                 MUST HAVE SPILL                              
*                                                                               
         USING NDELEM,R3                                                        
UPDSPL20 LA    RE,SVSPLMKS         UPDATE SPILL LIST                            
         LA    RF,SVSPLMKS+L'SVSPLMKS-1                                         
UPDSPL40 CR    RE,RF               END OF LIST?                                 
         JH    UPDSPL60             YES, DELETE THE SPILL                       
         OC    0(L'BUSPLAMK,RE),0(RE)  OPEN SLOT?                               
         JZ    UPDSPL60                 YES, DELETE THE SPILL                   
         CLC   NDAGYMKT,0(RE)      FOUND MATCH?                                 
         BE    UPDSPLX              YES, DON'T DELETE                           
         LA    RE,L'BUSPLAMK(RE)   GET NEXT SPILL IN LIST                       
         J     UPDSPL40                                                         
         DROP  R3                                                               
*                                                                               
UPDSPL60 BRAS  RE,DELEL                                                         
         CLI   0(R3),X'23'         DELETED SPILL HAVE POSTBUY-SPILL?            
         JE    UPDSPL60             YES, DELETE IT                              
*                                                                               
UPDSPL80 BRAS  RE,NEXTEL2          HAVE ANY MORE SPILL?                         
         JE    UPDSPL20             YES, CHECK AGAINST SPILL LIST               
UPDSPLX  J     EXITY                                                            
*================================================                               
* ADD THE NTDELEM TO THE BUY RECORD                                             
*================================================                               
         USING ADDNTELD,RC                                                      
ADDNTEL  NTR1  BASE=*,LABEL=*,WORK=(RC,ADDNTELL)                                
*                                                                               
         MVC   ADNTELEM,ELEM       SAVE ELEM                                    
*                                                                               
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,NTDELCDQ     X'50'-NON-TRAD DEMO ELEM                     
         MVI   ELCDHI,NTDELCDQ                                                  
         BRAS  RE,NEXTEL           DID WE FIND A X'50' NON TRAD ELEM?           
         JNE   ADNTE020             NO, LETS JUST ADD OURS                      
         XC    ELEM,ELEM            YES, LETS SAVE IT IN ELEM                   
         LLC   RE,1(R3)                                                         
         BCTR  RE,0                                                             
         MVC   ELEM(0),0(R3)                                                    
         EX    RE,*-6                                                           
         BRAS  RE,DELEL            DELETE THE OLD X'50' ELEM                    
*                                                                               
         LA    RF,ELEM+2                                                        
         LA    RE,SVNTELEM+2                                                    
         LLC   R1,SVNTELEM+1                                                    
         LA    R1,SVNTELEM(R1)                                                  
*                                                                               
ADNTE010 CR    RE,R1               END OF SVNTELEM? FINISHED?                   
         JNL   ADNTE020             YES                                         
         OC    0(NTDDLEN,RE),0(RE)  NO, SVNTELEM MISSING ENTRY?                 
         JNZ   *+10                  NO, DON'T COPY FROM OLD                    
         MVC   0(NTDDLEN,RE),0(RF)  YES, COPY FROM OLD                          
         LA    RF,NTDDLEN(RF)       BUMP TO NEXT IN ELEM                        
         LA    RE,NTDDLEN(RE)       BUMP TO NEXT IN SVNTELEM                    
         J     ADNTE010                                                         
*                                                                               
ADNTE020 MVC   ELEM,SVNTELEM       -YES, MOVE TO ELEM                           
         MVI   ELEM,NTDELCDQ                                                    
         BRAS  RE,ADDEL              AND ADD IT                                 
         MVC   ELEM,ADNTELEM       RESTORE ELEM                                 
         J     EXIT                                                             
ADDNTELD DSECT                                                                  
ADNTELEM DS    CL(L'ELEM)                                                       
ADDNTELL EQU   *-ADDNTELD                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
         DROP  RC                                                               
*                                                                               
*================================================                               
* READ THE C2 RECORDS TO BUILD THE AC2LKTBL                                     
*================================================                               
                                                                                
RDC2RECS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,AC2LKTBL                                                      
         XC    0(255,RE),0(RE)     CLEAR ANY GARBAGE WE MIGHT HAVE              
         MVI   0(RE),C'Y'          WE HAVE INITIALIZED THE TABLE                
*                                                                               
         MVC   FULL(3),=C'POL'     READ POL ESTIMATE AND SEE IF WE HAVE         
         BRAS  RE,GTESTREC            C2 FACTOR                                 
*                                                                               
         L     R4,AESTREC                                                       
         USING EKEY,R4                                                          
RDC2005  OC    ECOST2,ECOST2       IF COST2 IS NULLS, NO COS2 FACTOR            
         BZ    RDC2X                 AS COS2 CANNOT BE ZERO                     
         DROP  R4                                                               
*                                                                               
         XC    IOKEY,IOKEY         NOW WE LOOK FOR ALL THE COS2 RECORDS         
         LA    R4,IOKEY                                                         
         USING PWFKEY,R4           SPGENWIPW                                    
         MVI   PWKTYPE,PWKTYPEQ    X'0D7A' RECORDS                              
         MVI   PWKSTYPE,PWKSTYPQ                                                
         MVC   PWKAGMD,SVBAGYMD                                                 
         MVC   PWKCLT,SVBCLT                                                    
         MVC   PWKEST,SVBEST                                                    
         MVC   PWKMKT,SVBMKT                                                    
*                                                                               
RDC2010  GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#PWREC'                       
         CLC   IOKEY(PWKPRD-PWFKEY),IOKEYSAV  DONE WITH OUR MED/CLT?            
         BNE   RDC2X                          YES                               
         CLC   PWKEST(PWKSTA+L'PWKSTA-PWKEST),IOKEYSAV+PWKEST-PWFKEY            
         BE    RDC2020                                                          
*                                                                               
RDC2015  LA    R4,IOKEY                                                         
         MVC   IOKEY,IOKEYSAV     RESTORE OUR KEY                               
         LLC   R1,PWKPRD          BUMP TO THE NEXT PRODUCT CODE                 
         LA    R1,1(R1)                                                         
         CHI   R1,255                                                           
         BH    RDC2X                                                            
         STC   R1,PWKPRD                                                        
         B     RDC2010                                                          
                                                                                
RDC2020  GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#PWREC'                         
         L     R4,APWREC                                                        
         TM    PWGNFLG,PWGNBUYQ+PWGNBILQ   BUYS LOCKED?                         
         JZ    RDC2015                     NO, NEXT PRODUCT CODE                
         LLC   RF,PWKPRD                                                        
         L     RE,AC2LKTBL                                                      
         AR    RF,RE                                                            
         MVI   0(RF),C'Y'                  THIS PRODUCT IS LOCKED               
         B     RDC2015                     NEXT PRODUCT CODE                    
                                                                                
RDC2X    J     EXIT                                                             
                                                                                
         EJECT                                                                  
*===============================================================                
* VALIDATE REP CODE                                                             
*===============================================================                
                                                                                
VALREP   LM    R2,R4,LP_AINP                                                    
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         OC    WORK(3),SPACES                                                   
         GOTOR VRCPACK,DMCB,(C'P',WORK),(R4)                                    
         J     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE MEDIA CODE                                                           
*===============================================================                
                                                                                
VALMED   XC    SVBVALS(SVBVALSX-SVBVALS),SVBVALS                                
         GOTOR (#VALMED,AVALMED),LP_AINP                                        
         MVC   SVMED,QMEDA                                                      
         MVC   SVBAGYMD,QMEDX                                                   
         J     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE CLIENT CODE                                                          
*===============================================================                
                                                                                
VALCLT   XC    SVBCLT(SVBVALSX-SVBCLT),SVBCLT                                   
         MVC   QMEDA,SVMED                                                      
         MVC   QMEDX,SVBAGYMD                                                   
         GOTOR (#VALCLT,AVALCLT),LP_AINP                                        
         MVC   SVCLT,QCLTA                                                      
         MVC   SVBCLT,QCLTX                                                     
         J     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE ADJACENCY CODE (BDPROGT)                                             
*===============================================================                
                                                                                
VALADJ   LM    R2,R4,LP_AINP                                                    
*                                                                               
         MVC   0(1,R4),0(R2)       MOVE INPUT TO OUTPUT                         
         CHI   R3,2                IF INPUT LEN = 1                             
         JL    EXITY               EXIT                                         
*                                                                               
         PACK  DUB(2),0(3,R2)      PACK 1 EXTRA BYTE                            
         MVC   0(1,R4),DUB                                                      
         J     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* VALIDATE THE DEMO CODES                                                       
* ON ENTRY : PARM1 = A(INPUT FIELD)                                             
*            PARM2 = L'INPUT FIELD                                              
*            PARM3 = A(OUTPUT FIELD)                                            
*                                                                               
* ON EXIT  : PARM3 = CC IS EQ, OUTPUT IS SET                                    
*                    CC IS NEQ, ERROR HAS BEEN SENT                             
*===============================================================                
VALDCD   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         CLI   0(R2),C'X'          COMSCORE?                                    
         JE    VLDC020              YES                                         
         CLI   1(R2),C'X'          COMSCORE?                                    
         JE    VLDC020              YES                                         
*                                                                               
         LAY   RF,-1(R2,R3)        TREAT INPUT AS TRAD DEMO CATEGORY            
VLDC010  CLI   0(RF),C' '          GET THE LENGTH OF INPUT                      
         JH    *+8                                                              
         JCT   RF,VLDC010                                                       
         LAY   RE,-1(R2)                                                        
         SR    RF,RE                IF LESS THAN 1                              
         JNP   VALERR12              SEND ERROR                                 
         CHI   RF,4                 OR GREATER THAN 4                           
         JH    VALERR12              SEND ERROR                                 
         GOTOR (#VALDCD,AVALDCD),DMCB,(R2),(RF),(R4),0                          
         JNE   VALERR12                                                         
         J     VLDCYES                                                          
*                                                                               
VLDC020  GOTOR GTNTDEMC,DMCB,(R2),(R4)                                          
         JNE   VALERR12                                                         
VLDCYES  J     EXITY                                                            
*============================================                                   
* GET NON-TRADITIONAL INTERNAL DEMO CODE                                        
* ON ENTRY : PARM1 = A(NON-TRADITION DEMO NAME)                                 
*            PARM2 = A(OUTPUT)                                                  
*                                                                               
* ON EXIT  : PARM2 = CC IS EQ, OUTPUT IS SET                                    
*                    CC IS NEQ, ERROR HAS BEEN SENT                             
*============================================                                   
GTNTDEMC NTR1  LABEL=*                                                          
         L     R2,DMCB                                                          
         L     R4,DMCB+4                                                        
*                                                                               
         MVC   FULL(3),=C'POL'     READ THE POL ESTIMATE                        
         BRAS  RE,GTESTREC         INTO AESTREC                                 
*                                                                               
         L     R5,AESTREC          NON-TRAD DEMO NAMES                          
         USING ESTHDR,R5                                                        
GTNTD010 LA    RE,20               SPOT ONLY HAS 20 MAX ENTRIES                 
         LA    RF,ENONTDMS                                                      
GTNTD020 CLC   0(L'ENONTDMS,RF),0(R2)  FOUND MATCH?                             
         BE    GTNTD030                -YES                                     
         LA    RF,L'ENONTDMS(RF)       -NO, BUMP TO NEXT                        
         BCT   RE,GTNTD020                                                      
         J     EXITN                   IF EOL, EXIT NO                          
GTNTD030 LA    RE,ENONTDMS-L'ENONTDMS                                           
         SR    RF,RE               GET DISPLACEMENT                             
         SRL   RF,3                GET INDEX                                    
         STC   RF,1(R4)                                                         
         J     EXITY                                                            
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*===========================================                                    
* READ THE POL ESTIMATE IN AESTREC                                              
*  ON ENTRY :  FULL    SET TO BRAND OR POL                                      
*                                                                               
*  ON EXIT  :  AESTREC HAS ESTIMATE RECORD                                      
*===========================================                                    
         USING GERWORKD,RC                                                      
GTESTREC NTR1  LABEL=*,WORK=(RC,GERWORKL)                                       
         MVC   GERIOVAL,IOVALS                                                  
         LA    R2,IOKEY                                                         
         USING ESTHDR,R2           READ ESTIMATE TO ESTABLISH                   
         XC    EKEY,EKEY            NON-TRADITIONAL DEMO NAMES                  
         MVC   EKEYAM,SVBAGYMD     A/M                                          
         MVC   EKEYCLT,SVBCLT      CLT                                          
         MVC   EKEYPRD,FULL        POL PRODUCT                                  
         MVC   EKEYEST,SVBEST      EST                                          
         L     R2,AESTREC                                                       
         CLC   EKEY,IOKEY          TEST ESTMIATE RECORD AROUND                  
         JE    GTESTRECX                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#ESTREC'                         
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ESTREC'                        
         JNE   *+2                                                              
GTESTRECX MVC  IOVALS(IOVALL),GERIOVAL                                          
         J     EXITY                                                            
         DROP  R2                                                               
GERWORKD DSECT                                                                  
GERIOVAL DS    XL(IOVALL)                                                       
GERWORKL EQU   *-GERWORKD                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*===============================================================                
* CALL LINKIO TO BUILD ERROR RETURN                                             
*===============================================================                
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         MVC   WORK(2),2(R1)                                                    
*                                                                               
         L     RF,ALIOB                 SET ERROR MESSAGE FROM SYSTEM           
         MVC   LIOBMSYS-LIOBD(L'LIOBMSYS,RF),0(R1)                              
*                                                                               
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE                 
         JE    PUTERR10             YES, SEND I#SDXERP                          
         OC    LP_VRSN,LP_VRSN     NO PC VERSION?                               
         JZ    PUTERR10             YES, SEND I#SDXERP                          
         CLC   LP_VRSN,=AL1(3,0,0,88) >=03.00.00.88?                            
         JNL   PUTERR10               YES, SEND I#SDXERP                        
* SEND OLD ERROR RECORD MAP                                                     
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',116)                    
         J     PUTERR20                                                         
*                                                                               
PUTERR10 GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#SDXERP)               
*                                                                               
PUTERR20 GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_CHARQ',QTOKEN),(L'QTOKEN,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERR',40),             *        
               WORK,(L'XTRATEXT,XTRATEXT)                                       
         MVC   XTRATEXT,SPACES     RESET EXTRA MESSAGE TEXT TO SPACES           
PUTERRX  B     EXITY                                                            
         EJECT                                                                  
*===============================================================                
* CALL LINKIO TO BUILD NORMAL RETURN                                            
*===============================================================                
                                                                                
UPLRSP   NTR1                                                                   
         L     RE,0(R1)                                                         
*&&DO                                                                           
         OC    LP_VRSN,LP_VRSN     NO PC VERSION?                               
         JZ    UPRS10               YES, SEND I#SDXCNF                          
         CLC   LP_VRSN,=AL1(3,0,0,88) >=03.00.00.88?                            
         JNL   UPRS10                 YES, SEND I#SDXERP                        
* SEND OLD RESPONSE RECORD MAP                                                  
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',116)                    
         J     UPRS20                                                           
*&&                                                                             
UPRS10   GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(RE))                   
UPRS20   GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_HEXDQ',IODA),(4,0)                                          
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',2),              X        
               ('LD_CHARQ',QTOKEN),(L'QTOKEN,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',3),              X        
               ('LD_CHARQ',QACTION),(L'QACTION,0)                               
         J     EXITY                                                            
         EJECT                                                                  
*=================================================================              
* LOOK UP PRODUCT CODE IN CLIENT RECORD                                         
* NTRY:- R1=A(PRODUCT CODE)                                                     
* EXIT:- CC=LOW IF PRODUCT CODE IS NOT PRESENT                                  
*        CC=EQUAL IF PRODUCT CODE IS PRESENT AND CORRECT AND                    
*           R1=A(PRODUCT NUMBER)                                                
*        CC=HIGH IF PRODUCT CODE IS INVALID                                     
*=================================================================              
                                                                                
FINDPRD  CLI   0(R1),C'A'          TEST THERE IS A PRODUCT                      
         BLR   RE                  NO - EXIT WITH CC LOW                        
*                                                                               
         L     RF,ACLTREC          POINT TO CLIENT RECORD                       
         AHI   RF,CLIST-CLTHDR                                                  
*                                                                               
FINDPRD2 CLC   0(3,R1),0(RF)       MATCH PRODUCT CODE                           
         JNE   *+10                                                             
         LA    R1,3(RF)            FOUND - EXIT WITH CC EQUAL                   
         BR    RE                                                               
*                                                                               
         AHI   RF,4                                                             
         CLI   0(RF),C' '                                                       
         JH    FINDPRD2                                                         
         CLI   0(R1),0             NOT FOUND - EXIT WITH CC HIGH                
         BR    RE                                                               
         EJECT                                                                  
*=================================================================              
* ADD PRODUCT TO PRODUCT LIST FOR BUY                                           
* NTRY:- R1=A(PRODUCT NUMBER)                                                   
*=================================================================              
                                                                                
ADDPRD   STM   RE,R1,12(RD)                                                     
*                                                                               
         LLC   RF,0(R1)                                                         
         LA    RF,XPRDLIST(RF)                                                  
         CLI   0(RF),0                                                          
         JNE   ADDPRDX                                                          
         MVC   0(1,RF),0(R1)       PUT IT IN XPRDLIST NOW!                      
*                                                                               
         LA    RF,IOBRDLST                                                      
         LHI   R0,L'IOBRDLST-1                                                  
*                                                                               
ADDPRD02 CLI   0(RF),0                                                          
         JE    ADDPRD04                                                         
         CLC   0(1,RF),0(R1)                                                    
         JE    ADDPRDX                                                          
         AHI   RF,1                                                             
         BRCT  R0,ADDPRD02                                                      
         DC    H'0'                TOO MANY PRODUCTS                            
*                                                                               
ADDPRD04 MVC   0(1,RF),0(R1)                                                    
*                                                                               
ADDPRDX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
         EJECT                                                                  
*===================================================================            
* SET FLAGS IN XPRDLIST FOR ALL EXISTING PRODUCTS                               
*===================================================================            
                                                                                
BLDXPRD  NTR1  BASE=*,LABEL=*                                                   
         XC    XPRDLIST,XPRDLIST                                                
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,RCPOLOQ      X'0B'-POOL ORIGINAL                          
         MVI   ELCDHI,RCPOTOQ      X'0C' - POOL OTO                             
*                                                                               
BLDX2    BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         CLI   1(R3),10                                                         
         BNH   BLDX2                                                            
         LLC   RF,10(R3)           GET FIRST ALLOCATION                         
         LA    RF,XPRDLIST(RF)                                                  
         MVC   0(1,RF),10(R3)                                                   
*                                                                               
         CLI   1(R3),14                                                         
         BNH   BLDX2                                                            
*                                                                               
         LLC   RF,14(R3)           GET SECOND ALLOCATION                        
         LA    RF,XPRDLIST(RF)                                                  
         MVC   0(1,RF),10(R3)                                                   
         B     BLDX2                                                            
         EJECT                                                                  
*===================================================================            
* COMPUTE CHKSUM OF PAID SPOTS AND RETURN IN FULL                               
* AND INSURE THAT BDCOST, BDNTAX, AND BDCIND DON'T CHANGE!                      
*===================================================================            
                                                                                
*                                                                               
PDCHKSUM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FULL,FULL                                                        
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,RCPOLOQ      X'0B'-POOL ORIGINAL                          
         MVI   ELCDHI,RCPOTOQ      X'0C' - POOL OTO                             
         MVI   BYTE,0                                                           
*                                                                               
PDCHK2   BRAS  RE,NEXTEL                                                        
         BNE   PDCHK10                                                          
*                                                                               
         USING REGELEM,R3                                                       
         OC    RPAY,RPAY           TEST SPOT PAID                               
         BZ    PDCHK2                                                           
         MVI   BYTE,C'P'           SET FOUND PAID SPOT                          
* GET CHKSUM FOR ELEMENT BEING DELETED                                          
         XC    WORK(18),WORK       MOVE SPOT                                    
         LLC   RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R3)       MOVE SPOT                                    
         AHI   RF,1                RESTORE ELEMENT LENGTH                       
         LA    RE,WORK             POINT FOR CKSUM                              
*                                                                               
W        USING REGELEM,WORK                                                     
         NI    W.RSTATUS,RSRATOVQ  CLEAR STATUS EXCEPT X'20' COST OVRD          
         MVI   W.RPSTAT2,0         CLEAR MAKEGOOD SEQNUM                        
*                                                                               
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
         B     PDCHK2                                                           
         DROP  W,R3                                                             
                                                                                
* IF ANY PAID SPOTS, INCLUDE BDCOST, BDNTAX AND BDCIND IN THE CKSM              
                                                                                
PDCHK10  CLI   BYTE,C'P'           PAID SPOT FOUND?                             
         JNE   PDCHKX               NO                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'BDCOST),BDCOST                                            
         MVC   WORK+3(L'BDCIND),BDCIND                                          
         MVC   WORK+4(L'BDCIND2),BDCIND2                                        
         MVC   WORK+5(L'BDNTAX),BDNTAX                                          
         MVC   WORK+7(L'BDREP),BDREP                                            
*                                                                               
         LA    RE,WORK                                                          
         LHI   RF,L'BDCOST+L'BDCIND+L'BDCIND2+L'BDNTAX+L'BDREP                  
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0B0'                                                 
         MVC   WORK+4(2),LP_AGY                                                 
         MVC   WORK+6(1),QMEDA                                                  
         MVC   WORK+7(3),QCLTA                                                  
         MVI   WORK+10,C'*'                                                     
         L     RF,ACLTREC                                                       
         MVC   WORK+11(L'COFFICE),COFFICE-CLTHDR(RF)                            
*                                  RETURN DEFAULT IF AGY LVL NOT FOUND          
         GOTO1 VGETPROF,DMCB,(X'40',WORK),WORK2,VDATAMGR                        
*                                                                               
         CLI   WORK2+6,C'N'        CHANGE COS2 AFTER SPOT PAID ?                
         JNE   PDCHK30              YES                                         
*                                                                               
         MVI   ELCDLO,X'71'        LOOK FOR COST2 DOLLAR OR                     
         MVI   ELCDHI,X'73'        COST2 FACTOR                                 
         LA    R3,BDELEM                                                        
PDCHK20  BRAS  RE,NEXTEL                                                        
         BNE   PDCHK30                                                          
         CLI   0(R3),X'72'         HAVE INFOMERCIAL?                            
         BE    PDCHK20              SKIP IT                                     
*                                                                               
         LR    RE,R3                                                            
         LLC   RF,1(R3)                                                         
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
*                                                                               
PDCHK30  CLI   WORK2+3,C'N'        ALLOW ID CHANGE AFTER PAID/MATCH?            
         JNE   PDCHK40              YES                                         
*                                                                               
         MVI   ELCDLO,IDELCODQ     LOOK FOR X'70' ID ELEMENT                    
         MVI   ELCDHI,IDELCODQ                                                  
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PDCHK40                                                          
*&&DO                                                                           
         CLC   LP_VRSN,=AL1(4,6,0,102)  VERSION# >= V4.6.0.102?                 
         BL    PDCHK32                  NO                                      
         CLC   LP_VRSN,=AL1(4,6,0,233)  VERSION# <= V4.6.0.233?                 
         BH    PDCHK32                  NO                                      
         OC    IDCONNO-IDELEM(L'IDCONNO,R3),SPACES                              
*&&                                                                             
PDCHK32  LR    RE,R3                                                            
         LLC   RF,1(R3)                                                         
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
*                                                                               
PDCHK40  CLI   WORK2+4,C'N'        ALLOW AFFID DEL IF SPOT PAID ?               
         JNE   PDCHKX               YES                                         
*                                                                               
         MVI   ELCDLO,ACCODEQ      LOOK FOR X'10' AFFID ELEMENTS                
         MVI   ELCDHI,ACCODEQ                                                   
         LA    R3,BDELEM                                                        
PDCHK45  BRAS  RE,NEXTEL                                                        
         BNE   PDCHKX                                                           
*                                                                               
         LR    RE,R3                                                            
         LLC   RF,1(R3)                                                         
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
         B     PDCHK45                                                          
*                                                                               
PDCHKX   J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*=================================================================              
* FIND NEXT AVAILABLE MAKEGOOD CODE                                             
* ON ENTRY R4 POINTS TO NEW TABLE ENTRY                                         
*                                                                               
* CLOBBERS ELEM2                                                                
*                                                                               
*=================================================================              
                                                                                
MAXCODES EQU   240  <==== CHANGE FOR NEW MAKEGOODS                              
                                                                                
         USING MGTOKEND,R4                                                      
GETMGCD  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R1                                                            
         TM    SVMGCDTF,X'80'      ALREADY HAVE TABLE?                          
         JNZ   GETMG10                                                          
***                                                                             
         LA    R3,ELEM2                                                         
         USING MGABLKD,R3                                                       
         XC    0(MGALNQ,R3),0(R3)                                               
         MVI   MGAACT,MGAQBLD      SET ACTION - BUILD TABLE                     
         MVC   MGAACOM,ACOMFACS    SET A(COMFAS)                                
         MVC   MGATSAR,VTSAR                                                    
         MVC   MGAIO,AIO6          USE AIO6 AS WORK AREA                        
         MVC   MGUNTIME,VUNTIME                                                 
         MVC   MGSTAPAK,VSTAPACK                                                
         MVC   MGGETBUY,VGETBUY    SET A(GETBUY)                                
         MVI   MG1OR2,1            1 BYTE FOR USE BUYS                          
*                                                                               
         OI    MGAOPT2,MGAOPT2_NODEMS  SUPPRESS DEMOS                           
         MVC   MGAAGMD,SVBAGYMD    SET AGY/MED                                  
         MVC   MGACLT,SVBCLT           CLIENT                                   
         MVI   MGAPRD,X'FF'            POL PRODUCT                              
         MVC   MGASTA(2),SVBMKT        MKT                                      
         MVC   MGASTA+2(3),SVBSTA      STA                                      
         MVC   MGAEST,SVBEST           ESTIMATE                                 
*                                                                               
         OI    MGAOPT2,MGAOPT2_NODSK   SET DO NOT WRITE TO DISK                 
         MVI   MGATSRPGS,40       REQUEST 40 PAGES (18432 BYTES/PAGE)           
         OI    MGATSRPGS,X'80'    SET FLAG FOR TSAR TO USE BOTH BUFFERS         
*                                                                               
         GOTO1 VBLDMGN,MGABLKD     BUILD MAKEGOOD TABLE                         
         CLI   MGAERR,0            ANY ERRORS?                                  
         JNE   *+2                  YES, DIE                                    
*                                                                               
GETMG10  MVI   MGAACT,MGAQCOD                                                   
         LA    RE,SVMGCDTB                                                      
         ST    RE,MGCODTAB         PASS A(MG COD TABLE)                         
         OC    MGCODTBL,SVMGCDTF   SET MG COD TABLE FLAG                        
         GOTO1 VBLDMGN,MGABLKD     GET NEXT AVAIL MG CODE                       
         CLI   MGAERR,0            ANY ERRORS?                                  
         JNE   *+2                  YES, DIE                                    
MGA      USING MGENTRYD,MGAENTRY                                                
         MVC   MGCDALPH,MGA.MGECODE  SET MG CODE                                
         MVC   MGCDBIN,MGA.MGAECOD   SET MG BIN CODE                            
         OI    SVMGCDTF,X'80'        DON'T REBUILD TABLE AGAIN                  
         DROP  MGA,R4,R3                                                        
*&&DO                                                                           
* REREAD BUY                                                                    
         CLI   SVACTION,QACTADD    TEST ADD                                     
         BE    GETMGX                                                           
*                                                                               
         MVC   IOKEY(L'BUYKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+IO6'                         
*                                                                               
         CLC   IOKEY(12),IOKEYSAV  A-M/CLT/PRD/MKT/STA/EST/LIN                  
         JNE   *+2                                                              
* NOTE - DO NOT READ OVER NEW BUY RECORD IN IOBUY !!!                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO6'                        
*&&                                                                             
GETMGX   J     EXIT                                                             
                                                                                
*================================================================               
* CONVERT ALPHA CODE IN HALF TO 1 BYTE BINARY IN BYTE                           
*  ON ENTRY : R1   A(MAKEGOOD CODE)                                             
*                                                                               
*  ON EXIT  : HALF  XL2'BINARY MAKEGOOD CODE'                                   
*                                                                               
* CLOBBERS ELEM2                                                                
*                                                                               
*================================================================               
                                                                                
GETMGBIN NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TEMP1                                                         
         USING MGABLKD,R3                                                       
                                                                                
         XC    0(MGALNQ,R3),0(R3)                                               
                                                                                
         MVI   MGAACT,MGAQBIN      GET BINARY CODE FOR ALPHA IN MGABUY          
         MVC   MGQCODE,0(R1)                                                    
                                                                                
         LHI   R0,QBLDMGN                                                       
         ICM   R0,B'1110',T00A                                                  
         GOTOR VCALLOV,DMCB,0,(R0),0                                            
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
                                                                                
         L     RF,0(R1)                                                         
         GOTO1 (RF),MGABLKD                                                     
         CLI   MGAERR,0                                                         
         JNE   *+2                                                              
                                                                                
         MVC   HALF,MGBCODE                                                     
         DROP  R3                                                               
         J     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* GET THE DATE AND TIME FOR ACTIVITY                                            
*  OUPUT : WORK(2)   - BINARY DATE                                              
*          WORK+2(3) - TIME                                                     
*===============================================================                
                                                                                
GETDTTM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,WORK)                                      
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    GDT10                                                            
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTO1 VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTO1 VDATCON,DMCB,(0,DUB),(2,WORK)                                    
*                                                                               
GDT10    ICM   R1,15,PACKOF4B                                                   
         SRL   R1,4                GET RID OF SECONDS AND SIGN                  
         STCM  R1,7,WORK+2                                                      
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
*================================================================               
* CALL BUYVAL TO VALIDATE BUY RECORD                                            
*================================================================               
                                                                                
BUYVAL   NTR1  BASE=*,LABEL=*                                                   
         GOTOR VCALLOV,DMCB,0,X'D9000A2A'  GET ADDRESS OF BUYVAL                
         L     RF,0(R1)                                                         
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING SPBUYVLD,R3                                                      
         MVC   SPBYAREC,ABUYREC                                                 
         MVC   SPBYAFAC,ACOMFACS                                                
*                                                                               
         L     RE,ACLTREC          POINT TO CLIENT RECORD                       
         LLC   R0,CPROF-CLTHDR(RE)                                              
*                                                                               
         GOTO1 (RF),(R1),((R0),SPBYLEN)                                         
         CLI   SPBYERR,0                                                        
         JE    EXIT                                                             
*                                                                               
         CLC   SVTXPNUM,=XL2'0051' X'0051' - PRISMA/CONVERGENCE?                
         JNE   *+2                                                              
         L     R1,ATWA                                                          
         USING TWAD,R1                                                          
         LAY   RF,LNKHEADH                                                      
         OI    6(RF),X'80'            TRANSMIT FIELD                            
*                              >  |  < THESE BOTH ARE THE L(MESSAGE)            
         MVC   17(13,RF),=C'2 6 55 55 1 1'                                      
         LAY   RF,LNKINPH                                                       
         OI    6(RF),X'80'            TRANSMIT FIELD                            
         MVC   8(10,RF),=C'D D D=EEEE'  EEEE - ABEND RECORD MAP                 
*                                                                               
         MVC   18(3,RF),=C'TAC'       MAP 1 - PC TOKEN                          
         MVC   21(20,RF),QTOKEN                                                 
*                                                                               
         MVC   41(3,RF),=C'FBC'       MAP 2 - $ABEND MESSAGE                    
         MVC   44(6,RF),=C'$ABEND'                                              
*                                                                               
         MVC   50(3,RF),=C'HCC'       MAP 3 - MF MODULE                         
         MVC   53(8,RF),=C'SPBUYVAL'                                            
*                                                                               
         MVC   61(3,RF),=C'CDC'       MAP 4 - ERROR #                           
         LLC   R4,SPBYERR                                                       
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  64(3,RF),DUB                                                     
*                                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
         DROP  R3,R1                                                            
*                                                                               
*=====================================================*                         
* ADD DARE BATCH POINTERS IF THE AGENCY DO IT         *                         
*  NOTE: CLOBBERS ELEM2                                                         
*=====================================================*                         
         USING DARBATCD,RC                                                      
DARBATCH NTR1  BASE=*,LABEL=*,WORK=(RC,DARBATCL)                                
         XC    0(DARBATCL,RC),0(RC)   INIT LOCAL STORAGE                        
*                                                                               
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
                                                                                
         XC    WORK,WORK                                                        
         MVI   WORK,C'S'-X'40'     LOWERCASE S                                  
         MVC   WORK+1(3),=C'DAR'                                                
         MVC   WORK+4(2),LP_AGY                                                 
         MVC   WORK+6(1),QMEDA                                                  
         MVC   WORK+7(3),QCLTA                                                  
         MVI   WORK+10,C'*'                                                     
         L     RF,ACLTREC                                                       
         MVC   WORK+11(L'COFFICE),COFFICE-CLTHDR(RF)                            
         GOTO1 VGETPROF,DMCB,WORK,WORK2,VDATAMGR                                
                                                                                
         CLI   WORK2+4,C'Y'        USING DARE BATCH ORDERING?                   
         BNE   DBTCHX              NO, NOTHING TO DO HERE                       
*                                                                               
         CLI   QMEDA,C'T'          BATCH RECORDS FOR ONLY TV & RADIO            
         BE    *+12                                                             
         CLI   QMEDA,C'R'                                                       
         BNE   DBTCHX                                                           
*                                                                               
         MVI   WORK+3,C'C'         CHECK THIS FLAG LATER                        
         OC    BDREP,BDREP                                                      
         BZ    DBTCH05                                                          
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 VRCPACK,DMCB,(C'U',BDREP),WORK                                   
*                                                                               
         CLI   WORK2+14,C'Y'       MULTIPLE TRADE CODES?                        
         BE    *+14                                                             
         CLC   WORK(3),WORK2+6     DO THEY MATCH FOR 3 DIGITS?                  
         B     *+10                                                             
         CLC   WORK(2),WORK2+6     DO THEY MATCH FOR 2 DIGITS?                  
         BNE   *+8                                                              
         MVI   WORK+3,C'T'         CHECK THIS FLAG LATER                        
*                                                                               
DBTCH05  BAS   RE,FLTBL            BUILD FLIGHT TABLE                           
*                                                                               
         CLI   SVDRFLAG,C'Y'       FLIGHT REQUIRED?                             
         BNE   DBTCH10              NO                                          
*                                                                               
         BRAS  RE,CHKFLTS          HAVE FLIGHT?                                 
         BNE   DBTCHX              YES, BUT SPOTS OUTSIDE OF FLT DATES          
*                                                                               
DBTCH10  XC    IOKEY,IOKEY         SEE IF WE HAVE A DARE ORDER ALREADY          
         LA    R3,IOKEY                                                         
         USING DOKEY,R3                                                         
*                                                                               
         MVI   DCKTYPE,DCKTYPQ     READ DARE CLT PASSIVE KEY                    
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,BUYKAM                                                   
         MVC   DCKCLT,BUYKCLT                                                   
         MVC   DCKPRD,BDMASPRD                                                  
         MVC   DCKEST,BUYKEST                                                   
         MVC   DCKSTA,BUYKSTA                                                   
         CLI   DCKSTA,X'E8'        IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DCKSTA+2,X'80'      YES, WE ONLY WANT SYSCODE LEVEL              
         MVC   DCKPRD2,BDMASPRD+1                                               
         CLI   DARBFLT,DARBFLTZ    FLIGHT 0?                                    
         BE    *+10                 YES, LEAVE AS NULL                          
         MVC   DCKFLTNM,DARBFLT                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#ORDREC'                      
         JNE   *+2                                                              
DBTCH20  CLC   IOKEY(DCKFLAG-DOKEY),IOKEYSAV   M/C/P1/E/S/P2/FL                 
         BE    DBTCH21                                                          
*                                                                               
* IF WE GET HERE, WE NEED TO ADD BATCH                                          
*                                                                               
         CLI   DARBFLT,0           HAVE FLIGHTS?                                
         BE    DBTCH35              NO, DON'T CHECK OTHER FLIGHTS               
         B     DBTCH32              YES, NEED TO CHECK OTHER FLIGHTS            
*                                                                               
DBTCH21  CLI   WORK+3,C'T'         TRADE BUYLINE?                               
         BE    DBTCH25                                                          
         TM    DCKFLAG,DCKFTRDE    BUYLINE IS CASH, HAVE CASH ORDER?            
         BZ    DBTCH31              YES, DON'T ADD BATCH                        
         B     DBTCH30                                                          
DBTCH25  TM    DCKFLAG,DCKFTRDE    BUYLINE IS TRADE, HAVE TRADE ORDER?          
         BO    DBTCH31               YES, DON'T ADD BATCH                       
DBTCH30  GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOSPTDIR+B#ORDREC'                      
         B     DBTCH20                                                          
*                                                                               
* IF WE GET HERE, WE DO NOT NEED TO ADD BATCH                                   
*                                                                               
DBTCH31  CLI   DARBFLT,0           HAVE FLIGHTS?                                
         BE    DBTCHX              NO, WE'RE FINISHED                           
*                                                                               
* IF WE GET HERE, WE HAVE TO CHECK OTHER FLIGHTS                                
*                                                                               
DBTCH32  CLI   DARBFLT,DARBFLTZ    FLIGHT 0?                                    
         BNE   DBTCH32D             YES                                         
*                                                                               
         LA    RF,DARBFLTL+L'DARBFLTL  POINT RF TO END OF DARBFLTL              
         LA    RE,DARBFLTL         FORCE NEXT FLIGHT TO TO FLT1                 
         CLC   IOKEY(12),IOKEYSAV  DID WE FIND THE FLT ORDER?                   
         BNE   DBTCH34A             NO, HAVE TO ADD BATCH FOR THIS FLT          
         MVI   DARBFLT0,0           YES, CLEAR FLT0 SO WE DON'T ADD             
         B     DBTCH34A                                                         
*                                                                               
DBTCH32D LLC   RE,DARBFLT                                                       
         LA    RE,DARBFLTL-1(RE)   POINT TO CURRENT FLIGHT ENTRY                
*                                                                               
         CLC   IOKEY(12),IOKEYSAV  DID WE FIND THE FLT ORDER?                   
         BNE   DBTCH33              NO, HAVE TO ADD BATCH FOR THIS FLT          
         MVI   0(RE),0              YES, CLEAR THE FLIGHT ENTRY                 
                                                                                
DBTCH33  LA    RF,DARBFLTL+L'DARBFLTL  POINT RF TO END OF DARBFLTL              
DBTCH34  LA    RE,1(RE)            BUMP RE TO GET NEXT FLT IN DARBFLTL          
         CR    RE,RF               END OF FLIGHT TABLE?                         
         BNL   DBTCH34D                                                         
DBTCH34A CLI   0(RE),0             HAVE FLIGHT?                                 
         BZ    DBTCH34              NO, CHECK NEXT                              
         MVC   DARBFLT,0(RE)                                                    
         B     DBTCH10                                                          
*                                                                               
DBTCH34D OC    DARBFLTA,DARBFLTA   ANY BATCH-FLTS FL0-FLT16 TO ADD?             
         BZ    DBTCHX               NO, RESTORE READ-SEQ AND EXIT               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DARBFLT0       HAVE FLIGHT0?                                
         BNZ   DBTCH34J             YES                                         
*                                                                               
         LA    RF,DARBFLTL         POINT TO START OF FLT1-FLT16 TABLE           
DBTCH34G ICM   RE,1,0(RF)          ENTRY HAS FLIGHT?                            
         BNZ   DBTCH34J                                                         
         AHI   RF,1                BUMP                                         
         B     DBTCH34G                                                         
DBTCH34J STC   RE,DARBFLT          SET THE FIRST FLIGHT TO ADD                  
*                                                                               
DBTCH35  XC    IOKEY,IOKEY         SEE IF WE HAVE A BATCH ORDER ALREADY         
         LA    R3,IOKEY                                                         
         USING DBTKEY,R3                                                        
*                                                                               
         MVI   DBTKTYP,DBTKTYPQ                                                 
         MVI   DBTKSTYP,DBTKSTYQ                                                
         MVC   DBTKAGMD,BUYKAM                                                  
         MVC   DBTKMKT(L'BUYMSTA),BUYMSTA                                       
*                                                                               
         CLI   DBTKSTA,X'E8'       IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DBTKSTA+2,X'80'     YES, WE ONLY WANT SYSCODE LEVEL              
*                                                                               
         MVC   DBTKCLT,BUYKCLT                                                  
         MVC   DBTKEST,BUYKEST                                                  
*                                                                               
         MVC   DBTKPRD,BDMASPRD    YES, PRODUCT CODES STORED HERE               
         MVC   DBTKPRD2,BDMASPRD+1                                              
         B     DBTCH40                                                          
*                                                                               
DBTCH40  CLI   DBTKPRD,X'00'       IF NO PRODUCT ALLOCATED                      
         BNE   *+8                                                              
         MVI   DBTKPRD,X'FF'       THEN POL                                     
         DROP  R3                                                               
*                                                                               
         XC    ELEM,ELEM           SETUP AN INFO ELEMENT SO WE KNOW             
         NI    DARFLG1,X'FF'-DARF1RNF                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOBHIUPD+IOSPTDIR+B#BTCREC'                   
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   DBTCH50                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#BTCREC'                   
         L     R3,ABTCREC                                                       
         CLI   24(R3),DBINFELQ     X'01' - INFO ELEMENT?                        
         BNE   DBTCH51                                                          
         MVC   ELEM(DBINFLNQ),24(R3)         SAVE THE OLD ELEM                  
         GOTOR RECUP,DMCB,(R3),24(R3),24(R3)  DELETE INFO ELEM AND              
         B     DBTCH51                                                          
*                                                                               
DBTCH50  OI    DARFLG1,DARF1RNF    DIDN'T FIND IT!!                             
         L     R3,ABTCREC                                                       
         XC    0(256,R3),0(R3)                                                  
         MVC   IOKEY,IOKEYSAV                                                   
         MVC   0(13,R3),IOKEYSAV                                                
         MVC   13(2,R3),=AL2(DBTRFRST-DBTKEY)                                   
*                                                                               
DBTCH51  CLI   DARBFLT,0           HAVE FLIGHTS?                                
         BE    DBTCH60             NO                                           
         L     R3,ABTCREC                                                       
         LA    R3,24(R3)                                                        
DBTCH52  CLI   0(R3),0             END-OF-REC?                                  
         BE    DBTCH56             GO ADD A FLIGHT ELEM                         
         CLI   0(R3),DBFLTELQ      X'20'-FLIGHT ELEM?                           
         BE    DBTCH54                                                          
DBTCH53  LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DBTCH52                                                          
         USING DBFLTELD,R3                                                      
DBTCH54  SR    RF,RF                                                            
         ICM   RF,1,DBFLTFLT       FLIGHT0?                                     
         BNZ   *+8                                                              
         LHI   RF,C'0'             YES, MAKE IT A C'0'                          
         STC   RF,BYTE                                                          
         CLC   BYTE,DARBFLT                                                     
         BL    DBTCH53             NOT THERE YET, LOOK FOR NXT FLT ELEM         
         BH    DBTCH56             WE PASSED IT, LETS GO ADD IT                 
         CLI   WORK+3,C'T'                                                      
         BE    DBTCH55                                                          
         TM    DBFLTFL1,DBFLTTRD   HAVE CASH(NOT TRADE) FLIGHT ALREADY?         
         BNZ   DBTCH56              NO, THEN GO ADD IT                          
         B     DBTCH57              YES, SKIP IT                                
*                                                                               
DBTCH55  TM    DBFLTFL1,DBFLTTRD   HAVE TRADE FLIGHT ALREADY?                   
         BZ    DBTCH53              NO, GO CHK THE NEXT ONE                     
         B     DBTCH57              YES, SKIP IT                                
         DROP  R3                                                               
*                                                                               
DBTCH56  OI    DARFLG1,DARF1UPF+DARF1UPD  UPD FLT ELEM & WRITE REC              
         LA    R2,SVDBFLTEL        LETS ADD OUR FLT ELEM                        
         USING DBFLTELD,R2                                                      
         XC    SVDBFLTEL,SVDBFLTEL INIT SV-ELEM                                 
         MVI   DBFLTEL,DBFLTELQ    X'20'-FLIGHT ELEM                            
         MVI   DBFLTLEN,DBFLTOVH                                                
         CLI   DARBFLT,DARBFLTZ    ZERO FLIGHT?                                 
         BE    *+10                 YES, STORE AS BINARY 0 IN RECORD            
         MVC   DBFLTFLT,DARBFLT                                                 
         CLI   WORK+3,C'T'         ARE WE UPDATE TRADE?                         
         BNE   *+8                                                              
         OI    DBFLTFL1,DBFLTTRD   SET TRADE BIT                                
         GOTOR RECUP,DMCB,ABTCREC,SVDBFLTEL,(R3)                                
         CLI   DARBFLT,DARBFLTZ    JUST ADD FLIGHT0?                            
         BNE   DBTCH57                                                          
         LLC   R0,1(R3)            YES, KEEP FLT-0 ELEM AT START OF REC         
         AR    R3,R0                                                            
*                                                                               
DBTCH57  LA    RF,DARBFLTL+L'DARBFLTL   RF=A(END OF DARBFLTL)                   
*                                                                               
         LA    RE,DARBFLTL         IN CASE WE JUST PROCESSED FLT0               
*                                   POINT RE=A(START OF DARBFLTL)               
         CLI   DARBFLT,DARBFLTZ    DID WE JUST PROCESS FLIGHT0?                 
         BE    DBTCH59              YES                                         
*                                                                               
         LLC   RE,DARBFLT          PROCESS THE NEXT FLT                         
         LA    RE,DARBFLTL-1(RE)   POINT TO CURRENT FLT ENTRY                   
         SR    R1,R1                                                            
DBTCH58  LA    RE,1(RE)            BUMP TO NEXT FLT ENTRY                       
DBTCH59  CR    RE,RF               END OF FLIGHTS?                              
         BNL   DBTCH60              YES                                         
         ICM   R1,1,0(RE)          ENTRY HAS FLIGHT?                            
         BZ    DBTCH58              NO                                          
         STC   R1,DARBFLT          SET THE FLT TO PROCESS                       
         B     DBTCH52                                                          
*                                                                               
* NOTE : ELEM WILL HAVE COPY OF DBINFEL IF IT WAS PRESENT IN ORIG-REC           
*                                                                               
DBTCH60  L     R3,ABTCREC                                                       
         LA    RE,ELEM             WHEN WE CREATED THIS BATCH REC               
         USING DBINFELD,RE                                                      
         MVI   DBINFEL,DBINFELQ                                                 
         MVI   DBINFLEN,DBINFLNQ                                                
         LA    R4,DBINFDTC         POINT TO CASH (PWOS JULIAN)                  
         LA    R2,DBINFTMC         POINT TO CASH (TIME TU)                      
         CLI   WORK+3,C'T'         ARE WE UPDATE TRADE?                         
         BNE   DBTCH70                                                          
         LA    R4,DBINFDTT         POINT TO TRADE (PWOS JULIAN)                 
         LA    R2,DBINFTMT         POINT TO TRADE (TIME TU)                     
         DROP  RE                                                               
*                                                                               
DBTCH70  OC    0(3,R4),0(R4)       DO WE ALREADY HAVE A DATE?                   
         BZ    DBTCH75              NO                                          
         TM    DARFLG1,DARF1UPF    DID I UPDATE FLIGHT?                         
         BO    DBTCH79              YES, MUST PUT INFO ELEM BACK                
         B     DBTCHX               NOTHING TO DO, EXIT                         
*                                                                               
DBTCH75  OI    DARFLG1,DARF1UPP    UPDATE PASSIVE                               
         ST    R4,DMCB+4                                                        
         MVI   DMCB+4,19                                                        
*                                                                               
         TIME  TU                                                               
         STCM  R0,15,0(R2)         R2=A(TIME ADDED) SAVE FOR LATER              
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),,0  FOR TODAY                                 
DBTCH79  GOTOR RECUP,DMCB,ABTCREC,ELEM,24(R3)                                   
         OI    DARFLG1,DARF1UPD    WRITE RECORD BACK                            
*                                                                               
         TM    DARFLG1,DARF1UPD    WRITE RECORD BACK?                           
         BZ    DBTCH85                                                          
         TM    DARFLG1,DARF1RNF    DID WE FIND A BATCH REC?                     
         BZ    DBTCH80              YES                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBADD+IOSPTFIL+B#BTCREC'                     
         B     DBTCH85                                                          
*                                                                               
DBTCH80  GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#BTCREC'                     
*                                                                               
DBTCH85  TM    DARFLG1,DARF1UPP    UPDATE PASSIVE                               
         BZ    DBTCHX               NO                                          
         MVC   FULL,IODA                                                        
         LA    R2,ELEM                                                          
         USING DBINFELD,R2                                                      
         LA    R3,IOKEY            NO, LETS DELETE OLD PASSIVE                  
         USING DBTKEY,R3                                                        
         MVI   DDTKTYP,DDTKTYPQ                                                 
         MVI   DDTKSTYP,DDTKSTYQ                                                
         MVC   DDTKCLT,DBTKCLT                                                  
         TM    DARFLG1,DARF1RNF    WAS THIS AN ADD?                             
         BO    DBTCH100            YES, THEN NO CLEANUP, GO ADD PASSIVE         
         LA    R4,DBINFDTC                                                      
         MVC   DDTKTMTU,DBINFTMC                                                
         CLI   WORK+3,C'T'         TRADE BUYLINE ADDED?                         
         BE    DBTCH90             YES, THEN DELETE CASH PASSIVE                
         LA    R4,DBINFDTT         NO, THEN DELETE TRADE PASSIVE                
         MVC   DDTKTMTU,DBINFTMT                                                
*                                                                               
DBTCH90  ST    R4,DMCB                                                          
         MVI   DMCB,8                                                           
         GOTO1 VDATCON,DMCB,,(2,DDTKDATE),0                                     
         XC    DDTKDATE,=X'FFFF'                                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUPD+IOSPTDIR+B#BTCREC'                   
         BE    DBTCH95                                                          
         MVC   IOKEY,IOKEYSAV                                                   
         B     DBTCH100                                                         
*                                                                               
DBTCH95  OI    IOKEY+13,X'80'     DELETE IT                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR'                               
         NI    IOKEY+13,X'FF'-X'80'                                             
*                                                                               
DBTCH100 GOTO1 VDATCON,DMCB,(5,0),(2,DDTKDATE),0  TODAY                         
         XC    DDTKDATE,=X'FFFF'                                                
         MVC   DDTKTMTU,DBINFTMC                                                
         CLI   WORK+3,C'T'         TRADE BUYLINE ADDED?                         
         BNE   *+10                                                             
         MVC   DDTKTMTU,DBINFTMT                                                
         MVC   IOKEY+14(4),FULL                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTDIR'                               
*                                                                               
DBTCHX   J     EXIT                                                             
         DROP  R2,R3                                                            
*================================================================               
* IF FLIGHT IS REQUIRED, THEN CHECK EACH BUY ELEMENT AGAINST THE                
*  FLIGHT TABLE IN SVDRSTR1, AND BUILD LIST OF EFFECTIVE                        
*  FLIGHTS IN DARBFLTL. ALSO SET THE FIRST FLIGHT IN DARBFLT                    
*================================================================               
CHKFLTS  NTR1                                                                   
         L     R2,ABUYREC                                                       
         USING BUYRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,RCPOLOQ      MUST USE BUY ELEMENTS X'0B'                  
         MVI   ELCDHI,RCPOTOQ           X'0C'                                   
         LA    R3,BDELEM                                                        
*                                                                               
CHKFLT10 BRAS  RE,NEXTEL                                                        
         BNE   CHKFLTX                                                          
*                                                                               
         BAS   RE,FLNUM            FIND FLIGHT SEQ NUM                          
         B     CHKFLT10                                                         
*                                                                               
CHKFLTX  CLI   DARBFLT,0           HAVE ATLEAST 1 FLIGHT SET?                   
         JE    EXITN                NO, EXIT CC=NO                              
CHKFLTYS J     EXITY                                                            
         DROP  R2                                                               
*================================================================               
*    READ THE FLIGHT RECORDS AND BUILD FLIGHT TABLE                             
*================================================================               
                                                                                
FLTBL    NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING DFLRECD,R2          DARE FLIGHT RECORD                           
*                                                                               
         MVI   DFLKTYP,DFLKTYPQ    X'0D'                                        
         MVI   DFLKSUB,DFLKSUBQ    X'38' - DARE FLIGHT RECORD                   
         MVC   DFLKAGMD,SVBAGYMD   AG/MED                                       
         MVC   DFLKCLT,SVBCLT      CLIENT                                       
         MVC   DFLKPRD,=C'POL'     POL PRODUCT                                  
         MVC   DFLKEST,SVBEST      EST                                          
         DROP  R2                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOSPTDIR+B#FLTREC'                      
         JNE   *+2                                                              
         CLC   IOKEY(13),IOKEYSAV  FLIGHT RECORD?                               
         BNE   FTBL080             NO                                           
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOSPTFIL+B#FLTREC'                     
         JNE   *+2                                                              
*                                                                               
         LA    R5,SVDRSTR1        START OF THE TABLE                            
         LA    R7,SVDRDATX         END OF TABLE                                 
         LR    R2,R5                                                            
*                                                                               
         L     R3,AFLTREC                                                       
         LA    R3,24(R3)                                                        
*                                                                               
FTBL020  CLI   0(R3),0             FIRST ELEMENT                                
         BE    FTBL080                                                          
         CLI   0(R3),DFINFELQ      X'01'- FLIGHT0                               
         BE    FTBL050                                                          
         CLI   0(R3),DFFLTELQ      X'05'                                        
         BE    FTBL060                                                          
*                                                                               
FTBL040  SR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R3)          TEST VALID ELEM LENGTH                       
         JZ    *+2                                                              
         AR    R3,R0                                                            
         B     FTBL020                                                          
*                                                                               
         USING DFINFEL,R3          FLIGHT 0 ELEMENT                             
FTBL050  OC    SVDRFLT0,SVDRFLT0   DO WE ALREADY HAVE FLT0?                     
         JNZ   *+2                  YES, THAT'S WRONG, DIE                      
         GOTO1 VDATCON,DMCB,(3,DFINFSDT),(2,SVDRFLT0)                           
         B     FTBL040                                                          
         DROP  R3                                                               
*                                                                               
         USING DFFLTEL,R3          FLIGHT ELEMENT                               
FTBL060  CR    R5,R7               MAXIMUM REACHED?                             
         BNL   FTBL080                                                          
                                                                                
*                                                                               
* HOLES IN THE TABLE ARE IDENTIFIED BY X'FF' IN THE FIRST BYTE                  
* END OF THE TABLE IS IDENTIFIED BY NULLS IN THE FIRST TWO BYTES                
* FLIGHT1 GOES TO FIRST SLOT IN TABLE, FOURTH TO FOURTH, ETC                    
*                                                                               
                                                                                
         LR    R5,R2               START OF TABLE                               
         SR    R4,R4                                                            
         ICM   R4,1,DFFLTNUM                                                    
         JZ    *+2                                                              
         BCTR  R4,0                                                             
         SLL   R4,2                MULTIPLY IT BY 4                             
         AR    R5,R4                                                            
         GOTO1 VDATCON,DMCB,(3,DFFLTSTR),(2,0(R5))   START DATE                 
         GOTO1 VDATCON,DMCB,(3,DFFLTEND),(2,2(R5))   END DATE                   
         B     FTBL040                                                          
*                                                                               
FTBL080  LA    R4,SVDRSTR1         LIMIT                                        
*                                                                               
FTBL100  CR    R5,R4                                                            
         BL    FTBL120                                                          
         CLI   0(R5),0                                                          
         BNE   *+8                                                              
         MVI   0(R5),X'FF'                                                      
         AHI   R5,-4                                                            
         B     FTBL100                                                          
*                                                                               
FTBL120  MVI   SVDRFLAG,C'N'       INDICATE NO FLIGHTS                          
         CLI   SVDRSTR1,0          TEST ANY DATES IN TABLE                      
         BE    FTBLEX                                                           
         MVI   SVDRFLAG,C'Y'                                                    
*                                                                               
FTBLEX   J     EXITY                                                            
         DROP  R3                                                               
*================================================================               
*  FIND FLIGHT SEQ NUMBER IN FLIGHT TABLE                                       
*                                                                               
*  ON ENTRY:R3 POINTS TO BUY ELEMENT                                            
*                                                                               
*  ON EXIT :DAREBFLTL CONTAINS LIST OF ALL FLIGHTS                              
*          :DAREBFLT  CONTAINS FIRST FLIGHT#                                    
*          :DAREBFLT0 CONTAINS FIRST C'0', IF SPOTS FOUND IN FLIGHT-0           
*================================================================               
FLNUM    NTR1                                                                   
*                                                                               
         OC    SVDRFLT0,SVDRFLT0   HAVE FLIGHT0?                                
         BZ    FLNUM05                                                          
         CLI   DARBFLT0,0          ALREADY FOUND FLIGHT0?                       
         BNE   FLNUM05                                                          
         CLC   2(2,R3),SVDRFLT0                                                 
         BH    FLNUM05                                                          
         MVI   DARBFLT0,DARBFLTZ                                                
         MVI   DARBFLT,DARBFLTZ                                                 
*                                                                               
FLNUM05  LA    R4,SVDRSTR1         POINT R4 TO START/END DATES                  
         LA    R2,0                FLIGHT SEQ NUM                               
         LA    R5,16               MAX NUM OF FLIGHTS                           
*                                                                               
* HOLES IN THE TABLE ARE IDENTIFIED BY X'FF' IN THE FIRST BYTE                  
* END OF THE TABLE IS IDENTIFIED BY NULLS IN THE FIRST TWO BYTES                
*                                                                               
FLNUM10  CLC   0(2,R4),=X'0000'    NO MORE FLIGHTS?                             
         BE    FLNUMEX                                                          
         LLC   RE,DARBFLTL(R2)                                                  
         LTR   RE,RE               ALREADY HAVE SOMETHING FOR THIS FLT?         
         BNZ   FLNUM20               YES, SKIP IT THEN                          
         CLC   2(2,R3),0(R4)       ELEM DATE TO FLIGHT START                    
         BL    FLNUM20                                                          
         CLC   2(2,R3),2(R4)                                                    
         BH    FLNUM20                                                          
         LA    RE,1(R2)            ADD 1                                        
         STC   RE,DARBFLTL(R2)     AND SET FLIGHT ON IN LIST                    
         CLI   DARBFLT,0           FIRST FLIGHT NUM SET?                        
         BNE   FLNUMEX                                                          
         STC   RE,DARBFLT          FIRST TIME, LETS SET IT                      
         B     FLNUMEX                                                          
*                                                                               
FLNUM20  LA    R4,4(R4)            NEXT FLIGHT                                  
         LA    R2,1(R2)            NEXT SEQ NUM                                 
         BCT   R5,FLNUM10          END OF FLIGHT TABLE?                         
*                                                                               
FLNUMEX  J     EXITY                                                            
         DROP  RC                                                               
*                                                                               
DARBATCD DSECT                                                                  
SVDRFLAG DS    XL1                 C'Y' = TABLE BY FLIGHT                       
SVDRFLT0 DS    XL2                 FLIGHT 0 END DATE                            
SVDRSTR1 DS    XL2                 FLIGHT 1 START DATE                          
SVDREND1 DS    XL2                 FLIGHT 1 END DATE                            
         DS    15XL4               FLIGHTS 2-16 START/END DATES                 
SVDRDATX EQU   *                                                                
*                                                                               
DARBFLTA DS    0XL17               FLIGHT LIST, FOR ALL                         
DARBFLT0 DS    C                   FLIGHT0                                      
DARBFLTZ EQU   C'0'                FLIGHT 0 EQUATE                              
DARBFLTL DS    XL16                FLIGHT LIST, FOR 16 FLIGHTS                  
DARBFLT  DS    X                                                                
SVDBFLTEL DS   XL(DBFLTOVH)                                                     
DARFLG1  DS    X                                                                
DARF1RNF EQU   X'80'               RECORD NOT FOUND                             
DARF1UPF EQU   X'40'               FLIGHT ELEM UPDATED                          
DARF1UPD EQU   X'20'               WRITE RECORD BACK                            
DARF1UPP EQU   X'10'               UPDATE PASSIVE                               
DARF1SOF EQU   X'08'               HAVE SPOTS OUTSIDE FLIGHT                    
DARBATCL EQU   *-DARBATCD                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*================================================                               
* FIND UNUSED BUY LINES OR DELETED BUYLINES                                     
*  HALF   = 1ST SOFT DELETED LINE (X'80')                                       
*  MYHALF = LINE WE ARE READING FOR (WILL BE 'HOLE')                            
*================================================                               
                                                                                
NEXT     USING BUYKEY,IOKEY        BUILD NEW BUY KEY FROM INPUT DATA            
                                                                                
FINDHOLE NTR1  BASE=*,LABEL=*                                                   
         XC    HALF,HALF           SAVE 1ST DELETED LINE FOUND                  
         XC    MYHALF,MYHALF       NEXT LINE WE EXPECT                          
         MVI   MYHALF+1,1                                                       
*                                                                               
         MVC   IOKEY(L'BUYKEY),SVKEY                                            
         MVC   NEXT.BUYKBUY+1(2),=H'1' START READING WITH LINE 1                
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBHID+IOSPTDIR+B#BUYREC'                     
*                                                                               
FH10     CLC   IOKEY(10),IOKEYSAV  A-M/CLT/PRD/MKT/STA/EST                      
         JNE   FH60                                                             
         CLC   MYHALF,NEXT.BUYKBUY+1  IS NEXT BUY 1 NUMBER MORE?                
         BE    FH30                    YES - NO UNUSED HERE!                    
         MVC   SVKEY,IOKEYSAV          NO- THEN MYHALF HAS UNUSED LINE          
         MVC   SVKEY+11(2),MYHALF      (I DON'T THINK I NEED THIS)              
         J     FHXEQ                                                            
*                                                                               
FH30     TM    NEXT.BUYKCNTL,BUYRDEL IS THE LINE WE FOUND DELETED?              
         BZ    FH40                   NO                                        
         OC    HALF,HALF             ALREADY FOUND A DELETED LINE?              
         BNZ   FH40                   YES                                       
         MVC   HALF,NEXT.BUYKBUY+1   SAVE 1ST DELETED LINE                      
*                                                                               
FH40     LH    RE,MYHALF           NEXT LINE WE EXPECT TO GET                   
         AHI   RE,1                                                             
         STH   RE,MYHALF                                                        
         MVC   IOKEYSAV,IOKEY      SAVE LAST KEY READ                           
         MVI   GBYACT,GBYSEQ                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBSQD+IOSPTDIR+B#BUYREC'                     
         B     FH10                                                             
*                                                                               
* IF HERE, THERE WERE NO UNUSED LINE NUMBERS.  IF THERE IS A                    
* DELETED RECORD, BUILD THE BRAND LIST, CLEAR & BUILD THE RECORD,               
* SET THE KEY UNDELETED, AND MAKE SURE ENDBUY KNOWS NOT TO                      
* ADD THIS RECORD!                                                              
*                                                                               
FH60     OC    HALF,HALF           DID WE FIND A DELETED LINE TO USE?           
         JZ    FHXNEQ               NO - NO UNUSED LINES AVAIL                  
         MVC   IOKEY(L'BUYKEY),SVKEY                                            
         MVC   NEXT.BUYKBUY+1(2),HALF   GET THE DELETED BUY                     
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOBRDUPD+IOSPTDIR+B#BUYREC'                   
         MVC   SVKEY,IOKEYSAV     WE DETERMINED THAT THIS IS THE LINE           
*                                                                               
FH70     MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBGETUP+IOSPTFIL+B#BUYREC'                   
*                                                                               
         BRAS  RE,BLDXPRD          BUILD LIST OF EXISTING PRODUCTS              
*                                                                               
         L     R0,ABUYREC          INITIALIZE NEW BUY RECORD                    
         LHI   R1,IO5LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ABUYREC                                                       
         USING BUYRECD,RF                                                       
         MVC   BUYKEY(13),SVKEY                                                 
         MVC   BUYKBUY(2),SVKEY+11                                              
         MVI   BUYKBUY+2,0                                                      
*                                                                               
         LHI   R0,BDELEMX-BUYRECD                                               
         STCM  R0,3,BUYRLEN                                                     
         MVC   BUYALPHA,LP_AGY                                                  
         MVI   BDCODE,BDCODEQ      X'01' PRIMARY                                
         MVI   BDLEN,BDELEMX-BDELEM                                             
         DROP  RF                                                               
*                                                                               
* COMMENTING THIS CODE OUT SO THAT WE DON'T HAVE COPY/CHG IN RECOVERY           
* OF A EMPTY (INVALID) BUY RECORD                                               
*&&DO                                                                           
* OVERWRITE EXISTING DELETED BUY                                                
         MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBPUT+IOSPTFIL+B#BUYREC'                     
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
* COMMENTING THIS CODE OUT SO THAT WE DON'T HAVE COPY/CHG IN RECOVERY           
* OF A EMPTY (INVALID) BUY RECORD                                               
*                                                                               
         NI    IOKEY+13,X'FF'-X'80'   UNSET DELETED FLAG                        
         MVI   GBYACT,GBYWRT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOBWRT+IOSPTDIR+B#BUYREC'                     
         JNE   *+2                                                              
*                                                                               
         MVI   SVACTION,QACTOVER   MAKE SURE ENDBUY DOESN'T TRY TO ADD!         
         J     FHXEQ                                                            
*                                                                               
FHXEQ    J     EXITY                                                            
FHXNEQ   J     EXITN                                                            
*                                                                               
         EJECT                                                                  
*================================================                               
* CHANGE BUYER NAME                                                             
*================================================                               
                                                                                
CHGBYR   NTR1  BASE=*,LABEL=*                                                   
         USING GETBUBLD,WORK                                                    
         OC    QBUYER,QBUYER                                                    
         BZ    CBYX                                                             
         XC    GETBUBLD,GETBUBLD   SET BUYER CODE                               
         MVC   GBCOMFAC,ACOMFACS                                                
         MVC   GBIOA,AIO6                                                       
         XC    WORK2,WORK2                                                      
         MVI   WORK2,20            SET UP DUMMY FLDHDR                          
         MVI   WORK2+5,12                                                       
         MVC   WORK2+8(L'QBUYER),QBUYER                                         
         LA    R0,WORK2                                                         
         ST    R0,GBNAMFLD                                                      
         MVC   GBAGY,LP_AGY                                                     
         MVC   GBMEDEBC,SVMED                                                   
         MVC   GBCLTEBC,SVCLT                                                   
         L     RF,ACLTREC                                                       
         MVC   GBOFFICE,COFFICE-CLTRECD(RF)                                     
         MVC   GBAGYMD,SVBAGYMD                                                 
         MVC   GBCLT,SVBCLT                                                     
         MVI   GBPRD,X'FF'                                                      
         MVC   GBEST,SVBEST                                                     
         MVC   GBMKT(L'GBMKT+L'GBSTA),SVBMKT                                    
         MVI   GBTYPE,C'B'         SET FOR BUYER CODE                           
         GOTOR VGETBUBL,DMCB,GETBUBLD                                           
         CLI   GBERR,0             TEST HAVE ERROR                              
         JNE   *+2                                                              
                                                                                
CBYX     J     EXIT                                                             
         EJECT                                                                  
*================================================                               
* INSERT ACTIVITY ELEMENT                                                       
*================================================                               
ADDACTEL NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ELEM                                                          
         USING ACTVELEM,R3                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'99'                                                       
         MVI   ELEM+1,12                                                        
         L     R1,LP_ASECD                                                      
         MVC   ACTVADD,SECOPASS-SECD(R1)                                        
         GOTO1 VDATCON,DMCB,(5,0),(3,ACTVADD+2)                                 
*                                                                               
* IF THIS IS AN ADD, NO CHANGE DATE                                             
         CLI   SVACTION,QACTOVER                                                
         BE    ADDACT10                                                         
         CLI   SVACTION,QACTADD                                                 
         BE    ADDACT10                                                         
         MVC   ACTVCHG,ACTVADD                                                  
         DROP  R3                                                               
*                                                                               
ADDACT10 L     R3,ABUYREC          POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         EJECT                                                                  
*================================================                               
* UPDATE DESKTOP TRANSFER ELEM                                                  
*================================================                               
         USING DRVRECD,R4                                                       
ADDDTXEL NTR1  BASE=*,LABEL=*                                                   
         L     R4,AREVREC          R4=AVAIL/REVISION REC                        
                                                                                
         L     R3,ABUYREC                                                       
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,DTXCODEQ     REMOVE OLD TRANSFER ELEM                     
         MVI   ELCDHI,DTXCODEQ                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
                                                                                
         USING DTXELEM,R3                                                       
         XC    ELEM,ELEM           BUILD NEW TRANSFER ELEM                      
         LA    R3,ELEM                                                          
         MVI   DTXCODE,DTXCODEQ                                                 
         MVI   DTXLEN,DTXSLNQ                                                   
                                                                                
         MVI   DTXSTAT,DTXS_MNT    SET DEFAULT OF BUY MAINT                     
         MVC   DTXVER,LP_VRSN      SAVE PC VERSION NUMBER                       
         BRAS  RE,GETDTTM                                                       
         MVC   DTXTIME,WORK+2      TIME LAST CHANGED                            
                                                                                
         OC    QLINADDR,QLINADDR   IS THIS AVAIL/REV?                           
         BZ    ADDDT10              NO                                          
         MVI   DTXLEN,DTXLLNQ      SET LONGER LENGTH                            
         MVI   DTXSTAT,DTXS_REV    PRESET REVISION                              
         CLI   DRVKSUB,DRVKSUBQ    X'10' - REVISION SUBTYPE?                    
         BE    *+8                                                              
         MVI   DTXSTAT,DTXS_AVA    SET AVAIL                                    
         MVC   DTXSHT,DRVKREVS     SAVE SHEET NUMBER                            
         MVC   DTXLIN,DRVKREVL     SAVE LINE NUMBER                             
         DROP  R3                                                               
*                                                                               
ADDDT10  L     R3,ABUYREC          POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
*                                                                               
                                                                                
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,LP_AGY                                                 
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         MVC   L.LOCKMED,SVMED                                                  
         MVC   L.LOCKCLT,SVCLT                                                  
         MVC   L.LOCKSTA,SVSTA                                                  
         MVC   L.LOCKSTA+4(1),SVMED  SET MEDIA TO MATCH LOCK DATA               
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,LP_AGY                                                 
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,SVMED                                                  
         MVC   L.LOCKCLT,SVCLT                                                  
         LLC   R0,SVBEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVMED,C'T'          THIS ONLY WORKS FOR CANADA!                  
         BE    *+12                                                             
         CLI   SVMED,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
         BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         J     EXIT                                                             
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK IF AN AUTHORIZATION STATION RECORD EXISTS                     *         
*   BLOWS AWAY AIO3                                                             
***********************************************************************         
         USING CAWORKD,RC                                                       
**CHKAUTH  NTR1  BASE=*,LABEL=*,WORK=(RC,CAWORKL)                               
CHKAUTH  STM   RE,RC,12(RD)                                                     
         BASR  RB,0                                                             
         AHI   RB,-6                                                            
         USING CHKAUTH,RB                                                       
         STY   RD,(((CAWORKL+7)/8)*8)+76(RD)                                    
                                                                                
         LA    RC,72(RD)                                                        
         BASR  RE,0                                                             
         J     *+12                                                             
         DC    CL8'+CHKAUTH'                                                    
         MVC   0(4,RD),4(RE)                                                    
         LAY   RE,(((CAWORKL+7)/8)*8)+72(RD)                                    
                                                                                
         ST    RE,8(RD)                                                         
         LR    RD,RE                                                            
*                                                                               
         MVC   CAIOVALS,IOVALS                                                  
*                                                                               
         OC    BUMASPR1,BUMASPR1     BUY MASTER ALLOCATED?                      
         JZ    CAUT10                 NO, READ POL EST                          
         MVC   FULL,BUMASPR1          READ BRD EST                              
         L     R3,ACLTREC                                                       
         CLI   CPROF-CLTHDR(R3),C'0' TRUE POL BUYING?                           
         JNE   CAUT20                 NO, CONTINUE READ BRD EST                 
CAUT10   MVC   FULL(3),=C'POL'        READ POL EST                              
CAUT20   BRAS  RE,GTESTREC           INTO AESTREC                               
*                                                                               
         L     R4,AESTREC                                                       
         USING ESTHDR,R4                                                        
         TM    EFLAG1,EF1SDE       SDESK AUTH SET ON IN ESTIMATE?               
         JZ    CHKAUTHX             NO                                          
         DROP  R4                                                               
*                                                                               
         USING SPAUTHD,ELEM                                                     
CAUT30   XC    ELEM,ELEM           CALL SPAUTH                                  
         MVC   SPACOM,ACOMFACS                                                  
         LA    RF,CAIO                                                          
         ST    RF,SPAIO            USE AIO4 AS 4K IOAREA                        
*                                                                               
         MVC   SPAKAM(1),SVBAGYMD  A-M                                          
         MVC   SPAKCLT,SVBCLT      CLT                                          
         MVC   SPAKEST,SVBEST      ESTIMATE                                     
         MVC   SPAKMKT(5),SVBMKT   MKT/STA                                      
*                                                                               
         MVC   SPAKPRD(2),QBDMSPRD USE MASPRDS                                  
         CLI   SPAKPRD,0           MAKE SURE HAVE GOOD PRD                      
         JNE   *+8                                                              
         MVI   SPAKPRD,X'FF'       IF NO MASPRD USE POL                         
         OC    BUMASPR2,BUMASPR2   TEST PIGGYBACK?                              
         JZ    CAUT40                                                           
         CLI   SPAKPRD+1,0         WHY DON'T WE HAVE PIGGYBACK?                 
         JE    CAUT40              OH, JUST LEAVE IT                            
         CLC   BUMASPR1,BUMASPR2   PRODUCTS IN ALPHA SEQ                        
         JL    CAUT40                                                           
         MVC   SPAKPRD(1),QBDMSPRD+1                                            
         MVC   SPAKPRD+1(1),QBDMSPRD                                            
*                                                                               
CAUT40   LA    R4,SVSDEDTS                                                      
         ST    R4,SPAFATBL         SET DATE TABLE ADDRESS                       
*                                                                               
         LA    RE,SVSDEKEY                                                      
         CLC   SPAKEY,0(RE)        TEST KEYS AGREE                              
         JE    CAUT50                                                           
         MVC   0(L'SPAKEY,RE),SPAKEY     SAVE NEW KEY                           
*                                                                               
         OI    SPAFLAG,SPAFTBL     BUILD STA AUTH TABLE                         
         GOTO1 VSPAUTH,ELEM        BUILD DATE TABLE                             
*                                                                               
* COMPARE DATES IN TABLE TO DATES IN BUY RECORD                                 
*                                                                               
CAUT50   L     RE,ABUYREC                                                       
         MVI   ELCDLO,RCORGQ       X'06' ORIGINAL SPOT ELEM                     
         MVI   ELCDHI,RCPOTOQ      X'0C' POL OTO                                
         LA    R3,BDELEM-BUYREC(RE)                                             
*                                                                               
CAUT60   BRAS  RE,NEXTEL                                                        
         JNE   CHKAUTHX                                                         
*                                                                               
         LR    RE,R4               POINT TO START OF TABLE                      
         CLC   0(2,RE),=X'FFFF'    TEST EOT                                     
         JE    CHKAUTHX                                                         
*                                                                               
CAUT70   CLC   2(2,R3),0(RE)       SPOT TO AUTH START                           
         JL    CAUT80                                                           
         CLC   2(2,R3),2(RE)       SPOT TO AUTH END                             
         JNH   CAUT90              IN PERIOD, NEXT SPOT                         
*                                                                               
CAUT80   AHI   RE,4                                                             
         CLC   0(2,RE),=X'FFFF'    TEST EOT                                     
         JE    CAUT60                                                           
         J     CAUT70                                                           
*                                                                               
* NEED TO ADD STA LEVEL AUTH - SWING INTO ACTION                                
*                                                                               
CAUT90   MVI   SPAFLAG,SPAFBUY+SPAFUPT                                          
*&&DO                                                                           
         TM    UPSW,UPON           DOES SUPERDESK NEED TO KNOW DESKTOP          
         BZ    *+8                                                              
         OI    SPAFLAG,SPAFUPL                                                  
*&&                                                                             
         MVC   SPASDTE,2(R3)       SET ELEMENT DATE AS START                    
         MVC   SPAEDTE,2(R3)       AND END                                      
         GOTO1 VSPAUTH,ELEM                                                     
         CLI   SPAERR,0                                                         
         JE    CAUT60                                                           
         DC    H'0'                                                             
*                                                                               
CHKAUTHX MVC   IOVALS(IOVALL),CAIOVALS                                          
         J     EXITY                                                            
         EJECT                                                                  
CAWORKD  DSECT                                                                  
CAIOVALS DS    XL(IOVALL)                                                       
CAIO     DS    XL(4*ONEK)                                                       
CAWORKL  EQU   *-CAWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
*===============================================================                
* ROUTINE TO TRANSLATE BOOK TYPES                                               
*===============================================================                
                                                                                
TRNSBKT  LM    R2,R4,LP_AINP                                                    
         BCTR  R3,0                                                             
         XC    HALF,HALF                                                        
         BASR  RE,0                                                             
         MVC   HALF(0),0(R2)                                                    
         EX    R3,0(RE)                                                         
*                                                                               
         CLI   HALF,C' '                                                        
         JNH   EXITN                                                            
         OC    HALF,SPACES                                                      
*                                                                               
         GOTOR (#TRNSBT,ATRNSBT),DMCB,(R2),2,(R4),0                             
         CLI   0(R4),X'FF'                                                      
         JE    EXITN                                                            
         J     EXITY                                                            
         EJECT                                                                  
*=================================================================              
* ELEMENT MAINTENANCE ROUTINES                                                  
*=================================================================              
                                                                                
BUMPADD  CLI   0(R3),0             TEST AT EOR ALREADY                          
         JE    ADDEL                                                            
         LLC   R0,1(R3)            BUMP TO NEXT ELEM AND ADD ELEMENT            
         AR    R3,R0                                                            
                                                                                
ADDEL    NTR1  ,                   ADD AN ELEMENT                               
         GOTOR RECUP,DMCB,ABUYREC,ELEM,(R3)                                     
         J     EXIT                                                             
                                                                                
DELEL    NTR1  ,                   DELETE AN ELEMENT                            
         GOTOR RECUP,DMCB,ABUYREC,(R3)                                          
         J     EXIT                                                             
                                                                                
NEXTEL   CLI   0(R3),0             LOCATE AN ELEMENT                            
         JE    NEXTELN                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R3)          TEST VALID ELEM LENGTH                       
         JZ    *+2                                                              
         LA    R3,0(RF,R3)                                                      
NEXTEL2  CLC   ELCDLO,0(R3)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R3)                                                     
         JL    NEXTEL                                                           
NEXTELY  CR    RE,RE                                                            
         BR    RE                                                               
NEXTELN  LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBALLY ADDRESSABLE THINGS **            
                                                                                
VALERR02 LHI   RE,SE#MSGMD         MISSING MEDIA                                
         B     VALERXS2                                                         
                                                                                
VALERR03 MVI   XTRATEXT+1,C'|'                                                  
         MVC   XTRATEXT+3(L'QCLTA),QCLTA                                        
         LHI   RE,SE#INCLT         INVALID CLIENT                               
         B     VALERXS2                                                         
                                                                                
VALERR04 LHI   RE,SE#MSGCL         MISSING CLIENT                               
         B     VALERXS2                                                         
                                                                                
VALERR05 LHI   RE,SE#MSGMK         MISSING MARKET                               
         B     VALERXS2                                                         
                                                                                
VALERR06 MVI   XTRATEXT+1,C'|'                                                  
         MVC   XTRATEXT+3(L'QSTA),QSTA                                          
         LHI   RE,SE#INVST         INVALID STATION/NETWORK                      
         B     VALERXS2                                                         
                                                                                
VALERR07 LHI   RE,SE#MSGSN         MISSING STATION/NETWORK                      
         B     VALERXS2                                                         
                                                                                
VALERR08 LHI   RE,SE#MSGES         MISSING ESTIMATE                             
         B     VALERXS2                                                         
                                                                                
VALERR09 LHI   RE,SE#MSGSD         MISSING SPOT DATE                            
         B     VALERXS2                                                         
                                                                                
VALERR10 LHI   RE,SE#KEYNF         (BUY) KEY NOT FOUND                          
         B     VALERXS2                                                         
                                                                                
VALERR11 LHI   RE,SE#RECNF         (BUY) RECORD NOT FOUND                       
         B     VALERXS2                                                         
                                                                                
VALERR12 LHI   RE,SE#INVDM         INVALID DEMO                                 
         B     VALERXS2                                                         
                                                                                
VALERR13 MVI   XTRATEXT+1,C'|'                                                  
         MVC   XTRATEXT+3(L'QSTA),QSTA  SHOW THE STATION                        
         LHI   RE,SE#STLCK           STATION IS LOCKED                          
         B     VALERXS2               NOT A FATAL ERROR                         
                                                                                
VALERR14 LHI   RE,SE#NOALC         SPOT ALLOCATION NOT ALLOWED                  
         B     VALERXS2                                                         
                                                                                
VALERR15 LHI   RE,SE#CKSUM         CHKSUM MISMATCH                              
         B     VALERXS2               NOT A FATAL ERROR                         
                                                                                
VALERR16 LHI   RE,SE#MGMAX         NO MORE MAKEGOOD CODES                       
         B     VALERXS2                                                         
                                                                                
VALERR17 LHI   RE,SE#CKSUM         CHKSUM MISMATCH FROM 03B2                    
         B     VALERXS2               THIS ONE IS FATAL ERROR                   
*                                                                               
VALERR18 LHI   RE,SE#CSUPD         CAN'T UPDATE, COMSCORE DEMO ON EST,          
         B     VALERXS3            PLEASE UPGRADE                               
*                                                                               
VALERR19 LHI   RE,SE#NOAAA         PRODUCT AAA NOT ALLOWED!                     
         B     VALERXS2                                                         
                                                                                
MAXLNERR LHI   RE,SE#MAXLN         TOO MANY BUYLINES                            
         B     VALERXS2                                                         
                                                                                
MAXSZERR LHI   RE,SE#MAXSZ         REC TOO BIG                                  
         B     VALERXS2                                                         
                                                                                
VALERR20 MVI   XTRATEXT+1,C'|'                                                  
         MVC   XTRATEXT+3(L'EKEYPRD),0(R1)                                      
         LHI   RE,SE#INVPR         INVALID PRODUCT                              
         B     VALERXS2                                                         
*                                                                               
VALERR21 LHI   RE,SE#DATLK         DATA LOCKED FOR OFFLINE PROCESSING           
         B     VALERXS2                                                         
*                                                                               
VALERR22 LHI   RE,SE#ORDLK         WE HAVE A LOCKED ORDER!!!                    
         B     VALERXS2                                                         
*                                                                               
BUYC2LKD LHI   RE,SE#C2LKD         BUY DATA IS LOCKED BY C2 PROGRAM             
         B     VALERXS2                                                         
*                                                                               
BUYNOSPT LHI   RE,SE#NOSPT         NO SPOTS IN PERIOD                           
         B     VALERXS2                                                         
*                                                                               
HAVEUUID MVC   XTRATEXT(4),=C'UUID'  UUID IS ALREADY PRESENT.                   
*                                                                               
ALRDEXST LHI   RE,SE#TALPR             &T ALREADY PRESENT ON RECORD.            
         B     VALERXS2                                                         
*                                                                               
MISSUUID MVC   XTRATEXT(4),=C'UUID'  REQUIRED UUID IS MISSING.                  
*                                                                               
REQMISS  LHI   RE,SE#MISST             REQUIRED &T IS MISSING.                  
         B     VALERXS2                                                         
*                                                                               
NO2DECRT LHI   RE,SE#NO2DR         RADIO CANNOT USE 2-DECIMAL RATING            
         B     VALERXS2                                                         
*                                                                               
VALERXS2 LA    RF,SE#SY002         SET SYSTEM TO SPOT                           
         J     *+8                                                              
VALERXS3 LA    RF,SE#SY003         SET SYSTEM TO NET                            
*                                                                               
         GOTOR PUTERR,DMCB,((RF),(RE))                                          
*********                                                                       
* WE'RE SETTING ALL ERRORS TO FATAL AS SPOT DESKTOP IS NOT SHOWING              
* THE AVAIL DETAILS FOR EACH ERROR ANYWAYS.  EJOR SAID THAT BUY CONSTR          
* SHOULD HAVE CAUGHT THE PROBLEMS EARLIER.                                      
*********                                                                       
         MVI   ERRORFLG,FATALERR   SET TO SKIP ALL FURTHER INPUTS               
*                                                                               
EXITN    SR    RE,RE                                                            
         B     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
STAMPLIT DC    C'**SL1B**'                                                      
                                                                                
RECTAB   DS    0XL6                ** RECORD TABLE **                           
         DC    AL2(I#SDRXCK),AL3(VALCKSM),X'FF'  VALIDATE CHECKSUMS             
         DC    AL2(I#SDRXLK),AL3(CKDLCK),X'FF'   TEST FOR LOCKED ORDERS         
         DC    AL2(I#SDRXBH),AL3(VALBHD),X'80'   BUY HEADER                     
*                                                                               
         DC    AL2(I#SDRXBD),AL3(VALBDEL),X'00'  BUY DESC ELEM                  
         DC    AL2(I#SDRXBS),AL3(VALSPT),X'00'   SPOT DETAILS                   
         DC    AL2(I#SDRXBC),AL3(VALCOM),X'00'   COMMENTS                       
         DC    AL2(I#SDRXBI),AL3(VALCON),X'00'   CONTRACT/BUYID                 
         DC    AL2(I#SDRXOD),AL3(VALDEM),X'00'   DEMO VALUES (ORIG)             
         DC    AL2(I#SDRXOP),AL3(VALPBD),X'00'   POST BUY DEMO VALUES           
         DC    AL2(I#SDRXSD),AL3(VALSPL),X'00'   SPILL DEMO VALUES              
         DC    AL2(I#SDRXSP),AL3(VALPBD),X'00'   POST BUY SPILL DEMOS           
         DC    AL2(I#SDRXOR),AL3(VALORB),X'00'   ORBIT DATA                     
         DC    AL2(I#SDRXPK),AL3(VALPKG),X'00'   PACKAGE DATA                   
         DC    AL2(I#SDRXC2),AL3(VALCOS2),X'00'  SECOND COST DATA               
         DC    AL2(I#SDRXPU),AL3(VALPUR),X'00'   PURPOSE CODE DATA              
         DC    AL2(I#SDRXRC),AL3(VALRSN),X'00'   REASON CODE DATA               
         DC    AL2(I#SDRXUP),AL3(VALUPG),X'00'   UPGRADE FORMULA DATA           
         DC    AL2(I#SDRXAB),AL3(VALAVG),X'00'   AVERAGE BOOKS FORMULA          
         DC    AL2(I#SDRXAA),AL3(VALAAU),X'00'   AUTOMATE AVAIL UUID            
         DC    AL2(I#SDRXND),AL3(ENDBUY),X'00'   END BUY DATA                   
RECTABX  EQU   *                                                                
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
*                                                                               
         EJECT                                                                  
TTOKEN   DC    C'TOKEN'                                                         
TBDSTART DC    C'BDSTART'                                                       
TBDEND   DC    C'BDEND'                                                         
TBDWKS   DC    C'BDWKS'                                                         
TBDINPUT DC    C'BDINPUT'                                                       
TBDWKIND DC    C'BDWKIND'                                                       
TBDDAY   DC    C'BDDAY'                                                         
TBDNOWK  DC    C'BDNOWK'                                                        
TBDSEC   DC    C'BDSEC'                                                         
*TBDTIME  DC    C'BDTIME'                                                       
*TBDCOSTP DC    C'BDCOSTP'                                                      
TBDDAYPT DC    C'BDDAYPT'                                                       
TBDTIMST DC    C'BDTIMST'                                                       
TBDTIMND DC    C'BDTIMEND'                                                      
TBDPROCD DC    C'BDPROG CODE'                                                   
TBDPROGM DC    C'BDPROGRM'                                                      
TBDPROGT DC    C'BDPROGT'                                                       
TBDCOST  DC    C'BDCOST'                                                        
TBDCIND  DC    C'BDCIND'                                                        
TBDNTAX  DC    C'BDNTAX'                                                        
TBDWHY3  DC    C'BDWHY3'                                                        
TBDREP   DC    C'BDREP'                                                         
*TBDCHG   DC    C'BDCHG'                                                        
TBDWHY   DC    C'BDWHY'                                                         
*TBDPURP  DC    C'BDPURP'                                                       
TBDSEDAY DC    C'BDSEDAY'                                                       
TBDCIND2 DC    C'BDCIND2'                                                       
*TBCANAD  DC    C'BDCANAD'                                                      
TBDWHY2  DC    C'BDWHY2'                                                        
TBDSTAT  DC    C'BDSTAT'                                                        
TBDMGCOD DC    C'BDMGCODE'                                                      
TBDSTAT3 DC    C'BDSTAT3'                                                       
TBDNRGN  DC    C'BDNRGN'                                                        
TBDADVAG DC    C'BDADVAGY'                                                      
TBDMAST1 DC    C'BDMASPR1'                                                      
TBDMAST2 DC    C'BDMASPR2'                                                      
TBDSTAT2 DC    C'BDSTAT2'                                                       
TBDCON   DC    C'CONTRACT NUM'                                                  
TCMDATA1 DC    C'COM LINE 1'                                                    
TCMDATA2 DC    C'COM LINE 2'                                                    
TCMDATA3 DC    C'COM LINE 3'                                                    
TCMDATA4 DC    C'COM LINE 4'                                                    
TCMDATA5 DC    C'COM LINE 5'                                                    
*                                                                               
TBOOK    DC    C'BOOK'                                                          
TBKTYPE  DC    C'BOOKTYPE'                                                      
TPROGRAM DC    C'PROGRAM'                                                       
TDEMOVAL DC    C'DEMO VALUE'                                                    
TAGYMKT  DC    C'AGENCY MKT'                                                    
TRSMKT   DC    C'RTGSVC MKT'                                                    
TDEMOFLG DC    C'DEMO FLAGS'                                                    
TLKUPMKT DC    C'LKUP MKT'                                                      
TLKUPSTA DC    C'LKUP STA'                                                      
TLKUPRSM DC    C'LKUP RTG MKT'                                                  
TLKUPRSV DC    C'LKUP RTG SVC'                                                  
TPBDEM   DC    C'PBDEM'                                                         
TPBDEMOV DC    C'PBDEM FLAG'                                                    
TPBSDEM  DC    C'PBSDEM'                                                        
TPBSDMOV DC    C'PBSDEM FLAG'                                                   
TNTLKUP  DC    C'NON-TRAD DEMO LOOKED UP?'                                      
TNTSLKUP DC    C'NON-TRAD SPILL LOOKED UP?'                                     
TNTSLKNA DC    C'NON-TRAD LOOKUP IS N/A?'                                       
                                                                                
TPROVCD  DC    C'PROV CODE'                                                     
TSTAPCT  DC    C'STAPCT'                                                        
TSTACOST DC    C'STACOST'                                                       
TSTAFLAG DC    C'STA FLAGS'                                                     
TSTAGST  DC    C'GST'                                                           
TSTAPST  DC    C'PST'                                                           
TENDBUY  DC    C'END BUY ?'                                                     
                                                                                
TBUWEEK  DC    C'WEEK START'                                                    
TSDDATE  DC    C'SPOT DATE'                                                     
TSDSTAT  DC    C'SPOT STATUS'                                                   
TSDPRD1  DC    C'PRD1'                                                          
TSDLEN1  DC    C'SLN1'                                                          
TSDPRD2  DC    C'PRD2'                                                          
TSDLEN2  DC    C'SLN2'                                                          
TSDCOST  DC    C'SPOT COST'                                                     
TSDMGCD  DC    C'MKGD CODE'                                                     
TSDCDAT  DC    C'CLEAR DATE'                                                    
TSDCSEQ  DC    C'CLEAR SEQNUM'                                                  
TSDADAT  DC    C'AFFID DATE'                                                    
TSDATIM  DC    C'AFFID TIME'                                                    
TSDADAY  DC    C'AFFID DAY'                                                     
TSDAFL1  DC    C'AFFID FILM 1'                                                  
TSDAFL2  DC    C'AFFID FILM 2'                                                  
TSDDFLM  DC    C'DEALER TAG SEQNUM'                                             
TSDDTAG  DC    C'DEALER TAG NUMBER'                                             
TSDDDATE DC    C'TRAFFIC INST DATE'                                             
TSDTFL1  DC    C'TRAFFIC FILM 1'                                                
TSDTFL2  DC    C'TRAFFIC FILM 2'                                                
TSDTPAT  DC    C'TRAFFIC PTTN REF'                                              
*                                                                               
TFILE    DC    C'FILE'                                                          
TCASH    DC    C'CASH/TRADE'                                                    
TTIMST   DC    C'TIME START'                                                    
TTIMED   DC    C'TIME END'                                                      
TTPKGTYP DC    C'PACKAGE TYPE'                                                  
TTPKGLIN DC    C'PACKAGE LINE'                                                  
TCOS2    DC    C'SECOND COST/FACT'                                              
TFACT    DC    C'(F)ACTOR?'                                                     
TPURP    DC    C'PURPOSE CODE'                                                  
TLINADDR DC    C'AVAIL/REV D/A'                                                 
TRSNCODE DC    C'REASON CODE'                                                   
TRSNFLD  DC    C'REASON CHGD FLD'                                               
TRSNTXT  DC    C'REASON TEXT'                                                   
TUPTEXT  DC    C'UPGRADE FORMULA'                                               
TSPOTS   DC    C'NUMBER OF SPOTS'                                               
TROUTE   DC    C'DOWNLOAD ROUTES?'                                              
TPDCKSM  DC    C'PAID CKSMROUTES?'                                              
TAVGBK   DC    C'AVERAGE BOOKS FORMULA'                                         
TAAUUID  DC    C'AUTOMATED AVAIL UUID'                                          
         EJECT                                                                  
*============================================================                   
* BUY KEY                                                                       
*============================================================                   
                                                                                
SPTBUY   LKREQ H,I#SDRXBH,NEWREC=Y     MAPCODE 3A0                              
                                                                                
ACTION   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
MED      LKREQ F,2,(D,B#WORKD,QMEDX),(R,VALMED),TEXT=SP#MED                     
CLT      LKREQ F,3,(D,B#WORKD,QCLTX),(R,VALCLT),TEXT=SP#CLI                     
MKT      LKREQ F,4,(D,B#SAVED,QMKT),LBIN,TEXT=SP#MKT                            
STA      LKREQ F,5,(D,B#SAVED,QSTA),CHAR,TEXT=SP#STNET                          
EST      LKREQ F,6,(D,B#SAVED,QBEST),LBIN,TEXT=SP#EST                           
LIN      LKREQ F,7,(D,B#SAVED,QLIN),LBIN,TEXT=SP#BYLIN                          
BUYER    LKREQ F,8,(D,B#SAVED,QBUYER),CHAR,TEXT=SP#BUYER                        
CHKSUM   LKREQ F,9,(D,B#SAVED,QCHKSUM),HEXD,TEXT=SP#CKSUM                       
TOKEN    LKREQ F,10,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                     
DSKADDR  LKREQ F,11,(D,B#SAVED,QDSKADDR),HEXD,TEXT=SP#SDBDA                     
LINADDR  LKREQ F,12,(D,B#SAVED,QLINADDR),HEXD,TEXT=(*,TLINADDR)                 
ROUTE    LKREQ F,13,(D,B#SAVED,QROUTE),CHAR,TEXT=(*,TROUTE)                     
**IDCKSM LKREQ F,14,(D,B#SAVED,QPDCKSM),CHAR,TEXT=(*,TROUTE)                    
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* BUY DESC DATA                                                                 
*========================================================                       
                                                                                
BDDEFN   LKREQ H,I#SDRXBD,NEWREC=Y          MAPCODE 3A2                         
                                                                                
BDSTART  LKREQ F,1,(D,B#SAVED,QBDSTART),BDAT,TEXT=(*,TBDSTART)                  
BDEND    LKREQ F,2,(D,B#SAVED,QBDEND),BDAT,TEXT=(*,TBDEND)                      
BDWKS    LKREQ F,3,(D,B#SAVED,QBDWKS),LBIN,TEXT=(*,TBDWKS)                      
BDINPUT  LKREQ F,4,(D,B#SAVED,QBDINPUT),LBIN,TEXT=(*,TBDINPUT)                  
BDWKIND  LKREQ F,5,(D,B#SAVED,QBDWKIND),CHAR,TEXT=(*,TBDWKIND)                  
BDDAY    LKREQ F,6,(D,B#SAVED,QBDDAY),LBIN,TEXT=(*,TBDDAY)                      
BDNOWK   LKREQ F,7,(D,B#SAVED,QBDNOWK),LBIN,TEXT=(*,TBDNOWK)                    
BDSEC    LKREQ F,8,(D,B#SAVED,QBDSEC),LBIN,TEXT=(*,TBDSEC)                      
*BDTIME   LKREQ F,9,(D,B#SAVED,QBDTIME),LBIN,TEXT=(*,TBDTIME)                   
*BDCOSTP  LKREQ F,10,(D,B#SAVED,QBDCOSTP),LBIN,TEXT=(*,TBDCOSTP)                
BDDAYPT  LKREQ F,11,(D,B#SAVED,QBDDAYPT),CHAR,TEXT=(*,TBDDAYPT)                 
BDTIMST  LKREQ F,12,(D,B#SAVED,QBDTIMST),LBIN,TEXT=(*,TBDTIMST)                 
BDTIMEND LKREQ F,13,(D,B#SAVED,QBDTIMND),LBIN,TEXT=(*,TBDTIMND)                 
BDCODE   LKREQ F,37,(D,B#SAVED,QBDCODE),CHAR,TEXT=(*,TBDPROCD)                  
BDPROGRM LKREQ F,14,(D,B#SAVED,QBDPROGR),CHAR,TEXT=(*,TBDPROGM)                 
BDPROGT  LKREQ F,15,(D,B#SAVED,QBDPROGT),(R,VALADJ),TEXT=(*,TBDPROGT)           
BDCOST   LKREQ F,16,(D,B#SAVED,QBDCOST),CBIN,TEXT=(*,TBDCOST)                   
BDCIND   LKREQ F,17,(D,B#SAVED,QBDCIND),HEXD,TEXT=(*,TBDCIND)                   
BDNTAX   LKREQ F,18,(D,B#SAVED,QBDNTAX),LBIN,TEXT=(*,TBDNTAX)                   
BDWHY3   LKREQ F,19,(D,B#SAVED,QBDWHY3),HEXD,TEXT=(*,TBDWHY3)                   
BDREP    LKREQ F,20,(D,B#SAVED,QBDREP),(R,VALREP),TEXT=(*,TBDREP)               
*BDCHG    LKREQ F,21,(D,B#SAVED,QBDCHG),BDAT,TEXT=(*,TBDCHG)                    
BDWHY    LKREQ F,22,(D,B#SAVED,QBDWHY),HEXD,TEXT=(*,TBDWHY)                     
*BDPURP   LKREQ F,23,(D,B#SAVED,QBDPURP),CHAR,TEXT=(*,TBDPURP)                  
BDSEDAY  LKREQ F,24,(D,B#SAVED,QBDSEDAY),HEXD,TEXT=(*,TBDSEDAY)                 
*BDCANAD  LKREQ F,25,(D,B#SAVED,QBDCANAD),HEXD,TEXT=(*,TBCANAD)                 
BDCIND2  LKREQ F,26,(D,B#SAVED,QBDCIND2),HEXD,TEXT=(*,TBDCIND2)                 
BDWHY2   LKREQ F,27,(D,B#SAVED,QBDWHY2),HEXD,TEXT=(*,TBDWHY2)                   
BDSTAT   LKREQ F,28,(D,B#SAVED,QBDSTAT),HEXD,TEXT=(*,TBDSTAT)                   
BDMGDATE LKREQ F,29,(D,B#SAVED,QBDMGCD),CHAR,TEXT=(*,TBDMGCOD)                  
BDSTAT3  LKREQ F,30,(D,B#SAVED,QBDSTAT3),HEXD,TEXT=(*,TBDSTAT3)                 
BDNRGN   LKREQ F,31,(D,B#SAVED,QBDNRGN),CHAR,TEXT=(*,TBDNRGN)                   
BDMASPR1 LKREQ F,33,(D,B#SAVED,BUMASPR1),CHAR,TEXT=(*,TBDMAST1)                 
BDMASPR2 LKREQ F,34,(D,B#SAVED,BUMASPR2),CHAR,TEXT=(*,TBDMAST2)                 
BDSTAT2  LKREQ F,35,(D,B#SAVED,QBDSTAT2),HEXD,TEXT=(*,TBDSTAT2)                 
BDSTAT4  LKREQ F,36,(D,B#SAVED,QBDSTAT4),HEXD,TEXT=(*,TBDSTAT2)                 
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* SPOT DETAILS                                                                  
*============================================================                   
                                                                                
SPDEFN   LKREQ H,I#SDRXBS,NEWREC=Y              MAPCODE 3A3                     
                                                                                
BUWEEK   LKREQ F,01,(D,B#SAVED,BUWEEK),CDAT,TEXT=(*,TBUWEEK)                    
                                                                                
SDATE    LKREQ F,10,(I,B#SAVED,QASPOT),CDAT,OLEN=L'$SDDATE,            X        
               TEXT=(*,TSDDATE),ARRAY=S,SORT=NO                                 
                                                                                
SDSTAT   LKREQ F,11,,LBIN,OLEN=L'$SDSTAT,TEXT=(*,TSDSTAT)                       
SDPRD1   LKREQ F,12,,CHAR,OLEN=L'$SDPRD1,TEXT=(*,TSDPRD1)                       
SDLEN1   LKREQ F,13,,LBIN,OLEN=L'$SDLEN1,TEXT=(*,TSDLEN1)                       
SDPRD2   LKREQ F,14,,CHAR,OLEN=L'$SDPRD2,TEXT=(*,TSDPRD2)                       
SDLEN2   LKREQ F,15,,LBIN,OLEN=L'$SDLEN2,TEXT=(*,TSDLEN2)                       
SDCOST   LKREQ F,16,,LBIN,OLEN=L'$SDCOST,TEXT=(*,TSDCOST)                       
SDMGCD   LKREQ F,17,,CHAR,OLEN=L'$SDMGCD,TEXT=(*,TSDMGCD)                       
SDCDATE  LKREQ F,18,,CDAT,OLEN=L'$SDCLRDT,TEXT=(*,TSDCDAT)                      
SDCSEQ#  LKREQ F,19,,LBIN,OLEN=L'$SDCLRSQ,TEXT=(*,TSDCSEQ)                      
*** X'10' AFFID ELEM                                                            
SDADATE  LKREQ F,20,,CDAT,OLEN=L'$SDADATE,TEXT=(*,TSDADAT)                      
SDATIME  LKREQ F,21,,LBIN,OLEN=L'$SDATIME,TEXT=(*,TSDATIM)                      
*** X'12' FILM ELEM                                                             
SDADAY   LKREQ F,30,,HEXD,OLEN=L'$SDADAY,TEXT=(*,TSDADAY)                       
SDAFLM1  LKREQ F,22,,HEXD,OLEN=L'$SDAFLM1,TEXT=(*,TSDAFL1)                      
SDAFLM2  LKREQ F,23,,HEXD,OLEN=L'$SDAFLM2,TEXT=(*,TSDAFL2)                      
*** X'18' DEALER TAG ELEM (SUPPOSEDLY DEAD)                                     
SDDFLM   LKREQ F,24,,HEXD,OLEN=L'$SDDFLM,TEXT=(*,TSDDFLM)                       
SDDTAG   LKREQ F,25,,HEXD,OLEN=L'$SDDTAG,TEXT=(*,TSDDTAG)                       
SDDDATE  LKREQ F,26,,CDAT,OLEN=L'$SDDDATE,TEXT=(*,TSDDDATE)                     
*** X'18' TRAFFIC CML ASSIGN ELEM                                               
SDTFLM1  LKREQ F,27,,HEXD,OLEN=L'$SDTFLM1,TEXT=(*,TSDTFL1)                      
SDTFLM2  LKREQ F,28,,HEXD,OLEN=L'$SDTFLM2,TEXT=(*,TSDTFL2)                      
SDTPATT  LKREQ F,29,,HEXD,OLEN=L'$SDTPATT,TEXT=(*,TSDTPAT)                      
*                                                                               
SDSPOTS  LKREQ F,31,,LBIN,OLEN=L'$SDSPOTS,TEXT=(*,TSPOTS),             X        
               ARRAY=E                                                          
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* COMMENT ELEMENTS (MAP CODE IS COMMENT NUMBER)                                 
*============================================================                   
                                                                                
COMDEFN  LKREQ H,I#SDRXBC,NEWREC=Y        MAPCODE 3A4                           
                                                                                
CMDATA1  LKREQ F,1,(I,B#SAVED,QACOMM1),VSTR,TEXT=(*,TCMDATA1),         X        
               MAXLEN=255-(CMDATA-COMELEM)                                      
CMDATA2  LKREQ F,2,(I,B#SAVED,QACOMM2),VSTR,TEXT=(*,TCMDATA2),         X        
               MAXLEN=255-(CMDATA-COMELEM)                                      
CMDATA3  LKREQ F,3,(I,B#SAVED,QACOMM3),VSTR,TEXT=(*,TCMDATA3),         X        
               MAXLEN=255-(CMDATA-COMELEM)                                      
CMDATA4  LKREQ F,4,(I,B#SAVED,QACOMM4),VSTR,TEXT=(*,TCMDATA4),         X        
               MAXLEN=255-(CMDATA-COMELEM)                                      
CMDATA5  LKREQ F,5,(I,B#SAVED,QACOMM5),VSTR,TEXT=(*,TCMDATA5),         X        
               MAXLEN=255-(CMDATA-COMELEM)                                      
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* CONTRACT                                                                      
*============================================================                   
                                                                                
CONDEFN  LKREQ H,I#SDRXBI,NEWREC=Y        MAPCODE 3A5                           
                                                                                
BDCON#   LKREQ F,1,(D,B#SAVED,QCONTRCT),VSTR,TEXT=(*,TBDCON),          X        
               MAXLEN=L'QCONTRCT                                                
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* DEMO VALUES                                                                   
*============================================================                   
                                                                                
SPDEMEL  LKREQ H,I#SDRXOD,NEWREC=Y           MAPCODE 3A6                        
                                                                                
BUBOOK   LKREQ F,1,(D,B#SAVED,BUBOOK),BDAT,TEXT=(*,TBOOK)                       
BUBKTYPE LKREQ F,2,(D,B#SAVED,BUBKTYPE),(R,TRNSBKT),TEXT=(*,TBKTYPE)            
BUPROG   LKREQ F,3,(D,B#SAVED,BUPROG),VSTR,TEXT=(*,TPROGRAM),          X        
               MAXLEN=L'BUPROG                                                  
*&&DO                                                                           
DEMQMKT  LKREQ F,4,(D,B#SAVED,DEMQMKT),CHAR,TEXT=(*,TLKUPMKT) LKUP MKT          
DEMQSTA  LKREQ F,5,(D,B#SAVED,DEMQSTA),CHAR,TEXT=(*,TLKUPSTA)  LKUP STA         
DEMQRSV  LKREQ F,6,(D,B#SAVED,DEMQRSV),CHAR,TEXT=(*,TLKUPRSV) LKUP RSV          
*&&                                                                             
*                                                                               
DEMCODE  LKREQ F,10,(I,B#SAVED,QADEMO),CHAR,                           X        
               OLEN=L'$QDEMONM,ARRAY=S,SORT=NO,TEXT=SP#DEMO                     
DEMOVAL  LKREQ F,11,,LBIN,OLEN=L'$QDEMRAW,TEXT=(*,TDEMOVAL)                     
DEMOFLG  LKREQ F,12,,HEXD,OLEN=L'$QDEMFLG,TEXT=(*,TDEMOFLG)                     
DEMNTLKU LKREQ F,13,,MB80,OLEN=0,TEXT=(*,TNTLKUP)                               
DEMNTSLK LKREQ F,14,,MB40,OLEN=0,TEXT=(*,TNTSLKUP)                              
DEMNTNA  LKREQ F,15,,MB20,OLEN=L'$QDEMNTF,TEXT=(*,TNTSLKNA),ARRAY=E             
                                                                                
         LKREQ E                                                                
*============================================================                   
* POST BUY DEMO VALUES                                                          
*============================================================                   
                                                                                
PBDEMEL  LKREQ H,I#SDRXOP,NEWREC=Y           MAPCODE 3A7                        
                                                                                
PBDEMVL  LKREQ F,11,(I,B#SAVED,QAPBDEMO),LBIN,OLEN=4,                  X        
               ARRAY=S,SORT=NO,TEXT=(*,TPBDEM)                                  
PBDEMOV  LKREQ F,12,,HEXD,OLEN=1,TEXT=(*,TPBDEMOV),ARRAY=E                      
                                                                                
         LKREQ E                                                                
*============================================================                   
* SPILL DEMO VALUES                                                             
*============================================================                   
                                                                                
SPSPLEL  LKREQ H,I#SDRXSD,NEWREC=Y            MAPCODE 3A8                       
                                                                                
*BUSPLBK  LKREQ F,1,(D,B#SAVED,BUBOOK),BDAT,TEXT=(*,TBOOK)                      
BUBOOK   LKREQ F,1,(I,B#SAVED,QABOOK),VSTR,TEXT=(*,TBOOK)                       
BUBKTYPE LKREQ F,2,(D,B#SAVED,BUBKTYPE),(R,TRNSBKT),TEXT=(*,TBKTYPE)            
DEMQMKT  LKREQ F,4,(D,B#SAVED,DEMQMKT),CHAR,TEXT=(*,TLKUPMKT)                   
*&&DO                                                                           
DEMQSTA  LKREQ F,5,(D,B#SAVED,DEMQSTA),CHAR,TEXT=(*,TLKUPSTA)                   
*&&                                                                             
DEMQRSV  LKREQ F,6,(D,B#SAVED,DEMQRSV),CHAR,TEXT=(*,TLKUPRSV)                   
DEMRSMKT LKREQ F,13,(D,B#SAVED,DEMRSMKT),LBIN,TEXT=(*,TLKUPRSM)                 
BUSPLAMK LKREQ F,7,(D,B#SAVED,BUSPLAMK),LBIN,TEXT=(*,TAGYMKT)                   
* I THINK BUSPLRMK IS ONLY USED FOR POST BUY                                    
*&&DO                                                                           
BUSPLRMK LKREQ F,8,(D,B#SAVED,BUSPLRMK),LBIN,TEXT=(*,TRSMKT)                    
*&&                                                                             
SPDMCOD  LKREQ F,10,(I,B#SAVED,QADEMO),CHAR,                           X        
               OLEN=L'$QDEMONM,ARRAY=S,SORT=NO,TEXT=SP#DEMO                     
SPDMVAL  LKREQ F,11,,LBIN,OLEN=L'$QDEMRAW,TEXT=(*,TDEMOVAL)                     
SPDMFLG  LKREQ F,12,,HEXD,,OLEN=L'$QDEMFLG,TEXT=(*,TDEMOFLG)                    
*SPDNTLKU LKREQ F,13,,MB80,OLEN=0,TEXT=(*,TNTLKUP)                              
DEMNTSLK LKREQ F,14,,MB40,OLEN=0,TEXT=(*,TNTSLKUP)                              
DEMNTNA  LKREQ F,15,,MB20,OLEN=L'$QDEMNTF,TEXT=(*,TNTSLKNA),ARRAY=E             
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* SPILL POST BUY DEMO VALUES                                                    
*============================================================                   
                                                                                
PBSDEM   LKREQ H,I#SDRXSP,NEWREC=Y           MAPCODE 3A9                        
                                                                                
PBSAMK   LKREQ F,7,(D,B#SAVED,BUSPLAMK),LBIN,TEXT=(*,TAGYMKT)                   
PBSRMK   LKREQ F,8,(D,B#SAVED,BUSPLRMK),LBIN,TEXT=(*,TRSMKT)                    
PBSDMVL  LKREQ F,11,(I,B#SAVED,QAPBDEMO),LBIN,OLEN=4,                  X        
               ARRAY=S,SORT=NO,TEXT=(*,TPBSDEM)                                 
PBSDMOV  LKREQ F,12,,HEXD,OLEN=1,TEXT=(*,TPBSDMOV),ARRAY=E                      
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* BUY UPGRADE FORMULA                                                           
*============================================================                   
                                                                                
UPGRADE  LKREQ H,I#SDRXUP,NEWREC=Y          MAPCODE 3AA                         
                                                                                
UPGRADE  LKREQ F,1,(D,B#SAVED,UPTEXT),VSTR,MAXLEN=L'UPTEXT,            X        
               TEXT=(*,TUPTEXT)                                                 
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* BUY AVERAGE BOOKS FORMULA                                                     
*============================================================                   
                                                                                
AVGBK    LKREQ H,I#SDRXAB,NEWREC=Y          MAPCODE 3A1                         
                                                                                
AVGBKFRM LKREQ F,1,(I,B#SAVED,QAAVGBK),VSTR,TEXT=(*,TAVGBK),           X        
               MAXLEN=255-(AVGBKLNQ)                                            
                                                                                
         LKREQ E                                                                
*============================================================                   
* BUY ORBIT                                                                     
*============================================================                   
                                                                                
ORBIT    LKREQ H,I#SDRXOR,NEWREC=Y          MAPCODE 3AB                         
                                                                                
ORBDAY   LKREQ F,1,(I,B#SAVED,QAORBIT),LBIN,OLEN=L'$ORBDAY,            X        
               ARRAY=S,SORT=NO,TEXT=SP#DAY                                      
ORBTIMS  LKREQ F,2,,LBIN,OLEN=L'$ORBTIMS,TEXT=(*,TTIMST)                        
ORBTIME  LKREQ F,3,,LBIN,OLEN=L'$ORBTIME,TEXT=(*,TTIMED)                        
ORBPRG   LKREQ F,4,,CHAR,OLEN=L'$ORBPRG,TEXT=SP#PRG                             
ORBDEMV  LKREQ F,5,,LBIN,OLEN=L'$ORBDEMV,TEXT=SP#DEMO                           
ORBDEMF  LKREQ F,6,,HEXD,OLEN=L'$ORBDEMF,TEXT=(*,TDEMOFLG),ARRAY=E              
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* PACKAGE                                                                       
*============================================================                   
                                                                                
PACKAGE  LKREQ H,I#SDRXPK,NEWREC=Y          MAPCODE 3AC                         
                                                                                
PKGTYPE  LKREQ F,1,(D,B#SAVED,QPKGTYP),LBIN,TEXT=(*,TTPKGTYP)                   
PKGLINE  LKREQ F,2,(I,B#SAVED,QAPKGLIN),LBIN,OLEN=2,                   X        
               ARRAY=*,SORT=NO,TEXT=(*,TTPKGLIN)                                
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* SECOND COST                                                                   
*============================================================                   
                                                                                
COS2     LKREQ H,I#SDRXC2,NEWREC=Y          MAPCODE 3AD                         
                                                                                
C2VALUE  LKREQ F,1,(D,B#SAVED,MYDUB),SPAK,TEXT=(*,TCOS2)                        
C2FACT   LKREQ F,2,(D,B#SAVED,MYBYTE),CHAR,TEXT=(*,TFACT)                       
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* PURPOSE CODE                                                                  
*============================================================                   
                                                                                
PUR      LKREQ H,I#SDRXPU,NEWREC=Y          MAPCODE 3AE                         
                                                                                
PURCD    LKREQ F,1,(D,B#SAVED,PURCODE),VSTR,MAXLEN=L'PURCODE,          X        
               TEXT=(*,TPURP)                                                   
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* REASON CODE                                                                   
*============================================================                   
                                                                                
REASON   LKREQ H,I#SDRXRC,NEWREC=Y          MAPCODE 3AF                         
                                                                                
RSNCODE  LKREQ F,1,(D,B#SAVED,MYDUB),VSTR,MAXLEN=L'RCELRC,             X        
               TEXT=(*,TRSNCODE)                                                
RSNFLD   LKREQ F,2,(D,B#SAVED,MYBYTE),LBIN,OLEN=L'RCFLDID,             X        
               TEXT=(*,TRSNFLD)                                                 
RSNTXT   LKREQ F,3,(I,B#SAVED,QACOMM1),VSTR,TEXT=(*,TRSNTXT),          X        
               MAXLEN=255-(RCELTXT-RCELEM)                                      
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* AUTOMATED AVAIL UUID                                                          
*============================================================                   
AUTOAV   LKREQ H,I#SDRXAA,NEWREC=Y           MAPCODE 3B3                        
AAUUID   LKREQ F,1,(I,B#SAVED,QAAAUID),VSTR,TEXT=(*,TAAUUID)                    
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* END OF BUY                                                                    
*============================================================                   
                                                                                
ENDBYEL  LKREQ *,I#SDRXND,NEWREC=Y             MAPCODE 3B0                      
                                                                                
                                                                                
*============================================================                   
* CHECKSUM DATA                                                                 
*============================================================                   
                                                                                
SPCKSM   LKREQ H,I#SDRXCK,NEWREC=Y           MAPCODE 3B1                        
                                                                                
CKSMTOK  LKREQ F,1,(I,B#SAVED,QACKSM),CHAR,OLEN=L'$CKSMTOK,            X        
               ARRAY=S,SORT=NO,TEXT=(*,TTOKEN)                                  
CKSMFIL  LKREQ F,2,,CHAR,OLEN=L'$CKSMFIL,TEXT=(*,TFILE)                         
CKSMDA   LKREQ F,3,,HEXD,OLEN=L'$CKSMDA,TEXT=SP#SDBDA                           
CKSMVAL  LKREQ F,4,,HEXD,OLEN=L'$CKSMVAL,TEXT=SP#CKSUM,ARRAY=E                  
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* TEST FOR DARE LOCKED ORDERS                                                   
*============================================================                   
                                                                                
SPLOCK   LKREQ H,I#SDRXLK,NEWREC=Y           MAPCODE 3B2                        
                                                                                
LOCKMED  LKREQ F,1,(I,B#SAVED,QALOCK),(U,#VALMED,$VALMED),ARRAY=S,     X        
               OLEN=L'$LCKBAMD,MAXLEN=L'QMEDA,SORT=NO,TEXT=SP#MED               
LOCKCLT  LKREQ F,2,,(U,#VALCLT,$VALCLT),OLEN=L'$LCKBCLT,               X        
               MAXLEN=L'QCLTA,TEXT=SP#CLI                                       
LOCKPRD  LKREQ F,3,,(U,#VALPRD,$VALPRD),OLEN=L'$LCKBPRD,               X        
               MAXLEN=L'QPRDA,TEXT=SP#PRO                                       
LOCKPR2  LKREQ F,4,,(U,#VALPRD,$VALPRD),OLEN=L'$LCKBPR2,               X        
               MAXLEN=L'QPRDA,TEXT=SP#PRO                                       
LOCKEST  LKREQ F,5,,LBIN,OLEN=L'$LCKBEST,TEXT=SP#EST                            
LOCKSTA  LKREQ F,6,,(U,#VALSTA,$VALSTA),,OLEN=L'$LCKBSTA,              X        
               MAXLEN=L'QSTA,TEXT=SP#STA                                        
LOCKFLT  LKREQ F,7,,LBIN,OLEN=L'$LCKBFLT,TEXT=SP#FLGHT                          
LOCKCT   LKREQ F,8,,CHAR,OLEN=L'$LCKCORT,TEXT=SP#XMIT                           
LOCKPCKY LKREQ F,9,,CHAR,OLEN=L'$LCKPCKY,TEXT=SP#KEY,ARRAY=E                    
                                                                                
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
                                                                                
SAVED    DSECT                                                                  
                                                                                
STAMP    DS    CL(L'STAMPLIT)      OVERLAY STAMP                                
                                                                                
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
RECUP    DS    A                   A(RECUP)                                     
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
CKSUMBFR DS    F                                                                
*                                                                               
AC2LKTBL DS    A                   A(PRDS THAT ARE C2 LOCKED)                   
*                                                                               
AMAPNUM  DS    A                   A(RECORD MAP NUMBER)                         
SVTXPNUM DS    H                                                                
DATALEN  DS    H                   LENGTH OF INPUT DATA                         
ERRORFLG DS    X                                                                
FATALERR EQU   C'F'                FATAL ERROR, EXIT                            
ERRRGULR EQU   C'R'                REGULAR ERROR                                
ERRSTLCK EQU   C'L'                STATION IS LOCKED                            
XFRAVAIL DS    C                                                                
*                                                                               
SVNTELEM DS    XL(L'ELEM)                                                       
*                                                                               
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVACTION DS    C                   SAVED ACTION CODE                            
*                                                                               
PROCFLAG DS    X                   FIRST TIME FLAG                              
PROCSPIL EQU   X'80'               - SPILL WAS PROCESSED                        
*                                                                               
SVSPLMKS DS    XL(L'NDAGYMKT*MXSPLMKS)                                          
MXSPLMKS EQU   20                                                               
*                                                                               
SAVE1OR2 DS    X                                                                
*                                                                               
SVMED    DS    CL1                 MEDIA LETTER                                 
SVCLT    DS    CL3                 CLIENT CODE                                  
*                                                                               
SVBVALS  EQU   *                                                                
SVBAGYMD DS    XL1                 MEDIA CODE                                   
SVBCLT   DS    XL2                 CLIENT PACKED                                
SVBEST   DS    X                   ESTIMATE NUMBER                              
SVBLIN   DS    XL2                 BUY LINE NUMBER                              
SVBMKT   DS    XL2                 MARKET NUMBER                                
SVBSTA   DS    XL3                 STATION PACKED                               
SVSTA    DS    CL8                 STATION CALL LETTERS                         
SVKEY    DS    XL20                BUY KEY                                      
SVSPDATE DS    XL2                 SPOT DATE                                    
SVMKTSTA DS    XL5                                                              
SVSFLAG1 DS    XL1                 SAVED STATION FLAG1                          
SLINADDR DS    F                   SAVED AVAIL/REV LINE D/A                     
*                                                                               
SVUPBOOK DS    XL2                 SAVED BOOK FROM UPGRADE                      
SVUPBTYP DS    C                   SAVE BOOK TYPE FROM UPGRADE                  
*                                                                               
SVBVALSX EQU   *                                                                
*                                                                               
SVMGTKNS DS    (SVNWMGMX)XL(MGTKNLNQ)                                           
SVMGTKNX EQU   *                                                                
SVNWMGMX EQU   15                  MAXIMUM OF 15 NEW MAKEGOODS                  
*                                   OR IT WILL BLOW AWAY QBDMGCD                
SVMGCDTF DS    X                   SAVE MG CODE TABLE FLAG                      
SVMGCDTB DS    CL1024              SAVE MG CODE TABLE                           
*                                                                               
SVSDEKEY DS    XL11                KEY FOR DATE TABLE BELOW                     
         DS    0F                                                               
SVSDEDTS DS    XL128               SUPERDESK AUTHORIZATION DATES                
*                                                                               
XTRATEXT DS    CL20                EXTRA ERROR MESSAGE TEXT                     
                                                                                
         DS    0D                                                               
XPRDLIST DS    XL256               PRODUCT CODES IN EXISTING BUYREC             
                                                                                
BUWEEK   DS    XL2                 SPOT WEEK START DATE                         
BUWEEKX  DS    XL2                 SPOT WEEK END DATE                           
                                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
BUMASPR1 DS    CL3                                                              
BUMASPR2 DS    CL3                                                              
*                                                                               
DEMRSMKT DS    XL2                                                              
DEMQMKT  DS    CL(L'NDMKTALF)                                                   
DEMQSTA  DS    CL5                                                              
DEMQRSV DS     CL1                                                              
*                                                                               
BUBOOK   DS    XL3                 BINARY DATE                                  
QASPILBK DS    A                                                                
BUBKTYPE DS    CL1                                                              
BUPROG   DS    CL16                                                             
         ORG   BUPROG                                                           
LKUPKEY  DS    CL16                                                             
         ORG   BUPROG                                                           
PURCODE  DS    CL12                PURPOSE CODE                                 
         ORG                                                                    
*                                                                               
BUSPLAMK DS    XL2                                                              
BUSPLRMK DS    XL2                                                              
BUSPLBTY DS    CL1                                                              
*                                                                               
PACKOF4B DS    PL4                                                              
*                                                                               
         DS    0D                  RE-USABLE FIELDS FOR LKREQS                  
MYDUB    DS    D                                                                
MYDUB2   DS    D                                                                
MYFULL   DS    F                                                                
MYFULL2  DS    F                                                                
MYHALF   DS    H                                                                
MYHALF2  DS    H                                                                
MYBYTE   DS    X                                                                
MYBYTE2  DS    X                                                                
                                                                                
QACTION  DS    C                   ** ACTION CODE **                            
QACTADD  EQU   C'A'                ADD A NEW BUY                                
QACTOVER EQU   C'O'                OVERWRITE A DELETED BUY                      
QACTCHA  EQU   C'C'                CHANGE AN EXISTING BUY                       
QACTDEL  EQU   C'D'                DELETE EXISTING BUY                          
QACTDELA EQU   C'E'                DELETE EXISTING BUY, MARK HISTORY            
QBEST    DS    X                   ESTIMATE NUMBER                              
QLIN     DS    XL2                 BUY LINE NUMBER                              
QMKT     DS    XL2                                                              
QSTA     DS    CL8                                                              
QBUYER   DS    CL12                BUYER CODE                                   
QCHKSUM  DS    F                   CHECK SUM                                    
QDSKADDR DS    F                                                                
QLINADDR DS    F                   AVAIL/REVLINE DISK ADDR                      
QTOKEN   DS    CL20                                                             
QCONTRCT DS    CL12                                                             
QPKGTYP  DS    X                                                                
QROUTE   DS    C                                                                
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE AREA                          
*                                                                               
         DS    0D                                                               
QBDELEM  DS    XL96                BUY DESCRIPTION DATA                         
         ORG   QBDELEM              NOOP ORG BC UPTEXT CLOBBER QBDELEM          
QBDSTART DS    XL(L'BDSTART)       BUY START DATE                               
QBDEND   DS    XL(L'BDEND)         BUY END DATE                                 
QBDWKS   DS    XL(L'BDWKS)         NUMBER OF WEEKS                              
QBDINPUT DS    XL(L'BDINPUT)       INPUT METHOD                                 
QBDWKIND DS    XL(L'BDWKIND)       O=1/WK A=1/2WK T=1/3WK F=1/4WK               
QBDDAY   DS    XL(L'BDDAY)         DAY                                          
QBDNOWK  DS    XL(L'BDNOWK)        NUMBER OF SPOTS PER WEEK                     
QBDSEC   DS    XL(L'BDSEC)         SECONDS LENGTH                               
QBDTIME  DS    XL(L'BDTIME)        TIME PORTION FOR PIGGYBACK                   
QBDCOSTP DS    XL(L'BDCOSTP)       COST PORTION FOR PIGGYBACK                   
QBDDAYPT DS    XL(L'BDDAYPT)       DAYPART CODE                                 
QBDTIMST DS    XL(L'BDTIMST)       START TIME (MILITARY)                        
QBDTIMND DS    XL(L'BDTIMEND)      END TIME   (MILITARY)                        
QBDPROGR DS    XL(L'BDPROGRM)      PROGRAMMING                                  
QBDPROGT DS    XL(L'BDPROGT)       PROGRAM ADJACENCY CODE                       
         DS    XL(L'BDCOST)        SPOT COST PLACEHOLDER - SEE BELOW            
QBDCIND  DS    XL(L'BDCIND)        COST INDICATOR                               
         DS    XL(L'BDXFRAGY)                                                   
QBDNTAX  DS    XL(L'BDNTAX)        TAX RATE (3DP)                               
QBDWHY3  DS    XL(L'BDWHY3)        ACTIVTY BITS                                 
QBDREP   DS    XL(L'BDREP)         REP CODE                                     
QBDCHG   DS    XL(L'BDCHG)         LAST CHANGE DATE (YMD)                       
QBDWHY   DS    XL(L'BDWHY)         ACTIVITY BITS                                
QBDPURP  DS    XL(L'BDPURP)        PURPOSE CODE                                 
QBDSEDAY DS    XL(L'BDSEDAY)       START/END DAY                                
QBDCANAD DS    XL(L'BDCANAD)       C'C' FOR CANADIAN BUY                        
QBDCIND2 DS    XL(L'BDCIND2)       INDICATOR BYTE                               
QBDWHY2  DS    XL(L'BDWHY2)        ACTIVTY BITS                                 
QBDSTAT  DS    XL(L'BDSTAT)        STATUYS BITS                                 
QBDMGCD  DS    XL(L'BDMGDATE)      MAKEGOOD CODE                                
QBDSTAT3 DS    XL(L'BDSTAT3)       STATUS BITS                                  
QBDNRGN  DS    XL(L'BDNRGN)        NETWORK REGION CODE                          
QBDMGSPT DS    XL(L'BDMGSPOT)      MG SPOT NO FOR POOL MISSED MONTH             
QBDADVAG DS    XL(L'BDADVAGY)      ADVERTISER AGENCY CODE                       
QBDMSPRD DS    0XL(L'BDMASPRD)     ** MASTER PRODUCT CODE(S) **                 
         DS    X                   MASPRD1 - SET IN VALBDEL                     
         DS    X                   MASPRD2 - SET IN VALBDEL                     
QBDSTAT2 DS    XL(L'BDSTAT2)       STATUS BITS                                  
QBDSTAT4 DS    XL(L'BDSTAT4)       STATUS BITS                                  
*                                                                               
QBDMST1C DS    CL(L'EKEYPRD)       MASTER PRODUCT 1 CODE                        
QBDMST2C DS    CL(L'EKEYPRD)       MASTER PRODUCT 2 CODE                        
QBDCON#  DS    CL(L'IDCONNO)       CONTRACT NUMBER                              
QBDCODE  DS    CL5                 PROGRAM CODE                                 
QBDCOST  DS    CL4                 COST                                         
         ORG                                                                    
*                                   ALSO CHKAUTH USES QBDELEM                   
UPTEXT   DS    CL40                UPGRADE FORMULA TEXT                         
**NOOP   ORG                                                                    
                                                                                
QACOMM1  DS    A                   A(COMMENT LINE 1)                            
QACOMM2  DS    A                   A(COMMENT LINE 2)                            
QACOMM3  DS    A                   A(COMMENT LINE 3)                            
QACOMM4  DS    A                   A(COMMENT LINE 4)                            
QACOMM5  DS    A                   A(COMMENT LINE 5)                            
                                                                                
QADEMO   DS    0A                  A(ORIG/SPILL DEMO LIST)                      
QAPBDEMO DS    A                   A(PB DEMO LIST)                              
QASPOT   DS    A                   A(SPOT ARRAY)                                
QACKSM   DS    A                   A(CKSM ARRAY)                                
QALOCK   DS    A                   A(CHECK LOCKED ORDER ARRAY)                  
QAORBIT  DS    A                   A(ORBIT ARRAY)                               
QAPKGLIN DS    A                   A(PKG LINE ARRAY)                            
QAAVGBK  DS    A                   A(AVERAGE BOOK FORMULA)                      
QABOOK   DS    A                   A(BOOK)                                      
QAAAUID  DS    A                   A(AUTOMATED-AVAIL UUID)                      
                                                                                
SAVEDX   EQU   *                                                                
         EJECT                                                                  
* INCLUDED DSECTS FOLLOW                                                        
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
*                                                                               
B#B3IO1  EQU   3                   IO1                                          
B#AGYREC EQU   3                   - AGENCY RECORD                              
                                                                                
B#B4IO2  EQU   4                   IO2 -                                        
B#CLTREC EQU   4                   - CLIENT RECORD                              
                                                                                
B#B5IO3  EQU   5                   IO3 -                                        
B#ESTREC EQU   5                   - ESTIMATE RECORD                            
AESTREC  EQU   LP_BLKS+((B#ESTREC-1)*L'LP_BLKS),,C'A'                           
                                                                                
B#B6IO4  EQU   6                   IO4 -                                        
B#ORDREC EQU   6                   - ORDER RECORD                               
AORDREC  EQU   LP_BLKS+((B#ORDREC-1)*L'LP_BLKS),,C'A'                           
B#FLTREC EQU   6                   - FLIGHT RECORD                              
AFLTREC  EQU   LP_BLKS+((B#FLTREC-1)*L'LP_BLKS),,C'A'                           
B#BTCREC EQU   6                   - BATCH RECORD                               
ABTCREC  EQU   LP_BLKS+((B#BTCREC-1)*L'LP_BLKS),,C'A'                           
                                                                                
B#B7IO5  EQU   7                   IO5 -                                        
B#BUYREC EQU   7                   - BUY RECORD                                 
ABUYREC  EQU   LP_BLKS+((B#BUYREC-1)*L'LP_BLKS),,C'A'                           
                                                                                
B#B8IO6  EQU   8                   IO6 -                                        
B#REVREC EQU   8                   - REVISION/AVAIL RECORDS                     
AREVREC  EQU   LP_BLKS+((B#REVREC-1)*L'LP_BLKS),,C'A'                           
B#CKSMREC EQU  8                   - CHECKSUM RECORD                            
ACKSMREC EQU   LP_BLKS+((B#CKSMREC-1)*L'LP_BLKS),,C'A'                          
                                                                                
B#B9IO7  EQU   9                   IO7 -                                        
B#SDFREC EQU   9                   - SPILL DEFINITION RECORD                    
ASDFREC  EQU   LP_BLKS+((B#SDFREC-1)*L'LP_BLKS),,C'A'                           
B#PWREC  EQU   9                   - PROFIT WITHIN RECORD                       
APWREC   EQU   LP_BLKS+((B#PWREC-1)*L'LP_BLKS),,C'A'                            
B#B10IO8 EQU   10                  IO8 -                                        
                                                                                
B#SVRDEF EQU   12                  SPLNK2B                                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS),,C'A'                           
B#LP_D   EQU   14                  LP_D                                         
*                                                                               
         EJECT                                                                  
MGTOKEND DSECT                                                                  
MGSTATN  DS    XL3                                                              
MGTOKEN  DS    CL2                                                              
MGCDALPH DS    CL2                                                              
MGCDBIN  DS    XL1                                                              
MGTKNLNQ EQU   *-MGTOKEND                                                       
*                                                                               
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPGENDREV                                                      
       ++INCLUDE SPGENDRFLT                                                     
       ++INCLUDE SPMGADN                                                        
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRBTC                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPDEMEXTD                                                      
DEMEXTL  EQU   DXPBDEL+L'DXPBDEL-DEMEXTD                                        
*                                                                               
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE SPBUYVALD                                                      
       ++INCLUDE SPAUTHD                                                        
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
WORKD    DSECT                                                                  
         ORG   OVERWORK            REDEFINE 1K OVERLAY WORKING STORAGE          
*                                                                               
C2LKTABD DSECT                          0 INTO TABLE = Y MEANS INIT'D           
C2LKNTRY DS    255XL1                   BPRD = C'Y' MEANS LOCKED                
C2LKTABQ EQU   *-C2LKTABD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPLNK1B   02/23/21'                                      
         END                                                                    
