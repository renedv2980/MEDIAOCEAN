*          DATA SET SPLNK23    AT LEVEL 125 AS OF 09/12/17                      
*PHASE T21E23A                                                                  
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* HWON  12SEP17 124 SPEC-13049-RELINK FOR LARGER IOAREAS                        
* HWON  22JUL15 123 PASS CPROF+0 TO BUYVAL TO VALIDATE BDMASPRD                 
*               123 FIX ISSUE WHERE BDMASPRD HAD UNOPENED BRANDS                
* HWON  18JUN15 122 FIX BAD INSTRUCTIONS                                        
* HWON  30SEP14 121 COST2 SUPPORT FOR MEDIA T                                   
* HWON  08AUG14 120 SUPPORT NEW SPBLDMGN/SPMGADN                                
*               120 SUPPORT 240 MAKEGOODS                                       
*               120 FIX TRAP CODE BELOW                                         
* HWON  03JUL13 118 ADD TRAP TO FIND OUT WHY BDPROGRM/BUYID IS CLEARED          
* HWON  25FEB13 116 FIX CSECT NAME & ALLOW FOR MANUAL TESTING                   
* WHOA  27JUL12 109-115  EST LOCK HAD TO CHECK THE END DATE                     
* HWON  25MAY12 108 ALWAYS CLEAR BDCRNEGQ IN BDCIND2, NOT USED                  
* WHOA          108 ESTIMATE LOCK/UNLOCK REQUEST, USED FOR PINERGY              
* HWON  24JUL11 106 2 BYTE BUYLINE SUPPORT                                      
* HWON  24MAY11 105 CHECK TOTAL LOCAL COST MATCHES NETWORK                      
* HWON  27APR11 104 FIX PAIDCHECK SUM TO IGNORE NETWORK CUTIN FLAG              
* HWON  07FEB11 103 SEND RECORD OVERFLOW ERROR ONCE PER BUYLINE                 
* HWON  08DEC10 102 FIX DUMP WHEN SENDING CKSM VALIDATION ERROR                 
* WHOA  29NOV10 101 STATION VENDOR LOCK                                         
* HWON  13OCT10 101 AND ANOTHER FIX FOR FIND DELETED BUYLINES                   
* HWON  29SEP10 100 RETURN RECORD OVERFLOW ERROR                                
* HWON  23SEP10 099 YES ANOTHER FIX FOR FIND DELETED/HOLES                      
*               099 CALCULATE PST, GST AND TAX FOR PAID CHECKSUMS               
*               099 PC NOW SETS BDCIND2, SUPPORT LOCAL COST IN DOLLARS          
* WHOA  02SEP10 098 ANOTHER FIX FOR FIND DELETED/HOLES                          
* WHOA  12AUG10 097 FIX FOR FIND DELETED/HOLES                                  
* WHOA  07JUL10 096 WHEN BUYLINE 255 USED, FIND DELETED/HOLES                   
* HWON  19MAY10 095 FIX ISSUE WITH SPOT ALLOCATIONS                             
* HWON  15APR10 094 FIX ISSUE WITH DELETING SPILL FOR NETWORK BUYS              
* HWON  29MAR10 092 SEND ERROR IF MORE THAN 255 SPOT ALLOCATIONS                
* HWON  29MAR10 092 SEND ERROR IF MORE THAN 255 SPOT ALLOCATIONS                
* AKAT  11JAN10 091 POPULATE XPRDLIST ON ADD IF PRDS ARE ALLOCATED              
* EJOR  01DEC09 090 DON'T ADD SPILL ELEM IF SPILL MKT = HOME MKT                
* EJOR  20FEB09 085 ADD NEW FIELDS TO PAID CHECKSUM                             
* MHER  15JUL08 081 ADD 68 ELEMENTS AFTER PREVIOUS 68 ELEMS                     
* EJOR  25APR08 079 REALLY CALL SPBUYVAL BEFORE ADDS                            
*               --- ADD X'9B' DESKTOP TRANSFER ELEM                             
* EJOR  20FEB08 078 CALL SPBUYVAL BEFORE ADDING/CHANGING BUYS                   
*                                                                               
*=====================================================================*         
SPLNK23  TITLE 'CANADIAN SPOT BUY UPLOAD'                                       
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=CODE,REQUEST=*,SYSTEM=SPTSYSQ,LINKIO=Y,     X        
               WORKERKEY=SPCB,RLEN=512,                                X        
               BLOCKS=(B#SAVED,SAVED,B#WORKD,WORKD,B#BDEL,$BDD)                 
*                                                                               
SE#MAXLN EQU   1276                                                             
SE#CKSUM EQU   1277                                                             
SE#STLCK EQU   1372                STATION IS LOCKED                            
*                                                                               
CODE     NMOD1 0,**SL23**,RR=RE                                                 
         USING LP_D,R1                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
*                                                                               
         MVC   MAPNUM,LP_QMAPN     EXTRACT MAP NUMBER                           
         MVC   AGY,LP_AGY          EXTRACT AGENCY                               
         MVC   VERSION,LP_VRSN     EXTRACT PC VERSION                           
*                                                                               
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         B     EXITY                                                            
*                                                                               
INIT     CLC   STAMP,STAMPLIT      TEST CORRECT SAVE STORAGE STAMP              
         BE    INIT02                                                           
         MVC   DUB(4),SVMED        SAVE MEDIA/CLIENT ALPHA                      
         LA    R0,SAVED            NO - CLEAR SAVED STORAGE                     
         LHI   R1,SAVEDX-SAVED                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVMED(4),DUB        RESTORE MEDIA/CLIENT ALPHA                   
         MVC   STAMP,STAMPLIT                                                   
         L     R1,ALP              RESTORE A(LP_D)                              
*                                                                               
INIT02   MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   RECUP,CRECUP-COMFACSD(RF)                                        
         XC    SVMGTKNS(SVMGTKNX-SVMGTKNS),SVMGTKNS                             
         MVI   SVMGCDTF,L'SVMGCDTB/256                                          
                                                                                
         MVI   ERRORFLG,0                                                       
                                                                                
B#BDEL   EQU   3                                                                
         LA    R0,QBDELEM                                                       
         ST    R0,LP_BLKS+8        USE BLOCK 3 FOR BUY DETAILS                  
*                                                                               
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         L     R1,ALP                                                           
         MVC   GBYAGY,LP_AGY-LP_D(R1)                                           
         MVC   GBYCOMF,ACOMFACS                                                 
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         MVC   SAVE1OR2,GBY1OR2                                                 
         B     EXITY                                                            
         DROP  R1,R6,R7                                                         
         EJECT                                                                  
*                                                                               
         USING $BDELEM,QBDELEM     BUY DESCRIPTION DATA                         
*==========================================================                     
* PROCESS INPUT OF AN UPLOAD RECORD                                             
*==========================================================                     
INPUT    LA    RE,RECTAB                                                        
         LHI   R0,RECTABN                                                       
*                                                                               
INPUT10  CLC   0(2,RE),MAPNUM     LOOK UP RECORD MAP CODE IN TABLE              
         BE    INPUT20                                                          
         AHI   RE,L'RECTAB                                                      
         BCT   R0,INPUT10                                                       
         DC    H'0'                                                             
*                                                                               
INPUT20  DS    0H                                                               
         CLI   5(RE),X'FF'          CLEAR ALL ERRORS?                           
         BE    INPUT30               YES                                        
*                                                                               
         TM    5(RE),X'80'          CLEAR LOW LEVEL ERRORS?                     
         BZ    INPUT40                                                          
         CLI   ERRORFLG,FATALERR    FATAL ERROR, DON'T CLEAR                    
         BE    INPUT40                                                          
INPUT30  MVI   ERRORFLG,0                                                       
******   MVI   SVACTION,0                                                       
*                                                                               
INPUT40  CLI   ERRORFLG,0          HAVE ANY ERRORS?                             
         BNE   EXITY               YES                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,2(RE)                                                       
         A     R0,SRVRRELO                                                      
         ST    R0,RECADDR          SET A(PROCESSING ROUTINE)                    
*                                                                               
         MVC   XTRATEXT,SPACES     INITIALIZE EXTRA MESSAGE TEXT                
         GOTOR RECADDR                                                          
         B     EXITY                                                            
         DROP  RB                                                               
         EJECT                                                                  
*================================================================               
* ROUTINE TO VALIDATE AND BUILD BUY RECORD                                      
*================================================================               
                                                                                
VALBHD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TTLCLCST,TTLCLCST   CLEAR TOTAL LOCAL COST                       
         XC    IOBRDLST,IOBRDLST                                                
         XC    XPRDLIST,XPRDLIST   CLEAR EXISTING PRODUCT LIST                  
         XC    QACOMM1(20),QACOMM1 CLEAR COMMENT ADDRESSES                      
         XC    CKSUMBFR,CKSUMBFR                                                
         MVI   QENDBUY,C'N'        SET NETWORK BUY NOT ENDED                    
         MVI   Q68COUNT,0          RESET 68 ELEM COUNTER                        
*                                                                               
         CLI   QACTION,0           TEST ACTION GIVEN                            
         BE    *+10                                                             
         MVC   SVACTION,QACTION                                                 
*                                                                               
         CLI   SVACTION,0          TEST ACTION KNOWN                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QMEDX,0                                                          
         BE    VALBH010                                                         
         MVC   SVBAGYMD,QMEDX                                                   
         MVC   SVMED,QMEDA                                                      
*                                                                               
VALBH010 OC    QCLTX,QCLTX                                                      
         BZ    *+10                                                             
         MVC   SVBCLT,QCLTX                                                     
*                                                                               
         MVC   SV1OR2,SAVE1OR2                                                  
         CLC   QCLTA,=C'PG0'                                                    
         JE    VALBH015                                                         
         CLC   QCLTA,=C'TBL'                                                    
         JNE   VALBH020                                                         
VALBH015 CLC   AGY,=C'T1'                                                       
         JE    *+14                                                             
         CLC   AGY,=C'SJ'                                                       
         JNE   VALBH020                                                         
         MVI   SV1OR2,2                                                         
*                                                                               
VALBH020 CLI   SVBAGYMD,0          TEST MEDIA KNOWN                             
         BE    VALERR02                                                         
         OC    SVBCLT,SVBCLT       TEST CLIENT KNOWN                            
         BZ    VALERR04                                                         
*                                                                               
         L     R1,ACLTREC          TEST CLIENT RECORD IS AROUND                 
         CLC   SVBAGYMD(3),CKEYAM-CLTHDR(R1)                                    
         BE    VALBH030                                                         
*                                                                               
         MVC   QMEDX,SVBAGYMD      NO - GET CLIENT RECORD                       
         MVC   QCLTX,SVBCLT                                                     
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   VALERR03                                                         
*                                                                               
NEW      USING BUYKEY,SVKEY        BUILD NEW BUY KEY FROM INPUT DATA            
*                                                                               
VALBH030 XC    NEW.BUYKEY,NEW.BUYKEY                                            
*                                                                               
         MVC   NEW.BUYKAM,SVBAGYMD                                              
         MVC   NEW.BUYKCLT,SVBCLT                                               
         MVI   NEW.BUYKPRD,X'FF'   SET PRD=POL                                  
*                                                                               
         CLI   SVMED,C'N'          TEST CANADIAN NETWORK MEDIA                  
         BE    VALBH040            YES - NETWORK BUYS HAVE NO MARKET            
         OC    QMKT,QMKT           TEST MARKET GIVEN                            
         BNZ   *+10                                                             
         MVC   QMKT,SVBMKT                                                      
*                                                                               
         MVC   SVBMKT,QMKT                                                      
         MVC   NEW.BUYKMKT,QMKT                                                 
         OC    NEW.BUYKMKT,NEW.BUYKMKT                                          
         BZ    VALERR05                                                         
*                                                                               
VALBH040 OC    QSTA,QSTA           TEST STATION GIVEN                           
         BZ    VALBH050                                                         
*                                                                               
         MVC   SVSTA,QSTA                                                       
         CLI   SVMED,C'N'          TEST CANADIAN NETWORK                        
         BNE   *+14                                                             
         MVI   QSTA+4,C'N'                                                      
         MVC   SVNETWK,QSTA        SAVE NETWORK CALL LETTERS                    
*                            ALSO WANT SFLAG1 FROM STATION REC                  
         MVI   SVSFLAG1,0                                                       
         L     R1,ALP                                                           
         MVI   LP_VPARM-LP_D(R1),$VALFLG1                                       
         GOTOR (#VALSTA,AVALSTA),DMCB,SVSTA,L'SVSTA,WORK                        
         BNE   VALERR06                                                         
         MVC   SVBSTA,STAPSTA-STAPACKD+WORK                                     
         MVC   SVSFLAG1,WORK+STAPACKL                                           
*                                                                               
VALBH050 DS    0H                                                               
         CLI   SVACTION,QACTADD     ADDING A NEW RECORD?                        
         BNE   VALBH060             NO                                          
         TM    SVSFLAG1,SLOCK       X'04' - STATION LOCKED?                     
         BNZ   VALERR13                                                         
VALBH060 MVC   NEW.BUYKSTA,SVBSTA                                               
         OC    NEW.BUYKSTA,NEW.BUYKSTA                                          
         BZ    VALERR07                                                         
*                                                                               
         CLI   SVMED,C'N'                                                       
         BNE   *+8                                                              
         BRAS  RE,GETNETBT          INTO SVNETBTS                               
*                                                                               
         CLI   QBEST,0             TEST ESTIMATE NUMBER GIVEN                   
         BE    *+10                                                             
         MVC   SVBEST,QBEST                                                     
*                                                                               
         MVC   NEW.BUYKEST,SVBEST                                               
         CLI   NEW.BUYKEST,0                                                    
         BE    VALERR08                                                         
*                                                                               
         BRAS  RE,TSTLOCK          TEST SOON LOCKS                              
         BNE   VALERR21                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLIN           TEST LINE NUMBER                             
         BNZ   VALBH070                                                         
         CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         BE    VALBH070                                                         
         DC    H'0'                NO - LINE NUMBER MUST BE GIVEN               
*                                                                               
VALBH070 STCM  R0,3,SVBLIN         SAVE LINE NUMBER                             
         MVC   NEW.BUYKBUY+1(2),SVBLIN                                          
***TBL   MVI   NEW.BUYKBUY+2,1     ALWAYS A 1 FOR 1 BYTE LINE NUMBERS           
*                                                                               
         CLI   QACTION,C'A'        TEST ADDING A NEW RECORD                     
         BNE   VALBH120                                                         
*                                                                               
         MVC   IOKEY(L'BUYKEY),SVKEY                                            
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+IOBUY'                       
*                                                                               
VALBH080 CLC   IOKEY(11),IOKEYSAV  A-M/CLT/PRD/MKT/STA/EST                      
         BNE   VALBH090                                                         
         MVC   IOKEYSAV,IOKEY      SAVE LAST KEY READ                           
         MVI   GBYACT,GBYSEQ                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IOSPTDIR+IOBUY'                       
         B     VALBH080                                                         
*                                                                               
VALBH090 MVC   SVKEY,IOKEYSAV      RESTORE LAST KEY FOUND                       
         CLI   SV1OR2,2                                                         
         BE    *+14                                                             
         CLC   SVKEY+11(2),=X'00FF'  255 MAX 1-BYTE BUYLINES REACHED?           
         B     *+10                                                             
         CLC   SVKEY+11(2),=X'01F3'  499 MAX 1-BYTE BUYLINES REACHED?           
         BL    VALBH100                                                         
         BRAS  RE,FINDHOLE         LOOK FOR OPEN/DELETED LINES                  
         BE    VALBH110                                                         
         B     MAXLNERR                                                         
*                                                                               
VALBH100 SR    R0,R0                                                            
         ICM   R0,3,SVKEY+11                                                    
         AHI   R0,1                                                             
         STCM  R0,3,SVKEY+11                                                    
*                                                                               
VALBH110 L     R0,AIO3             INITIALIZE NEW BUY RECORD                    
         LHI   R1,IO3LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,AIOBUY                                                        
         USING BUYRECD,RF                                                       
         MVC   BUYKEY(13),SVKEY                                                 
         MVC   BUYKBUY(2),SVKEY+11                                              
***TBL   MVI   BUYKBUY+1,1         ALWAYS A 1                                   
         MVI   BUYKBUY+2,0                                                      
*                                                                               
         LHI   R0,BDELEMX-BUYRECD                                               
         STCM  R0,3,BUYRLEN                                                     
         MVC   BUYALPHA,AGY                                                     
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,BDELEMX-BDELEM                                             
         B     EXITY                                                            
         DROP  RF                                                               
*                                                                               
VALBH120 MVC   IOKEY(L'BUYKEY),SVKEY                                            
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOSPTDIR+IOBUY'                        
         BNE   VALERR10                                                         
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IOBUY'                      
         BNE   VALERR11                                                         
* COMPUTE CHECKSUM FOR RECORD                                                   
         L     RE,AIOBUY                                                        
         SR    RF,RF                                                            
         ICM   RF,3,BUYRLEN-BUYREC(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,FULL                                                       
         CLC   QCHKSUM,=F'1'                                                    
         BE    VALBH121                                                         
         CLC   FULL,QCHKSUM                                                     
         JNE   VALERR15                                                         
*                                                                               
VALBH121 BRAS  RE,PDCHKSUM         GET CHKSUM OF PAID SPOTS                     
         MVC   CKSUMBFR,FULL       SAVE CHKSUM BEFORE CHANGES                   
*                                                                               
         BRAS  RE,BLDXPRD          BUILD LIST OF EXISTING PRODUCTS              
*                                                                               
         CLI   SVACTION,C'D'       TEST DELETE                                  
         JNE   EXITY                                                            
*                                                                               
         OI    IOKEY+13,X'80'      SET DELETED FLAG                             
         MVI   GBYACT,GBYWRT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOBUY                                                        
         OI    15(R3),X'80'        SET RECORD DELETED                           
         MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOSPTFIL+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* SEND DISK ADDRESS OF DELETED BUY                                              
                                                                                
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#UPLRSP)               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',1),            X        
               ('LD_HEXDQ',IODA),(4,0)                                          
*                                                                               
DELBUY2  CLI   SVMED,C'N'                                                       
         BNE   EXITY                                                            
*                                                                               
         L     RE,AIOBUY           SAVE THE NETWORK BUY                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIOBUYSV                                                      
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         AHI   R1,2                SET FOR 2X'00' FOLLOWING                     
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AIOBUYSV                                                      
         AHI   R3,24                                                            
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
*                                                                               
         USING NTWKELEM,R3                                                      
*                                                                               
DELBUY4  BRAS  RE,NEXTEL                                                        
         BNE   EXITY                                                            
*                                                                               
         MVC   IOKEY+4(5),NTWKMKST SET MKT AND STATION IN IOKEY                 
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOSPTDIR+IOBUY'                        
         BNE   DELBUY4             IGNORE MISSING EXPLODED BUYS!                
*                                                                               
         MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IOBUY'                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    IOKEY+13,X'80'      SET DELETED FLAG                             
         MVI   GBYACT,GBYWRT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIOBUY                                                        
         OI    15(RE),X'80'        SET RECORD DELETED                           
         MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOSPTFIL+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DELBUY4                                                          
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
         MVC   NEXT.BUYKBUY+1(2),=X'0001'  START READING WITH LINE 1            
         MVI   GBYACT,GBYHIGH                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+IOBUY'                       
*                                                                               
FH10     CLC   IOKEY(10),IOKEYSAV  A-M/CLT/PRD/MKT/STA/EST                      
         JNE   FH60                                                             
         CLC   MYHALF,NEXT.BUYKBUY+1   IS NEXT BUY 1 NUMBER MORE?               
         BE    FH30                    YES - NO UNUSED HERE!                    
         MVC   SVKEY,IOKEYSAV          NO- THEN MYHALF HAS UNUSED LINE          
         MVC   SVKEY+11(2),MYHALF      (I DON'T THINK I NEED THIS)              
         J     FHXEQ                                                            
*                                                                               
FH30     TM    NEXT.BUYKCNTL,BUYRDEL IS THE LINE WE FOUND DELETED?              
         BZ    FH40                   NO                                        
         OC    HALF,HALF             ALREADY FOUND A DELETED LINE?              
         BNZ   FH40                   YES                                       
         MVC   HALF,NEXT.BUYKBUY+1    SAVE 1ST DELETED LINE                     
*                                                                               
FH40     SR    RE,RE                                                            
         ICM   RE,3,MYHALF         NEXT LINE WE EXPECT TO GET                   
         AHI   RE,1                                                             
         STCM  RE,3,MYHALF                                                      
         MVC   IOKEYSAV,IOKEY      SAVE LAST KEY READ                           
         MVI   GBYACT,GBYSEQ                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IOSPTDIR+IOBUY'                       
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
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOSPTDIR+IOBUY'                       
         MVC   SVKEY,IOKEYSAV     WE DETERMINED THAT THIS IS THE LINE           
                                                                                
FH70     MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IORDEL+IOSPTFIL+IOBUY'               
*                                                                               
         BRAS  RE,BLDXPRD          BUILD LIST OF EXISTING PRODUCTS              
*                                                                               
         L     R0,AIOBUY           INITIALIZE NEW BUY RECORD                    
         LHI   R1,IO3LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,AIOBUY                                                        
         USING BUYRECD,RF                                                       
         MVC   BUYKEY(13),SVKEY                                                 
         MVC   BUYKBUY(2),SVKEY+11                                              
***TBL   MVI   BUYKBUY+1,1         ALWAYS A 1                                   
         MVI   BUYKBUY+2,0                                                      
*                                                                               
         LHI   R0,BDELEMX-BUYRECD                                               
         STCM  R0,3,BUYRLEN                                                     
         MVC   BUYALPHA,AGY                                                     
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,BDELEMX-BDELEM                                             
         DROP  RF                                                               
*                                                                               
* COMMENTING THIS CODE OUT SO THAT WE DON'T HAVE BAD BUYS IN RECOVERY           
*&&DO                                                                           
* OVERWRITE EXISTING DELETED BUY                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOSPTFIL+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
* COMMENTING THIS CODE OUT SO THAT WE DON'T HAVE BAD BUYS IN RECOVERY           
*                                                                               
         NI    IOKEY+13,X'FF'-X'80'   UNSET DELETED FLAG                        
         MVI   GBYACT,GBYWRT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   QACTION,QACTOVER    MAKE SURE ENDBUY DOESN'T TRY TO ADD!         
FHXEQ    J     EXITY                                                            
FHXNEQ   J     EXITN                                                            
*                                                                               
         EJECT                                                                  
*==================================================================             
* GET NETWORK BITS FROM PASSIVE NETWORK POINTERS                                
*==================================================================             
                                                                                
GETNETBT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVNETBTS,0                                                       
*                                                                               
K        USING NWKPASS,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   K.NWKPTYP,X'0D'                                                  
         MVI   K.NWKPSUB,X'91'                                                  
         MVC   K.NWKPAGY,AGY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IOBUY'                          
         B     GETNET4                                                          
*                                                                               
GETNET2  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IOBUY'                          
*                                                                               
GETNET4  CLC   IOKEY(4),IOKEYSAV                                                
         BE    *+6                                                              
         DC    H'0'                OUGHT TO FIND NETWORK                        
*                                                                               
         CLC   K.NWKPNET(4),SVSTA                                               
         BNE   GETNET2                                                          
         MVC   SVNETBTS,K.NWKPSEQ  SAVE NETWORK SEQNUM                          
         J     EXIT                                                             
         DROP  K                                                                
         EJECT                                                                  
*====================================================================           
* VALIDATE LOCAL STATION DATA FOR ALL ADDS AND CHANGES                          
* QENDBUY IS SET TO Y AFTER THE NETWORK BUY PROCESSING HAS ENDED                
*                                                                               
* 268 ELS ARE SENT 2X - ONCE FOR EACH STATION IN THE NETWORK BUY                
* AND THEN ONCE FOR EACH LOCAL BUY ON THE NETWORK                               
*                                                                               
* THE NETWORK BUY IS SAVED WHEN THE FIRST 268 ELEMENT IS RECEIVED               
* SO THAT IT CONTAINS ALL DATA EXCEPT THE X'68' ELEMENTS                        
* ON BOTH ADDS AND CHANGES                                                      
*====================================================================           
                                                                                
VALNETST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    QSTA,QSTA           TEST NO STATION THIS ELEMENT                 
         BNZ   *+10                                                             
         MVC   QSTA,SVSTA          THEN USE CABLE NET AS STA                    
*                                                                               
         BAS   RE,GET68STA         GET PACKED MKT/STA FOR 68 ELEM               
*                                                                               
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   QENDBUY,C'Y'        IF NETWORK BUY HAS ENDED                     
         BE    VALNET10            THIS EL IS A LOCAL BUY                       
*                                                                               
         CLI   Q68COUNT,0          TEST ANY 68 ELS ADDED YET                    
         BNE   VALNET2             YES                                          
*                                                                               
         L     RE,AIOBUY           SAVE THE NETWORK BUY                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIOBUYSV                                                      
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         AHI   R1,2                SET FOR 2X'00' FOLLOWING                     
         MVCL  R0,RE                                                            
*                                                                               
E        USING NTWKELEM,ELEM                                                    
*                                                                               
VALNET2  XC    ELEM,ELEM           CREATE 680C ELEM FOR THIS STATION            
         MVI   ELEM,X'68'                                                       
         MVI   ELEM+1,12                                                        
         MVC   E.NTWKMKST,SVMKTSTA                                              
         MVC   E.NTWKSHR,BUPCT                                                  
         MVC   E.NTWKFLG,BUFLAGS                                                
*                                                                               
         CLI   SVACTION,C'A'       TEST ADD                                     
         BE    VALNET6                                                          
*                                                                               
         MVI   ELCDLO,X'68'        REMOVE PREV 68 ELEM ON CHANGE                
         MVI   ELCDHI,X'68'                                                     
         LA    R3,BDELEM                                                        
*                                                                               
VALNET4  BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                68 EL MUST BE IN RECORD !                    
*                                                                               
         USING NTWKELEM,R3                                                      
         CLC   NTWKMKST,E.NTWKMKST                                              
         BNE   VALNET4                                                          
         BRAS  RE,DELEL                                                         
         DROP  E,R3                                                             
*                                                                               
VALNET6  MVI   ELCDLO,X'68'        ADD AFTER LAST 68 EL OR AT EOR               
         MVI   ELCDHI,X'68'                                                     
         LA    R3,BDELEM                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    VALNET8             GOT ONE                                      
         BRAS  RE,ADDEL            NO 68 - ADD AT EOR                           
         B     VALNET8X                                                         
*                                                                               
VALNET8  LR    R0,R3               SAVE LAST 68 ELEM ADDRESS                    
         BRAS  RE,NEXTEL                                                        
         BE    VALNET8                                                          
         LR    R3,R0               POINT TO LAST 68 ELEM                        
         BRAS  RE,BUMPADD                                                       
*                                                                               
VALNET8X IC    R0,Q68COUNT                                                      
         AHI   R0,1                                                             
         STC   R0,Q68COUNT                                                      
         J     VALNETX                                                          
         EJECT                                                                  
*=================================================================              
* PROCESS LOCAL BUYS.                                                           
* FOR ADDS, ALWAYS INSERT 68 ELEMENT TO POINT BACK TO NETWORK.                  
* FOR CHANGES, READ LOCAL BUY                                                   
*=================================================================              
                                                                                
VALNET10 XC    XPRDLIST,XPRDLIST   CLEAR EXISTING PRODUCT LIST                  
         XC    IOBRDLST,IOBRDLST   CLEAR EXISTING PRODUCT LIST                  
         MVI   BUGST,0                                                          
         XC    BUPST,BUPST                                                      
*                                                                               
         CLI   QACTION,C'A'        TEST ADD                                     
         BNE   VALNET20            NO                                           
         BRAS  RE,CPNTWKBY                                                      
         J     VALNETX                                                          
*=================================================================              
* FOR CHANGES NEED TO READ LOCAL BUY                                            
*   (FOR OVERWRITE, COPY NETWORK OVER WHAT WAS READ)                            
*=================================================================              
                                                                                
VALNET20 MVC   IOKEY(L'BUYKEY),SVKEY                MOVE KEY                    
         MVC   IOKEY+(BUYKMSTA-BUYKEY)(5),SVMKTSTA  MOVE NEW MKTSTA             
*                                                                               
         NI    MISCFLG1,X'FF'-LOCLRNF                                           
         MVI   GBYACT,GBYREAD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOSPTDIR+IOBUY' LOCAL BUY             
         BE    VALNET30                                                         
         CLI   IOERR,IOERNF                                                     
         BNE   VALNET22                                                         
         OI    MISCFLG1,LOCLRNF                                                 
         CLI   QACTION,QACTOVER                                                 
         BE    VALNET45                                                         
*                                                                               
VALNET22 CLI   IOERR,IOEDEL        KEY IS DELETED?                              
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLI   QACTION,QACTOVER    ARE WE OVERWRITING A DELETED?                
         BNE   VALNET30            NO, DON'T CARE, WILL OVERWRITE               
         NI    IOKEY+BUYKCNTL-BUYKEY,X'FF'-X'80'                                
         MVI   GBYACT,GBYWRT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOSPTDIR+IOBUY'                         
         BE    VALNET30            YES, UNDELETE THE KEY                        
         DC    H'0'                                                             
*                                                                               
VALNET30 MVI   GBYACT,GBYGET                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IORDEL+IOSPTFIL+IOBUY'               
         BE    VALNET40                                                         
         CLI   IOERR,IOEDEL        RECORD IS DELETED?                           
         BE    VALNET40            YES, DON'T CARE, WILL OVERWRITE              
         DC    H'0'                                                             
*                                                                               
VALNET40 CLI   QACTION,QACTOVER    OVERWRITE DUE TO DELETED RECORD?             
         BNE   VALNET50            YES, DONE GETTING THE RECORD                 
VALNET45 BRAS  RE,CPNTWKBY                                                      
         B     VALNETX                                                          
*                                                                               
VALNET50 BRAS  RE,PDCHKSUM         GET CHKSUM OF PAID SPOTS                     
         MVC   CKSUMBFR,FULL       SAVE CHKSUM BEFORE CHANGES                   
*                                                                               
         BRAS  RE,BLDXPRD          BUILD EXISTING PRODUCT LIST                  
                                                                                
VALNETX  J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* COPY NETBUY TO IOBUY (NEEDED FOR ADD AND OVERWRITE)                           
**********************************************************************          
CPNTWKBY NTR1                                                                   
         L     RE,AIOBUYSV         FOR ADD - COPY NETBUY TO IOBUY               
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,AIOBUY                                                        
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         AHI   R1,2                SET FOR 2X'00' FOLLOWING                     
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AIOBUY           BUY RECORD                                   
         AHI   R3,24               POINT TO FIRST ELEMENT                       
*                                                                               
CNET10   CLI   0(R3),0             END OF BUY REC?                              
         BE    CNET20              YES                                          
         CLI   0(R3),X'0B'         X'0B' ELEMENT?                               
         BE    *+12                YES                                          
         CLI   0(R3),X'0C'         X'0C' ELEMENT?                               
         BNE   CNET15              NO - GET NEXT ELEMENT                        
*                                                                               
         CLI   1(R3),14            HAVE BRD PRD?                                
         BL    CNET20              NOPE - NO BRANDS FOR THIS BUY                
         LA    R1,10(R3)           POINT TO PRD                                 
         BRAS  RE,ADDPRD           ADD PRD TO XPRDLIST                          
*                                                                               
         CLI   1(R3),18            HAVE PIGGY PRD?                              
         BL    CNET15              NOPE                                         
         LA    R1,14(R3)           POINT TO PIGGY PRD                           
         BRAS  RE,ADDPRD           ADD PIGGY PRD TO XPRDLIST                    
*                                                                               
CNET15   LLC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     CNET10                                                           
*                                                                               
CNET20   MVC   BUYKMSTA,SVMKTSTA   SET CORRECT MKT/STATION                      
*                                                                               
         XC    ELEM,ELEM           CREATE 6806 EL TO POINT TO NTWK              
         MVI   ELEM,X'68'          NOTE 68 ELS NOT IN SAVED BUY                 
         MVI   ELEM+1,6                                                         
         MVC   ELEM+2(4),SVNETWK                                                
*                                                                               
         L     R3,AIOBUY           POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
*                                                                               
         MVC   BDCOST,BUCOST+1     SET COST                                     
         OC    BUTIMST(4),BUTIMST  TEST TIME OVERRIDE                           
         BZ    *+10                NO                                           
         MVC   BDTIMST(4),BUTIMST  SET LOCAL BUY TIME                           
*                                                                               
         CLI   BUCIND2,0                                                        
         BZ    *+10                                                             
         MVC   BDCIND2,BUCIND2                                                  
*                                                                               
         XR    R0,R0                                                            
         ICM   R1,15,BUCOST                                                     
         TM    BUCIND2,BDCNBRDQ                                                 
         BZ    *+8                                                              
         M     R0,=F'100'                                                       
         A     R1,TTLCLCST                                                      
         STCM  R1,15,TTLCLCST                                                   
                                                                                
         J     EXIT                                                             
*=================================================================              
* GET PACKED MKT/STATION FOR NETWORK 68 ELEMENT                                 
*=================================================================              
                                                                                
GET68STA NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGY                                                      
         MVI   STAPCTRY,C'C'        THE AGENCY PROFILE IS LONG GONE!            
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'    USE ZEROS, QMKT IS BINARY !                 
         MVC   STAPQSTA(5),QSTA                                                 
*                                                                               
         OC    BUPROVCD,BUPROVCD    TEST CABLE                                  
         BZ    *+10                                                             
         MVC   STAPQNET,BUPROVCD                                                
*                                                                               
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVMKTSTA,STAPMKST                                                
         MVC   SVMKTSTA(2),QMKT     SET MARKET NUMBER                           
         OC    BUPROVCD,BUPROVCD    TEST CABLE                                  
         BNZ   *+10                 YES - LEAVE PROVINCE BITS                   
         MVC   SVMKTSTA+4(1),SVNETBTS                                           
         J     EXIT                                                             
         EJECT                                                                  
*================================================                               
* BUY DESCRIPTION DATA                                                          
*================================================                               
                                                                                
VALBDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
*                                                                               
         MVC   SVPROGRM,BDPROGRM                                                
*                                                                               
         LHI   RE,BDELEMX-BDELEM                                                
         AHI   RE,-3               -ELCODE/LEN                                  
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   BDELEM+2(0),QBDELEM                                              
*                                                                               
         LA    R3,SVPROGRM                                                      
         BRAS  RE,CHKALNM          OLD BUY HAVE PROGRAM NAME OR LIVE?           
         JNE   VALBD005             NO, SKIP IT                                 
*                                                                               
         LA    R3,BDPROGRM                                                      
         BRAS  RE,CHKALNM          NEW BUY HAVE PROGRAM NAME OR LIVE?           
         JE    VALBD005             YES, SKIP IT                                
         LLC   R0,SVPRGNM                                                       
         AHI   R0,1                                                             
         STC   R0,SVPRGNM          SAVE # OF OCCURANCES                         
         J     VALBD005                                                         
*                                                                               
CHKALNM  LA    RF,ALPHANUM         RF-->TABLE OF VALID ALPHANUMERICS            
CKAN010  CLI   0(RF),X'FF'          IF AT END OF THIS TABLE,                    
         JE    CKANNO               THEN INPUT NOT ALPHANUMERIC                 
         CLC   0(1,R3),0(RF)       IF MATCH FOUND                               
         BER   RE                                                               
         LA    RF,1(RF)             ELSE, KEEP LOOKING FOR MATCH                
         J     CKAN010                                                          
CKANNO   CLI   0(RF),0                                                          
         BR    RE                                                               
*                                                                               
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=',X'FF'                   
*                                                                               
*====================================================================           
* NOTE DO NOT SET BDCIND2 X'10' FOR NETWORK, BECAUSE RATE IS ASSUMED            
* TO BE IN DOLLARS. BUT IT MUST BE SET FOR SELECT TV.                           
*====================================================================           
                                                                                
VALBD005 MVC   BDCOST,$BDCOST+1    MOVE 3 BYTES OF COST                         
*                                                                               
         L     R1,ALP                                                           
         L     RF,LP_ALPXD-LP_D(R1) GET ADDR OF EXTENSION BLOCK                 
         CLC   (LP_XPINF-LP_XD)+(TXPNUM-TXPINFO)(2,RF),=X'0036'                 
         BNE   VALBD010                                                         
         CLC   VERSION,=AL1(1,3,0,29)                                           
         BNL   VALBD040                                                         
         B     VALBD030                                                         
*                                                                               
VALBD010 CLC   (LP_XPINF-LP_XD)+(TXPNUM-TXPINFO)(2,RF),=X'0039'                 
         BE    VALBD015                                                         
         DC    H'0'                NOOP THIS FOR MANUAL TESTING                 
         B     VALBD040                                                         
VALBD015 CLC   VERSION,=AL1(1,6,0,48)                                           
         BNL   VALBD040                                                         
*                                                                               
VALBD030 CLI   $BDCOST,0           TEST FITS IN 3 BYTES                         
         BE    VALBD060                                                         
         ICM   R1,15,$BDCOST                                                    
         M     R0,=F'2'                                                         
         D     R0,=F'100'          CONVERT TO PENNIES                           
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STCM  R1,7,BDCOST                                                      
         CLI   SVMED,C'N'          TEST NETWORK REQUEST                         
         BE    VALBD060            YES - DOLLARS ARE ASSUMED                    
         OI    BDCIND2,BDCNBRDQ    X'10' ELSE SET COST IN DOLLARS               
         B     VALBD060                                                         
*                                                                               
VALBD040 TM    $BDCIND2,BDCNBRDQ                                                
         BO    VALBD050                                                         
         CLI   SVMED,C'N'                                                       
         BNE   VALBD060                                                         
         TM    $BDCIND2,BDCRATPQ                                                
         BO    VALBD060                                                         
*                                                                               
VALBD050 SR    R0,R0                                                            
         ICM   R1,15,$BDCOST                                                    
         M     R0,=F'100'                                                       
         STCM  R1,15,$BDCOST                                                    
*                                                                               
VALBD060 LA    R1,BUMASPR1         POINT TO EBCDIC PRD                          
         BRAS  RE,FINDPRD                                                       
         BL    VALBD061                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDMASPRD(1),0(R1)                                                
VALBD061 DS    0H                                                               
         XC    BUMASPR1,BUMASPR1                                                
*                                                                               
VALBD070 LA    R1,BUMASPR2         POINT TO EBCDIC PRD                          
         BRAS  RE,FINDPRD                                                       
         BL    VALBD071                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDMASPRD+1(1),0(R1)                                              
VALBD071 DS    0H                                                               
         XC    BUMASPR2,BUMASPR2                                                
*                                                                               
VALBD080 CLI   BDMGDATE,C'$'       TEST MGCODE IS A TOKEN                       
         BNE   VALBD120                                                         
         LA    R4,SVMGTKNS         POINT TO LIST OF TOKENS DONE                 
         LHI   R5,(SVMGTKNX-SVMGTKNS)/L'SVMGTKNS                                
         USING MGTOKEND,R4                                                      
*                                                                               
VALBD090 CLI   MGTOKEN,C'$'        ANY MORE TOKENS                              
         BNE   VALBD100                                                         
         CLC   BDMGDATE,MGTOKEN                                                 
         BE    VALBD110                                                         
         AHI   R4,L'SVMGTKNS                                                    
         BCT   R5,VALBD090                                                      
         DC    H'0'                TOO MANY TOKENS                              
*                                                                               
VALBD100 MVC   MGTOKEN,BDMGDATE    SAVE NEW TOKEN IN TABLE                      
         BRAS  RE,GETMGCD          GET NEW ALPHA/BINARY CODES                   
*                                                                               
VALBD110 MVC   BDMGDATE,MGCDALPH   REPLACE TOKEN IN BUY RECORD                  
         DROP  R4                                                               
*                                                                               
VALBD120 MVI   BDCANAD,X'80'       YES - THIS IS THE RIGHT VALUE !              
         MVI   BDNRGN,C' '         THIS FIELD IS DECEASED                       
*==================================================================             
* CHECK FOR -S AND SET LAST BYTE OF BDPROGRM TO X'00'                           
*==================================================================             
                                                                                
         LA    RE,BDPROGRM+17                                                   
         LHI   RF,16                                                            
*                                                                               
VALBD130 CLI   0(RE),C' '                                                       
         BH    VALBD140                                                         
         BCTR  RE,0                                                             
         BCT   RF,VALBD130                                                      
         B     VALBD150                                                         
*                                                                               
VALBD140 BCTR  RE,0                BACK UP ONE MORE                             
         CLC   0(2,RE),=C'-S'                                                   
         BNE   VALBD150                                                         
         MVI   BDPROGRM+17,X'00'                                                
                                                                                
*=================================================================              
* NOW REPLACE QBDELEM SO VALUES ARE CORRECT IF IT IS USED AGAIN                 
*=================================================================              
                                                                                
VALBD150 LHI   RE,BDELEMX-BDELEM                                                
         AHI   RE,-3               -ELCODE/LEN                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   QBDELEM(0),BDELEM+2                                              
                                                                                
*==========================================================                     
* ON ADD INSERT DUMMY 02 DEMO ELEMENT SO CAN REPLACE LATER                      
*==========================================================                     
                                                                                
         LA    R3,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),2                                                          
         BE    EXIT                                                             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,2                                                           
         MVI   ELEM+1,24                                                        
         BRAS  RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================================================                               
* DEMOS AND SPILL DEMOS                                                         
*================================================                               
                                                                                
VALDEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
E        USING NDELEM,ELEM                                                      
         XC    ELEM,ELEM                                                        
         MVI   E.NDCODE,2          SET ORIGINATING DEMOS                        
         MVC   E.NDBOOK,BUBOOK                                                  
         MVC   E.NDPROG,BUPROG                                                  
*                                                                               
         BRAS  RE,BLDDEM           BUILD DEMO CODES/VALUES                      
*                                                                               
D        USING DLUELEM,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVI   D.DLUCODE,DLUCODEQ                                               
         MVI   D.DLULEN,DLULENQ                                                 
         MVC   D.DLUBKTYP,BUBKTYPE                                              
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
*                                                                               
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         BRAS  RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  D,R2                                                             
*                                                                               
VALSPL   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
         CLC   BUSPLAMK,BUYKMKTN   TEST SPILL MKT=ACTUAL MKT                    
         BE    VALSPLX              YES - SKIP IT                               
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   E.NDCODE,3          SET SPILL DEMOS                              
         MVC   E.NDBOOK,BUBOOK                                                  
*                                                                               
         MVC   E.NDAGYMKT,BUSPLAMK AGENCY MARKET CODE                           
         MVC   E.NDSTA,DEMQSTA                                                  
         MVC   E.NDMKTALF,DEMQMKT                                               
         MVC   E.NDBKTYPE,BUBKTYPE                                              
         MVI   E.NDRTGSVC,C'0'                                                  
         CLI   DEMQRSV,C'N'                                                     
         BE    *+8                                                              
         MVI   E.NDRTGSVC,C'1'                                                  
*                                                                               
         BRAS  RE,BLDDEM                                                        
*                                                                               
VALSPLX  B     EXIT                                                             
         DROP  E                                                                
         EJECT                                                                  
BLDDEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,ELEM                                                          
         USING NDELEM,R3                                                        
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,QADEMO                                                     
         BNZ   BLDDEM1             SHOULD NOT HAPPEN, EXIT OTHERWISE            
         CLI   ELEM,3              DOING SPILL?                                 
         BNE   EXIT                                                             
         CLC   VERSION,=AL1(1,3,0,5)                                            
         BL    EXITY                                                            
         CLI   BSPACT,C'D'         AND ACTION DELETE?                           
         BE    BLDDEM3                                                          
         B     EXITY                                                            
*                                                                               
         USING LW_D,R2                                                          
BLDDEM1  SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
*                                                                               
BLDDEM2  MVC   NDEMNO,0(R2)        MOVE DEMO TYPE/CODE                          
         MVI   NDSVI,100                                                        
         MVC   NDEMRAW,3(R2)                                                    
         CLC   NDEMRAW,=X'00100000' CHECK REASONABLE VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         OC    NDEMRAW(1),7(R2)    'OR' IN FLAGS                                
*                                                                               
         AHI   R2,8                NEXT ARRAY ENTRY                             
         AHI   R3,8                NEXT DEMO IN ELEMENT                         
         BCT   R0,BLDDEM2                                                       
*                                                                               
BLDDEM3  LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         AHI   R3,NDEMNO-NDELEM                                                 
         STC   R3,ELEM+1                                                        
*                                                                               
         L     R2,AIO3                                                          
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
*                                                                               
         CLI   ELEM,3              TEST DOING SPILL DEMOS                       
         BE    BLDDEM22            YES                                          
*                                                                               
BLDDEM10 SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             IF REACH EOR, INSERT NOW                     
         BE    BLDDEM12                                                         
* ORIG DEMOS                                                                    
         CLI   0(R3),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,DELEL                                                         
*                                                                               
BLDDEM12 BRAS  RE,ADDEL                                                         
         B     EXIT                                                             
                                                                                
* SPILL DEMOS                                                                   
                                                                                
         USING NDELEM,R3                                                        
BLDDEM20 CLI   0(R3),3             THIS A SPILL DEMO ELEM                       
         BNE   BLDDEM22                                                         
         CLC   NDAGYMKT,ELEM+(NDAGYMKT-NDELEM)  MATCH AGY MKT NUM               
         BNE   BLDDEM22                                                         
         BRAS  RE,DELEL                                                         
         B     BLDDEM30                                                         
*                                                                               
BLDDEM22 SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   BLDDEM20                                                         
*                                                                               
BLDDEM24 LA    R3,BDELEM           FIND INSERTION POINT                         
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
BLDDEM26 BRAS  RE,NEXTEL                                                        
         BNE   BLDDEM28                                                         
         LR    R0,R3               SAVE LAST 02/03 ADDRESS                      
         B     BLDDEM26                                                         
*                                                                               
BLDDEM28 LR    R3,R0               POINT TO LAST 02/03                          
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R3),X'22'         TEST 02/03 FOLLOWED BY 22/23                 
         BL    BLDDEM30                                                         
         CLI   0(R3),X'23'                                                      
         BH    BLDDEM30                                                         
         IC    R0,1(R3)            MAKE SURE TO ADD AFTER 22/23                 
         AR    R3,R0                                                            
*                                                                               
BLDDEM30 CLC   VERSION,=AL1(1,3,0,5)                                            
         BL    BLDDEM40                                                         
         CLI   BSPACT,C'D'         ACTION DELETE?                               
         BE    EXIT                                                             
BLDDEM40 BRAS  RE,ADDEL                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=================================================================              
* VALIDATE POSTBUY DEMOS FOR ORIGINATING AND SPILL                              
*=================================================================              
                                                                                
VALPBD  NTR1  BASE=*,LABEL=*                                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
*                                                                               
         USING PDELEM,R3                                                        
         MVI   PDCODE,X'22'                                                     
         LA    R3,PDEMO            POINT TO FIRST DEMO VALUE                    
         DROP  R3                                                               
*                                                                               
         CLC   MAPNUM,=AL2(I#CDPSPL)  TEST THIS IS SPILL                        
         BNE   VALPBD2                                                          
*                                                                               
         LA    R3,ELEM                                                          
         USING SDELEM,R3                                                        
         MVI   SDCODE,X'23'                                                     
         MVC   SDAGYMKT,BUSPLAMK                                                
         MVC   SDRSVMKT,BUSPLRMK                                                
         LA    R3,SDEMO            POINT TO FIRST DEMO VALUE                    
*                                                                               
VALPBD2  SR    R2,R2                                                            
         ICM   R2,15,QADEMO                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING LW_D,R2                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET NUMBER OF DATA VALUES                    
         LA    R2,LW_DATA2         POINT TO FIRST DATA VALUE                    
         DROP  R2                                                               
*                                                                               
VALPBD4  MVC   0(3,R3),1(R2)        MOVE 3 BYTES FROM FULLWORD                  
         CLC   0(3,R3),=X'100000'  CHECK FOR REASONABLE VALUE                   
         BL    *+6                                                              
         DC    H'0'                                                             
         OC    0(1,R3),4(R2)        'OR' IN FLAGS                               
*                                                                               
         AHI   R2,5                NEXT ARRAY ENTRY                             
         AHI   R3,3                NEXT DEMO IN ELEMENT                         
         BCT   R0,VALPBD4                                                       
*                                                                               
         LA    R0,ELEM             CALCULATE ELEMENT LENGTH                     
         SR    R3,R0                                                            
         STC   R3,ELEM+1                                                        
*                                                                               
         L     R2,AIO3                                                          
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
*                                                                               
VALPBD10 SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         CLI   ELEM,X'22'          TEST DOING ORIG DEMOS                        
         BNE   VALPBD20            NO                                           
* ORIG DEMOS                                                                    
         CLI   0(R3),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
*                                                                               
         CLI   0(R3),X'22'                                                      
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
*NOOP*   CLI   BSPACT,C'D'         ?? DELETE ACTION NOT VALID FOR **            
*NOOP*   BE    EXIT                ?? POST BUY DEMO VALUES                      
         BRAS  RE,ADDEL            X'22' FOLLOWS 02                             
         B     EXIT                                                             
                                                                                
* SPILL DEMOS                                                                   
                                                                                
VALPBD20 CLI   0(R3),0                                                          
         BNE   VALPBD21                                                         
         CLI   BSPACT,C'D'         IS ACTION DELETE?                            
         BE    EXIT                 - THEN ITS OK, JUST EXIT                    
         DC    H'0'                                                             
*                                                                               
         USING NDELEM,R3                                                        
VALPBD21 CLI   BSPACT,C'D'         ACTION DELETE?                               
         BE    VALPBD30             - OK, DON'T LOOK FOR SPILL DEMO             
         CLI   0(R3),3             THIS A SPILL DEMO ELEM                       
         BNE   VALPBD22                                                         
         CLC   NDAGYMKT,ELEM+(SDAGYMKT-SDELEM)  MATCH AGY MKT NUM               
         BE    VALPBD35                                                         
VALPBD22 LLC   R0,1(R3)             GET NEXT SPILL ELEM                         
         AR    R3,R0                                                            
         B     VALPBD20                                                         
*                                                                               
         USING SDELEM,R3                                                        
VALPBD30 CLI   0(R3),X'23'         TEST EXISTING X'23'                          
         BNE   VALPBD22                                                         
         CLC   SDAGYMKT,ELEM+(SDAGYMKT-SDELEM)  MATCH AGY MKT NUM               
         BE    VALPBD45                                                         
         B     VALPBD22                                                         
         DROP  R3                                                               
                                                                                
VALPBD35 LLC   R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R3),X'23'         TEST EXISTING X'23'                          
         BNE   VALPBD50                                                         
VALPBD45 BRAS  RE,DELEL                                                         
*                                                                               
VALPBD50 CLI   BSPACT,C'D'         ACTION DELETE?                               
         BE    EXIT                                                             
         BRAS  RE,ADDEL                                                         
         B     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* VALSPT  -  PROCESS SPOT ELEMENTS                                              
*            DELETE SPOTS FOR ONE (OR ALL WEEKS)                                
*            THEN PROCESS ALL SPOTS FOR A WEEK                                  
*============================================================                   
                                                                                
VALSPT   NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R2,AIO3                                                          
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,RCPOLOQ                                                   
         MVI   ELCDHI,RCPOTOQ                                                   
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
         USING $SDD,R4                                                          
         MVC   SVSPDATE,BUWEEK     INITIALIZE SPOT DATE (IF GIVEN)              
*                                                                               
VALSPT14 BRAS  RE,BLDREG           BUILD AND ADD NEW SPOT ELEMENTS              
         BNE   VALSPTX                                                          
*                                                                               
         AHI   R4,$SDDX2-$SDD      BUMP TO NEXT ARRAY ENTRY                     
         BCT   R0,VALSPT14         DO FOR NUMBER OF SPOTS                       
*                                                                               
VALSPT20 DS    0H                                                               
         B     VALSPTX                                                          
***NOPP  BE    *+6                                                              
***NOPP  DC    H'0'                                                             
*                                                                               
VALSPTX  B     EXIT                                                             
         EJECT                                                                  
*========================================================                       
* BUILD REGEL AND ASSOCIATED ELEMENTS USING $SDD                                
*========================================================                       
                                                                                
BLDREG   NTR1  ,                                                                
                                                                                
         XC    ELEM,ELEM                                                        
EL       USING REGELEM,ELEM                                                     
*                                                                               
         MVI   EL.RCODE,X'0B'                                                   
         TM    $SDSTAT3,X'06'      TEST OTO                                     
         BZ    *+12                                                             
         MVI   EL.RCODE,X'0C'                                                   
*                                                                               
         TM    $SDSTAT3,X'02'      TEST MINUS OTO                               
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'80'    SET MINUS FLAG                               
*                                                                               
         TM    $SDSTAT3,X'10'      TEST MAKEGOOD ON NEW LINE                    
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'02'    SET MAKEGOOD ON NEW LINE                     
*                                                                               
         MVI   EL.RLEN,10          SET UNALLOCATED ELEM LENGTH                  
         OC    $SDDATE,$SDDATE     TEST SPOT DATE PRESENT                       
         BNZ   *+10                                                             
         MVC   $SDDATE,SVSPDATE                                                 
*                                                                               
         OC    EL.RDATE,$SDDATE                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
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
         BNZ   BLDREG04                                                         
         DC    H'0'                MUST BE THERE!                               
*                                                                               
BLDREG04 TM    $SDSTAT3,X'80'      TEST HIATUS                                  
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'04'                                                 
*                                                                               
         TM    $SDSTAT3,X'08'      TEST MISSED                                  
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'40'                                                 
*                                                                               
         TM    $SDSTAT3,$SDSMGPD   TEST MAKEGOOD PENDING                        
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'10'                                                 
*                                                                               
         TM    $SDSTAT4,X'80'      TEST PRD1 PAYS ALL                           
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'08'                                                 
*                                                                               
         TM    $SDSTAT4,X'04'      TEST PRE-ALLOCATED                           
         BZ    *+8                                                              
         OI    EL.RSTATUS,X'01'                                                 
*                                                                               
         TM    $SDSTAT4,X'40'      TEST FOR COST OVERRIDE                       
         BZ    BLDREG06                                                         
         OI    EL.RSTATUS,X'20'    SET COST OVERRIDE FLAG                       
         MVC   EL.RPCOST,$SDCOST   AND AMOUNT                                   
*                                                                               
BLDREG06 LA    R1,$SDPRD1                                                       
         BRAS  RE,FINDPRD                                                       
         BL    BLDREG10            NOT ALLOCATED                                
         BH    VALERR20            INVALID                                      
*                                                                               
         MVI   EL.RLEN,14          UDPATE ELEM LENGTH                           
         MVC   EL.RPPRD,0(R1)                                                   
         MVC   EL.RPTIME,$SDLEN1                                                
         MVC   EL.RPPAYSEQ,$SDCLRSQ                                             
*                                                                               
         BRAS  RE,ADDPRD           ADD PRODUCT TO PRDLIST                       
*                                                                               
         LA    R1,$SDPRD2          TEST FOR SECOND PRD                          
         BRAS  RE,FINDPRD                                                       
         BL    BLDREG10            NOT ALLOCATED                                
         BH    VALERR20            INVALID                                      
*                                                                               
         MVI   EL.RLEN,18                                                       
         MVC   EL.RPPRD+4(1),0(R1)                                              
         MVC   EL.RPTIME+4(1),$SDLEN2                                           
         GOTOR ADDPRD              ADD PRODUCT TO PRDLIST                       
*                                                                               
BLDREG10 LA    R1,$SDMGCD                                                       
         CLI   0(R1),C' '          TEST NOT THERE                               
         BNH   BLDREG20                                                         
         CLI   0(R1),C'$'          TEST TOKEN                                   
         BE    BLDREG12            YES                                          
         BRAS  RE,GETMGBIN         ELSE CONVERT 2 CHAR TO BINARY                
         OC    HALF,HALF           DID WE GET SOMETHING?                        
         JZ    *+2                                                              
         MVC   EL.RPSTAT2,HALF+1                                                
         B     BLDREG20                                                         
*                                                                               
BLDREG12 LA    R1,SVMGTKNS         TRANSLATE MG TOKEN                           
         LHI   R0,(SVMGTKNX-SVMGTKNS)/L'SVMGTKNS                                
         USING MGTOKEND,R1                                                      
*                                                                               
BLDREG14 CLC   MGTOKEN,$SDMGCD                                                  
         BE    BLDREG16                                                         
         AHI   R1,L'SVMGTKNS                                                    
         BCT   R0,BLDREG14                                                      
         DC    H'0'                                                             
*                                                                               
BLDREG16 MVC   EL.RPSTAT2,MGCDBIN                                               
         DROP  R1                                                               
*                                                                               
BLDREG20 LA    R3,BDELEM           FIND INSERTION POINT                         
         USING REGELEM,R3                                                       
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         SR    R7,R7                                                            
*                                                                               
BLDREG22 BRAS  RE,NEXTEL                                                        
         BNE   BLDREG24                                                         
         CLC   EL.RDATE,RDATE      COMPARE DATES                                
         BL    BLDREG30            IF LOW, INSERT NOW                           
         LR    R7,R3               SAVE ADDRESS OF LAST 0B/0C                   
         B     BLDREG22                                                         
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
BLDREG30 SR    RF,RF                                                            
         ICM   RF,1,$SDSPOTS       GET NUMBER OF OCCURRENCES                    
         BNZ   *+8                                                              
         LA    RF,1                                                             
*                                                                               
         CHI   RF,100                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDREG35 MVI   RCUPRTRN,C'R'       RETURN IF RECORD OVERFLOW                    
         BRAS  RE,ADDEL                                                         
         MVI   RCUPRTRN,0                                                       
         CLI   DMCB+8,C'R'                                                      
         BNE   VALERR23                                                         
         BCT   RF,BLDREG35                                                      
*                                                                               
         OC    $SDADATE,$SDADATE   TEST FOR AFFID DATA                          
         BZ    BLDREG36                                                         
*                                                                               
         XC    ELEM,ELEM           ADD AFFIDAVIT ELEMENT                        
EL       USING AFFELEM,ELEM                                                     
         MVI   EL.ACODE,ACCODEQ                                                 
         MVI   EL.ALEN,6                                                        
         MVC   EL.ADATE,$SDADATE                                                
         MVC   EL.ATIME,$SDATIME                                                
         BRAS  RE,BUMPADD                                                       
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
*                                                                               
BLDREGX  B     EXITY                                                            
         DROP  EL,R4                                                            
         EJECT                                                                  
*============================================================                   
* VALTAX  -  PROCESS GST/PST CODES                                              
*============================================================                   
                                                                                
VALTAX   NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'6A'                                                       
         MVI   ELEM+1,3                                                         
         MVC   ELEM+2(1),BUGST                                                  
*                                                                               
         L     R2,AIOBUY                                                        
         LA    R3,BDELEM                                                        
*                                                                               
         MVI   ELCDLO,X'6A'                                                     
         MVI   ELCDHI,X'6A'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         CLI   BUGST,C' '          TEST GST SENT                                
         BNH   VALTAX2                                                          
*                                                                               
         BRAS  RE,ADDEL                                                         
*                                                                               
VALTAX2  XC    ELEM,ELEM                                                        
         MVI   ELEM,X'6B'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),BUPST                                                 
*                                                                               
         LA    R1,ELEM+2                                                        
         LHI   R0,10                                                            
*                                                                               
VALTAX4  CLI   0(R1),C' '          USE X'00' INSTEAD OF SPACE                   
         BH    *+8                                                              
         MVI   0(R1),0                                                          
         AHI   R1,1                                                             
         BCT   R0,VALTAX4                                                       
*                                                                               
         L     R2,AIOBUY                                                        
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,X'6B'                                                     
         MVI   ELCDHI,X'6B'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         BRAS  RE,DELEL                                                         
*                                                                               
         CLC   BUPST,=10C' '       TEST PST SENT                                
         BNH   VALTAXX                                                          
         BRAS  RE,ADDEL                                                         
*                                                                               
VALTAXX  J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*================================================                               
* SECOND COST                                                                   
*================================================                               
                                                                                
VALCOST2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   MYBYTE,C'F'         FORCE COST2 FACTOR ONLY                      
*                                                                               
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
         L     R3,AIOBUY                                                        
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
         L     R3,AIOBUY                                                        
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
         L     R3,AIOBUY                                                        
         OI    BDCIND2-BUYREC(R3),BDCNBRDQ  SET X'10' - BUY IN DOLLARS          
         B     VALCOS10                                                         
*                                                                               
         EJECT                                                                  
*=================================================================              
* BUILD COMMENT ELEMENTS                                                        
*=================================================================              
                                                                                
VALCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
*                                                                               
         MVI   ELCDLO,X'66'        REMOVE ALL COMMENTS FROM BUY                 
         MVI   ELCDHI,X'66'                                                     
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   VALCOM4                                                          
*                                                                               
VALCOM2  BRAS  RE,DELEL                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    VALCOM2                                                          
*                                                                               
VALCOM4  LA    R4,QACOMM1                                                       
*                                                                               
VALCOM10 XC    ELEM,ELEM                                                        
         MVI   ELEM,X'66'                                                       
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
         EJECT                                                                  
*=================================================================              
* BUILD CONTRACT (ID) ELEMENT                                                   
*=================================================================              
                                                                                
VALCON   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIOBUY           GET BUYREC ADDRESS                           
         USING BUYRECD,R2                                                       
*                                                                               
         XC    SVQCONT,SVQCONT                                                  
         MVI   ELCDLO,X'70'        NOW FIND THE OLD ELEMENT                     
         MVI   ELCDHI,X'70'                                                     
         LA    R3,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   VALCON10                                                         
         MVC   SVQCONT(12),3(R3)                                                
         BRAS  RE,DELEL                                                         
*                                                                               
VALCON10 XC    ELEM,ELEM                                                        
         MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15                                                        
         MVC   ELEM+3(12),QCONTRCT                                              
*                                                                               
         CLC   SVQCONT,SPACES      HAVE PREVIOUS BUYID?                         
         BNH   VALCON20            NO, SKIP                                     
         CLI   QCONTRCT,C'*'       FIRST CHAR IS ASTERISK?                      
         BE    VALCON15                                                         
         CLC   QCONTRCT,SPACES     USER REMOVED BUYID?                          
         BH    VALCON20                                                         
*                                                                               
VALCON15 LLC   R0,SVCONNM                                                       
         AHI   R0,1                                                             
         STC   R0,SVCONNM                                                       
*                                                                               
         CHI   R0,3                BUYID REMOVED 3 TIMES?                       
         BL    VALCON20             NO, SKIP                                    
         LLC   RF,SVPRGNM           YES, CHECK PROGRAM NAME                     
         CHI   RF,3                PROGRAM NAME REMOVED 3 TIMES?                
         BL    VALCON20                                                         
         DC    H'0',C'BAD BUYID'                                                
*                                                                               
VALCON20 CLI   QCONTRCT,C' '       NO CONTRACT, JUST EXIT                       
         JNH   EXIT                                                             
         BRAS  RE,ADDEL            INSERT WHERE IT WAS OR AT EOR                
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*===============================================================                
* WRITE BUY RECORD TO FILE                                                      
*===============================================================                
                                                                                
ENDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
*                                                                               
         BRAS  RE,GTNSPTAL         TEST # OF SPOT ALLOC                         
         BNE   VALERR22             - TOO MANY, SEND ERROR TO SPLIT             
*                                                                               
         OC    BUYKMKT,BUYKMKT     TEST MKT 0 BUY IN PROCESS                    
         BNZ   ENDBUY10                                                         
                                                                                
* NEED TO FIX THE PROGRAM NAME FOR MARKET 0                                     
                                                                                
         CLI   $BDPROGR,C'='       BUT NOT FOR LIVE SHOWS                       
         BE    ENDBUY10                                                         
         MVC   BDPROGRM(4),$BDCODE MOVE PROGRAM CODE                            
         MVI   BDPROGRM+4,C'-'                                                  
         MVC   BDPROGRM+5(13),$BDPROGR                                          
*                                                                               
ENDBUY10 MVI   QENDBUY,C'Y'        SET FIRST (NETWORK) BUY ENDED                
         CLI   SVACTION,QACTADD     ACTION ADD?                                 
         BNE   ENDBUY30                                                         
*                                                                               
ENDBUY15 OI    BDSTAT3,BDST3_DSKADD  SET ADDED BY DESKTOP FOR DMDAPTRS          
         BRAS  RE,ADDACTEL                                                      
         BRAS  RE,ADDDTXEL         UPDATE DESKTOP XFER ELEM                     
         BRAS  RE,BUYVAL                                                        
*                                                                               
         CLI   QACTION,QACTOVER    WE'RE ON A DELETED REC?                      
         BNE   ENDBUY20             NO, OKAY TO JUST ADD                        
         OC    BUYKMKT,BUYKMKT     MKT 0 BUY IN PROCESS?                        
         BZ    ENDBUY65             YES, GO PUT THE RECORD BACK                 
         TM    MISCFLG1,LOCLRNF    IS THIS A NEW LOCAL BUY?                     
         BZ    ENDBUY65             NO, GO PUT THE RECORD                       
*                                   YES, GO ADD IT                              
ENDBUY20 MVI   GBYACT,GBYADD                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOSPTFIL+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     ENDBUY70                                                         
*                                                                               
ENDBUY30 CLI   SVMED,C'N'          TEST NETWORK                                 
         BE    ENDBUY35                                                         
         XC    QBDELEM,QBDELEM     NOT NETWORK, CLEAR THIS O/W IT GETS          
         B     ENDBUY50             SAVED ON THE NEXT LINE AT VALBDEL           
*                                                                               
ENDBUY35 OC    BUYKMKT,BUYKMKT     TEST MKT0                                    
         BZ    ENDBUY50                                                         
*                                                                               
         L     R2,AIOBUY                                                        
         USING BUYRECD,R2                                                       
         NI    BUYRCNTL,X'FF'-BUYRDEL                                           
                                                                                
         CLI   QBDELEM,0           TEST ANY BUY DESC DATA SENT                  
         BE    ENDBUY40            NO                                           
*                                                                               
         LHI   RE,BDELEMX-BDELEM                                                
         AHI   RE,-3               -ELCODE/LEN                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BDELEM+2(0),QBDELEM                                              
*                                                                               
ENDBUY40 MVC   BDCOST,BUCOST+1     SET COST                                     
*                                                                               
         OC    BUTIMST(4),BUTIMST  OVERRIDE TIME PROVIDED                       
         BZ    *+10                                                             
         MVC   BDTIMST(4),BUTIMST  SET IT IN RECORD                             
*                                                                               
         CLI   BUCIND2,0                                                        
         BE    *+10                                                             
         MVC   BDCIND2,BUCIND2                                                  
*                                                                               
         XR    R0,R0                                                            
         ICM   R1,15,BUCOST                                                     
         TM    BUCIND2,BDCNBRDQ                                                 
         BZ    *+8                                                              
         M     R0,=F'100'                                                       
         A     R1,TTLCLCST                                                      
         STCM  R1,15,TTLCLCST                                                   
*                                                                               
         MVI   ELCDLO,X'66'        SET TO COPY COMMENTS                         
         MVI   ELCDHI,X'66'                                                     
         BAS   RE,COPYDATA         COPY COMMENTS AND BUYID TO LOCAL             
*                                                                               
         MVI   ELCDLO,X'70'        SET TO COPY BUY ID                           
         MVI   ELCDHI,X'70'                                                     
         BAS   RE,COPYDATA         COPY COMMENTS AND BUYID TO LOCAL             
*                                                                               
ENDBUY50 BRAS  RE,PDCHKSUM         GET PAID ELEMENT CHKSUM                      
         CLC   FULL,CKSUMBFR                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    BDSTAT3,BDST3_DSKCHG  SET CHANGED BY DESKTOP                     
*                                                                               
* FIND AND UPDATE ACTIVITY ELEMENT                                              
*                                                                               
         L     R3,AIOBUY                                                        
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,X'99'                                                     
         MVI   ELCDHI,X'99'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+12                                                             
         BRAS  RE,ADDACTEL                                                      
         B     ENDBUY60                                                         
*                                                                               
         USING ACTVELEM,R3                                                      
         L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   ACTVCHG,SECOPASS-SECD(R1)                                        
         GOTO1 VDATCON,DMCB,(5,0),(3,ACTVCHG+2)                                 
         DROP  R3                                                               
*                                                                               
ENDBUY60 BRAS  RE,ADDDTXEL         UPDATE DESKTOP XFER ELEM                     
         BRAS  RE,BUYVAL                                                        
ENDBUY65 MVI   GBYACT,GBYPUT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOSPTFIL+IOBUY'                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* RUN SINGLE BUY DOWNLOAD UNLESS OPTION TO SUPPRESS                             
                                                                                
ENDBUY70 CLI   SVACTION,C'D'       TEST DELETE                                  
         BE    ENDBUYX              DO NOT DOWNLOAD RESULT                      
         CLI   QRETURN,C'Y'        TEST SUPPRESS RETURN                         
         BE    ENDBUY80                                                         
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRUN',I#CDSBDL)               
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_HEXDQ',IODA),(4,0)                                          
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTERU',0),0,0                    
*                                                                               
ENDBUY80 CLI   SVACTION,C'A'       RETURN TOKEN ON ADD                          
         BNE   ENDBUYX                                                          
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',64)                     
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              X        
               ('LD_CHARQ',QTOKEN),(L'QTOKEN,0)                                 
*                                                                               
ENDBUYX  B     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* GET THE TOTAL # OF SPOT ALLOCATIONS                                           
*=============================================================                  
GTNSPTAL NTR1                                                                   
         L     R2,AIO3                                                          
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,RCPOLOQ      X'0B'                                        
         MVI   ELCDHI,RCPOTOQ      X'0C'                                        
*                                                                               
         SR    R1,R1                                                            
GNSA10   BRAS  RE,NEXTEL                                                        
         BNE   GNSA20                                                           
         LA    R1,1(R1)                                                         
         B     GNSA10                                                           
*                                                                               
GNSA20   CHI   R1,MAXALLOC         MORE THAN 255 SPOT ALLOCS?                   
         JH    EXITN                                                            
         J     EXITY                                                            
*                                                                               
**XALLOC EQU   208*2 **THIS IS WRONG** (MAX 208 X'0B' + MAX 208 X'0C')          
MAXALLOC EQU   (18*ONEK)/$SDDDL    (L'TIA == 18X1024)/(L'SDD == 49)             
         EJECT                                                                  
*=============================================================                  
* COPY NETWORK DATA IN AIOBUYSV TO CURRENT BUY RECORD                           
* ON ENTRY ELCDLO AND ELCDHI CONTAIN PROPER ELEMENT CODES                       
*=============================================================                  
                                                                                
COPYDATA NTR1                                                                   
         L     R3,AIOBUY           REMOVE COMMENTS FROM EXISTING BUY            
         AHI   R3,BDELEM-BUYREC                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   COPYDAT4                                                         
*                                                                               
COPYDAT2 BRAS  RE,DELEL                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    COPYDAT2                                                         
*                                                                               
COPYDAT4 LR    R4,R3               SAVE EOR ADDRESS                             
*                                                                               
         L     R3,AIOBUYSV                                                      
         AHI   R3,BDELEM-BUYREC                                                 
*                                                                               
COPYDAT6 BRAS  RE,NEXTEL                                                        
         BNE   COPYDATX                                                         
*                                                                               
         GOTOR RECUP,DMCB,AIO3,(R3),(R4)                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0               SET NEXT INSERTION ADDRESS                   
         B     COPYDAT6            AND GO LOOK FOR MORE                         
*                                                                               
COPYDATX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*===============================================================                
* END OF NETWORK DATA - RETURN RECORD TO PC                                     
*===============================================================                
                                                                                
ENDNET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   TTLCLCST,$BDCOST                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    QBDELEM,QBDELEM     CLEAR THIS O/W IT GETS SAVED                 
*                                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',99)                     
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* VALIDATE ESTIMATE LOCK/UNLOCK                                                 
*===============================================================                
                                                                                
VALELU   NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY         GET ESTIMATE RECORD                          
         LA    R4,IOKEY                                                         
         USING EKEY,R4                                                          
         MVC   EKEYAM,QMEDX                                                     
         MVC   EKEYCLT,QCLTX                                                    
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,QBEST                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO4'                            
         JNE   VELAMSES                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO4'                        
         JNE   VELAMSES                                                         
*                                                                               
         L     R4,AIO4                                                          
         USING EKEY,R4                                                          
*                                                                               
         CLI   QACTION,C'L'             LOCK REQUESTED?                         
         JNE   VALELU10                 NO                                      
         TM    ECNTRL,X'04'             HELD ESTIMATE?                          
         JNZ   VELALHLD                 YES, CAN'T LOCK IT                      
         TM    ECNTRL,X'08'             ESTIMATE PREV LOCKED?                   
         JNZ   VELALLCK                 NO                                      
         J     VALELU20                 OKAY, IF PREV LOCKED                    
*                                                                               
VALELU10 CLI   QACTION,C'U'             UNLOCK  REQUESTED?                      
         JNE   VELAINVL                 NEITHER, INVALID ACTION                 
         TM    ECNTRL,X'04'             HELD ESTIMATE?                          
         JNZ   VELAUHLD                 NO, THEN CAN'T UNLOCK IT                
         TM    ECNTRL,X'08'             ESTIMATE PREV LOCKED?                   
         JNZ   VELALLCK                 NO                                      
*                                                                               
VALELU20 DS    0H                                                               
         CLI   QACTION,C'L'                                                     
         JNE   VALELU40                                                         
*                                                                               
         MVC   FULL(2),QMON                                                     
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,DMCB,(3,QMON),(0,WORK)                                   
         CLC   WORK(4),ESTART      CHECK REQUESTED MONTH INTERSECTS             
         JL    VELADEST               ESTIMATE MONTHS                           
         CLC   WORK(4),EEND                                                     
         JH    VELADEST                                                         
*                                                                               
         OC    ELOCKYM,ELOCKYM     DO WE ALREADY HAVE A LOCK?                   
         BZ    VALELU30                                                         
*                                                                               
         TM    ELOCKMON,X'80'      WE HAVE A MONTH AND PRIOR??                  
         BZ    VALELU25                                                         
         MVC   DMCB+4(4),=X'D9000A1D'  GETBROAD                                 
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,ESTART),WORK,VGETDAY,VADDAY                         
         CLI   DMCB,X'FF'          INVALID DATE??                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)                                 
         MVC   ELOCKYM,WORK        LOCKING ENTIRE ESTIMATE                      
         B     VALELU35                                                         
*                                                                               
VALELU25 MVC   WORK(2),ELOCKYM                                                  
         NI    WORK+1,X'FF'-X'C0'  TURN OFF PRIOR AND SUSBSEQUENT BITS          
         CLC   WORK(2),QMON        LOCKED MONTH BEFORE REQUESTED MONTH?         
         BH    VALELU30            MAKE IT LOCKED MONTH AND SUBSEQUENT          
         MVC   ELOCKYM,WORK                                                     
         B     VALELU35            MAKE IT LOCKED MONTH AND SUBSEQUENT          
VALELU30 MVC   ELOCKYM,QMON                                                     
VALELU35 OI    ELOCKMON,X'40'      THIS MONTH AND SUBSEQUENT                    
         J     VALELUPT                                                         
*                                                                               
VALELU40 XC    ELOCKYM,ELOCKYM                                                  
*                                                                               
VALELUPT GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOSPTFIL+IO4'                           
         JNE   VALELUD0                                                         
*                                                                               
VALELUX  B     EXIT                                                             
VALELUD0 DC    H'0'                                                             
*                                                                               
VALELUN  B     EXITN                                                            
*                                                                               
VELAINVL LHI   R1,SE#INACT              INVALID ACTION                          
         J     VALERRXX                                                         
*                                                                               
VELAMSES LHI   R1,SE#MSGES              MISSING ESTIMATE                        
         J     VALERRXX                                                         
*                                                                               
VELADEST LHI   R1,SE#DTEST              DATES NOT WITHIN ESTIMATE               
         J     VALERRXX                                                         
*                                                                               
VELALHLD LHI   R1,SE#LKHLD              CAN'T LOCK HELD ESTIMATE                
         J     VALERRXX                                                         
*                                                                               
VELAUHLD LHI   R1,SE#ULHLD              CAN'T UNLOCK HELD ESTIMATE              
         J     VALERRXX                                                         
*                                                                               
VELALLCK LHI   R1,SE#LKLCK              CAN'T USE DATE FOR LOCKED EST           
         J     VALERRXX                                                         
*                                                                               
SE#DTEST EQU   0079                     DATES NOT WITHIN ESTIMATE               
SE#LKHLD EQU   0680                     CAN'T LOCK HELD ESTIMATE                
SE#ULHLD EQU   0683                     CAN'T UNLOCK HELD ESTIMATE              
SE#ULLCK EQU   0684                     ESTIMATE WASN'T PREV LOCKED             
SE#LKLCK EQU   0689                     CAN'T USE DATE FOR LOCKED EST           
*                                                                               
         EJECT                                                                  
*===============================================================                
* VALIDATE REP CODE                                                             
*===============================================================                
                                                                                
VALREP   L     R1,ALP                                                           
         LM    R2,R4,LP_AINP-LP_D(R1)                                           
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         OC    WORK(3),SPACES                                                   
         GOTOR VRCPACK,DMCB,(C'P',WORK),(R4)                                    
         B     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE MEDIA CODE                                                           
*===============================================================                
                                                                                
VALMED   XC    SVBVALS(SVBVALSX-SVBVALS),SVBVALS                                
         L     R1,ALP                                                           
         AHI   R1,LP_AINP-LP_D                                                  
         L     RF,0(R1)                                                         
         GOTOR (#VALMED,AVALMED),(R1)                                           
         MVC   SVMED,QMEDA                                                      
         MVC   SVBAGYMD,QMEDX                                                   
         B     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE CLIENT CODE                                                          
*===============================================================                
                                                                                
VALCLT   XC    SVBCLT(SVBVALSX-SVBCLT),SVBCLT                                   
         MVC   QMEDA,SVMED                                                      
         MVC   QMEDX,SVBAGYMD                                                   
         L     R1,ALP                                                           
         AHI   R1,LP_AINP-LP_D                                                  
         GOTOR (#VALCLT,AVALCLT),(R1)                                           
         BNE   EXIT                                                             
         MVC   SVCLT,QCLTA                                                      
         MVC   SVBCLT,QCLTX                                                     
*                                                                               
         B     EXITY                                                            
                                                                                
*===============================================================                
* VALIDATE ADJACENCY CODE (BDPROGT)                                             
*===============================================================                
                                                                                
VALADJ   L     R1,ALP                                                           
         LM    R2,R4,LP_AINP-LP_D(R1)                                           
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
* CALL LINKIO TO BUILD ERROR RETURN                                             
*===============================================================                
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#UPLRSP)               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',M#UPLRSP),     *        
               WORK,(L'XTRATEXT,XTRATEXT)                                       
         MVC   XTRATEXT,SPACES     RESET EXTRA MESSAGE TEXT TO SPACES           
PUTERRX  B     EXITY                                                            
*                                                                               
M#UPLRSP EQU   X'0002'             JUST A MAP CODE                              
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
         SR    RF,RF               TEST PRODUCT ALREADY IN BUYREC               
         IC    RF,0(R1)                                                         
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
                                                                                
BLDXPRD  NTR1 BASE=*,LABEL=*                                                    
         XC    XPRDLIST,XPRDLIST                                                
         L     R3,AIOBUY                                                        
         AHI   R3,BDELEM-BUYREC                                                 
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
BLDX2    BRAS  RE,NEXTEL                                                        
         BNE   EXIT                                                             
         CLI   1(R3),10                                                         
         BNH   BLDX2                                                            
         SR    RF,RF                                                            
         IC    RF,10(R3)           GET FIRST ALLOCATION                         
         LA    RF,XPRDLIST(RF)                                                  
         MVC   0(1,RF),10(R3)                                                   
*                                                                               
         CLI   1(R3),14                                                         
         BNH   BLDX2                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,14(R3)           GET SECOND ALLOCATION                        
         LA    RF,XPRDLIST(RF)                                                  
         MVC   0(1,RF),10(R3)                                                   
         B     BLDX2                                                            
         EJECT                                                                  
*===================================================================            
* COMPUTE CHKSUM OF PAID SPOTS AND RETURN IN FULL                               
*===================================================================            
                                                                                
PDCHKSUM NTR1  BASE=*,LABEL=*                                                   
         XC    FULL,FULL                                                        
*                                                                               
         L     R2,AIO3                                                          
         USING BUYRECD,R2                                                       
* TURN FLAG OFF BECAUSE DTM SHOULD NEVER TURN IT ON                             
         NI    BDCIND2,X'FF'-BDCRNEGQ                                           
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         MVI   PAIDFLAG,C'N'       RESET PAID SPOTS FLAG                        
*                                                                               
PDCHK2   BRAS  RE,NEXTEL                                                        
         BNE   PDCHK10                                                          
*                                                                               
         USING REGELEM,R3                                                       
         OC    RPAY,RPAY           TEST SPOT PAID                               
         BZ    PDCHK2                                                           
         MVI   PAIDFLAG,C'Y'                                                    
* GET CHKSUM FOR ELEMENT BEING DELETED                                          
         XC    WORK(18),WORK       MOVE SPOT                                    
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R3)       MOVE SPOT                                    
         AHI   RF,1                RESTORE ELEMENT LENGTH                       
         LA    RE,WORK             POINT FOR CKSUM                              
*                                                                               
W        USING REGELEM,WORK                                                     
         NI    W.RSTATUS,X'20'     CLEAR STATUS EXCEPT COST OVRD                
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
                                                                                
PDCHK10  CLI   PAIDFLAG,C'Y'                                                    
         JNE   EXIT                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(L'BDCOST),BDCOST                                            
         MVC   WORK+3(L'BDCIND),BDCIND                                          
         MVC   WORK+4(L'BDCIND2),BDCIND2                                        
         MVC   WORK+5(L'BDNTAX),BDNTAX                                          
                                                                                
         LA    RE,WORK                                                          
         LHI   RF,L'BDCOST+L'BDCIND+L'BDCIND2+L'BDNTAX                          
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
                                                                                
         MVI   ELCDLO,X'68'        LOOK FOR NETWORK PCT                         
         MVI   ELCDHI,X'68'                                                     
         LA    R3,BDELEM                                                        
*                                                                               
PDCHK20  BRAS  RE,NEXTEL                                                        
         JNE   PDCHK30                                                          
*                                                                               
         LLC   RF,1(R3)                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(0),0(R3)                                                    
         NI    WORK+NTWKFLG-NTWKELEM,X'FF'-NTWKFLG_CUTIN                        
*                                                                               
         AHI   RF,-2               LENGTH FOR CKSM                              
         BNP   PDCHK20                                                          
         LA    RE,WORK+2                                                        
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
         B     PDCHK20                                                          
* AND INCLUDE TAX ELEMENT IN CHKSUM                                             
                                                                                
PDCHK30  MVI   ELCDLO,X'69'        LOOK FOR TAX, GST                            
         MVI   ELCDHI,X'6B'        AND PST ELEM                                 
         LA    R3,BDELEM                                                        
*                                                                               
PDCHK35  BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
*                                                                               
         LLC   RF,1(R3)                                                         
         AHI   RF,-2               LENGTH FOR CKSM                              
         BNP   PDCHK35                                                          
         LA    RE,2(R3)            ADDRESS FOR CKSM                             
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,FULL                                                          
         ST    R0,FULL                                                          
         B     PDCHK35                                                          
         EJECT                                                                  
*=================================================================              
* FIND NEXT AVAILABLE MAKEGOOD CODE                                             
* ON ENTRY R4 POINTS TO NEW TABLE ENTRY                                         
*=================================================================              
                                                                                
MAXCODES EQU   241  <==== CHANGE FOR NEW MAKEGOODS                              
                                                                                
         USING MGTOKEND,R4                                                      
GETMGCD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,TEMP1                                                         
         USING MGABLKD,R6                                                       
         XC    0(MGALNQ,R6),0(R6)                                               
*                                                                               
         TM    SVMGCDTF,X'80'      ALREADY BUILT TABLE?                         
         JNZ   GETMG10                                                          
*                                                                               
         MVI   MGAACT,MGAQBLD      SET ACTION - BUILD TABLE                     
         MVC   MGAACOM,ACOMFACS    SET A(COMFAS)                                
         MVC   MGATSAR,VTSAR                                                    
         MVC   MGAIO,AIO6          USE AIO6 AS WORK AREA                        
         MVC   MGUNTIME,VUNTIME                                                 
         MVC   MGSTAPAK,VSTAPACK                                                
         MVC   MGGETBUY,VGETBUY    SET A(GETBUY)                                
         MVC   MG1OR2,SV1OR2       SET 1 OR 2 BYTE BUYS                         
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
         STCM  RE,7,MGCODTAB+1     PASS A(MG COD TABLE)                         
         OC    MGCODTBL,SVMGCDTF                                                
         GOTO1 VBLDMGN,MGABLKD     GET NEXT AVAIL MG CODE                       
         CLI   MGAERR,0            ANY ERRORS?                                  
         JNE   *+2                  YES, DIE                                    
MGA      USING MGENTRYD,MGAENTRY                                                
         MVC   MGCDALPH,MGA.MGECODE  SET MG CODE                                
         MVC   MGCDBIN,MGA.MGAECOD   SET MG BIN CODE                            
         OC    SVMGCDTF,MGCODTBL                                                
         DROP  MGA,R4,R6                                                        
*                                                                               
* REREAD BUY                                                                    
         CLI   SVACTION,QACTADD    TEST ADD                                     
         BE    GETMGX                                                           
*                                                                               
         MVC   IOKEY(L'BUYKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOSPTDIR+IO6'                         
*                                                                               
         CLC   IOKEY(12),IOKEYSAV  A-M/CLT/PRD/MKT/STA/EST/LIN                  
         BE    *+6                                                              
         DC    H'0'                                                             
* NOTE - DO NOT READ OVER NEW BUY RECORD IN IOBUY !!!                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOSPTFIL+IO6'                        
*                                                                               
GETMGX   J     EXIT                                                             
                                                                                
*================================================================               
* CONVERT ALPHA CODE IN HALF TO 1 BYTE BINARY IN BYTE                           
*                                                                               
* CLOBBERS TEMP1                                                                
*                                                                               
*================================================================               
                                                                                
GETMGBIN NTR1  BASE=*,LABEL=*                                                   
         LA    R6,TEMP1                                                         
         USING MGABLKD,R6                                                       
                                                                                
         XC    0(MGALNQ,R6),0(R6)                                               
                                                                                
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
         DROP  R6                                                               
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
         MVC   SPBYAREC,AIOBUY                                                  
         MVC   SPBYAFAC,ACOMFACS                                                
*                                                                               
         L     RE,ACLTREC          POINT TO CLIENT RECORD                       
         LLC   R0,CPROF-CLTHDR(RE)                                              
*                                                                               
         GOTO1 (RF),(R1),((R0),SPBYLEN)                                         
         CLI   SPBYERR,0                                                        
         JE    EXIT                                                             
         DCHO                                                                   
         DROP  R3                                                               
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
         L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   ACTVADD,SECOPASS-SECD(R1)                                        
         GOTO1 VDATCON,DMCB,(5,0),(3,ACTVADD+2)                                 
*                                                                               
         CLI   SVACTION,QACTADD    NO CHANGE DATE ON ADD                        
         BE    ADDACT10                                                         
         MVC   ACTVCHG,ACTVADD                                                  
         DROP  R3                                                               
*                                                                               
ADDACT10 L     R3,AIOBUY           POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
*================================================                               
* UPDATE DESKTOP TRANSFER ELEM                                                  
*================================================                               
                                                                                
ADDDTXEL NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO6             R4=AVAIL/REVISION REC                        
                                                                                
         L     R3,AIOBUY                                                        
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
         MVC   DTXVER,VERSION      SAVE PC VERSION NUMBER                       
         BRAS  RE,GETDTTM                                                       
         MVC   DTXTIME,WORK+2      TIME LAST CHANGED                            
         DROP  R3                                                               
*                                                                               
ADDDT10  L     R3,AIOBUY           POINT TO END OF BUYREC                       
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)                                                      
         AR    R3,R0                                                            
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
*=================================================================              
* ELEMENT MAINTENANCE ROUTINES                                                  
*=================================================================              
                                                                                
BUMPADD  CLI   0(R3),0             TEST AT EOR ALREADY                          
         JE    ADDEL                                                            
         SR    R0,R0               BUMP TO NEXT ELEMENT AND ADD                 
         IC    R0,1(R3)            ELEMENT                                      
         AR    R3,R0                                                            
                                                                                
ADDEL    NTR1  ,                   ADD AN ELEMENT                               
         GOTOR RECUP,DMCB,AIO3,ELEM,(RCUPRTRN,(R3))                             
         J     EXIT                                                             
                                                                                
DELEL    NTR1  ,                   DELETE AN ELEMENT                            
         GOTOR RECUP,DMCB,AIO3,(R3)                                             
         J     EXIT                                                             
                                                                                
NEXTEL   CLI   0(R3),0             LOCATE AN ELEMENT                            
         JE    NEXTELN                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R3)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
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
                                                                                
VALERR02 LHI   R1,SE#MSGMD         MISSING MEDIA                                
         B     VALERRX                                                          
                                                                                
VALERR03 MVC   XTRATEXT+5(L'QCLTA),QCLTA                                        
         LHI   R1,SE#INCLT         INVALID CLIENT                               
         B     VALERRX                                                          
                                                                                
VALERR04 LHI   R1,SE#MSGCL         MISSING CLIENT                               
         B     VALERRX                                                          
                                                                                
VALERR05 LHI   R1,SE#MSGMK         MISSING MARKET                               
         B     VALERRX                                                          
                                                                                
VALERR06 MVC   XTRATEXT+5(L'QSTA),QSTA                                          
         LHI   R1,SE#INVST         INVALID STATION/NETWORK                      
         B     VALERRX                                                          
                                                                                
VALERR07 LHI   R1,SE#MSGSN         MISSING STATION/NETWORK                      
         B     VALERRX                                                          
                                                                                
VALERR08 LHI   R1,SE#MSGES         MISSING ESTIMATE                             
         B     VALERRX                                                          
                                                                                
VALERR09 LHI   R1,SE#MSGSD         MISSING SPOT DATE                            
         B     VALERRX                                                          
                                                                                
VALERR10 LHI   R1,SE#KEYNF         (BUY) KEY NOT FOUND                          
         B     VALERRX                                                          
                                                                                
VALERR11 LHI   R1,SE#RECNF         (BUY) RECORD NOT FOUND                       
         B     VALERRX                                                          
                                                                                
VALERR13 MVC   XTRATEXT(L'QSTA),QSTA  SHOW THE STATION                          
         LHI   R1,SE#STLCK           STATION IS LOCKED                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XTRATEXT+L'QSTA+2(4),DUB                                         
         MVI   ERRORFLG,FATALERR   SET TO SKIP ALL FURTHER INPUTS               
         B     VALERRXX               NOT A FATAL ERROR                         
                                                                                
VALERR14 LHI   R1,SE#NOALC         SPOT ALLOCATION NOT ALLOWED                  
         B     VALERRX                                                          
                                                                                
VALERR15 LHI   R1,SE#CKSUM         CHKSUM MISMATCH                              
         B     VALERRX                                                          
                                                                                
MAXLNERR LHI   R1,SE#MAXLN         TOO MANY BUYLINES                            
         B     VALERRX                                                          
                                                                                
VALERR20 MVC   XTRATEXT+5(L'EKEYPRD),0(R1)                                      
         LHI   R1,SE#INVPR         INVALID PRODUCT                              
         B     VALERRX                                                          
*                                                                               
VALERR21 LHI   R1,SE#DATLK         DATA LOCKED FOR OFFLINE PROCESSING           
         B     VALERRX                                                          
*                                                                               
VALERR22 LHI   R1,SE#MXALC         TOO MANY SPOT ALLOCS - SPLIT BUYLINE         
         B     VALERRX                                                          
*                                                                               
VALERR23 LHI   R1,SE#MAXSZ         APPROACHING MAX RECORD SIZE - SPLIT          
         B     VALERRX                                                          
*********                                                                       
* WE'RE SETTING ALL ERRORS TO FATAL AS DDS DESKTOP IS NOT SHOWING               
* THE AVAIL DETAILS FOR EACH ERROR ANYWAYS.  EJOR SAID THAT BUY CONSTR          
* SHOULD HAVE CAUGHT THE PROBLEMS EARLIER.                                      
*&&DO*&& VALERRX  MVI   ERRORFLG,ERRRGULR   REGULAR ERROR                       
*********                                                                       
VALERRX  MVI   ERRORFLG,FATALERR   SET TO SKIP ALL FURTHER INPUTS               
VALERRX1 CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XTRATEXT(4),DUB                                                  
VALERRXX GOTOR PUTERR                                                           
*                                                                               
EXITN    SR    RE,RE                                                            
         B     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
STAMPLIT DC    C'**SL23**'                                                      
                                                                                
RECTAB   DS    0XL6                ** RECORD TABLE **                           
         DC    AL2(I#CDSBHD),AL3(VALBHD),X'80'   BUY HEADER                     
         DC    AL2(I#CDNTEL),AL3(VALNETST),X'00' LOCAL BUY HEADER               
         DC    AL2(I#CDSBUY),AL3(VALBDEL),X'00'  BUY DESC ELEM                  
         DC    AL2(I#CDSSPT),AL3(VALSPT),X'00'   SPOT DETAILS                   
         DC    AL2(I#CDSCOM),AL3(VALCOM),X'00'   COMMENTS                       
         DC    AL2(I#CDSCON),AL3(VALCON),X'00'   CONTRACT                       
         DC    AL2(I#CDSDMV),AL3(VALDEM),X'00'   DEMO VALUES                    
         DC    AL2(I#CDPDMV),AL3(VALPBD),X'00'   POST BUY DEMO VALUES           
         DC    AL2(I#CDSSPL),AL3(VALSPL),X'00'   SPILL DEMO VALUES              
         DC    AL2(I#CDPSPL),AL3(VALPBD),X'00'   POST BUY SPILL DEMOS           
         DC    AL2(I#CDTXEL),AL3(VALTAX),X'00'   GST/PST VALUES                 
         DC    AL2(I#CDCOS2),AL3(VALCOST2),X'00' COST2 VALUES                   
         DC    AL2(I#CDEBUY),AL3(ENDBUY),X'00'   END BUY DATA                   
         DC    AL2(I#CDENET),AL3(ENDNET),X'00'   END NETWORK DATA               
         DC    AL2(I#CESTLK),AL3(VALELU),X'00'   ESTIMATE LOCK/UNLOCK           
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
TBDTIME  DC    C'BDTIME'                                                        
TBDCOSTP DC    C'BDCOSTP'                                                       
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
TBDCHG   DC    C'BDCHG'                                                         
TBDWHY   DC    C'BDWHY'                                                         
TBDPURP  DC    C'BDPURP'                                                        
TBDSEDAY DC    C'BDSEDAY'                                                       
TBDCIND2 DC    C'BDCIND2'                                                       
TBCANAD  DC    C'BDCANAD'                                                       
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
                                                                                
TBOOK    DC    C'BOOK'                                                          
TBKTYPE  DC    C'BOOKTYPE'                                                      
TPROGRAM DC    C'PROGRAM'                                                       
TDEMOVAL DC    C'DEMOVAL'                                                       
TAGYMKT  DC    C'AGENCY MKT'                                                    
TRETURN  DC    C'RETURN BUYS'                                                   
TRSMKT   DC    C'RTGSVC MKT'                                                    
TDEMOVER DC    C'FLAGS'                                                         
TLKUPMKT DC    C'LKUP MKT'                                                      
TLKUPSTA DC    C'LKUP STA'                                                      
TLKUPRSV DC    C'LKUP RTG SVC'                                                  
TPBDEM   DC    C'PBDEM'                                                         
TPBDEMOV DC    C'PBDEM FLAG'                                                    
TPBSDEM  DC    C'PBSDEM'                                                        
TPBSDMOV DC    C'PBSDEM FLAG'                                                   
TBSPLACT DC    C'BUY SPILL ACTION'                                              
TPSPLACT DC    C'POST BUY SPILL ACTION'                                         
                                                                                
TPROVCD  DC    C'PROV CODE'                                                     
TSTAPCT  DC    C'STAPCT'                                                        
TSTACOST DC    C'STACOST'                                                       
TSTACIN2 DC    C'STACOST IND2'                                                  
TSTAFLAG DC    C'STA FLAGS'                                                     
TSTAGST  DC    C'GST'                                                           
TSTAPST  DC    C'PST'                                                           
TENDBUY  DC    C'END BUY ?'                                                     
                                                                                
TCOS2    DC    C'SECOND COST/FACT'                                              
TFACT    DC    C'(F)ACTOR?'                                                     
                                                                                
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
TSPOTS   DC    C'NUMBER OF SPOTS'                                               
         EJECT                                                                  
*============================================================                   
* BUY KEY                                                                       
*============================================================                   
                                                                                
SPTBUY   LKREQ H,I#CDSBHD,NEWREC=Y     MAPCODE 260                              
                                                                                
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
BUYSBACK LKREQ F,11,(D,B#SAVED,QRETURN),CHAR,TEXT=(*,TRETURN)                   
                                                                                
         LKREQ E                                                                
                                                                                
*========================================================                       
* BUY DESC DATA                                                                 
*========================================================                       
                                                                                
BDDEFN   LKREQ H,I#CDSBUY,NEWREC=Y          MAPCODE 262                         
                                                                                
BDSTART  LKREQ F,1,(D,B#BDEL,$BDSTART),BDAT,TEXT=(*,TBDSTART)                   
BDEND    LKREQ F,2,(D,B#BDEL,$BDEND),BDAT,TEXT=(*,TBDEND)                       
BDWKS    LKREQ F,3,(D,B#BDEL,$BDWKS),LBIN,TEXT=(*,TBDWKS)                       
BDINPUT  LKREQ F,4,(D,B#BDEL,$BDINPUT),LBIN,TEXT=(*,TBDINPUT)                   
BDWKIND  LKREQ F,5,(D,B#BDEL,$BDWKIND),CHAR,TEXT=(*,TBDWKIND)                   
BDDAY    LKREQ F,6,(D,B#BDEL,$BDDAY),LBIN,TEXT=(*,TBDDAY)                       
BDNOWK   LKREQ F,7,(D,B#BDEL,$BDNOWK),LBIN,TEXT=(*,TBDNOWK)                     
BDSEC    LKREQ F,8,(D,B#BDEL,$BDSEC),LBIN,TEXT=(*,TBDSEC)                       
BDTIME   LKREQ F,9,(D,B#BDEL,$BDTIME),LBIN,TEXT=(*,TBDTIME)                     
BDCOSTP  LKREQ F,10,(D,B#BDEL,$BDCOSTP),LBIN,TEXT=(*,TBDCOSTP)                  
BDDAYPT  LKREQ F,11,(D,B#BDEL,$BDDAYPT),CHAR,TEXT=(*,TBDDAYPT)                  
BDTIMST  LKREQ F,12,(D,B#BDEL,$BDTIMST),LBIN,TEXT=(*,TBDTIMST)                  
BDTIMEND LKREQ F,13,(D,B#BDEL,$BDTIMND),LBIN,TEXT=(*,TBDTIMND)                  
BDCODE   LKREQ F,37,(D,B#BDEL,$BDCODE),CHAR,TEXT=(*,TBDPROCD)                   
BDPROGRM LKREQ F,14,(D,B#BDEL,$BDPROGR),CHAR,TEXT=(*,TBDPROGM)                  
BDPROGT  LKREQ F,15,(D,B#BDEL,$BDPROGT),(R,VALADJ),TEXT=(*,TBDPROGT)            
BDCOST   LKREQ F,16,(D,B#BDEL,$BDCOST),CBIN,TEXT=(*,TBDCOST)                    
BDCIND   LKREQ F,17,(D,B#BDEL,$BDCIND),HEXD,TEXT=(*,TBDCIND)                    
BDNTAX   LKREQ F,18,(D,B#BDEL,$BDNTAX),LBIN,TEXT=(*,TBDNTAX)                    
BDWHY3   LKREQ F,19,(D,B#BDEL,$BDWHY3),HEXD,TEXT=(*,TBDWHY3)                    
BDREP    LKREQ F,20,(D,B#BDEL,$BDREP),(R,VALREP),TEXT=(*,TBDREP)                
BDCHG    LKREQ F,21,(D,B#BDEL,$BDCHG),BDAT,TEXT=(*,TBDCHG)                      
BDWHY    LKREQ F,22,(D,B#BDEL,$BDWHY),HEXD,TEXT=(*,TBDWHY)                      
BDPURP   LKREQ F,23,(D,B#BDEL,$BDPURP),CHAR,TEXT=(*,TBDPURP)                    
BDSEDAY  LKREQ F,24,(D,B#BDEL,$BDSEDAY),HEXD,TEXT=(*,TBDSEDAY)                  
BDCANAD  LKREQ F,25,(D,B#BDEL,$BDCANAD),HEXD,TEXT=(*,TBCANAD)                   
BDCIND2  LKREQ F,26,(D,B#BDEL,$BDCIND2),HEXD,TEXT=(*,TBDCIND2)                  
BDWHY2   LKREQ F,27,(D,B#BDEL,$BDWHY2),HEXD,TEXT=(*,TBDWHY2)                    
BDSTAT   LKREQ F,28,(D,B#BDEL,$BDSTAT),HEXD,TEXT=(*,TBDSTAT)                    
BDMGDATE LKREQ F,29,(D,B#BDEL,$BDMGCD),CHAR,TEXT=(*,TBDMGCOD)                   
BDSTAT3  LKREQ F,30,(D,B#BDEL,$BDSTAT3),HEXD,TEXT=(*,TBDSTAT3)                  
BDNRGN   LKREQ F,31,(D,B#BDEL,$BDNRGN),CHAR,TEXT=(*,TBDNRGN)                    
BDMASPR1 LKREQ F,33,(D,B#SAVED,BUMASPR1),CHAR,TEXT=(*,TBDMAST1)                 
BDMASPR2 LKREQ F,34,(D,B#SAVED,BUMASPR2),CHAR,TEXT=(*,TBDMAST2)                 
BDSTAT2  LKREQ F,35,(D,B#BDEL,$BDSTAT2),HEXD,TEXT=(*,TBDSTAT2)                  
BDSTAT4  LKREQ F,36,(D,B#BDEL,$BDSTAT4),HEXD,TEXT=(*,TBDSTAT2)                  
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* CONTRACT                                                                      
*============================================================                   
                                                                                
CONDEFN  LKREQ H,I#CDSCON,NEWREC=Y        MAPCODE 265                           
                                                                                
BDCON#   LKREQ F,1,(D,B#SAVED,QCONTRCT),VSTR,TEXT=(*,TBDCON),          X        
               MAXLEN=L'QCONTRCT                                                
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* COMMENT ELEMENTS (MAP CODE IS COMMENT NUMBER)                                 
*============================================================                   
                                                                                
COMDEFN  LKREQ H,I#CDSCOM,NEWREC=Y        MAPCODE 264                           
                                                                                
CMDATA1  LKREQ F,1,(I,B#SAVED,QACOMM1),VSTR,TEXT=(*,TCMDATA1)                   
CMDATA2  LKREQ F,2,(I,B#SAVED,QACOMM2),VSTR,TEXT=(*,TCMDATA2)                   
CMDATA3  LKREQ F,3,(I,B#SAVED,QACOMM3),VSTR,TEXT=(*,TCMDATA3)                   
CMDATA4  LKREQ F,4,(I,B#SAVED,QACOMM4),VSTR,TEXT=(*,TCMDATA4)                   
CMDATA5  LKREQ F,5,(I,B#SAVED,QACOMM5),VSTR,TEXT=(*,TCMDATA5)                   
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* DEMO VALUES                                                                   
*============================================================                   
                                                                                
SPDEMEL  LKREQ H,I#CDSDMV,NEWREC=Y           MAPCODE 266                        
                                                                                
BUBOOK   LKREQ F,1,(D,B#SAVED,BUBOOK),BDAT,TEXT=(*,TBOOK)                       
BUBKTYPE LKREQ F,2,(D,B#SAVED,BUBKTYPE),CHAR,TEXT=(*,TBKTYPE)                   
BUPROG   LKREQ F,3,(D,B#SAVED,BUPROG),VSTR,TEXT=(*,TPROGRAM),          X        
               MAXLEN=L'BUPROG                                                  
DEMQMKT  LKREQ F,4,(D,B#SAVED,DEMQMKT),CHAR,TEXT=(*,TLKUPMKT) LKUP MKT          
DEMQSTA  LKREQ F,5,(D,B#SAVED,DEMQSTA),CHAR,TEXT=(*,TLKUPSTA)  LKUP STA         
DEMQRSV  LKREQ F,6,(D,B#SAVED,DEMQRSV),CHAR,TEXT=(*,TLKUPRSV) LKUP RSV          
*                                                                               
DEMCODE  LKREQ F,10,(I,B#SAVED,QADEMO),(U,#VALDCD,$VALDCD),OLEN=3,     X        
               ARRAY=S,SORT=NO,TEXT=SP#DEMO                                     
DEMOVAL  LKREQ F,11,,LBIN,OLEN=4,TEXT=(*,TDEMOVAL)                              
DEMOVER  LKREQ F,12,,HEXD,OLEN=1,TEXT=(*,TDEMOVER),ARRAY=E                      
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* POST BUY DEMO VALUES                                                          
*============================================================                   
                                                                                
PBDEMEL  LKREQ H,I#CDPDMV,NEWREC=Y           MAPCODE 276                        
                                                                                
PBDEMVL  LKREQ F,11,(I,B#SAVED,QADEMO),LBIN,OLEN=4,                    X        
               ARRAY=S,SORT=NO,TEXT=(*,TPBDEM)                                  
PBDEMOV  LKREQ F,12,,HEXD,OLEN=1,TEXT=(*,TPBDEMOV),ARRAY=E                      
                                                                                
         LKREQ E                                                                
*============================================================                   
* SPILL DEMO VALUES                                                             
*============================================================                   
                                                                                
SPSPLEL  LKREQ H,I#CDSSPL,NEWREC=Y            MAPCODE 267                       
                                                                                
BUSPACT  LKREQ F,20,(D,B#SAVED,BSPACT),CHAR,TEXT=(*,TBSPLACT)                   
BUSPLBK  LKREQ F,1,(D,B#SAVED,BUBOOK),BDAT,TEXT=(*,TBOOK)                       
BUBKTYPE LKREQ F,2,(D,B#SAVED,BUBKTYPE),CHAR,TEXT=(*,TBKTYPE)                   
DEMQMKT  LKREQ F,4,(D,B#SAVED,DEMQMKT),CHAR,TEXT=(*,TLKUPMKT)                   
DEMQSTA  LKREQ F,5,(D,B#SAVED,DEMQSTA),CHAR,TEXT=(*,TLKUPSTA)                   
DEMQRSV  LKREQ F,6,(D,B#SAVED,DEMQRSV),CHAR,TEXT=(*,TLKUPRSV)                   
BUSPLAMK LKREQ F,7,(D,B#SAVED,BUSPLAMK),LBIN,TEXT=(*,TAGYMKT)                   
BUSPLRMK LKREQ F,8,(D,B#SAVED,BUSPLRMK),LBIN,TEXT=(*,TRSMKT)                    
SPDMCOD  LKREQ F,10,(I,B#SAVED,QADEMO),(U,#VALDCD,$VALDCD),OLEN=3,     X        
               ARRAY=S,SORT=NO,TEXT=SP#DEMO                                     
SPDMVAL  LKREQ F,11,,LBIN,OLEN=4,TEXT=(*,TDEMOVAL)                              
SPDMOVR  LKREQ F,12,,HEXD,,OLEN=1,TEXT=(*,TDEMOVER),ARRAY=E                     
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* SPILL POST BUY DEMO VALUES                                                    
*============================================================                   
                                                                                
PBSDEM   LKREQ H,I#CDPSPL,NEWREC=Y           MAPCODE 277                        
                                                                                
PBSPACT  LKREQ F,20,(D,B#SAVED,BSPACT),CHAR,TEXT=(*,TPSPLACT)                   
PBSAMK   LKREQ F,7,(D,B#SAVED,BUSPLAMK),LBIN,TEXT=(*,TAGYMKT)                   
PBSRMK   LKREQ F,8,(D,B#SAVED,BUSPLRMK),LBIN,TEXT=(*,TRSMKT)                    
PBSDMVL  LKREQ F,11,(I,B#SAVED,QADEMO),LBIN,OLEN=4,                    X        
               ARRAY=S,SORT=NO,TEXT=(*,TPBSDEM)                                 
PBSDMOV  LKREQ F,12,,HEXD,OLEN=1,TEXT=(*,TPBSDMOV),ARRAY=E                      
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* SPOT DETAILS                                                                  
*============================================================                   
                                                                                
SPDEFN   LKREQ H,I#CDSSPT,NEWREC=Y              MAPCODE 263                     
                                                                                
BUWEEK   LKREQ F,01,(D,B#SAVED,BUWEEK),CDAT,TEXT=(*,TBUWEEK)                    
                                                                                
SDATE    LKREQ F,10,(I,B#SAVED,QASPOT),CDAT,OLEN=L'$SDDATE,            X        
               TEXT=(*,TSDDATE),ARRAY=S,SORT=NO                                 
                                                                                
SDSTAT   LKREQ F,11,,HEXD,OLEN=L'$SDSTAT,TEXT=(*,TSDSTAT)                       
SDPRD1   LKREQ F,12,,CHAR,OLEN=L'$SDPRD1,TEXT=(*,TSDPRD1)                       
SDLEN1   LKREQ F,13,,LBIN,OLEN=L'$SDLEN1,TEXT=(*,TSDLEN1)                       
SDPRD2   LKREQ F,14,,CHAR,OLEN=L'$SDPRD2,TEXT=(*,TSDPRD2)                       
SDLEN2   LKREQ F,15,,LBIN,OLEN=L'$SDLEN2,TEXT=(*,TSDLEN2)                       
SDCOST   LKREQ F,16,,LBIN,OLEN=L'$SDCOST,TEXT=(*,TSDCOST)                       
SDMGCD   LKREQ F,17,,CHAR,OLEN=L'$SDMGCD,TEXT=(*,TSDMGCD)                       
SDCDATE  LKREQ F,18,,CDAT,OLEN=L'$SDCLRDT,TEXT=(*,TSDCDAT)                      
SDCSEQ#  LKREQ F,19,,LBIN,OLEN=L'$SDCLRSQ,TEXT=(*,TSDCSEQ)                      
SDADATE  LKREQ F,20,,CDAT,OLEN=L'$SDADATE,TEXT=(*,TSDADAT)                      
SDATIME  LKREQ F,21,,LBIN,OLEN=L'$SDATIME,TEXT=(*,TSDATIM)                      
SDADAY   LKREQ F,30,,LBIN,OLEN=L'$SDADAY,TEXT=(*,TSDADAY)                       
SDAFLM1  LKREQ F,22,,LBIN,OLEN=L'$SDAFLM1,TEXT=(*,TSDAFL1)                      
SDAFLM2  LKREQ F,23,,LBIN,OLEN=L'$SDAFLM2,TEXT=(*,TSDAFL2)                      
SDDFLM   LKREQ F,24,,LBIN,OLEN=L'$SDDFLM,TEXT=(*,TSDDFLM)                       
SDDTAG   LKREQ F,25,,LBIN,OLEN=L'$SDDTAG,TEXT=(*,TSDDTAG)                       
SDDDATE  LKREQ F,26,,CDAT,OLEN=L'$SDDDATE,TEXT=(*,TSDDDATE)                     
SDTFLM1  LKREQ F,27,,LBIN,OLEN=L'$SDTFLM1,TEXT=(*,TSDTFL1)                      
SDTFLM2  LKREQ F,28,,LBIN,OLEN=L'$SDTFLM2,TEXT=(*,TSDTFL2)                      
SDTPATT  LKREQ F,29,,LBIN,OLEN=L'$SDTPATT,TEXT=(*,TSDTPAT)                      
SDSPOTS  LKREQ F,31,,LBIN,OLEN=L'$SDSPOTS,TEXT=(*,TSPOTS),             X        
               ARRAY=E                                                          
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* NETWORK STATION ELEMENT                                                       
*============================================================                   
                                                                                
NTSTAEL  LKREQ H,I#CDNTEL,NEWREC=Y          MAPCODE 268                         
                                                                                
MKT      LKREQ F,1,(D,B#SAVED,QMKT),LBIN,TEXT=SP#MKT                            
STA      LKREQ F,2,(D,B#SAVED,QSTA),CHAR,TEXT=SP#STNET                          
PROVCD   LKREQ F,3,(D,B#SAVED,BUPROVCD),CHAR,TEXT=(*,TPROVCD)                   
NETPCT   LKREQ F,4,(D,B#SAVED,BUPCT),LBIN,TEXT=(*,TSTAPCT)                      
NETFLAGS LKREQ F,5,(D,B#SAVED,BUFLAGS),HEXD,TEXT=(*,TSTAFLAG)                   
NETCOST  LKREQ F,6,(D,B#SAVED,BUCOST),LBIN,TEXT=(*,TSTACOST)                    
NETGST   LKREQ F,7,(D,B#SAVED,BUGST),VSTR,TEXT=(*,TSTAGST),            X        
               MAXLEN=L'BUGST                                                   
NETPST   LKREQ F,8,(D,B#SAVED,BUPST),VSTR,TEXT=(*,TSTAPST),            X        
               MAXLEN=L'BUPST                                                   
NETCIND2 LKREQ F,11,(D,B#SAVED,BUCIND2),HEXD,TEXT=(*,TSTACIN2)                  
                                                                                
NETTIMST LKREQ F,9,(D,B#SAVED,BUTIMST),LBIN,TEXT=(*,TBDTIMST)                   
NETTIMND LKREQ F,10,(D,B#SAVED,BUTIMND),LBIN,TEXT=(*,TBDTIMND)                  
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* TAX CODE ELEMENT                                                              
*============================================================                   
                                                                                
TAXEL    LKREQ H,I#CDTXEL,NEWREC=Y          MAPCODE 269                         
                                                                                
GSTCODE  LKREQ F,1,(D,B#SAVED,BUGST),VSTR,TEXT=(*,TSTAGST),            X        
               MAXLEN=L'BUGST                                                   
PSTCODES LKREQ F,2,(D,B#SAVED,BUPST),VSTR,TEXT=(*,TSTAPST),            X        
               MAXLEN=L'BUPST                                                   
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* SECOND COST                                                                   
*============================================================                   
                                                                                
COS2     LKREQ H,I#CDCOS2,NEWREC=Y          MAPCODE 26A                         
                                                                                
C2VALUE  LKREQ F,1,(D,B#SAVED,MYDUB),SPAK,TEXT=(*,TCOS2)                        
C2FACT   LKREQ F,2,(D,B#SAVED,MYBYTE),CHAR,TEXT=(*,TFACT)                       
                                                                                
         LKREQ E                                                                
                                                                                
*============================================================                   
* END OF BUY                                                                    
*============================================================                   
                                                                                
ENDBYEL  LKREQ *,I#CDEBUY,NEWREC=Y             MAPCODE 270                      
                                                                                
*============================================================                   
* END OF NETWORK UPLOAD                                                         
*============================================================                   
                                                                                
ENDNTEL  LKREQ *,I#CDENET,NEWREC=Y             MAPCODE 271                      
                                                                                
*============================================================                   
* ESTIMATE LOCK/UNLOCK                                                          
*============================================================                   
                                                                                
ESTLKUL  LKREQ H,I#CESTLK,NEWREC=Y             MAPCODE 29C                      
TOKEN    LKREQ F,1,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                      
ACTION   LKREQ F,2,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
MONTH    LKREQ F,3,(D,B#SAVED,QMON),BMON,TEXT=SP#MONTH                          
MED      LKREQ F,4,(D,B#WORKD,QMEDX),(R,VALMED),TEXT=SP#MED                     
CLT      LKREQ F,5,(D,B#WORKD,QCLTX),(R,VALCLT),TEXT=SP#CLI                     
EST      LKREQ F,6,(D,B#SAVED,QBEST),LBIN,TEXT=SP#EST                           
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
                                                                                
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
         MVC   L.LOCKAGY,AGY                                                    
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
         MVC   L.LOCKAGY,AGY                                                    
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,SVMED                                                  
         MVC   L.LOCKCLT,SVCLT                                                  
         SR    R0,R0                                                            
         IC    R0,SVBEST                                                        
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
         XIT1                                                                   
         LTORG                                                                  
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE SPLNKRECS                                                      
         ORG   $SDAV2                                                           
$SDSPOTS DS    XL1                                                              
$SDDX2   EQU   *                                                                
                                                                                
MAXRECLN EQU   3972                SPTFIL MAXIMUM RECORD LENGTH                 
                                                                                
SAVED    DSECT                                                                  
                                                                                
STAMP    DS    CL(L'STAMPLIT)      OVERLAY STAMP                                
                                                                                
LINKIO   DS    A                   A(LINKIO)                                    
ALIOB    DS    A                   A(LINKIO CONTROL BLOCK)                      
RECUP    DS    A                   A(RECUP)                                     
RECADDR  DS    A                   A(RECORD HANDLING ROUTINE)                   
CKSUMBFR DS    F                                                                
*                                                                               
AGY      DS    CL(L'LP_AGY)        AGENCY CODE                                  
MAPNUM   DS    XL(L'LP_QMAPN)      RECORD MAP NUMBER                            
VERSION  DS    XL(L'LP_VRSN)       PC VERSION NUMBER                            
DATALEN  DS    H                   LENGTH OF INPUT DATA                         
ERRORFLG DS    X                                                                
FATALERR EQU   C'F'                FATAL ERROR, EXIT                            
ERRRGULR EQU   C'R'                REGULAR ERROR                                
ERRSTLCK EQU   C'L'                STATION IS LOCKED                            
*                                                                               
PAIDFLAG DS    C                                                                
*                                                                               
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVACTION DS    C                   SAVED THE ORIGINAL REQUEST ACTION            
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS 1                        
LOCLRNF  EQU   X'80'               LOCAL STATION NOT FOUND                      
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
SVNETWK  DS    CL4                 NETWORK CALL LETTERS                         
SVNETBTS DS    XL1                 NETWORK BINARY                               
SVMKTSTA DS    XL5                                                              
SVSFLAG1 DS    XL1                 SAVED STATION FLAG1                          
*                                                                               
SVBVALSX EQU   *                                                                
                                                                                
SVMGTKNS DS    (SVNWMGMX)XL(MGTKNLNQ)                                           
SVMGTKNX EQU   *                                                                
SVNWMGMX EQU   15                  MAXIMUM OF 15 NEW MAKEGOODS                  
*                                   OR IT WILL BLOW AWAY $BDMGCD                
SVMGCDTF DS    X                   SAVE MG CODE TABLE FLAG                      
SVMGCDTB DS    CL1024              SAVE MG CODE TABLE                           
*                                                                               
XTRATEXT DS    CL20                EXTRA ERROR MESSAGE TEXT                     
                                                                                
         DS    0D                                                               
XPRDLIST DS    XL256               PRODUCT CODES IN EXISTING BUYREC             
*                                  NEW PRODUCTS GO IN IOBRDLST                  
                                                                                
BUWEEK   DS    XL2                 SPOT WEEK START DATE                         
BUWEEKX  DS    XL2                 SPOT WEEK END DATE                           
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
SAVE1OR2 DS    X                                                                
*                                                                               
BUMASPR1 DS    CL3                                                              
BUMASPR2 DS    CL3                                                              
*                                                                               
DEMQMKT  DS    CL(L'NDMKTALF)                                                   
DEMQSTA  DS    CL5                                                              
DEMQRSV DS     CL1                                                              
*                                                                               
BSPACT   DS    C                   SPILL ACTION                                 
BSPADD   EQU   C'A'                - ADD                                        
BSPCHG   EQU   C'C'                - CHANGE                                     
BSPDEL   EQU   C'D'                - DELETE                                     
*                                                                               
BUBOOK   DS    XL3                 BINARY DATE                                  
BUBKTYPE DS    CL1                                                              
BUPROG   DS    CL16                                                             
         ORG   BUPROG                                                           
LKUPKEY  DS    CL16                                                             
*                                                                               
BUSPLAMK DS    XL2                                                              
BUSPLRMK DS    XL2                                                              
BUSPLBTY DS    CL1                                                              
*                                                                               
PACKOF4B DS    PL4                                                              
*                                                                               
TTLCLCST DS    F                                                                
*                                                                               
BUPCT    DS    F                                                                
BUCIND2  DS    X                                                                
BUCOST   DS    F                                                                
BUTIMST  DS    H                                                                
BUTIMND  DS    H                                                                
BUFLAGS  DS    X                                                                
BUGST    DS    C                                                                
BUPST    DS    CL10                                                             
BUEXPBUY DS    C                                                                
BUPROVCD DS    CL2                                                              
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
RCUPRTRN DS    X                   RECUP RETURN FLAG                            
                                                                                
QACTION  DS    C                   ** ACTION CODE **                            
QACTADD  EQU   C'A'                ADD A NEW BUY                                
QACTOVER EQU   C'O'                OVERWRITE A DELETED BUY                      
QACTCHA  EQU   C'C'                CHANGE AN EXISTING BUY                       
QACTDEL  EQU   C'D'                DELETE AN EXISTING BUY                       
QBEST    DS    X                   ESTIMATE NUMBER                              
QLIN     DS    XL2                 BUY LINE NUMBER                              
QMKT     DS    XL2                                                              
QSTA     DS    CL8                                                              
QBUYER   DS    CL12                BUYER CODE                                   
QCHKSUM  DS    F                   CHECK SUM                                    
QDSKADDR DS    F                                                                
QTOKEN   DS    CL5                                                              
QMON     DS    XL2                 SEE SPGENEST:ELOCKYM                         
QCONTRCT DS    CL12                                                             
QENDBUY  DS    C                                                                
Q68COUNT DS    X                                                                
QRETURN  DS    C                   IF C'Y', DO NOT RETURN BUYS                  
*                                                                               
         DS    0D                                                               
QBDELEM  DS    XL96                BUY DESCRIPTION DATA                         
                                                                                
QACOMM1  DS    A                   A(COMMENT LINE 1)                            
QACOMM2  DS    A                   A(COMMENT LINE 2)                            
QACOMM3  DS    A                   A(COMMENT LINE 3)                            
QACOMM4  DS    A                   A(COMMENT LINE 4)                            
QACOMM5  DS    A                   A(COMMENT LINE 5)                            
                                                                                
QADEMO   DS    A                   A(DEMO LIST)                                 
QASPOT   DS    A                   A(SPOT ARRAY)                                
                                                                                
SVPROGRM DS    CL(L'BDPROGRM)                                                   
SVPRGNM  DS    X                                                                
SVQCONT  DS    CL(L'IDCONNO)                                                    
SVCONNM  DS    X                                                                
SAVEDX   EQU   *                                                                
         EJECT                                                                  
* INCLUDED DSECTS FOLLOW                                                        
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
*                                                                               
IOBUY    EQU   IO3                                                              
AIOBUY   EQU   AIO3                                                             
IOBUYSV  EQU   IO5                                                              
AIOBUYSV EQU   AIO5                                                             
         EJECT                                                                  
MGTOKEND DSECT                                                                  
MGTOKEN  DS    CL2                                                              
MGCDALPH DS    CL2                                                              
MGCDBIN  DS    XL1                                                              
MGTKNLNQ EQU   *-MGTOKEND                                                       
*                                                                               
$BDD     DSECT                     ** BUY LINE DETAILS **                       
$BDELEM  EQU   $BDD                                                             
                                                                                
********************************** START OF BDELEM FIELD MAPPING                
$BDSTART DS    XL(L'BDSTART)       BUY START DATE                               
$BDEND   DS    XL(L'BDEND)         BUY END DATE                                 
$BDWKS   DS    XL(L'BDWKS)         NUMBER OF WEEKS                              
$BDINPUT DS    XL(L'BDINPUT)       INPUT METHOD                                 
$BDWKIND DS    XL(L'BDWKIND)       O=1/WK A=1/2WK T=1/3WK F=1/4WK               
$BDDAY   DS    XL(L'BDDAY)         DAY                                          
$BDNOWK  DS    XL(L'BDNOWK)        NUMBER OF SPOTS PER WEEK                     
$BDSEC   DS    XL(L'BDSEC)         SECONDS LENGTH                               
$BDTIME  DS    XL(L'BDTIME)        TIME PORTION FOR PIGGYBACK                   
$BDCOSTP DS    XL(L'BDCOSTP)       COST PORTION FOR PIGGYBACK                   
$BDDAYPT DS    XL(L'BDDAYPT)       DAYPART CODE                                 
$BDTIMST DS    XL(L'BDTIMST)       START TIME (MILITARY)                        
$BDTIMND DS    XL(L'BDTIMEND)      END TIME   (MILITARY)                        
$BDPROGR DS    XL(L'BDPROGRM)      PROGRAMMING                                  
$BDPROGT DS    XL(L'BDPROGT)       PROGRAM ADJACENCY CODE                       
         DS    XL(L'BDCOST)        SPOT COST PLACEHOLDER - SEE BELOW            
$BDCIND  DS    XL(L'BDCIND)        COST INDICATOR                               
         DS    XL(L'BDXFRAGY)                                                   
$BDNTAX  DS    XL(L'BDNTAX)        TAX RATE (3DP)                               
$BDWHY3  DS    XL(L'BDWHY3)        ACTIVTY BITS                                 
$BDREP   DS    XL(L'BDREP)         REP CODE                                     
$BDCHG   DS    XL(L'BDCHG)         REASON FOR LAST CHANGE                       
$BDWHY   DS    XL(L'BDWHY)         ACTIVITY BITS                                
$BDPURP  DS    XL(L'BDPURP)        PURPOSE CODE                                 
$BDSEDAY DS    XL(L'BDSEDAY)       START/END DAY                                
$BDCANAD DS    XL(L'BDCANAD)       C'C' FOR CANADIAN BUY                        
$BDCIND2 DS    XL(L'BDCIND2)       INDICATOR BYTE                               
$BDWHY2  DS    XL(L'BDWHY2)        ACTIVTY BITS                                 
$BDSTAT  DS    XL(L'BDSTAT)        STATUYS BITS                                 
$BDMGCD  DS    XL(L'BDMGDATE)      MAKEGOOD CODE                                
$BDSTAT3 DS    XL(L'BDSTAT3)       STATUS BITS                                  
$BDNRGN  DS    XL(L'BDNRGN)        NETWORK REGION CODE                          
$BDMGSPT DS    XL(L'BDMGSPOT)      MG SPOT NO FOR POOL MISSED MONTH             
$BDADVAG DS    XL(L'BDADVAGY)      ADVERTISER AGENCY CODE                       
$BDMSPRD DS    0XL(L'BDMASPRD)     ** MASTER PRODUCT CODE(S) **                 
         DS    X                   MASPRD1 PLACEHOLDER                          
         DS    X                   MASPRD2                                      
$BDSTAT2 DS    XL(L'BDSTAT2)       STATUS BITS                                  
$BDSTAT4 DS    XL(L'BDSTAT4)       STATUS BITS                                  
                                                                                
$BDMST1C DS    CL(L'EKEYPRD)       MASTER PRODUCT 1 CODE                        
$BDMST2C DS    CL(L'EKEYPRD)       MASTER PRODUCT 2 CODE                        
$BDCON#  DS    CL(L'IDCONNO)       CONTRACT NUMBER                              
$BDCODE  DS    CL5                 PROGRAM CODE                                 
$BDCOST  DS    CL4                 COST                                         
$BDELEMX EQU   *                                                                
                                                                                
*                                                                               
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPMGADN                                                        
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE SPBUYVALD                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
                                                                                
WORKD    DSECT                                                                  
         ORG   OVERWORK            REDEFINE 1K OVERLAY WORKING STORAGE          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125SPLNK23   09/12/17'                                      
         END                                                                    
