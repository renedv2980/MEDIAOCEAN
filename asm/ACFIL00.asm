*          DATA SET ACFIL00    AT LEVEL 025 AS OF 05/21/20                      
*&&      SET   NOP=N                                                            
*PHASE T62300A                                                                  
*INCLUDE GEFILSET                                                               
*INCLUDE ACSRCHC                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE VATICAN                                                                
*INCLUDE BMONVAL                                                                
*INCLUDE ACSRCHP                                                                
*INCLUDE SRCHPASS                                                               
*INCLUDE AC1RMNT                                                                
*INCLUDE ACRAPPER                                                               
*INCLUDE CONVERT                                                                
*INCLUDE PUBVAL                                                                 
*INCLUDE VEMAIL                                                                 
*INCLUDE NUMVAL                                                                 
***********************************************************************         
* RGUP 024 SPEC-46141 RELINK TO PULL IN NEW VER OF DDVEMAIL .ME EXT   *         
***********************************************************************         
FIL00    TITLE 'FILE PROGRAM ROOT'                                              
FIL00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**ACF0**,CLEAR=YES,RR=RE                             
                                                                                
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         ST    RE,BCRELO                                                        
                                                                                
         L     RE,20(R1)                                                        
         MVC   FACFLAG,7(RE)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(RE)       AND UPDATIVE FACPAK ID                       
         MVC   FACFLAG2,22(RE)       SAVE 2ND CONNECT FLAG                      
                                                                                
         L     RF,VFILSET                                                       
         A     RF,BCRELO                                                        
         GOTOR (RF),BCPARM                                                      
*                                                                               
         USING TSARD,RF            CAN'T SET THIS BIT IN GEFIL00                
         L     RF,ATSABLK          BECAUSE IT CAUSED PROBLEMS FOR REP           
         OI    TSINDS,TSIXTTWA                                                  
         DROP  RF                                                               
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(FILE PROGRAM W/S)                       
                                                                                
         MVC   GSDSPMAX,=Y(TWSAVE-TWAD)                                         
         MVC   GSDSPREC,=Y(BASRECH-TWAD)                                        
         MVC   GSDSPACT,=Y(BASACTH-TWAD)                                        
         MVC   GSDSPSCR,=Y(BASSCRH-TWAD)                                        
*                                                                               
* SUPPRESSED THE PAGE FIELD BECAUSE WHEN YOU ARE ON THE 2ND PAGE OF THE         
* ACCOUNT RECORDS THE PAGE FIELD WOULD DISAPPEAR BECAUSE OF THE FACT            
* THAT THERE IS ONLY ONE PAGE 1 SCREEN RECORD DEFINED                           
*                                                                               
*        MVC   GSDSPPAG,=Y(BASPAGH-TWAD)                                        
         MVC   GSDSPSAV,=Y(BASOPTH-TWAD)                                        
         MVC   GSDSPOPT,=Y(BASOPTH-TWAD)                                        
         MVC   GSDSPOVR,=Y(BASOLY1H-TWAD)                                       
                                                                                
         LA    RF,NTRRET           SET NTRSES RETURN POINTS                     
         ST    RF,GCANTR                                                        
         LA    RF,VALREC                                                        
         ST    RF,GCAXIT                                                        
         LA    RF,XITMSG                                                        
         ST    RF,GCAEXIT                                                       
                                                                                
         LA    R2,PHASES           R2=A(PHASE LIST)                             
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAXIMUM NUMBER OF PHASES                  
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,BCPARM                                                        
         L     RF,VCOLY                                                         
INITAD02 CLI   0(R2),FF                                                         
         BE    INITAD06                                                         
         ICM   R0,1,0(R2)                                                       
         BZ    INITAD04                                                         
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(L'APHASES,R3),0(R1)                                            
INITAD04 LA    R2,1(R2)                                                         
         LA    R3,L'APHASES(R3)                                                 
         BCT   R4,INITAD02                                                      
                                                                                
INITAD06 LA    R0,ROUT             SET CONTROL ROUTINE ADDRESSES                
         ST    R0,BCFULL                                                        
         LA    R0,ADDRCDAN                                                      
         XR    RF,RF                                                            
         LA    R1,ADDRCDA                                                       
         BASR  RE,0                                                             
         MVC   0(L'ADDRCDA,R1),BCFULL                                           
         STC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         LA    R1,L'ADDRCDA(R1)                                                 
         BCTR  R0,RE                                                            
                                                                                
         LA    R0,ADDRS1N          SET CONTROLLER ADDRESSES 1                   
         SR    RE,RE               RE=INDEX TO ADDRESS VALUE                    
         LA    RF,ADDRS1                                                        
INITAD08 L     R1,0(RF,RE)                                                      
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         A     R1,BCRELO           RELOCATE AND STORE IN W/S                    
         ST    R1,AADDRS1(RE)                                                   
         LA    RE,L'ADDRS1(RE)                                                  
         BCT   R0,INITAD08                                                      
                                                                                
         LA    R0,ADDRS2N          SET CONTROLLER ADDRESSES 2                   
         SR    RF,RF               RF=INDEX TO ADDRESS VALUE                    
         BASR  RE,0                                                             
         L     R1,ADDRS2(RF)                                                    
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         A     R1,BCRELO           RELOCATE AND STORE IN W/S                    
         ST    R1,AADDRS2(RF)                                                   
         LA    RF,L'ADDRS2(RF)                                                  
         BCTR  R0,RE                                                            
                                                                                
INITAD10 LA    R1,ANAWS            SET SUNDRY STORAGE ADDRESSES                 
         LA    R0,ANAWSN                                                        
INITAD12 ICM   RE,15,0(R1)         RE=AL2(AREA),AL2(ADDRESS)                    
         SRDL  RE,16                                                            
         SRL   RF,16                                                            
         LA    RE,WORKD(RE)                                                     
         LA    RF,WORKD(RF)                                                     
         STCM  RE,15,0(RF)         SET AREA ADDRESS                             
         LA    R1,L'ANAWS(R1)                                                   
         BCT   R0,INITAD12                                                      
                                                                                
         LA    R0,BCLDGTAB         SET A(LEDGER TABLE)                          
         STCM  R0,15,ACALDG                                                     
         MVC   AIO,AGROUTS         BUILD AIO CALLING WORD                       
         MVI   AIO,XIO                                                          
                                                                                
INITADX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE GENERAL VALUES                                           *         
***********************************************************************         
                                                                                
INITGV   L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         MVC   BCDTADSP,=Y(ACCORFST)                                            
         TM    TWAINDS1,TW1INIT                                                 
         BNZ   INITGV04                                                         
*                                                                               
         GOTOR VDICTAT,BCPARM,C'LU  ',DICGEN,BC@YES                             
         GOTOR VDATCON,BCPARM,(7,0),('FF',0)                                    
*        XR    RE,RE                                                            
*        ICM   RE,7,5(R1)                                                       
*        MVC   BCMONTHS,0(RE)                                                   
         GOTOR (RF),(R1),(5,0),(0,BCWORK)                                       
         GOTOR (RF),(R1),(0,BCWORK),(1,BCTODAYP)                                
         GOTOR (RF),(R1),,(2,BCTODAYC)                                          
         GOTOR (RF),(R1),,(3,BCTODAYB)                                          
                                                                                
         LA    R2,IOKEY            READ COMPANY RECORD & EXTRACT VALUES         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTOR AIO                                                              
         BNE   INITGV04                                                         
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1             LOCATE COMPANY ELEMENT                       
         LA    R1,CPYRFST                                                       
         USING CPYEL,R1                                                         
         SR    R0,R0                                                            
INITGV02 CLI   CPYEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    R1,R0                                                            
         B     INITGV02                                                         
         SR    RE,RE               EXTRACT COMPANY ELEMENT                      
         ICM   RE,1,CPYLN                                                       
         CLM   RE,1,=AL1(L'BCCPYEL)                                             
         BNH   *+8                                                              
         LA    RE,L'BCCPYEL                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCCPYEL(0),CPYELD                                                
                                                                                
         TM    CPYSTAT1,CPYSOROE   SET LENGTH OF OFFICE CODE                    
         BZ    *+8                                                              
         MVI   BCOFFLEN,1                                                       
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+8                                                              
         MVI   BCOFFLEN,2                                                       
                                                                                
         MVI   BCDPTLEN,2          SET LENGTH OF DEPARTMENT CODE                
         CLI   CPYDEPTL,0                                                       
         BE    *+10                                                             
         MVC   BCDPTLEN,CPYDEPTL                                                
                                                                                
         OC    CPYPROD,CPYPROD                                                  
         BZ    INITGV04                                                         
         LA    R2,IOKEY            READ PRODUCTION LEDGER RECORD                
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'CPYPROD),CPYPROD                                       
         GOTO1 AGETLDG                                                          
         BNE   INITGV04                                                         
         ICM   RF,15,ACALDG        SET L'CLIENT, PRODUCT AND JOB CODES          
         USING LDGTABD,RF                                                       
         MVC   BCCLILEN(L'BCCLILEN+L'BCPROLEN+L'BCJOBLEN),LDGTLVA               
         DROP  RF                                                               
                                                                                
INITGV04 L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL BLOCK)                            
         MVC   OFFACOMF,ACOM                                                    
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,CUABIN                                                   
         MVC   OFFACST1,BCCPYST1                                                
         MVC   OFFACST2,BCCPYST2                                                
         MVC   OFFACST3,BCCPYST3                                                
         MVC   OFFACST4,BCCPYST4                                                
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF   OFFICE SECURITY CHECK                        
         MVI   OFFAACT,OFFAINI     INITIALIZE                                   
         OC    OFFASAV(OFFASAVL),BCOFFSAV                                       
         BZ    *+8                                                              
         MVI   OFFAACT,OFFARES     RESTORE                                      
         GOTOR VOFFAL                                                           
         BE    INITGV05                                                         
         CLC   TWAACCS,BCSPACES    IF AN OPEN ID (NO LIMITED ACCESS)            
         BNH   *+6                 AND NO OFFICES FOUND THAN LET IT             
         DC    H'0'                GO THROUGH                                   
         TM    OFFAERR,OFFAESEC    SECURITY LOCKOUT?                            
         BO    *+6                                                              
         DC    H'0'                                                             
INITGV05 MVC   BCOFFSAV,OFFASAV    SAVE OFFAL VALUES                            
         DROP  R1                                                               
                                                                                
         TM    TWAINDS1,TW1INIT    TEST FIRST TIME                              
         BZ    INITGV06                                                         
         GOTOX ('RESVAL',AGROUTS)  RESTORE SAVED VALUES                         
         XC    NSSAV,NSSAV         CLEAR NEXT SESSION SAVE AREA                 
                                                                                
INITGV06 OI    TWAINDS1,TW1INIT    SET INITIALIZED                              
         MVI   GCSWSYSN,QSACC      DEFAULT IS ACC SYSTEM                        
         MVI   GCSWSYSC,QSACC                                                   
         MVI   GCSWSYSP,QSACC                                                   
                                                                                
         CLI   GSSYS,0             TEST SYSTEM ALREADY SET                      
         BNE   *+8                                                              
         MVI   GSSYS,QSACC         SET ACC AS DEFAULT                           
         MVC   GCFILNAM,ACCMST     SET ACCMST AS FILE NAME                      
         MVI   GCOVSYS,QSACC       FASYSTAB ENTRY FOR LOGON SYSTEM              
         MVI   GCPRGNO,X'23'       FAPGMTAB ENTRY FOR THIS PROGRAM              
         GOTOX ('SWCHFC',AGROUTS),GSSYS MAKE SURE SWITCH SYSTEM                 
*        GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)  SWITCH TO CONTROL SYS            
*        BE    INITGV10                                                         
*                                                                               
*        MVC   BASMSG(28),=CL28'ERROR: NO CONTROL ACCESS YET'                   
*        OI    BASMSGH+6,X'80'                                                  
*        OI    BASRECH+1,X'20'     CHANGE TO PROTECTED                          
*        OI    BASACTH+1,X'20'                                                  
*        OI    BASRECH+6,X'80'                                                  
*        OI    BASACTH+6,X'80'                                                  
*        B     EXIT                                                             
*                                                                               
*NITGV10 DS    0H                                                               
*        GOTOX ('SWCHFC',AGROUTS),GSSYS MAKE SURE SWITCH SYSTEM BACK            
                                                                                
         OC    CSOVER,CSOVER       LOAD OVERLAY IF RESOLVED                     
         BZ    INITX                                                            
         GOTOX ('OVRLAY',AGROUTS),CSOVER                                        
                                                                                
INITX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD, ACTION, OPTIONS AND PROCESS                        *         
***********************************************************************         
                                                                                
TSTINP   CLI   BASRECH+FHILD,0                                                  
         BNE   INIT10                                                           
         CLI   BASACTH+FHILD,0                                                  
         BNE   INIT10                                                           
         TM    BCINDS1,BCIANYPF                                                 
         BNZ   INIT10                                                           
         LA    R0,BASRECH          NO RECORD/ACTION OR PFKEY THIS TIME          
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AC#ERF-DD#DISK)                                     
         MVI   FVOMTYP,GTMDIC                                                   
         MVI   CSACT,0                                                          
         B     XITMSG                                                           
                                                                                
INIT10   MVC   SNLSAVE,TWASESNL    SAVE SESSION LEVEL                           
         MVI   PFKPEND,0           NO PFKEY PENDING                             
         CLI   BCPFKEY,0           PFKEY ACTIVE?                                
         BE    VALPFK              NO, SKIP                                     
         L     R1,AINP                                                          
         CLC   TIOBLAST-TIOBD(,R1),=Y(BASOLY1H-TWAD)                            
         BL    VALPFK              ACTION PF IF NO DATA FIELD CHANGED           
         MVC   PFKPEND,BCPFKEY     ELSE SAVE PFKEY AS PENDING                   
         MVI   BCPFKEY,0           CANCEL PFKEY FOR NOW                         
         NI    BCINDS1,FF-BCIANYPF SO WE VALIDATE NEW INPUT FIRST               
                                                                                
VALPFK   GOTOR AGEN,BCPARM,('GCBOVER',OPFK),PFVAL                               
         BNE   XITMSG                                                           
*                                                                               
VALREC   DS    0H                  ** XITSES CALL RETURN POINT **               
         GOTOR AGEN,BCPARM,ORTYPE,RTVAL                                         
         BNE   XITMSG                                                           
                                                                                
VALACT   DS    0H                                                               
         CLI   CSACT,0             NO ACTION YET?                               
         BE    VACT10                                                           
         ZIC   R1,BASACTH+5                                                     
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VACT10                                                           
         EXMVC R1,BCWORK,BASACT                                                 
         OC    BCWORK,BCSPACES     MAKE SURE UPPERCASE                          
         EXCLC R1,BCWORK,=CL8'DISPLAY'   IF UPDATIVE ACTION CHECK               
         BE    VACT10                       FOR SOX ACCESS.  DISPLAY,           
         EXCLC R1,BCWORK,=CL8'DOWNLOAD'  DOWNLOAD, REPORT, LIST,                
         BE    VACT10                       SELECT AND ACTIVITY ARE NOT         
         EXCLC R1,BCWORK,=CL8'REPORT'    UPDATIVE.                              
         BE    VACT10                                                           
         EXCLC R1,BCWORK,=CL8'LIST'                                             
         BE    VACT10                                                           
         EXCLC R1,BCWORK,=CL8'SELECT'                                           
         BE    VACT10                                                           
         EXCLC R1,BCWORK,=CL8'ACTIVITY'                                         
         BE    VACT10                                                           
         TM    FACFLAG2,XICSCADV   CONNECTED TO CSC?                            
         BO    *+12                YES CONTINUE WITH SOX CHECK                  
         TM    FACFLAG,XITSTADV    IF NOT CSC BUT TEST FACPAK DON'T             
         BO    VACT10              BOTHER WITH CHECK.                           
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    VACT10                                                           
         MVI   FVOMTYP,GTMERR      SET ERROR TYPE MESSAGE                       
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    ERRNAUPD            YES                                          
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    ERRHMADV            YES                                          
         B     ERRNADVU            CONNECTED TO READ ONLY SYSTEM                
*                                                                               
ERRNAUPD MVC   FVMSGNO,=AL2(AE$NAUPD)                                           
         B     XITMSG                                                           
ERRHMADV MVC   FVMSGNO,=AL2(AE$HMADV)                                           
         MVC   FVXTRA(L'FACUPD),FACUPD                                          
         B     XITMSG                                                           
ERRNADVU MVC   FVMSGNO,=AL2(AE$UPDNO)                                           
         B     XITMSG                                                           
*                                                                               
VACT10   GOTOR AGEN,BCPARM,OACT,AVAL                                            
         BNE   XITMSG                                                           
                                                                                
         TM    BCINDS1,BCINREC+BCINACT IF NOT CHANGE OF REC OR ACTN             
         BZ    TSTOPT              SKIP OPTION HELP SET                         
                                                                                
NTRRET   DS    0H                  ** NTRSES CALL RETURN POINT **               
         MVI   LSSUBHLP,0          RESET SUBACTION HELP PANEL                   
         MVI   LSLSTSCR,0                                                       
         CLI   CSREC,10            RECORD NUMBERS LESS THAN 10 ARE              
         BL    TSTOPT              CONTROLLER SPECIALS (E.G. HELP)              
         MVI   BASOPTX,1           RESET OPTION HELP FIELD (AGAIN)              
         MVI   BASOPTX+4,FF        AND SCREEN NUMBER                            
         GOTOR AGEN,BCPARM,('GCBOVER',OOPT),OHLP,(CSREC,0) SET OPT HELP         
         BNE   TSTOPT              EQ=OPTIONS USED, FVIXNU MAY BE RESET         
         MVC   BASOPTX(1),BCPARM+8 SET HELP NUMBER (DEFAULTS TO CSREC)          
                                                                                
TSTOPT   CLI   BASOPTX,1           IF HELP SET FOR OPTIONS NOT USED             
         BNE   PRCACT                                                           
         CLI   BASOPTX+4,FF        CHECK SREEN NUMBER TOO                       
         BNE   PRCACT                                                           
         CLI   BASOPTH+5,0         MAKE SURE NO OPTIONS ENTERED                 
         BE    PRCACT              SKIP IF NO DATA IN OPTIONS                   
         CLC   BASOPT,BCSPACES                                                  
         BNH   PRCACT                                                           
         LA    R0,BASOPTH          ELSE 'NO OPTION ALLOWED'                     
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$NOOPT)                                           
         B     XITMSG                                                           
                                                                                
PRCACT   GOTOR AGEN,BCPARM,OACT,APRC                                            
         BNE   XITMSG              SKIP IF ERROR                                
         CLI   PFKPEND,0           DID WE SAVE A PFKEY AS PENDING?              
         BE    XITMSG              NO, SKIP                                     
         CLC   SNLSAVE,TWASESNL    HAS SESSION LEVEL CHANGED?                   
         BNE   XITMSG              YES, IGNORE PENDING PFKEY                    
         MVC   BCPFKEY,PFKPEND     RESTORE PENDING PFKEY                        
         OI    BCINDS1,BCIANYPF                                                 
         MVI   PFKPEND,0           CLEAR PENDING (OTHERWISE WE LOOP)            
         B     VALPFK              GO ROUND AGAIN TO HANDLE PFKEY               
*                                                                               
XITMSG   DS    0H                  ** EXIT CALL RETURN POINT **                 
         TM    GCINDS2,GCIXITS     IF ACCOUNT RECORD AND JUST RETURNED          
         BZ    XITMSG2             FROM ACTIVITY/DISPLAY THEN MAY NEED          
         CLI   NSREC,O#ACV         TO SET FVMSGNO B/C IF ZERO'S WILL            
         BNE   XITMSG2             DISPLAY THE INCORRECT MSG 'ORDER NOW         
         CLI   CSREC,X'19'         MARKED AS PREVIOUS'                          
         BNE   XITMSG2                                                          
         OC    FVMSGNO,FVMSGNO                                                  
         BNZ   XITMSG2                                                          
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         MVC   FVMSGNO,=AL2(AI$PSRES)    'PREVIOUS SESSION RESTORED'            
XITMSG2  GOTOX ('SETMSG',AGROUTS)                                               
         GOTOR AGEN,BCPARM,OPFK,PFBLD                                           
         GOTOX ('SAVVAL',AGROUTS)                                               
                                                                                
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    TSTINP2                                                          
         NI    CSINDSG1,FF-CSINDUNW                                             
         DC    H'0',C'$ABEND'      UNWIND RECORDS AND PASSIVES                  
                                                                                
*                                                                               
TSTINP2  TM    GENINDS,GENIENTR    REQUEST TO PRESS ENTER?                      
         BZ    EXIT                NO - EXIT                                    
         MVI   CSACT,A#CHA         THIS IS FOR WHEN ADDING ACCOUNT RECS         
         SR    R3,R3               AND USER HITS PF8 TO ADD PROFILES            
         ICM   R3,3,GSDSPACT       ON 2ND PAGE-ADDS REC & HITS PF8 &            
         A     R3,ATWA             CHANGES THE ACTION TO CHANGE ALL IN          
         USING FHD,R3              ONE KEYSTROKE                                
         MVC   FHDA(6),=CL6'CHANGE'                                             
TSTINP5  L     RD,4(,RD)           UNWIND TO CALLER                             
         LM    RE,RC,12(RD)                                                     
         BR    RF                  RE-ENTER AT TOP AS IF ENTER PRESSED          
                                                                                
EXIT     NI    GENINDS,X'FF'-(GENIREP+GENIDLD)                                  
         CLI   ASONOFF,ASOFF     RUNNING OFFLINE?                               
         BNE   *+14              NO                                             
         L     RF,ASECBLK        SPOOF REDEFINES TWAUSER - SAVE IT              
         MVC   0(L'TWAUSER,RF),TWAUSER                                          
         XIT1  ,                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
                                                                                
FF       EQU   X'FF'                                                            
                                                                                
         LTORG                                                                  
                                                                                
VFILSET  DC    V(GEFILSET)                                                      
                                                                                
ACCMST   DC    C'ACCMST  '                                                      
                                                                                
DICUPR   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
DICUPRX  DC    AL1(EOT)            512 BYTES MAX                                
                                                                                
DICMIX   DS    0XL4                ** MIXED CASE DICTIONARY LIST **             
DICMIXX  DC    AL1(EOT)                                                         
                                                                                
ADDRS2   DS    0A                  ** CONTROLLER ADDRESSES 2 **                 
         DC    (ADDRS2N)A(0)                                                    
         ORG   ADDRS2+(ADICUPR-AADDRS2)                                         
         DC    A(DICUPR)                                                        
         ORG   ADDRS2+(ADICMIX-AADDRS2)                                         
         DC    A(DICMIX)                                                        
         ORG                                                                    
                                                                                
ANAWS    DS    0AL4                ** A(NON-ADDRESSABLE W/S AREAS) **           
         DC    AL2(GOPBLK-WORKD,AGOPBLK-WORKD)                                  
         DC    AL2(GOXBLK-WORKD,AGOXBLK-WORKD)                                  
         DC    AL2(OFFBLK-WORKD,AOFFBLK-WORKD)                                  
         DC    AL2(VATBLK-WORKD,AVATBLK-WORKD)                                  
         DC    AL2(ADTBLK-WORKD,AADTBLK-WORKD)                                  
         DC    AL2(JOBBLK-WORKD,AJOBBLK-WORKD)                                  
ANAWSN   EQU   (*-ANAWS)/L'ANAWS                                                
                                                                                
DICGEN   DS     0XL4               ** UPPER CASE DICTIONARY LIST **             
         DCDDL AC#YES,L'BC@YES                                                  
         DCDDL AC#NO,L'BC@NO                                                    
         DCDDL AC#DR,L'BC@DR                                                    
         DCDDL AC#CR,L'BC@CR                                                    
         DCDDL AC#ALL,L'BC@ALL                                                  
         DCDDL AC#ZERO,L'BC@ZERO                                                
         DCDDL AC#ONLY,L'BC@ONLY                                                
         DCDDL AC#DEL,L'BC@DEL                                                  
         DCDDL AC#NONE,L'BC@NONE                                                
         DCDDL AC#DEF,L'BC@DEF              DEFAULT                             
         DCDDL AC#COMP,L'BC@COMP            COMPULSORY                          
         DCDDL AC#OPTNL,L'BC@OPTL           OPTIONAL                            
DICGENX  DC    AL1(EOT)                                                         
                                                                                
PHASES   DS    0AL1                ** LOADED PHASE LIST **                      
         DC    AL1(QADDTRN)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QOFFAL)                                                      
         DC    AL1(0)              UNAVAILABLE SEE APHASES IN GEGENWORK         
         DC    AL1(0)                   "       "    "      "  "                
         DC    AL1(0)                                                           
         DC    AL1(QSQUASH)                                                     
         DC    AL1(0)              N/D                                          
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QGETOPT)                                                     
         DC    AL1(0)              N/D                                          
         DC    AL1(0)              N/D                                          
         DC    AL1(0)              N/D                                          
         DC    AL1(QQSORT)                                                      
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(0)              USED BY VGETIDS IN GEGENWORK                 
         DC    AL1(QGETBANK)                                                    
         DC    AL1(QPADDLE)                                                     
PHASESN  EQU   *-PHASES                                                         
                                                                                
ADDRS1   DS    0A                  ** CONTROLLER ADDRESSES 1 **                 
         DC    V(CONVERT)                                                       
         DC    V(ADSCAN)                                                        
         DC    V(BMONVAL)                                                       
         DC    V(VATICAN)                                                       
         DC    V(ACSRCHC)                                                       
         DC    V(ACSRCHP)                                                       
         DC    V(AC1RMNT)                                                       
         DC    V(ACRAPPER)                                                      
         DC    V(PUBVAL)                                                        
         DC    V(SRCHCALL)                                                      
         DC    V(VEMAIL)                                                        
         DC    V(NUMVAL)                                                        
         ORG   ADDRS1+(L'ADDRS1*ADDRS1N)                                        
         EJECT                                                                  
***********************************************************************         
* ACCPAK SPECIFIC CALLABLE ROUTINES                                   *         
***********************************************************************         
                                                                                
ROUT     DS    0D                                                               
         NMOD1 300,**ROUT**                                                     
         L     R8,AGWORK                                                        
         USING GWORKD,R8           R8=A(FILE PROGRAM W/S)                       
                                                                                
         SRL   RF,32-8                                                          
         CHI   RF,ROUTSN-1         TEST FOR VALID ROUTINE NUMBER                
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
                                                                                
ROUTS    DS    0XL4                ** ROUTINE BRANCH TABLE **                   
         B     GETACT              GET ACCOUNT/TEST SECURITY                    
         B     TSTSEC              TEST ACCOUNT SECURITY                        
         B     GETLDG              GET A LEDGER RECORD                          
         B     GETELS              GET ELEMENT ADDRESSES                        
         B     VALDOPT             VALIDATE DELETE OPTION                       
         B     CHKFLD              CHECK INVALID CHARS IN THE FVIFLD            
         B     TSTOFF              TEST OFFICE SECURITY                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
                                                                                
ROUTL    CLI   *+0,FF              SET CC=LOW                                   
         B     ROUTX                                                            
ROUTH    CLI   *+0,0               SET CC=EQUAL                                 
         B     ROUTX                                                            
ROUTE    CLI   *+1,0               SET CC=HIGH                                  
                                                                                
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AN ACCOUNT - NEW FILE                                *         
*                                                                     *         
* NTRY - IOKEY=ACCOUNT RECORD KEY                                     *         
*        R1=A(LIST OF VALID LEDGERS OR ZERO)  ***NOT ANYMORE          *         
*        IF R1=1 THAN READ FOR DELETES IF R1=0 DO NOT READ FOR DELETES*         
***********************************************************************         
                                                                                
         USING GAWORKD,RC                                                       
GETACT   MVC   BOWORK2,BCSPACES                                                 
         MVC   BOWORK2(L'IOKEY),IOKEY      SAVE KEY                             
         STC   R1,BCBYTE1                                                       
         LA    R3,BOWORK2                                                       
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         GOTO1 AGETLDG             CHECK LEDGER IS VALID                        
         BL    ROUTL                                                            
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4          R4=A(LEDGER TABLE ENTRY)                     
         MVC   GASAVULA,1(R3)      LEAVE OUT COMPANY CODE                       
*        MVC   ACTKCPY,CUABIN                                                   
*                                                                               
* 1ST LEVEL                                                                     
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKACT-ACTRECD),0(R3)                                     
         ZIC   R1,LDGTLVA                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+(ACTKACT-ACTRECD)(0),ACTKACT-ACTRECD(R3)                   
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         CLI   CSREC,X'19'         ACCOUNT,APGRULES,FEE OR LEDGER REC?          
         BE    GETAC10             IF SO THAN READ FOR DELETES B/C THEY         
         CLI   CSREC,X'14'         ARE ACCT TYPE RECS SO NEED TO READ           
         BE    GETAC10             FOR DELETES IN ORDER TO DISPLAY OR           
         CLI   CSREC,X'1A'         RESTORE A DELETED RECORD.                    
         BE    GETAC10                                                          
         CLI   CSREC,X'26'                                                      
         BNE   *+12                                                             
GETAC10  CLI   BCBYTE1,1           DO WE WANT TO READ FOR DELETES?              
         BE    GETAC12             YES                                          
         GOTO1 AIO                                                              
         BL    ROUTL                                                            
         BH    GETACTIA                                                         
         B     GETAC15                                                          
*                                                                               
GETAC12  L     R1,=AL4(XORDD+XOACCDIR+XIO1)     READ FOR DELETES                
         GOTO1 AIO                                                              
         BL    ROUTL                                                            
         BE    GETAC15                                                          
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    GETAC15                                                          
         B     GETACTIA            NO SOMETHING ELSE MUST BE WRONG              
*                                                                               
GETAC15  LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             R2=A(ACCOUNT RECORD)                         
         SR    R5,R5                                                            
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),ACTKUNT                      
         BNE   *+8                 OK - NOT SJ                                  
         LA    R5,PSCLIPPR         A(CLIENT PROFILE AREA)                       
         GOTO1 AGETELS,BCPARM,ACTRFST,(R5)                                      
*                                                                               
* 2ND LEVEL                                                                     
         MVC   IOKEY(L'ACTKEY),BCSPACES                                         
         MVC   IOKEY(ACTKACT-ACTRECD),0(R3)                                     
         ZIC   R1,LDGTLVB                                                       
         LTR   R1,R1                                                            
         BZ    GETAC50             MUST BE 1 LEVEL LEDGER                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+(ACTKACT-ACTRECD)(0),ACTKACT-ACTRECD(R3)                   
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         CLI   CSREC,X'19'         ACCOUNT,APGRULES,FEE OR LEDGER REC?          
         BE    GETAC20             IF SO THAN READ FOR DELETES B/C THEY         
         CLI   CSREC,X'14'         ARE ACCT TYPE RECS SO NEED TO READ           
         BE    GETAC20             FOR DELETES IN ORDER TO DISPLAY OR           
         CLI   CSREC,X'1A'         RESTORE A DELETED RECORD.                    
         BE    GETAC20                                                          
         CLI   CSREC,X'26'                                                      
         BNE   *+12                                                             
GETAC20  CLI   BCBYTE1,1           DO WE WANT TO READ FOR DELETES?              
         BE    GETAC22             YES                                          
         GOTO1 AIO                                                              
         BL    ROUTL                                                            
         BH    GETACTIA                                                         
         B     GETAC25                                                          
*                                                                               
GETAC22  L     R1,=AL4(XORDD+XOACCDIR+XIO1)     READ FOR DELETES                
         GOTO1 AIO                                                              
         BL    ROUTL                                                            
         BE    GETAC25                                                          
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    GETAC25                                                          
         B     GETACTIA            NO SOMETHING ELSE MUST BE WRONG              
*                                                                               
GETAC25  LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1             R2=A(ACCOUNT RECORD)                         
         SR    R5,R5                                                            
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),ACTKUNT                      
         BNE   *+8                 OK - NOT SJ                                  
         LA    R5,PSPROPPR         A(CLIENT PROFILE AREA)                       
         GOTO1 AGETELS,BCPARM,ACTRFST,(R5)                                      
*                                                                               
* SUBSEQUENT LEVELS                                                             
         MVC   IOKEY(L'ACTKEY),0(R3)                                            
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         CLI   CSREC,X'19'         ACCOUNT,APGRULES,FEE OR LEDGER REC?          
         BE    GETAC30             IF SO THAN READ FOR DELETES B/C THEY         
         CLI   CSREC,X'14'         ARE ACCT TYPE RECS SO NEED TO READ           
         BE    GETAC30             FOR DELETES IN ORDER TO DISPLAY OR           
         CLI   CSREC,X'1A'         RESTORE A DELETED RECORD.                    
         BE    GETAC30                                                          
         CLI   CSREC,X'26'                                                      
         BNE   *+12                                                             
GETAC30  CLI   BCBYTE1,1           DO WE WANT TO READ FOR DELETES?              
         BE    GETAC32             YES                                          
         GOTO1 AIO                                                              
         BL    ROUTL                                                            
         BH    GETACTIA                                                         
         B     GETAC40                                                          
*                                                                               
GETAC32  L     R1,=AL4(XORDD+XOACCDIR+XIO1)     READ FOR DELETES                
         GOTO1 AIO                                                              
         BL    ROUTL                                                            
         BE    GETAC40                                                          
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    GETAC40                                                          
         B     GETACTIA            NO SOMETHING ELSE MUST BE WRONG              
*                                                                               
GETAC40  LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING ACTRECD,R2          R2=A(ACCOUNT RECORD)                         
         GOTO1 AGETELS,BCPARM,ACTRFST,0                                         
*                                                                               
GETAC50  MVC   IOKEY,BOWORK2    RESTORE KEY                                     
         L     RF,AOFFBLK                                                       
         OI    OFFACTRL-OFFALD(RF),OFFACCNV                                     
         GOTOR ATSTSEC             TEST ACCOUNT SECURITY                        
         B     ROUTX                                                            
                                                                                
GETACTIA MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     GETACTIX                                                         
                                                                                
GETACTIL MVC   FVMSGNO,=AL2(AE$INLDG)                                           
                                                                                
GETACTIX MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'GASAVULA),GASAVULA                                      
         B     ROUTH                                                            
         DROP  R2,R4,RC                                                         
                                                                                
GAWORKD  DSECT                     ** GETACC/GETACT LOCAL W/S **                
GASAVULA DS    CL(L'ACTKULA)      SAVED UNIT/LEDGER/ACCOUNT                     
FIL00    CSECT                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TEST ACCOUNT SECURITY                                               *         
* SINCE CALLING TSTSEC DIRECTLY (INSTEAD OF GOING THROUGH GETACT AND  *         
* GETLDG ROUTINES FOR ACFIL19, ACFIL14 & ACFIL1D I NEED TO MAKE SURE  *         
* THAT ACALDG IS POINTING TO THE CORRECT LEDGER TABLE ENTRY           *         
***********************************************************************         
         SPACE 1                                                                
TSTSEC   DS    0H                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
*        ICM   R2,15,ACALDG                                                     
         USING LDGTABD,R2          R2=A(LEDGER TABLE ENTRY)                     
         LA    R0,LDGTMAXN         MAX # OF LEDGER TABLE ENTRIES                
         LA    R2,BCLDGTAB                                                      
TSTSEC01 CLC   LDGTUL,IOKEY+1      SAME LEDGER?                                 
         BE    TSTSEC02                                                         
         LA    R2,LDGTABL(R2)                                                   
         BCT   R0,TSTSEC01                                                      
         DC    H'0'                                                             
TSTSEC02 STCM  R2,15,ACALDG        STORE THE ADDRESS                            
*                                                                               
         CLC   LDGTSEC,TWAAUTH+1   TEST SECURITY LEVEL                          
         BH    TSTSECSL                                                         
         CLC   ACSECY,TWAAUTH+1                                                 
         BH    TSTSECSL                                                         
         CLC   IOKEY+3(L'ACTKACT),BCSPACES  IF LEDGER REC THAN DON'T            
         BE    ROUTE                        CALL OFFAL                          
                                                                                
         TM    BCCPYST1,CPYSOROE   TEST COMPANY USES OFFICES                    
         BZ    ROUTE                                                            
         CLC   TWAACCS,BCSPACES    TEST ANY LIMIT ACCESS                        
         BNH   ROUTE                                                            
*        CLC   BCCPYPRD,IOKEY+(ACTKUNT-ACTRECD)   *NOT SUPPORTING SJ            
*        BNE   TSTSEC04                                                         
*        OC    PSPROPPR,PSPROPPR   TEST PRODUCT PROFILE RESOLVED                
*        BNZ   TSTSEC06                                                         
*        B     ROUTE                                                            
*                                                                               
TSTSEC04 CLI   LDGTOFFP,LDGONONE   TEST NO OFFICE IN THIS LEDGER                
         BE    ROUTE                                                            
         CLI   LDGTOFFP,LDGOTRAN   TEST OFFICE IN TRANSACTIONS                  
         BE    ROUTE                                                            
         CLI   LDGTOFFP,LDGOFLT1   TEST OFFICE IN FILTER                        
         BNL   ROUTE                                                            
         CLI   LDGTOFFP,LDGOPROF   TEST OFFICE IN PRODUCTION PROFILE            
         BNE   TSTSEC06                                                         
         CLC   ACOFFC,BCSPACES     WAS OFFICE FILLED IN FROM GETELS             
         BH    TSTSEC10            YES SO USE IT                                
         B     ROUTE                                                            
                                                                                
TSTSEC06 MVC   ACOFFC,BCSPACES                                                  
         MVC   BCWORK(1),LDGTOFFP                                               
         NI    BCWORK,FF-LDGOKEY2                                               
         CLI   BCWORK,LDGOKEY                                                   
         BH    TSTSEC10                                                         
         SR    R1,R1                                                            
         IC    R1,BCWORK                                                        
         LA    R1,IOKEY+(ACTKACT-ACTRECD-1)(R1)                                 
         MVC   ACOFFC+0(1),0(R1)                                                
TSTSEC10 L     R1,=AL4(XORDD+XOACCDIR+XIO5)     READ FOR DELETES                
         GOTO1 AIO                                                              
         BE    TSTSEC20                                                         
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    *+6                                                              
         DC    H'0'                NO SOMETHING ELSE MUST BE WRONG              
TSTSEC20 LHI   R1,XOGET+XOACCMST+XIO5                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIO5                                                     
         MVC   OFFAOPOS,LDGTOFFP                                                
         MVC   OFFAOFFC,ACOFFC                                                  
         MVI   OFFAACT,OFFATST                                                  
         OI    OFFACTRL,OFFACCNV                                                
         GOTOR VOFFAL                                                           
         BE    ROUTE                                                            
                                                                                
TSTSECSL MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(ACTKEND-L'ACTKCPY),IOKEY+(ACTKUNT-ACTRECD)                
         B     ROUTH                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A LEDGER RECORD AND BUILD LDGTAB (SEE LDGTABD)       *         
*                                                                     *         
* NTRY - IOKEY=KEY OF A LEDGER OR LOWER LEVEL RECORD                  *         
*                                                                     *         
* EXIT - ACALDG POINTS TO LEDGER TABLE ENTRY IF VALID LEDGER          *         
***********************************************************************         
                                                                                
         USING GLWORKD,RC                                                       
GETLDG   LA    R0,LDGTMAXN         R0=MAXIMUM N'LEDGER TABLE ENTRIES            
         LA    R2,BCLDGTAB                                                      
         USING LDGTABD,R2          R2=A(LEDGER TABLE)                           
GETLDG02 OC    LDGTUL,LDGTUL       TEST FREE SLOT                               
         BZ    GETLDG04                                                         
         CLC   LDGTUL,IOKEY+(LDGKUNT-LDGKEY)                                    
         BE    GETLDGX                                                          
         LA    R2,LDGTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,GETLDG02         DO FOR NUMBER OF ENTRIES                     
         SHI   R2,LDGTABL          USE LAST ENTRY IF TABLE FULL                 
                                                                                
GETLDG04 MVC   GLKEYSAV,IOKEY      SAVE CALLER'S KEY                            
         MVC   IOKEY,BCSPACES      TAKE LEDGER PORTION & READ                   
         MVC   IOKEY(LDGKEND),GLKEYSAV                                          
*        LA    R1,GLIOAREA                                                      
*        ST    R1,IOADDR                                                        
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         MVC   IODAOVER,IOKEY+(LDGKDA-LDGRECD)                                  
         MVC   IOKEY,GLKEYSAV      RESTORE CALLER'S KEY                         
         BNE   GETLDGN                                                          
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO2             PROCESS LEDGER RECORD & BUILD ENTRY          
         MVC   LDGTUL,LDGKUNT-LDGKEY(R1)                                        
         AHI   R1,LDGRFST-LDGRECD                                               
         SR    R0,R0                                                            
GETLDG06 CLI   0(R1),0             TEST EOR                                     
         BE    GETLDGX                                                          
                                                                                
         USING LDGELD,R1                                                        
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   GETLDG08                                                         
         MVC   LDGTSTA2,LDGSTAT2                                                
         MVC   LDGTLIKE,LDGLIKE                                                 
         MVC   LDGTOFFP,LDGOPOS                                                 
         MVC   LDGTCLOS,LDGCLOS                                                 
         CLI   LDGLN,LDGLNQ                                                     
         BL    GETLDG16                                                         
         PACK  LDGTDDL,LDGDPOS     EXTRACT DEPARTMENT VALUES                    
         NI    LDGDLEN,X'0F'                                                    
         OC    LDGTDDL,LDGDLEN                                                  
         B     GETLDG16                                                         
                                                                                
         USING ACLELD,R1                                                        
GETLDG08 CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BNE   GETLDG10                                                         
         MVC   LDGTLVA,ACLVALS                                                  
         MVC   LDGTLVB,ACLVALS+(L'ACLVALS*1)                                    
         MVC   LDGTLVC,ACLVALS+(L'ACLVALS*2)                                    
         MVC   LDGTLVD,ACLVALS+(L'ACLVALS*3)                                    
         B     GETLDG16                                                         
                                                                                
         USING RSTELD,R1                                                        
GETLDG10 CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BNE   GETLDG12                                                         
         MVC   LDGTSEC,RSTSECY+1                                                
         B     GETLDG16                                                         
                                                                                
         USING FFTELD,R1                                                        
GETLDG12 CLI   FFTEL,FFTELQ        TEST FREE FOR TEXT ELEMENT                   
         BNE   GETLDG14                                                         
         CLI   FFTTYPE,FFTTVATC    TEST VAT CODE                                
         BNE   GETLDG14                                                         
         MVC   LDGTVATC,FFTDATA    SET LEDGER DEFAULT VAT TYPE                  
         B     GETLDG16                                                         
                                                                                
GETLDG14 DS    0H                                                               
                                                                                
GETLDG16 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETLDG06                                                         
                                                                                
GETLDGX  STCM  R2,15,ACALDG        SET A(LEDGER TABLE ENTRY)                    
*        MVC   FVXTRA,BCSPACES                                                  
         XC    IODAOVER,IODAOVER   CLEAR D/A OVERRIDE                           
         B     ROUTE                                                            
                                                                                
GETLDGN  MVC   FVXTRA(L'LDGTUL),IOKEY+(ACTKUNT-ACTKEY)                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         XC    IODAOVER,IODAOVER   CLEAR D/A OVERRIDE                           
         B     ROUTL                                                            
         DROP  R1,R2,RC                                                         
                                                                                
GLWORKD  DSECT                     ** GETLDG S/R LOCAL W/S **                   
GLKEYSAV DS    XL(L'IOKEY)                                                      
GLIOAREA DS    2000X                                                            
FIL00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT VALUES FROM AN ACCOUNT RECORD INTO ACVALS        *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST AS FOLLOWS):-                            *         
*        P1=A(FIRST ELEMENT ON ACCOUNT RECORD)                        *         
*        P2=A(WHERE TO PUT THE PROFILE ELEMENT IF FOUND)              *         
*        IOKEY=KEY OF ACCOUNT RECORD                                  *         
*        IODA=RECORD DISK ADDRESS IF FILE IS I/S D/A PAIR             *         
***********************************************************************         
                                                                                
GETELS   L     R3,4(R1)            R3=A(OUTPUT PROFILE AREA)                    
         L     R1,0(R1)            R1=A(FIRST ELEMENT ON RECORD)                
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         XC    ACVALS(ACVALSL),ACVALS                                           
         SR    R0,R0                                                            
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING ACTRECD,R2                                                       
         CLC   ACTKACT,BCSPACES                                                 
         BE    GETELS06                                                         
         CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),BCSPACES                        
         BNE   GETELS06                                                         
         MVC   ACCODE,ACTKEY       SET ACCOUNT CODE                             
         OI    ACINDS1,ACIACTHI    SET HIGH LEVEL ACCOUNT                       
         MVC   ACDA,IODA           SET ACCOUNT DISK ADDRESS OR ZERO             
         CLC   BCCPYEL+(CPYPROD-CPYELD)(L'CPYPROD),ACTKUNT                      
         BNE   GETELS06                                                         
                                                                                
         SR    RE,RE               SET PRODUCTION ACCOUNT VALUES                
         IC    RE,LDGTLVC          RE=L'CLIENT+PRODUCT+JOB                      
         SR    RF,RF                                                            
         IC    RF,LDGTLVB          RF=L'CLIENT+PRODUCT                          
         SR    RE,RF               RE=L'JOB                                     
         LA    RF,ACTKACT(RF)      RF=A(JOB IN KEY)                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES    TEST JOB CODE PRESENT                        
         BE    GETELS02                                                         
         OI    ACINDS1,ACIPRJOB    SET PRODUCTION JOB                           
         B     GETELS06                                                         
                                                                                
GETELS02 IC    R0,LDGTLVA          R0=L'CLIENT                                  
         IC    RE,LDGTLVB          RE=L'CLIENT+L'PRODUCT                        
         SR    RE,R0               RE=L'PRODUCT                                 
         SR    RF,RE               RF=A(PRODUCT IN KEY)                         
         BCTR  RE,0                                                             
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES    TEST PRODUCT CODE PRESENT                    
         BE    GETELS04                                                         
         OI    ACINDS1,ACIPRPRO    SET PRODUCTION PRODUCT                       
         B     GETELS06                                                         
                                                                                
GETELS04 OI    ACINDS1,ACIPRCLI    SET PRODUCTION CLIENT                        
                                                                                
GETELS06 CLI   0(R1),0             TEST EOR                                     
         BE    GETELS32                                                         
         CLI   0(R1),NAMELQ        TEST NAME ELEMENT                            
         BE    GETELS10                                                         
         CLI   0(R1),RSTELQ        TEST STATUS ELEMENT                          
         BE    GETELS12                                                         
         CLI   0(R1),ABLELQ        TEST BALANCE ELEMENT                         
         BE    GETELS14                                                         
         CLI   0(R1),PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BE    GETELS16                                                         
         CLI   0(R1),RATETAXQ      TEST (TAX) RATE ELEMENT                      
         BE    GETELS18                                                         
         CLI   0(R1),SNMELQ        TEST SHORT NAME ELEMENT                      
         BE    GETELS20                                                         
         CLI   0(R1),SPAELQ        TEST SPECIAL POSTING A/C ELEMENT             
         BE    GETELS22                                                         
         CLI   0(R1),ASTELQ        TEST ACCOUNT STATUS ELEMENT                  
         BE    GETELS24                                                         
         CLI   0(R1),FFTELQ        TEST FREE FORM TEXT ELEMENT                  
         BE    GETELS26                                                         
                                                                                
GETELS08 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETELS06                                                         
                                                                                
         USING NAMELD,R1                                                        
GETELS10 SR    RE,RE               EXTRACT NAME                                 
         IC    RE,NAMLN                                                         
         SHI   RE,NAMEREC+1-NAMELD                                              
         MVC   ACNAME,BCSPACES                                                  
         EX    RE,*+8                                                           
         B     GETELS08                                                         
         MVC   ACNAME(0),NAMEREC                                                
                                                                                
         USING RSTELD,R1                                                        
GETELS12 MVC   ACSTAT1,RSTSTAT1    STATUS BYTES 1 & 3 ALWAYS PRESENT            
         MVC   ACSTAT3,RSTSTAT3                                                 
         CLI   RSTLN,RSTLN2Q                                                    
         BL    *+16                                                             
         MVC   ACSTAT2,RSTSTAT2    STATUS BYTES 2 & 4 PRESENT                   
         MVC   ACSTAT4,RSTSTAT4                                                 
         MVI   ACSTAT5,0                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         BL    *+10                                                             
         MVC   ACSTAT5,RSTSTAT5    STATUS BYTE 5 PRESENT                        
         MVC   ACCOST,RSTCOSTG                                                  
         MVC   ACSECY,RSTSECY+1                                                 
         MVC   ACFLT1,RSTFILT1                                                  
         MVC   ACFLT2,RSTFILT2                                                  
         MVC   ACFLT3,RSTFILT3                                                  
         MVC   ACFLT4,RSTFILT4                                                  
         MVC   ACFLT5,RSTFILT5                                                  
                                                                                
         TM    ACSTAT1,RSTSGPEI    SET BATCH STATUS                             
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSPERS                                                 
         TM    ACSTAT1,RSTSACIC                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSCLSE                                                 
         TM    ACSTAT1,RSTSACIL                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSLOCK                                                 
         TM    ACSTAT1,RSTSEADD                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSDEPT                                                 
*&&US                                                                           
         TM    ACSTAT1,RSTSVB2C                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSVEND                                                 
*&&                                                                             
*&&UK                                                                           
         TM    ACSTAT1,RSTSIVAT                                                 
         BZ    *+8                                                              
         OI    ACBSTAT,ACBSIVAT                                                 
*&&                                                                             
         B     GETELS08                                                         
                                                                                
GETELS14 OI    ACBSTAT,ACBSABAL    SET BALANCE ELEMENT PRESENT                  
         NI    ACINDS1,FF-ACIACTHI CLEAR HIGH LEVEL ACCOUNT                     
         OI    ACINDS1,ACIACTLO    SET LOW LEVEL ACCOUNT                        
         B     GETELS08                                                         
                                                                                
GETELS16 STCM  R1,15,ACAPPR        SAVE A(PROFILE ELEMENT)                      
         B     GETELS08                                                         
                                                                                
         USING RATELD,R1                                                        
GETELS18 MVC   ACTAXR,RATRATE      EXTRACT (TAX) RATE                           
         B     GETELS08                                                         
                                                                                
         USING SNMELD,R1                                                        
GETELS20 MVC   ACNAMESH,SNMNAME    EXTRACT SHORT NAME                           
         B     GETELS08                                                         
                                                                                
GETELS22 OC    ACASPA,ACASPA       TEST A(FIRST SPAEL SET)                      
         BNZ   *+8                                                              
         STCM  R1,15,ACASPA                                                     
         B     GETELS08                                                         
                                                                                
         USING ASTELD,R1                                                        
GETELS24 MVC   ACCURCOD,ASTCUR     EXTRACT ACCOUNT CURRENCY CODE                
         MVC   ACKSVTYP,ASTKSVTY   EXTRACT KSV TYPE FOR GERMANY                 
         B     GETELS08                                                         
                                                                                
         USING FFTELD,R1                                                        
GETELS26 CLI   FFTTYPE,FFTTVATC    TEST VAT CODE ELEMENT                        
         BNE   *+10                                                             
         MVC   ACVATCOD,FFTDATA                                                 
         B     GETELS08                                                         
                                                                                
GETELS32 CLC   LDGTUL,BCCPYPRD     TEST PRODUCTION LEDGER                       
         BNE   GETELS34                                                         
         LTR   R3,R3               TEST OUTPUT PROFILE AREA PASSED              
         BZ    GETELS34                                                         
         ICM   RF,15,ACAPPR        TEST RECORD HAS A PROFILE ELEMENT            
         BZ    GETELS34                                                         
         MVC   0(L'PSJOBPPR,R3),0(RF)                                           
                                                                                
GETELS34 MVC   ACOFFC,BCSPACES                                                  
         CLC   BCCPYPRD,ACCODE+(ACTKUNT-ACTRECD)                                
         BNE   GETELS36                                                         
         OC    PSPROPPR,PSPROPPR   TEST PRODUCT PROFILE RESOLVED                
         BZ    GETELSX                                                          
*        MVC   ACOFFC,PSJOBPPR+(PPRGAOFF-PPRELD)  * NOT READING FOR JOB         
*        CLC   ACOFFC,BCSPACES                      LEVEL                       
*        BH    GETELSX                                                          
         MVC   ACOFFC,PSPROPPR+(PPRGAOFF-PPRELD)                                
         CLC   ACOFFC,BCSPACES                                                  
         BH    GETELSX                                                          
         MVC   ACOFFC,PSCLIPPR+(PPRGAOFF-PPRELD)                                
         B     GETELSX                                                          
                                                                                
GETELS36 CLI   LDGTOFFP,LDGONONE   TEST ANY OFFICE IN THIS LEDGER               
         BE    GETELSX                                                          
         CLI   LDGTOFFP,LDGOTRAN   TEST OFFICE IN TRANSACTIONS                  
         BE    GETELSX                                                          
         CLI   LDGTOFFP,LDGOFLT1   TEST OFFICE IN FILTERS                       
         BNL   GETELS38                                                         
         MVC   BCWORK(1),LDGTOFFP                                               
         NI    BCWORK,FF-LDGOKEY2                                               
         CLI   BCWORK,LDGOKEY                                                   
         BH    GETELSX                                                          
         SR    R1,R1                                                            
         IC    R1,BCWORK                                                        
         LA    R1,ACCODE+(ACTKACT-ACTRECD-1)(R1)                                
         MVC   ACOFFC+0(1),0(R1)                                                
         TM    LDGTOFFP,LDGOKEY2   TEST 2 CHARACTER OFFICE IN KEY               
         BZ    *+10                                                             
         MVC   ACOFFC+1(1),1(R1)                                                
         B     GETELSX                                                          
                                                                                
GETELS38 PACK  BCDUB,LDGTOFFP      OFFICE IN FILTERS                            
         CVB   R1,BCDUB            VALUE IS OF FORM X'F1'-X'F4'                 
         LA    R1,ACFLTS-1(R1)                                                  
         MVC   ACOFFC(1),0(R1)                                                  
                                                                                
GETELSX  B     ROUTE                                                            
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DELETE OPTION                                   *         
*                                                                     *         
* THIS ROUTINE WILL ONLY VALIDATE DELETE=YES/NO/ONLY                  *         
* IF MORE THAN ONE OPTION, YOU SHOULD PUT IT INTO YOU OVERLAY         *         
*                                                                     *         
* CDOPTION - CURRENT OPTION - SEE ACFILWORK                           *         
* SDOPTION - SAVED OPTION                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING VOWORKD,RC                                                       
VALDOPT  XC    OPTBLK,OPTBLK                                                    
         MVI   CRECDEL,0                                                        
         XR    R2,R2                                                            
         ICM   R2,3,GSDSPOPT                                                    
         BZ    ROUTE                                                            
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         ST    R2,FVADDR                                                        
         CLI   FHIL,0              NO OPTION                                    
         BE    ROUTE                                                            
                                                                                
         GOTO1 VSCANNER,BOPARM,(R2),('SBLKNUM',SBLK),0                          
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)         NUMBER OF LINES USED                          
         BZ    VDOPTERR                                                         
         CHI   R0,1                                                             
         BNE   VDOPTERR            MUST BE 'DELETE=' OPTION ONLY                
                                                                                
         LA    RF,SBLK                                                          
         USING SCANBLKD,RF                                                      
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         CHI   R1,0                                                             
         BE    VDOPTERR            ERROR - NO INPUT                             
         BCTR  R1,0                                                             
                                                                                
         EXCLC R1,SC1STFLD,=CL8'DELETE'                                         
         BNE   VDOPTERR            IT MUST BE 'DELETE=' OPTION                  
                                                                                
         IC    R1,SC2NDLEN                                                      
         CHI   R1,0                                                             
         BE    VDOPTERR            NO 2ND ENTRY                                 
         BCTR  R1,0                                                             
         MVI   CRECDEL,YES         DELETE=YES                                   
         EXCLC R1,SC2NDFLD,BC@YES  UPPER CASE                                   
         BE    ROUTE                                                            
                                                                                
         MVI   CRECDEL,NO          DELETE=NO                                    
         EXCLC R1,SC2NDFLD,BC@NO                                                
         BE    ROUTE                                                            
                                                                                
         MVI   CRECDEL,ONLY        DELETE=ONLY                                  
         EXCLC R1,SC2NDFLD,=CL8'ONLY'                                           
         BE    ROUTE                                                            
                                                                                
VDOPTERR MVC   FVMSGNO,=AL2(AE$INOPT)                                           
         B     ROUTL                                                            
         DROP  R2,RF                                                            
                                                                                
VOWORKD  DSECT                     ** VALDOPT LOCAL W/S **                      
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
                                                                                
OPTBLK   DS    0XL20           *** OPTIONS BLOCK                                
         ORG   OPTBLK+L'OPTBLK                                                  
SBLKNUM  EQU   3                                                                
SBLK     DS    (SBLKNUM)XL(SCBLKLQ)                                             
VOWORKX  EQU   *-VOWORKD                                                        
FIL00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK INVALID CHARACTERS IN THE FVIFLD                   *         
*                                                                     *         
* NTRY: P1=(CKKEYQ OR CKDATAQ, TABLE NUMBER)                          *         
*       FVIFLD=FIELD TEXT                                             *         
***********************************************************************         
         SPACE 1                                                                
CHKFLD   CLI   0(R1),CKKEYQ        FVIFLD CONTAINS KEY FIELD                    
         BNE   CKFLD10                                                          
         CLI   CSACT,A#ADD         CHECK KEY, IF ADDING OR COPYING              
         BE    *+12                                                             
         CLI   CSACT,A#CPY                                                      
         BNE   ROUTE                                                            
*                                                                               
CKFLD10  ICM   R3,B'0111',1(R1)    R3=TABLE NUMBER                              
         LA    R2,OVTRTAB1         USE TABLE 1                                  
         CHI   R3,CKTAB1Q                                                       
         BE    *+12                                                             
         LA    R2,OVTRTAB2         USE TABLE 2                                  
         CHI   R3,CKTAB2Q                                                       
         BE    *+8                                                              
         LA    R2,OVTRTAB3         USE TABLE 3                                  
*                                                                               
         LR    R5,R1               SAVE R1 AND R2 FOR BEYOND TRT                
         LR    R6,R2                                                            
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     CKFLD20                                                          
         TRT   FVIFLD(0),0(R2)                                                  
CKFLD20  BNZ   ROUTL                                                            
         LR    R1,R5                                                            
         LR    R2,R6                                                            
         B     ROUTE                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO TEST OFFICE SECURITY                                     *         
* NTRY - R1=A(OFFICE CODE)                                            *         
* EXIT - CC=EQUAL IF OKAY                                             *         
*        CC=HIGH  IF NOT OKAY (WITH ERROR SET)                        *         
*        CC=LOW   IF OFFICE RECORD NOT FOUND (WITH ERROR SET)         *         
***********************************************************************         
                                                                                
         USING TOWORKD,RC                                                       
TSTOFF   MVC   TOOFFC,0(R1)        OFFICE CODE                                  
         CLC   TOOFFC,BCSPACES                                                  
         BNH   TSTOFFE             EXIT IF NO OFFICE PROVIDED                   
         MVC   FVXTRA,BCSPACES                                                  
         MVC   TOIOKEY,IOKEY                                                    
         MVC   FVMSGNO,=AL2(AE$IVOFF)                                           
         TM    BCCPYST4,CPYSOFF2   TEST 2CO?                                    
         BNZ   TSTOF10             YES                                          
         CLI   TOOFFC+1,C' '       IF NOT CONVERTED - 2 BYTES IS BAD            
         BH    TSTOFFH                                                          
         MVI   TOOFFC+1,C' '       AND FILL WITH SPACE                          
*                                                                               
         CLI   CUCTRY,CTRYGER      IN GERMANY ONLY                              
         BNE   TSTOF04                                                          
T        USING OGRRECD,IOKEY       PRODUCTION OFFICE RECORD                     
         XC    T.OGRKEY,T.OGRKEY                                                
         MVC   T.OGRKCPY,CUABIN                                                 
         MVI   T.OGRKSUB,OGRKOFFQ  IN PRODUCTION SYSTEM                         
         MVI   T.OGRKTYP,OGRKTYPQ  CHECK OFFICE EXISTS                          
         MVC   T.OGRKUNT(L'BCCPYPRD),BCCPYPRD   SJ LEDGER                       
         MVC   T.OGRKOFC,TOOFFC                                                 
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   TSTOFFH                                                          
*                                                                               
TSTOF04  L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFAVAL                                                  
         MVC   OFFAOFFC,TOOFFC                                                  
         GOTO1 VOFFAL                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         B     TSTOFFH                                                          
         DROP  R1                                                               
*                                                                               
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BZ    TSTOFFE             NO - OK                                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKULA(2),=C'2D' READ 2D ACCOUNT                              
         MVC   T.ACTKACT(1),TOOFFC                                              
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFCNF)                                           
         B     TSTOFFH                                                          
         TM    T.ACTKSTAT,ACTSLOCK ACCOUNT LOCKED?                              
         BZ    TSTOFFE                                                          
         MVC   FVMSGNO,=AL2(AE$ACTLK)                                           
         B     TSTOFFH                                                          
         DROP  T                                                                
*                                                                               
TSTOF10  CLI   TOOFFC+1,C' '       MUST HAVE 2 NON-SPACE CHARS                  
         BE    TSTOFFH             INVALID 2CO                                  
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL BLOCK)                            
         CLC   =C'**',TWAACCS      TEST OFFICE LIMIT ACCESS                     
         BNE   TSTOF20                                                          
         OI    OFFACTRL,OFFACCNV                                                
         MVC   OFFAOFFC,TWAACCS+2  OFFICE CODE/OFFICE LIST                      
         LA    RF,TOOFFWRK         R2=A(OFFICE LIST)                            
         ST    RF,OFFAREQL         R2=A(REQUESTED LIST OUTPUT)                  
         MVI   OFFAACT,OFFAREQ                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         BNE   TSTOFFH             INVALID OFFICE                               
         DROP  R1,RA                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TOOFFNUM       NUMBER OF OFFICES IN THE LIST                
         BZ    TSTOFFH             NO OFFICE                                    
         LA    RE,TOOFFLST         OFFICE LIST                                  
TSTOF14  CLC   TOOFFC,0(RE)        MATCH OFFICE                                 
         BE    TSTOF20                                                          
         LA    RE,2(,RE)                                                        
         BCT   RF,TSTOF14                                                       
         B     TSTOFFH                                                          
*                                                                               
T        USING OFFRECD,IOKEY                                                    
TSTOF20  MVC   T.OFFKEY,BCSPACES   READ THE OFFICE RECORD                       
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN    COMPANY                                      
         MVC   T.OFFKOFF,TOOFFC                                                 
         LHI   R1,XORD+XOACCDIR+XIO1                                            
         GOTOR AIO                                                              
         BNE   TSTOFFL                                                          
         TM    T.OFFKSTAT,OFFSLIST MUSTN'T BE AN OFFICE LIST RECORD             
         BZ    TSTOFFE                                                          
         MVC   FVMSGNO,=AL2(AE$TCDOG)                                           
         B     TSTOFFH             CODE DEFINED AS OFFICE GROUP                 
         DROP  T                                                                
                                                                                
TSTOFFE  MVC   IOKEY,TOIOKEY                                                    
         B     ROUTE                                                            
*                                                                               
TSTOFFH  MVC   IOKEY,TOIOKEY                                                    
         MVC   FVXTRA(L'TOOFFC),TOOFFC                                          
         B     ROUTH                                                            
*                                                                               
TSTOFFL  MVC   IOKEY,TOIOKEY                                                    
         MVC   FVXTRA(L'TOOFFC),TOOFFC                                          
         B     ROUTL                                                            
                                                                                
TOWORKD  DSECT                     ** TSTOFF LOCAL W/S **                       
TOOFFC   DS    CL2                                                              
TOIOKEY  DS    CL(L'IOKEY)                                                      
TOOFFWRK DS    0CL(L'OFFAWORK)                                                  
TOOFFNUM DS    XL2                 NUMBER OF OFFICES IN LIST                    
TOOFFLST DS    XL510               OFFICE LIST                                  
TOWORKX  EQU   *-TOWORKD                                                        
FIL00    CSECT                                                                  
         EJECT ,                                                                
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
*  X'00' IN OVTRTAB IS A VALID CHARACTER                              *         
*  X'FF' IN OVTRTAB IS AN INVALID CHARACTER                           *         
***********************************************************************         
                                                                                
* TABLE 1 - KEY IS ALPHA/NUMERIC                                                
*                         RHS HALF BYTE                                         
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F                               
OVTRTAB1 DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20 L                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30 H                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 40 S                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 50                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 60 H                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 70 A                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 80 L                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 90 F                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A0                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0 B                         
         DC    X'FF000000000000000000FFFFFFFFFFFF' C0 Y                         
         DC    X'FF000000000000000000FFFFFFFFFFFF' D0 T                         
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E0 E                         
         DC    X'00000000000000000000FFFFFFFFFFFF' F0                           
                                                                                
* TABLE 2 - KEY IS ALPHA/NUMERIC + '-' AND '.'                                  
*                         RHS HALF BYTE                                         
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F                               
OVTRTAB2 DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20 L                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30 H                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 40 S                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 50                           
         DC    X'00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 60 H                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 70 A                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 80 L                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 90 F                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A0                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0 B                         
         DC    X'FF000000000000000000FFFFFFFFFFFF' C0 Y                         
         DC    X'FF000000000000000000FFFFFFFFFFFF' D0 T                         
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E0 E                         
         DC    X'00000000000000000000FFFFFFFFFFFF' F0                           
                                                                                
* TABLE 3 - KEY IS VALID CHARACTERS                                             
*                         RHS HALF BYTE                                         
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F                               
OVTRTAB3 DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20 L                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30 H                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 40 S                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF' 50                           
         DC    X'0000FFFFFFFFFFFFFFFFFFFF00FFFFFF' 60 H                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 70 A                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 80 L                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 90 F                         
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' A0                           
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0 B                         
         DC    X'FF000000000000000000FFFFFFFFFFFF' C0 Y                         
         DC    X'FF000000000000000000FFFFFFFFFFFF' D0 T                         
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E0 E                         
         DC    X'00000000000000000000FFFFFFFFFFFF' F0                           
*ACFILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*FAXTRAINF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025ACFIL00   05/21/20'                                      
         END                                                                    
