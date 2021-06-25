*          DATA SET ACREC00    AT LEVEL 033 AS OF 02/25/20                      
*PHASE T60800C                                                                  
*INCLUDE CONVMOS                                                                
*INCLUDE ACSRCHC                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE BMONVAL                                                                
*&&US                                                                           
*INCLUDE CONVERT                                                                
*&&                                                                             
RECALL   TITLE '- RECEIVABLE ALLOCATION'                                        
*                                                                               
* SSKI   189  SKIP TWAAUTH TEST FOR DUE DATE CHANGE IF NEW SECURITY             
* DCUR   003  CHANGE CHECK OF RECVCNT TO 0 FROM 1 IN GETRCV ROUTINE             
* DCUR   004  SET LOWLEV BIT WHEN USER ENTERS A LOW LEVEL SR ACCT               
*             SINCE RECVCNT WILL BE DIFFERENT WHEN LOW LVL VS HIGH LVL          
* ABID   029  SPEC-15672 RCV HEADER ERROR TOO MANY TRANSACTIONS TO              
*             PROCESS SB11021155010 BUT FIS SHOWS ONLY 20 ITEMS                 
* ABID   030  SPEC-15343 255 ISSUE IN RCV                                       
* JSAY   030  SPEC-21208 CARRY BILL TYPE ON TYPE 30 POSTINGS IN RCV             
* VGUP   031  SPEC-31421 255+ DUE DATE ISSUE IN RCV                             
* VGUP   031  SPEC-21843 ERROR RETURNED WHEN UPDATING VIA RCV WITH              
*             ZERO AMOUNT AS INPUT                                              
* JSAY   032  SPEC-36076 CARRY PO NUMBER ON TYPE 30 POSTINGS.                   
* RKEJ   033  SPEC-36365 RCV WRITE OFF SESSION ERROR WHEN TOTAL=0               
*                                                                               
RECALL   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL (WORKX-WORKD),**REC0**,RA,R9,R8,RR=RE,CLEAR=YES                  
         USING WORKD,RC                                                         
         MVC   LITVALS(GLOBALSL),GLOBALS                                        
         ST    RE,RELO                                                          
         MVC   AINP,0(R1)          SAVE A(TIOB)                                 
         L     R6,4(R1)                                                         
         ST    R6,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
         USING TWAD,R6                                                          
         MVC   COMPANY,0(R1)       EXTRACT COMPANY FROM FAPARMS                 
         LA    R7,SAVEAREA                                                      
         USING SAVED,R7                                                         
*                                                                               
         L     RF,20(R1)           RF=A(EXTRA INFO BLOCK)                       
         MVC   AGYOPTS,0(R1)                                                    
         MVC   AGYCTRY,1(RF)                                                    
         MVC   AGYLANG,3(RF)                                                    
         OC    AGYCURR,AGYCURR                                                  
         BNZ   *+10                                                             
         MVC   AGYCURR,4(RF)                                                    
         CLI   AGYCTRY,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   AGYCTRY,CTRYGBR                                                  
*&&US*&& MVI   AGYCTRY,CTRYUSA                                                  
         CLI   AGYCTRY,CTRYSCA     CONVERT SCANDINAVIA TO UK                    
         BNE   *+8                                                              
         MVI   AGYCTRY,CTRYGBR                                                  
         CLI   AGYLANG,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   AGYLANG,LANGEUK                                                  
*&&US*&& MVI   AGYLANG,LANGEUS                                                  
*                                                                               
         SR    RF,RF               SET SCANNER DELIMITER CHARACTER              
         IC    RF,AGYLANG                                                       
         LA    RF,DELIMS-1(RF)                                                  
         MVC   DELIM,0(RF)                                                      
*                                                                               
         L     R1,16(R1)                                                        
         ST    R1,ACOM             EXTRACT A(COMFACS) FROM FAPARMS              
         USING COMFACSD,R1                                                      
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VCUREDIT,CCUREDIT                                                
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETPROF,CGETPROF                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VREPORT,CREPORT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VADDAY,CADDAY                                                    
*&&UK*&& MVC   VCONVERT,CCONVERT                                                
         MVC   VBLDCUR,CBLDCUR                                                  
         MVC   VGETCUR,CGETCUR                                                  
         MVC   VSECRET,CSECRET                                                  
         MVC   VXTRAINF,CXTRAINF                                                
         MVC   VPROTON,CPROTON                                                  
         MVC   VPROTOFF,CPROTOFF                                                
         DROP  R1                                                               
*                                                                               
         GOTO1 VSWITCH,DMCB,X'FFFFFFFF'                                         
         MVC   AUTL,0(R1)          SAVE A(UTL ENTRY)                            
*                                                                               
         LA    R1,ROUT1            SET A(ROOT ROUTINES 1 IN W/S)                
         LA    R0,ROUT1N                                                        
         SR    RE,RE                                                            
         L     RF,=A(ROUTN1)                                                    
         A     RF,RELO             RF=A(GLOBAL ROUTINES)                        
*                                                                               
INIT10   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         LA    R1,ROUT2            SET A(ROOT ROUTINES 2 IN W/S)                
         LA    R0,ROUT2N                                                        
         SR    RE,RE                                                            
         L     RF,=A(ROUTN2)                                                    
         A     RF,RELO             RF=A(GLOBAL ROUTINES)                        
INIT12   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT12                                                        
*                                                                               
         LA    R0,TABLESN          SET A(ROOT TABLES IN W/S)                    
         LA    R1,TABLES                                                        
         LA    RE,ATABLES                                                       
INIT18   SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         LA    RF,RECALL(RF)                                                    
         ST    RF,0(RE)            SET TABLE ADDRESS                            
         LA    R1,2(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,INIT18                                                        
*                                                                               
         LA    R1,SAVED                                                         
         AH    R1,=Y(RECVTAB-SAVED)                                             
         ST    R1,ARECVTAB                                                      
         LA    R1,WORKD                                                         
         AH    R1,=Y(OFFBLK-WORKD)                                              
         ST    R1,AOFFBLK                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(TSARBLK-WORKD)                                             
         ST    R1,ATSARBLK                                                      
         LA    R1,WORKD                                                         
         AH    R1,=Y(PQBUFF-WORKD)                                              
         ST    R1,APQBUFF                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(SCANOUT-WORKD)                                             
         ST    R1,ASCANOUT                                                      
         LA    R1,WORKD                                                         
         AH    R1,=Y(OVERWORK-WORKD)                                            
         ST    R1,AOVERWRK                                                      
         LA    R1,WORKD                                                         
         AH    R1,=Y(REPWORK-WORKD)                                             
         ST    R1,AREPWRK                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(IO-WORKD)                                                  
         ST    R1,AIO1                                                          
         AH    R1,=Y(IO2-IO)                                                    
         ST    R1,AIO2                                                          
         AH    R1,=Y(IO3-IO2)                                                   
         ST    R1,AIO3                                                          
         AH    R1,=Y(IO4-IO3)                                                   
         ST    R1,AIO4                                                          
         AH    R1,=Y(IO5-IO4)                                                   
         ST    R1,AIO5                                                          
         AH    R1,=Y(IO6-IO5)                                                   
         ST    R1,AIO6                                                          
         L     R1,=V(CONVMOS)                                                   
         A     R1,RELO                                                          
         ST    R1,VCONVMOS                                                      
         L     R1,=V(ACSRCHC)                                                   
         A     R1,RELO                                                          
         ST    R1,VACSRCHC                                                      
*&&US                                                                           
         L     R1,=V(CONVERT)                                                   
         A     R1,RELO                                                          
         ST    R1,VCONVERT                                                      
*&&                                                                             
         GOTO1 VCALLOV,DMCB,0,X'D9000A5D'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)         GET TSAR ADDRESS - CORERES                   
         GOTO1 (RF),(R1),0,X'D9000A62'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFAL,0(R1)        GET OFFAL ADDRESS - CORERES                  
         GOTO1 (RF),(R1),0,X'D9000A63'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VADDTRN,0(R1)       GET ADDTRN ADDRESS - CORERES                 
         MVC   DATADISP,=Y(ACRECORD-ACKEYD)                                     
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   LARE,=X'41E0'                                                    
         MVC   LARF,=X'41F0'                                                    
         LA    R5,KEY              R5=A(KEY)                                    
*                                                                               
         TM    TWAMODE,TWAMINIT    FIRST TIME?                                  
         BNZ   INIT32                                                           
         MVC   WORK(L'ACTNAME),ACTNAME                                          
         LA    R0,SAVEAREA         CLEAR SAVED VALUES                           
         LH    R1,=Y(SAVEDLEN)                                                  
         CH    R1,=Y(SAVEAREL)                                                  
         BNH   *+6                                                              
         DC    H'0'                ENSURE WE HAVEN'T OVERSTEPPED                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   ACTNAME,WORK        RESTORE LAST SAVED ACTION NAME               
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LU  ',ADCLISTU,DSLISTU                           
         GOTO1 (RF),(R1),C'LL  ',ADCLISTL,DSLISTL                               
*                                                                               
*        MVI   FILEFORM,VLISQ      ESTABLISH ACCOUNT FILE FORMAT                
*        GOTO1 VDATAMGR,DMCB,=C'DTFAD',ACCFIL                                   
*        L     R1,12(R1)                                                        
*        TM    DTFTYPE-DTFPHD(R1),DTFTEMU                                       
*        BZ    *+8                                                              
         MVI   FILEFORM,ISDAQ                                                   
*                                                                               
         L     RF,ACOM             EXTRACT USER PASSWORD NUMBER                 
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         XC    TWAPASS#,TWAPASS#                                                
         TM    FATFLAG-FACTSD(RF),X'08'                                         
         BZ    *+10                                                             
         MVC   TWAPASS#,FAPASSWD-FACTSD(RF)                                     
         MVC   GFACTST6,FATSTAT6-FACTSD(RF)   UPLOAD STATUS                     
*                                                                               
         OC    TWASAGN,TWASAGN     TEST NEW SECURITY IN USE                     
         BZ    INIT20                                                           
         USING SECD,R2                                                          
         LA    R2,SECBLK                                                        
         TM    SECINDS,SECIINIT    TEST SECRET BLOCK INITIALISED                
         BNZ   INIT20                                                           
         LA    RF,L'SECBLK                                                      
         GOTO1 VSECRET,DMCB,('SECPINIT',SECD),(RF)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         USING CPYRECD,R5                                                       
INIT20   MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,COMPANY                                                  
         GOTO1 AIOREAD             READ COMPANY RECORD                          
         BE    *+6                                                              
         DC    H'0'                NO COMPANY RECORD                            
         GOTO1 AACCELS,0                                                        
         ICM   R1,15,RECCOMP                                                    
         BNZ   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
*                                                                               
         USING CPYELD,R1                                                        
         MVC   COMPALFA,CPYALPHA   EXTRACT DATA FROM COMPANY ELEMENT            
         MVC   COMPSTAT,CPYSTAT1                                                
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
                                                                                
         CLI   CPYLN,CPYLN3Q       LONG ENOUGH FOR THESE FIELDS?                
         BL    *+16                NO                                           
         MVC   COMPSTA9,CPYSTAT9                                                
         MVC   COMPSTAA,CPYSTATA                                                
         XC    COMPGMOA,COMPGMOA                                                
         CLI   CPYLN,CPYLN4Q       MAKE SURE IT IS LARGER ONE                   
         BL    *+10                                                             
         MVC   COMPGMOA,CPYGLMOA   GLMOA DATE                                   
                                                                                
         MVI   COMPOFFL,1          SET DEFAULT LENGTH OF OFFICE CODES           
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES                             
         BZ    *+8                                                              
         MVI   COMPOFFL,2          SET 2 CHARACTER OFFICE CODES                 
         MVI   COMPDPTL,2          SET DEFAULT LENGTH OF DEPT. CODES            
         CLI   CPYDEPTL,0          TEST OVERRIDE DEPARTMENT LENGTH              
         BE    *+10                                                             
         MVC   COMPDPTL,CPYDEPTL   SET LENGTH OF DEPARTMENT CODES               
         MVC   COCDPASS,CPYCDC                                                  
*&&UK*&& MVC   VATRATES,CPYVATR                                                 
         MVC   TENO,CPYTENO                                                     
         MVC   RECVUL,CPYRECV                                                   
         MVC   BANKUL,CPYBANK                                                   
         MVC   ANALUL,CPYBANK                                                   
         MVC   PRODUL,CPYPROD                                                   
*&&UK*&& MVC   COMPDEBO,CPYDEBOT                                                
*&&US*&& MVC   GSTUL,CPYTAX                                                     
*&&US*&& MVC   PSTUL,CPYTAX                                                     
*&&UK*&& MVC   VATUL,=C'SG'                                                     
*&&UK                                                                           
         CLI   COMPDEBO,0          TEST SPECIAL OVERLAY SPECIFIED               
         BNE   *+16                                                             
         TM    CPYSTAT6,CPYSFBIL   TEST FOREIGN BILLING USED                    
         BZ    *+8                                                              
         MVI   COMPDEBO,1          SET TO GET FOREIGN OVERLAY                   
*&&                                                                             
         CLI   CPYLN,CPYLN2Q                                                    
         BL    *+10                                                             
         MVC   AGYCURR,CPYCURR                                                  
         DROP  R1                                                               
*                                                                               
         USING LDGRECD,R5                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(L'PRODUL),PRODUL                                         
         DROP  R5                                                               
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                NO LEDGER RECORD                             
         ICM   R1,15,RECLEDG                                                    
         MVC   PRODLEVS,LEDGTLVA-LEDGTABD(R1)                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         GOTO1 (RF),(R1),(0,WORK),(1,TODAYP)                                    
         GOTO1 (RF),(R1),(0,WORK),(2,TODAYC)                                    
         GOTO1 (RF),(R1),(0,WORK),(3,TODAYB)                                    
*                                                                               
INIT30   CLI   RECACTH+(FVILEN-FVIHDR),0                                        
         BNE   INIT32                                                           
         L     RF,AINP                                                          
         CLI   TIOBAID-TIOBD(RF),0                                              
         BNE   INIT32                                                           
         MVI   TIOBAID-TIOBD(RF),PFK01                                          
*                                                                               
INIT32   MVC   TSARLEN,=Y(TSARRECL)                                             
*                                                                               
         L     R1,ATSARBLK                                                      
         USING TSARD,R1            R1=A(TSAR BLOCK)                             
         MVC   TSABUF,ATIA         SET A(BUFFER)                                
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC           SET A(RECORD)                                
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,TSARKEYL     SET KEY LENGTH                               
         MVC   TSRECL,=Y(TSARMAXL) SET MAXIMUM RECORD LENGTH                    
         MVI   TSINDS,TSIALLOC            ALLOCATE FROM TEMPEST                 
         MVI   TSRECI,TSRVAR       SET VARIABLE LENGTH RECORDS                  
         MVI   TSNBUF,2            SET NUMBER OF CORE BUFFERS                   
         MVI   TSPAGN,TSPEXPN      SET NUMBER OF TEMPEST PAGES                  
         TM    TWAMODE,TWAMRSRV    TEST TEMPEST PREVIOUSLY RESERVED             
         BZ    INIT40                                                           
*                                                                               
         MVC   TSINDS,TWATSARI     SET INDICATORS                               
         OI    TSINDS,TSIREUSE     SET TO RE-USE PREVIOUS ALLOCATION            
         MVC   TSPAGL,TWALOWPG     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWANUMPG     SET NUMBER OF PAGES ALLOCATED                
*                                                                               
INIT40   MVI   TSACTN,TSAINI       SET INITIALISE                               
         TM    TWAMODE,TWAMINIT                                                 
         BZ    *+8                                                              
         MVI   TSACTN,TSARES       SET RESTORE                                  
         GOTO1 VTSAR               CALL TO INITIALISE/RESTORE                   
         BE    INIT50                                                           
         TM    TWAMODE,TWAMRSRV    TEST FIRST TIME ALLOCATION                   
         BZ    *+6                                                              
         DC    H'0'                KILL IF RESTORE (INITIALISED)                
         NI    TSINDS,255-TSIALLOC RESET TEMPEST ALLOCATION                     
         MVI   TSPAGL,3            SET TO USE TEMPSTR PAGE 3                    
         MVI   TSPAGN,1                                                         
         BASR  RE,RF               INITIALISE TEMPSTR PAGE 3                    
         BE    INIT50                                                           
         DC    H'0'                KILL IF CAN'T INITIALISE TEMPSTR             
*                                                                               
INIT50   MVC   TWALOWPG,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   TWANUMPG,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         MVC   TWATSARI,TSINDS     SAVE TEMPSTR/TEMPEST INDICATOR               
         NI    TWATSARI,TSIALLOC                                                
         OI    TWAMODE,TWAMINIT+TWAMRSRV                                        
         DROP  R1                                                               
*                                                                               
         L     R1,AOFFBLK          INITIALISE OFFAL FOR OFFICE ACCESS           
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOM                                                    
         MVC   OFFATSAR,VTSAR                                                   
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTAT                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),TWAOFFSV                                       
         BZ    *+8                                                              
         MVI   OFFAACT,OFFARES                                                  
         OI    OFFAINDS,OFFAISEC+OFFAIOFF                                       
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T INITIALISE OFFAL                
         MVC   TWAOFFSV,OFFASAV                                                 
         DROP  R1                                                               
*                                                                               
         L     R1,ACTRYTAB                                                      
         USING CTRYTABD,R1         SET COUNTRY DEPENDANT VALUES                 
         SR    R0,R0                                                            
INIT70   CLI   CTRYTNUM,EOT        TEST E-O-T                                   
         BNE   INIT72                                                           
         LTR   R1,R0               TEST ANY ENTRY FOR THIS COUNTRY              
         BNZ   INIT76              YES - USE FIRST                              
         DC    H'0'                                                             
*                                                                               
INIT72   CLC   CTRYTNUM,AGYCTRY    MATCH ON AGENCY COUNTRY                      
         BNE   INIT74                                                           
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         LR    R0,R1               SAVE A(FIRST ENTRY FOR COUNTRY)              
         CLC   CTRYTDEB,COMPDEBO   MATCH ON DEBTORS OVERLAY TYPE                
         BE    INIT76                                                           
*                                                                               
INIT74   LA    R1,CTRYTABL(R1)     BUMP TO NEXT TABLE ENTRY                     
         B     INIT70                                                           
*                                                                               
INIT76   MVC   TWASCRBH,CTRYTHDR   SAVE HEADER SCREEN NUMBER                    
         MVC   PROFDFT(PROFDFTL),CTRYPROF                                       
         MVC   CDEFCUR,CTRYDCUR                                                 
         IC    R0,CTRYTOVR         R0=COUNTRY SPECIFIC OVERLAY                  
         DROP  R1                                                               
         GOTO1 VCALLOV,DMCB,((R0),0),0,0                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OVERLAY,0(R1)       SAVE A(OVERLAY ENTRY POINT)                  
*&&UK                                                                           
         MVC   WORK(L'AGYCURR),AGYCURR                                          
         OC    AGYCURR,AGYCURR                                                  
         BNZ   *+10                                                             
         MVC   WORK(L'AGYCURR),CDEFCUR                                          
         GOTO1 VBLDCUR,DMCB,(0,WORK),(0,AGYCURT),ACOM                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*&&US*&& MVI   AGYCURT+(CURTDECP-CURTABD),2                                     
         IC    R0,ACTION                                                        
         MVI   ACTION,ACTINIT                                                   
         GOTO1 OVERLAY,WORKD       CALL OVERLAY WITH INITIAL ACTION             
         STC   R0,ACTION                                                        
         OI    RECSCRH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
INITX    DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION                                                     *         
***********************************************************************         
         SPACE 1                                                                
         LA    R1,RECACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
*                                                                               
         USING XTRAINFD,RE                                                      
         L     RE,VXTRAINF                                                      
         TM    XIFLAG1,XIROMODE    CONNECTED IN READ ONLY MODE                  
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NAUPD)                                           
         B     FVERR                                                            
*                                                                               
         TM    XIFLAG1,XIWRONGF    CONNECTED TO WRONG FACPAK                    
         BNO   *+26                                                             
         MVC   FVMSGNO,=AL2(AE$HMADV)                                           
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'XIUPDFAC),XIUPDFAC                                      
         B     FVERR                                                            
*                                                                               
         TM    XIFLAG1,XIROSYS     CONNECTED TO READ ONLY SYSTEM                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$UPDNO)                                           
         B     FVERR                                                            
*                                                                               
VALACT   TM    TWAMODE,TWAMCFRM    TEST ACTION CONFIRMATION REQUIRED            
         BZ    VALACT02                                                         
         GOTO1 ACONFRM             YES - CALL CONFIRMATION ROUTINE              
         BE    FVERRX              CONFIRMED                                    
         BH    FVERRX8             SEEK CONFIRMATION                            
*                                                                               
VALACT02 LA    R1,RECACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         XC    TEMP(L'RECACT),TEMP                                              
         L     RF,AINP                                                          
         SR    RE,RE                                                            
         ICM   RE,1,TIOBAID-TIOBD(RF)                                           
         BNZ   *+16                                                             
         ICM   RE,1,TWANXTPF       TAKE DEFAULT PF KEY IF SET                   
         BZ    VALACT16                                                         
         MVI   TWANXTPF,0                                                       
         STC   RE,WORK                                                          
         CLI   WORK,PFK13          TEST ALTERNATE PF KEY ENTERED                
         BNE   VALACT06                                                         
         XI    TWAMODE,TWAMALTP    SWITCH TO/FROM ALTERNATE PFKEYS              
         MVI   TIOBAID-TIOBD(RF),0                                              
         B     FVERRX                                                           
*                                                                               
VALACT06 CLI   WORK,PFK15          TEST ALTERNATE TOTALS PFKEY ENTERED          
         BNE   VALACT08                                                         
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+8                                                              
         XI    TWAMODE2,TWA2ALTO   SWITCH TO/FROM ALTERNATE TOTALS              
         MVI   TIOBAID-TIOBD(RF),0                                              
         B     FVERRX                                                           
*                                                                               
VALACT08 L     R2,AAPFTAB                                                       
         USING APFTABD,R2          R2=A(ACTION TABLE)                           
VALACT10 CLI   APFTPFK,EOT         TEST E-O-T                                   
         BE    VALACT16                                                         
         CLC   APFTPFK,WORK        MATCH ON PFKEY NUMBER                        
         BNE   *+14                                                             
         CLC   APFTACT,ACTION      AND LAST TIME ACTION NUMBER                  
         BE    *+12                                                             
         LA    R2,APFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALACT10                                                         
*                                                                               
         L     R3,AACTTAB          ENSURE ACTION VALID AT THIS TIME             
         USING ACTTABD,R3                                                       
VALACT12 CLI   ACTTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTTNUM,APFTACT2    MATCH ON ACTION NUMBER                       
         BE    *+12                                                             
         LA    R3,ACTTABL(R3)                                                   
         B     VALACT12                                                         
         CLI   ACTTNUM,ACTHEAD     TEST PF KEY FOR HEADER                       
         BNE   *+12                                                             
         TM    TWAMODE,TWAMHDRS    TEST HEADER ALREADY SAVED                    
         BNZ   VALACT16            HEADER IS NOT VALID                          
         TM    ACTTIND1,ACTISHDR   DOES ACTION REQUIRE SAVED HEADER             
         BZ    *+12                                                             
         TM    TWAMODE,TWAMHDRS    TEST SAVED HEADER AVAILABLE                  
         BZ    VALACT16                                                         
         TM    ACTTIND1,ACTINALL   TEST IF ACTION REQUIRES TWA2NALL OFF         
         BZ    *+12                                                             
         TM    TWAMODE2,TWA2NALL   YES - TEST STATUS                            
         BNZ   VALACT16                                                         
         TM    ACTTIND2,ACTISECY   TEST CALL SECRET                             
         BZ    VALACT14                                                         
         OC    TWASAGN,TWASAGN     TEST USING NEW SECURITY                      
         BZ    VALACT14                                                         
         GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',ACTTNUM)             
         BNE   VALACT16                                                         
         DROP  R3                                                               
*                                                                               
VALACT14 MVC   TEMP(L'RECACT),RECACT                                            
         MVC   LAREADDR,APFTADDR                                                
         EX    0,LARE              GET A(ACTION WORD)                           
         MVC   WORK(ACTLGNLQ),0(RE)                                             
         DROP  R2                                                               
*                                                                               
         LA    RF,RECACTH          SET ACTION NAME & TRANSMIT                   
         ST    RF,FVADDR                                                        
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         SR    R1,R1                                                            
         IC    R1,FVTLEN-FVIHDR(RF)                                             
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(RF),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         XC    L'FVIHDR(0,RF),L'FVIHDR(RF)                                      
         EX    R1,*-6                                                           
         MVC   L'FVIHDR(ACTLGNLQ,RF),WORK                                       
*                                                                               
VALACT16 MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECACTH                                                    
         BNE   FVERR                                                            
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2                                                       
         SR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         CLI   ACTION,ACTINIT      IF WE HAVE AN ACTION                         
         BE    VALACT18                                                         
         MVC   ACTIONL,ACTION      SAVE IT                                      
         MVI   ACTION,ACTINIT      AND CLEAR ACTION                             
VALACT18 CLI   FVILEN,ACTSHTLQ     DOES L'INPUT EXCEED L'SHORT NAME?            
         BH    VALACT20            YES - TRY LONG NAME                          
         MVC   LAREADDR,ACTTACSH                                                
         EX    0,LARE                                                           
         CLC   0(ACTSHTLQ,RE),SPACES DO WE HAVE A SHORT NAME?                   
         BE    VALACT20            NO - TRY LONG NAME                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH ACTION SHORT NAME           
         BNE   VALACT20                                                         
         MVC   LAREADDR,ACTTACT    TAKE FULL NAME                               
         EX    0,LARE                                                           
         B     VALACT22                                                         
VALACT20 MVC   LAREADDR,ACTTACT                                                 
         EX    0,LARE                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH ACTION NAME                 
         BNE   VALACT30                                                         
VALACT22 CLI   ACTION,ACTINIT      DO WE ALREADY HAVE A MATCH?                  
         BE    VALACT24                                                         
         MVI   ACTION,ACTINIT      YES - ERROR.  CLEAR ACTION NUMBER            
         MVC   RECACT,FVIFLD       RE-TRANSMIT USER'S ACTION                    
         B     ERRSHRT             USER MUST BE MORE SPECIFIC                   
VALACT24 OC    TWASAGN,TWASAGN     TEST NEW SECURITY IN USE                     
         BNZ   VALACT26                                                         
         TM    ACTTIND1,ACTIAUTH   TEST ACTION REQUIRES AUTHORISATION           
         BZ    VALACT26                                                         
         MVC   WORK(1),ACTTIND1    TEST USER AUTHORISATION                      
         NI    WORK,ACTIAUTH                                                    
         MVC   WORK+1(1),WORK                                                   
         NC    WORK(1),TWAAUTH                                                  
         CLC   WORK(1),WORK+1                                                   
         BNE   VALACT30                                                         
*                                                                               
VALACT26 CLI   ACTTNUM,ACTHEAD     TEST ACTION VALIDITY                         
         BNE   *+12                                                             
         TM    TWAMODE,TWAMHDRS    TEST HEADER ALREADY SAVED                    
         BNZ   VALACT30            HEADER IS NOT VALID                          
         TM    ACTTIND1,ACTISHDR   DOES ACTION REQUIRE SAVED HEADER             
         BZ    *+12                                                             
         TM    TWAMODE,TWAMHDRS    TEST SAVED HEADER AVAILABLE                  
         BZ    VALACT30                                                         
         TM    ACTTIND1,ACTINALL   TEST IF ACTION REQUIRES TWA2NALL OFF         
         BZ    *+12                                                             
         TM    TWAMODE2,TWA2NALL   YES - TEST STATUS                            
         BNZ   VALACT30                                                         
         TM    ACTTIND2,ACTISECY   TEST CALL SECRET                             
         BZ    VALACT28                                                         
         OC    TWASAGN,TWASAGN     TEST USING NEW SECURITY                      
         BZ    VALACT28                                                         
         GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',ACTTNUM)             
         BNE   ERRSECLK                                                         
*                                                                               
VALACT28 MVC   ACTION,ACTTNUM      TAKE ACTION NUMBER                           
         MVC   ACTINDS,ACTTIND1    SAVE ACTION INDICATORS                       
         MVC   LAREADDR,ACTTACT    TAKE FULL NAME                               
         EX    0,LARE                                                           
         MVC   RECACT,0(RE)        SET FULL ACTION NAME                         
         OI    RECACTH+(FVOIND-FVIHDR),FVOXMT                                   
         ST    R2,AACTNTRY                                                      
*                                                                               
VALACT30 LA    R2,ACTTABL(R2)                                                   
         CLI   0(R2),EOT           FINISHED WITH TABLE?                         
         BNE   VALACT18            NO - NEXT ACTION                             
         DROP  R2                                                               
*                                                                               
         OC    TEMP(L'RECACT),TEMP                                              
         BZ    VALACT32                                                         
         TM    ACTINDS,ACTIXSET    TEST WANT ORIGINAL ACTION                    
         BZ    VALACT32                                                         
         MVC   RECACT,TEMP         YES - RESTORE SAVED ACTION                   
*                                                                               
VALACT32 SR    RF,RF                                                            
         IC    RF,ACTION                                                        
         SLL   RF,2                                                             
         AHI   RF,4                                                             
         B     *(RF)                                                            
         B     ERRNOTV             ACTION=0, ERROR                              
         B     VALACT36            HEADER                                       
         B     VALACT40            INPUT                                        
         B     VALACT38            SPECIAL                                      
         B     VALACT44            RESTORE                                      
         B     VALACT50            QUIT                                         
         B     VALACT60            UPDATE                                       
         B     VALACT70            DRAFT  (JOURNAL)                             
         B     VALACT70            FILTER (JOURNAL)                             
         B     VALACT80            ALTPFS                                       
         B     VALACT84            ADD/CHANGE BILLING SOURCE LIST               
*                                                                               
VALACT36 TM    TWAMODE2,TWA2BSRC+TWA2MEMO  TEST BILLING SOURCE MODE             
         BNZ   ERRNOTV                                                          
         TM    TWAMODE,TWAMHDRS    HEADER - TEST HEADER SAVED                   
         BZ    VALACTX             OK IF NO HEADER SAVED                        
         B     ERRNOTV                                                          
VALACT38 TM    TWAMODE2,TWA2MEMO   SPECIAL - TEST TRANSACTION MEMO MODE         
         BNZ   ERRNOTV                                                          
VALACT40 TM    TWAMODE2,TWA2BSRC   INPT/SPEC - TEST BILLING SOURCE MODE         
         BNZ   ERRNOTV                                                          
         TM    TWAMODE,TWAMHDRS    INPUT/SPECIAL REQ PREVIOUS HEADER            
         BNZ   VALACTX                                                          
         B     ERRNOTV                                                          
VALACT44 TM    TWAMODE,TWAMHDRS    RESTORE REQUIRES PREVIOUS HEADER             
         BZ    ERRNOTV                                                          
         TM    TWAMODE2,TWA2BSRC+TWA2MEMO  TEST BILLING SOURCE MODE             
         BNZ   ERRNOTV                                                          
         CLI   ACTIONL,ACTINPT     RESTORE MAY FOLLOW INPUT/SPECIAL             
         BE    VALACTX                                                          
         CLI   ACTIONL,ACTOVER                                                  
         BE    VALACTX                                                          
         CLI   ACTIONL,ACTREST                                                  
         BE    VALACTX                                                          
         B     ERRNOTV                                                          
VALACT50 TM    TWAMODE2,TWA2BSRC+TWA2MEMO  TEST BILLING SOURCE MODE             
         BNZ   ERRNOTV                                                          
         TM    TWAMODE,TWAMHDRS    QUIT REQUIRES PREVIOUS HEADER                
         BNZ   VALACTX                                                          
         B     ERRNOTV                                                          
VALACT60 TM    TWAMODE2,TWA2BSRC+TWA2MEMO  TEST BILLING SOURCE MODE             
         BNZ   ERRNOTV                                                          
         TM    TWAMODE,TWAMHDRS    UPDATE REQUIRES PREVIOUS HEADER              
         BZ    ERRNOTV                                                          
         CLI   ACTIONL,ACTINPT     UPDATE MAY FOLLOW INPUT/REST/SPECIAL         
         BE    VALACTX                                                          
         CLI   ACTIONL,ACTREST                                                  
         BE    VALACTX                                                          
         CLI   ACTIONL,ACTOVER                                                  
         BE    VALACTX                                                          
         B     ERRNOTV                                                          
VALACT70 TM    TWAMODE,TWAMHDRS    DRAFT/FILTER REQ PREVIOUS HEADER             
         BZ    ERRNOTV                                                          
         TM    TWAMODE2,TWA2BSRC+TWA2MEMO  TEST BILLING SOURCE MODE             
         BNZ   ERRNOTV                                                          
         CLI   ACTIONL,0           DRAFT/FILTER CAN'T BE FIRST TIME             
         BNE   VALACTX                                                          
         B     ERRNOTV                                                          
VALACT80 XI    TWAMODE,TWAMALTP    SWITCH TO/FROM ALTERNATE PFKEYS              
         MVC   ACTION,ACTIONL      RESET ACTION NUMBER TO LAST ACTION           
         MVC   RECACT,ACTNAME      AND ACTION NAME TO LAST ACTION NAME          
         OI    RECACTH+(FVOIND-FVIHDR),FVOXMT                                   
         B     FVERRX                                                           
VALACT84 TM    TWAMODE2,TWA2MEMO   SOURCE - TEST TRANSACTION MEMO MODE          
         BNZ   ERRNOTV                                                          
         B     GO                                                               
VALACTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SCROLL AMOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
VALSCR   XC    SCRVALS(SCRVALSL),SCRVALS                                        
         MVC   TEMP(L'RECSCR),RECSCR                                            
         CLI   ACTION,ACTINPT                                                   
         BNE   VALSCR30                                                         
         L     RF,AINP                                                          
         MVC   WORK(1),TIOBAID-TIOBD(RF)                                        
         CLI   WORK,0                                                           
         BE    VALSCR04                                                         
         L     R1,ASPFTAB                                                       
         USING SPFTABD,R1          R1=A(SCROLL PFKEY TABLE)                     
VALSCR02 CLI   SPFTPFK,EOT         TEST E-O-T                                   
         BE    VALSCR04                                                         
         CLC   SPFTPFK,WORK        MATCH TABLE PFKEY TO INPUT                   
         BE    *+12                                                             
         LA    R1,SPFTABL(R1)                                                   
         B     VALSCR02                                                         
         MVC   LAREADDR,SPFTADDR                                                
         EX    0,LARE                                                           
         XC    RECSCR,RECSCR       CLEAR & BUILD SCROLL FIELD                   
         LA    RF,RECSCR                                                        
         CLI   SPFTUPDN,0          TEST EXPLICIT DIRECTION                      
         BE    *+14                                                             
         MVC   0(L'SPFTUPDN,RF),SPFTUPDN                                        
         LA    RF,L'SPFTUPDN(RF)                                                
         MVC   0(L'SCR@PAGE,RF),0(RE)                                           
         MVI   RECSCRH+(FVILEN-FVIHDR),L'SCR@PAGE+L'SPFTUPDN                    
         OI    RECSCRH+(FVOIND-FVIHDR),FVOXMT                                   
         DROP  R1                                                               
*                                                                               
VALSCR04 CLI   RECSCRH+(FVILEN-FVIHDR),0                                        
         BE    VALSCR30                                                         
         CLI   RECSCR,SCRFIND      TEST KEY LOCATE                              
         BE    VALSCR20                                                         
         MVI   SCRUPDN,SCRDN       PRESET TO SCROLL DOWN                        
*                                                                               
         CLI   RECSCR,SCRDN        TEST VALID DIRECTION                         
         BE    *+12                                                             
         CLI   RECSCR,SCRUP                                                     
         BNE   VALSCR06                                                         
         MVC   SCRUPDN,RECSCR      SET DIRECTION & REMOVE IT                    
         MVC   RECSCR(L'RECSCR-1),RECSCR+1                                      
         MVI   RECSCR+(L'RECSCR-1),C' '                                         
*                                                                               
VALSCR06 MVI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         GOTO1 AFVAL,RECSCRH                                                    
         BNE   FVERR                                                            
         TM    FVIIND,FVINUM       IS INPUT NUMERIC                             
         BZ    VALSCR08                                                         
         L     R0,FULL                                                          
         CH    R0,=Y(SCRPAGE)      UP TO A PAGE ALLOWED                         
         BH    ERRISCRL                                                         
         STCM  R0,3,SCROLL         SAVE SCROLL AMOUNT                           
*                                                                               
         XC    RECSCR,RECSCR       RE-DISPLAY SCROLL AMOUNT                     
         MVC   RECSCR(1),SCRUPDN                                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RECSCR+1(2),DUB                                                  
         CLI   RECSCR+1,C'0'                                                    
         BNE   *+10                                                             
         MVC   RECSCR+1(2),RECSCR+2                                             
         B     VALSCR30                                                         
*                                                                               
VALSCR08 CLI   FVILEN,L'SCR@PAGE   TEST INPUT GR MAXIMUM LEN                    
         BH    ERRLONG                                                          
         L     R1,ASCRTAB          LOOK-UP SCOLL TABLE                          
         USING SCRTABD,R1                                                       
VALSCR10 SR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         MVC   LAREADDR,SCRTSCR                                                 
         EX    0,LARE                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH SCROLL NAME                 
         BNE   VALSCR16                                                         
*                                                                               
VALSCR12 OC    SCROLL,SCROLL       DO WE ALREADY HAVE A MATCH?                  
         BZ    VALSCR14                                                         
         MVC   RECSCR,TEMP         RESTORE ORIGINAL SCROLL FIELD                
         B     ERRSHRT                                                          
*                                                                               
VALSCR14 XC    RECSCR,RECSCR       RE-DISPLAY SCROLL                            
         MVC   RECSCR(1),SCRUPDN                                                
         LA    RF,RECSCR+1                                                      
         TM    SCRTIND1,SCRTIHRZ   TEST HORIZONTAL SCROLL                       
         BZ    *+6                                                              
         BCTR  RF,0                OVERWRITE DIRECTION                          
         MVC   0(L'SCR@PAGE,RF),0(RE)                                           
         MVC   SCROLL,SCRTAMT      TAKE SCROLL MAGNITUDE FROM TABLE             
         CLI   SCRTUPDN,0          TEST DIRECTION IS EXPLICIT                   
         BE    *+10                                                             
         MVC   SCRUPDN,SCRTUPDN    YES - SET DIRECTION                          
*                                                                               
VALSCR16 LA    R1,SCRTABL(R1)                                                   
         CLI   0(R1),EOT           FINISHED WITH TABLE?                         
         BNE   VALSCR10            NO - NEXT SCROLL KEYWORD                     
         OC    SCROLL,SCROLL                                                    
         BZ    ERRNOTV             ERROR - NO MATCH                             
*                                                                               
VALSCR18 CLC   SCROLL,SCRMAXIL     TEST MAXIMUM SCROLL REQUEST                  
         BNE   VALSCR30                                                         
         XC    RECSCR,RECSCR       SET SCROLL TO +PAGE OR -PAGE                 
         MVI   RECSCR,SCRDN                                                     
         CLI   SCRUPDN,SCRUP                                                    
         BE    *+8                                                              
         MVI   RECSCR,SCRUP                                                     
         MVC   RECSCR+1(L'SCR@PAGE),SCR@PAGE                                    
         B     VALSCR30                                                         
*                                                                               
VALSCR20 GOTO1 AFVAL,RECSCRH       LOCATION SCROLLING (*KEYWORD=VALUE)          
         CLI   FVILEN,4                                                         
         BNH   ERRNOTV                                                          
         LA    R1,FVIFLD+2                                                      
         LA    RE,1                                                             
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD-1(RF)                                                  
         CLI   0(R1),C'='                                                       
         BE    *+12                                                             
         BXLE  R1,RE,*-8                                                        
         B     ERRNOTV                                                          
         LA    R0,FVIFLD+1                                                      
         SR    R1,R0                                                            
         STC   R1,WORK+00          WORK+00(1)=LENGTH OF KEYWORD                 
         BCTR  R1,0                R1=LENGTH-1 OF KEYWORD                       
         MVC   WORK+01(0),FVIFLD+1                                              
         EX    R1,*-6                                                           
*                                                                               
         LA    R1,3(R1)            R1=LENGTH OF *KEYWORD= PORTION               
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         SR    RF,R1               RF=LENGTH OF VALUE                           
         STC   RF,WORK+20          WORK+20(1)=LENGTH OF VALUE                   
         LA    R1,FVIFLD(R1)                                                    
         BCTR  RF,0                                                             
         MVC   WORK+21(0),0(R1)                                                 
         EX    RF,*-6                                                           
*                                                                               
         L     R1,ALOCTAB          TABLE OF LOCATION FIELDS                     
         USING LOCTABD,R1                                                       
         IC    RF,WORK                                                          
         CLM   RF,1,=AL1(OPTLONG)                                               
         BH    ERRLONG             KEYWORD TOO LONG                             
VALSCR22 MVC   LAREADDR,LOCLOSH    TAKE SHORT KEYWORD                           
         CLM   RF,1,=AL1(OPTSHRT)                                               
         BNH   *+10                                                             
VALSCR24 MVC   LAREADDR,LOCLOC     TAKE LONG KEYWORD                            
         EX    0,LARE                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK+01(0),0(RE)                                                 
         BE    VALSCR26                                                         
         IC    RF,WORK             REFRESH LENGTH                               
         CLC   LAREADDR,LOCLOC     HAVE WE TRIED LONG KEYWORD?                  
         BNE   VALSCR24                                                         
         LA    R1,LOCTABL(R1)                                                   
         CLI   0(R1),EOT                                                        
         BNE   VALSCR22                                                         
         B     ERRNOTV                                                          
*                                                                               
VALSCR26 IC    RF,WORK+20          L'SECOND PART                                
         CLM   RF,1,LOCRHMAX       CHECK MAXIMUM LENGTH OF RH                   
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         STC   RF,WORK+20                                                       
         MVC   LARFADDR,LOCADDR    ROUTINE SCON                                 
         EX    0,LARF              GET ROUTINE ADDRESS                          
         BR    RF                  BRANCH TO ROUTINE                            
VALSCR28 XC    RECSCR,RECSCR       CLEAR SCROLL FIELD                           
         OI    DISIND,DISIRST      SET TO RESTART FROM BEGINNING                
*                                                                               
VALSCR30 OC    SCRVALS(SCRVALSL),SCRVALS                                        
         BNZ   VALSCRX                                                          
         MVC   SCROLL,SCRPAGEL     SET DEFAULT SCROLL (+PAGE)                   
         MVI   SCRUPDN,SCRDN                                                    
         XC    RECSCR,RECSCR                                                    
         MVC   RECSCR(1),SCRUPDN                                                
         MVC   RECSCR+1(L'SCR@PAGE),SCR@PAGE                                    
VALSCRX  B     VALREP                                                           
         EJECT                                                                  
***********************************************************************         
* SPECIFIC VALIDATION FOR LOCATE-TYPE SCROLL COMMANDS.                *         
* ON ENTRY WORK(1)=L'INPUT-1 FOR EX.  WORK+1(N) CONTAINS R.H.S. OF    *         
* LOCATE. WORK IS SPACE-FILLED.                                       *         
***********************************************************************         
         SPACE 1                                                                
VLODATE  GOTO1 VDATVAL,DMCB,(0,WORK+21),WORK                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRINVDT                                                         
         GOTO1 VDATCON,DMCB,(0,WORK),(1,LOCDAT)                                 
         OC    LOCDAT,LOCDAT                                                    
         BZ    ERRINVDT                                                         
         B     VLOEXIT                                                          
         SPACE 1                                                                
VLOBILL  MVC   LOCBILXQ,WORK+20    LENGTH-1                                     
         MVC   LOCBIL,WORK+21      VALUE                                        
         B     VLOEXIT                                                          
         SPACE 1                                                                
VLOACCT  MVC   LOCACTXQ,WORK+20    LENGTH-1                                     
         MVC   LOCACT,WORK+21      VALUE                                        
         B     VLOEXIT                                                          
         SPACE 1                                                                
VLOSRCE  MVC   LOCCACXQ,WORK+20    LENGTH-1                                     
         MVC   LOCCAC,WORK+21      VALUE                                        
         B     VLOEXIT                                                          
         SPACE 1                                                                
VLOEXIT  B     VALSCR28                                                         
VLOERRX  B     ERRNOTV                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT-ID                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALREP   XC    PRTSUB,PRTSUB       CLEAR REPORT-ID                              
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    VALREPX             DONT BOTHER WIH REPORT                       
*                                                                               
         GOTO1 AFVAL,RECREPH                                                    
         BNE   VALREPX                                                          
         MVC   PRTSUB,FVIFLD       SET REPORT-ID                                
VALREPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   MVC   TEMP(KEYOPTSL),KEYOPTS                                           
         XC    OPTIONS(OPTIONSL),OPTIONS                                        
         CLI   RECOPTH+(FVILEN-FVIHDR),0                                        
         BE    VALOPTX                                                          
         LA    R1,RECOPTH                                                       
         ST    R1,FVADDR           TAKE FIELD ADDRESS FOR ERRORS                
         L     R0,ASCANOUT         CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,RECOPTH),                     *        
               ('SCANMAXN',ASCANOUT),C',=  '                                    
         MVC   BYTE,4(R1)                                                       
         CLI   BYTE,0                                                           
         BE    ERRNOTV                                                          
         MVI   FVINDX,1                                                         
         L     R2,ASCANOUT                                                      
         USING SCANOUTD,R2                                                      
VALOPT10 MVI   FLAG,0                                                           
         CLI   SCANTXT1,C'*'       TEST 'NOT' PREFIX ON KEYWORD                 
         BNE   VALOPT20                                                         
         CLI   SCANLEN1,2                                                       
         BL    VALOPT20                                                         
         MVC   SCANTXT1(L'SCANTXT1-1),SCANTXT1+1                                
         IC    RF,SCANLEN1                                                      
         BCTR  RF,0                                                             
         STC   RF,SCANLEN1                                                      
         OI    FLAG,1              SET 'NOT' INPUT                              
VALOPT20 L     R1,AOPTTAB          R1=A(OPTIONS TABLE)                          
         USING OPTTABD,R1                                                       
VALOPT22 SR    RF,RF               EXTRACT INPUT LENGTH & CHECK                 
         ICM   RF,1,SCANLEN1                                                    
         BZ    ERRNONE                                                          
         BCTR  RF,0                                                             
         CLM   RF,1,=AL1(OPTLONG-1)                                             
         BH    ERRLONG             OPTION KEYWORD TOO LONG                      
         CLM   RF,1,=AL1(OPTSHRT-1)                                             
         BH    VALOPT24                                                         
         MVC   LAREADDR,OPTOPSH    TRY SHORT KEYWORD                            
         B     *+10                                                             
VALOPT24 MVC   LAREADDR,OPTOPT     FULL KEYWORD                                 
         EX    0,LARE                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCANTXT1(0),0(RE)                                                
         BE    VALOPT30                                                         
         CLC   LAREADDR,OPTOPT     HAVE WE TRIED FULL KEYWORD YET               
         BNE   VALOPT24            NO - TRY IT                                  
*                                                                               
VALOPT26 LA    R1,OPTTABL(R1)                                                   
         CLI   0(R1),EOT           AND IF THERE'S ANOTHER ENTRY                 
         BNE   VALOPT22            GO BACK TO TRY IT                            
         B     ERRNOTV             NO MATCH - ERROR                             
*                                                                               
VALOPT30 CLI   FLAG,1              TEST 'NOT' INPUT                             
         BNE   *+12                                                             
         TM    OPTIND,OPTINOT      TEST 'NOT' ALLOWED                           
         BZ    VALOPT26                                                         
         SR    RF,RF                                                            
         ICM   RF,3,OPTADDR                                                     
         TM    OPTIND,OPTI2PT                                                   
         BO    VALOPT40                                                         
         CLI   SCANLEN2,0          ONE-PART MUST NOT HAVE 2ND PART              
         BNE   VALOPT26                                                         
         MVC   LARFADDR,OPTADDR                                                 
         EX    0,LARF                                                           
         CLI   0(RF),0             TEST OPTION ALREADY INPUT                    
         BNE   ERRKDUP             DUPLICATE OPTION                             
         TM    OPTIND,OPTISWT      SWITCH OPTION TO ALLOCATE                    
         BZ    VALOPT32                                                         
         MVC   FVIFLD(1),AC@YES    DUMMY ALLOCATE                               
         LR    R0,RF               SAVE RF                                      
         GOTO1 ATSTSEC                                                          
         LR    RF,R0               RESTORE RF                                   
         BNE   ERRSECLK                                                         
VALOPT32 MVI   0(RF),INCLUDE                                                    
         TM    FLAG,1              TEST 'NOT' INPUT                             
         BZ    *+8                                                              
         MVI   0(RF),EXCLUDE                                                    
         B     VALOPT60                                                         
*                                                                               
VALOPT40 CLI   SCANLEN2,0          TWO-PART REQUIRE 2ND PART                    
         BE    VALOPT26                                                         
         CLC   SCANLEN2,OPTRMAX    CHECK MAXIMUM LENGTH ALLOWED                 
         BH    ERRLONG                                                          
         MVC   LARFADDR,OPTADDR                                                 
         EX    0,LARF                                                           
         BR    RF                  BRANCH TO MINI-ROUTINE                       
*                                                                               
VALOPT60 IC    R1,FVINDX           BUMP TO NEXT SCANNER ENTRY                   
         LA    R1,1(R1)                                                         
         CLM   R1,1,BYTE                                                        
         BH    VALOPTX                                                          
         STC   R1,FVINDX                                                        
         LA    R2,SCANOUTL(R2)     TAKE NEXT OPTION                             
         B     VALOPT10                                                         
*                                                                               
VALOPTX  CLC   KEYOPTS(KEYOPTSL),TEMP                                           
         BE    *+8                                                              
         OI    DISIND,DISIRST      SET TO RESTART FROM BEGINNING                
         MVI   FVINDX,0                                                         
         MVC   FVMSGNO,=AL2(IGOK)                                               
         B     GO                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIFIC VALIDATION FOR TWO-PART OPTIONS.                           *         
* ON ENTRY R2=A(SCANNER BLOCK ELEMENT).  FVMSGNO=EGOPTDUP.            *         
***********************************************************************         
         SPACE 1                                                                
VOPDATE  OC    OPTDATE,OPTDATE                                                  
         BNZ   ERRKDUP                                                          
         GOTO1 VDATVAL,DMCB,(0,SCANTXT2),WORK                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINVDT                                                         
         GOTO1 VDATCON,DMCB,(0,WORK),(1,OPTDATE)                                
         OC    OPTDATE,OPTDATE                                                  
         BZ    ERRINVDT                                                         
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPBILL  OC    OPTBIL,OPTBIL                                                    
         BNZ   ERRKDUP                                                          
         MVC   OPTBIL,SCANTXT2                                                  
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         STC   R1,OPTBILXQ                                                      
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPACCT  OC    OPTACTX,OPTACTX                                                  
         BNZ   ERRKDUP                                                          
         LA    R0,RECVMAXN                                                      
         L     R1,ARECVTAB                                                      
         USING RECVTABD,R1                                                      
VOPAC02  CLI   SCANTXT2,C'*'       TEST *ULACCOUNT                              
         BE    *+14                                                             
         CLC   RECVACT+3(L'RECVACT-3),SCANTXT2                                  
         B     *+10                                                             
         CLC   RECVACT+1(L'RECVACT-1),SCANTXT2+1                                
         BE    VOPAC10                                                          
         LA    R1,RECVTABL(R1)                                                  
         BCT   R0,VOPAC02                                                       
         B     ERRERNF                                                          
VOPAC10  MVC   OPTACTX,RECVNDX                                                  
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPSRCE  OC    OPTCAC,OPTCAC                                                    
         BNZ   ERRKDUP                                                          
         MVC   OPTCAC,SCANTXT2                                                  
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         STC   R1,OPTCACXQ                                                      
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPAMNT  OC    OPTAMT,OPTAMT                                                    
         BNZ   ERRKDUP                                                          
         SR    R0,R0                                                            
         IC    R0,SCANLEN2                                                      
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,PARM,((RF),SCANTXT2),(X'C0',(R0))                       
         CLI   0(R1),0                                                          
         BNE   ERRIAMNT                                                         
         ZAP   DUB,4(8,R1)                                                      
         OC    DUB(2),DUB          ENSURE WILL PACK INTO 6 BYTES                
         BNZ   ERRIAMNT                                                         
         ZAP   OPTAMT,DUB          SET FILTER AMOUNT                            
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPOFFC  OC    OPTOFF,OPTOFF                                                    
         BNZ   ERRKDUP                                                          
         MVC   OPTOFF,SCANTXT2     SET OFFICE FILTER                            
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPDISP  OC    OPTDIS,OPTDIS                                                    
         BNZ   ERRKDUP                                                          
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RE,SCANTXT2-1(RF)                                                
         CLI   0(RE),C'+'          TEST PREFIX COLUMN(S)                        
         BE    *+12                                                             
         CLI   SCANTXT2,C'+'       TEST SUFFIX COLUMN(S)                        
         BNE   VOPDIS04                                                         
         CLI   SCANLEN2,2          TEST NO ACTUAL COLUMNS (DIS=+)               
         BL    ERRNOTV                                                          
         BNE   VOPDIS02                                                         
         CLC   SCANTXT2(1),SCANTXT2+1                                           
         BNE   VOPDIS02                                                         
         OI    OPTDIND,OPTDIALQ    SET ALL COLUMNS INDICATOR (DIS=++)           
         LA    RF,L'OPTDIS-1                                                    
         CLM   RF,1,=AL1(COLTABL-1)                                             
         BNH   *+8                                                              
         LA    RF,COLTABL-1                                                     
         MVC   OPTDIS(0),COLTAB+COLTABL                                         
         EX    RF,*-6                                                           
         B     VOPEXIT                                                          
VOPDIS02 CLI   SCANTXT2,C'+'       TEST SUFFIX COLUMN(S)                        
         BE    *+12                                                             
         OI    OPTDIND,OPTDIPRQ    SET PREFIX COLUMNS INDICATOR                 
         B     *+14                                                             
         OI    OPTDIND,OPTDIADQ    SET SUFFIX COLUMNS INDICATOR                 
         MVC   SCANTXT2,SCANTXT2+1 AND SHUFFLE UP FIELD                         
         MVI   SCANTXT2+L'SCANTXT2-1,C' '                                       
         BCTR  RF,0                DROP 1                                       
         STC   RF,SCANLEN2         SAVE NEW LENGTH                              
*                                                                               
VOPDIS04 LA    RE,SCANTXT2                                                      
VOPDIS06 LA    R1,COLTAB                                                        
         LA    R0,COLTABN                                                       
         CLC   0(1,RE),0(R1)                                                    
         BE    VOPDIS08                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     ERRNOTV             COLUMN NOT FOUND                             
VOPDIS08 MVC   0(1,RE),COLTABL(R1) SUBSTITUTE HEX VALUE                         
         LA    RE,1(RE)            NEXT COLUMN SELECTION                        
         BCT   RF,VOPDIS06                                                      
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         MVC   OPTDIS(0),SCANTXT2                                               
         EX    RF,*-6                                                           
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPCURR  OC    OPTCUR,OPTCUR                                                    
         BNZ   ERRKDUP                                                          
         GOTO1 VBLDCUR,DMCB,(0,SCANTXT2),(0,WORK),ACOM                          
         CLI   0(R1),0                                                          
         BNE   ERRNOTV                                                          
         MVC   OPTCUR,SCANTXT2     SET CURRENCY FILTER                          
         B     VOPEXIT                                                          
         SPACE 1                                                                
VOPERRX  B     FVERR                                                            
VOPEXIT  B     VALOPT60                                                         
         DROP  R1,R2                                                            
         EJECT                                                                  
COLTAB   DC    C'123456789ABCDEFGHIJK'  DISPLAY COLUMNS IN CHARACTER            
COLTABN  EQU   *-COLTAB                 CURRENT KNOWN COLUMNS                   
         DC    C'LMNOPQRSTUVWXYZ'       FUTURE COLUMNS                          
         DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)          
         DC    AL1(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)                
COLTABL  EQU   (*-COLTAB)/2             LENGTH OF EACH HALF                     
         EJECT                                                                  
***********************************************************************         
* GO TO ROUTINE BASED ON ACTION NUMBER                                *         
***********************************************************************         
         SPACE 1                                                                
GO       SR    RF,RF               GO TO ACTION ROUTINE                         
         IC    RF,ACTION                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     VALHED              HEADER  - VALIDATE ALL FIELDS                
         B     DISVAL              INPUT   - DISP/VAL TRANSACTIONS              
         B     OVRVAL              SPECIAL - DISP/VAL SPECIAL ITEMS             
         B     RESHED              RESTORE - RESTORE HEADER SCREEN              
         B     NXTHED              QUIT    - CLEAR/RESTORE HEADER               
         B     UPDATE              UPDATE  - ADD TRANSACTIONS                   
         B     PRTREP              DRAFT   - (UNFILTERED) JOURNAL               
         B     PRTREP              FILTER  - (FILTERED) JOURNAL                 
         DC    AL4(0)              ALTPFS  - HANDLED SEPARATELY                 
         B     BILSRC              SOURCE  - ADD/CHANGE BILLING SOURCES         
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN                                              *         
***********************************************************************         
         SPACE 1                                                                
VALHED   NI    TWAMODE2,FF-(TWA2NALL)                                           
         L     R1,ATSARBLK                                                      
         USING TSARD,R1            R1=A(TSAR BLOCK)                             
         TM    TSINDS,TSIANYAD                                                  
         BZ    VALHED2                                                          
         NI    TSINDS,255-TSIINIOK-TSIANYAD                                     
         MVI   TSACTN,TSAINI                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
VALHED2  IC    R1,TWASCRBH                                                      
         CLM   R1,1,TWASCROV       TEST HEADER SCREEN LOADED                    
         BE    VALHED4                                                          
         GOTO1 AOVRSCR                                                          
         GOTO1 AHDRSEC             SECURE THE HEADER SCREEN                     
         LA    R1,RECOLAYH         SET CURSOR TO FIRST UNPROT                   
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+14                                                             
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         ST    R1,FVADDR                                                        
         CLI   ACTION,ACTUPDT      TEST ACTION IS UPDATE                        
         BE    FVERR                                                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(IAENTBAT)                                           
         B     FVERR                                                            
*                                                                               
VALHED4  ZAP   DISCAMT,PZERO                                                    
*&&UK*&& ZAP   DISCAM2,PZERO                                                    
         ZAP   WOFFAMT,PZERO                                                    
*&&UK*&& ZAP   WOFFAM2,PZERO                                                    
*&&UK*&& ZAP   VATAMT,PZERO                                                     
         LA    R1,MAINTOT                                                       
         LA    R0,MAINTOTN                                                      
         ZAP   0(L'MAINTOT,R1),PZERO                                            
         LA    R1,L'MAINTOT(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 OVERLAY,WORKD       VALIDATE THE HEADER SCREEN                   
         BNE   FVERR                                                            
*                                                                               
VALHEDX  GOTO1 AREADTR             READ AND FILTER SR TRANSACTIONS              
         BH    FVERR               ERROR                                        
         BL    OVRBLD              NO TRANSACTIONS FOUND                        
         B     DISVAL              ALL OK                                       
         EJECT                                                                  
***********************************************************************         
* RESTORE - RESTORE HEADER SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
RESHED   MVC   FVMSGNO,=AL2(EGINVASQ)                                           
         TM    TWAMODE,TWAMHDRS    TEST VALID HEADER SAVED                      
         BZ    FVERR                                                            
         CLC   TWASCROV,TWASCRBH   TEST BATCH HEADER SCREEN LOADED              
         BE    CHAHED                                                           
         MVC   TWASCRSV,TWASCROV   SAVE CURRENT SCREEN NUMBER                   
         LA    RF,SAVHEADL                                                      
         ICM   RF,12,=C'L='                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(1,0),ATIA,,(RF)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,RECOLAYH                                                      
         LA    R1,SAVHEADL         RECOLAY->TWAD+3072 SAVED                     
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE TIA SCREEN INTO TWA                     
         GOTO1 AXMTSCR                                                          
         MVC   TWASCROV,TWASCRBH                                                
         GOTOR VPROTOFF                                                         
         L     RF,AUTL             RESTORE UTL MAP FOR HEADER SWAP              
         MVC   TSCRNE-UTLD(L'TWAUTLSV,RF),TWAUTLSV                              
         GOTOR VPROTON                                                          
         GOTO1 OVERLAY,WORKD       GIVE APPLICATION A CALL                      
*                                                                               
         LA    R1,RECOLAYH         LOCATE UNPROT FOR CURSOR POSITION            
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
RESHED2  ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    RESHED4                                                          
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+8                                                              
         BXLE  R1,RE,RESHED2                                                    
         ST    R1,FVADDR                                                        
*                                                                               
RESHED4  MVC   FVMSGNO,=AL2(IACHABAT)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* RESTORE - CHANGE BATCH HEADER VALUES                                *         
***********************************************************************         
         SPACE 1                                                                
CHAHED   MVC   ACTIONL,ACTION      SAVE CURRENT ACTION                          
         MVI   ACTION,ACTCHNG      SET ACTION TO CHANGE (HEADER)                
         GOTO1 OVERLAY,WORKD                                                    
         MVC   ACTION,ACTIONL      RESTORE ACTION & EXIT ON ERROR               
         BNE   FVERR                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(1,0),RECOLAYH                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
*                                                                               
         CLI   TWASCRSV,TWASCRIN   TEST INPUT SCREEN SAVED                      
         BNE   CHAHED2                                                          
         GOTO1 AOVRSCR,TWASCRIN    OVERLAY INPUT SCREEN                         
         MVI   TWASCRSV,0          RESET SAVED SCREEN NUMBER                    
         XC    RECACT,RECACT                                                    
         OI    RECACTH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   ACTION,ACTINPT                                                   
         MVC   RECACT(L'AC8INP),AC8INP                                          
         B     DISOUT                                                           
*                                                                               
CHAHED2  CLI   TWASCRSV,TWASCROP   TEST SPECIAL SCREEN SAVED                    
         BNE   CHAHED4                                                          
         MVI   TWASCRSV,0          RESET SAVED SCREEN NUMBER                    
         MVC   RECACT(L'AC8SPCL),AC8SPCL                                        
         MVI   ACTION,ACTOVER                                                   
         B     OVRBLD                                                           
*                                                                               
CHAHED4  DC    H'0'                UNKNOWN SCREEN                               
         EJECT                                                                  
***********************************************************************         
* PRINT DRAFT (FILTERED) REPORT                                       *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   OC    PRTSUB,PRTSUB       TEST REPORT-ID GIVEN                         
         BNZ   PRTREP02                                                         
         MVC   ACTION,ACTIONL      RESET ACTION TO PREVIOUS                     
         NI    TWAMODE,255-TWAMDRFT                                             
         OI    TWAMODE,TWAMSETP    SET PF KEY ON EXIT                           
         LA    R1,RECREPH          NO - MISSING INPUT FIELD                     
         ST    R1,FVADDR                                                        
         B     ERRNONE                                                          
*                                                                               
PRTREP02 OI    TWAMODE,TWAMDRFT    SET DRAFT JOURNAL MODE                       
         MVC   UPDACTN,ACTION      SAVE CURRENT ACTION                          
         MVI   ACTION,ACTUPDT      SET ACTION CODE FOR OVERLAY                  
         GOTO1 AUPDTRN             UPDATE/DRAFT/FILTER                          
         BL    FVERR               DRAFT/FILTERED JOURNAL - CONTINUE            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS (PRINT REPORT)                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDATE   DS    0H                                                               
*&&US                                                                           
         OC    PRTSUB,PRTSUB       TEST REPORT-ID GIVEN                         
         BNZ   UPDAT10                                                          
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    UPDAT10             DONT BOTHER WIH REPORT                       
         CLI   PROFRREP,C'Y'       TEST REPORT REQUIRED FOR UPDATE              
         BNE   UPDAT10                                                          
         OI    TWAMODE,TWAMSETP    SET PF KEY ON EXIT                           
         LA    R1,RECREPH          NO - MISSING INPUT FIELD                     
         ST    R1,FVADDR                                                        
         B     ERRNONE                                                          
*&&                                                                             
UPDAT10  MVC   UPDACTN,ACTION      SAVE CURRENT ACTION                          
         CLI   ACTION,ACTUPDT      ACTION IS UPDATE                             
         JNE   UPDAT15             NO, CONTINUE                                 
UPDAT12  CLI   ANYUPDAT,1          ANY UPDATE?                                  
         BE    UPDAT15             YES, CONTINUE                                
UPDAT14  MVC   ACTION,ACTIONL      RESET ACTION TO PREVIOUS                     
         LA    R1,RECACTH          SET CURSOR & EXIT WITH ERROR IF NOT          
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$NTGUP)                                           
         J     FVERR                                                            
UPDAT15  DS    0H                                                               
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE FOREIGN CURRENCY                 
         BZ    UPDAT20                                                          
         ZAP   DUB,CHQAM2          CHEQUE AMOUNT (CURRENCY)                     
         SP    DUB,MRKTO2          MINUS ALLOCATION (CURRENCY)                  
         SP    DUB,DIFTO2          MINUS DIFFERENCES (CURRENCY)                 
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    DUB,DISCAM2         PLUS DISCOUNT (CURRENCY)                     
         CP    VATAMT,PZERO        SINGLE CURRENCY - NO VAT                     
         BE    UPDAT30                                                          
         DC    H'0'                                                             
*&&                                                                             
UPDAT20  ZAP   DUB,CHQAMT          CHEQUE AMOUNT                                
         SP    DUB,MRKTOT          MINUS ALLOCATION                             
         SP    DUB,DIFTOT          MINUS DIFFERENCES                            
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    DUB,DISCAMT         PLUS DISCOUNT                                
*&&UK*&& AP    DUB,VATAMT          PLUS VAT                                     
UPDAT30  CP    DUB,PZERO           TEST BATCH IS IN BALANCE                     
         BE    UPDAT40                                                          
         MVC   ACTION,ACTIONL      RESET ACTION TO PREVIOUS                     
         LA    R1,RECACTH          SET CURSOR & EXIT WITH ERROR IF NOT          
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EABATDIF)                                           
         B     FVERR                                                            
*                                                                               
UPDAT40  DS    0H                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   UPDAT70                                                          
*                                                                               
         L     R1,ARECVTAB                                                      
         USING RECVTABD,R1                                                      
         ZIC   R0,RECVCNT                                                       
UPDAT50  DS    0H                                                               
         CP    RECVTOT,PZERO       DOES TOTAL OFFSET ADD UP TO ZERO             
         BNE   UPDAT80                                                          
UPDAT60  LA    R1,RECVTABL(R1)                                                  
         BCT   R0,UPDAT50                                                       
         DROP  R1                                                               
*                                                                               
UPDAT70  CP    OFSTOT,PZERO        TEST OFFSET TOTAL ZERO                       
*&&UK*&& BNE   *+14                                                             
*&&UK*&& CP    OFSTO2,PZERO        TEST CURRENCY OFFSET TOTAL ZERO              
         BE    UPDAT90                                                          
UPDAT80  MVC   ACTION,ACTIONL      RESET ACTION TO PREVIOUS                     
         LA    R1,RECACTH          SET CURSOR & EXIT WITH ERROR IF NOT          
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAOFSTNZ)                                           
         B     FVERR                                                            
*                                                                               
UPDAT90  TM    TWAMODE,TWAMDOIT    TEST CONFIRMATION RECEIVED                   
         BNZ   UPDAT100                                                         
         CLI   PROFCFRM,PROFBOTH   TEST UPDATE & QUIT CONFIRMATION              
         BE    *+12                                                             
         CLI   PROFCFRM,PROFUPDT   TEST UPDATE CONFIRMATION                     
         BNE   UPDAT100                                                         
         GOTO1 ACONFRM             CONFIRM UPDATE ACTION                        
         BE    FVERRX              CONFIRMED                                    
         BH    FVERRX8             SEEK CONFIRMATION                            
*                                                                               
UPDAT100 GOTO1 AUPDTRN             UPDATE/DRAFT/FILTER                          
         BL    FVERR               DRAFT/FILTERED JOURNAL - CONTINUE            
         BE    NXTHED1             OK - RETURN TO HEADER SCREEN                 
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* MAINTAIN BILLING SOURCE RECORD FOR THIS COMPANY                     *         
***********************************************************************         
         SPACE 1                                                                
BILSRC   GOTO1 AVALBSC                                                          
         BL    EXIT                FINISHED PROCESSING SOURCE SCREEN            
         BH    FVERR               ERROR PROCESSING SOURCE SCREEN               
         B     FVERR               STILL PROCESSING SOURCE SCREEN               
         EJECT                                                                  
***********************************************************************         
* BATCH UPDATED SUCCESSFULLY. RETURN HEADER SCREEN TO USER WITH       *         
* UNPROTECTED INPUT FIELDS RESTORED.                                  *         
***********************************************************************         
         SPACE 1                                                                
NXTHED   TM    TWAMODE,TWAMDOIT    TEST CONFIRMATION RECEIVED                   
         BNZ   NXTHED1                                                          
         CLI   PROFCFRM,PROFBOTH   TEST UPDATE & QUIT CONFIRMATION              
         BE    *+12                                                             
         CLI   PROFCFRM,PROFQUIT   TEST QUIT CONFIRMATION                       
         BNE   NXTHED1                                                          
         GOTO1 ACONFRM             CONFIRM QUIT ACTION                          
         BE    FVERRX              CONFIRMED                                    
         BH    FVERRX8             SEEK CONFIRMATION                            
*                                                                               
NXTHED1  LA    RF,SAVHEADL                                                      
         ICM   RF,12,=C'L='                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(1,0),ATIA,,(RF)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,RECOLAYH                                                      
         LA    R1,SAVHEADL         RECOLAY->TWAD+3072 SAVED                     
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE TIA SCREEN INTO TWA                     
         GOTO1 AXMTSCR                                                          
*                                                                               
         MVC   TWASCROV,TWASCRBH                                                
         GOTOR VPROTOFF                                                         
         L     RF,AUTL             RESTORE UTL MAP FOR HEADER SWAP              
         MVC   TSCRNE-UTLD(L'TWAUTLSV,RF),TWAUTLSV                              
         GOTOR VPROTON                                                          
         MVC   ACTIONL,ACTION                                                   
         MVI   ACTION,ACTLAST                                                   
         GOTO1 OVERLAY,WORKD       GIVE LAST TIME CALL TO APPLICATION           
         GOTO1 AHDRSEC             SECURE HEADER SCREEN                         
         MVI   ACTION,ACTHEAD      BATCH HEADER NEXT TIME THROUGH               
*                                                                               
         LA    R1,RECOLAYH         LOCATE UNPROT FOR CURSOR POSITION            
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
NXTHED2  ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    NXTHED4                                                          
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+8                                                              
         BXLE  R1,RE,NXTHED2                                                    
         ST    R1,FVADDR                                                        
*                                                                               
NXTHED4  XC    RECACT,RECACT       SET ACTION TO START                          
         MVC   RECACT(L'AC8HEADR),AC8HEADR                                      
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IABATUPD)                                           
         OC    PRTSUB,PRTSUB       TEST REPORT PRINTED                          
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IABATREP)                                           
         CLI   ACTIONL,ACTQUIT     TEST HERE BECAUSE OF QUIT                    
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(IAENTBAT)                                           
         NI    TWAMODE,TWAMRSRV    RE-INITIALISE NEXT TIME THORUGH              
*                                                                               
         LA    R2,KEY                                                           
         USING BATRECD,R2                                                       
         MVC   BATKEY,SPACES                                                    
         XC    BATKEY(BATKEND),BATKEY                                           
         MVI   BATKTYP,BATKTYPQ                                                 
         MVC   BATKCPY,COMPANY                                                  
         MVC   BATKOFF,TWAUSRID                                                 
         MVI   BATKGRUP,TBAGGENQ   GENERAL ACCOUNTING                           
         MVI   BATKTYPE,30         BT30                                         
         MVC   BATKDATE,TODAYP     DATE                                         
         MVC   BATKREF,BATMON                                                   
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCFIL,KEY,AIO1                     
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         L     R2,AIO1                                                          
         CLI   ACTIONL,ACTQUIT     TEST HERE BECAUSE OF QUIT                    
         BNE   NXTHED6                                                          
         TM    BATRECD+(ACSTATUS-ACKEYD),BATSUPD TEST ALREADY UPDATED           
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   BATRECD+(ACSTATUS-ACKEYD),BATSDELT+BATSRECV                      
         B     NXTHED8                                                          
*                                                                               
NXTHED6  LA    RF,BATRECD+(ACRECORD-ACKEYD)                                     
         USING BTHELD,RF                                                        
         CLI   BTHEL,BTHELQ        ENSURE RECORD OK                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BATRECD+(ACSTATUS-ACKEYD),BATSUPD+BATSRECV                       
         ZAP   BTHCASH,CHQAMT                                                   
         LA    RE,UPDBLOCK                                                      
         LH    RE,TRNBSEQN-TRNBLKD(RE)                                          
         CVD   RE,DUB                                                           
         ZAP   BTHITEM,DUB                                                      
         DROP  RF                                                               
*                                                                               
NXTHED8  GOTO1 VDATAMGR,DMCB,(0,DMWRT),ACCFIL,BATRECD,BATRECD                   
         DROP  R2                                                               
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
NXTHEDX  B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT SCREEN AND UPDATE TSAR RECORDS FOR LINES THAT HAVE   *         
* BEEN INPUT THIS TIME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISVAL   TM    TWAMODE2,TWA2MEMO   TEST MEMO SCREEN PRESENT                     
         BZ    DISVL10                                                          
         GOTO1 APROMEM                                                          
         BL    DISVL20             FINISHED WITH MEMO SCREEN                    
         BH    FVERR               ERROR PROCESSING MEMO SCREEN                 
         B     FVERR               STILL PROCESSING MEMO SCREEN                 
*                                                                               
DISVL10  CLI   TWASCROV,TWASCRIN   TEST INPUT SCREEN PRESENT                    
         BNE   DISBLD                                                           
         MVI   ANYMARK,0           NOTHING MARKED THIS TIME                     
*                                                                               
DISVL20  LA    R3,RECLINH                                                       
         USING DISLINED,R3         R3=A(FIRST TWA LINE)                         
         SR    R5,R5                                                            
         ICM   R5,3,DISLIN         R5=NUMBER OF LINES IN TWA                    
         BZ    DISVALX                                                          
         LA    R2,DISREC           R2=A(RECORD NUMBERS)                         
*                                                                               
DISVL30  TM    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   DISVL420                                                         
         GOTO1 ATSARGET,(R2)       GET THE RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFVAL,DISLHDR2                                                   
         GOTO1 ATSTSEC             TEST LINE ACTION SECURITY                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     FVERR               INVALID ACTION                               
         CLC   FVIFLD(1),AC@RPLC   TEST REPLICATE                               
         BE    DISREP                                                           
         CLC   FVIFLD(1),AC@XFR    TEST TRANSFER                                
         BE    DISXFR                                                           
         CLC   FVIFLD(1),AC@QUERD  TEST QUERIED                                 
         BE    DISQRD                                                           
         CLC   FVIFLD(1),AC@HELD   TEST HELD                                    
         BE    DISHLD                                                           
*&&UK                                                                           
         CLC   FVIFLD(1),AC2CLRNG  TEST CLEARING                                
         BE    DISCLR                                                           
*&&                                                                             
         CLC   FVIFLD(1),AC3DUEDT  TEST DUE DATE                                
         BE    DISDUE                                                           
         CLC   FVIFLD(1),AC@MEMO   TEST MEMO                                    
         BNE   DISVL40                                                          
         MVC   DISLMARK,AC@NO      CLEAR MEMO                                   
         GOTO1 APROMEM                                                          
         BL    DISVL20             FINISHED WITH MEMO SCREEN                    
         BH    FVERR               ERROR PROCESSING MEMO SCREEN                 
         B     FVERR               STILL PROCESSING MEMO SCREEN                 
DISVL40  CLC   FVIFLD(1),AC@NO     TEST 'NO' INPUT                              
         BE    DISVL410                                                         
*                                                                               
         CLC   FVIFLD(1),AC@OFFST  TEST OFFSET                                  
         BNE   DISVL70                                                          
*&&US                                                                           
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BO    DISVL50                                                          
         MVC   FVMSGNO,=AL2(EAOFSMLT)                                           
         CLI   RECVCNT,1           CAN'T OFFSET IF MULTIPLE RECEIVABLES         
         BNE   FVERR                                                            
*&&                                                                             
DISVL50  MVC   FVMSGNO,=AL2(EAOFSINV)                                           
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   FVERR                                                            
         TM    TSARINDS,TSARIOFS   TEST RECORD IS AN OFFSET                     
         BNZ   DISVL60                                                          
         CP    TSARPOST,PZERO      NO - MUST NOT HAVE ALLOCATED                 
         BNE   FVERR                                                            
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISVL60                                                          
         CP    TSARPO2,PZERO       ENSURE NO ALLOCATION (CURRENCY)              
         BNE   FVERR                                                            
*&&                                                                             
DISVL60  OI    TSARINDS,TSARIOFS   SET OFFSET                                   
         MVI   FVIFLD,C' '                                                      
         TM    PROFTOT,PROFTOFS    TEST OFFSET TOTAL WANTED                     
         BZ    *+8                                                              
         OI    TOTPROF,PROFTOFS    YES - SET TOTAL ON                           
         B     DISVL250                                                         
*                                                                               
DISVL70  CLC   FVIFLD(1),AC@WRTF   TEST WRITE-OFF                               
         BNE   DISVL100                                                         
         MVC   FVMSGNO,=AL2(EAWOFINV)                                           
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   FVERR                                                            
         TM    TSARINDS,TSARIWOF   TEST RECORD IS A WRITE-OFF                   
         BNZ   DISVL80                                                          
         CP    TSARPOST,PZERO      NO - MUST NOT HAVE ALLOCATED                 
         BNE   FVERR                                                            
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISVL80                                                          
         CP    TSARPO2,PZERO       ENSURE NO ALLOCATION (CURRENCY)              
         BNE   FVERR                                                            
*&&                                                                             
DISVL80  MVC   FVMSGNO,=AL2(EAWOFMIS)                                           
         OC    WOFF,WOFF           CAN'T WRITE-OFF IF NO W/OFF ACCOUNT          
         BZ    FVERR                                                            
*&&US                                                                           
         CLI   AGYCTRY,CTRYCAN     IF CANADIAN BILL                             
         BNE   DISVL90                                                          
         CP    TSARPST,PZERO       CARRIES PST                                  
         BE    DISVL90                                                          
         OC    PST,PST             AND NO PST ACCOUNT IS SPECIFED               
         BNZ   DISVL90                                                          
         MVC   FVMSGNO,=AL2(EAPSTMIS) GIVE AN APPROPRIATE ERROR                 
         B     FVERR                                                            
*&&                                                                             
DISVL90  MVI   FVIFLD,C' '                                                      
         OI    TSARINDS,TSARIWOF   SET WRITE-OFF                                
         B     DISVL250                                                         
*                                                                               
DISVL100 CLC   FVIFLD(1),AC@UALCX  IS USER UNALLOCATING?                        
         BNE   DISVL210                                                         
         TM    TSARIND2,TSAR2QRD+TSAR2HLD+TSAR2CLR                              
         BZ    DISVL110                                                         
         NI    TSARIND2,FF-(TSAR2QRD+TSAR2HLD+TSAR2CLR)                         
         MVI   DISLMARK,C' '                                                    
         CLI   PROFAUTM,C'Y'       TEST AUTOMATIC MEMO                          
         BNE   DISVL400                                                         
         OC    TSARXSEQ,TSARXSEQ   TEST EXTENSION RECORD                        
         BNZ   *+12                                                             
         TM    TSARIND2,TSAR2MEM   TEST MEMORANDUM HELD ON FILE                 
         BZ    DISVL400                                                         
         L     R1,ATSARBLK                                                      
         MVI   TSACTN-TSARD(R1),TSAPUT                                          
         GOTO1 VTSAR               PUT THE RECORD TO TSAR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    ANYMARK,1           INDICATE SOMETHING MARKED THIS TIME          
         MVI   ANYUPDAT,1          SET UPDATE FLAG                              
         GOTO1 APROMEM                                                          
         BL    DISVL20             FINISHED WITH MEMO SCREEN                    
         BH    FVERR               ERROR PROCESSING MEMO SCREEN                 
         B     FVERR               STILL PROCESSING MEMO SCREEN                 
*                                                                               
DISVL110 MVC   FVMSGNO,=AL2(EANOTALC)                                           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARPOST,PZERO      TEST ANY ALLOCATION                          
         BE    FVERR                                                            
         MVI   DISLMARK,C' '                                                    
*                                                                               
         TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BZ    DISVL120                                                         
         NI    TSARINDS,255-TSARIWOF                                            
         SP    WOFFAMT,TSARPOST    REDUCE WRITE-OFF AMOUNT                      
*&&UK*&& SP    WOFFAM2,TSARPO2                                                  
         B     DISVL180                                                         
*                                                                               
DISVL120 TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BZ    DISVL150                                                         
         NI    TSARINDS,255-TSARIOFS-TSARIGRS                                   
         SP    OFSTOT,TSARPOST     REDUCE OFFSET AMOUNT                         
*&&UK*&& SP    OFSTO2,TSARPO2                                                   
         ZAP   TSARPOST,PZERO      CLEAR OFFSET AMOUNT                          
*&&UK*&& ZAP   TSARPO2,PZERO                                                    
         L     R1,ARECVTAB                                                      
         USING RECVTABD,R1                                                      
         LHI   R0,RECVMAXN                                                      
DISVL130 CLC   RECVNDX,RECVCNDX                                                 
         BNE   DISVL140                                                         
         CLC   RECVACT,RECVCACT    MAKE SURE SAME ACCOUNT AS IN TABLE           
         BNE   DISVL140                                                         
         SP    RECVTOT,TSARPOST    SAVE OFF OFFSET TOTAL PER ACCOUNT            
         B     DISVL400                                                         
DISVL140 LA    R1,RECVTABL(R1)                                                  
         BCT   R0,DISVL130                                                      
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
DISVL150 TM    TSARINDS,TSARITRF   TEST TRANSFER                                
         BZ    DISVL160                                                         
         NI    TSARINDS,255-TSARITRF                                            
         SP    XFRTOT,TSARPOST                                                  
*&&UK*&& SP    XFRTO2,TSARPO2                                                   
         ZAP   TSARPOST,PZERO                                                   
*&&UK*&& ZAP   TSARPO2,PZERO                                                    
         XC    TSARTRFA,TSARTRFA                                                
         XC    TSARTRFO,TSARTRFO                                                
         B     DISVL400                                                         
*                                                                               
DISVL160 CLI   TSARTYPE,TSARTOVR   TEST SPECIAL                                 
         BNE   DISVL170                                                         
         SP    DIFTOT,TSARPOST                                                  
*&&UK*&& SP    DIFTO2,TSARPO2                                                   
         B     DISVL180                                                         
DISVL170 SP    MRKTOT,TSARPOST     REDUCE TOTAL THIS TIME ALLOCATION            
*&&UK*&& SP    MRKTO2,TSARPO2                                                   
*                                                                               
DISVL180 ZAP   TSARPOST,PZERO      CLEAR THIS TIME ALLOCATION                   
*&&UK*&& ZAP   TSARPO2,PZERO                                                    
         TM    TSARINDS,TSARIGRS   TEST ALLOCATED GROSS                         
         BZ    DISVL190                                                         
         OC    DISC,DISC           TEST DISCOUNT A/C                            
         BZ    DISVL400            NO - GROSS IS AUTOMATIC IN TSARADD           
         NI    TSARINDS,255-TSARIGRS                                            
         B     DISVL400                                                         
*                                                                               
DISVL190 DS    0H                                                               
*&&US                                                                           
         TM    TSARINDS,TSARILAT   TEST ALLOCATED LATE                          
         BZ    *+8                                                              
         NI    TSARINDS,255-TSARILAT                                            
*&&                                                                             
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT KNOWN                  
         BZ    DISVL200                                                         
         SP    DISCAMT,TSARDISC    YES - REDUCE DISCOUNT (-VE AMOUNT)           
*&&UK*&& SP    DISCAM2,TSARDI2                                                  
DISVL200 B     DISVL400                                                         
*                                                                               
DISVL210 CLI   OPTSWTC,INCLUDE     TEST SWITCH OPTION ON                        
         BE    DISVL240                                                         
         CLI   FVIFLD,C'-'         PRESERVE NEGATIVE                            
         BNE   DISVL220                                                         
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   ERRNOTV                                                          
         TM    TWAMODE2,TWA2NALL   TEST NOT ALLOWING ALLOCATION                 
         BZ    DISVL250                                                         
         B     ERRINVAL            CANNOT ALLOCATE THIS ITEM                    
*                                                                               
DISVL220 CLI   FVIFLD,X'80'        ELSE <X'80' MEANS NO CHANGE                  
         BNH   DISVL410            SKIP TO NEXT                                 
         TM    TWAMODE2,TWA2NALL   TEST NOT ALLOWING ALLOCATION                 
         BNZ   ERRINVAL            CANNOT ALLOCATE THIS ITEM                    
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   ERRNOTV                                                          
*&&US                                                                           
         CLC   FVIFLD(1),AC@LATE   TEST ALLOCATING LATE                         
         BNE   DISVL230                                                         
         OC    DISC,DISC           TEST MAKING DISCOUNT POSTINGS                
         BZ    ERRNLDAC            NO DISCOUNT ACCOUNT PRESENT                  
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARDI2,PZERO       TEST ANY DISCOUNT (CURRENCY)                 
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARDISC,PZERO      TEST ANY DISCOUNT TO POST                    
         BE    ERRNLDSC            NO DISCOUNT AMOUNT TO TAKE                   
         CP    TSARPOST,PZERO      TEST PRIOR ALLOCATION                        
         BNE   ERRNOTV                                                          
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       ENSURE NO ALLOCATION (CURRENCY)              
         BNE   ERRNOTV                                                          
*&&                                                                             
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   ERRNOTV                                                          
         OI    TSARINDS,TSARILAT   SET LATE DISCOUNT                            
         MVI   FVIFLD,C' '         CLEAR THIS CHARACTER                         
         B     DISVL250            CONTINUE WITH ALLOCATION VALIDATION          
*&&                                                                             
DISVL230 CLC   FVIFLD(1),AC@YES                                                 
         BNE   DISVL250                                                         
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   ERRNOTV                                                          
         MVI   FVIFLD,C' '                                                      
         B     DISVL250                                                         
*                                                                               
DISVL240 TM    TSARINDS,TSARIWOF+TSARIOFS+TSARITRF                              
         BNZ   DISVL410                                                         
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   DISVL410                                                         
*&&US                                                                           
         TM    TSARINDS,TSARILAT                                                
         BNZ   DISVL410                                                         
*&&                                                                             
         TM    TWAMODE2,TWA2NALL   TEST NOT ALLOWING ALLOCATION                 
         BNZ   DISVL410                                                         
         CLI   TSARTYPE,TSARTOVR   TEST SPECIAL                                 
         BNE   *+12                                                             
         CLI   FVIFLD,C' '         TEST FIRST CHARACTER CHANGED                 
         BNH   DISVL410                                                         
         CLI   FVIFLD,DISLMPPD     TEST PART PAID                               
         BNE   *+8                                                              
         MVI   FVIFLD,C' '         CLEAR PART PAID INDICATOR                    
*                                                                               
DISVL250 TM    TSARINDS,TSARITRF   TRANSFERS CAN'T BE ALLOCATED                 
         BNZ   ERRNALTR                                                         
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         CLI   TSARTYPE,TSARTOVR   TEST SPECIAL ITEM (ALWAYS GROSS)             
         BE    DISVL270                                                         
         CP    TSARPOST,PZERO      TEST PREVIOUSLY ALLOCATED                    
         BNE   DISVL270            YES - CAN'T ALLOCATE GROSS                   
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       ENSURE NO ALLOCATION (CURRENCY)              
         BNE   DISVL270                                                         
*&&                                                                             
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    DISVL270            NO - CAN'T ALLOCATE GROSS                    
         LA    RE,FVIFLD                                                        
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLC   0(1,RE),AC@GROSS    TEST GROSS ALLOCATION                        
         BNE   DISVL260                                                         
*&&US                                                                           
         TM    TSARINDS,TSARILAT   TEST TAKING DISCOUNT LATE                    
         BNZ   ERRLATDE            CAN'T BE LATE AND DEDUCTED                   
*&&                                                                             
         OI    TSARINDS,TSARIGRS   SET GROSS ALLOCATION                         
         MVC   0(L'DISLPOST,RE),1(RE)                                           
         BCTR  R0,0                                                             
         STC   R0,FVILEN                                                        
         CLI   0(RE),C' '          TEST ALLOCATE FULL GROSS AMOUNT              
         BH    DISVL270                                                         
         ZAP   DUB,TSARDR          YES - CALCULATE GROSS AMOUNT                 
         SP    DUB,TSARCR                                                       
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISVL320                                                         
         ZAP   DUB,TSARDR2         YES - USE CURRENCY AMOUNTS                   
         SP    DUB,TSARCR2                                                      
*&&                                                                             
         B     DISVL320                                                         
*                                                                               
DISVL260 TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BZ    DISVL270                                                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARDI2,PZERO       TEST ANY DISCOUNT (CURRENCY)                 
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARDISC,PZERO      TEST ANY DISCOUNT PRESENT                    
         BE    DISVL270                                                         
         MVC   FVMSGNO,=AL2(EAOFSGRS)                                           
         B     FVERR               MUST OFFSET/WRITE-OFF GROSS AMOUNT           
*                                                                               
DISVL270 SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
*&&UK                                                                           
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
*&&                                                                             
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   ERRIAMNT            INVALID AMOUNT                               
         ZAP   DUB,4(8,R1)                                                      
         OC    DUB(2),DUB          MUST FIT INTO PL6                            
         BNZ   ERRIAMNT            INVALID AMOUNT (TOO LARGE)                   
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISVL290                                                         
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         MVC   AFCX,ATLX           ALLOCATION RATE                              
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         EXCHP DUB,TEMP,DUB=DUB3   DUB3=ALLOCATION IN AGENCY CURRENY            
         ZAP   PKWK16(L'DUB),DUB3  SAVE THIS VALUE                              
         TM    TSARINDS,TSARIWOF+TSARITRF+TSARIOFS                              
         BNZ   DISVL280            NO DIFFERENCE TO BE POSTED                   
         LA    RF,TEMP+L'AFCX                                                   
         MVC   AFCX,TSARAFCX       TRANSACTION RATE                             
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP DUB,TEMP+L'AFCX,DUB=DUB2                                         
         SP    PKWK16(L'DUB),DUB2  PKWK16(L'DUB)=DIFFERENCE AMOUNT              
         SP    DUB3,PKWK16(L'DUB)  DUB3=TOTAL POSTING FOR ALLOCATION            
*                                                                               
DISVL280 ZAP   DUB2,TSARDR         CALCULATE BALANCE                            
         TM    TSARINDS,TSARITRF                                                
         BZ    *+14                                                             
         AP    DUB2,TSARCR                                                      
         B     *+10                                                             
         SP    DUB2,TSARCR                                                      
         TM    TSARINDS,TSARIGRS   TEST GROSS ALLOCATION                        
         BNZ   *+10                                                             
         AP    DUB2,TSARDISC                                                    
         CP    DUB2,DUB3           TEST ITEM CLOSED IN AGENCY CURRENCY          
         BNE   DISVL290                                                         
         ZAP   DUB2,TSARDR2        CALCULATE BALANCE IN CURRENCY                
         TM    TSARINDS,TSARITRF                                                
         BZ    *+14                                                             
         AP    DUB2,TSARCR2                                                     
         B     *+10                                                             
         SP    DUB2,TSARCR2                                                     
         TM    TSARINDS,TSARIGRS   TEST GROSS ALLOCATION                        
         BNZ   *+10                                                             
         AP    DUB2,TSARDI2                                                     
         CP    DUB2,DUB            TEST ITEM CLOSED IN CURRENCY                 
         BNE   ERRCUBAL            NO - CANNOT LEAVE CURRENCY BALANCE           
*&&                                                                             
DISVL290 CLI   TSARTYPE,TSARTOVR   TEST SPECIAL ITEM                            
         BNE   DISVL320                                                         
         CP    DUB,PZERO           TEST NON-ZERO AMOUNT INPUT                   
         BE    DISVL410                                                         
         SP    DIFTOT,TSARPOST     SUBTRACT THIS ALLOCATION                     
*&&UK*&& SP    DIFTO2,TSARPO2                                                   
         ZAP   TSARPOST,DUB        SET MARKED AMOUNT                            
         MVC   TEMP(L'AFCX),ATLX                                                
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISVL300                                                         
         ZAP   TSARPO2,DUB                                                      
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP DUB,TEMP            CALCULATE USING EXCHANGE RATE                
         ZAP   TSARPOST,DUB                                                     
*&&                                                                             
DISVL300 AP    DIFTOT,TSARPOST     ADD TO MARKED TOTAL                          
*&&UK                                                                           
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BZ    DISVL310                                                         
         EXCHP DUB,TEMP            CALCULATE USING EXCHANGE RATE                
         ZAP   TSARPO2,DUB                                                      
DISVL310 AP    DIFTO2,TSARPO2                                                   
*&&                                                                             
         B     DISVL400                                                         
*                                                                               
DISVL320 TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BZ    DISVL325                                                         
         CP    DUB,PZERO           MUST HAVE ALLOCATED                          
         BNE   DISVL325                                                         
         MVC   FVMSGNO,=AL2(AE$PZERO)                                           
         B     FVERR                                                            
DISVL325 CP    DUB,PZERO           TEST NON-ZERO AMOUNT INPUT                   
         BE    DISVL410                                                         
*&&UK                                                                           
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         MVC   AFCX,ATLX                                                        
         TM    TSARINDS,TSARIWOF+TSARIOFS                                       
         BZ    *+10                                                             
         MVC   AFCX,TSARAFCX       ALLOCATE USING ORIGINAL RATE                 
         ZAP   DUB2,PZERO          CLEAR CURRENCY AMOUNT                        
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISVL330                                                         
         AP    TSARPO2,DUB                                                      
         ZAP   DUB2,DUB            SAVE CURRENCY AMOUNT                         
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP DUB,TEMP            CALCULATE USING EXCHANGE RATE                
*&&                                                                             
DISVL330 AP    TSARPOST,DUB                                                     
*&&UK                                                                           
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BZ    DISVL340                                                         
         ZAP   DUB2,DUB                                                         
         EXCHP DUB2,TEMP,DUB=DUB2  CALCULATE USING EXCHANGE RATE                
         AP    TSARPO2,DUB2                                                     
*&&                                                                             
DISVL340 TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BZ    DISVL350                                                         
         AP    WOFFAMT,DUB         YES - ADD TO WRITE-OFF AMOUNT                
*&&UK*&& AP    WOFFAM2,DUB2                                                     
         B     DISVL390                                                         
*                                                                               
DISVL350 TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BZ    DISVL380                                                         
         AP    OFSTOT,DUB          YES - ADD TO OFFSET AMOUNT                   
*&&UK*&& AP    OFSTO2,DUB2                                                      
         L     R1,ARECVTAB                                                      
         USING RECVTABD,R1                                                      
         LHI   R0,RECVMAXN                                                      
DISVL360 CLC   RECVNDX,RECVCNDX                                                 
         BNE   DISVL370                                                         
         CLC   RECVACT,RECVCACT    MAKE SURE SAME ACCOUNT AS IN TABLE           
         BNE   DISVL370                                                         
         AP    RECVTOT,DUB         SAVE OFF OFFSET TOTAL PER ACCOUNT            
         B     DISVL400                                                         
DISVL370 LA    R1,RECVTABL(R1)                                                  
         BCT   R0,DISVL360                                                      
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
DISVL380 AP    MRKTOT,DUB          ELSE ADD TO MARKED TOTAL                     
*&&UK*&& AP    MRKTO2,DUB2                                                      
*                                                                               
DISVL390 OC    DISC,DISC           TEST DISCOUNT ACCOUNT KNOWN                  
         BZ    DISVL400                                                         
         TM    TSARINDS,TSARIGRS   TEST ALLOCATED GROSS                         
         BNZ   DISVL400                                                         
         AP    DISCAMT,TSARDISC    YES - ADD TO DISCOUNT TOTAL                  
*&&UK*&& AP    DISCAM2,TSARDI2                                                  
*                                                                               
DISVL400 L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR               PUT THE RECORD TO TSAR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         OI    ANYMARK,1           INDICATE SOMETHING MARKED THIS TIME          
         MVI   ANYUPDAT,1          SET UPDATE FLAG                              
*                                                                               
DISVL410 GOTO1 ABLDLIN,DISLINED    RE-DISPLAY THIS LINE                         
         CLI   OPTSWTC,0           TEST SWITCH OPTION SET                       
         BE    *+10                                                             
         MVC   DISLMARK,AC@NO      YES - MARK LINE AS 'SEEN' THIS TIME          
*                                                                               
DISVL420 LA    R3,DISLINEL(R3)     R3=A(NEXT TWA LINE)                          
         LA    R2,2(R2)                                                         
         BCT   R5,DISVL30                                                       
         CLI   ANYMARK,0           DID USER DO ANYTHING THIS TIME               
         BNE   DISOUTX             YES - EXIT WITH SCREEN DISPLAYED             
DISVALX  B     DISBLD                                                           
         EJECT                                                                  
***********************************************************************         
* REPLICATE AN ORIGINAL TRANSACTION (SPLITS DEBIT AMOUNT)             *         
***********************************************************************         
         SPACE 1                                                                
DISREP   CLI   TSARTYPE,TSARTOVR   SPECIAL PAYMENTS CAN'T BE REPLICATED         
         BE    ERRCRASP                                                         
         CP    TSARPOST,PZERO      ALLOCATED BILLS CAN'T BE EITHER              
         BNE   ERRCRAAL                                                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       ENSURE NO ALLOCATION (CURRENCY)              
         BNE   ERRCRAAL                                                         
*&&                                                                             
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   ERRNOTV                                                          
         MVI   FVIFLD,C' '         VALIDATE SPLIT AMOUNT                        
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
*&&UK                                                                           
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
*&&                                                                             
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   ERRIAMNT                                                         
         ZAP   DUB2,4(8,R1)                                                     
         OC    DUB2(2),DUB2        AMOUNT MUST FIT INTO PL6                     
         BNZ   ERRIAMNT                                                         
         CP    DUB2,PZERO          AND MUST NOT BE ZERO                         
         BE    ERRIAMNT                                                         
*&&UK                                                                           
         ZAP   DUB3,PZERO          SECOND AMOUNT                                
         TM    CURIND1,CUR1SINC+CUR1ALLC                                        
         BZ    DISREP08                                                         
         ZAP   DUB3,DUB2                                                        
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    DISREP02                                                         
         MVC   TEMP(L'AFCX),TSARAFCX                                            
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP DUB2,TEMP,DUB=DUB2  CALCULATE USING EXCHANGE RATE                
         B     DISREP08                                                         
*                                                                               
DISREP02 OC    TSARAFCX,TSARAFCX   TEST EXCHANGE RULES RESOLVED                 
         BNZ   DISREP06                                                         
         ZAP   DUB,TSARDR2         TAKE CURRENCY DEBITS                         
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT KNOWN                  
         BZ    *+10                                                             
         AP    DUB,TSARDI2         YES - SUBTRACT CURRENCY DISCOUNT             
         CP    DUB,TSARCR2         IF CURRENCY DRS(-DISCOUNT)=CRS               
         BE    DISREP08            CANNOT SET EXCHANGE RULES                    
         MVC   FORCURT,AGYCURT                                                  
         OC    TSARCUR,TSARCUR                                                  
         BZ    DISREP04                                                         
         GOTO1 VBLDCUR,DMCB,(0,TSARCUR),(0,FORCURT),ACOM                        
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
DISREP04 LA    RF,TSARAFCX         BUILD EXCHANGE RULES                         
         USING AFCX,RF                                                          
         XC    AFCX,AFCX           CLEAR AND BUILD EXCHANGE RULES               
         ZAP   PKWK16,TSARDR2      CALCULATE OPEN ITEM BALANCE                  
         ZAP   DUB,TSARDR          IN BOTH CURRENCIES                           
         OC    DISC,DISC                                                        
         BZ    *+16                                                             
         AP    PKWK16,TSARDI2                                                   
         AP    DUB,TSARDISC                                                     
         SP    PKWK16,TSARCR2                                                   
         SP    DUB,TSARCR                                                       
         LA    RE,11                                                            
         SR    R0,R0                                                            
         IC    R0,FORCURT+(CURTDECP-CURTABD)                                    
         SR    RE,R0                                                            
         SRP   PKWK16,0(RE),0                                                   
         LA    RE,5                                                             
         SR    R0,R0                                                            
         IC    R0,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,R0                                                            
         SRP   DUB,0(RE),0                                                      
         DP    PKWK16,DUB                                                       
         SRP   PKWK16(8),64-1,5                                                 
         LM    R0,R1,PKWK16        FIRST 8 BYTES IS QUOTIENT                    
         SRDL  R0,4                                                             
         STM   R0,R1,PKWK16        LOSE PACKED SIGN                             
         OC    PKWK16(3),PKWK16    ENSURE RATE WILL FIT                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   AFCXRATE,PKWK16+3                                                
         SR    RE,RE                                                            
         IC    RE,FORCURT+(CURTDECP-CURTABD)                                    
         SR    R0,R0                                                            
         IC    R0,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,R0                                                            
         STC   RE,AFCXSHFT         SET SHIFT VALUE                              
         DROP  RF                                                               
         MVC   TEMP(L'AFCX),TSARAFCX                                            
DISREP06 EXCHP DUB3,TEMP,DUB=DUB3  CALCULATE USING EXCHANGE RATE                
*&&                                                                             
DISREP08 MVI   TSARINDS,0          BUILD REPLACEMENT RECORD                     
         MVI   TSARIND2,TSAR2REP   INDICATE REPLICANT                           
         ZAP   TSARDR,DUB2         SET SPLIT DEBIT AMOUNT                       
         ZAP   TSARCR,PZERO                                                     
         ZAP   TSARDISC,PZERO                                                   
         ZAP   TSARPOST,PZERO                                                   
*&&UK                                                                           
         ZAP   TSARDR2,DUB3                                                     
         ZAP   TSARCR2,PZERO                                                    
         ZAP   TSARDI2,PZERO                                                    
         ZAP   TSARPO2,PZERO                                                    
*&&                                                                             
*&&US                                                                           
         ZAP   TSARGSTB,PZERO                                                   
         ZAP   TSARGST,PZERO                                                    
         ZAP   TSARPSTB,PZERO                                                   
         ZAP   TSARPST,PZERO                                                    
         XC    TSARGSTT,TSARGSTT                                                
         XC    TSARPSTT,TSARPSTT                                                
         XC    TSARPSTC,TSARPSTC                                                
*&&                                                                             
         MVI   TSARTYPE,TSARTRPL-1                                              
*                                                                               
DISREP10 SR    R1,R1               INCREMENT RECORD TYPE                        
         IC    R1,TSARTYPE                                                      
         LA    R1,1(R1)                                                         
         GOTO1 ATSARADD,(R1)       CALL TSAR TO ADD RECORD                      
         BE    DISREP12                                                         
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         TM    TSERRS,TSEEOF       TEST E-O-F                                   
         BNZ   FVERR                                                            
         TM    TSERRS,TSEDUP       TEST DUPLICATE KEY                           
         BNZ   DISREP10            YES - TRY AGAIN                              
         DC    H'0'                FUNNY ERRORS                                 
*                                                                               
DISREP12 GOTO1 ATSARGET,(R2)       GET REPLICATED RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SP    TSARDR,DUB2         REDUCE DEBIT AMOUNT ON ORIGINAL              
*&&UK*&& SP    TSARDR2,DUB3                                                     
         OI    TSARINDS,TSARISMT   SET MULTI BIT ON TO STOP TRANSFER            
*&&UK                                                                           
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BZ    DISREP14                                                         
         OC    TSARAFCX,TSARAFCX   TEST EXCHANGE RULES KNOWN                    
         BNZ   DISREP14                                                         
         XC    FORCURT,FORCURT                                                  
*&&                                                                             
DISREP14 L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAPUT       AND PUT BACK ORIGINAL RECORD                 
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R2,0(R2)                                                         
         BCTR  R2,0                                                             
         XC    DISREC(DISRECL),DISREC                                           
         XC    DISLIN,DISLIN                                                    
         MVI   SCRUPDN,SCRDN                                                    
         MVI   DISIND,DISIRST                                                   
         LH    R0,SCRPAGEL                                                      
         B     DISBLD6                                                          
         EJECT                                                                  
***********************************************************************         
* TRANSFER A BILL TO ANOTHER RECEIVABLE ACCOUNT                       *         
***********************************************************************         
         SPACE 1                                                                
DISXFR   CP    TSARPOST,PZERO      TEST ANY ALLOCATION, ETC.                    
         BNE   ERRCTALC                                                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       ENSURE NO ALLOCATION (CURRENCY)              
         BNE   ERRCTALC                                                         
*&&                                                                             
         CLI   TSARTYPE,TSARTTRN   CAN'T BE SPECIAL OR REPLICANT                
         BNE   ERRNSTR                                                          
         TM    TSARINDS,TSARISMT   TEST MULTI TRANSACTION BIT ON                
         BNZ   ERRNSTR                                                          
         TM    TSARIND2,TSAR2HLD+TSAR2QRD+TSAR2CLR                              
         BNZ   ERRNOTV                                                          
         CLI   FVILEN,1            TEST ACCOUNT CODE INPUT                      
         BNH   ERRNONE                                                          
         CLC   RECVCACT+3(L'ACTKACT),FVIFLD+1  TEST SELF-TRANSFER               
         BE    ERRSELF                                                          
         LA    R1,KEY              BUILD KEY OF RECEIVABLE ACCOUNT              
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVUL),RECVUL                                         
         MVC   ACTKACT,FVIFLD+1                                                 
         GOTO1 AGETACC,0           READ THE ACCOUNT RECORD                      
         BNE   FVERR                                                            
*&&UK                                                                           
         CLC   RECVOFF,RECOFFC     TEST OFFICE MATCH                            
         BE    DISXFR02                                                         
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    DISXFR02                                                         
         MVC   FVMSGNO,=AL2(EAOFFMIX)                                           
         B     FVERR                                                            
*&&                                                                             
DISXFR02 MVC   TSARTRFA,FVIFLD+1   SET RECEIVABLE ACCOUNT KEY                   
         MVC   TSARTRFO,RECOFFC    SET RECEIVABLE ACCOUNT OFFICE CODE           
         TM    TSARINDS,TSARISDR   TEST DEBIT                                   
         BZ    DISXFR04                                                         
         ZAP   TSARPOST,TSARDR     SET POSTING AMOUNT                           
*&&UK                                                                           
         OC    TSARDR2,TSARDR2                                                  
         BZ    *+10                                                             
         ZAP   TSARPO2,TSARDR2     TRANSFER AT ORIGINAL RATE                    
*&&                                                                             
         B     DISXFR06                                                         
DISXFR04 ZAP   TSARPOST,TSARCR                                                  
*&&UK                                                                           
         OC    TSARCR2,TSARCR2                                                  
         BZ    *+10                                                             
         ZAP   TSARPO2,TSARCR2     TRANSFER AT ORIGINAL RATE                    
*&&                                                                             
DISXFR06 OI    TSARINDS,TSARITRF+TSARIGRS                                       
         AP    XFRTOT,TSARPOST     ADD TO TRANSFER TOTAL                        
*&&UK*&& AP    XFRTO2,TSARPO2                                                   
*&&US                                                                           
         TM    PROFTOT,PROFTXFR    TEST TRANSFER TOTAL WANTED                   
         BZ    *+8                                                              
         OI    TOTPROF,PROFTXFR    YES - SET TOTAL ON                           
*&&                                                                             
         B     DISVL400                                                         
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET QUERIED/HELD/CLEARING STATUS                                    *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
DISCLR   TM    TSARIND2,TSAR2CLR   TEST ALREADY CLEARING                        
         BO    DISVL410                                                         
         TM    TSARIND2,TSAR2HLD+TSAR2QRD                                       
         BNZ   ERRNOTV                                                          
         B     DISHLD02                                                         
*&&                                                                             
DISQRD   TM    TSARIND2,TSAR2QRD   TEST ALREADY QUERIED                         
         BO    DISVL410                                                         
         TM    TSARIND2,TSAR2HLD+TSAR2CLR                                       
         BNZ   ERRNOTV                                                          
         B     DISHLD02                                                         
DISHLD   TM    TSARIND2,TSAR2HLD   TEST ALREADY HELD                            
         BO    DISVL410                                                         
         TM    TSARIND2,TSAR2QRD+TSAR2CLR                                       
         BNZ   ERRNOTV                                                          
DISHLD02 CLC   FVIFLD(1),AC@QUERD  TEST/SET QUERIED                             
         BNE   *+8                                                              
         OI    TSARIND2,TSAR2QRD                                                
         CLC   FVIFLD(1),AC@HELD   TEST/SET HELD                                
         BNE   *+8                                                              
         OI    TSARIND2,TSAR2HLD                                                
*&&UK                                                                           
         CLC   FVIFLD(1),AC2CLRNG  TEST/SET CLEARING                            
         BNE   *+8                                                              
         OI    TSARIND2,TSAR2CLR   SET                                          
*&&                                                                             
         CLI   PROFAUTM,C'Y'       TEST AUTOMATIC MEMO                          
         BNE   DISVL400                                                         
         L     R1,ATSARBLK                                                      
         MVI   TSACTN-TSARD(R1),TSAPUT                                          
         GOTO1 VTSAR               PUT THE RECORD TO TSAR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    ANYMARK,1           INDICATE SOMETHING MARKED THIS TIME          
         MVI   ANYUPDAT,1          SET UPDATE FLAG                              
         GOTO1 APROMEM                                                          
         BL    DISVL20             FINISHED WITH MEMO SCREEN                    
         BH    FVERR               ERROR PROCESSING MEMO SCREEN                 
         B     FVERR               STILL PROCESSING MEMO SCREEN                 
         EJECT                                                                  
***********************************************************************         
* SET NEW DUE DATE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISDUE   OC    TWASAGN,TWASAGN     TEST NEW SECURITY IN USE                     
         BNZ   *+12                                                             
         TM    TWAAUTH,ACTIAUT1    TEST AUTHORISED TO AMEND DUE DATES           
         BZ    ERRSECLK                                                         
         TM    TSARINDS,TSARISDR   TEST DEBIT HELD                              
         BZ    ERRDDTDR                                                         
         CLI   FVIFLD+1,C' '       TEST RESTORE ORIGINAL DUE DATE               
         BNH   DISDUE04                                                         
         LA    R1,WORK                                                          
         USING CONBLKD,R1                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVC   CONCOMF,ACOM        A(COMFACS)                                   
         MVI   CONACTN,CONAVGTQ    VALIDATE AND SET DUE DATE                    
         MVI   CONFLD,CONFIDUE     INVOICE DUE DATE FOR DEBTORS                 
         MVC   CONIDATE,TSARDAT    INVOICE DATE                                 
         MVC   CONILEN,FVXLEN      L'INPUT                                      
         LA    R0,FVIFLD+1                                                      
         STCM  R0,15,CONIADD       A(INPUT)                                     
         LA    R0,TEMP                                                          
         STCM  R0,15,CONOADD       A(OUTPUT)                                    
         GOTO1 VCONVERT,(R1)                                                    
         BNE   ERRINVDT                                                         
         GOTO1 VDATCON,DMCB,(2,TEMP),(1,TEMP+10)                                
         CLC   TSARDAT,TEMP+10                                                  
         BH    ERRINVDT                                                         
         CLC   TSARDUED,TEMP                                                    
         BE    ERRINVDT                                                         
         MVC   TSARDUE2,TEMP                                                    
         B     DISVL400                                                         
         DROP  R1                                                               
*                                                                               
DISDUE04 OC    TSARDUE2,TSARDUE2   RESTORE ORIGINAL DUE DATE                    
         BZ    ERRINVDT                                                         
         XC    TSARDUE2,TSARDUE2                                                
         B     DISVL400                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD A TABLE OF RECORD NUMBERS TO BE DISPLAYED                     *         
***********************************************************************         
         SPACE 1                                                                
DISBLD   CLI   TWASCROV,TWASCRIN   TEST INPUT TO BE VALIDATED                   
         BE    DISBLD2                                                          
         GOTO1 AOVRSCR,TWASCRIN    OVERLAY INPUT SCREEN                         
         MVI   DISIND,DISIRST      SET TO START FROM BEGINNING                  
*                                                                               
DISBLD2  TM    DISIND,DISIRST      TEST RESTART FROM BEGINNING                  
         BZ    DISBLD3                                                          
         MVC   SCROLL,SCRMAXIL                                                  
         MVI   SCRUPDN,SCRUP                                                    
*                                                                               
DISBLD3  NI    DISIND,DISIRST      RESET BOF/EOF INDICATORS                     
         CLC   SCROLL,SCRLEFTL     TEST LEFT SCROLL                             
         BE    DISBLDX                                                          
         CLC   SCROLL,SCRRGHTL     TEST RIGHT SCROLL                            
         BE    DISBLDX                                                          
         CLC   SCROLL,SCRMAXIL     TEST MAXIMUM SCROLL                          
         BNE   DISBLD4                                                          
         XC    TEMP(DISRECL+L'DISLIN),TEMP                                      
         XC    DISREC(DISRECL),DISREC                                           
         XC    DISLIN,DISLIN                                                    
         LH    R0,SCRPAGEL                                                      
         STCM  R0,3,SCROLL         SET SCROLL TO A PAGE                         
         LH    R2,DISMAX                                                        
         AH    R2,=H'1'                                                         
         CLI   SCRUPDN,SCRDN       TEST MAXIMUM SCROLL DOWN                     
         BNE   *+12                                                             
         MVI   SCRUPDN,SCRUP       YES - SET SCROLL UP                          
         B     DISBLD6                                                          
         SR    R2,R2               SET MINIMUM RECORD NUMBER-1                  
         MVI   SCRUPDN,SCRDN       SET SCROLL DOWN                              
         OI    DISIND,DISIRST      INDICATE FIRST PAGE                          
         B     DISBLD6                                                          
*                                                                               
DISBLD4  OC    DISLIN,DISLIN       CLEAR DISREC IF NO PREVIOUS RECORD           
         BNZ   *+10                                                             
         XC    DISREC(DISRECL),DISREC                                           
         MVC   TEMP(DISRECL),DISREC                                             
         MVC   TEMP+DISRECL(L'DISLIN),DISLIN                                    
         SR    R0,R0                                                            
         ICM   R0,3,SCROLL         R0=SCROLL AMOUNT (NN LINES)                  
         LH    R2,DISREC           R2=FIRST DISPLAYED RECORD                    
         CLI   SCRUPDN,SCRUP                                                    
         BE    DISBLD6                                                          
         LH    R2,DISLIN           SET R2 TO LAST DISPLAYED RECORD              
         LTR   R2,R2                                                            
         BZ    *+12                                                             
         SLL   R2,1                                                             
         LH    R2,DISREC-L'DISREC(R2)                                           
*                                                                               
DISBLD6  CLI   SCRUPDN,SCRUP       TEST SCROLL UP (BACKWARDS)                   
         BNE   DISBLD8                                                          
         SH    R2,=H'1'            DECREMENT RECORD COUNT                       
         BP    DISBLD10            GET RECORD IF NOT AT BOF                     
         OI    DISIND,DISIBOF      SET BOF REACHED                              
         B     DISBLD16                                                         
*                                                                               
DISBLD8  AH    R2,=H'1'            INCREMENT RECORD COUNTER                     
         CH    R2,DISMAX           GET RECORD IF NOT AT EOF                     
         BNH   DISBLD10                                                         
         OI    DISIND,DISIEOF      SET EOF REACHED                              
         B     DISBLD16                                                         
*                                                                               
DISBLD10 STH   R2,DISNUM           SET RECORD NUMBER AND GET RECORD             
         GOTO1 ATSARGET,DISNUM                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER             APPLY LOCATE & FILTER VALUES                 
         BNE   DISBLD6             GET NEXT IF FILTERED OUT                     
*                                                                               
         LH    RE,DISLIN           RE=NUMBER OF ENTRIES IN DISREC               
         CLI   SCRUPDN,SCRDN       TEST SCROLL UP OR DOWN                       
         BNE   DISBLD12                                                         
*                                                                               
         LR    RF,RE                                                            
         CH    RF,=Y(LINES)        SHIFT OFF ENTRY IF TABLE FULL                
         BL    *+12                                                             
         MVC   DISREC(DISRECL-L'DISREC),DISREC+L'DISREC                         
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         LA    RF,DISREC(RF)       RF=A(NEW LAST ENTRY IN TABLE)                
         B     DISBLD14                                                         
*                                                                               
DISBLD12 MVC   WORK(DISRECL-L'DISREC),DISREC                                    
         MVC   DISREC+L'DISREC(DISRECL-L'DISREC),WORK                           
         LA    RF,DISREC           RF=A(NEW FIRST ENTRY IN TABLE)               
*                                                                               
DISBLD14 MVC   0(L'DISREC,RF),DISNUM                                            
         AH    RE,=H'1'                                                         
         CH    RE,=Y(LINES)        TEST DISREC FULL                             
         BH    *+8                                                              
         STH   RE,DISLIN           NO - SET NEW NUMBER OF ENTRIES               
         BCT   R0,DISBLD6                                                       
*                                                                               
DISBLD16 LTR   RE,R0               TEST ANY SCOLLING LEFT TO DO                 
         BZ    DISBLD18                                                         
         CLI   SCRUPDN,SCRDN       TEST SCROLLING DOWN (NOT FIRST)              
         BNE   DISBLD18                                                         
         TM    DISIND,DISIRST      TEST SCROLL FROM BEGINNING                   
         BNZ   DISBLD18                                                         
         LH    RF,DISLIN                                                        
         SR    RF,RE               RF=NUMBER OF ENTRIES REMAINING               
         BNP   DISBLD18                                                         
         STH   RF,DISLIN                                                        
         SLL   RE,1                                                             
         LA    RE,DISREC(RE)       RE=A(SHIFT FROM POSITION)                    
         SLL   RF,1                                                             
         BCTR  RF,0                                                             
         XC    WORK(DISRECL),WORK                                               
         MVC   WORK(0),0(RE)       SAVE REMAINDER & SET AS FIRST                
         EX    RF,*-6                                                           
         XC    DISREC(DISRECL),DISREC                                           
         MVC   DISREC(DISRECL),WORK                                             
*                                                                               
DISBLD18 NI    DISIND,255-DISIRST                                               
         OC    DISREC(DISRECL),DISREC                                           
         BNZ   DISBLDX                                                          
         MVC   DISREC(DISRECL),TEMP                                             
         MVC   DISLIN,TEMP+DISRECL                                              
DISBLDX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD INPUT SCREEN FROM TABLE OF RECORD NUMBERS (DISTAB)            *         
* MARKED ITEMS ARE HIGHLIGHTED - UNMARKED ARE NORMAL                  *         
***********************************************************************         
         SPACE 1                                                                
DISOUT   GOTO1 ABLDDIS             BUILD DISPLAY LINE HEADER ETC.               
         MVC   RECLHD1,DISHED1     SET HEADLINE DATA                            
         OI    RECLHD1H+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   RECLHD2,DISHED2                                                  
         OI    RECLHD2H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    RECLTAU,RECLTAU                                                  
         TM    CURIND1,CUR1SINC                                                 
         BO    *+14                                                             
         MVC   RECLTAU(L'ACUAMT),ACUAMT                                         
         B     DISOUT02                                                         
         MVI   RECLTAU,C'('                                                     
         MVC   RECLTAU+1(L'CURTCUR),FORCURT+(CURTCUR-CURTABD)                   
         MVI   RECLTAU+1+L'CURTCUR,C')'                                         
*                                                                               
DISOUT02 LA    R1,RECLINH          CLEAR, TRANSMIT AND NORMALISE SCREEN         
         SR    RE,RE                                                            
         LA    RF,RECTOTH-1                                                     
DISOUT04 IC    RE,FVTLEN-FVIHDR(R1)                                             
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RE,=Y(L'FVIHDR)                                                  
         BCTR  RE,0                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         EX    RE,*-6                                                           
         NI    FVATRB-FVIHDR(R1),255-FVAHIGH                                    
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         IC    RE,FVTLEN-FVIHDR(R1)                                             
         BXLE  R1,RE,DISOUT04                                                   
*                                                                               
         LA    R3,RECLINH                                                       
         USING DISLINED,R3         R3=A(FIRST TWA LINE)                         
         SR    R5,R5                                                            
         ICM   R5,3,DISLIN         R5=NUMBER OF LINES IN TWA                    
         BZ    DISOUTX                                                          
         LA    R2,DISREC           R2=A(RECORD NUMBERS)                         
DISOUT06 GOTO1 ATSARGET,(R2)       GET TSAR RECORD AND EXPLODE KEY              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDLIN,DISLINED    DISPLAY THIS RECORD                          
         LA    R3,DISLINEL(R3)     BUMP TO NEXT LINE AND RECORD                 
         LA    R2,2(R2)                                                         
         BCT   R5,DISOUT06         DO FOR N'ITEMS IN TABLE                      
*                                                                               
DISOUTX  MVC   FVMSGNO,=AL2(IAENTPAG)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IANOMORE)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         LA    R1,RECLAMTH         CURSOR TO FIRST AMOUNT FIELD                 
         ST    R1,FVADDR                                                        
         B     FVERR                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT SCREEN AND UPDATE TSAR RECORDS FOR LINES THAT HAVE   *         
* BEEN INPUT THIS TIME                                                *         
***********************************************************************         
         SPACE 1                                                                
OVRVAL   CLI   TWASCROV,TWASCROP                                                
         BNE   OVRBLD                                                           
         LA    R1,ROVACCH          TRANSMIT THE WHOLE SCREEN                    
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+8                                                              
         BXLE  R1,RE,*-12                                                       
*                                                                               
         L     R2,AIO2                                                          
         USING BSCRECD,R2                                                       
         XC    BSCKEY,BSCKEY                                                    
         MVI   BSCKTYP,BSCKTYPQ                                                 
         MVC   BSCKCPY,COMPANY                                                  
         GOTO1 AIORDNSI,(R2)                                                    
         BE    *+10                                                             
         XC    BSCKEY,BSCKEY       CLEAR KEY ON ERROR                           
*                                                                               
         LA    R3,ROVACCH                                                       
         USING OVRLINED,R3         R3=A(CURRENT LINE)                           
         SR    R4,R4               R4=A(PREVIOUS LINE)                          
         LA    R5,LINES2           R5=NUMBER OF LINES ON SPECIAL SCREEN         
         MVI   ANYMARK,0           SET NOTHING INPUT THIS TIME                  
*                                                                               
OVRVAL02 TM    OVRLAMTH+(FVATRB-FVIHDR),FVAPROT                                 
         BNZ   OVRVAL56                                                         
         MVI   TSARINDS,0          INITIALISE TSAR RECORD                       
         MVI   TSARIND2,0                                                       
         ZAP   TSARDR,PZERO                                                     
         ZAP   TSARCR,PZERO                                                     
         ZAP   TSARPOST,PZERO                                                   
         ZAP   TSARDISC,PZERO                                                   
*&&UK                                                                           
         ZAP   TSARDR2,PZERO                                                    
         ZAP   TSARCR2,PZERO                                                    
         ZAP   TSARDI2,PZERO                                                    
         ZAP   TSARPO2,PZERO                                                    
*&&                                                                             
*&&US                                                                           
         ZAP   TSARGSTB,PZERO                                                   
         ZAP   TSARGST,PZERO                                                    
         ZAP   TSARPSTB,PZERO                                                   
         ZAP   TSARPST,PZERO                                                    
         XC    TSARGSTT,TSARGSTT                                                
         XC    TSARPSTT,TSARPSTT                                                
         XC    TSARPSTC,TSARPSTC                                                
*&&                                                                             
         MVI   RECVCNDX,0                                                       
         XC    TSARVALS(TSARVALL),TSARVALS                                      
         MVC   TSAROFFC,SPACES                                                  
         XC    TSARDUED,TSARDUED                                                
         MVI   FLAG,0              INPUT FIELD BITS                             
         MVI   BYTE,0              DEFAULT FIELD BITS                           
         SR    R0,R0               R0=A(FIRST NOT INPUT FIELD)                  
*                                                                               
         TM    OVRLACTH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    *+18                                                             
         MVC   RECVCNDX,RECVCNT                                                 
         OI    BYTE,X'80'                                                       
         B     OVRVAL10                                                         
*                                                                               
         GOTO1 AFVAL,OVRLACTH      VALIDATE ACCOUNT FIELD                       
         BE    *+10                                                             
         LR    R0,R1                                                            
         B     OVRVAL10            NO INPUT - NEXT FIELD                        
         OI    FLAG,X'80'          SET ACCOUNT WAS INPUT                        
         CLI   FVIFLD,C'"'         TEST DUPLICATE PREVIOUS LINE                 
         BNE   OVRVAL04                                                         
         LTR   R4,R4                                                            
         BZ    ERRNOTV                                                          
         MVC   OVRLACT,OVRLACT-OVRLINED(R4)                                     
         GOTO1 AFVAL,OVRLACTH                                                   
*                                                                               
OVRVAL04 L     R1,ARECVTAB                                                      
         USING RECVTABD,R1         R1=A(RECEIVABLE TABLE)                       
         SR    RE,RE                                                            
         IC    RE,RECVCNT          RE=NUMBER OF ENTRIES IN TABLE                
OVRVAL06 CLI   FVIFLD,C'*'         TEST *ULACCOUNT INPUT                        
         BNE   *+14                                                             
         CLC   RECVACT+1(L'RECVACT-1),FVIFLD+1                                  
         B     *+10                                                             
         CLC   RECVACT+3(L'RECVACT-3),FVIFLD                                    
         BNE   OVRVAL08                                                         
         MVC   RECVCNDX,RECVNDX                                                 
         MVC   RECVOFF,RECVOFFC                                                 
         B     OVRVAL10                                                         
OVRVAL08 LA    R1,RECVTABL(R1)     BUMP TO NEXT TABLE ENTRY                     
         BCT   RE,OVRVAL06                                                      
         B     ERRERNF                                                          
         DROP  R1                                                               
*                                                                               
OVRVAL10 MVC   TSARCAC,SPACES                                                   
         TM    OVRLCACH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    *+18                                                             
         OI    BYTE,X'40'                                                       
         MVC   TSARCAC+3(L'TSARCAC-3),OVRLCAC                                   
         B     OVRVAL20                                                         
*                                                                               
         GOTO1 AFVAL,OVRLCACH      VALIDATE BILLING SOURCE                      
         BH    FVERR                                                            
         BE    OVRVAL12                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         LR    R0,R1                                                            
         B     OVRVAL20                                                         
*                                                                               
OVRVAL12 OI    FLAG,X'40'          SET SOURCE INPUT                             
         CLI   FVIFLD,C'"'         TEST DUPLICATE PREVIOUS                      
         BNE   OVRVAL14                                                         
         LTR   R4,R4               YES - TEST PREVIOUS SET                      
         BZ    ERRNOTV                                                          
         MVC   OVRLCAC,OVRLCAC-OVRLINED(R4)                                     
         GOTO1 AFVAL,OVRLCACH                                                   
*                                                                               
OVRVAL14 CLI   FVILEN,2            TEST L'SOURCE                                
         BL    ERRSHRT                                                          
         MVC   TSARCAC+3(L'TSARCAC-3),FVIFLD                                    
         CLI   BSCKTYP,BSCKTYPQ    TEST BILLING SOURCE RECORD FOUND             
         BNE   OVRVAL20                                                         
*                                                                               
         LA    R1,BSCRECD          LOOK UP BILLING SOURCE IN RECORD             
         DROP  R2                                                               
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
         USING BSCELD,R1                                                        
         CLI   BSCEL,0             TEST ANY ELEMENTS ON RECORD                  
         BE    OVRVAL20                                                         
OVRVAL16 CLI   BSCEL,0             TEST EOR                                     
         BE    ERRINVBS                                                         
         CLI   BSCEL,BSCELQ        TEST BILLING SOURCE ELEMENT                  
         BNE   OVRVAL18                                                         
         IC    RF,BSCLN            TEST CORRECT LENGTH                          
         SH    RF,=Y(BSCBSRC+1-BSCELD)                                          
         CHI   RF,11                                                            
         BNH   *+8                                                              
         LA    RF,11                                                            
*        CLM   RF,1,FVXLEN                                                      
*        BNE   OVRVAL18                                                         
         EX    RF,*+8              MATCH INPUT STRING TO ELEMENT                
         BE    OVRVAL20                                                         
         CLC   FVIFLD(0),BSCBSRC                                                
OVRVAL18 IC    RF,BSCLN            BUMP TO NEXT RECORD ELEMENT                  
         AR    R1,RF                                                            
         B     OVRVAL16                                                         
         DROP  R1                                                               
*                                                                               
OVRVAL20 OC    TSARCAC,SPACES                                                   
         GOTO1 AFVAL,OVRLBILH      VALIDATE REFERENCE                           
         BH    FVERR                                                            
         BE    OVRVAL22                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         LR    R0,R1                                                            
         B     OVRVAL26                                                         
*                                                                               
OVRVAL22 OI    FLAG,X'20'                                                       
         CLI   FVIFLD,C'"'         TEST DUPLICATE PREVIOUS                      
         BNE   OVRVAL24                                                         
         LTR   R4,R4               YES - TEST PREVIOUS SET                      
         BZ    ERRNOTV                                                          
         MVC   OVRLBIL,OVRLBIL-OVRLINED(R4)                                     
         GOTO1 AFVAL,OVRLBILH                                                   
*                                                                               
OVRVAL24 MVC   TSARREF,FVIFLD                                                   
*                                                                               
OVRVAL26 GOTO1 AFVAL,OVRLDATH      VALIDATE DATE                                
         BH    FVERR                                                            
         BE    OVRVAL28                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         LR    R0,R1                                                            
         B     OVRVAL34                                                         
*                                                                               
OVRVAL28 OI    FLAG,X'10'                                                       
         CLI   FVIFLD,C'"'         TEST DUPLICATE PREVIOUS                      
         BNE   OVRVAL30                                                         
         LTR   R4,R4               YES - TEST PREVIOUS SET                      
         BZ    ERRNOTV                                                          
         MVC   OVRLDAT,OVRLDAT-OVRLINED(R4)                                     
         GOTO1 AFVAL,OVRLDATH                                                   
*                                                                               
OVRVAL30 MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ERRINVDT                                                         
         LA    R2,WORK             EDIT OUT DATE                                
         USING PERVALD,R2                                                       
*&&UK                                                                           
         GOTO1 VDATCON,DMCB,(1,TODAYP),(0,TEMP)                                 
         GOTO1 VADDAY,DMCB,TEMP,TEMP+6,7                                        
         MVC   FVMSGNO,=AL2(AE$DTFIF)                                           
         CLC   PVALESTA,TEMP+6     CAN'T BE HIGHER THAN TODAY+7 DAYS            
         BH    FVERR                                                            
*&&                                                                             
*&&US                                                                           
         CLC   PVALPSTA,TODAYP     CAN'T BE HIGHER THAN TODAY                   
         BH    ERRINVDT                                                         
*&&                                                                             
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   OVRVAL32                                                         
         SR    RE,RE                                                            
         ICM   RE,1,TODAYB                                                      
         BNZ   *+8                                                              
         LA    RE,100              YEAR 2000 IS 100 RELATIVE                    
         MH    RE,=H'12'                                                        
         SR    R1,R1                                                            
         IC    R1,TODAYB+1                                                      
         AR    RE,R1               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'12'                                                        
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         BNZ   *+8                                                              
         LA    RE,100                                                           
         MH    RE,=H'12'                                                        
         IC    R1,PVALBSTA+1                                                    
         AR    RE,R1               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN NN MONTHS AGO                 
         BL    ERRINVDT                                                         
OVRVAL32 MVC   TSARDAT,PVALPSTA    PACKED START DATE                            
         XC    OVRLDAT,OVRLDAT                                                  
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(17,OVRLDAT)                            
         DROP  R2                                                               
*                                                                               
OVRVAL34 GOTO1 AFVAL,OVRLOFFH      VALIDATE OFFICE                              
         BH    FVERR                                                            
         BL    OVRVAL40                                                         
         OI    FLAG,X'08'                                                       
         CLI   FVIFLD,C'"'         TEST DUPLICATE PREVIOUS                      
         BNE   OVRVAL36                                                         
         LTR   R4,R4               YES - TEST PREVIOUS SET                      
         BZ    ERRNOTV                                                          
         MVC   OVRLOFF,OVRLOFF-OVRLINED(R4)                                     
         GOTO1 AFVAL,OVRLOFFH                                                   
*                                                                               
OVRVAL36 MVC   TSAROFFC,FVIFLD     SET OFFICE CODE IN TSAR VALUES               
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    OVRVAL46            NO - USE WHATEVER THEY INPUT                 
         CLC   FLTOFFC,SPACES      TEST OFFICE FILTER VALUE SET                 
         BNH   *+14                                                             
         CLC   TSAROFFC,FLTOFFC    TEST INPUT OFFICE MATCHES FILTER             
         BNE   ERRNOTV                                                          
         CLC   RECVOFF,SPACES      TEST RECV A/C SPECIFIES OFFICE               
         BNH   OVRVAL37                                                         
         CLC   TSAROFFC,RECVOFF    YES - INPUT MUST MATCH RECV A/C              
         BE    OVRVAL46                                                         
*&&UK*&& B     ERRNOTV                                                          
*&&US*&& B     ERROFDIF                                                         
*                                                                               
OVRVAL37 CLI   COMPOFFL,1          TEST ONE CHARACTER OFFICE CODES              
         BNE   OVRVAL38                                                         
         L     R2,AIO2             CHECK FOR OFFICE IN 2D                       
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVI   ACTKUNT,C'2'                                                     
         MVI   ACTKLDG,C'D'                                                     
         MVC   ACTKACT(L'TSAROFFC),TSAROFFC                                     
         GOTO1 AIORDNSI,(R2)                                                    
         BNE   ERRNOTV                                                          
         B     OVRVAL39                                                         
         DROP  R2                                                               
*                                                                               
OVRVAL38 DS    0H                                                               
*&&US                                                                           
         TM    BATCHSEC,CPYBSOFF   TEST OVERRIDE OFFICE SECURITY                
         BNZ   OVRVAL39                                                         
         L     R2,AIO2             READ OFFICE RECORD                           
         USING OFFRECD,R2                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,TSAROFFC                                                 
         GOTO1 AIORDNSI,(R2)                                                    
         BNE   ERROFCNF            MUST BE ON FILE                              
         TM    OFFRSTAT,OFFSLIST                                                
         BNZ   ERROFCNF            AND MUST NOT BE A LIST                       
         B     OVRVAL46                                                         
         DROP  R2                                                               
*&&                                                                             
OVRVAL39 L     R1,AOFFBLK          TEST OFFICE SECURITY                         
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFAVAL                                                  
         MVC   OFFAOFFC,TSAROFFC                                                
         GOTO1 VOFFAL,OFFALD                                                    
*&&US*&& BNE   ERRSECLK            ERROR FROM OFFAL                             
*&&UK*&& BNE   ERRNOTV                                                          
         DROP  R1                                                               
*                                                                               
OVRVAL40 TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BNZ   OVRVAL42                                                         
         MVC   TSAROFFC,SPACES     NO - DEFAULT OFFICE IS SPACES                
         CLC   FLTOFFC,SPACES      TEST OFFICE FILTER VALUE SET                 
         BNH   *+10                                                             
         MVC   TSAROFFC,FLTOFFC    YES - USE FILTER VALUE                       
         OI    BYTE,X'08'                                                       
         B     OVRVAL46                                                         
*                                                                               
OVRVAL42 CLC   FLTOFFC,SPACES      TEST OFFICE FILTER VALUE SET                 
         BNH   *+14                                                             
         MVC   TSAROFFC,FLTOFFC    YES - USE OFFICE FILTER VALUE                
         B     OVRVAL43                                                         
         CLC   RECVOFF,SPACES      IS THERE A DEFAULT OFFICE                    
         BNH   OVRVAL44                                                         
         MVC   TSAROFFC,RECVOFF    YES - USE RECV A/C OFFICE CODE               
*                                                                               
OVRVAL43 OI    BYTE,X'08'                                                       
         CLI   FLAG,0              TEST FIRST INPUT FIELD ON LINE               
         BE    *+10                                                             
         MVC   OVRLOFF,TSAROFFC    NO - DISPLAY DEFAULT OFFICE                  
         B     OVRVAL46                                                         
*                                                                               
OVRVAL44 LTR   R0,R0               POINT R0 TO MISSING FIELD                    
         BNZ   *+6                                                              
         LR    R0,R1                                                            
*                                                                               
OVRVAL46 GOTO1 AFVAL,OVRLAMTH      VALIDATE AMOUNT                              
         BH    FVERR                                                            
         BE    OVRVAL48                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         LR    R0,R1                                                            
         B     OVRVAL52                                                         
*                                                                               
OVRVAL48 OI    FLAG,X'04'                                                       
         GOTO1 AVALAMT,DMCB,0,(X'80',TSARPOST)                                  
         BNE   FVERR                                                            
         ZAP   DUB,TSARPOST        EXTRACT 6 BYTE PACKED FIELD                  
*&&UK                                                                           
         MVC   TEMP(L'AFCX),ATLX                                                
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    OVRVAL50                                                         
         MVC   TSARCUR,FORCURT+(CURTCUR-CURTABD)                                
         MVC   TSARAFCX,ATLX                                                    
         ZAP   TSARPO2,DUB                                                      
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   RE,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   RE,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   RE,RE                                                            
         STC   RE,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP DUB,TEMP            CALCULATE USING EXCHANGE RATE                
         CP    DUB,PZERO           TEST ZERO AGENCY CURRENCY                    
         BE    ERRCUZER                                                         
*&&                                                                             
OVRVAL50 ZAP   TSARPOST,DUB                                                     
*&&UK                                                                           
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BZ    OVRVAL52                                                         
         EXCHP DUB,TEMP            CALCULATE USING EXCHANGE RATE                
         CP    DUB,PZERO           TEST ZERO LOCAL CURRENCY                     
         BE    ERRCUZER                                                         
         ZAP   TSARPO2,DUB                                                      
*&&                                                                             
OVRVAL52 CLI   FLAG,0              TEST ANY INPUT ON THIS LINE                  
         BE    OVRVAL56                                                         
         OC    FLAG,BYTE           SET ON DEFAULT FIELD BITS                    
         TM    FLAG,X'FC'          TEST ALL FIELDS INPUT ON THIS LINE           
         BO    *+12                                                             
         ST    R0,FVADDR           SET CURSOR TO FIRST NOT INPUT & EXIT         
         B     ERRNONE                                                          
*                                                                               
         OI    ANYMARK,1           SET INPUT ON THIS SCREEN                     
         MVI   ANYUPDAT,1          SET UPDATE FLAG                              
         ZAP   POSTAMNT,TSARPOST   SAVE TSARPOST                                
*&&UK*&& ZAP   POSTAM2,TSARPO2                                                  
         GOTO1 ATSARADD,TSARTOVR   ADD RECORD TO TSAR BUFFER                    
         BNE   FVERR                                                            
         AP    DIFTOT,POSTAMNT     ADD TO TOTAL DIFFERENCES                     
*&&UK*&& AP    DIFTO2,POSTAM2                                                   
         XC    POSTAMNT,POSTAMNT                                                
*&&UK*&& XC    POSTAM2,POSTAM2                                                  
         OI    OVRLACTH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         OI    OVRLCACH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         OI    OVRLBILH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         OI    OVRLDATH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         OI    OVRLOFFH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         OI    OVRLAMTH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         TM    PROFTOT,PROFTDIF    TEST DIFFERENCES TOTAL WANTED                
         BZ    *+8                                                              
         OI    TOTPROF,PROFTDIF    YES - SET TOTAL ON                           
*                                                                               
OVRVAL56 LR    R4,R3               R4=A(PREVIOUS LINE)                          
         LA    R3,OVRLINEL(R3)     BUMP TO NEXT LINE                            
         BCT   R5,OVRVAL02         DO FOR NUMBER OF LINES ON SCREEN             
*                                                                               
         MVI   TWASCROV,0          RESET SCREEN TO FORCE CALLOV                 
         CLI   ANYMARK,0           TEST ANY INPUT THIS TIME                     
         BNE   OVRVALX                                                          
         XC    RECACT,RECACT       CHANGE ACTION TO INPUT                       
         MVC   RECACT(L'AC8INP),AC8INP                                          
         OI    RECACTH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   ACTIONL,ACTION                                                   
         MVI   ACTION,ACTINPT                                                   
         B     DISBLD              GO AND DISPLAY INPUT SCREEN                  
OVRVALX  DS    0H                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD OVERPAYMENT SCREEN READY FOR USER INPUT                       *         
***********************************************************************         
         SPACE 1                                                                
OVRBLD   CLI   TWASCROV,TWASCROP   OVERLAY SPECIALS SCREEN                      
         BE    OVRBLD06            IF SCREEN IS DISPLAYED, VALIDATE             
         OC    TWASAGN,TWASAGN     TEST NEW SECURITY IN USE                     
         BZ    OVRBLD02                                                         
         GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',ACTION)              
         BNE   ERRSECLK                                                         
OVRBLD02 GOTO1 AOVRSCR,TWASCROP                                                 
         CLI   RECVCNT,1           TEST MULTIPLE RECEIVABLE ACCOUNTS            
         BNE   OVRBLD12                                                         
         LA    R1,ROVACCH          NO - PRESET ACCOUNT CODE & PROTECT           
         USING OVRLINED,R1                                                      
         LA    R0,LINES2                                                        
OVRBLD04 CLC   RECVCACT+1(L'RECVUL),RECVUL                                      
         BNE   *+14                                                             
         MVC   OVRLACT(L'RECVACT-3),RECVCACT+3                                  
         B     *+14                                                             
         MVI   OVRLACT+0,C'*'                                                   
         MVC   OVRLACT+1(L'RECVCACT-1),RECVCACT+1                               
         OI    OVRLACTH+(FVATRB-FVIHDR),FVAPROT                                 
         LA    R1,OVRLINEL(R1)                                                  
         BCT   R0,OVRBLD04                                                      
*                                                                               
         CLI   AGYCTRY,CTRYGER     TEST GERMAN                                  
         BNE   OVRBLD12                                                         
         LA    RF,SOURCES          TEST PREDEFINED SOURCES PASSED               
         LA    R1,ROVACCH          R1=A(FIRST TWA LINE)                         
         LA    R0,LINES2           R0=NUMBER OF LINES ON SCREEN                 
OVRBLD06 CLI   0(RF),0                                                          
         BNE   OVRBLD08                                                         
         XC    OVRLACT,OVRLACT                                                  
         OI    OVRLACTH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    OVRLCAC,OVRLCAC                                                  
         OI    OVRLCACH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    OVRLBIL,OVRLBIL                                                  
         OI    OVRLBILH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    OVRLDAT,OVRLDAT                                                  
         OI    OVRLDATH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    OVRLOFF,OVRLOFF                                                  
         OI    OVRLOFFH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    OVRLAMT,OVRLAMT                                                  
         OI    OVRLAMTH+(FVATRB-FVIHDR),FVAPROT                                 
         B     OVRBLD10                                                         
OVRBLD08 MVC   OVRLCAC,0(RF)       SET SOURCE CODE                              
         OI    OVRLCACH+(FVATRB-FVIHDR),FVAPROT                                 
         LA    RF,L'OVRLCAC(RF)    BUMP TO NEXT TABLE ENTRY                     
OVRBLD10 LA    R1,OVRLINEL(R1)     BUMP TO NEXT INPUT LINE                      
         BCT   R0,OVRBLD06         DO FOR NUMBER OF LINES                       
         DROP  R1                                                               
*                                                                               
OVRBLD12 MVC   FVMSGNO,=AL2(IAENTDIF)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         LA    R1,ROVACCH          POINT TO ACCOUNT FIELD                       
         TM    ROVACCH+(FVATRB-FVIHDR),FVAPROT                                  
         BZ    *+8                                                              
         LA    R1,ROVSRCH          OR SOURCE FIELD (IF SINGLE ACCOUNT)          
         TM    ROVSRCH+(FVATRB-FVIHDR),FVAPROT                                  
         BZ    *+8                                                              
         LA    R1,ROVBILH          OR BILL FIELD IF SOURCE DEFINED              
         ST    R1,FVADDR                                                        
         B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ERROR EXITS                                                  *         
***********************************************************************         
         SPACE 1                                                                
ERRSHRT  MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         B     FVERR                                                            
ERRNOTV  MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     FVERR                                                            
ERRISCRL MVC   FVMSGNO,=AL2(EASCRINV)                                           
         B     FVERR                                                            
ERRLONG  MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     FVERR                                                            
ERRINVDT MVC   FVMSGNO,=AL2(EGDATINV)                                           
         B     FVERR                                                            
ERRNONE  MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     FVERR                                                            
ERRKDUP  MVC   FVMSGNO,=AL2(EGOPTDUP)                                           
         B     FVERR                                                            
ERRERNF  MVC   FVMSGNO,=AL2(EGRECNOF)                                           
         B     FVERR                                                            
ERRIAMNT MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     FVERR                                                            
ERRNSTR  MVC   FVMSGNO,=AL2(AE$NSDCT)                                           
         B     FVERR                                                            
ERRSELF  MVC   FVMSGNO,=AL2(EASELFTR)                                           
         B     FVERR                                                            
ERRINVBS MVC   FVMSGNO,=AL2(EAINVSRC)                                           
         B     FVERR                                                            
ERRINVAL MVC   FVMSGNO,=AL2(EAINVALC)                                           
         B     FVERR                                                            
ERRNALTR MVC   FVMSGNO,=AL2(EANALTRF)                                           
         B     FVERR                                                            
ERRCRASP MVC   FVMSGNO,=AL2(EACRASPE)                                           
         B     FVERR                                                            
ERRCRAAL MVC   FVMSGNO,=AL2(EACRAALC)                                           
         B     FVERR                                                            
ERRCTALC MVC   FVMSGNO,=AL2(EACTRALC)                                           
         B     FVERR                                                            
ERRNLDAC MVC   FVMSGNO,=AL2(EANLDACC)                                           
         B     FVERR                                                            
ERRNLDSC MVC   FVMSGNO,=AL2(EANLDISC)                                           
         B     FVERR                                                            
ERRLATDE MVC   FVMSGNO,=AL2(EALATDED)                                           
         B     FVERR                                                            
ERRSECLK MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     FVERR                                                            
ERROFDIF MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         B     FVERR                                                            
ERRQHSDR MVC   FVMSGNO,=AL2(AE$QHSDR)                                           
         B     FVERR                                                            
ERRDDTDR MVC   FVMSGNO,=AL2(AE$DDTDR)                                           
         B     FVERR                                                            
ERRCUBAL MVC   FVMSGNO,=AL2(AE$CUBAL)                                           
         B     FVERR                                                            
ERRCUZER MVC   FVMSGNO,=AL2(AE$CUZER)                                           
         B     FVERR                                                            
ERROFCNF MVC   FVMSGNO,=AL2(AE$OFCNF)                                           
         B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE, FIELD INDEX INFO & EXTRA MESSAGE   *         
*                                                                     *         
* NTRY - FVADDR=A(FIELD HEADER OF FIELD IN ERROR)                     *         
*        FVMSGNO=FIELD ERROR NUMBER                                   *         
*        FVFLAG=ZERO IF A STANDARD CONTROLLER ERROR MESSAGE REQUIRED  *         
*        FVOSYS=OVERRIDE SYSTEM FOR GETTXT CALL (ZERO=STANDARD)       *         
*        FVINDX=MULTIPLE FIELD INDEX NUMBER                           *         
*        FVSUBX=MULTIPLE FIELD SUB-INDEX NUMBER                       *         
*        FVXTRA=USER SUPPLIED MESSAGE TO TACK ONTO GENERAL MESSAGE    *         
*                                                                     *         
* NTR AT FVERR  TO SET MULTIPLE FIELD INDEX VALUES TO ZERO            *         
*        FVERRX ONLY TO SET CURSOR TO FIELD ADDRESSED BY FVADDR       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
FVERR    LA    R2,PARM             DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FVINDX                                                    
         MVC   GTSUBX,FVSUBX                                                    
         MVC   GTMSGNO,FVMSGNO                                                  
         MVC   GTMSYS,FVOSYS       OVERRIDE SYSTEM (IF SET)                     
         MVC   GTMTYP,FVOMTYP      OVERRIDE MESSAGE TYPE (IF SET)               
         CLI   GTMSGNO,X'FF'       STD CONTROLLER MSG                           
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,X'FF'        GENERAL SYSTEM MESSAGE                       
         OC    GTMSGNO,GTMSGNO     MESSAGE 0 = DATAMGR ERR                      
         BNZ   FVERR10                                                          
         LA    R1,DMCB                                                          
         STCM  R1,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
FVERR10  CLI   FVXTRA,C' '         LOOK FOR ADDITIONAL TEXT                     
         BH    FVERR12                                                          
         CLI   FVXTRA,X'00'        LOOK SUBSTITUTION TEXT                       
         BNH   FVERR20                                                          
         LA    R1,FVXTRA                                                        
         STCM  R1,7,GTASUBST                                                    
         B     FVERR20                                                          
FVERR12  LA    R1,FVXTRA                                                        
         STCM  R1,7,GTATXT                                                      
         LA    RF,L'FVXTRA-1(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    RF,1(RF)                                                         
         STC   RF,GTLTXT                                                        
*                                                                               
FVERR20  LA    R2,PARM             PARM DEFINED INTERNALLY                      
         CLI   GTMSGNO,X'FF'       CHECK FOR GENERAL MESSAGES                   
         BNE   *+12                                                             
         MVI   GTMSYS,X'FF'        FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
         XC    RECMSG,RECMSG                                                    
         GOTO1 VGETTXT,GETTXTD                                                  
         SR    RF,RF                                                            
         DROP  R2                                                               
*                                                                               
FVERRX   OI    RECMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    *+8                                                              
         OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         SPACE 1                                                                
         L     R1,ATSARBLK         SAVE TSAR BUFFER ON DISK                     
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 VTSAR                                                            
         DROP  R1                                                               
*                                                                               
         LA    R1,RECTOTH          DISPLAY TOTALS                               
         CLI   TWASCROV,TWASCRIN                                                
         BE    FVERRX2                                                          
         LA    R1,ROVTOTH                                                       
         CLI   TWASCROV,TWASCROP                                                
         BE    FVERRX2                                                          
         CLC   TWASCROV,TWASCRBH                                                
         BNE   FVERRX4                                                          
         SR    R1,R1                                                            
         ICM   R1,3,SHEADTOT                                                    
         BZ    FVERRX4                                                          
         LA    R1,RECOLAYH(R1)                                                  
FVERRX2  GOTO1 ABLDTOT                                                          
*                                                                               
FVERRX4  LA    R1,RECPFKH          DISPLAY PF KEYS                              
         CLI   TWASCROV,TWASCRIN                                                
         BE    FVERRX6                                                          
         LA    R1,ROVPFKH                                                       
         CLI   TWASCROV,TWASCROP                                                
         BE    FVERRX6                                                          
         CLC   TWASCROV,TWASCRBH                                                
         BNE   FVERRX8                                                          
         SR    R1,R1                                                            
         ICM   R1,3,SHEADPFK                                                    
         BZ    FVERRX8                                                          
         LA    R1,RECOLAYH(R1)                                                  
FVERRX6  GOTO1 ABLDPFK                                                          
*                                                                               
FVERRX8  L     RF,AINP             SET NEXT DEFAULT PF KEY IF REQUIRED          
         MVI   TWANXTPF,0                                                       
         TM    TWAMODE,TWAMSETP                                                 
         BZ    *+10                                                             
         MVC   TWANXTPF,TIOBAID-TIOBD(RF)                                       
         NI    TWAMODE,255-TWAMSETP-TWAMDOIT                                    
         MVC   ACTNAME,RECACT      SAVE THIS TIME ACTION NAME                   
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0X                  ** GLOBAL (LITERAL) VALUES **                
         DC    P'0'                                                             
         DC    C'ACCFIL  '                                                      
         DC    C'ACCDIR  '                                                      
         DC    C'ACCMST  '                                                      
         DC    C'ACCARC  '                                                      
         DC    C'TEMPSTR '                                                      
         DC    C'DMADD   '                                                      
         DC    C'DMRDHI  '                                                      
         DC    C'DMREAD  '                                                      
         DC    C'DMRSEQ  '                                                      
         DC    C'DMWRT   '                                                      
GLOBALSL EQU   *-GLOBALS                                                        
         SPACE 1                                                                
         DS    0H                                                               
SCRMAXIL DC    AL2(SCRMAXI)                                                     
SCRPAGEL DC    AL2(SCRPAGE)                                                     
SCRLEFTL DC    AL2(SCRLEFT)                                                     
SCRRGHTL DC    AL2(SCRRGHT)                                                     
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
DELIMS   DC    C',,#,,,,,'         DELIMITER CHARACTER                          
         SPACE 2                                                                
TABLES   DS    0H                  ** ADDRESSES OF TABLES **                    
         DC    AL2(CTRYTAB-RECALL)                                              
         DC    AL2(KEYTAB-RECALL)                                               
         DC    AL2(DISTAB-RECALL)                                               
         DC    AL2(TOTTAB-RECALL)                                               
         DC    AL2(DCLISTU-RECALL)                                              
         DC    AL2(DCLISTL-RECALL)                                              
         DC    AL2(APFTAB-RECALL)                                               
         DC    AL2(ACTTAB-RECALL)                                               
         DC    AL2(SPFTAB-RECALL)                                               
         DC    AL2(SCRTAB-RECALL)                                               
         DC    AL2(LOCTAB-RECALL)                                               
         DC    AL2(OPTTAB-RECALL)                                               
         DC    AL2(LACTAB-RECALL)                                               
TABLESN  EQU   (*-TABLES)/L'TABLES                                              
         EJECT                                                                  
LOCTAB   DS    0H                  ** LOCATE TABLE (LOCTABD) **                 
         DC    S(OP8DATE),S(OP3DATE),S(VLODATE),AL1(8),AL1(0)                   
         DC    S(OP8BILC),S(OP3BILC),S(VLOBILL),AL1(6),AL1(0)                   
         DC    S(OP8ACC),S(OP3ACC),S(VLOACCT),AL1(15),AL1(0)                    
         DC    S(OP8SRC),S(OP3SRC),S(VLOSRCE),AL1(12),AL1(0)                    
LOCTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
OPTTAB   DS    0H                  ** OPTION TABLE (OPTTABD) **                 
         DC    S(OP8SWTCH),S(OP3SWTCH),S(OPTSWTC)                               
         DC    AL1(OPTI1PT+OPTISWT),AL1(0)                                      
         DC    S(OP8MARKD),S(OP3MARKD),S(OPTMARK)                               
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(OP8BAL),S(OP3BAL),S(OPTBALS)                                   
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(AC8SPCL),S(AC3SPCL),S(OPTSPCL)                                 
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(AC@WRTF),S(AC@WRTF),S(OPTWOFF)                                 
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(AC@DISS),S(AC@DISS),S(OPTDISC)                                 
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(AC@OFFST),S(AC@OFFST),S(OPTOFST)                               
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(OP8DATE),S(OP3DATE),S(VOPDATE)                                 
         DC    AL1(OPTI2PT),AL1(8)                                              
         DC    S(OP8BILC),S(OP3BILC),S(VOPBILL)                                 
         DC    AL1(OPTI2PT),AL1(6)                                              
         DC    S(OP8ACC),S(OP3ACC),S(VOPACCT)                                   
         DC    AL1(OPTI2PT),AL1(15)                                             
         DC    S(OP8SRC),S(OP3SRC),S(VOPSRCE)                                   
         DC    AL1(OPTI2PT),AL1(12)                                             
         DC    S(AC@AMT),S(AC@AMT),S(VOPAMNT)                                   
         DC    AL1(OPTI2PT),AL1(11)                                             
         DC    S(OP8OFF),S(OP3OFF),S(VOPOFFC)                                   
         DC    AL1(OPTI2PT),AL1(2)                                              
         DC    S(OP8DSP),S(OP3DSP),S(VOPDISP)                                   
         DC    AL1(OPTI2PT),AL1(L'OPTDIS)                                       
         DC    S(AC@XFR),S(AC@XFR),S(OPTXFRS)                                   
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(AC@HELD),S(AC@HELD),S(OPTHELD)                                 
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    S(AC@QUERD),S(AC@QUERD),S(OPTQUER)                               
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
*&&UK*&& DC    S(AC@CLRNG),S(AC@CLRNG),S(OPTCLRNG)                              
*&&UK*&& DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
*&&UK*&& DC    S(OP8CURRY),S(OP8CURRY),S(VOPCURR)                               
*&&UK*&& DC    AL1(OPTI2PT),AL1(L'CURTCUR)                                      
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* CONTROLLER ROUTINES - 1                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
ROUTN1   NMOD1 0,**ROU1**,RA,R9,R8                                              
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         AHI   RF,4                                                             
         B     *(RF)                                                            
         B     VALAMT                                                           
         B     OVRSCR                                                           
         B     VALBAT                                                           
         B     VALMON                                                           
         B     VALRCV                                                           
         B     VALWCP                                                           
         B     VALBNK                                                           
         B     VALANL                                                           
         B     VALDEP                                                           
         B     VALCDT                                                           
         B     VALICO                                                           
         B     VALWAC                                                           
         B     VALDAT                                                           
         B     VALMOS                                                           
         B     VALBNR                                                           
         B     VALBSO                                                           
         B     VALDSC                                                           
*&&UK*&& B     VALVAT                                                           
*&&US*&& B     VALGST                                                           
*&&US*&& B     VALPST                                                           
         B     IOHIGH                                                           
         B     IOSEQ                                                            
         B     IOREAD                                                           
         B     IORDNSI                                                          
         B     TSARADD                                                          
         B     TSARGET                                                          
         B     ACCELS                                                           
         B     FVAL                                                             
         B     GETLDG                                                           
         B     BLDDIS                                                           
         B     BLDTRN                                                           
         B     BLDNAR                                                           
         B     GETACC                                                           
         B     BLDSTA                                                           
         B     GETRCV                                                           
         B     MRKTRN                                                           
         B     READTR                                                           
*                                                                               
ROU1L    MVI   DUB,0               SET CC=LOW                                   
         B     ROU1CC                                                           
ROU1E    MVI   DUB,1               SET CC=EQUAL                                 
         B     ROU1CC                                                           
ROU1H    MVI   DUB,2               SET CC=HIGH                                  
ROU1CC   CLI   DUB,1                                                            
*                                                                               
ROU1X    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A CASH AMOUNT                                   *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) AS FOLLOWS:-                            *         
*        P1=A(INPUT FIELD HEADER)                                     *         
*        P2=X'80' - USE FORCURT, IF AVAILABLE                         *         
*        P2=A(6 BYTE PACKED OUTPUT VALUE)                             *         
*                                                                     *         
* EXIT - CC=LOW IF FIELD NOT INPUT                                    *         
*        CC=EQUAL IF FIELD INPUT AND CORRECT                          *         
*        CC=HIGH IF FIELD INPUT AND INVALID (WITH FVMSGNO SET)        *         
***********************************************************************         
         SPACE 1                                                                
VALAMT   LR    R2,R1               R2=A(PARAMETER LIST)                         
         L     R1,4(R2)                                                         
         ZAP   0(6,R1),PZERO       CLEAR OUTPUT VALUE                           
         ICM   R1,15,0(R2)                                                      
         BZ    VALAMT02                                                         
         GOTO1 AFVAL               VALIDATE THE INPUT                           
         BNE   ROU1X                                                            
VALAMT02 MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         TM    4(R2),X'80'         TEST USE CURRENCY ENTRY, IF KNOWN            
         BZ    *+8                                                              
*&&UK                                                                           
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
*&&                                                                             
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,PARM,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   ROU1H                                                            
         ZAP   DUB,4(8,R1)                                                      
         OC    DUB(2),DUB          ENSURE WILL PACK INTO 6 BYTES                
         BNZ   ROU1H                                                            
         L     R1,4(R2)                                                         
         ZAP   0(6,R1),DUB         RETURN PACKED NUMBER TO CALLER               
         BAS   RE,CLRFLD                                                        
         LA    R3,AGYCURT          AGENCY DEFAULT                               
         TM    4(R2),X'80'         TEST USE CURRENCY ENTRY, IF KNOWN            
         BZ    *+8                                                              
*&&UK                                                                           
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         LA    R3,FORCURT                                                       
*&&                                                                             
         LA    R2,L'FVIHDR(RF)                                                  
         CURED DUB,((R0),(R2)),(R3),ALIGN=LEFT,FLOAT=-                          
VALAMTX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY A SCREEN INTO TWA0 AT RECOLAYH                   *         
*                                                                     *         
* NTRY - R1=SCREEN OVERLAY NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
OVRSCR   STC   R1,TWASCROV         SET SCREEN NUMBER                            
         MVC   DMCB+4(3),=X'D90608'                                             
         MVC   DMCB+7(1),TWASCROV                                               
         GOTO1 VCALLOV,DMCB,(0,RECOLAYH)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KILL IF CAN'T LOAD SCREEN                    
         LA    R1,RECMSGH          RE-TRANSMIT ALL HEADER FIELDS                
         SR    RE,RE                                                            
         LA    RF,RECOLAYH-1                                                    
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,*-12                                                       
         CLC   TWASCROV,TWASCRBH   TEST HEADER SCREEN JUST LOADED               
         BNE   OVRSCRX                                                          
         L     RF,AUTL                                                          
         MVC   TWAUTLSV,TSCRNE-UTLD(RF)                                         
OVRSCRX  B     ROU1X                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALBAT   XC    BATREF,BATREF                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VALBAT2  CLI   0(R1),C' '          TEST VALID BATCH REFERENCE                   
         BL    ROU1H                                                            
         BE    VALBAT4                                                          
*&&UK                                                                           
         CLI   0(R1),C'A'          ONLY ALLOW ALPHAMERIC IN THE UK              
         BL    ROU1H                                                            
*&&                                                                             
*&&US                                                                           
         CLC   0(1,R1),DELIM       ALLOW ANY EXCEPT A DELIMITER                 
         BE    ROU1H                                                            
         CLC   0(1,R1),EQUALS      OR AN EQUALS SIGN                            
         BE    ROU1H                                                            
*&&                                                                             
VALBAT4  LA    R1,1(R1)                                                         
         BCT   R0,VALBAT2                                                       
         MVC   BATREF,FVIFLD                                                    
VALBATX  B     ROU1E                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
         SPACE 1                                                                
VALMON   XC    BATMON,BATMON                                                    
*&&US                                                                           
         CLI   FVILEN-FVIHDR(R1),0                                              
         BNE   VALMON2                                                          
         LR    R0,R1               DEFAULT IS CURRENT CALENDAR MONTH            
         LA    RF,L'FVIHDR(R1)                                                  
         GOTO1 VDATCON,DMCB,(1,TODAYP),(18,(RF))                                
         LR    R1,R0                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
*&&                                                                             
VALMON2  MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
W        USING BMONVALD,WORK                                                    
                                                                                
         L     RF,=V(BMONVAL)                                                   
         A     RF,RELO                                                          
         ST    RF,VBMONVAL                                                      
         GOTO1 VBMONVAL,DMCB,(0,FVIHDR),(30,ACOM),                     X        
               (AGYLANG,W.BMONVALD),(COMPANY,0)                                 
         CLI   W.BMOERR,BMOEOKQ                                                 
         BE    *+14                                                             
         MVC   FVMSGNO,W.BMOMSG                                                 
         B     ROU1H                                                            
         MVC   BATCHSEC,0(R1)      SET BATCH SECURITY LEVEL                     
         MVC   BATMONP,W.BMOMOSP   SET PWOS BATCH MONTH                         
         MVC   BATMON,W.BMOMOSC    SET CHARACTER BATCH MONTH                    
         MVC   MOSLOCK,W.BMOLCKP   SET LATEST LOCK MOS                          
         DROP  W                                                                
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         MVC   TEMP(L'BATMONP),BATMONP                                          
         MVI   TEMP+L'BATMONP,X'01'                                             
         GOTO1 VDATCON,DMCB,(1,TEMP),(9,(RF))                                   
VALMONX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECEIVING ACCOUNT (NEED NOT BE IN RECEIVABLE LEDGER)       *         
***********************************************************************         
         SPACE 1                                                                
VALRCV   L     RF,0(R1)                                                         
         ST    RF,FVADDR           SAVE 1ST RECEIVABLE ACCOUNT FIELD            
         L     RF,4(R1)                                                         
         ST    RF,SVADDR           AND 2ND RECEIVABLE ACCOUNT FIELD             
         L     R2,FVADDR                                                        
         BAS   RE,CHKCOMMA         CHECK FOR COMMA AT END OF FIELD              
         L     R2,SVADDR                                                        
         BAS   RE,CHKCOMMA                                                      
         MVI   FVINDX,0                                                         
         NI    RECBIT,X'FF'-(TWORCV+LOWLEV)                                     
         MVI   BYTE,0                                                           
         L     R0,ARECVTAB          CLEAR RECEIVABLES TABLE & EOT               
         LH    R1,=Y(RECVLTAB)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R0,ASCANOUT         CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VALRCV2  MVI   RECVCNT,0           RESET ACCOUNT COUNTER                        
         MVI   FVMINL,1                                                         
         XC    HALF,HALF                                                        
         L     R1,FVADDR                                                        
         MVC   FVUNLD,RECVUL                                                    
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,FVIHDR),                      *        
               ('RECVMAXN',ASCANOUT),C',=  '                                    
         MVC   RECVCNDX,4(R1)                                                   
         CLI   RECVCNDX,0          TEST FOR SCANNER ERRORS                      
         BE    ROU1H                                                            
         ZIC   R1,RECVCNDX                                                      
         STH   R1,HALF             STORE # OF ENTRIES FROM SCANNER              
         L     R2,ASCANOUT                                                      
         USING SCANOUTD,R2         R2=A(SCANNER ENTRY)                          
         LA    R3,KEY                                                           
         USING ACTRECD,R3          R3=A(RECORD KEY)                             
         L     R4,ARECVTAB                                                      
         USING RECVTABD,R4         R4=A(RECEIVABLE TABLE ENTRY)                 
         B     VALRCV3                                                          
*                                                                               
* NOW VALIDATE 2ND RECEIVABLE A/C LINE                                          
*                                                                               
VALRCV2A OI    RECBIT,TWORCV       INDICATE USING 2ND RECEIVABLE LINE           
         MVI   RECVCNT,0           RESET ACCOUNT COUNTER                        
         XC    HALF,HALF                                                        
         L     R0,ASCANOUT         CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R1,SVADDR                                                        
         ST    R1,FVADDR                                                        
         MVC   FVUNLD,RECVUL                                                    
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BNE   VALRCV28                                                         
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,FVIHDR),                      *        
               ('RECVMAXN',ASCANOUT),C',=  '                                    
         MVC   RECVCNDX,4(R1)                                                   
         CLI   RECVCNDX,0          TEST FOR SCANNER ERRORS                      
         BE    VALRCV28            DONE                                         
         ZIC   R1,RECVCNDX                                                      
         STH   R1,HALF             STORE # OF ENTRIES FROM SCANNER              
         L     R2,ASCANOUT                                                      
         USING SCANOUTD,R2         R2=A(SCANNER ENTRY)                          
         LA    R3,KEY                                                           
         USING ACTRECD,R3          R3=A(RECORD KEY)                             
*                                                                               
VALRCV3  ZIC   RF,RECVCNT                                                       
         LA    RF,1(RF)                                                         
         CLM   RF,1,RECVCNDX                                                    
         BH    VALRCV16                                                         
         USING LDGRECD,R3                                                       
         MVC   LDGKEY,SPACES       GET RECEIVABLE LEDGER VALUES                 
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(L'RECVUL),RECVUL                                         
         GOTO1 AGETLDG                                                          
         BNE   ROU1H                                                            
         ICM   R1,15,RECLEDG       R1=A(LEDGER TABLE ENTRY)                     
         LA    R1,LEDGTLVD-LEDGTABD(R1)                                         
         LA    R0,4                R0=MAX NUMBER OF ACCOUNT LEVELS              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
VALRCV4  CLI   0(R1),0             TEST FOUND LOWEST LEVEL                      
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   R0,VALRCV4                                                       
         DC    H'0'                                                             
*                                                                               
         CH    R0,=H'1'            TEST 1 LEVEL LEDGER                          
         BE    VALRCV20                                                         
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         BCTR  R1,0                                                             
         BCTR  R0,0                                                             
         SR    RE,RE                                                            
         IC    RE,0(R1)            RE=NEXT LOWEST ACCOUNT LENGTH                
         SR    RF,RE                                                            
         BCTR  RF,0                RF=L'LOWEST ACCOUNT LEVEL-1                  
         LA    RE,SCANTXT1(RE)                                                  
         EX    RF,*+8              TEST LOW LEVEL ACCOUNT ENTERED               
         BNE   VALRCV20                                                         
         CLC   0(0,RE),SPACES                                                   
*                                                                               
         CH    R0,=H'1'            TEST 2 LEVEL LEDGER                          
         BE    VALRCV6                                                          
         IC    RF,0(R1)                                                         
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         SR    RF,RE                                                            
         BCTR  RF,0                RF=L'LOWEST ACCOUNT LEVEL-1                  
         LA    RE,SCANTXT1(RE)                                                  
         EX    RF,*+8              TEST LOW LEVEL ACCOUNT ENTERED               
         BE    GETACCIP                                                         
         CLC   0(0,RE),SPACES                                                   
*                                                                               
         USING ACTRECD,R3                                                       
VALRCV6  MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVUL),RECVUL                                         
         MVC   ACTKACT,SCANTXT1                                                 
         SR    RE,RE                                                            
         IC    RE,SCANLEN1                                                      
         LA    RE,2(RE)                                                         
         STC   RE,KEYWORK          SET LENGTH FOR KEY COMPARE & VALUE           
         MVC   KEYWORK+1(L'ACTKCULA),ACTKCULA                                   
         GOTO1 AIOHIGH             READ FOR THE ACCOUNT MAKE SURE VALID         
         BNE   VALRCV15                                                         
*        ZIC   R1,FVINDX           INCREMENT INDEX # FOR ERROR MESSAGES         
*        LA    R1,1(R1)                                                         
*        STC   R1,FVINDX                                                        
         L     RF,AIO1                                                          
         SR    RE,RE                                                            
         IC    RE,KEYWORK                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEYWORK+1(0),0(RF)                                               
         BNE   GETACCIA                                                         
                                                                                
VALRCV12 IC    RE,ACTKCULA+L'ACTKCULA-1                                         
         LA    RE,1(RE)                                                         
         STC   RE,ACTKCULA+L'ACTKCULA-1                                         
         GOTO1 AIOHIGH             GET NEXT ACCOUNT LEVEL                       
         BNE   VALRCV15                                                         
         L     RF,AIO1                                                          
         SR    RE,RE                                                            
         IC    RE,KEYWORK                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEYWORK+1(0),0(RF)                                               
         BNE   VALRCV15                                                         
         MVC   KEY,0(RF)                                                        
         GOTO1 AGETRCV                                                          
         BL    ROU1X               LEAVE WITH ERROR CODE INTACT                 
         BE    VALRCV13                                                         
         TM    RECSTAT,RSTSACIL    ACCT LOCKED?                                 
         BNO   VALRCV15            NO-LOOK FOR NEXT ACCT                        
         B     VALRCV14            YES-CONTINUE                                 
VALRCV13 MVC   RECVACT,ACTKCULA    BUILD TABLE ENTRY  CODE                      
         MVC   RECVACTN,RECNAME                       NAME                      
         MVC   RECVOFFC,RECOFFC                       OFFICE                    
         LA    R4,RECVTABL(R4)     BUMP TO NEXT TABLE ENTRY                     
         MVI   RECVTABD,RECVTEOT                                                
         ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
VALRCV14 CLI   BYTE,RECVMAXN            TOO MANY ACCOUNTS (17 MAX)              
         BNH   VALRCV12                                                         
         LA    R1,RECVTABL         LENGTH OF ONE ENTRY                          
         SR    R4,R1               POINT BACK TO ACCT THAT WON'T FIT            
         MVC   FVMSGNO,=AL2(EAACCMAX)                                           
         MVC   FVXTRA(L'RECVACT-1),RECVACT+1  ADD ACCOUNT CODE TO MSG           
         B     ROU1H                                                            
*                                                                               
VALRCV15 LA    R2,SCANOUTL(R2)     BUMP SCANNER LINE                            
         LH    R1,HALF             DECREMENT # OF ACCTS FROM SCANNER            
         SHI   R1,1                                                             
         STH   R1,HALF                                                          
         BNZ   VALRCV3                                                          
*                                                                               
VALRCV16 TM    RECBIT,TWORCV       DID WE ALREADY VALIDATE THE 2ND RCV?         
         BZ    VALRCV2A            NO                                           
         B     VALRCV28                                                         
*                                                                               
VALRCV20 OI    RECBIT,LOWLEV       USER ENTERED A LOW LEVEL ACCOUNT             
         ZIC   RF,RECVCNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,RECVCNT                                                       
*        ZIC   RF,FVINDX           INCREMENT INDEX # FOR MESSAGES               
*        LA    RF,1(RF)                                                         
*        STC   RF,FVINDX                                                        
         ZIC   RF,BYTE             KEEP NUMBER OF TABLE ENTRIES                 
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CLI   SCANLEN2,0                                                       
         BNE   ROU1H                                                            
         MVC   FVMSGNO,=AL2(EAACCMAX)                                           
         CLI   BYTE,RECVMAXN                                                    
         BH    ROU1H                                                            
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLI   SCANTXT1,C'*'       ASTERISK IN POSITION 1                       
         BNE   VALRCV24                                                         
         CLI   SCANLEN1,L'ACTKCULA                                              
         BH    ROU1H                                                            
         MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         CLI   SCANLEN1,4                                                       
         BL    ROU1H                                                            
         MVC   ACTKUNT(L'ACTKCULA-1),SCANTXT1+1                                 
         B     VALRCV26                                                         
*                                                                               
VALRCV24 CLI   SCANLEN1,L'ACTKACT                                               
         BNH   VALRCV25                                                         
         LA    RE,SCANTXT1+L'SCANTXT1-1  VERIFY THAT LEN IS WRONG               
         LA    R1,L'SCANTXT1                                                    
         CLI   0(RE),X'40'                                                      
         BH    VALRC24A                                                         
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
         DC    H'0'                                                             
VALRC24A CHI   R1,L'ACTKACT                                                     
         BH    ROU1H                                                            
         STC   R1,SCANLEN1         FIX LENGTH                                   
VALRCV25 MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVUL),RECVUL                                         
         SR    R1,R1                                                            
         IC    R1,SCANLEN1                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),SCANTXT1                                              
*                                                                               
VALRCV26 GOTO1 AGETRCV                                                          
         BNE   ROU1X               LEAVE CONDITION CODE INTACT ON ERROR         
*                                                                               
* DON'T FILL IN THE INDEX NUMBER (RECVNDX) UNTIL PROCESSED EVERYTHING           
*        MVC   RECVNDX,RECVCNT     RECEIVABLE ACCOUNT INDEX NUMBER              
         NI    RECBIT,X'FF'-LOWLEV                                              
         MVC   RECVACT,ACTKCULA                       CODE                      
         MVC   RECVACTN,RECNAME                       NAME                      
         MVC   RECVOFFC,RECOFFC                       OFFICE                    
         LA    R4,RECVTABL(R4)     BUMP TO NEXT TABLE ENTRY                     
         MVI   RECVTABD,RECVTEOT                                                
         LA    R2,SCANOUTL(R2)     BUMP TO NEXT SCANNER ENTRY                   
         LH    R1,HALF             DECREMENT # OF ACCTS FROM SCANNER            
         SHI   R1,1                                                             
         STH   R1,HALF                                                          
         BNZ   VALRCV3                                                          
         B     VALRCV16            NO - VALIDATE NEXT LINE                      
*                                                                               
VALRCV28 DS    0H                                                               
         L     R4,ARECVTAB          NOW POINT TO BEGINNING OF TABLE             
         ZIC   RF,BYTE             AND FILL IN THE INDEX NUMBERS                
         LTR   RF,RF               NO ENTRIES IN TABLE-GIVE ERROR               
         BZ    GETACCIP                                                         
         LA    R1,1                BYTE=THE NUMBER OF ENTRIES                   
         STC   R1,0(R4)            RECVNDX                                      
         ZAP   RECVTOT,PZERO                                                    
         LA    R1,1(R1)                                                         
         LA    R4,RECVTABL(R4)                                                  
         BCT   RF,*-18                                                          
*                                                                               
VALRCV29 XC    WORK,WORK           ESTABLISH PROFILE                            
         MVI   WORK+00,C'A'-X'40'  LOWER CASE 'A' SYSTEM                        
*&&UK*&& MVC   WORK+01(3),=C'DEB'                                               
*&&US*&& MVC   WORK+01(3),=C'RCV'                                               
         MVC   WORK+05(2),RECVCACT+(ACTKUNT-ACTKEY)                             
         MVC   WORK+12(2),TWAAGY                                                
         CLI   BYTE,1              GET LEDGER PROFILE IF >1 ACCOUNT             
         BNE   VALRCV32                                                         
         SR    R1,R1               ASSUME CLIENT AT ACTKACT                     
         CLC   RECVUL,RECVCACT+(ACTKUNT-ACTKEY)                                 
         BNE   VALRCV31                                                         
         CLI   TWASRCLI,1          TEST CLIENT SPECIFIED OR LEVEL 1             
         BNH   VALRCV31                                                         
         IC    R1,TWASRCLI                                                      
         BCTR  R1,0                                                             
VALRCV31 LA    R1,RECVCACT+(ACTKACT-ACTKEY)(R1)                                 
         MVC   WORK+07(3),0(R1)    EXTRACT THREE CHARACTERS FOR CLIENT          
VALRCV32 GOTO1 VGETPROF,DMCB,WORK,PROFILE,VDATAMGR                              
         MVI   WORK+03,C'2'        GET PROFILE PAGE 2                           
         GOTO1 (RF),(R1),,PROFIL2                                               
         CLI   PROFAUTM,0          TEST AUTOMATIC MEMO PROFILE SET              
         BNE   *+8                                                              
         MVI   PROFAUTM,C'Y'       NO - SET DEFAULT TO YES                      
         CLI   PROFCFRM,0          TEST CONFIRMATION PROFILE SET                
         BNE   *+8                                                              
         MVI   PROFCFRM,PROFBOTH   NO - SET DEFAULT TO BOTH ACTIONS             
*                                                                               
         OC    PROFSEQ,PROFSEQ     TEST KEY SEQUENCE SET                        
         BNZ   VALRCV36                                                         
VALRCV34 MVC   PROFSEQ,PROFDFS1    TAKE DEFAULT FROM PROGRAM                    
         CLI   RECVCNT,1                                                        
         BE    VALRCV36                                                         
         MVC   PROFSEQ,PROFDFS2                                                 
VALRCV36 XC    KEYDISP(KEYDISPL),KEYDISP                                        
         MVI   WORK,0                                                           
         LA    R1,PROFSEQ          R1=A(KEY PROFILE)                            
         LA    R0,L'PROFSEQ                                                     
         SR    RE,RE               RE=DISPLACEMENT TO KEY VALUE                 
VALRCV38 SR    R2,R2                                                            
         IC    R2,0(R1)            R2=KEY ELEMENT NUMBER                        
         LR    RF,R2                                                            
         LA    RF,KEYDISP-1(RF)    RF=A(KEY ELEMENT DISPLACEMENT)               
         BCTR  R2,0                                                             
         SLL   R2,1                KEYNUM-1*2                                   
         A     R2,AKEYTAB                                                       
         USING KEYTABD,R2          R2=A(KEY ELEMENT TABLE ENTRY)                
         MVC   WORK+L'KEYMASK(L'KEYMASK),KEYMASK                                
         NC    WORK+L'KEYMASK(L'KEYMASK),WORK                                   
         BNZ   VALRCV34                                                         
         OC    WORK(L'KEYMASK),KEYMASK                                          
         STC   RE,0(RF)                                                         
         SR    RF,RF                                                            
         IC    RF,KEYLEN                                                        
         AR    RE,RF               RE=DISPLACEMENT OF NEXT ELEMENT              
         LA    R1,1(R1)                                                         
         BCT   R0,VALRCV38                                                      
*                                                                               
         CLI   PROFTOT,0           TEST TOTALS PROFILE SET                      
         BNE   *+10                                                             
         MVC   PROFTOT,PROFDFTC    NO - SET DEFAULT VALUE                       
         MVC   TOTPROF,PROFTOT                                                  
         NI    TOTPROF,PROFTBAL+PROFTCHQ+PROFTMRK                               
VALRCVX  MVC   FVXTRA,SPACES                                                    
         MVC   RECVCNT,BYTE        FILL IN RECVCNT WITH TRUE # OF ACCTS         
         B     ROU1E                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK FOR COMMA AT END OF RECEIVABLE ACCOUNT FIELD                  *         
* NTRY R2 POINTS TO RECEIVABLE ACCOUNT FIELD                                    
***********************************************************************         
*                                                                               
CHKCOMMA ZIC   RF,5(R2)            RF=INPUT LENGTH                              
         LR    R0,R2               SAVE R2 FOR LATER                            
         LA    R2,8(R2)            BUMP TO INPUT FIELD                          
         AR    R2,RF               BUMP TO END OF INPUT                         
         BCTR  R2,0                BACK UP ONE                                  
         CLI   0(R2),C','                                                       
         BNE   CHKCOMMX                                                         
         MVI   0(R2),X'00'                                                      
         BCTR  RF,0                                                             
         LR    R2,R0                                                            
         STC   RF,5(R2)            FIX THE LENGTH                               
CHKCOMMX BR    RE                                                               
***********************************************************************         
* GET A RECEIVABLE ACCOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
GETRCV   LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         SR    R1,R1                                                            
         CLC   ACTKUNT(L'RECVUL),RECVUL                                         
         BE    *+8                                                              
         LA    R1,RECVLIST                                                      
         GOTO1 AGETACC             GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1X                                                            
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         TM    RECSTAT,RSTSACIC+RSTSACIL                                        
         BNZ   ROU1H                                                            
         TM    RECINDS,RECIBAL                                                  
         BZ    ROU1H                                                            
         TM    RECBIT,LOWLEV       USER ENTERED A LOW LVL ACCT?                 
         BO    *+16                RECVCNT WILL BE DIFFERENT                    
         CLI   RECVCNT,0           WHEN ENTER A LOW LVL ACCOUNT VS              
         BNE   GETRCV2             A HIGHER LEVEL ACCOUNT                       
         B     GETRCV1                                                          
         CLI   RECVCNT,1                                                        
         BNE   GETRCV2                                                          
GETRCV1  L     R1,AIO1                                                          
         MVC   RECVCACT,0(R1)      SAVE RECEIVING ACCOUNT                       
         MVC   RECVOFF,RECOFFC     SAVE RECEIVING ACCOUNT OFFICE                
*                                                                               
GETRCV2  DS    0H                  TEST MIXED OFFICES                           
*&&UK                                                                           
         CLC   RECVOFF,RECOFFC                                                  
         BE    GETRCV4                                                          
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    GETRCV4                                                          
         MVC   FVMSGNO,=AL2(EAOFFMIX)                                           
         B     ROU1L                                                            
*&&                                                                             
GETRCV4  DS    0H                  CHECK POSTING TO INTERAGENCY SR A/C          
*&&US                                                                           
         CLC   RECVUL,ACTKUNT      TEST FOR SR ACCOUNT                          
         BNE   GETRCV8             NO-SKIP CHECK                                
*                                                                               
*MN      MVC   KEYSAVE,KEY         SAVE SR ACCOUNT KEY                          
*MN      L     R3,AIO1             POINT TO SR RECORD                           
*MN      LA    R4,KEY                                                           
*MN      USING INTRECD,R4                                                       
*MN      XC    INTKEY,INTKEY       BUILD INTERAGENCY ESTIMATE KEY               
*MN      MVI   INTKTYP,INTKTYPQ                                                 
*MN      MVI   INTKSUB,INTKSUBQ                                                 
*MN      MVC   INTKCULA,ACTKCULA   SR ACCOUNT                                   
*MN      GOTO1 VDATAMGR,DMCB,(0,DMRDHI),ACCFIL,INTRECD,AIO2,0                   
*MN      L     R4,AIO2                                                          
*MN      CLC   INTKEY(INTKCLT-INTKEY),KEY  TEST IF ANY ESTIMATES THERE          
*MN      BNE   GETRCV6             NO                                           
*MN                                                                             
*MN      MVC   FVMSGNO,=AL2(EAINTACC)                                           
*MN      MVC   FVXTRA,SPACES                                                    
*MN      MVC   FVXTRA(L'INTKACT),INTKACT                                        
*MN      B     ROU1L                                                            
*MN                                                                             
GETRCV6  LA    R3,KEY              RESTORE ADDRESSABILITY TO SR KEY             
*MN      MVC   KEY,KEYSAVE         RESTORE SR KEY                               
*MN      DROP  R4                                                               
*&&                                                                             
GETRCV8  DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,BYTE             # OF TABLE ENTRIES SO FAR                    
*        SH    RF,=H'1'                                                         
         AHI   RF,1                INCREMENT BY ONE TO GET THE REAL #           
         BNP   GETRCVX                                                          
         CHI   RF,2                                                             
         BL    GETRCVX                                                          
         LHI   RF,RECVMAXN         CHECK FOR MAX # OF ENTRIES                   
         MVC   FVMSGNO,=AL2(EAACCDUP)                                           
         L     R1,ARECVTAB          PREPARE TABLE OF RECEIVING ACCOUNTS         
         ST    R1,ARCVTAB          SET START ADDRESS                            
*                                                                               
         USING RECVTABD,R1                                                      
         USING LEDGTABD,RE                                                      
* THE FOLLOWING CODE WAS NEVER EXECUTED BECAUSE CLILEV WAS NEVER                
* TURNED ON                                                                     
*ETRCV10 TM    RECBIT,CLILEV       CHECK UP TO CLT LEVEL ONLY?                  
*        BZ    GETRCV12            NO CHECK UP TO PRODUCT LEVEL                 
*        L     RE,RECLEDG          POINT TO SR LEDGER ENTRY                     
*        ZIC   R5,LEDGTLVD                                                      
*        ZIC   R4,LEDGTLVC                                                      
*        SR    R5,R4                                                            
*        BCTR  R5,0                                                             
*        BNP   ROU1L                                                            
*ETRCV11 EXCLC R5,RECVACT+1,ACTKUNT   COMPARE U/L/ACCOUNT                       
*        BE    ROU1L                                                            
*        LA    R1,RECVTABL(R1)                                                  
*        BCT   RF,GETRCV11                                                      
*        B     GETRCVX                                                          
*                                                                               
GETRCV12 CLC   RECVACT+1(L'RECVACT-1),ACTKUNT    COMPARE U/L/ACCOUNT            
         BE    ROU1L                                                            
         LA    R1,RECVTABL(R1)                                                  
         BCT   RF,GETRCV12                                                      
*                                                                               
GETRCVX  B     ROU1E                                                            
         DROP  R1,R3,RE                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF CLIENT/PRODUCT                                   *         
***********************************************************************         
         SPACE 1                                                                
VALWCP   MVI   WCPCNT,0            RESET SAVE STORAGE VALUES                    
         L     RF,ARECVTAB                                                      
         USING RECVTABD,RF         RF=A(RECEIVABLE TABLE)                       
VALWCP02 CLI   RECVTABD,RECVTEOT   TEST E-O-T                                   
         BE    VALWCP04                                                         
         XC    RECVPCPV(RECVPCPL),RECVPCPV                                      
         LA    RF,RECVTABL(RF)     BUMP TO NEXT TABLE ENTRY                     
         B     VALWCP02                                                         
         DROP  RF                                                               
*                                                                               
VALWCP04 DS    0H                                                               
         MVC   FVUNLD,PRODUL                                                    
         MVI   FVMAMI,X'22'        PRODUCT LEVEL ONLY                           
         GOTO1 AFVAL               VALIDATE INPUT FIELD                         
         BNE   ROU1X                                                            
         L     R0,ASCANOUT         CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,FVIHDR),                      *        
               ('RECVMAXN',ASCANOUT),C',=  '                                    
         MVC   WCPCNT,4(R1)                                                     
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CLI   WCPCNT,0                                                         
         BE    ROU1H                                                            
         MVI   FVINDX,1                                                         
         CLI   WCPCNT,1                                                         
         BE    VALWCP06                                                         
         MVC   FVMSGNO,=AL2(EAACCMAX)                                           
         CLC   WCPCNT,RECVCNT                                                   
         BH    ROU1H                                                            
         MVC   FVMSGNO,=AL2(EAWOFMIN)                                           
         BL    ROU1H                                                            
VALWCP06 L     R2,ASCANOUT                                                      
         USING SCANOUTD,R2                                                      
         L     R4,ARECVTAB                                                      
         USING RECVTABD,R4                                                      
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
*                                                                               
VALWCP08 SR    RE,RE                                                            
         CLI   SCANLEN1,1          LENGTH OF ENTRY                              
         BNE   VALWCP10                                                         
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CLI   SCANTXT1,C'*'       ASTERISK IN POSITION 1                       
         BNE   ROU1H                                                            
         IC    RE,PRODALEN         L'CLI IN PRODUCTION LEDGER                   
         LA    R1,RECVACT+3(RE)    GET TO PRODUCT IN KEY                        
         SR    RF,RF                                                            
         IC    RF,PRODBLEN         L'CLI+PRO IN PRODUCTION LEDGER               
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         EXCLC RF,0(R1),SPACES                                                  
         BNH   ROU1H                                                            
         IC    RE,PRODBLEN         L'CLI+PRO IN PRODUCTION LEDGER               
         LA    R1,RECVACT+3        TAKE RECEIVABLE CODE AS DEFAULT              
         B     VALWCP12                                                         
*                                                                               
VALWCP10 IC    RE,SCANLEN1         TEST FOR PRODUCT LEVEL                       
         LA    R1,SCANTXT1                                                      
         MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         CLM   RE,1,PRODALEN                                                    
         BNH   ROU1H                                                            
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLM   RE,1,PRODBLEN                                                    
         BH    ROU1H                                                            
*                                                                               
VALWCP12 MVC   ACTKACT,SPACES                                                   
         BCTR  RE,0                                                             
         MVC   ACTKACT(0),0(R1)                                                 
         EX    RE,*-6                                                           
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'ACTKCULA-1),ACTKUNT                                     
         MVC   FVMSGNO,=AL2(EGRECNOF)                                           
         GOTO1 AIOREAD                                                          
         BNE   ROU1H                                                            
         GOTO1 AACCELS,RECSTATQ+RECNAMEQ                                        
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         TM    RECSTAT,RSTSACIC+RSTSACIL                                        
         BNZ   ROU1H               ERROR ON EITHER OF THESE                     
         MVC   RECVPCP,ACTKCULA    PRODUCTION CLI/PRO                           
         MVC   RECVPCPN,RECNAME                                                 
         ICM   R1,15,RECPROF       EXTRACT COSTING ACCOUNT FROM PROFILE         
         BZ    VALWCP14                                                         
         USING PPRELD,R1                                                        
         OC    PPRCOST,PPRCOST     TEST COSTING ACCOUNT SET                     
         BZ    VALWCP14                                                         
         MVC   RECVCOST,PPRCOSTU                                                
         B     VALWCP16                                                         
*                                                                               
VALWCP14 MVC   KEYSAVE,ACTKEY                                                   
         MVC   ACTKACT,SPACES                                                   
         SR    RE,RE                                                            
         IC    RE,PRODALEN                                                      
         BCTR  RE,0                                                             
         MVC   ACTKACT(0),KEYSAVE+(ACTKACT-ACTKEY)                              
         EX    RE,*-6                                                           
         CLC   ACTKEY(ACTKEND),KEYSAVE                                          
         BE    VALWCP16                                                         
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'ACTKCULA-1),ACTKUNT                                     
         MVC   FVMSGNO,=AL2(EGRECNOF)                                           
         GOTO1 AIOREAD                                                          
         BNE   ROU1H                                                            
         GOTO1 AACCELS,RECSTATQ+RECNAMEQ                                        
         ICM   R1,15,RECPROF       EXTRACT COSTING ACCOUNT FROM PROFILE         
         BZ    VALWCP16                                                         
         USING PPRELD,R1                                                        
         OC    PPRCOST,PPRCOST     TEST COSTING ACCOUNT SET                     
         BZ    VALWCP16                                                         
         MVC   RECVCOST,PPRCOSTU                                                
         DROP  R1                                                               
*                                                                               
VALWCP16 MVC   FVXTRA,SPACES                                                    
         CLI   WCPCNT,1            ONE ONLY - SPECIAL TREATMENT                 
         BE    VALWCP22                                                         
         LA    R4,RECVTABL(R4)                                                  
         CLI   RECVTABD,RECVTEOT   TEST EOT                                     
         BE    VALWCP18                                                         
         LA    R2,SCANOUTL(R2)                                                  
         IC    R1,FVINDX                                                        
         LA    R1,1(R1)                                                         
         STC   R1,FVINDX                                                        
         B     VALWCP08                                                         
*                                                                               
VALWCP18 BAS   RE,CLRFLD           CLEAR THE INPUT FIELD                        
         LR    R0,R1               R0=L'FIELD-1                                 
         SR    RF,RF                                                            
         IC    RF,WCPCNT           EFFECTIVELY WCPCNT-1, SEE ABOVE              
         SR    R0,RF               R0=L'FIELD-(WCPCNT-1)                        
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         L     R1,FVADDR                                                        
         LA    R1,L'FVIHDR-1(R1)   POINT TO BYTE AHEAD OF DESTINATION           
         L     RE,ARECVTAB                                                      
         LA    RE,(RECVPCP+2)-RECVTABD(RE)                                      
VALWCP20 LA    RF,L'RECVPCP-3(RE)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE               RF=SIGNIFICANT LENGTH                        
         SR    R0,RF               CHECK ENOUGH SPACE IN LINE                   
         BM    ROU1H                                                            
         BCTR  RF,0                                                             
         MVC   1(0,R1),1(RE)       0(RE)=LEDGER BYTE, REMEMBER                  
         EX    RF,*-6                                                           
         LA    RE,RECVTABL(RE)                                                  
         CLI   0(RE),EOT           ZERO MEANS E-O-T                             
         BE    VALWCPX                                                          
         LA    R1,2(RF,R1)                                                      
         MVC   0(1,R1),DELIM                                                    
         B     VALWCP20                                                         
*                                                                               
VALWCP22 BAS   RE,CLRFLD           SINGLE RECEIVABLE                            
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(PRODBLEN,0),                 X        
               (X'C0',RECNDSP),RECVPCP,(L'RECVPCPN,RECVPCPN)                    
         L     R1,ARECVTAB          PROPAGATE RECEIVABLE VALUES                 
         LR    R4,R1                                                            
VALWCP24 LA    R4,RECVTABL(R4)     R4=A(NEXT TABLE ENTRY)                       
         CLI   RECVTABD,RECVTEOT   TEST EOT                                     
         BE    VALWCPX                                                          
         MVC   RECVPCP,RECVPCP-RECVTABD(R1)                                     
         MVC   RECVPCPN,RECVPCPN-RECVTABD(R1)                                   
         MVC   RECVCOST,RECVCOST-RECVTABD(R1)                                   
         B     VALWCP24                                                         
*                                                                               
VALWCPX  MVI   FVINDX,0                                                         
         B     ROU1E                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALBNK   XC    BANK,BANK                                                        
         MVC   FVUNLD,BANKUL                                                    
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BE    *+12                                                             
         OI    TWAMODE2,TWA2NALL   SET NO ALLOCATION                            
         B     ROU1X                                                            
         MVC   BANK(L'BANKUL),BANKUL                                            
         LA    RE,BANK+L'BANKUL                                                 
         LA    R1,L'BANK-L'BANKUL-1                                             
         LA    RF,FVIFLD                                                        
         CLI   FVIFLD,C'*'         TEST USER OVERRIDING UNIT/LEDGER             
         BNE   *+16                                                             
         LA    RE,BANK                                                          
         LA    R1,L'BANK-1                                                      
         LA    RF,FVIFLD+1                                                      
         MVC   0(0,RE),0(RF)       SET BANK (UNIT/LEDGER/)ACCOUNT               
         EX    R1,*-6                                                           
         LA    R5,KEY                                                           
         USING ACTRECD,R5          BUILD KEY OF BANK ACCOUNT                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'BANK),BANK                                             
         SR    R1,R1                                                            
         CLC   ACTKUNT(L'BANKUL),BANKUL                                         
         BE    *+8                                                              
         LA    R1,BANKLIST                                                      
         GOTO1 AGETACC             GET ACCOUNT, TEST SECURITY ETC.              
         BE    *+12                                                             
         OI    TWAMODE2,TWA2NALL   SET NO ALLOCATION                            
         B     ROU1H                                                            
         MVC   BANKNAME,RECNAME                                                 
         MVC   BANKOFF,RECOFFC                                                  
         MVC   BANKCUR,RECCURR                                                  
VALBNKX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ANALYSIS BANK ACCOUNT                                      *         
***********************************************************************         
         SPACE 1                                                                
VALANL   XC    ANAL,ANAL                                                        
         MVC   FVUNLD,ANALUL                                                    
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   ANAL(L'ANALUL),ANALUL                                            
         LA    RE,ANAL+L'ANALUL                                                 
         LA    R1,L'ANAL-L'ANALUL-1                                             
         LA    RF,FVIFLD                                                        
         CLI   FVIFLD,C'*'         TEST USER OVERRIDING UNIT/LEDGER             
         BNE   *+16                                                             
         LA    RE,ANAL                                                          
         LA    R1,L'ANAL-1                                                      
         LA    RF,FVIFLD+1                                                      
         MVC   0(0,RE),0(RF)       SET ANAL BANK (UNIT/LEDGER/)ACCOUNT          
         EX    R1,*-6                                                           
         LA    R5,KEY                                                           
         USING ACTRECD,R5          BUILD KEY OF ANAL BANK ACCOUNT               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'ANAL),ANAL                                             
         SR    R1,R1                                                            
         CLC   ACTKUNT(L'ANALUL),ANALUL                                         
         BE    *+8                                                              
         LA    R1,ANALLIST                                                      
         GOTO1 AGETACC             GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         MVC   ANALNAME,RECNAME                                                 
VALANLX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE CHEQUE WAS DEPOSITED                                  *         
***********************************************************************         
         SPACE 1                                                                
VALDEP   XC    BANKDATE,BANKDATE                                                
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ROU1H                                                            
         LA    R2,WORK                                                          
         USING PERVALD,R2                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    VALDEP2                                                          
         MVC   FVMSGNO,=AL2(EADTUNSP)                                           
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    ROU1H               ERROR IF ONLY YEAR INPUT                     
*                                                                               
VALDEP2  DS    0H                                                               
         GOTO1 VDATCON,DMCB,(1,TODAYP),(0,TEMP)                                 
         GOTO1 VADDAY,DMCB,TEMP,TEMP+6,7                                        
         MVC   FVMSGNO,=AL2(AE$DTFIF)                                           
         CLC   PVALESTA,TEMP+6     CAN'T BE HIGHER THAN TODAY+7 DAYS            
         BH    ROU1H                                                            
*                                                                               
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALDEP8                                                          
*                                                                               
* UK RULE:  7 DAYS INTO THE FUTURE AND 12 MONTHS INTO THE PAST                  
*           EG. IF TODAY IS 8/31/99,THE EARLIEST DATE YOU CAN ENTER IS          
*           8/01/98. (THIS IS DIFFERENT THAN THE US RULE)                       
*                                                                               
*&&UK                                                                           
         SR    RE,RE                                                            
         ICM   RE,1,TODAYB                                                      
         BNZ   *+8                                                              
         LA    RE,100              YEAR 2000 IS 100 RELATIVE                    
         MH    RE,=H'12'                                                        
         SR    RF,RF                                                            
         IC    RF,TODAYB+1                                                      
         AR    RE,RF               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'6'                                                         
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         BNZ   *+8                                                              
         LA    RE,100                                                           
         MH    RE,=H'12'                                                        
         IC    RF,PVALBSTA+1                                                    
         AR    RE,RF               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN NN MONTHS AGO                 
         MVC   FVMSGNO,=AL2(EADTPAST)                                           
         BL    ROU1H                                                            
*&&                                                                             
*                                                                               
* US RULE:  7 DAYS INTO THE FUTURE AND ONE YEAR IN THE PAST.  EG. IF            
*           TODAY IS 8/31/99, THE EARLIEST DTE YOU CAN ENTER IS 8/31/98         
*           (CAN OVERRIDE THE ONE YEAR BACK RULE WITH THE CONTROL REC)          
*                                                                               
*&&US                                                                           
         MVC   FVMSGNO,=AL2(EADTPAST) DATE TOO FAR IN THE PAST                  
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)    GET TODAY'S DATE                  
         GOTO1 VADDAY,DMCB,(C'Y',TEMP),TEMP+10,F'-1' SUBTRACT 1 YEAR            
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BL    ROU1H                                                            
*&&                                                                             
*                                                                               
VALDEP8  MVC   BANKDATE,PVALESTA   EBCDIC START DATE                            
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         GOTO1 VDATCON,DMCB,(1,PVALPSTA),(17,(RF))                              
VALDEPX  B     ROU1E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHEQUE DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALCDT   XC    CHQDATE,CHQDATE                                                  
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ROU1H                                                            
         LA    R2,WORK                                                          
         USING PERVALD,R2                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    VALCDT2                                                          
         MVC   FVMSGNO,=AL2(EADTUNSP)                                           
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    ROU1H               ERROR IF ONLY YEAR INPUT                     
*                                                                               
VALCDT2  DS    0H                                                               
         MVC   FVMSGNO,=AL2(EADTFUTR)   DATE TOO FAR IN THE FUTURE              
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)    GET TODAY'S DATE                  
         GOTO1 VADDAY,DMCB,(C'D',TEMP),TEMP+10,F'7' ADD 7 DAYS                  
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BH    ROU1H                                                            
*                                                                               
* UK RULE:  7 DAYS INTO THE FUTURE AND 6 MONTHS IN THE PAST                     
*                                                                               
*&&UK                                                                           
         GOTO1 VDATCON,DMCB,(1,TODAYP),(0,TEMP)                                 
         GOTO1 VADDAY,DMCB,TEMP,TEMP+6,7                                        
         MVC   FVMSGNO,=AL2(AE$DTFIF)                                           
         CLC   PVALESTA,TEMP+6     CAN'T BE HIGHER THAN TODAY+7 DAYS            
         BH    ROU1H                                                            
         SR    RE,RE                                                            
         ICM   RE,1,TODAYB                                                      
         BNZ   *+8                                                              
         LA    RE,100              YEAR 2000 IS 100 RELATIVE                    
         MH    RE,=H'12'                                                        
         SR    RF,RF                                                            
         IC    RF,TODAYB+1                                                      
         AR    RE,RF               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'6'                                                         
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         BNZ   *+8                                                              
         LA    RE,100                                                           
         MH    RE,=H'12'                                                        
         IC    RF,PVALBSTA+1                                                    
         AR    RE,RF               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN 6 MONTHS AGO                  
         MVC   FVMSGNO,=AL2(EADTPAST)                                           
         BL    ROU1H                                                            
*&&                                                                             
*                                                                               
* US RULE:  7 DAYS IN THE FUTURE AND 1 YEAR IN THE PAST (CAN OVERRIDE           
* THE 1 YEAR BACK RULE WITH THE CONTROL RECORD)                                 
*                                                                               
*&&US                                                                           
*                                                                               
         TM    COMPSTA4,CPYSOV12   TEST IF OVERRIDE RULES IS SET TO YES         
         BNZ   VALCDT10            YES, THEN DONE                               
         MVC   FVMSGNO,=AL2(EADTPAST) DATE TOO FAR IN THE PAST                  
         XC    TEMP,TEMP                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP)    GET TODAY'S DATE                  
         GOTO1 VADDAY,DMCB,(C'Y',TEMP),TEMP+10,F'-1' SUBTRACT 1 YEAR            
         LA    RF,WORK             POINT BACK TO PERVAL BLOCK                   
         CLC   PVALESTA,TEMP+10                                                 
         BL    ROU1H                                                            
*&&                                                                             
VALCDT10 MVC   CHQDATE,PVALESTA    EBCDIC START DATE                            
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         GOTO1 VDATCON,DMCB,(1,PVALPSTA),(17,(RF))                              
VALCDTX  B     ROU1E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INTERCOMPANY ACCOUNT FOR INTER-OFFICE POSTINGS             *         
***********************************************************************         
         SPACE 1                                                                
VALICO   XC    ICPY,ICPY                                                        
         ST    R1,FVADDR           SET FIELD ADDRESS FOR ERROR                  
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    VALICO4                                                          
         TM    COMPSTA4,CPYSICPY   TEST INTERCOMPANY POSTINGS?                  
         BZ    VALICO4                                                          
         CLC   RECVOFF,BANKOFF     TEST RECEIVABLE VS BANK OFFICE               
         BE    VALICO4                                                          
         MVI   FVMINL,1                                                         
         MVC   FVUNLD,=AL2(FFFF)                                                
         GOTO1 AFVAL                                                            
         BNE   ROU1X               LOW MEANS NO INPUT - OK                      
         MVC   ICPY,FVIFLD                                                      
*                                                                               
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'ICPY),ICPY                                             
         GOTO1 AGETACC,ICPYLIST    GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         L     R1,AIO1                                                          
         MVC   ICPY,ACTKUNT-ACTKEY(R1)                                          
         MVC   ICPYNAME,RECNAME                                                 
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   RECVOFF,RECOFFC     MATCH ICPY TO RECEIVABLE OFFICE              
         BNE   ROU1H                                                            
         MVC   FVMSGNO,=AL2(EAPTRMIS)                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,RECAPTR       LOCATE INTERCOMPANY POINTER                  
         BZ    ROU1H                                                            
         USING APTELD,R1                                                        
VALICO2  CLI   APTEL,0             TEST E-O-R                                   
         BE    ROU1H                                                            
         CLI   APTEL,APTELQ        TEST POINTER ELEMENT                         
         BNE   *+12                                                             
         TM    APTSTAT,APTSINTL    TEST INTERCOMPANY POINTER                    
         BNZ   *+14                                                             
         IC    R0,APTLN                                                         
         AR    R1,R0                                                            
         B     VALICO2                                                          
         MVC   ICP2,APTACCU        EXTRACT INTERCOMPANY 2 ACCOUNT               
         DROP  R1                                                               
*                                                                               
         MVC   ACTKEY,SPACES       READ POINTER RECORD                          
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'ICP2),ICP2                                             
         GOTO1 AGETACC,0           GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         MVC   ICP2NAME,RECNAME                                                 
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   BANKOFF,RECOFFC     MATCH OFFICE TO BANK OFFICE CODE             
         BNE   ROU1H                                                            
         B     VALICOX                                                          
*                                                                               
VALICO4  MVC   FVMSGNO,=AL2(EAINTINV)                                           
         CLI   FVILEN-FVIHDR(R1),0 INPUT IN INTERCOMPANY IS INVALID             
         BNE   ROU1H                                                            
*                                                                               
VALICOX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF ACCOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALWAC   XC    WOFFVALS(WOFFVALL),WOFFVALS                                      
         ZAP   WOFFAMT,PZERO                                                    
         ZAP   WOFFAM2,PZERO                                                    
         NI    TOTPROF,255-PROFTWOF                                             
         MVC   FVUNLD,=AL2(FFFF)                                                
         GOTO1 AFVAL                                                            
         BNE   ROU1X               NO INPUT IS OK                               
         MVC   WOFF,FVIFLD                                                      
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'WOFF),WOFF                                             
         GOTO1 AGETACC,WOFFLIST    GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         L     R1,AIO1                                                          
         MVC   WOFF,ACTKUNT-ACTKEY(R1)                                          
         MVC   WOFFNAME,RECNAME                                                 
         MVC   WOFFOFF,RECOFFC                                                  
         MVC   WOFFCOST,RECCSTG                                                 
         MVC   WOFFSTAT,RECSTAT                                                 
         MVC   WOFFANAL,RECANAL    EXTRACT ANALYSIS A/C FROM SPACEL             
         TM    PROFTOT,PROFTWOF    TEST WRITE-OFF TOTAL WANTED                  
         BZ    *+8                                                              
         OI    TOTPROF,PROFTWOF    YES - SET TOTAL ON                           
VALWACX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE RANGE FILTERS                                         *         
*                                                                     *         
* NOTE - LENGTH OF DATE FIELD MUST BE THE SAME AS L'PVALCPER          *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   XC    DATSTA,DATSTA                                                    
         MVI   DATEND,X'FF'                                                     
         MVC   DATEND+1(L'DATEND-1),DATEND                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(AGYLANG,WORK)                      
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   ROU1H                                                            
         BAS   RE,CLRFLD                                                        
         LA    R2,WORK                                                          
         USING PERVALD,R2                                                       
         TM    PVALASSM,STARTASS   START ENTIRELY ASSUMED?                      
         BO    VALDAT4             YES - JUST TAKE PERVAL END DATE              
         MVC   DATSTA,PVALPSTA     TRANSACTION START DATE FOR FILTERING         
         TM    PVALASSM,PVALASD    START DAY ASSUMED?                           
         BO    *+12                                                             
         TM    PVALASSM,ENDASS     IF NOT, WAS END ENTIRELY ASSUMED?            
         BO    VALDAT2                                                          
         L     RE,FVADDR                                                        
         MVC   L'FVIHDR(L'PVALCPER,RE),PVALCPER                                 
         B     VALDAT8                                                          
*                                                                               
VALDAT2  LA    RF,PVALCPER         TAKE PERVAL START DATE                       
         LA    R1,L'PVALCPER-1(RF)                                              
         CLI   0(R1),C'-'                                                       
         BE    *+10                                                             
         BCT   R1,*-8                                                           
         DC    H'0'                BAD DATE FROM PERVAL                         
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         B     VALDAT6                                                          
*                                                                               
VALDAT4  LA    RF,PVALCPER+(L'PVALCPER-1)                                       
         LR    R1,RF                                                            
         CLI   0(RF),C'-'                                                       
         BE    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF                                                            
*                                                                               
VALDAT6  L     RE,FVADDR                                                        
         MVC   L'FVIHDR(0,RE),0(RF)                                             
         EX    R1,*-6                                                           
*                                                                               
VALDAT8  MVC   DATEND,PVALPEND     TRANSACTION END DATE FOR FILTERING           
VALDATX  B     ROU1E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOS RANGE FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALMOS   XC    MOSSTA,MOSSTA                                                    
         MVI   MOSEND,X'FF'                                                     
         MVC   MOSEND+1(L'MOSEND-1),MOSEND                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(AGYLANG,WORK)                      
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   ROU1H                                                            
         BAS   RE,CLRFLD                                                        
         LA    R3,L'FVIHDR(RF)                                                  
         LA    R2,WORK                                                          
         USING PERVALD,R2                                                       
         TM    PVALASSM,STARTASS   START ENTIRELY ASSUMED?                      
         BO    VALMOS02            YES - JUST TAKE PERVAL END DATE              
         MVC   MOSSTA,PVALPSTA     MOS RANGE START FOR FILTERING                
         GOTO1 VDATCON,DMCB,(1,PVALPSTA),(9,(R3))                               
         LA    R3,7(R3)                                                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,1(R3)                                                         
VALMOS02 MVI   0(R3),C'-'                                                       
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    ROU1E               DON'T SET END DATE                           
         GOTO1 VDATCON,DMCB,(1,PVALPEND),(9,1(R3))                              
         MVC   MOSEND,PVALPEND     MOS RANGE END FOR FILTERING                  
         B     ROU1E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BILL RANGE FILTERS                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBNR   XC    BILSTA,BILSTA                                                    
         XC    BILEND,BILEND                                                    
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         SR    R0,R0                                                            
         IC    R0,FVILEN           R0=TOTAL I/P LENGTH                          
         LA    R1,FVIFLD                                                        
         LA    RE,L'BILSTA         RE=MAX START LENGTH                          
         LR    RF,RE                                                            
VALBNR2  CLI   0(R1),C'A'                                                       
         BL    VALBNR6                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,VALBNR2          MAX LENGTH OR LESS - OK                      
VALBNR4  CLI   0(R1),C'A'          ELSE ADVANCE R1 TO SEPARATOR                 
         BL    VALBNR8             AND MOVE FOR MAX START LENGTH                
         LA    R1,1(R1)                                                         
         BCT   R0,VALBNR4                                                       
         B     VALBNR20            DIDN'T FIND SEPARATOR - NO END               
*                                                                               
VALBNR6  SR    RF,RE                                                            
         BZ    VALBNR10            NO START NUMBER                              
VALBNR8  BCTR  RF,0                                                             
         STC   RF,BILSTAXQ         SAVE EXECUTE LENGTH                          
         EX    RF,*+8                                                           
         B     VALBNR10                                                         
         MVC   BILSTA(0),FVIFLD                                                 
VALBNR10 CLI   0(R1),C'A'          SEEK END BILL NUMBER                         
         BNL   VALBNR12                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VALBNR10                                                      
         B     VALBNR20                                                         
*                                                                               
VALBNR12 ST    R1,FULL             SAVE A(END BILL NUMBER)                      
         LA    RE,L'BILEND                                                      
         LR    RF,RE                                                            
         B     *+12                                                             
VALBNR14 CLI   0(R1),C'A'                                                       
         BL    VALBNR16                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,*+8              MAX LENGTH OR LESS - OK                      
         B     VALBNR18            ELSE MOVE FOR L'BILEND                       
         BCT   R0,VALBNR14                                                      
*                                                                               
VALBNR16 SR    RF,RE                                                            
         BZ    ROU1E               NO END NUMBER                                
VALBNR18 BCTR  RF,0                                                             
         STC   RF,BILENDXQ                                                      
         L     R1,FULL                                                          
         EX    RF,*+8                                                           
         B     VALBNR20                                                         
         MVC   BILEND(0),0(R1)                                                  
*                                                                               
VALBNR20 BAS   RE,CLRFLD                                                        
         LA    RE,L'FVIHDR(RF)     RE=A(SCREEN BILL NUMBER RANGE)               
         SR    RF,RF                                                            
         OC    BILSTA,BILSTA       HAVE WE A START NUMBER?                      
         BZ    VALBNR22            MUST BE AN END                               
         IC    RF,BILSTAXQ                                                      
         MVC   0(0,RE),BILSTA                                                   
         EX    RF,*-6                                                           
         OC    BILEND,BILEND       HAVE WE AN END BILL NUMBER?                  
         BZ    ROU1E                                                            
         LA    RE,1(RF,RE)                                                      
VALBNR22 MVI   0(RE),C'-'                                                       
         IC    RF,BILENDXQ                                                      
         MVC   1(0,RE),BILEND                                                   
         EX    RF,*-6                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(EARNGINV)                                           
         CLC   BILSTA,BILEND       ENSURE START DOESN'T EXCEED END              
         BH    ROU1H                                                            
VALBNRX  B     ROU1E                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILLING SOURCE FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
VALBSO   XC    SOURCE,SOURCE                                                    
         MVI   SOURCEXQ,0                                                       
         GOTO1 AFVAL                                                            
         BNE   ROU1X               LOW MEANS NO INPUT - OK                      
         MVC   SOURCE,FVIFLD                                                    
         MVC   SOURCEXQ,FVXLEN                                                  
VALBSOX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISCOUNT ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   XC    DISCVALS(DISCVALL),DISCVALS                                      
         ZAP   DISCAMT,PZERO                                                    
         ZAP   DISCAM2,PZERO                                                    
         NI    TOTPROF,255-PROFTDSC                                             
         MVC   FVUNLD,=AL2(FFFF)                                                
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   DISC,FVIFLD                                                      
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'DISC),DISC                                             
         GOTO1 AGETACC,DISCLIST    GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         L     R1,AIO1                                                          
         MVC   DISC,ACTKUNT-ACTKEY(R1)                                          
         MVC   DISCNAME,RECNAME                                                 
         MVC   DISCOFF,RECOFFC                                                  
         MVC   DISCCOST,RECCSTG                                                 
         MVC   DISCANAL,RECANAL    EXTRACT ANALYSIS A/C FROM SPACEL             
         TM    PROFTOT,PROFTDSC    TEST DISCOUNT TOTAL WANTED                   
         BZ    *+8                                                              
         OI    TOTPROF,PROFTDSC    YES - SET TOTAL ON                           
VALDSCX  B     ROU1E                                                            
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE VAT ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
VALVAT   XC    VAT,VAT                                                          
         MVC   FVUNLD,VATUL                                                     
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   VAT(L'VATUL),VATUL  SET DEFAULT UNIT/LEDGER                      
         LA    RE,VAT+L'VATUL                                                   
         LA    R1,L'VAT-L'VATUL-1                                               
         LA    RF,FVIFLD                                                        
         CLI   FVIFLD,C'*'         TEST USER OVERRIDING UNIT/LEDGER             
         BNE   *+16                                                             
         LA    RE,VAT                                                           
         LA    R1,L'VAT-1                                                       
         LA    RF,FVIFLD+1                                                      
         MVC   0(0,RE),0(RF)       SET BANK (UNIT/LEDGER/)ACCOUNT               
         EX    R1,*-6                                                           
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'VAT),VAT                                               
         SR    R1,R1                                                            
         CLC   ACTKUNT(L'VATUL),VATUL                                           
         BE    *+8                                                              
         LA    R1,VATLIST                                                       
         GOTO1 AGETACC             GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         TM    RECSTAT,RSTSIVAT    TEST INPUT VAT ACCOUNT                       
         BNZ   GETACCIP                                                         
         MVC   VATNAME,RECNAME                                                  
         MVC   VATOFF,RECOFFC                                                   
         MVC   VATRATE,RECVATR                                                  
VALVATX  B     ROU1E                                                            
*&&                                                                             
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* VALIDATE GST ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
VALGST   XC    GST,GST                                                          
         MVC   FVUNLD,GSTUL                                                     
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   GST(L'GSTUL),GSTUL  SET DEFAULT UNIT/LEDGER                      
         LA    RE,GST+L'GSTUL                                                   
         LA    R1,L'GST-L'GSTUL-1                                               
         LA    RF,FVIFLD                                                        
         CLI   FVIFLD,C'*'         TEST USER OVERRIDING UNIT/LEDGER             
         BNE   *+16                                                             
         LA    RE,GST                                                           
         LA    R1,L'GST-1                                                       
         LA    RF,FVIFLD+1                                                      
         MVC   0(0,RE),0(RF)       SET BANK (UNIT/LEDGER/)ACCOUNT               
         EX    R1,*-6                                                           
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'GST),GST                                               
         SR    R1,R1                                                            
         CLC   ACTKUNT(L'GSTUL),GSTUL                                           
         BE    *+8                                                              
         LA    R1,GSTLIST                                                       
         GOTO1 AGETACC             GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         MVC   GSTNAME,RECNAME                                                  
         MVC   GSTOFF,RECOFFC                                                   
VALGSTX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PST ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
VALPST   XC    PST,PST                                                          
         MVC   FVUNLD,PSTUL                                                     
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFVAL                                                            
         BNE   ROU1X                                                            
         MVC   PST(L'PSTUL),PSTUL  SET DEFAULT UNIT/LEDGER                      
         LA    RE,PST+L'PSTUL                                                   
         LA    R1,L'PST-L'PSTUL-1                                               
         LA    RF,FVIFLD                                                        
         CLI   FVIFLD,C'*'         TEST USER OVERRIDING UNIT/LEDGER             
         BNE   *+16                                                             
         LA    RE,PST                                                           
         LA    R1,L'PST-1                                                       
         LA    RF,FVIFLD+1                                                      
         MVC   0(0,RE),0(RF)       SET BANK (UNIT/LEDGER/)ACCOUNT               
         EX    R1,*-6                                                           
         LA    R5,KEY                                                           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PST),PST                                               
         SR    R1,R1                                                            
         CLC   ACTKUNT(L'PSTUL),PSTUL                                           
         BE    *+8                                                              
         LA    R1,PSTLIST                                                       
         GOTO1 AGETACC             GET ACCOUNT, TEST SECURITY ETC.              
         BNE   ROU1H                                                            
         MVC   PSTNAME,RECNAME                                                  
         MVC   PSTOFF,RECOFFC                                                   
VALPSTX  B     ROU1E                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE DATAMGR CALLS TO ACCOUNT FILES                     *         
***********************************************************************         
         SPACE 1                                                                
IOHIGH   LA    RF,DMRDHI                                                        
         L     R1,AIO1                                                          
         MVC   0(L'KEY,R1),KEY                                                  
         L     R0,AIO1                                                          
         MVI   IOINDS,0                                                         
         B     IOEX                                                             
IOSEQ    LA    RF,DMRSEQ                                                        
         L     R0,AIO1                                                          
         MVI   IOINDS,0                                                         
         B     IOEX                                                             
IOREAD   LA    RF,DMREAD                                                        
         L     R1,AIO1                                                          
         MVC   0(L'KEY,R1),KEY                                                  
         L     R0,AIO1                                                          
         MVI   IOINDS,0                                                         
         B     IOEX                                                             
IORDNSI  LA    RF,DMREAD           READ TO NON-STANDARD IOAREA                  
         LR    R0,R1               R0=A(NON-STANDARD IO AREA)                   
         MVI   IOINDS,0                                                         
         B     IOEX                                                             
         SPACE 2                                                                
IOEX     GOTO1 VDATAMGR,DMCB,(IOINDS,(RF)),ACCFIL,(R0),(R0)                     
         BE    IOXX                                                             
         TM    8(R1),X'40'         DISK ERROR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   8(R1),0                                                          
IOXX     B     ROU1X                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT A RECORD TO TSAR                                     *         
* NTRY - R1 CONTAINS RECORD TYPE (SEE TSARTYPE EQUATES)               *         
***********************************************************************         
         SPACE 1                                                                
TSARADD  STC   R1,TSARTYPE         SET TSAR RECORD TYPE                         
         L     R5,AIO1                                                          
         L     R2,ATSARBLK                                                      
         USING TSARD,R2                                                         
         MVI   TSACTN,TSAADD                                                    
         MVI   TSERRS,0                                                         
         CLI   TSARTYPE,TSARTOVR   TEST OVERPAYMENT ITEM                        
         BE    TSARAD04                                                         
         TM    TSARIND2,TSAR2REP   TEST REPLICANT ITEM                          
         BO    TSARAD04                                                         
         ZAP   DUB,TSARDR          TAKE DEBITS                                  
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT KNOWN                  
         BZ    *+14                                                             
         AP    DUB,TSARDISC        YES - SUBTRACT DISCOUNT (-VE AMOUNT)         
         B     *+8                                                              
         OI    TSARINDS,TSARIGRS   SET GROSS AMOUNT IF NO DISC ACCOUNT          
         CP    DUB,TSARCR          IF DEBITS(-DISCOUNT)=CREDITS                 
         BE    TSARAD06                                                         
*&&UK                                                                           
         CP    TSARDR,PZERO        OR IF DEBITS=ZERO                            
         BNE   *+14                                                             
         CP    TSARCR,PZERO        AND CREDITS=ZERO                             
         BE    TSARAD06            DROP THIS ITEM                               
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BO    TSARAD02                                                         
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BO    TSARAD04            YES - ADD THIS ITEM                          
         OC    TSARCUR,TSARCUR     NO - TEST CURRENCY ITEM                      
         BZ    TSARAD04                                                         
         TM    COMPSTA6,CPYSFBIL   TEST FOREIGN BILLING USED                    
         BZ    TSARAD04            NO - INCLUDE                                 
         B     TSARAD06            ELSE DROP CURRENCY ITEM                      
*                                                                               
TSARAD02 CLC   TSARCUR,FORCURT+(CURTCUR-CURTABD)                                
         BNE   TSARAD06            WRONG CURRENCY  - DROP THIS ITEM             
         ZAP   DUB,TSARDR2         TAKE CURRENCY DEBITS                         
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT KNOWN                  
         BZ    *+10                                                             
         AP    DUB,TSARDI2         YES - SUBTRACT CURRENCY DISCOUNT             
         CP    DUB,TSARCR2         IF CURRENCY DRS(-DISCOUNT)=CRS               
         BE    TSARAD06            DROP THIS ITEM                               
         CP    TSARDR2,PZERO       OR IF CURRENCY DEBITS=ZERO                   
         BNE   *+14                                                             
         CP    TSARCR2,PZERO       AND CURRENCY CREDITS=ZERO                    
         BE    TSARAD06            DROP THIS ITEM                               
         LA    RF,TSARAFCX         BUILD EXCHANGE RULES                         
         USING AFCX,RF                                                          
         TM    AFCXSTAT,AFCXSMEM   TEST ONE OR MORE MEMO ITEMS                  
         BO    TSARAD06            DROP THIS ITEM                               
         XC    AFCX,AFCX           CLEAR AND BUILD EXCHANGE RULES               
         ZAP   PKWK16,TSARDR2      CALCULATE OPEN ITEM BALANCE                  
         ZAP   DUB,TSARDR          IN BOTH CURRENCIES                           
         OC    DISC,DISC                                                        
         BZ    *+16                                                             
         AP    PKWK16,TSARDI2                                                   
         AP    DUB,TSARDISC                                                     
         SP    PKWK16,TSARCR2                                                   
         SP    DUB,TSARCR                                                       
         LA    RE,11                                                            
         SR    R0,R0                                                            
         IC    R0,FORCURT+(CURTDECP-CURTABD)                                    
         SR    RE,R0                                                            
         SRP   PKWK16,0(RE),0                                                   
         LA    RE,5                                                             
         SR    R0,R0                                                            
         IC    R0,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,R0                                                            
         SRP   DUB,0(RE),0                                                      
         DP    PKWK16,DUB                                                       
         SRP   PKWK16(8),64-1,5                                                 
         LM    R0,R1,PKWK16        FIRST 8 BYTES IS QUOTIENT                    
         SRDL  R0,4                                                             
         STM   R0,R1,PKWK16        LOSE PACKED SIGN                             
         OC    PKWK16(3),PKWK16    ENSURE RATE WILL FIT                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   AFCXRATE,PKWK16+3                                                
         SR    RE,RE                                                            
         IC    RE,FORCURT+(CURTDECP-CURTABD)                                    
         SR    R0,R0                                                            
         IC    R0,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,R0                                                            
         STC   RE,AFCXSHFT         SET SHIFT VALUE                              
*&&                                                                             
TSARAD04 MVC   TSARSUBR,TSARSUB    SET SUB-REFERENCE NUMBER                     
         SR    RF,RF                                                            
         IC    RF,KEYACTD          SET ACCOUNT INDEX VALUE                      
         LA    RE,TSARKEY(RF)                                                   
         MVC   0(L'RECVCNDX,RE),RECVCNDX                                        
         IC    RF,KEYCACD          SET SOURCE                                   
         LA    RE,TSARKEY(RF)                                                   
         MVC   0(L'TSARCAC,RE),TSARCAC                                          
         IC    RF,KEYDATD          SET DATE                                     
         LA    RE,TSARKEY(RF)                                                   
         MVC   0(L'TRNKDATE,RE),TSARDAT                                         
         IC    RF,KEYREFD          SET REFERENCE                                
         LA    RE,TSARKEY(RF)                                                   
         MVC   0(L'TRNKREF,RE),TSARREF                                          
         IC    RF,KEYOFFD          SET OFFICE                                   
         LA    RE,TSARKEY(RF)                                                   
         MVC   0(L'TSAROFFC,RE),TSAROFFC                                        
         IC    RF,KEYDUED          SET DUE DATE                                 
         LA    RE,TSARKEY(RF)                                                   
         MVC   0(L'TSARDUED,RE),TSARDUED                                        
*&&US                                                                           
TSARAD4A DS    0H                                                               
         L     R3,AIO6             POINT TO THE WORK AREA                       
         USING SAVEVALS,R3         MAP WITH SAVE KEY                            
         LA    R0,TBLEMAXQ         MAXIMUM NUMBER OF ENTRIES                    
         BCTR  R0,0                REDUCE COUNT BY 1                            
         LA    R4,TBLENTRY         POINT TO THE WORK AREA                       
         USING TABLVALD,R4         MAP WITH SAVE KEY                            
*                                                                               
TSARAD4B DS    0H                                                               
         OC    TABLVALS(TABLVALL),TABLVALS   DO WE HAVE ANY ENTRY ?             
         BZ    TSARAD4E            NO , ADD THE RECORD TO TSAR                  
*                                  YES,                                         
         CLC   TABLVALS(TABLVALL),TSARVALS   IS RECORD ALREADY ADDED ?          
         BE    TSARAD4C            YES, GET RECORD DETAILS FROM TSAR            
*                                                                               
         AHI   R4,TABLVALL         POINT TO THE NEXT ENTRY                      
         BCT   R0,TSARAD4B         PROCESS NEXT ENTRY                           
         B     TSARAD4E            NO RECORD FOUND, ADD IT                      
*                                                                               
TSARAD4C DS    0H                                                               
         MVI   TSACTN,TSARDH       SET ACTION AS A READ HIGH                    
         ZAP   WORKDR,TSARDR       SAVE DEBIT AMOUNT                            
         ZAP   WORKCR,TSARCR       SAVE CREDIT AMOUNT                           
         ZAP   WORKDISC,TSARDISC   SAVE DISCOUNT (OR SURCHARGE) AMOUNT          
         ZAP   WORKPOST,TSARPOST   SAVE POSTING AMOUNT                          
         ZAP   WORKGSTB,TSARGSTB   SAVE GST BASIS                               
         ZAP   WORKGST,TSARGST     SAVE GST AMOUNT                              
         ZAP   WORKPSTB,TSARPSTB   SAVE PST BASIS                               
         ZAP   WORKPST,TSARPST     SAVE PST AMOUNT                              
*                                                                               
         XR    RF,RF               CLEAR RF                                     
         IC    RF,KEYDUED                                                       
         LA    RE,TSARKEY(RF)                                                   
         XC    0(L'TSARDUED,RE),0(RE) CLEAR DUE DATE                            
*                                                                               
         GOTO1 VTSAR,TSARD         READ RECORD FROM THE TSAR                    
         BE    *+6                 SUCESSFUL                                    
         DC    H'00'               NO, ABEND                                    
*                                  YES                                          
         CLC   TSARVALS(TSARVALL),TSARKEY+L'RECVNDX    IS KEY SAME ?            
         BE    TSARAD4D            YES , CONTINUE                               
         DC    H'00'               NO, ABEND                                    
*                                  YES                                          
TSARAD4D DS    0H                                                               
         L     R2,ATSARBLK         ADDRESS TSAR BLOCK                           
         USING TSARD,R2            MAP WITH THE RECORD LAYOUT                   
         AP    TSARDR,WORKDR       ADD  DEBIT AMOUNT                            
         AP    TSARCR,WORKCR       ADD  CREDIT AMOUNT                           
         AP    TSARDISC,WORKDISC   ADD  DISCOUNT (OR SURCHARGE) AMOUNT          
         AP    TSARPOST,WORKPOST   ADD  POSTING AMOUNT                          
         AP    TSARGSTB,WORKGSTB   ADD  GST BASIS                               
         AP    TSARGST,WORKGST     ADD  GST AMOUNT                              
         AP    TSARPSTB,WORKPSTB   ADD  PST BASIS                               
         AP    TSARPST,WORKPST     ADD  PST AMOUNT                              
*                                  NO, UPDATE DETAILS                           
         MVI   TSACTN,TSAPUT       AND PUT BACK ORIGINAL RECORD                 
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TSARAD06            CONTINUE                                     
*                                                                               
TSARAD4E DS    0H                                                               
*&&                                                                             
         GOTO1 VTSAR,TSARD         ADD RECORD TO TSAR                           
         BNE   TSARADDN                                                         
         SR    RF,RF                                                            
         ICM   RF,3,DISMAX                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,DISMAX         UPDATE HIGH (MAIN) RECORD NUMBER             
*&&US                                                                           
         LA    R0,TBLEMAXQ         MAXIMUM NUMBER OF ENTRIES                    
         BCTR  R0,0                REDUCE COUNT BY 1                            
         CLI   SAVETBLE,SAVETBLY   ADD ENTRY TO THE TABLE ?                     
         BNE   TSARAD4H            NO, CONTINUE                                 
*                                  YES                                          
TSARAD4F DS    0H                  SEARCH FOR THE EMPTY BOX                     
         OC    TABLVALS(TABLVALL),TABLVALS   DO WE HAVE ANY ENTRY ?             
         BZ    TSARAD4G            NO , ADD THE RECORD TO TABLE                 
*                                  YES,                                         
         AHI   R4,TABLVALL         POINT NEW SLOT                               
         BCT   R0,TSARAD4F         PROCESS NEXT SLOT                            
         B     TSARAD4H            NO SPACE TO ADD RECORD                       
TSARAD4G MVC   TABLVALS(TABLVALL),TSARVALS   YES, SAVE TABLE ENTRY              
*                                                                               
TSARAD4H MVI   SAVETBLE,SAVETBLN   MARK DONT ADD TABLE ENTRY AS DEFULT          
*&&                                                                             
TSARAD06 XC    TSARVALS(TSARVALL),TSARVALS CLEAR TSAR RECORD VALUES             
         MVC   TSAROFFC,SPACES                                                  
         XC    TSARDUED,TSARDUED                                                
         XC    TSARSUB,TSARSUB                                                  
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         ZAP   TSARDR,PZERO                                                     
         ZAP   TSARCR,PZERO                                                     
         ZAP   TSARDISC,PZERO                                                   
         ZAP   TSARPOST,PZERO                                                   
*&&UK                                                                           
         ZAP   TSARTAX,PZERO                                                    
         ZAP   TSARDR2,PZERO                                                    
         ZAP   TSARCR2,PZERO                                                    
         ZAP   TSARDI2,PZERO                                                    
         ZAP   TSARPO2,PZERO                                                    
*&&                                                                             
*&&US                                                                           
         ZAP   TSARGSTB,PZERO                                                   
         ZAP   TSARGST,PZERO                                                    
         ZAP   TSARPSTB,PZERO                                                   
         ZAP   TSARPST,PZERO                                                    
         XC    TSARGSTT,TSARGSTT                                                
         XC    TSARPSTT,TSARPSTT                                                
         XC    TSARPSTC,TSARPSTC                                                
*&&                                                                             
         B     TSARADDX                                                         
*                                                                               
TSARADDN MVC   FVMSGNO,=AL2(EGRECAOF)                                           
         TM    TSERRS,TSEDUP                                                    
         BNZ   TSARADDX                                                         
         MVC   FVMSGNO,=AL2(EATRNMAX)                                           
         MVC   FVXTRA(L'RECVCACT-1),RECVCACT+1  ADD ACCOUNT CODE TO             
         TM    TSERRS,TSEEOF                    ERROR MESSAGE                   
         BNZ   TSARADDX                                                         
         DC    H'0'                                                             
*                                                                               
TSARADDX CLI   TSERRS,0            SET CONDITION CODE NEQ ON ERROR              
         B     ROU1X                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A RECORD FROM TSAR AND SET KEY VALUES                *         
*                                                                     *         
* NTRY - R1=A(RECORD NUMBER)                                          *         
***********************************************************************         
         SPACE 1                                                                
TSARGET  L     R2,ATSARBLK                                                      
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
         MVI   TSACTN,TSAGET                                                    
         MVC   TSRNUM,0(R1)                                                     
         GOTO1 VTSAR,TSARD                                                      
         BNE   TSARGETX                                                         
         SR    RF,RF               EXTRACT RECEIVABLE ACCOUNT FROM KEY          
         IC    RF,KEYACTD                                                       
         LA    RE,TSARKEY(RF)                                                   
         SR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         STC   R1,RECVCNDX         TABLE INDEX NUMBER                           
         LA    R0,RECVTABL         MULTIPLY BY THE LENGTH OF AN ENTRY           
         MR    R0,R0               TO GET DISPLACEMENT INTO TABLE               
         LA    RF,RECVTABL                                                      
         SR    R1,RF                                                            
*        LA    R1,ARECVTAB-RECVTABL(R1)                                         
         L     R3,ARECVTAB                                                      
         LA    R3,0(R1,R3)         R1 CONTAINS THE DISPLACEMENT TO              
         USING RECVTABD,R3         THE CORRECT TABLE ENTRY                      
         CLC   RECVNDX,RECVCNDX                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RECVCACT,RECVACT                                                 
         IC    RF,KEYCACD          EXTRACT CONTRA ACCOUNT FROM KEY              
         LA    RE,TSARKEY(RF)                                                   
         MVC   TSARCAC,0(RE)                                                    
         IC    RF,KEYDATD          EXTRACT DATE FROM KEY                        
         LA    RE,TSARKEY(RF)                                                   
         MVC   TSARDAT,0(RE)                                                    
         IC    RF,KEYREFD          EXTRACT BILL NUMBER FROM KEY                 
         LA    RE,TSARKEY(RF)                                                   
         MVC   TSARREF,0(RE)                                                    
         IC    RF,KEYOFFD          EXTRACT OFFICE FROM KEY                      
         LA    RE,TSARKEY(RF)                                                   
         MVC   TSAROFFC,0(RE)                                                   
         IC    RF,KEYDUED          EXTRACT DUE DATE FROM KEY                    
         LA    RE,TSARKEY(RF)                                                   
         MVC   TSARDUED,0(RE)                                                   
         MVC   TSARSUB,TSARSUBR    MOVE OUT SUBREFERENCE                        
TSARGETX CLI   TSERRS,0            SET CC FOR CALLER                            
         B     ROU1X                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE EXTRACTS ELEMENTS FROM RECORD AT IO.  ROUTINE DIES IF       *         
* COMPULSORY ELEMENTS ARE NOT FOUND                                   *         
***********************************************************************         
         SPACE 1                                                                
ACCELS   XC    RECVALS(RECVALSL),RECVALS                                        
         STC   R1,RECELS           SAVE WHAT'S WANTED                           
         L     R1,AIO1                                                          
         AH    R1,DATADISP                                                      
         MVI   RECINDS,0                                                        
         SR    RF,RF                                                            
*                                                                               
         USING CPYELD,R1                                                        
ACCEL01  CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BNE   ACCEL02                                                          
         STCM  R1,15,RECCOMP       SAVE A(COMPANY ELEMENT)                      
         B     ACCELSN                                                          
*                                                                               
         USING NAMELD,R1                                                        
ACCEL02  CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   ACCEL03                                                          
         TM    RECELS,RECNAMEQ     NAME REQUIRED ?                              
         BZ    ACCELSN                                                          
         NI    RECELS,255-RECNAMEQ CLEAR NAME REQUIRED                          
         MVC   RECNAME,SPACES                                                   
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     ACCELSN                                                          
         MVC   RECNAME(0),NAMEREC                                               
*                                                                               
         USING RSTELD,R1                                                        
ACCEL03  CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BNE   ACCEL04                                                          
         TM    RECELS,RECSTATQ     STATUS REQUIRED ?                            
         BZ    ACCELSN                                                          
         NI    RECELS,255-RECSTATQ CLEAR STATUS REQUIRED                        
         MVC   RECSTAT,RSTSTAT                                                  
         MVC   RECCSTG,RSTCOSTG                                                 
         MVC   RECSECY,RSTSECY                                                  
         MVC   RECFILT1,RSTFILT1                                                
         MVC   RECFILT2,RSTFILT2                                                
         MVC   RECFILT3,RSTFILT3                                                
         MVC   RECFILT4,RSTFILT4                                                
         MVC   RECFILT5,RSTFILT5                                                
         MVC   RECOFFC,SPACES      SET OFFICE TO SPACES                         
         B     ACCELSN                                                          
*                                                                               
         USING ABLELD,R1                                                        
ACCEL04  CLI   ABLEL,ABLELQ        TEST BALANCE ELEMENT                         
         BNE   ACCEL05                                                          
         OI    RECINDS,RECIBAL     SET ACCOUNT HAS A BALANCE ELEMENT            
         B     ACCELSN                                                          
*                                                                               
         USING PPRELD,R1                                                        
ACCEL05  CLI   PPREL,PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BNE   ACCEL06                                                          
         STCM  R1,15,RECPROF       SAVE A(PROFILE ELEMENT)                      
         B     ACCELSN                                                          
*                                                                               
         USING APTELD,R1                                                        
ACCEL06  CLI   APTEL,APTELQ        TEST ACCOUNT POINTER ELEMENT                 
         BNE   ACCEL07                                                          
         OC    RECAPTR,RECAPTR     TEST ADDRESS ALREADY SET                     
         BNZ   *+8                                                              
         STCM  R1,15,RECAPTR       SAVE ADDRESS OF FIRST ELEMENT                
         B     ACCELSN                                                          
*                                                                               
         USING RATELD,R1                                                        
ACCEL07  CLI   RATEL,RATEVATQ      TEST VAT RATE ELEMENT                        
         BNE   ACCEL08                                                          
         MVC   RECVATR,RATRATE     SAVE VAT RATE                                
         B     ACCELSN                                                          
*                                                                               
         USING ASTELD,R1                                                        
ACCEL08  CLI   ASTEL,ASTELQ        TEST ACCOUNT STATUS ELEMENT                  
         BNE   ACCEL09                                                          
         MVC   RECASTS1,ASTSTAT1   EXTRACT STATUS BYTE 1                        
         CLC   ASTCUR,SPACES       TEST CURRENCY                                
         BNH   *+10                                                             
         MVC   RECCURR,ASTCUR      SET CURRENCY                                 
         B     ACCELSN                                                          
*                                                                               
         USING SPAELD,R1                                                        
ACCEL09  CLI   SPAEL,SPAELQ        TEST SPECIAL POSTING A/C ELEMENT             
         BNE   ACCEL16                                                          
         CLI   SPATYPE,SPATANAL                                                 
         BNE   ACCEL10                                                          
         MVC   RECANAL,SPAAANAL    EXTRACT 12 BYTE ANALYSIS ACCOUNT             
         B     ACCELSN                                                          
ACCEL10  CLI   SPATYPE,SPATEXDF                                                 
         BNE   ACCEL11                                                          
         MVC   RECEXDF,SPAAULA     EXTRACT EXCHANGE DIFFERENCE ACCOUNT          
         B     ACCELSN                                                          
ACCEL11  CLI   SPATYPE,SPATBCHA                                                 
         BNE   ACCEL12                                                          
         MVC   RECBCHA,SPAAULA     EXTRACT BANK CHARGES ACCOUNT                 
         B     ACCELSN                                                          
ACCEL12  DS    0H                  NEXT SPATYPE                                 
*                                                                               
ACCEL16  DS    0H                                                               
*                                                                               
ACCELSN  IC    RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         CLI   0(R1),0                                                          
         BNE   ACCEL01                                                          
*                                                                               
ACCELSX  CLI   RECELS,0            TEST ALL REQUIRED VALUES SET                 
         BE    ROU1X                                                            
         DC    H'0'                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* STANDARD FIELD VALIDATION ROUTINE                                   *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER)                                           *         
*        FVMINL=MINIMUM FIELD LENGTH OR 0 IF NOT REQUIRED             *         
*        FVMAXL=MAXIMUM FIELD LENGTH OR 0                             *         
*        FVUNLD=SEARCH TRIGGER AS FOLLOWS:                            *         
*               X'0000' DON'T CALL SEARCH FOR THIS FIELD              *         
*               C'UL'   SEARCH USING THIS UNIT/LEDGER                 *         
*               X'FFFF' SEARCH USING USER'S UNIT/LEDGER               *         
*        FVPREF=IF FVUNLD SET, PREFIX CHARACTER FOR OVERRIDE OR 0     *         
***********************************************************************         
         SPACE 1                                                                
FVAL     ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         OC    FVUNLD,FVUNLD                                                    
         BZ    FVAL02              X'0000' DON'T SEARCH FOR THIS FIELD          
         CLI   0(R1),94            MAX ALLOWABLE FLD LENGTH FOR SEARCH          
         BH    FVAL02                                                           
         SR    RF,RF               PRESUME UNIT/LEDGER UNKNOWN                  
         CLC   FVUNLD,=AL2(FFFF)                                                
         BE    *+12                X'FFFF' UNIT/LEDGER UNKNOWN                  
         LA    RF,FVUNLD           C'UL'   FIXED UNIT/LEDGER                    
         ICM   RF,8,FVPREF         SET PREFIX CHARACTER FOR OVERRIDE            
         GOTO1 VACSRCHC,DMCB,(4,FVADDR),TWAD,(RF),ACOM,(FVMAMI,0)               
         XC    FVUNLD,FVUNLD       CLEAR SEARCH TRIGGER                         
         XC    FVPREF,FVPREF       CLEAR PREFIX CHARACTER FOR OVERRIDE          
         XC    FVMAMI,FVMAMI       CLEAR MINIMUM/MAXIMUM LENGTH                 
         L     R1,FVADDR           RESTORE R1=A(FIELD HEADER)                   
*                                                                               
FVAL02   MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVC   FVIFLD,SPACES                                                    
         SR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         LA    R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    *+8                                                              
         LA    R0,L'FVIHDR+L'FVIHDR+1                                           
         SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         MVC   FVIFLD(0),L'FVIHDR(R1)                                           
         EX    RF,*-6              EXTRACT FIELD DATA                           
*                                                                               
         LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         LA    RF,1(RF)            RF=LOOP COUNT                                
FVAL04   CLI   0(R1),C' '          LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FVAL06                                                           
         MVI   0(R1),C' '          SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCT   RF,FVAL04                                                        
FVAL06   STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(EGIFSHRT) ENSURE NOT TOO SHORT OR LONG              
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         CLM   RF,1,FVMINL                                                      
         BL    FVALX                                                            
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLM   RF,1,FVMAXL                                                      
         BH    FVALX                                                            
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(IGOK)                                               
         B     FVAL16                                                           
*                                  SET FIELD VALIDITY BITS                      
         MVC   FVMSGNO,=AL2(IGOK)  INDICATE FIELD IS OK                         
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
FVAL08   TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FVAL12                                                           
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         B     FVAL10                                                           
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,255-FVIALF                                                
         B     FVAL10                                                           
         NI    FVIIND,255-FVINUM                                                
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,255-FVIHEX                                                
FVAL10   BCTR  R1,0                                                             
         BCT   RF,FVAL08                                                        
FVAL12   IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         CLI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         BNE   FVAL14                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    FVAL14                                                           
         EX    RF,*+8              SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  DUB,FVIFLD(0)                                                    
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
FVAL14   MVC   FVMSGNO,=AL2(IGOK)                                               
*                                                                               
FVAL16   MVI   FVHELP,0            RESET THIS TIME VALUES                       
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVNUMER,0                                                        
         MVC   FVXTRA,SPACES                                                    
         B     FVALX                                                            
*                                  HANDLE ERRORS HERE                           
FVALX    CLC   FVMSGNO,=AL2(EGIFMISS)                                           
         BE    FVALX2                                                           
         MVI   FVFLAG,0                                                         
         CLI   FVILEN,0                                                         
         BE    FVALXX                                                           
         MVI   FVFLAG,1                                                         
         CLC   FVMSGNO,=AL2(IGOK)                                               
         BE    FVALXX                                                           
FVALX2   MVI   FVFLAG,2                                                         
FVALXX   CLI   FVFLAG,1            SET CONDITION CODE FOR CALLER                
         MVI   FVFLAG,0                                                         
         B     ROU1X               RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A LEDGER RECORD AND BUILD LEDGTAB (LEDGTABD)         *         
*                                                                     *         
* NTRY - KEY=KEY OF A LEDGER OR LOWER LEVEL RECORD                    *         
*                                                                     *         
* EXIT - RECLEDG POINTS TO LEDGER TABLE ENTRY                         *         
***********************************************************************         
         SPACE 1                                                                
GETLDG   LA    R0,LEDGMAXN         R0=MAXIMUM N'LEDGER TABLE ENTRIES            
         LA    R2,LEDGTAB                                                       
         USING LEDGTABD,R2         R2=A(LEDGER TABLE)                           
GETLDG1  OC    LEDGTUL,LEDGTUL     TEST FREE SLOT                               
         BZ    GETLDG2                                                          
         CLC   LEDGTUL,KEY+(LDGKUNT-LDGKEY)                                     
         BE    GETLDGX                                                          
         LA    R2,LEDGTABL(R2)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,GETLDG1          DO FOR NUMBER OF ENTRIES                     
         SH    R2,=Y(LEDGTABL)     USE LAST ENTRY IF TABLE FULL                 
*                                                                               
GETLDG2  MVC   KEYSAVE,KEY         SAVE CALLER'S KEY                            
         MVC   KEY,SPACES          TAKE LEDGER PORTION & READ                   
         MVC   KEY(LDGKEND),KEYSAVE                                             
         GOTO1 AIOREAD                                                          
         MVC   KEY,KEYSAVE         RESTORE CALLER'S KEY                         
         BNE   GETLDGN                                                          
*                                                                               
         L     R1,AIO1             PROCESS LEDGER RECORD & BUILD ENTRY          
         MVC   LEDGTUL,LDGKUNT-LDGKEY(R1)                                       
         AH    R1,DATADISP                                                      
         SR    R0,R0                                                            
GETLDG3  CLI   0(R1),0             TEST E-O-R                                   
         BE    GETLDGX                                                          
*                                                                               
         USING LDGELD,R1                                                        
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   GETLDG4                                                          
         MVC   LEDGTTYP,LDGTYPE                                                 
         MVC   LEDGTLIK,LDGLIKE                                                 
         MVC   LEDGTOFF,LDGOPOS                                                 
         MVC   LEDGTCLO,LDGCLOS                                                 
         CLI   LDGLN,LDGLNQ                                                     
         BL    GETLDG9                                                          
         PACK  LEDGTDDL,LDGDPOS    EXTRACT DEPARTMENT VALUES                    
         NI    LDGDLEN,X'0F'                                                    
         OC    LEDGTDDL,LDGDLEN                                                 
         CLC   LEDGTUL,RECVUL      TEST RECEIVABLE LEDGER                       
         BNE   GETLDG9                                                          
         MVC   TWASRCLI,LDGCPOS    EXTRACT POSITION OF CLIENT IN KEY            
         B     GETLDG9                                                          
*                                                                               
         USING ACLELD,R1                                                        
GETLDG4  CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BNE   GETLDG5                                                          
         MVC   LEDGTLVA,ACLVALS                                                 
         MVC   LEDGTLVB,ACLVALS+(L'ACLVALS*1)                                   
         MVC   LEDGTLVC,ACLVALS+(L'ACLVALS*2)                                   
         MVC   LEDGTLVD,ACLVALS+(L'ACLVALS*3)                                   
         B     GETLDG9                                                          
*                                                                               
         USING RSTELD,R1                                                        
GETLDG5  CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BNE   GETLDG6                                                          
         MVC   LEDGTSEC,RSTSECY+1                                               
         B     GETLDG9                                                          
*                                                                               
GETLDG6  DS    0H                                                               
GETLDG9  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETLDG3                                                          
*                                                                               
GETLDGX  STCM  R2,15,RECLEDG       SET A(LEDGER TABLE ENTRY)                    
         MVC   FVXTRA,SPACES                                                    
         B     ROU1E                                                            
*                                                                               
GETLDGN  MVC   FVXTRA(L'LEDGTUL),KEY+(ACTKUNT-ACTKEY)                           
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     ROU1H                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH LEDGER AND ACCOUNT VALUES FOR AN ACCOUNT AND   *         
* TEST FOR SECURITY.                                                  *         
*                                                                     *         
* NTRY - KEY CONTAINS ACCOUNT RECORD KEY                              *         
***********************************************************************         
         SPACE 1                                                                
GETACC   LTR   R1,R1               TEST A(LEDGER LIST PASSED)                   
         BZ    GETACC4                                                          
         MVC   FVXTRA(L'LEDGTUL),KEY+(ACTKUNT-ACTKEY)                           
         ICM   RF,3,0(R1)          NO DISPLACEMENT MUST BE BAD                  
         BZ    GETACCIL                                                         
         A     RF,OVERLAY          RF=A(VALID UNIT/LEDGER TABLE)                
         LR    R0,RF               R0=TABLE SAVE ADDRESS                        
         SR    RE,RE               RE=NUMBER OF ENTRIES IN TABLE                
GETACC2  CLI   0(RF),EOT           TEST E-O-T                                   
         BE    GETACC3                                                          
         LA    RE,1(RE)                                                         
         CLC   0(L'LEDGTUL,RF),KEY+(ACTKUNT-ACTKEY)                             
         BE    GETACC4                                                          
         LA    RF,L'LEDGTUL(RF)                                                 
         B     GETACC2                                                          
GETACC3  CH    RE,=H'1'            TEST LIST CONTAINS ONE ENTRY                 
         BNE   GETACCIL                                                         
         LR    RF,R0               YES - USE LIST UNIT/LEDGER                   
         MVC   WORK(L'ACTKACT),KEY+(ACTKUNT-ACTKEY)                             
         MVC   KEY+(ACTKUNT-ACTKEY)(L'LEDGTUL),0(RF)                            
         MVC   KEY+(ACTKACT-ACTKEY)(L'ACTKACT),WORK                             
*                                                                               
GETACC4  GOTO1 AGETLDG             ESTABLISH THE LEDGER                         
         BNE   ROU1H                                                            
         ICM   R2,15,RECLEDG                                                    
         USING LEDGTABD,R2         R2=A(LEDGER TABLE ENTRY)                     
*&&US*&& TM    BATCHSEC,CPYBSSEC   TEST LEDGER SECURITY OVERRIDE                
*&&US*&& BZ    *+14                                                             
         CLC   TWAAUTH+1(1),LEDGTSEC                                            
         BL    GETACCSL                                                         
         MVC   FVXTRA(L'ACTKCULA-1),KEY+(ACTKUNT-ACTKEY)                        
         GOTO1 AIOREAD             READ THE ACCOUNT RECORD                      
         BNE   GETACCIA                                                         
         GOTO1 AACCELS,RECSTATQ+RECNAMEQ                                        
         TM    RECSTAT,RSTSACIL+RSTSACIC                                        
         BNZ   GETACCIP                                                         
         TM    RECINDS,RECIBAL                                                  
         BZ    GETACCIP                                                         
         CLI   LEDGTOFF,LDGOFLT1   IS OFFICE IN A/C FILTERS                     
         BL    GETACC20                                                         
         CLI   LEDGTLVA,L'ACTKACT  IS THIS A ONE LEVEL LEDGER                   
         BE    GETACC20                                                         
         LA    R3,LEDGTLVD                                                      
         LA    R0,LEDGTLVD+1-LEDGTLVA                                           
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10             ESTABLISH LOWEST LEVEL                       
         LA    RE,GETACC20         SET RETURN ADDRESS                           
         EJECT                                                                  
***********************************************************************         
* GRAB SOME STORAGE FOR INTERMEDIATE IO, ADDRESSED BY R4              *         
* NTRY - R0=NUMBER OF LEVELS                                          *         
*        R3=A(L'LOWEST LEVEL)                                         *         
***********************************************************************         
         SPACE 1                                                                
GETACC6  NTR1  WORK=(R4,IO2-IO)                                                 
GETACC8  MVC   0(L'ACTKEY,R4),SPACES                                            
         L     R1,AIO1                                                          
         IC    RF,0(R3)            TAKE L'LEVEL                                 
         LA    RF,2(RF)            ADD CUL (-1 FOR EXECUTE)                     
         MVC   0(0,R4),0(R1)       MOVE TO NON-STANDARD IOAREA FROM IO          
         EX    RF,*-6                                                           
         GOTO1 AIORDNSI,(R4)       READ TO NON-STANDARD IOAREA                  
         BE    *+6                                                              
         DC    H'0'                MISSING HIGH LEVEL ACCOUNT                   
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
         USING RSTELD,R1                                                        
GETACC10 CLI   RSTEL,0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT IN HIGH LEVEL A/C          
         CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BE    GETACC12                                                         
         IC    RF,RSTLN                                                         
         AR    R1,RF                                                            
         B     GETACC10                                                         
*                                                                               
GETACC12 MVC   WORK(1),LEDGTOFF    EXTRACT FILTER POSITION FOR OFFICE           
         NI    WORK,X'07'          X'F1'...X'F5' -> X'01'...X'05'               
         SR    RE,RE                                                            
         IC    RE,WORK                                                          
         IC    RE,FILDISP-1(RE)                                                 
         LA    RE,RSTELD(RE)                                                    
         CLI   0(RE),C' '          TEST OFFICE FILTER SET AT THIS LEVEL         
         BH    GETACC14                                                         
         BCTR  R3,0                R3=A(L'PREVIOUS LEVEL)                       
         BCT   R0,GETACC8          TRY AGAIN ONE LEVEL HIGHER                   
         B     ROU1E               NO FILTER VALUE - EXIT TO GETACC20           
*                                                                               
GETACC14 L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         MVC   OFFAOFFC(1),0(RE)   EXTRACT OFFICE CODE                          
         MVI   OFFAOFFC+1,C' '                                                  
         B     ROU1E               EXIT TO GETACC20                             
         DROP  R1                                                               
         EJECT                                                                  
GETACC20 DS    0H                                                               
*&&US*&& TM    BATCHSEC,CPYBSOFF   TEST OFFICE SECURITY OVERRIDE                
*&&US*&& BZ    GETACC22                                                         
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         MVC   OFFAREC,AIO1                                                     
         MVC   OFFAOPOS,LEDGTOFF   SET LEDGER VALUES FOR OFFAL                  
         MVC   OFFALDGL,LEDGTLVA                                                
         MVC   OFFALDGT,LEDGTCLO                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL              CALL OFFAL FOR OFFICE/SECURITY CHECK         
         BNE   GETACCSL                                                         
         MVC   RECOFFC,OFFAOFFC    SET OFFICE CODE                              
*                                                                               
GETACC22 DS    0H                                                               
*                                                                               
GETACCX  MVC   FVXTRA,SPACES                                                    
         B     ROU1E                                                            
         DROP  R1,R2                                                            
         SPACE 1                                                                
GETACCIL MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     ROU1H                                                            
GETACCIA MVC   FVMSGNO,=AL2(EAACCINV)                                           
         B     ROU1H                                                            
GETACCIP MVC   FVMSGNO,=AL2(EAACCNOP)                                           
         B     ROU1H                                                            
GETACCSL MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     ROU1L                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD DISTAB FOR INPUT DISPLAY                           *         
***********************************************************************         
         SPACE 1                                                                
BLDDIS   XC    TEMP(L'SCOLDIS+1),TEMP                                           
         CLC   SOPTDIS,OPTDIS      TEST DISPLAY OPTION CHANGED                  
         MVC   SOPTDIS,OPTDIS      SAVE DISPLAY OPTION VALUES                   
         BNE   BLDDIS10            CHANGED - START AGAIN                        
         OC    SCOLDIS,SCOLDIS     TEST SAVED DISPLAY COLUMNS                   
         BZ    BLDDIS10                                                         
         CLC   SCROLL,=AL2(SCRLEFT)                                             
         BE    BLDDIS02                                                         
         CLC   SCROLL,=AL2(SCRRGHT)                                             
         BE    BLDDIS02                                                         
         MVC   TEMP(L'SCOLDIS),SCOLDIS                                          
         B     BLDDIS20            NO SCROLLING                                 
BLDDIS02 SR    R1,R1               SCROLL ONE COLUMN LEFT/RIGHT                 
         IC    R1,SCOLDNO                                                       
         BCT   R1,*+8                                                           
         B     BLDDIS08            NO SCROLLING POSSIBLE                        
         CLC   SCROLL,=AL2(SCRLEFT)                                             
         BE    BLDDIS06                                                         
         MVC   TEMP(0),SCOLDIS+1                                                
         EX    R1,*-6              SCROLL RIGHT                                 
         LA    RF,TEMP(R1)                                                      
         MVC   0(1,RF),SCOLDIS                                                  
         B     BLDDIS08                                                         
BLDDIS06 LA    RE,SCOLDIS(R1)      SCROLL LEFT                                  
         MVC   TEMP(1),0(RE)                                                    
         BCTR  R1,0                                                             
         MVC   TEMP+1(0),SCOLDIS                                                
         EX    R1,*-6                                                           
BLDDIS08 MVC   SCOLDIS,TEMP                                                     
         B     BLDDIS20                                                         
*                                                                               
BLDDIS10 OC    TEMP(L'PROFDIS),PROFDIS                                          
         BNZ   BLDDIS12                                                         
         MVC   TEMP(L'PROFDIS),PROFDFD1                                         
         CLI   RECVCNT,1                                                        
         BE    BLDDIS12                                                         
         MVC   TEMP(L'PROFDIS),PROFDFD2                                         
BLDDIS12 OC    OPTDIS,OPTDIS       TEST DISPLAY OPTION                          
         BZ    BLDDIS14                                                         
         LA    R4,TEMP                                                          
         TM    OPTDIND,OPTDIALQ    TEST ALL COLUMNS                             
         BZ    *+14                                                             
         MVC   0(L'OPTDIS,R4),OPTDIS                                            
         B     BLDDIS14                                                         
         TM    OPTDIND,OPTDIPRQ    TEST PREFIX COLUMNS                          
         BZ    *+10                                                             
         MVC   L'OPTDIS(L'PROFDIS,R4),TEMP                                      
         TM    OPTDIND,OPTDIADQ    TEST SUFFIX COLUMNS                          
         BZ    *+8                                                              
         LA    R4,L'PROFDIS(R4)    ADD THEM TO WHATEVER'S THERE                 
         MVC   0(L'OPTDIS,R4),OPTDIS                                            
BLDDIS14 XC    SCOLDIS,SCOLDIS     SAVE COLUMNS CURRENTLY DISPLAYED             
         LA    R0,L'SCOLDIS                                                     
         SR    R1,R1                                                            
         LA    RE,TEMP                                                          
         LA    RF,SCOLDIS                                                       
         XC    WORK(L'DISMASK),WORK                                             
BLDDIS16 SR    R2,R2                                                            
         ICM   R2,1,0(RE)          TEST/SET DISPLAY LINE ELEMENT NUMBER         
         BZ    BLDDIS18                                                         
         CH    R2,=Y(DISTABN)      TEST WITHIN DISPLAY RANGE                    
         BH    BLDDIS18                                                         
         BCTR  R2,0                                                             
         MH    R2,=Y(DISTABL)      DISNUM-1*4                                   
         A     R2,ADISTAB                                                       
         USING DISTABD,R2          R2=A(DISPLAY ELEMENT TABLE ENTRY)            
         MVC   WORK+L'DISMASK(L'DISMASK),DISMASK                                
         NC    WORK+L'DISMASK(L'DISMASK),WORK                                   
         BNZ   BLDDIS18            SKIP DUPLICATED COLUMN                       
         OC    WORK(L'DISMASK),DISMASK                                          
         MVC   0(1,RF),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
BLDDIS18 LA    RE,1(RE)                                                         
         BCT   R0,BLDDIS16                                                      
         STC   R1,SCOLDNO                                                       
         MVC   TEMP(L'SCOLDIS),SCOLDIS                                          
*                                                                               
BLDDIS20 XC    DISDISP(DISDISPL),DISDISP                                        
         MVC   DISHED1,SPACES                                                   
         MVC   DISHED2,SPACES                                                   
         LA    R4,TEMP             R4=A(KEY PROFILE)                            
         SR    RE,RE               RE=DISPLACEMENT TO DISPLAY VALUE             
BLDDIS22 SR    R2,R2                                                            
         ICM   R2,1,0(R4)          R2=DISPLAY LINE ELEMENT NUMBER               
         BZ    BLDDIS32            NO MORE DISPLAY COLUMNS                      
         LR    RF,R2                                                            
         LA    RF,DISDISP-1(RF)    RF=A(DISPLAY ELEMENT DISPLACEMENT)           
         BCTR  R2,0                                                             
         MH    R2,=Y(DISTABL)      DISNUM-1*4                                   
         A     R2,ADISTAB                                                       
         USING DISTABD,R2          R2=A(DISPLAY ELEMENT TABLE ENTRY)            
         MVC   LARFADDR,DISADR1                                                 
         LA    R3,L'FVIHDR(RE)     ADD ON LENGTH OF FIELD HEADER                
         STC   R3,0(RF)                                                         
         IC    R3,DISLEN           TAKE LENGTH OF ELEMENT                       
         AR    R3,RE               ADD DISPLACEMENT                             
         CLM   R3,1,=AL1(L'DISLLINE)                                            
         BNH   *+12                                                             
         MVI   0(RF),0             DISPLAY FULL - RESET DISPLACEMENT            
         B     BLDDIS32            AND EXIT                                     
         ICM   R3,1,DISTLEN        TEST/SET LENGTH OF TEXT                      
         BNZ   *+8                                                              
         IC    R3,DISLEN           ELSE USE LENGTH OF ELEMENT                   
         BCTR  R3,0                                                             
         LA    R1,DISHED1(RE)                                                   
         EX    0,LARF                                                           
         MVC   0(0,R1),0(RF)                                                    
         EX    R3,*-6                                                           
         ICM   R0,3,DISADR2                                                     
         BZ    BLDDIS24                                                         
         TM    CURIND1,CUR1SINC    CURRENCY CODE WILL USE HEADING 2             
         BO    BLDDIS24                                                         
         MVC   LARFADDR,DISADR2                                                 
         LA    R1,DISHED2(RE)      SECOND HEADING                               
         EX    0,LARF                                                           
         MVC   0(0,R1),0(RF)                                                    
         EX    R3,*-6                                                           
         LA    R1,DISHED1(RE)                                                   
BLDDIS24 TM    DISTIND,DISTICUR    TEST COLUMN IS A CURRENCY VALUE              
         BO    BLDDIS26                                                         
         LA    RF,0(R3,R1)                                                      
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R3,*-12                                                          
         ST    R1,FULL             SAVE START POINT                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R3,*-10                                                          
         L     R1,FULL                                                          
         LA    R1,DISHED2-DISHED1(R1)                                           
         MVI   0(R1),C'-'                                                       
         BCT   R3,*+8                                                           
         B     *+14                COLUMN IS 1 CHARACTER WIDE                   
         MVC   1(0,R1),0(R1)                                                    
         EX    R3,*-6                                                           
         B     BLDDIS28                                                         
*                                                                               
BLDDIS26 TM    CURIND1,CUR1SINC                                                 
         BZ    BLDDIS28                                                         
         LA    R1,DISHED2(RE)                                                   
         SH    R3,=Y(L'CURTCUR+1)                                               
         BNM   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R3                                                            
         MVI   0(R1),C'('                                                       
         MVC   1(L'CURTCUR,R1),FORCURT+(CURTCUR-CURTABD)                        
         MVI   1+L'CURTCUR(R1),C')'                                             
*                                                                               
BLDDIS28 IC    R3,DISLEN           RESET LENGTH OF ELEMENT                      
         LA    RE,1(RE,R3)         RE=DISPLACEMENT OF NEXT ELEMENT              
*                                                                               
BLDDIS30 LA    R4,1(R4)            BUMP TO NEXT FIELD                           
         B     BLDDIS22                                                         
*                                                                               
BLDDIS32 DS    0H                                                               
         B     ROU1E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSACTION RECORD & PRINT JOURNAL LINE IF REQUIRED         *         
*                                                                     *         
* NTRY - POSTVALS CONTAIN VARIABLE RECORD DATA                        *         
*      - R1=A(LIST OF ANALYSIS POINTERS) OR ZERO (NO POINTERS REQD):- *         
*           HIGH ORDER BYTE IS POINTER STATUS (X'FF'=END OF LIST)     *         
*           LOW ORDER THREE BYTES POINT TO UNIT/LEDGER/ACCOUNT CODE   *         
*                                                                     *         
* E.G. - GOTO1 ABLDTRN,0                                              *         
* E.G. - GOTO1 ABLDTRN,DMCB,(0,ULA1),('TRNSDR',ULA2),(X'FF',0)        *         
***********************************************************************         
         SPACE 1                                                                
BLDTRN   L     R2,AIO1                                                          
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         LTR   R1,R1               TEST SPECIAL CALL                            
         BM    BLDTRN40                                                         
         XC    TRNRECD(256),TRNRECD                                             
*                                                                               
         MVC   TRNKEY,SPACES       BUILD TRANSACTION RECORD KEY                 
         MVC   TRNKCPY,COMPANY                                                  
         MVC   TRNKUNT(L'TRNKCULA-1),POSTACT                                    
         MVC   TRNKCULC,POSTCAC                                                 
         MVC   TRNKDATE,POSTDATE                                                
         MVC   TRNKREF,POSTREF                                                  
         MVI   TRNKSBR,0                                                        
*                                                                               
         LA    R3,TRNRECD          BUILD TRANSACTION ELEMENT                    
         AH    R3,DATADISP                                                      
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,POSTDATE                                                 
         MVC   TRNREF,POSTREF                                                   
         MVI   TRNTYPE,30                                                       
         MVC   TRNMOS,BATMON                                                    
         MVC   TRNBREF,BATREF                                                   
         MVC   TRNSTAT,POSTSTAT                                                 
         ZAP   TRNAMNT,POSTAMNT                                                 
         MVC   TRNOFFC,POSTOFFC                                                 
         LA    RE,L'POSTNARR-1     CALCULATE LENGTH OF NARRATIVE                
         LA    RF,POSTNARR+L'POSTNARR-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         MVC   TRNNARR(0),POSTNARR                                              
         EX    RE,*-6                                                           
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LA    R3,TRNELD(RE)                                                    
*                                                                               
         ZAP   POSTAM2,PZERO       CLEAR POSTING AMOUNT (CURRENCY)              
         ICM   RF,15,OVERXTRA      TEST ANY EXTRA ELEMENTS REQUIRED             
         BZ    BLDTRN04                                                         
         SR    RE,RE                                                            
BLDTRN02 CLI   0(RF),0             TEST END OF ELEMENT LIST                     
         BE    BLDTRN04                                                         
         IC    RE,1(RF)                                                         
         MVC   0(0,R3),0(RF)                                                    
         EX    RE,*-6              MOVE ELEMENT TO TRANSACTION RECORD           
         MVI   0(RF),0             USER MUST RESET NEXT TIME THROUGH            
         CLI   0(R3),AFCELQ        TEST FOREIGN CURRENCY ELEMENT                
         BNE   *+10                                                             
         ZAP   POSTAM2,AFCAMNT-AFCELD(L'AFCAMNT,R3)                             
         AR    RF,RE               BUMP TO NEXT INPUT ELEMENT                   
         AR    R3,RE               BUMP TO NEXT OUTPUT ELEMENT                  
         B     BLDTRN02                                                         
*                                                                               
BLDTRN04 LTR   R1,R1               TEST ANALYSIS POINTERS PASSED                
         BZ    BLDTRN18                                                         
         USING APEELD,R3           R3=A(ANALYSIS POINTER ELEMENT)               
         XC    APEELD(256),APEELD                                               
         MVI   APEEL,APEELQ        INITIALISE ANALYSIS POINTER ELEMENT          
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
         LA    R4,APENTRY                                                       
                                                                                
         USING APENTRY,R4          R4=A(ANALYSIS POINTER SUB-ELEMENT)           
BLDTRN06 CLI   0(R1),X'FF'         TEST END OF POINTER LIST                     
         BE    BLDTRN16                                                         
         L     RE,0(R1)            RE=A(ANALYSIS UNIT/LEDGER/ACCOUNT)           
         LA    RE,0(RE)                                                         
                                                                                
         CLC   POSTACT,0(RE)                                                    
         BE    BLDTRN07                                                         
         CLC   POSTCAC+1(L'POSTACT),0(RE)                                       
         BE    BLDTRN07                                                         
                                                                                
         LA    RF,L'ACTKCULA-2(RE)                                              
         LA    R0,L'ACTKCULA-2                                                  
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BNE   *+12                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                BAD ANALYSIS POINTER PASSED                  
         SR    RF,RE               RF=L'ACCOUNT-1                               
         MVC   APENACT(0),0(RE)    MOVE ANALYSIS POINTER TO ELEMENT             
         EX    RF,*-6                                                           
         LA    RF,APELN2Q+1(RF)                                                 
         STC   RF,APENLEN          SET LENGTH OF SUB-ELEMENT                    
         MVC   APENSTAT,0(R1)      SET DR/CR STATUS                             
         SR    RE,RE                                                            
         IC    RE,APELN            UPDATE TOTAL ELEMENT LENGTH                  
         AR    RE,RF                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         LA    RE,1(RE)                                                         
         STC   RE,APENUM                                                        
         AR    R4,RF               R4=A(NEXT ANALYSIS SUB-ELEMENT)              
         MVI   APENSTAT,0                                                       
*                                                                               
BLDTRN07 LA    R1,4(R1)            BUMP TO NEXT ANALYSIS POINTER                
         B     BLDTRN06                                                         
*                                                                               
BLDTRN16 SR    R0,R0                                                            
         IC    R0,APELN                                                         
         CLI   APELN,APELN1Q       TEST ANY POINTERS IN ELEMENT                 
         BE    *+6                 NO                                           
         AR    R3,R0                                                            
         MVI   0(R3),0                                                          
         DROP  R3,R4                                                            
*                                                                               
BLDTRN18 CLI   UPDMODE,UPDMPROC    BUILD ALLOCATION ELEMENTS                    
         BNE   BLDTRN54                                                         
*                                                                               
         CLI   TSARSOR,SORELQ      TEST SOURCE ELEMENT HELD                     
         BNE   BLDTRN20                                                         
         USING SORELD,R3                                                        
         XC    SORELD(SORALNQ+1),SORELD  CLEAR FOR LONGER ELEMENT + EOR         
         SR    R1,R1                                                            
         IC    R1,TSARSOR+(SORLN-SORELD)                                        
         BCTR  R1,0                                                             
         MVC   SORELD(0),TSARSOR                                                
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)         R3=NEW EOR                                   
*                                                                               
BLDTRN20 CLC   TRNKUNT(L'RECVUL),RECVUL                                         
         BNE   BLDTRN54                                                         
         XC    TEMP(L'DUEDATE),TEMP                                             
         TM    POSTSTAT,TRNSDR     TEST POSTING A DEBIT                         
         BZ    BLDTRN26                                                         
         TM    TSARINDS,TSARITRF   TRANSFER - ONLY ORIGINAL DUE DATE            
         BO    *+14                                                             
         OC    TEMP(L'DUEDATE),TSARDUE2                                         
         BNZ   *+14                                                             
         OC    TEMP(L'DUEDATE),TSARDUED                                         
         BZ    BLDTRN26                                                         
         SR    R0,R0                                                            
         LA    R1,TRNRECD+ACCORFST BUILD/UPDATE DUE DATE ELEMENT                
         USING DUEELD,R1                                                        
         B     *+10                                                             
BLDTRN22 IC    R0,DUELN            LOCATE END OF RECORD                         
         AR    R1,R0                                                            
         CLI   DUEEL,DUEELQ        TEST DUE DATE ELEMENT FOUND                  
         BNE   BLDTRN24                                                         
         MVC   DUEDATE,TSARDUE2    SET NEW DUE DATE                             
         B     BLDTRN26            R3=A(EOR) STILL                              
BLDTRN24 CLI   DUEEL,0             TEST EOR                                     
         BNE   BLDTRN22                                                         
         XC    DUEELD(DUELNQ),DUEELD                                            
         MVI   DUEEL,DUEELQ        ADD DUE DATE ELEMENT                         
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,TEMP        SET ORIGINAL/NEW DUE DATE                    
         LA    R3,DUEELD+DUELNQ    R3=A(NEW EOR)                                
*                                                                               
BLDTRN26 CLI   TSARTYPE,TSARTOVR   TEST SPECIAL SCREEN ITEM                     
         BE    *+12                                                             
         CLI   TSARTYPE,TSARTRPL   OR REPLICANT ITEM                            
         BNE   BLDTRN32                                                         
         OC    TSARXSEQ,TSARXSEQ   TEST EXTENSION RECORD                        
         BZ    BLDTRN32                                                         
         L     R1,ATSARBLK         READ EXTENSION TSAR RECORD                   
         USING TSARD,R1                                                         
         MVC   STSRNUM,TSRNUM      SAVE CURRENT TSAR RECORD NUMBER              
         MVI   TSACTN,TSARDH                                                    
         MVC   TSARLEN,=Y(TSARMAXL)                                             
         XC    TSARKEY(TSARKEYL),TSARKEY                                        
         MVC   TS2TYP,=AL2(FFFF)                                                
         MVC   TS2SEQ,TSARXSEQ                                                  
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         USING NOTELD,R3                                                        
         LA    R4,TS2ELE                                                        
         USING TS2ELE,R4                                                        
         LA    R0,TS2ELEN                                                       
BLDTRN28 OC    TS2ELE(TS2ELEL),TS2ELE                                           
         BZ    BLDTRN30                                                         
         MVI   NOTEL,NOTELQ        BUILD NOTE ELEMENT                           
         MVI   NOTLN,NOTLN1Q       SET MINIMUM LENGTH                           
         MVC   NOTDATE,TS2DATE                                                  
         MVC   NOTREF,TS2REF                                                    
         LA    RE,TS2NOTE                                                       
         LA    RF,TS2NOTE+(L'TS2NOTE-1)                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE                                                            
         BM    BLDTRN30                                                         
         MVC   NOTNOTE(0),TS2NOTE                                               
         EX    RF,*-6                                                           
         LA    RF,NOTLN1Q+1(RF)                                                 
         STC   RF,NOTLN                                                         
         AR    R3,RF               R3=A(NEXT ELEMENT)                           
BLDTRN30 LA    R4,TS2ELEL(R4)      BUMP TO NEXT TSAR DATA ELEMENT               
         DROP  R3,R4                                                            
         BCT   R0,BLDTRN28                                                      
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         GOTO1 ATSARGET,STSRNUM    READ SAVED TSAR RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RALELD,R3                                                        
BLDTRN32 XC    RALELD(RALDATA-RALELD),RALELD                                    
         MVI   RALEL,RALELQ                                                     
*                                                                               
         TM    TSARINDS,TSARIWOF   HANDLE WRITE-OFFS                            
         BZ    BLDTRN34                                                         
         MVI   RALLN,RALWOFLQ                                                   
         MVI   RALTYPE,RALTWOF                                                  
         MVC   RALWREF,WOFREF                                                   
         MVC   RALWULA,WOFF                                                     
         OC    RALWDAT,WOFDATE     TEST/SET WRITE-OFF DATE                      
         BNZ   *+16                                                             
         MVC   RALWDAT,TODAYP      NOT GIVEN - USE TODAY                        
         MVC   RALWREF,TSARREF     AND BILL REFERENCE                           
*&&UK                                                                           
         CLI   AGYCTRY,CTRYGER     TEST GERMANY                                 
         BNE   BLDTRN46                                                         
         MVC   RALWREF,BANKREF     USE BANK VALUES                              
         GOTO1 VDATCON,DMCB,BANKDATE,(1,RALWDEP)                                
         MVC   RALWDAT,RALWDEP                                                  
*&&                                                                             
         B     BLDTRN46                                                         
*                                                                               
BLDTRN34 TM    TSARINDS,TSARIOFS   HANDLE OFFSETS                               
         BZ    BLDTRN36                                                         
         MVI   RALLN,RALOFSLQ                                                   
         MVI   RALTYPE,RALTOFS                                                  
         OC    RALODAT,OFSDATE     TEST/SET OFFSET DATE                         
         BNZ   *+10                                                             
         MVC   RALODAT,TODAYP      NOT GIVEN - USE TODAY                        
         B     BLDTRN46                                                         
*                                                                               
BLDTRN36 TM    TSARINDS,TSARITRF   HANDLE TRANSFER TO......                     
         BZ    BLDTRN38                                                         
         MVI   RALLN,RALTRFLQ                                                   
         MVI   RALTYPE,RALTTTO     SET "TRANSFER TO" TYPE                       
         MVC   RALTMOS,TSARPMOS                                                 
         MVC   RALTDAT,TODAYP                                                   
         MVC   RALTULA(L'RECVUL),RECVUL                                         
         MVC   RALTACT,TSARTRFA                                                 
         B     BLDTRN46                                                         
*                                                                               
BLDTRN38 MVI   RALLN,RALALCLQ      HANDLE REGULAR ALLOCATIONS                   
         MVI   RALTYPE,RALTALC                                                  
         MVC   RALAREF,BANKREF                                                  
         GOTO1 VDATCON,DMCB,BANKDATE,(1,RALADEP)                                
         CLI   AGYCTRY,CTRYGER     GERMANS DON'T HAVE CHEQUE DATE               
         BNE   *+14                                                             
         MVC   RALADAT,RALADEP                                                  
         B     BLDTRN46                                                         
         GOTO1 VDATCON,DMCB,CHQDATE,(1,RALADAT)                                 
         B     BLDTRN46                                                         
*                                                                               
BLDTRN40 CLI   UPDMODE,UPDMPROC    HANDLE TRANSFER FROM......                   
         BNE   BLDTRN56                                                         
         CLC   TRNKUNT(L'RECVUL),RECVUL                                         
         BNE   BLDTRN56                                                         
         TM    TSARINDS,TSARITRF                                                
         BZ    BLDTRN56                                                         
         GOTO1 VHELLO,DMCB,(C'D',ACCFIL),('RALELQ',TRNRECD),0,0                 
*                                                                               
         LA    R3,TRNRECD+ACCORFST                                              
         SR    R0,R0                                                            
         OC    TSARDUE2,TSARDUE2   TEST NEW DUE DATE                            
         BZ    BLDTRN44                                                         
         USING DUEELD,R3                                                        
BLDTRN42 IC    R0,DUELN            LOCATE END OF RECORD                         
         AR    R3,R0                                                            
         CLI   DUEEL,DUEELQ        TEST DUE DATE ELEMENT FOUND                  
         BNE   *+14                                                             
         MVC   DUEDATE,TSARDUE2    SET NEW DUE DATE                             
         B     BLDTRN44                                                         
         CLI   DUEEL,0             TEST EOR                                     
         BNE   BLDTRN42                                                         
         XC    DUEELD(DUELNQ),DUEELD                                            
         MVI   DUEEL,DUEELQ        BUILD DUE DATE ELEMENT                       
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,TSARDUE2                                                 
         MVI   DUEELD+DUELNQ,0     RESET EOR                                    
*                                                                               
         USING RALELD,R3                                                        
BLDTRN44 IC    R0,RALLN            LOCATE END OF RECORD                         
         AR    R3,R0                                                            
         CLI   RALEL,0                                                          
         BNE   BLDTRN44                                                         
         XC    RALELD(RALTRFLQ+1),RALELD                                        
         MVI   RALEL,RALELQ        SET "TRANSFER FROM" TYPE                     
         MVI   RALLN,RALTRFLQ                                                   
         MVI   RALTYPE,RALTTFR                                                  
         MVC   RALTMOS,TSARPMOS                                                 
         MVC   RALTDAT,TODAYP                                                   
         MVC   RALTULA,RECVCACT+1                                               
         LA    R3,RALTRFLQ(R3)                                                  
         B     BLDTRN54                                                         
*                                                                               
BLDTRN46 SR    R0,R0               POINT TO NEW END OF RECORD                   
         IC    R0,RALLN                                                         
         AR    R3,R0                                                            
         MVI   0(R3),0                                                          
*                                                                               
         TM    TSARINDS,TSARITRF+TSARIOFS+TSARIWOF                              
         BNZ   BLDTRN48                                                         
         OC    BANK,BANK           TEST BANK ACCOUNT KNOWN                      
         BZ    BLDTRN48                                                         
         USING SPAELD,R3                                                        
         XC    SPAELD(SPALNQ+1),SPAELD                                          
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATBANK    BANK ACCOUNT SPAEL                           
         MVC   SPAAULA,BANK                                                     
         LA    R3,SPALNQ(R3)                                                    
         OC    ANAL,ANAL           TEST ANALYSIS BANK ACCOUNT KNOWN             
         BZ    BLDTRN48                                                         
         XC    SPAELD(SPALNQ+1),SPAELD                                          
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATABNK    ANALYSIS BANK ACCOUNT SPAEL                  
         MVC   SPAAULA,ANAL                                                     
         LA    R3,SPALNQ(R3)                                                    
*                                                                               
BLDTRN48 DS    0H                  NEXT STANDARD SR POSTING ELEMENT             
*                                                                               
BLDTRN54 LA    R0,TRNRECD          SET TRANSACTION RECORD LENGTH                
         LA    R3,1(R3)                                                         
         MVI   0(R3),0                                                          
         SR    R3,R0                                                            
         STCM  R3,3,TRNRLEN                                                     
         CP    POSTAMNT,PZERO      TEST AMOUNT WITH RECORD BUILT                
         BNE   BLDTRN56                                                         
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    BLDTRNX                                                          
         CP    POSTAM2,PZERO       TEST FOR AMOUNT (CURRENCY)                   
         BE    BLDTRNX                                                          
*                                                                               
BLDTRN56 DS    0H                                                               
*&&US                                                                           
         TM    COMPSTA5,CPYSNCST   TEST NEW COSTING POSTINGS                    
         BZ    BLDTRN62                                                         
         CLC   BATOFF,SPACES       TEST ANALYSIS OFFICE GIVEN                   
         BNH   BLDTRN62                                                         
         LA    R3,TRNRECD                                                       
         AH    R3,DATADISP                                                      
         USING TRNELD,R3                                                        
         CLC   TRNOFFC,BATOFF      TEST TRANSACTION OFFICE MATCHES              
         BE    BLDTRN62                                                         
         SR    R0,R0                                                            
         USING ANOELD,R3                                                        
BLDTRN58 CLI   ANOEL,ANOELQ                                                     
         BE    BLDTRN60                                                         
         CLI   ANOEL,0                                                          
         BE    *+14                                                             
         IC    R0,ANOLN                                                         
         AR    R3,R0                                                            
         B     BLDTRN58                                                         
         MVI   ANOEL,ANOELQ        BUILD NEW ANALYSED OFFICE ELEMENT            
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER                                                  
         MVC   ANOOFFC,BATOFF                                                   
         MVI   ANOELD+ANOLNQ,0                                                  
         LA    R3,ANOELD+ANOLNQ+1                                               
         LA    R0,TRNRECD                                                       
         SR    R3,R0                                                            
         STCM  R3,3,TRNRLEN                                                     
         B     BLDTRN62                                                         
*                                                                               
BLDTRN60 MVI   ANOTYPE,ANOTPER     REFRESH ANALYSED OFFICE ELEMENT              
         MVC   ANOOFFC,BATOFF                                                   
         DROP  R3                                                               
*&&                                                                             
BLDTRN62 TM    TWAMODE,TWAMDRFT    TEST DRAFT/LIVE REQUEST                      
         BNZ   BLDTRN74                                                         
         USING TIDELD,R3                                                        
         LA    R3,TRNRECD                                                       
         AH    R3,DATADISP                                                      
         SR    R0,R0                                                            
         IC    R0,TIDLN                                                         
         AR    R3,R0                                                            
         CLI   TIDEL,0                                                          
         BNE   *-10                                                             
*                                                                               
         MVI   TIDEL,TIDELQ        ATTACH A TERMINAL ID ELEMENT                 
         MVI   TIDLN,TIDLNQ                                                     
         L     RF,AUTL                                                          
         MVC   TID,TSYM-UTLD(RF)                                                
         LA    R3,TIDLNQ(R3)                                                    
*                                                                               
         OC    TWAPASS#,TWAPASS#   ATTACH A PERSON ID ELEMENT IF THE            
         BZ    BLDTRN63            USER LOGGED ON WITH A PASSWORD               
         USING PIDELD,R3                                                        
         XC    PIDELD(PIDLNQ+1),PIDELD                                          
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,TWAPASS#                                                   
         LA    R3,PIDLNQ(R3)                                                    
*                                                                               
BLDTRN63 DS    0H                                                               
         CLC   TRNKUNT(2),=C'SR'   CHECK IF SR                                  
         BNE   BLDTRN64                                                         
*        CLC   TRNKCACT,=CL12'UNAPPLIED'                                        
*        BNE   *+12                                                             
*        TM    TSARINDS,TSARIOFS   IS THIS AN OFFSET?                           
*        BNO   BLDTRN64             IF NOT -> SKIP                              
         OC    TSARSYS,TSARSYS                                                  
         BZ    BLDTRN64                                                         
         USING MDTELD,R3                                                        
         XC    MDTELD(MDTLNQ),MDTELD                                            
         MVI   MDTEL,MDTELQ                                                     
         MVI   MDTLN,MDTLNQ                                                     
         MVC   MDTSYS,TSARSYS                                                   
         MVC   MDTMED,TSARMED                                                   
         MVC   MDTCLI,TSARCLI                                                   
         LA    R3,MDTLNQ(R3)                                                    
*                                                                               
BLDTRN64 OC    TSARINVN,TSARINVN   CHECK FOR INVOICE NUMBER                     
         BZ    BLDTRN66                                                         
         USING FFTELD,R3                                                        
         MVC   0(L'TSARINVN,R3),TSARINVN  MOVE IN WHOLE ELEMENT                 
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R3,RF               POINT PAST END OF ELEM                       
*                                                                               
BLDTRN66 OC    TSARKREF,TSARKREF   CHECK FOR INVOICE NUMBER                     
         BZ    BLDTRN68                                                         
         USING FFTELD,R3                                                        
         MVC   0(L'TSARKREF,R3),TSARKREF  MOVE IN WHOLE ELEMENT                 
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         AR    R3,RF               POINT PAST END OF ELEM                       
*                                                                               
         USING FFTELD,R3                                                        
BLDTRN68 OC    TSARBTYC,TSARBTYC   CHECK FOR TRANSACTION BILL TYPE CODE         
         BZ    BLDTRN69                                                         
         MVI   FFTEL,FFTELQ        X'DB'                                        
         MVI   FFTTYPE,FFTTMXTY    MEDIA TRANSFER INFO (67)                     
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,FFTMLNQ     ACTUAL LENGTH OF TEXT                        
         LA    RE,FFTLN1Q          ELEMENT OVERHEAD                             
         LA    R1,L'FFTDLEN                                                     
         AR    RE,R1                                                            
         LA    R1,FFTMLNQ          LENGTH OF DATA                               
         AR    RE,R1                                                            
         STC   RE,FFTLN            LENGTH OF ELEMENT                            
         MVC   FFTMXTYP,TSARBTYC                                                
         AR    R3,RE               POINT PAST END OF ELEM                       
                                                                                
         USING UFSELD,R3                                                        
BLDTRN69 OC    TSARPOEL,TSARPOEL   CHECK FOR UFSELQ ELE FOR PO                  
         JZ    BLDTRN70                                                         
         MVI   UFSEL,UFSELQ        ELEMENT CODE X'A2'                           
         MVC   UFSCODE,=C'PO'      SET CODE TO 'PO'                             
         MVC   UFSDESC,TSARPODS    PO DESCRIPTION                               
         MVI   UFSMXLN,L'TSARPONO  MAX LENGTH OF PO NUMBER                      
         MVC   UFSDATA(L'TSARPONO),TSARPONO FIXED LENGTH PO                     
         MVI   UFSLN,UFSLN1Q+L'TSARPONO SET ELEMENT LENGTH                      
         MVC   UFSSTAT,TSARPSTS    PO STATUS                                    
         LLC   RE,UFSLN                                                         
         AR    R3,RE               POINT PAST END OF ELEM                       
                                                                                
BLDTRN70 MVI   0(R3),0             SET END OF RECORD AND LENGTH                 
         LA    R3,1(R3)                                                         
         LA    R0,TRNRECD                                                       
         SR    R3,R0                                                            
         STCM  R3,3,TRNRLEN                                                     
*                                                                               
         LA    RF,UPDBLOCK                                                      
         USING TRNBLKD,RF          RF=A(ADDTRN BLOCK)                           
         STCM  R2,15,TRNREC        SET A(TRANSACTION RECORD)                    
         ICM   RE,3,TRNBSEQN       INCREMENT TRANSACTION SEQUENCE#              
         LA    RE,1(RE)                                                         
         STCM  RE,3,TRNBSEQN                                                    
         MVC   TRNCACNM,POSTCACN   SET CONTRA-ACCOUNT NAME                      
         OI    TRNINDS2,TRNIADDG                                                
         GOTO1 VADDTRN,TRNBLKD     ADD TRANSACTION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
BLDTRN74 OC    PRTSUB,PRTSUB       TEST REPORT WANTED                           
         BZ    BLDTRNX                                                          
         L     R2,AREPWRK                                                       
         LA    R2,REPP1-REPD(R2)                                                
         USING JRNLINED,R2         R2=A(PRINT LINE 1)                           
         L     R3,AIO1                                                          
         USING TRNRECD,R3          R3=A(TRANSACTION RECORD)                     
*&&US                                                                           
         CLC   TRNKUNT,BANKUL      TEST UNIT S POSTING                          
         BE    *+8                 YES - PRINT AS NORMAL                        
         LA    R2,1(R2)            NO - INDENT ACCOUNT AND CONTRA               
*&&                                                                             
         MVC   JRNACT,TRNKUNT                                                   
         MVC   JRNCAC,TRNKCUNT                                                  
         LA    R0,3                                                             
         CLI   JRNCAC,C' '                                                      
         BNE   *+14                                                             
         MVC   JRNCAC,JRNCAC+1                                                  
         BCT   R0,*-14                                                          
*&&US                                                                           
         CLC   TRNKUNT,BANKUL      TEST UNIT S POSTING                          
         BE    *+6                                                              
         BCTR  R2,0                NO - RE-POSITION R2                          
*&&                                                                             
         AH    R3,DATADISP                                                      
         USING TRNELD,R3           R3=A(TRANSACTION ELEMENT)                    
         MVC   JRNOFFC,TRNOFFC                                                  
         GOTO1 VDATCON,DMCB,(1,TRNDATE),(17,JRNDATE)                            
         MVC   JRNREF,TRNREF                                                    
         LA    RF,JRNDR            DR INTO DEBIT COLUMN                         
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    RF,JRNCR            CR INTO CREDIT COLUMN                        
         CURED TRNAMNT,(L'JRNDR,(RF)),AGYCURT,MINUS=YES                         
         TM    UPDINDS,UPDIPRTS    TEST RECORD STATUS REQUIRED                  
         BZ    BLDTRN76                                                         
         NI    UPDINDS,255-UPDIPRTS                                             
         GOTO1 ABLDSTA,JRNSTAT1    BUILD STATUS DISPLAY                         
*                                                                               
BLDTRN76 TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    BLDTRN80                                                         
         SR    R0,R0                                                            
         LA    R4,TRNELD                                                        
         USING AFCELD,R4                                                        
BLDTRN78 IC    R0,AFCLN            LOCATE FOREIGN CURRENCY AMOUNT               
         AR    R4,R0                                                            
         CLI   AFCEL,0                                                          
         BE    BLDTRN80                                                         
         CLI   AFCEL,AFCELQ                                                     
         BNE   BLDTRN78                                                         
         LA    RF,JRNDR2           DR INTO DEBIT COLUMN                         
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    RF,JRNCR2           CR INTO CREDIT COLUMN                        
         CURED AFCAMNT,(L'JRNDR2,(RF)),FORCURT,MINUS=YES                        
BLDTRN80 L     R1,AREPWRK                                                       
         GOTO1 VREPORT             PRINT THE LINE                               
*                                                                               
BLDTRNX  B     ROU1E                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE BUILDS TRANSACTION NARRATIVE                                *         
***********************************************************************         
         SPACE 1                                                                
BLDNAR   LR    R2,R1               R2=A(NARRATIVE AREA)                         
         MVC   0(L'AC@CHKC,R2),AC@CHKC                                          
         LA    R2,L'AC@CHKC-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                 GET TO FIRST CHARACTER                       
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'BANKREF,R2),BANKREF                                          
         LA    R2,L'BANKREF-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@DATED,R2),AC@DATED                                        
         LA    R2,L'AC@DATED-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         GOTO1 VDATCON,DMCB,(0,CHQDATE),(8,(R2))                                
         CLI   0(R2),C' '                                                       
         BNE   *+10                                                             
         MVC   0(9,R2),1(R2)                                                    
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@DPSON,R2),AC@DPSON                                        
         LA    R2,L'AC@DPSON-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         GOTO1 VDATCON,DMCB,(0,BANKDATE),(8,(R2))                               
         CLI   0(R2),C' '                                                       
         BNE   *+10                                                             
         MVC   0(9,R2),1(R2)                                                    
BLDNARX  B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD STATUS DISPLAY FROM A TSAR RECORD                             *         
*                                                                     *         
* NTRY - R1=A(4 BYTE STATUS DISPLAY AREA)                             *         
***********************************************************************         
         SPACE 1                                                                
BLDSTA   MVC   0(1,R1),AC8SPCL     BYTE 1 - ALLOCATION TYPE                     
         CLI   TSARTYPE,TSARTOVR                                                
         BE    BLDSTA02                                                         
         MVI   0(R1),C'.'                                                       
         TM    TSARIND2,TSAR2QRD   TEST QUERIED                                 
         BZ    *+14                                                             
         MVC   0(1,R1),AC@QUERD                                                 
         B     BLDSTA02                                                         
         TM    TSARIND2,TSAR2HLD   TEST HELD                                    
         BZ    *+14                                                             
         MVC   0(1,R1),AC@HELD                                                  
         B     BLDSTA02                                                         
*&&UK                                                                           
         TM    TSARIND2,TSAR2CLR   TEST CLEARING                                
         BZ    *+14                                                             
         MVC   0(1,R1),AC2CLRNG                                                 
         B     BLDSTA02                                                         
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARPOST,PZERO      TEST ANY ALLOCATION                          
         BE    BLDSTA02                                                         
         MVC   0(1,R1),AC@ALCTL                                                 
*&&US                                                                           
         TM    TSARINDS,TSARILAT                                                
         BZ    *+14                                                             
         MVC   0(1,R1),AC@LATE                                                  
         B     BLDSTA02                                                         
*&&                                                                             
         TM    TSARINDS,TSARIWOF+TSARIOFS+TSARITRF                              
         BZ    BLDSTA02                                                         
         MVC   0(1,R1),AC@OFFST                                                 
         TM    TSARINDS,TSARIOFS                                                
         BNZ   BLDSTA02                                                         
         MVC   0(1,R1),AC@WRTF                                                  
         TM    TSARINDS,TSARIWOF                                                
         BNZ   BLDSTA02                                                         
         MVC   0(1,R1),AC@XFR                                                   
         TM    TSARINDS,TSARITRF                                                
         BNZ   BLDSTA02                                                         
         DC    H'0'                                                             
*                                                                               
BLDSTA02 MVC   1(1,R1),AC@YES      BYTE 2 - ALLOCATION STATUS                   
         CLI   TSARTYPE,TSARTOVR   TEST OVERPAYMENT ITEM                        
         BE    BLDSTA06                                                         
         TM    TSARINDS,TSARITRF   TEST TRANSFER                                
         BNZ   BLDSTA06            ALWAYS FULLY ALLOCATED                       
         MVI   1(R1),C'.'                                                       
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARCR2,PZERO       TEST PRIOR CREDITS (CURRENCY)                
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARCR,PZERO        TEST PRIOR CREDITS                           
         BE    *+8                                                              
         MVI   1(R1),C'*'          SET SET PART ALLOCATED PREVIOUSLY            
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARPOST,PZERO      TEST ALLOCATED THIS TIME                     
         BE    BLDSTA06                                                         
         ZAP   DUB,TSARDR          YES - CALCULATE FULL AMOUNT IN DUB           
         SP    DUB,TSARCR                                                       
         TM    TSARINDS,TSARIGRS   TEST GROSS ALLOCATION                        
         BNZ   *+10                                                             
         AP    DUB,TSARDISC                                                     
         ZAP   DUB2,TSARPOST                                                    
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    BLDSTA04                                                         
         ZAP   DUB,TSARDR2         YES - USE CURRENCY AMOUNTS                   
         SP    DUB,TSARCR2                                                      
         TM    TSARINDS,TSARIGRS                                                
         BNZ   *+10                                                             
         AP    DUB,TSARDI2                                                      
         ZAP   DUB2,TSARPO2                                                     
*&&                                                                             
BLDSTA04 MVC   1(1,R1),AC@YES      Y MEANS FULLY ALLOCATED THIS TIME            
         CP    DUB2,DUB                                                         
         BE    *+8                                                              
         MVI   1(R1),C'+'          + MEANS PARTLY ALLOCATED THIS TIME           
*                                                                               
BLDSTA06 MVI   2(R1),C'.'          BYTE 3 - DISCOUNT/SURCHARGE STATUS           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARDI2,PZERO       TEST ANY DISCOUNT (CURRENCY)                 
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARDISC,PZERO      TEST ANY DISCOUNT HERE                       
         BE    BLDSTA08                                                         
         MVC   2(1,R1),AC@DISS     SET DISCOUNT PRESENT                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARPOST,PZERO      TEST ALLOCATED                               
         BE    BLDSTA08                                                         
         MVC   2(1,R1),AC@NET      SET DISCOUNT TAKEN                           
         TM    TSARINDS,TSARIGRS                                                
         BZ    *+10                                                             
         MVC   2(1,R1),AC@GROSS    SET DISCOUNT NOT TAKEN                       
*                                                                               
BLDSTA08 MVI   3(R1),C'.'          BYTE 4 - MEMORANDUM STATUS                   
         OC    TSARXSEQ,TSARXSEQ   TEST EXTENSION RECORD                        
         BNZ   *+12                                                             
         TM    TSARIND2,TSAR2MEM   TEST MEMORANDUM HELD ON FILE                 
         BZ    *+10                                                             
         MVC   3(1,R1),AC@MEMO     SET MEMORANDUM HELD                          
*                                                                               
BLDSTA10 B     ROU1E                                                            
         EJECT                                                                  
***********************************************************************         
* READ AND MARK FILE TRANSACTIONS FOR QUERY/HOLD/DUE DATES            *         
***********************************************************************         
         SPACE 1                                                                
MRKTRN   XC    UPDRECN,UPDRECN     INITIALISE RECORD COUNT                      
MRKTRN02 LH    R1,UPDRECN          BUMP RECORD NUMBER                           
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX           TEST ALL MAIN RECORDS PROCESSED              
         BH    ROU1E                                                            
         STH   R1,UPDRECN                                                       
         GOTO1 ATSARGET,UPDRECN                                                 
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T READ RECORD                     
         CLI   TSARTYPE,TSARTTRN                                                
         BNE   MRKTRN02                                                         
         LA    R2,KEY                                                           
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,RECVCACT                                                
         MVC   TRNKCULC,TSARCAC                                                 
         TM    COMPSTA4,CPYSOFF2   TEST TWO CHARACTER OFFICES IN USE            
         BZ    *+18                                                             
         CLI   FILEFORM,ISDAQ      TEST NEW FILE IN USE                         
         BNE   *+10                                                             
         MVC   TRNKOFF,TSAROFFC                                                 
         MVC   TRNKDATE,TSARDAT                                                 
         MVC   TRNKREF,TSARREF                                                  
         MVC   TRNKSBR,TSARSUB                                                  
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCFIL,TRNRECD,AIO1                 
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    UPDINDS,FF-(UPDITRUP)                                            
MRKTRN04 SR    R0,R0                                                            
         L     R2,AIO1             R2=A(TRANSACTION)                            
         LA    R3,TRNRECD+ACCORFST R3=A(FIRST ELEMENT)                          
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BZ    MRKTRN20                                                         
         OC    TSARDUE2,TSARDUE2   TEST NEW DUE DATE                            
         BZ    MRKTRN20                                                         
         TM    TSARINDS,TSARITRF   TEST BEING TRANSFERRED THIS TIME             
         BO    MRKTRN20                                                         
         USING DUEELD,R3                                                        
MRKTRN10 CLI   DUEEL,0             TEST EOR                                     
         BE    MRKTRN14                                                         
         CLI   DUEEL,DUEELQ        TEST DUE DATE ELEMENT                        
         BE    MRKTRN12                                                         
         IC    R0,DUELN                                                         
         AR    R3,R0                                                            
         B     MRKTRN10                                                         
MRKTRN12 CLC   DUEDATE,TSARDUE2    TEST DUE DATE HAS CHANGED                    
         BE    MRKTRN18                                                         
         MVC   DUEDATE,TSARDUE2    SET NEW DUE DATE                             
         B     MRKTRN16                                                         
*                                                                               
MRKTRN14 LA    R3,TEMP             BUILD DUE DATE ELEMENT                       
         XC    DUEELD(DUELNQ),DUEELD                                            
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,TSARDUE2    SET DUE DATE                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCFIL),TRNRECD,DUEELD,0,0                     
         CLI   12(R1),0                                                         
         BE    MRKTRN16                                                         
         DC    H'0'                                                             
*                                                                               
MRKTRN16 OI    UPDINDS,UPDITRUP    SET TO UPDATE TRANSACTION                    
*                                                                               
MRKTRN18 LA    R3,TRNRECD+ACCORFST RESET A(FIRST ELEMENT)                       
*                                                                               
         USING TRXELD,R3                                                        
MRKTRN20 CLI   TRXEL,0                                                          
         BE    MRKTRN24            ADD NEW ELEMENT, IF NECESSARY                
         CLI   TRXEL,TRXELQ                                                     
         BE    MRKTRN26            UPDATE ELEMENT, IF NECESSARY                 
         IC    R0,TRXLN                                                         
         AR    R3,R0                                                            
         B     MRKTRN20                                                         
*                                                                               
MRKTRN24 TM    TSARIND2,TSAR2QRD+TSAR2HLD+TSAR2CLR                              
         BZ    MRKTRN30                                                         
         LA    R3,TEMP             BUILD TRANSACTION EXTRA STATUS               
         XC    TRXELD(TRXLN1Q),TRXELD                                           
         MVI   TRXEL,TRXELQ                                                     
         MVI   TRXLN,TRXLN1Q                                                    
         TM    TSARIND2,TSAR2HLD                                                
         BZ    *+8                                                              
         OI    TRXSTA1,TRXSRHLD    SET HELD                                     
         TM    TSARIND2,TSAR2QRD                                                
         BZ    *+8                                                              
         OI    TRXSTA1,TRXSRQRD    SET QUERIED                                  
         TM    TSARIND2,TSAR2CLR                                                
         BZ    *+8                                                              
         OI    TRXSTA1,TRXSRCLR    SET CLEARING                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCFIL),TRNRECD,TRXELD,0,0                     
         CLI   12(R1),0                                                         
         BE    MRKTRN28                                                         
         DC    H'0'                                                             
*                                                                               
MRKTRN26 MVC   BYTE,TRXSTA1        SAVE CURRENT STATUS                          
         NI    TRXSTA1,FF-(TRXSRHLD+TRXSRQRD+TRXSRCLR)                          
         TM    TSARIND2,TSAR2HLD                                                
         BZ    *+8                                                              
         OI    TRXSTA1,TRXSRHLD                                                 
         TM    TSARIND2,TSAR2QRD                                                
         BZ    *+8                                                              
         OI    TRXSTA1,TRXSRQRD                                                 
         TM    TSARIND2,TSAR2CLR                                                
         BZ    *+8                                                              
         OI    TRXSTA1,TRXSRCLR                                                 
         CLC   TRXSTA1,BYTE        TEST STATUS CHANGED                          
         BE    MRKTRN30                                                         
*                                                                               
MRKTRN28 OI    UPDINDS,UPDITRUP    SET TO UPDATE TRANSACTION                    
*                                                                               
MRKTRN30 OC    TSARXSEQ,TSARXSEQ   TEST EXTENSION RECORD                        
         BZ    MRKTRN40                                                         
         OI    UPDINDS,UPDITRUP    SET TO UPDATE TRANSACTION                    
         GOTO1 VHELLO,DMCB,(C'D',ACCFIL),('NOTELQ',TRNRECD),0,0                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ATSARBLK         READ EXTENSION TSAR RECORD                   
         USING TSARD,R1                                                         
         MVC   STSRNUM,TSRNUM      SAVE CURRENT TSAR RECORD NUMBER              
         MVI   TSACTN,TSARDH                                                    
         MVC   TSARLEN,=Y(TSARMAXL)                                             
         XC    TSARKEY(TSARKEYL),TSARKEY                                        
         MVC   TS2TYP,=AL2(FFFF)                                                
         MVC   TS2SEQ,TSARXSEQ                                                  
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         LA    R3,TEMP                                                          
         USING NOTELD,R3                                                        
         LA    R4,TS2ELE                                                        
         USING TS2ELE,R4                                                        
         LA    R0,TS2ELEN                                                       
         MVI   NOTEL,NOTELQ                                                     
MRKTRN32 MVI   NOTLN,NOTLN1Q       (RE)SET MINIMUM LENGTH                       
         OC    TS2ELE(TS2ELEL),TS2ELE                                           
         BZ    MRKTRN36                                                         
         MVC   NOTDATE,TS2DATE                                                  
         MVC   NOTREF,TS2REF                                                    
         LA    RE,TS2NOTE                                                       
         LA    RF,TS2NOTE+(L'TS2NOTE-1)                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE                                                            
         BM    MRKTRN34                                                         
         MVC   NOTNOTE(0),TS2NOTE                                               
         EX    RF,*-6                                                           
         LA    RF,NOTLN1Q+1(RF)                                                 
         STC   RF,NOTLN                                                         
MRKTRN34 GOTO1 VHELLO,DMCB,(C'P',ACCFIL),TRNRECD,NOTELD,0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
MRKTRN36 LA    R4,TS2ELEL(R4)      BUMP TO NEXT TSAR DATA ELEMENT               
         DROP  R4                                                               
         BCT   R0,MRKTRN32                                                      
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         GOTO1 ATSARGET,STSRNUM    READ SAVED TSAR RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
MRKTRN40 TM    TSARINDS,TSARITRF   TEST BEING TRANSFERRED THIS TIME             
         BO    *+12                                                             
         TM    CURIND1,CUR1SINC    TEST NOT SINGLE CURRENCY ALLOCATION          
         BO    MRKTRN44                                                         
         CP    TSARPOST,PZERO      TEST ANYTHING POSTED                         
         BE    MRKTRN44                                                         
         L     R2,AIO1             DEAL WITH AFCEL                              
         LA    R3,TRNRECD+ACCORFST R3=A(FIRST ELEMENT)                          
         USING AFCELD,R3                                                        
         SR    R0,R0                                                            
MRKTRN42 IC    R0,AFCLN            SKIP PAST TRNEL                              
         AR    R3,R0                                                            
         CLI   AFCEL,0             TEST EOR                                     
         BE    MRKTRN44                                                         
         CLI   AFCEL,AFCELQ        TEST AFCEL                                   
         BNE   MRKTRN42                                                         
         TM    AFCXSTAT,AFCXSMEM   TEST ALREADY A CURRENCY MEMO ONLY            
         BO    MRKTRN44                                                         
         OI    AFCXSTAT,AFCXSMEM   SET THIS IS NOW A CURRENCY MEMO ONLY         
         OI    UPDINDS,UPDITRUP    SET TO UPDATE TRANSACTION                    
*                                                                               
MRKTRN44 TM    UPDINDS,UPDITRUP    TEST TRANSACTION TO BE UPDATED               
         BZ    MRKTRN02                                                         
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),ACCFIL,TRNRECD,TRNRECD               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,TRNRECD         SAVE THIS KEY                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCFIL,TRNRECD,TRNRECD              
MRKTRN46 GOTO1 VDATAMGR,DMCB,(X'80',DMRSEQ),ACCFIL,TRNRECD,TRNRECD              
         CLC   TRNRECD(TRNKSBR-TRNRECD),KEY                                     
         BNE   MRKTRN02                                                         
*&&UK*&& CLI   OFFAOPOS-OFFALD(R1),LDGOTRAN                                     
*&&UK*&& BNE   MRKTRN48            TEST FOR TRANSACTION LEVEL ONLY              
         L     R1,AOFFBLK          APPLY LIMIT ACCESS TO TRANSACTION            
         GOTO1 VOFFAL                                                           
         BNE   MRKTRN46                                                         
*                                                                               
MRKTRN48 B     MRKTRN04            PROCESS THIS OPEN ITEM MEMBER                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* READ TRANSACTIONS AND POST TO TSAR                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R5                                                       
READTR   XC    DISVALS(DISVALSL),DISVALS                                        
         LA    R1,RECACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         L     R1,ARECVTAB                                                      
         ST    R1,ARCVTAB          SET POINTER TO FIRST RECEIVABLE A/C          
*                                                                               
READ01   DS    0H                                                               
         GOTO1 SETKEY,SETACC       BUILD RECEIVABLE ACCOUNT KEY                 
         GOTO1 AGETLDG             GET A(LEDGER TABLE ENTRY)                    
         ICM   R2,15,RECLEDG                                                    
         USING LEDGTABD,R2         R2=A(LEDGER TABLE ENTRY)                     
         L     R1,AOFFBLK          BUILD OFFALD FOR OFFICE TESTING              
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         L     R0,AIO1                                                          
         ST    R0,OFFAREC          SET A(TRANSACTION RECORD)                    
         MVC   OFFAOPOS,LEDGTOFF   SET LEDGER VALUES FOR OFFAL                  
         MVC   OFFALDGL,LEDGTLVA                                                
         MVC   OFFALDGT,LEDGTCLO                                                
         MVI   OFFAACT,OFFATST     SET ACTION TO TEST (TRANSACTIONS)            
         DROP  R1,R2                                                            
*                                                                               
         L     R2,AIO1                                                          
         AH    R2,DATADISP                                                      
         USING TRNELD,R2           R2=A(TRANSACTION ELEMENT)                    
*                                                                               
READ02   XC    TSARKEY(TSARKEYL),TSARKEY                                        
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         XC    PREVPONO,PREVPONO                                                
         ZAP   TSARDR,PZERO                                                     
         ZAP   TSARCR,PZERO                                                     
         ZAP   TSARPOST,PZERO                                                   
         ZAP   TSARDISC,PZERO                                                   
         MVC   TSAROFFC,SPACES                                                  
         XC    TSARDUED,TSARDUED                                                
*&&UK                                                                           
         ZAP   TSARTAX,PZERO                                                    
         ZAP   TSARDR2,PZERO                                                    
         ZAP   TSARCR2,PZERO                                                    
         ZAP   TSARDI2,PZERO                                                    
         ZAP   TSARPO2,PZERO                                                    
*&&                                                                             
*&&US                                                                           
         ZAP   TSARGSTB,PZERO                                                   
         ZAP   TSARGST,PZERO                                                    
         ZAP   TSARPSTB,PZERO                                                   
         ZAP   TSARPST,PZERO                                                    
         XC    TSARGSTT,TSARGSTT                                                
         XC    TSARPSTT,TSARPSTT                                                
         XC    TSARPSTC,TSARPSTC                                                
         XC    TSARBTYC,TSARBTYC                                                
         XC    TSARPOEL,TSARPOEL                                                
*&&                                                                             
*                                                                               
         L     R0,AIO6            POINT TO THE WORK AREA                        
         LHI   R1,L'IO6           LENGTH OF THE WORK AREA                       
         SR    RE,RE              SET ADDRESS TO ZERO                           
         SR    RF,RF              SET LENGTH TO ZERO                            
         MVCL  R0,RE              INITALIZE TO LOW VALUES                       
         L     R3,AIO6            POINT TO THE WORK AREA                        
         USING SAVEVALS,R3        MAP WITH SAVE KEY                             
         MVI   SAVETBLE,SAVETBLN  SET DEFULT TO 'NO' FOR TABLE ENTRY            
*                                                                               
         XC    TSARVALS(TSARVALL),TSARVALS                                      
         GOTO1 SETKEY,SETALL                                                    
         GOTO1 AIOHIGH                                                          
         B     READ10                                                           
*                                                                               
READ04   LR    R0,R1               SAVE SETKEY PARAMETER                        
         GOTO1 ATSARADD,TSARTTRN                                                
         BNE   ROU1H                                                            
         LR    R1,R0               RESTORE SETKEY PARAMETER                     
         BAS   RE,SETKEY                                                        
         GOTO1 AIOHIGH             READ HIGH                                    
         B     READ10                                                           
*                                                                               
READ06   GOTO1 ATSARADD,TSARTTRN                                                
         BNE   ROU1H                                                            
READ08   GOTO1 AIOSEQ              READ SEQUENTIAL                              
*                                                                               
READ10   BNE   READ12                                                           
         L     R5,AIO1                                                          
         CLC   TRNKCULA,RECVCACT                                                
         BE    READ14                                                           
*                                                                               
READ12   GOTO1 ATSARADD,TSARTTRN   FILE LAST TRANSACTION SET                    
         BNE   ROU1H                                                            
         B     READ80                                                           
*                                                                               
READ14   CLI   TRNEL,TRNELQ        HAVE WE REACHED TRANSACTION?                 
         BE    READ18                                                           
         LA    R1,SETSDT+SETBNR                                                 
         B     READ04                                                           
*                                                                               
READ18   OC    TRNRECD+ACCOPEEL(L'TRSPDAT),TRNRECD+ACCOPEEL                     
         BNZ   READ08                                                           
         TM    TRNRSTAT,TRNSDRFT+TRNSREVS                                       
         BNZ   READ08                                                           
*&&US*&& CLI   PROFEX55,C'N'       TEST EXCLUDE BT55/BT56                       
*&&US*&& BE    READ20                                                           
*&&UK*&& CLI   PROFEX55,C'Y'       TEST EXCLUDE BT55/BT56                       
*&&UK*&& BNE   READ20                                                           
         CLI   TRNTYPE,BT55                                                     
         BE    READ08                                                           
         CLI   TRNTYPE,BT56                                                     
         BE    READ08                                                           
*                                                                               
READ20   OC    SOURCE,SOURCE       FILTERING BY SOURCE?                         
         BZ    READ21                                                           
         IC    RF,SOURCEXQ                                                      
         EXCLC RF,TRNKCACT,SOURCE                                               
         BE    *+12                                                             
         LA    R1,SETNXT                                                        
         B     READ04                                                           
*                                                                               
READ21   CLC   TRNDATE,DATSTA      START DATE FILTER                            
         BNL   *+12                                                             
         LA    R1,SETSDT                                                        
         B     READ04                                                           
         CLC   TRNDATE,DATEND      END DATE FILTER                              
         BNH   READ22                                                           
         LA    R1,SETSDT+SETBNR+SETNXT                                          
         B     READ04                                                           
*                                                                               
READ22   OC    BILSTA,BILSTA       START BILL NUMBER FILTER                     
         BZ    READ24                                                           
         SR    RF,RF                                                            
         IC    RF,BILSTAXQ                                                      
         EX    RF,*+8                                                           
         BL    READ06                                                           
         CLC   TRNREF(0),BILSTA                                                 
*                                                                               
READ24   OC    BILEND,BILEND       END BILL NUMBER FILTER                       
         BZ    READ26                                                           
         SR    RF,RF                                                            
         IC    RF,BILENDXQ                                                      
         EX    RF,*+8                                                           
         BH    READ06                                                           
         CLC   TRNREF(0),BILEND                                                 
*                                                                               
READ26   L     R1,AOFFBLK          IF OFFICE IS AT TRANSACTION LEVEL            
*&&UK                                                                           
         CLI   OFFAOPOS-OFFALD(R1),LDGOTRAN                                     
         BNE   READ28                                                           
*&&                                                                             
         GOTO1 VOFFAL              APPLY LIMIT ACCESS TO TRANSACTION            
         BNE   READ08                                                           
*                                                                               
READ28   CLC   FLTOFFC,SPACES      TEST OFFICE FILTER SET                       
         BNH   READ30                                                           
         CLC   TRNOFFC+0(1),FLTOFFC+0                                           
         BNE   READ06                                                           
         CLI   FLTOFFC+1,C' '                                                   
         BNH   READ30                                                           
         CLC   TRNOFFC+1(1),FLTOFFC+1                                           
         BNE   READ06                                                           
*                                                                               
READ30   GOTO1 VCONVMOS,DMCB,(X'FE',TRNELD),WORK                                
         OC    MOSSTA(L'MOSSTA+L'MOSEND),MOSSTA                                 
         BZ    READ32                                                           
         CLC   WORK(L'MOSSTA),MOSSTA                                            
         BL    READ06                                                           
         CLC   WORK(L'MOSEND),MOSEND                                            
         BH    READ06                                                           
*                                                                               
READ32   CLC   TSARVALS(TSARVALL),TRNKCULC                                      
         BE    READ36                                                           
*                                                                               
         XC    PREVPONO,PREVPONO                                                
*                                                                               
         CLI   TRNKREF+3,C'*'      DO WE HAVE REFERENCE WITH 255+ SREF?         
         BNE   READ32B             NO, CONTINUE                                 
*                                  YES                                          
         MVC   SAVEVALS(SAVEVALL),TRNKCULC SAVE TRANSACTION KEY                 
*                                                                               
         LA    R1,TRNELD           R1=A(TRANSACTION ELEMENT)                    
         USING FFTELD,R1           MAP WITH FFTEL                               
*                                                                               
READ32A  LLC   R0,FFTLN            LENGTH OF THE ELEMENT                        
         AR    R1,R0               POINT TO THE NEXT ELEMENT                    
         CLI   FFTEL,0             TEST E-O-R                                   
         BE    READ32B             YES, PROCESS REF WITH 255+ SREF              
*                                                                               
         CLI   FFTEL,FFTELQ        FFT ELEMENT  ?                               
         BNE   READ32A             NO , CHECK FOR NEXT ELEMENT                  
*                                  YES ,                                        
         CLI   FFTTYPE,FFTTKREF    KEY REFERANCE NUMBER TYPE FFT ?              
         BNE   READ32A             NO, PROCESS NEXT                             
*                                  YES , SAVE REFERENCE NUMBER                  
         MVC   SAVEREF,FFTDATA     SAVE KEY REFERENCE NUMBER                    
         CLC   TSARVALS(TSARVALL),SAVEVALS IS KEY SAME ?                        
         BE    READ36              YES, CONTINUE                                
*                                  NO , ADD TSAR RECORD FOR THE KEY             
         DROP  R1                                                               
READ32B  DS    0H                                                               
         OC    TSARVALS(TSARVALL),TSARVALS                                      
         BZ    READ34              FIRST TIME FOR TRANSACTION SET               
*                                                                               
         GOTO1 ATSARADD,TSARTTRN   MAJOR KEY CHANGE                             
         BNE   ROU1H                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
READ34   MVC   TSARVALS(TSARVALL),TRNKCULC                                      
         CLI   TRNKREF+3,C'*'      DO WE HAVE REFERENCE WITH 255+               
         BNE   READ34A             NO, CONTINUE                                 
*                                                                               
         MVI   SAVETBLE,SAVETBLY   MARK ADD TABLE ENTRY                         
         MVC   TSARVALS(TSARVALL),SAVEVALS                                      
         DROP  R3                                                               
*                                                                               
READ34A  DS    0H                                                               
*                                                                               
         MVC   TSARSUB,TRNKSBR     TAKE SUB-REFERENCE                           
         MVC   TSAROFFC,TRNOFFC    TAKE OFFICE                                  
*        CLI   TSAROFFC+0,C'A'     OFFICES ARE C'A'-C'9'                        
*        BNL   *+8                                                              
*        MVI   TSAROFFC+0,C' '                                                  
*        CLI   TSAROFFC+1,C'A'     IN BOTH PLACES                               
*        BNL   *+8                                                              
*        MVI   TSAROFFC+1,C' '                                                  
         MVC   TSARPMOS,WORK       SET MONTH-OF-SERVICE                         
*                                                                               
READ36   TM    TSARINDS,TSARISDR+TSARISCR                                       
         BZ    *+8                                                              
         OI    TSARINDS,TSARISMT   SET MULTI TRANSACTION BIT ON                 
*MN                                                                             
*&&US                                                                           
         CLI   TRNTYPE,58                                                       
         BNE   *+8                                                              
         OI    TSARIND2,TSARPRTC                                                
*&&                                                                             
*MN                                                                             
         SR    RF,RF                                                            
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+12                                                             
         OI    TSARINDS,TSARISDR   SET DR FOUND BIT ON                          
         B     READ52                                                           
         OI    TSARINDS,TSARISCR   SET CR FOUND BIT ON                          
         AP    TSARCR,TRNAMNT      CREDIT TRANSACTION                           
         ZAP   TSARDISC,PZERO      CLEAR ANY DISCOUNT (OR SURCHARGE)            
*&&UK*&& ZAP   TSARDI2,PZERO                                                    
         SR    R0,R0                                                            
         LA    R1,TRNELD           R1=A(TRANSACTION ELEMENT)                    
         USING NOTELD,R1                                                        
READ38   IC    R0,NOTLN            BUMP TO NEXT CREDIT ELEMENT                  
         AR    R1,R0                                                            
         CLI   NOTEL,0             TEST E-O-R                                   
         BE    READ76              YES - DR/CR COMMON CODE                      
         CLI   NOTEL,NOTELQ        TEST NOTE ELEMENT                            
         BNE   READ40                                                           
         OI    TSARIND2,TSAR2MEM   SET MEMORANDUM HELD                          
         B     READ38                                                           
*&&US                                                                           
READ40   DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
         USING AFCELD,R1                                                        
READ40   CLI   AFCEL,AFCELQ        TEST ACCOUNTING FOREIGN CURRENCY             
         BNE   READ42                                                           
         CLI   AFCCURR+0,C'A'      ENSURE CURRENCY CODE LOOKS GOOD              
         BL    READ52                                                           
         CLI   AFCCURR+1,C'A'                                                   
         BL    READ52                                                           
         AP    TSARCR2,AFCAMNT                                                  
         TM    AFCXSTAT,AFCXSMEM   TEST MEMO RATE ELEMENT ENCOUNTERED           
         BZ    *+8                                                              
         OI    TSARAFCX+(AFCXSTAT-AFCX),AFCXSMEM                                
         OC    TSARCUR,TSARCUR     TEST CURRENCY IS ESTABLISHED                 
         BNZ   *+10                                                             
         MVC   TSARCUR,AFCCURR                                                  
         CLC   TSARCUR,AFCCURR     TEST CURRENCY MATCHES                        
         BE    READ38                                                           
         B     READERMX                                                         
*&&                                                                             
         USING TRXELD,R1                                                        
READ42   CLI   TRXEL,TRXELQ        TEST TRANSACTION EXTRA STATUS                
         BNE   READ44                                                           
         TM    TRXSTA1,TRXSRHLD                                                 
         BZ    *+12                                                             
         OI    TSARIND2,TSAR2HLD   SET HELD                                     
         B     READ38                                                           
         TM    TRXSTA1,TRXSRQRD                                                 
         BZ    *+12                                                             
         OI    TSARIND2,TSAR2QRD   SET QUERIED                                  
         B     READ38                                                           
         TM    TRXSTA1,TRXSRCLR                                                 
         BZ    *+12                                                             
         OI    TSARIND2,TSAR2CLR   SET CLEARING                                 
         B     READ38                                                           
         B     READ44                                                           
*                                                                               
         USING SCIELD,R1                                                        
READ44   DS    0H                                                               
         CLI   SCIEL,SCIELQ        TEST SUBSIDIARY CASH ELEMENT                 
         BNE   READ50                                                           
*&&UK                                                                           
READ45   CLI   SCITYPE,SCITTTAX    TEST TAX                                     
         BNE   READ50                                                           
         SP    TSARTAX,SCIAMNT     SUBTRACT TAX AMOUNT FOR CREDITS              
         B     READ50                                                           
*&&                                                                             
*&&US                                                                           
READ45   CLI   SCITYPE,SCITTAXP    TEST GST                                     
         BNE   READ46                                                           
         SP    TSARGST,SCIAMNT     EXTRACT AMOUNT/BASIS/TYPE                    
         CLI   SCILN,SCILN1Q                                                    
         BE    READ45A                                                          
         SP    TSARGSTB,SCIBASE                                                 
         MVC   TSARGSTT,SCISUBPT                                                
READ45A  BCTR  RF,0                FOUND SCIEL                                  
         B     READ50                                                           
*                                                                               
READ46   CLI   SCITYPE,SCITTQST    TEST QST                                     
         BNE   READ50                                                           
         SP    TSARPST,SCIAMNT     EXTRACT AMOUNT/BASIS/TYPE/PROVINCE           
         CLI   SCILN,SCILN1Q                                                    
         BE    READ50                                                           
         SP    TSARPSTB,SCIBASE                                                 
         MVC   TSARPSTC,SCISUBPR                                                
         MVC   TSARPSTT,SCISUBPT                                                
         B     READ50                                                           
*&&                                                                             
         USING FFTELD,R1                                                        
READ50   DS    0H                                                               
         CLI   FFTEL,FFTELQ        TEST FREEFORM TEXT ELEM                      
         BNE   READ38                                                           
         CLI   FFTTYPE,FFTTINVN    INVOICE NUMBER?                              
         BNE   READ51                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FFTLN                                                       
         BNP   READ38                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSARINVN(0),FFTEL   MOVE IN WHOLE ELEM                           
         B     READ38              NEXT CREDIT ELEMENT                          
*                                                                               
READ51   DS    0H                                                               
         CLI   FFTTYPE,FFTTKREF    KEY REFERENCE NUMBER FOR BANK VOID?          
         BNE   READ38                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FFTLN                                                       
         BNP   READ38                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSARKREF(0),FFTEL   MOVE IN WHOLE ELEM                           
         B     READ38              NEXT CREDIT ELEMENT                          
*                                                                               
READ52   AP    TSARDR,TRNAMNT      DEBIT TRANSACTION                            
         SR    R0,R0                                                            
         LA    R1,TRNELD           R1=A(TRANSACTION ELEMENT)                    
         USING SCIELD,R1                                                        
READ54   IC    R0,SCILN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   SCIEL,0             TEST E-O-R                                   
         BE    READ76              YES - DR/CR COMMON CODE                      
         CLI   SCIEL,SCIELQ        TEST SUBSIDIARY CASH ELEMENT                 
         BNE   READ60                                                           
         CLI   SCITYPE,SCITCDSC    TEST CASH DISCOUNT                           
         BNE   READ56                                                           
         AP    TSARDISC,SCIAMNT    ADD DISCOUNT/SURCHARGE AMOUNT                
*&&UK                                                                           
         CLI   SCILN,SCILN2Q                                                    
         BL    READ54                                                           
         AP    TSARDI2,SCIADMN     ADD CURRENCY DISCOUNT AMOUNT                 
*&&                                                                             
         B     READ54                                                           
*&&UK                                                                           
READ56   CLI   SCITYPE,SCITTTAX    TEST TAX                                     
         BNE   READ54                                                           
         AP    TSARTAX,SCIAMNT     ADD TAX AMOUNT                               
         B     READ54                                                           
*&&                                                                             
*&&US                                                                           
READ56   CLI   SCITYPE,SCITTAXP    TEST GST                                     
         BNE   READ58                                                           
         AP    TSARGST,SCIAMNT     EXTRACT AMOUNT/BASIS/TYPE                    
         CLI   SCILN,SCILN1Q                                                    
         BE    READ56A                                                          
         AP    TSARGSTB,SCIBASE                                                 
         MVC   TSARGSTT,SCISUBPT                                                
READ56A  BCTR  RF,0                FOUND THE SCIEL                              
         B     READ54                                                           
*                                                                               
READ58   CLI   SCITYPE,SCITTQST    TEST QST                                     
         BNE   READ54                                                           
         AP    TSARPST,SCIAMNT     EXTRACT AMOUNT/BASIS/TYPE/PROVINCE           
         CLI   SCILN,SCILN1Q                                                    
         BE    READ54                                                           
         AP    TSARPSTB,SCIBASE                                                 
         MVC   TSARPSTC,SCISUBPR                                                
         MVC   TSARPSTT,SCISUBPT                                                
         B     READ54                                                           
*&&                                                                             
         USING DUEELD,R1                                                        
READ60   CLI   DUEEL,DUEELQ        TEST DUE DATE ELEMENT                        
         BNE   READ62                                                           
         MVC   TSARDUED,DUEDATE    SET DUE DATE VALUE                           
         B     READ54                                                           
*                                                                               
         USING TRXELD,R1                                                        
READ62   CLI   TRXEL,TRXELQ        TEST TRANSACTION EXTRA STATUS                
         BNE   READ64                                                           
         TM    TRXSTA1,TRXSRHLD    TEST/SET HELD                                
         BZ    *+12                                                             
         OI    TSARIND2,TSAR2HLD                                                
         B     READ54                                                           
         TM    TRXSTA1,TRXSRQRD    TEST/SET QUERIED                             
         BZ    *+12                                                             
         OI    TSARIND2,TSAR2QRD                                                
         B     READ54                                                           
         TM    TRXSTA1,TRXSRCLR    TEST/SET CLEARING                            
         BZ    *+12                                                             
         OI    TSARIND2,TSAR2CLR                                                
         B     READ54                                                           
         B     READ54                                                           
*                                                                               
         USING NOTELD,R1                                                        
READ64   CLI   NOTEL,NOTELQ        TEST TRANSACTION NOTE                        
         BNE   READ66                                                           
         OI    TSARIND2,TSAR2MEM   SET MEMORANDUM HELD                          
         B     READ54                                                           
*&&US                                                                           
READ66   DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
         USING AFCELD,R1                                                        
READ66   CLI   AFCEL,AFCELQ        TEST ACCOUNTING FOREIGN CURRENCY             
         BNE   READ68                                                           
         CLI   AFCCURR+0,C'A'      ENSURE CURRENCY CODE LOOKS GOOD              
         BL    READ54                                                           
         CLI   AFCCURR+1,C'A'                                                   
         BL    READ54                                                           
         AP    TSARDR2,AFCAMNT     ADD TO CURRENCY DEBIT AMOUNT                 
         TM    AFCXSTAT,AFCXSMEM   TEST MEMO RATE ELEMENT ENCOUNTERED           
         BZ    *+8                                                              
         OI    TSARAFCX+(AFCXSTAT-AFCX),AFCXSMEM                                
         OC    TSARCUR,TSARCUR     TEST CURRENCY IS ESTABLISHED                 
         BNZ   *+10                                                             
         MVC   TSARCUR,AFCCURR                                                  
         CLC   TSARCUR,AFCCURR     TEST CURRENCY MATCHES                        
         BNE   READERMX                                                         
         CP    TSARDI2,PZERO       TEST CURRENCY DISCOUNT KNOWN                 
         BNE   READ54                                                           
         ZAP   DUB,TSARDISC        TAKE AGENCY CURRENCY DISCOUNT                
         MVC   TEMP(L'AFCX),AFCX                                                
         EXCHP DUB,TEMP            CALCULATE USING EXCHANGE RATE                
         ZAP   TSARDI2,DUB                                                      
         B     READ54                                                           
*&&                                                                             
         USING SORELD,R1                                                        
READ68   CLI   SOREL,SORELQ        TEST SOURCE ELEMENT                          
         BNE   READ70                                                           
         CLI   SORLN,L'TSARSOR     TEST THIS WILL FIT                           
         BNH   *+6                                                              
         DC    H'0'                NEED TO EXTEND TSARSOR                       
         XC    TSARSOR,TSARSOR     CLEAR ANY PREVIOUS SOURCE                    
         IC    RF,SORLN                                                         
         BCTR  RF,0                                                             
         MVC   TSARSOR(0),SORELD                                                
         EX    RF,*-6                                                           
         B     READ54                                                           
*                                                                               
         USING FFTELD,R1                                                        
READ70   DS    0H                                                               
         CLI   FFTEL,FFTELQ        TEST FREEFORM TEXT ELEMENT                   
         BNE   READ74                                                           
         CLI   FFTTYPE,FFTTINVN    INVOICE NUMBER?                              
         BNE   READ72                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FFTLN                                                       
         BNP   READ72                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSARINVN(0),FFTEL                                                
         B     READ74                                                           
*                                                                               
READ72   DS    0H                                                               
         CLI   FFTTYPE,FFTTKREF    KEY REFERENCE NUMBER FOR BANK VOID?          
         BNE   READ74                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FFTLN                                                       
         BNP   READ74                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSARKREF(0),FFTEL                                                
         B     READ54                                                           
*                                                                               
READ74   DS    0H                  NEXT DEBIT ELEMENT                           
         CLI   FFTTYPE,FFTTMXTY    MX ELEMENT AND BILL TYPES                    
         BNE   READ75                                                           
*                                                                               
         MVC   TSARBTYC,FFTMXTYP   MOVE BILL TYPE WITH ELE CODE                 
         B     READ54                                                           
*                                                                               
         USING UFSELD,R1                                                        
READ75   DS    0H                  UFSEL ELEMENT FOR PO NUMBER                  
         CLI   UFSEL,UFSELQ                                                     
         JNE   READ54                                                           
         CLC   UFSCODE,=C'PO'      UFSEL FOR PO NUMBER                          
         JNE   READ54              NO, SKIP IT                                  
*                                                                               
         OC    PREVPONO,PREVPONO        FIRST TIME?                             
         JZ    READ75A                  YES, CONTINUE                           
         CLC   PREVPONO,UFSDATA         NO, SAME PO'S?                          
         JE    READ75A                  YES, CONTINUE                           
         MVC   TSARPONO,=C'MULTIPLE       ' NO, MARK MULTIPLE PO'S              
         MVC   PREVPONO,TSARPONO        STORE CURRENT PO                        
         MVC   TSARPODS,=C'MULTIPLE PO '                                        
         XC    TSARPSTS,TSARPSTS        INIT PO STATUS                          
         J     READ54                                                           
*                                                                               
READ75A  MVC   TSARPONO,UFSDATA    GET PO NUMBER - 15 BYTE FIXED PO             
         MVC   PREVPONO,TSARPONO   SAVE CURRENT PO NUMBER                       
         MVC   TSARPODS,UFSDESC    GET PO DESC                                  
         MVC   TSARPSTS,UFSSTAT    GET PO STATUS                                
         J     READ54                                                           
         DROP  R1                                                               
*                                                                               
         USING MDTELD,R1                                                        
READ76   DS    0H                  DR/CR TRANSACTION COMMON CODE                
*&&US                                                                           
         LR    R1,R2               POINT TO TRANSACTION                         
READ76A  CLI   MDTEL,0             END OF RECORD?                               
         BE    READ76X                                                          
         CLI   MDTEL,MDTELQ        HAS TO BE MEDIA TRANSFER ELEMENT             
         BE    READ76B                                                          
         CLI   MDTEL,MDPELQ                                                     
         BE    READ78                                                           
         IC    R0,MDTLN             NO, BUMP TO NEXT ONE                        
         AR    R1,R0                                                            
         B     READ76A                                                          
*                                                                               
READ76B  DS    0H                                                               
         MVC   TSARSYS,MDTSYS                                                   
         MVC   TSARMED,MDTMED                                                   
         MVC   TSARCLI,MDTCLI                                                   
         LTR   RF,RF               DID WE FIND THE GST ELEMENT?                 
         BNZ   READ76X             YES, DON'T USE VAT                           
         OC    MDTVAT,MDTVAT       DO WE HAVE AN AMOUNT?                        
         BZ    READ76X                                                          
         ICM   R1,15,MDTVAT                                                     
         CVD   R1,DUB3             CONVERT TO PACK                              
         TM    TSARINDS,TSARISDR   DEBIT                                        
         BZ    READ76D                                                          
         AP    TSARGST,DUB3                                                     
         B     READ76X                                                          
READ76D  TM    TSARINDS,TSARISCR   CREDIT                                       
         BZ    READ76X                                                          
READ76C  SP    TSARGST,DUB3                                                     
*&&                                                                             
READ76X  B     READ08                                                           
         DROP  R1                                                               
*                                                                               
         USING MDPELD,R1                                                        
READ78   DS    0H                                                               
         MVC   TSARSYS,MDPSYS                                                   
         MVC   TSARMED,MDPMED                                                   
         MVC   TSARCLI,MDPCLI                                                   
         LTR   RF,RF               DID WE FIND THE GST ELEMENT?                 
         BNZ   READ78X             YES, DON'T USE VAT                           
         OC    MDPVAT,MDPVAT       DO WE HAVE AN AMOUNT?                        
         BZ    READ78X                                                          
         ZAP   DUB3,MDPVAT                                                      
         TM    TSARINDS,TSARISDR   DEBIT                                        
         BZ    READ78A                                                          
         AP    TSARGST,DUB3                                                     
         B     READ78X                                                          
*                                                                               
READ78A  TM    TSARINDS,TSARISCR   CREDIT                                       
         BZ    READ78X                                                          
         SP    TSARGST,DUB3                                                     
*                                                                               
READ78X  B     READ08                                                           
         DROP  R1                                                               
*                                                                               
READ80   L     R1,ARCVTAB          BUMP TO NEXT RECEIVABLE ACCOUNT              
         LA    R1,RECVTABL(R1)                                                  
         ST    R1,ARCVTAB          REFRESH RECEIVABLE VALUES                    
         CLI   0(R1),X'FF'                                                      
         BNE   READ01                                                           
*                                                                               
         OC    DISMAX,DISMAX       TEST IF ANY TRANSACTIONS WERE READ           
         BNZ   READ82                                                           
         TM    TWAMODE2,TWA2NALL   TEST NOT ALLOWING ALLOCATION                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(EANOITEM)                                           
         B     ROU1H                                                            
READ82   LA    R2,KEY                                                           
         USING BATRECD,R2                                                       
         MVC   BATKEY,SPACES                                                    
         XC    BATKEY(BATKEND),BATKEY                                           
         MVI   BATKTYP,BATKTYPQ                                                 
         MVC   BATKCPY,COMPANY                                                  
         MVC   BATKOFF,TWAUSRID                                                 
         MVI   BATKGRUP,TBAGGENQ   GENERAL ACCOUNTING                           
         MVI   BATKTYPE,30         BT30                                         
         MVC   BATKDATE,TODAYP     DATE                                         
         MVC   BATKREF,BATMON                                                   
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),ACCFIL,KEY,AIO1                     
         BNE   READ84                                                           
         MVC   FVMSGNO,=AL2(EGRECAOF)                                           
         LA    R1,RECACTH          CURSOR TO ACTION FIELD                       
         ST    R1,FVADDR                                                        
         B     ROU1H               IF FOUND AND NOT DELETED - ERROR             
*                                                                               
READ84   TM    8(R1),X'FF'-(X'10'+X'02')                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         XC    BATRECD(256),BATRECD                                             
         MVC   BATKEY,KEY                                                       
         MVI   BATRECD+(ACSTATUS-ACKEYD),BATSNOK+BATSRECV                       
         LA    RF,BATRECD+(ACRECORD-ACKEYD)                                     
         USING BTHELD,RF                                                        
         MVI   BTHEL,BTHELQ                                                     
         MVI   BTHLN,BTHLNQ                                                     
*&&UK                                                                           
         MVC   BTHNAME,SPACES      SET BATCH NAME FROM TERMINAL NO.             
         L     RE,AUTL                                                          
         MVC   BTHNAME(L'TSYM),TSYM-UTLD(RE)                                    
*&&                                                                             
*&&US*&& MVC   BTHNAME,BATNAME     TAKE USER'S BATCH NAME                       
         ZAP   BTHCASH,CHQAMT                                                   
         ZAP   BTHITEM,=P'1'                                                    
         LA    RF,BTHLNQ+1(RF)                                                  
         DROP  RF                                                               
         SR    RF,R2                                                            
         STCM  RF,3,BATRECD+(ACLENGTH-ACKEYD)                                   
         LA    R0,DMADD            ADD RECORD                                   
         TM    DMCB+8,X'02'        TEST RECORD FOUND BUT DELETED                
         BZ    *+8                                                              
         LA    R0,DMWRT            WRITE BACK RECORD                            
         GOTO1 VDATAMGR,DMCB,(0,(R0)),ACCFIL,BATRECD,BATRECD                    
         BE    *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         DROP  R2                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(1,0),RECOLAYH                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         OC    DISMAX,DISMAX       TEST IF ANY TRANSACTIONS WERE READ           
         BNZ   READ86                                                           
         MVC   ACTIONL,ACTION      UPDATE LAST ACTION                           
         MVI   ACTION,ACTOVER                                                   
         XC    RECACT,RECACT       NO - GO STRAIGHT TO SPECIAL SCREEN           
         MVC   RECACT(L'AC8SPCL),AC8SPCL                                        
         OI    RECACTH+(FVOIND-FVIHDR),FVOXMT                                   
         B     ROU1L                                                            
*                                                                               
READ86   MVC   ACTIONL,ACTION      UPDATE ACTION VALUES                         
         MVI   ACTION,ACTINPT                                                   
         XC    RECACT,RECACT       CHANGE ACTION TO INPUT                       
         MVC   RECACT(L'AC8INP),AC8INP                                          
         OI    RECACTH+(FVOIND-FVIHDR),FVOXMT                                   
         B     ROU1E                                                            
*                                                                               
READERMX LA    R1,RECACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$MXCUR)                                           
         XC    FVXTRA,FVXTRA                                                    
         MVI   FVXTRA,L'TSARREF+1                                               
         MVC   FVXTRA+1(L'TSARREF),TSARREF                                      
         B     ROU1H                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
*                                                                     *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
***********************************************************************         
         SPACE 1                                                                
SETKEY   STM   RE,R1,12(RD)        BUILD A KEY                                  
         STC   R1,WORK                                                          
         LA    RF,KEY                                                           
         USING TRNRECD,RF                                                       
         L     R1,AIO1                                                          
         MVC   TRNKEY,0(R1)        SET LAST KEY VALUES                          
*                                                                               
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY1                                                          
         MVC   TRNKEY,SPACES       SPACES INTO KEY                              
         L     R1,ARCVTAB                                                       
         MVC   RECVCACT,RECVACT-RECVTABD(R1)                                    
         MVC   RECVCNDX,RECVNDX-RECVTABD(R1)                                    
         MVC   TRNKCULA,RECVCACT                                                
*                                                                               
SETKEY1  TM    WORK,SETCON         SET CONTRA (BILLING SOURCE)                  
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCACT+3,X'41'                                                 
         OC    SOURCE,SOURCE                                                    
         BZ    SETKEY2                                                          
         MVC   TRNKCACT,SOURCE                                                  
*                                                                               
SETKEY2  TM    WORK,SETSDT         SET MIN TRANSACTION DATE                     
         BZ    SETKEY3                                                          
         MVC   TRNKDATE,DATSTA                                                  
         OC    TRNKDATE,TRNKDATE                                                
         BNZ   *+8                                                              
         MVI   TRNKDATE,X'41'      MUST BE GREATER THAN SPACES                  
*                                                                               
SETKEY3  TM    WORK,SETBNR         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY4                                                          
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    BILSTA,BILSTA                                                    
         BZ    SETKEY4                                                          
         MVC   TRNKREF,BILSTA                                                   
*                                                                               
SETKEY4  TM    WORK,SETNXT         BUMP CONTRA (BILLING SOURCE)                 
         BZ    SETKEY5                                                          
         IC    RE,TRNKCACT+(L'TRNKCACT-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKCACT+(L'TRNKCACT-1)                                       
*                                                                               
SETKEY5  DS    0H                                                               
SETKEYX  MVI   TRNKSBR,0           ALWAYS CLEAR SUB-REFERENCE                   
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR AND TRANSMIT AN INPUT FIELD ADDRESSED BY FVADDR               *         
***********************************************************************         
         SPACE 1                                                                
CLRFLD   L     RF,FVADDR                                                        
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         SR    R1,R1                                                            
         IC    R1,FVTLEN-FVIHDR(RF)                                             
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(RF),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         XC    L'FVIHDR(0,RF),L'FVIHDR(RF)                                      
         EX    R1,*-6                                                           
CLRFLDX  BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* CLEAR NAME FROM FIELDS THAT CONTAIN ACCOUNT CODE AND NAME           *         
***********************************************************************         
         SPACE 1                                                                
CLRNAM   ST    R1,FVADDR                                                        
         SR    RF,RF                                                            
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         LA    R1,L'FVIHDR(R1)     POINT TO START OF FIELD                      
         B     CLRNAM4                                                          
CLRNAM2  LTR   R0,R0               TEST 3 CONSECUTIVE SPACES FOUND              
         BNZ   *+12                                                             
         MVI   0(R1),C' '          YES - CLEAR REST OF FIELD                    
         B     CLRNAM6                                                          
         CLI   0(R1),C' '          TEST THIS IS SPACE OR LOWER                  
         BH    CLRNAM4                                                          
         BCTR  R0,0                YES - DECREMENT SPACE COUNT                  
         B     CLRNAM6                                                          
CLRNAM4  LA    R0,3                SET SPACE COUNT TO 3                         
CLRNAM6  LA    R1,1(R1)            BUMP TO NEXT INPUT BYTE                      
         BCT   RF,CLRNAM2                                                       
CLRNAMX  L     R1,FVADDR           RESTORE A(INPUT FIELD)                       
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
EQUALS   DC    C'='                EQUALS CHARACTER                             
MONTAB   DC    C'.123456789......ABC'                                           
         SPACE 1                                                                
FILDISP  DS    0AL1                DISPLACEMENTS TO ACCOUNT FILTERS             
         DC    AL1(RSTFILT1-RSTELD)                                             
         DC    AL1(RSTFILT2-RSTELD)                                             
         DC    AL1(RSTFILT3-RSTELD)                                             
         DC    AL1(RSTFILT4-RSTELD)                                             
         DC    AL1(RSTFILT5-RSTELD)                                             
         EJECT                                                                  
***********************************************************************         
* CONTROLLER ROUTINES - 2                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
ROUTN2   NMOD1 0,**ROU2**,RA,R9,R8                                              
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         AHI   RF,4                                                             
         B     *(RF)                                                            
         B     CONFRM                                                           
         B     UPDTRN                                                           
         B     FILTER                                                           
         B     VALBSC                                                           
         B     PROMEM                                                           
         B     XMTSCR                                                           
         B     BLDTOT                                                           
         B     BLDPFK                                                           
         B     BLDLIN                                                           
         B     TSTSEC                                                           
         B     HDRSEC                                                           
*                                                                               
ROU2L    MVI   DUB,0               SET CC=LOW                                   
         B     ROU2CC                                                           
ROU2E    MVI   DUB,1               SET CC=EQUAL                                 
         B     ROU2CC                                                           
ROU2H    MVI   DUB,2               SET CC=HIGH                                  
ROU2CC   CLI   DUB,1                                                            
*                                                                               
ROU2X    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CONFIRM SPECIAL ACTION                                              *         
***********************************************************************         
         SPACE 1                                                                
CONFRM   LA    R2,RECTOTH          LOCATE CONFIRMATION LOAD POINT               
         CLI   TWASCROV,TWASCRIN                                                
         BE    CONFRM02                                                         
         LA    R2,ROVTOTH                                                       
         CLI   TWASCROV,TWASCROP                                                
         BE    CONFRM02                                                         
         CLC   TWASCROV,TWASCRBH                                                
         BNE   ROU2L                                                            
         SR    R2,R2                                                            
         ICM   R2,3,SHEADTOT                                                    
         BZ    ROU2L                                                            
         LA    R2,RECOLAYH(R2)                                                  
*                                                                               
         USING TWACFRMD,R2                                                      
CONFRM02 TM    TWAMODE,TWAMCFRM    TEST CONFIRMATION SCREEN LOADED              
         BNZ   CONFRM04                                                         
         MVC   DMCB+4(3),=X'D90608'                                             
         MVI   DMCB+7,TWASCRCA     LOAD CONFIRMATION OVERLAY                    
         GOTO1 VCALLOV,DMCB,(0,TWACFRMD)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KILL IF CAN'T LOAD SCREEN                    
         LA    R1,RECMSGH          RETRANSMIT ALL HEADER FIELDS                 
         SR    RE,RE                                                            
         LA    RF,TWACFRMD                                                      
         BCTR  RF,0                                                             
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,*-12                                                       
*                                                                               
         LA    R1,PARM                                                          
         USING GETTXTD,R1          GET CONFIRMATION MESSAGE                     
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(SAENTCMA)                                           
         MVI   GTMTYP,GTMSCR                                                    
         XC    FVXTRA,FVXTRA                                                    
         MVI   FVXTRA+0,2          &1=CONFIRMATION CHARACTER                    
         MVC   FVXTRA+1(1),AC@YES                                               
         MVI   FVXTRA+2,ACTLGNLQ+1 &2=ACTION NAME                               
         L     RE,AACTNTRY                                                      
         MVC   LAREADDR,ACTTACT-ACTTABD(RE)                                     
         EX    0,LARE                                                           
         MVC   FVXTRA+3(ACTLGNLQ),0(RE)                                         
         LA    R0,FVXTRA                                                        
         STCM  R0,7,GTASUBST                                                    
         LA    R0,RCAMSGH                                                       
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT,GETTXTD                                                  
         SR    RF,RF                                                            
         OI    RCAINPH+(FVOIND-FVIHDR),FVOCUR                                   
         LA    R0,RCAINPH                                                       
         ST    R0,FVADDR                                                        
         OI    TWAMODE,TWAMCFRM+TWAMSETP                                        
         MVC   ACTION,ACTIONL                                                   
         B     ROU2H                                                            
         DROP  R1                                                               
*                                                                               
CONFRM04 NI    TWAMODE,255-TWAMCFRM                                             
         CLC   RCAINP(1),AC@YES    TEST ACTION CONFIRMED                        
         BNE   *+8                                                              
         OI    TWAMODE,TWAMDOIT    USER SAYS DO IT                              
         MVC   DMCB+4(3),=X'D90608'                                             
         MVI   DMCB+7,TWASCRTP     LOAD TOTALS/PFKEY OVERLAY                    
         GOTO1 VCALLOV,DMCB,(0,TWACFRMD)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KILL IF CAN'T LOAD SCREEN                    
         LA    R1,RECMSGH          RE-TRANSMIT ALL HEADER FIELDS                
         SR    RE,RE                                                            
         LA    RF,TWACFRMD                                                      
         BCTR  RF,0                                                             
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,*-12                                                       
         TM    TWAMODE,TWAMDOIT    TEST USER SAYS DO IT                         
         BNZ   ROU2L                                                            
         LA    R1,RECACTH          POSITION CURSOR TO ACTION & EXIT             
         ST    R1,FVADDR                                                        
         B     ROU2E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS AND/OR PRINT REPORT                             *         
***********************************************************************         
         SPACE 1                                                                
UPDTRN   LA    R0,UPDVALS          CLEAR UPDATE VALUES                          
         LH    R1,=Y(UPDVALSL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         OC    PRTSUB,PRTSUB       TEST REPORT TO BE GENERATED                  
         BZ    UPDTRN12                                                         
*                                                                               
         L     R0,ATIA             BUILD HEADER SCREEN IN TIA                   
         LH    R1,=Y(RECOLAYH-RECMSGH)                                          
         LA    RE,RECMSGH                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY UP TO RECOLAYH FROM CURRENT TWA         
         L     R1,ATIA             CLEAR MESSAGE FIELD & SET ACTION             
         XC    L'FVIHDR(L'RECMSG,R1),L'FVIHDR(R1)                               
         XC    RECACT-RECMSGH(L'RECACT,R1),RECACT-RECMSGH(R1)                   
         MVC   RECACT-RECMSGH(L'AC8DRAFT,R1),AC8DRAFT                           
         CLI   UPDACTN,ACTDRFT                                                  
         BE    *+14                                                             
         MVC   RECACT-RECMSGH(L'AC8FLT,R1),AC8FLT                               
         CLI   UPDACTN,ACTFILT                                                  
         BE    *+10                                                             
         MVC   RECACT-RECMSGH(L'AC8UPD,R1),AC8UPD                               
         LA    RF,SAVHEADL                                                      
         ICM   RF,12,=C'L='                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(1,0),APQBUFF,,(RF)                 
         L     R0,ATIA                                                          
         AH    R0,=Y(RECOLAYH-RECMSGH)                                          
         LA    R1,SAVHEADL                                                      
         L     RE,APQBUFF                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY HEADER SCREEN FROM TWA PAGE 1           
*                                                                               
         SR    R1,R1               DISPLAY TOTALS (IF FIELD PRESENT)            
         ICM   R1,3,SHEADTOT                                                    
         BZ    UPDTRN02                                                         
         AH    R1,=Y(RECOLAYH-RECMSGH)                                          
         A     R1,ATIA                                                          
         GOTO1 ABLDTOT                                                          
*                                                                               
UPDTRN02 SR    R1,R1               CLEAR PF FIELD IF PRESENT                    
         ICM   R1,3,SHEADPFK                                                    
         BZ    UPDTRN04                                                         
         AH    R1,=Y(RECOLAYH-RECMSGH)                                          
         A     R1,ATIA                                                          
         XC    L'FVIHDR(L'RECPFK,R1),L'FVIHDR(R1)                               
*                                                                               
UPDTRN04 L     R5,AREPWRK          INITIALISE REPD FOR PRINTING                 
         USING REPD,R5             R5=A(REPORT W/S)                             
         MVC   REPACOM,ACOM                                                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPACTN,REPAINI                                                  
         MVI   REPIND2,REPILOW                                                  
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         MVI   REPWIDTH,REPWREGQ                                                
         MVC   REPDATE,TODAYB      SET ONLINE VALUES                            
         MVC   REPAPQB,APQBUFF                                                  
         MVC   REPSYSID,=C'AC'                                                  
         MVC   REPPRGID,=C'RE'                                                  
         MVC   REPSUBID,PRTSUB                                                  
         MVC   REPRLH,=AL2(48)     SET LIVE RETAIN=48 HOURS                     
         MVC   REPRDH,=AL2(12)     SET DEAD RETAIN=12 HOURS                     
         MVC   REPDESC,AC@PGDEB    SET REPORT DESCRIPTION                       
*&&UK*&& MVI   REPCLASS,C'A'       UK CLASS A                                   
*&&US*&& MVI   REPCLASS,C'Z'       US CLASS A                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPBLK      CALL REPORT TO INITIALISE REPD               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
         L     R0,AIO1             CLEAR SPACE FOR HEADER SCREEN FORMAT         
         LH    R1,=Y(TWACOLS*TWAROWS)                                           
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,ATIA             FORMAT HEADER SCREEN INTO IO/IO2             
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
UPDTRN06 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    UPDTRN08                                                         
         SR    R2,R2                                                            
         ICM   R2,3,FVABSA-FVIHDR(R1)                                           
         A     R2,AIO1                                                          
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RE,=Y(L'FVIHDR)                                                  
         BCTR  RE,0                                                             
         TM    FVATRB-FVIHDR(R1),FVAZERO                                        
         BO    *+14                                                             
         MVC   0(0,R2),L'FVIHDR(R1)                                             
         EX    RE,*-6                                                           
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BXLE  R1,RE,UPDTRN06                                                   
*                                                                               
UPDTRN08 L     RF,VREPORT          INITIALISE HEADER PRINT LOOP                 
         LA    R1,REPD                                                          
         L     R2,AIO1                                                          
         LA    R0,TWAROWS                                                       
*                                                                               
         MVI   REPP1,C'*'          PRINT HEADER SCREEN ON FIRST PAGE            
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
UPDTRN10 MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS),0(R2)                                           
         MVI   REPP1+1+TWACOLS,C'*'                                             
         BASR  RE,RF                                                            
         LA    R2,TWACOLS(R2)                                                   
         BCT   R0,UPDTRN10                                                      
         MVI   REPP1,C'*'                                                       
         MVC   REPP1+1(TWACOLS+1),REPP1                                         
         BASR  RE,RF                                                            
*                                                                               
         L     R1,ATSARBLK         RESTORE DESTROYED TSAR BUFFER                
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARES                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
UPDTRN12 LA    R3,UPDBLOCK                                                      
         USING TRNBLKD,R3          R3=A(ADDTRN BLOCK)                           
         MVC   TRNCTRY,AGYCTRY     SET COUNTRY CODE                             
         MVC   TRNCOMF,ACOM        A(COMFACS)                                   
         MVC   TRNCPYS1,COMPSTAT                                                
         MVC   TRNCPYS2,COMPSTA2                                                
         MVC   TRNCPYS3,COMPSTA3                                                
         MVC   TRNCPYS4,COMPSTA4                                                
         MVC   TRNCPYS5,COMPSTA5                                                
         MVC   TRNCPYS6,COMPSTA6                                                
         MVC   TRNCPYS7,COMPSTA7                                                
         MVC   TRNCPYS8,COMPSTA8                                                
         MVC   TRNCPYS9,COMPSTA9                                                
         MVC   TRNCPYSA,COMPSTAA                                                
         MVC   TRNGLMOA,COMPGMOA   GLMOA DATE                                   
         MVC   TRNACC,AIO2         A(ACCOUNT RECORD)                            
         MVC   TRNBUK,AIO3         A(BUCKET BUFFER)                             
         MVC   TRNCAC,AIO4         A(CONTRA-ACCOUNT)                            
         MVC   TRNOFA,AIO5         A(OFFICE/ACCOUNT)                            
         LA    R1,LEDGTAB                                                       
         STCM  R1,15,TRNLDG                                                     
         MVI   TRN#LDGS,10         SET NUMBER OF LEDGER TABLE ENTRIES           
         MVC   TRNPUSER,TWAUSRID   SET USER-ID                                  
         MVI   TRNINDS1,TRNIVDAT   SET DON'T CHECK ACCOUNT FLAG                 
         MVC   TRNBMOS,BATMONP     SET (PWOS) BATCH MONTH                       
                                                                                
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
                                                                                
         MVI   UPDMODE,UPDMFRST                                                 
         GOTO1 OVERLAY,WORKD       CALL APPLICATION FOR FIRST TIME              
         MVC   REPAPHS,OVERSPEC    SET A(SPEC POOL)                             
         MVC   REPAUSR,OVERHOOK    SET A(USER HOOK)                             
         OI    REPHEADI,REPHFRCE   FORCE HEADLINE PRINTING                      
         MVC   REPPAGE,=H'1'                                                    
         L     R4,ARECVTAB          R4=A(RECEIVABLES TABLE)                     
         USING RECVTABD,R4                                                      
         MVI   UPDINDS,0                                                        
*                                                                               
UPDTRN20 XC    UPDRECN,UPDRECN     INITIALISE FOR RECEIVABLE ACCOUNT            
         NI    UPDINDS,255-UPDIRECA                                             
         ZAP   RECVAMT,PZERO                                                    
         ZAP   RECVAM2,PZERO                                                    
*                                                                               
UPDTRN22 LH    R1,UPDRECN          BUMP RECORD NUMBER                           
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX           TEST ALL MAIN RECORDS PROCESSED              
         BH    UPDTRN28                                                         
         STH   R1,UPDRECN                                                       
         GOTO1 ATSARGET,UPDRECN                                                 
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T READ RECORD                     
*                                                                               
         TM    TWAMODE,TWAMDRFT    TEST DRAFT JOURNAL REQUEST                   
         BZ    UPDTRN24                                                         
         CLI   UPDACTN,ACTFILT     TEST FILTERED JOURNAL REQUESTED              
         BNE   UPDTRN24                                                         
         GOTO1 AFILTER             YES - APPLY OPTIONAL FILTERS                 
         BNE   UPDTRN22                                                         
*                                                                               
UPDTRN24 CLC   RECVNDX,RECVCNDX    TEST FOR THIS RECEIVABLE ACCOUNT             
         BNE   UPDTRN22                                                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARPOST,PZERO      TEST ANYTHING POSTED FOR THIS RECORD         
         BE    UPDTRN22                                                         
         TM    UPDINDS,UPDIRECA    TEST THIS ACCOUNT ACTIVE                     
         BNZ   UPDTRN26                                                         
         OI    UPDINDS,UPDIRECA    SET THIS ACCOUNT IS ACTIVE                   
         MVI   UPDMODE,UPDMRCVF                                                 
         GOTO1 OVERLAY,WORKD       CALL APPLICATION WITH ACCOUNT FIRST          
*                                                                               
UPDTRN26 MVI   UPDMODE,UPDMPROC                                                 
         GOTO1 OVERLAY,WORKD       CALL APPLICATION TO PROCESS RECORD           
         B     UPDTRN22                                                         
*                                                                               
UPDTRN28 TM    UPDINDS,UPDIRECA    TEST ACTIVITY ON THIS ACCOUNT                
         BZ    UPDTRN30                                                         
         OI    UPDINDS,UPDIANYA    YES - SET ANY ACCOUNT WAS ACTIVE             
         MVI   UPDMODE,UPDMRCVL                                                 
         GOTO1 OVERLAY,WORKD                                                    
*                                                                               
UPDTRN30 LA    R4,RECVTABL(R4)     BUMP TO NEXT ACCOUNT                         
         CLI   RECVTABD,RECVTEOT   TEST ALL ACCOUNTS PROCESSED                  
         BNE   UPDTRN20                                                         
*                                                                               
         TM    TWAMODE,TWAMDRFT    TEST DRAFT/LIVE                              
         BO    UPDTRN32                                                         
         GOTO1 AMRKTRN             LIVE - READ AND MARK TRANSACTIONS            
*                                                                               
UPDTRN32 TM    UPDINDS,UPDIANYA    TEST ACTIVITY ON ANY ACCOUNT                 
         BZ    UPDTRN34                                                         
         MVI   UPDMODE,UPDMLAST                                                 
         GOTO1 OVERLAY,WORKD       PASS LAST TIME FOR REPORT                    
*                                                                               
         TM    TWAMODE,TWAMDRFT    TEST DRAFT/LIVE                              
         BO    UPDTRN34                                                         
*                                                                               
         L     R1,AIO1             LIVE - CALL ADDTRN                           
         STCM  R1,15,TRNREC                                                     
         OI    TRNINDS,TRNILAST                                                 
         OI    TRNINDS2,TRNIUPDG                                                
         GOTO1 VADDTRN,TRNBLKD     PASS FINAL CALL TO ADDTRN                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDTRN34 OC    PRTSUB,PRTSUB       TEST REPORT WAS GENERATED                    
         BZ    UPDTRN36                                                         
         MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         LA    R1,RECACTH                                                       
         ST    R1,FVADDR                                                        
         LA    RF,FVXTRA           SET UP XXX,99999                             
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,REPREPNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK(5),DUB                                                      
         LA    R1,4                                                             
         LA    RE,WORK                                                          
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         MVC   4(0,RF),0(RE)                                                    
         EX    R1,*-6                                                           
*                                                                               
UPDTRN36 TM    TWAMODE,TWAMDRFT    TEST DRAFT JOURNAL PRINTED                   
         BZ    ROU2E               UPDATE COMPLETE - RESTORE HEADER             
         NI    TWAMODE,255-TWAMDRFT                                             
         MVC   ACTION,ACTIONL      RESET ACTION                                 
         LA    R1,RECACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IGREPSPL) REPORT XXX,99999 HAS BEEN SPOOLED         
         MVI   FVOMTYP,GTMINF                                                   
         B     ROU2L               LOW - RESTORE BATCH HEADER SCREEN            
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TSAR RECORDS BY LOCATION FILTERS (FROM SCROLL)               *         
* AND BY ABSOLUTE FILTERS (FROM OPTIONS).                             *         
* ON EXIT CC EQU IF TSAR RECORD OK, ELSE CC NEQ                       *         
***********************************************************************         
         SPACE 1                                                                
FILTER   SR    RF,RF                                                            
         OC    LOCACT,LOCACT       LOCATE AN ACCOUNT                            
         BZ    FILT04                                                           
         IC    RF,LOCACTXQ                                                      
         CLI   LOCACT,C'*'         TEST *ULACCOUNT INPUT                        
         BE    FILT02                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RECVCACT+3(0),LOCACT                                             
         BL    ROU2H                                                            
         XC    LOCACT,LOCACT                                                    
         B     FILT04                                                           
*                                                                               
FILT02   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RECVCACT+1(0),LOCACT+1                                           
         BL    ROU2H                                                            
         XC    LOCACT,LOCACT                                                    
*                                                                               
FILT04   OC    LOCCAC,LOCCAC       LOCATE A CONTRA-ACCOUNT                      
         BZ    FILT06                                                           
         IC    RF,LOCCACXQ                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TSARCAC+3(0),LOCCAC                                              
         BL    ROU2H                                                            
         XC    LOCCAC,LOCCAC                                                    
*                                                                               
FILT06   OC    LOCDAT,LOCDAT       LOCATE A DATE                                
         BZ    FILT08                                                           
         CLC   TSARDAT,LOCDAT                                                   
         BL    ROU2H                                                            
         XC    LOCDAT,LOCDAT                                                    
*                                                                               
FILT08   OC    LOCBIL,LOCBIL       LOCATE A BILL                                
         BZ    FILT10                                                           
         IC    RF,LOCBILXQ                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TSARREF(0),LOCBIL                                                
         BL    ROU2H                                                            
         XC    LOCBIL,LOCBIL                                                    
*                                                                               
FILT10   CLI   OPTMARK,0           TEST MARKED FILTER SET                       
         BE    FILT14                                                           
         CLI   OPTMARK,INCLUDE     DOES USER WANT MARKED ONLY                   
         BNE   FILT12                                                           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   FILT14                                                           
*&&                                                                             
         CP    TSARPOST,PZERO                                                   
         BE    ROU2H                                                            
FILT12   CLI   OPTMARK,EXCLUDE     DOES USER WANT UNMARKED ONLY                 
         BNE   FILT14                                                           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST ANY ALLOCATION (CURRENCY)               
         BNE   ROU2H                                                            
*&&                                                                             
         CP    TSARPOST,PZERO                                                   
         BNE   ROU2H                                                            
*                                                                               
FILT14   CLI   OPTBALS,0           TEST BALANCE FILTER SET                      
         BE    FILT18                                                           
         ZAP   DUB,TSARDR          TAKE ORIGINAL DEBITS                         
         TM    TSARINDS,TSARIGRS   TEST INCLUDING DISCOUNT                      
         BNZ   *+10                                                             
         AP    DUB,TSARDISC        YES - SUBTRACT DISCOUNT (-VE AMOUNT)         
         SP    DUB,TSARCR          SUBTRACT CREDITS                             
         SP    DUB,TSARPOST        AND THIS TIME ALLOCATION                     
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE FOREIGN CURRENCY                 
         BZ    FILT16                                                           
         AP    DUB,TSARDR2         ADD ORIGINAL DEBITS (CURRENCY)               
         TM    TSARINDS,TSARIGRS   TEST INCLUDING DISCOUNT                      
         BNZ   *+10                                                             
         AP    DUB,TSARDI2         YES - SUBTRACT DISCOUNT (-VE AMOUNT)         
         SP    DUB,TSARCR2         SUBTRACT CREDITS                             
         SP    DUB,TSARPO2         AND THIS TIME ALLOCATION                     
*&&                                                                             
FILT16   CLI   OPTBALS,INCLUDE     TEST USER WANTS OUT OF BALANCE               
         BNE   *+14                                                             
         CP    DUB,PZERO                                                        
         BE    ROU2H                                                            
         CLI   OPTBALS,EXCLUDE     TEST USER WANTS IN BALANCE ONLY              
         BNE   *+14                                                             
         CP    DUB,PZERO                                                        
         BNE   ROU2H                                                            
*                                                                               
FILT18   CLI   OPTSPCL,0           TEST SPECIAL FILTER SET                      
         BE    FILT20                                                           
         CLI   OPTSPCL,INCLUDE     TEST USER WANTS SPECIALS ONLY                
         BNE   *+12                                                             
         CLI   TSARTYPE,TSARTOVR                                                
         BNE   ROU2H                                                            
         CLI   OPTSPCL,EXCLUDE     TEST USER WANTS NOT SPECIALS                 
         BNE   *+12                                                             
         CLI   TSARTYPE,TSARTOVR                                                
         BE    ROU2H                                                            
*                                                                               
FILT20   CLI   OPTWOFF,0           TEST WRITE-OFF FILTER SET                    
         BE    FILT22                                                           
         CLI   OPTWOFF,INCLUDE     TEST USER WANTS WRITE-OFFS ONLY              
         BNE   *+12                                                             
         TM    TSARINDS,TSARIWOF                                                
         BZ    ROU2H                                                            
         CLI   OPTWOFF,EXCLUDE     TEST USER WANTS NOT WRITE-OFFS               
         BNE   *+12                                                             
         TM    TSARINDS,TSARIWOF                                                
         BNZ   ROU2H                                                            
*                                                                               
FILT22   CLI   OPTDISC,0           TEST DISCOUNT FILTER SET                     
         BE    FILT26                                                           
         CLI   OPTDISC,INCLUDE     TEST USER WANTS DISCOUNTS ONLY               
         BNE   FILT24                                                           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARDI2,PZERO       TEST ANY DISCOUNT (CURRENCY)                 
         BNE   FILT26                                                           
*&&                                                                             
         CP    TSARDISC,PZERO                                                   
         BE    ROU2H                                                            
FILT24   CLI   OPTDISC,EXCLUDE     TEST USER WANTS NOT DISCOUNTS                
         BNE   FILT26                                                           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARDI2,PZERO       TEST ANY DISCOUNT (CURRENCY)                 
         BNE   ROU2H                                                            
*&&                                                                             
         CP    TSARDISC,PZERO                                                   
         BNE   ROU2H                                                            
*                                                                               
FILT26   CLI   OPTXFRS,0           TEST TRANSFER FILTER SET                     
         BE    FILT28                                                           
         CLI   OPTXFRS,INCLUDE     TEST USER WANTS TRANSFERS ONLY               
         BNE   *+12                                                             
         TM    TSARINDS,TSARITRF                                                
         BZ    ROU2H                                                            
         CLI   OPTXFRS,EXCLUDE     TEST USER WANTS NOT TRANSFERS                
         BNE   *+12                                                             
         TM    TSARINDS,TSARITRF                                                
         BNZ   ROU2H                                                            
*                                                                               
FILT28   CLI   OPTHELD,0           TEST STATUS FILTERS                          
         BNE   *+8                                                              
         CLI   OPTCLRNG,0                                                       
         BNE   *+8                                                              
         CLI   OPTQUER,0                                                        
         BE    FILT30                                                           
         MVI   DUB,EXCLUDE         ASSUME RECORD NOT WANTED                     
         CLI   OPTHELD,INCLUDE     TEST WANT HELD                               
         BNE   *+16                                                             
         TM    TSARIND2,TSAR2HLD   TEST HELD                                    
         BZ    *+8                                                              
         MVI   DUB,INCLUDE                                                      
         CLI   OPTHELD,EXCLUDE     TEST DON'T WANT HELD                         
         BNE   *+16                                                             
         TM    TSARIND2,TSAR2HLD   TEST HELD                                    
         BO    *+8                                                              
         MVI   DUB,INCLUDE                                                      
         CLI   OPTQUER,INCLUDE     TEST WANT QUERIED                            
         BNE   *+16                                                             
         TM    TSARIND2,TSAR2QRD   TEST QUERIED                                 
         BZ    *+8                                                              
         MVI   DUB,INCLUDE                                                      
         CLI   OPTQUER,EXCLUDE     TEST DON'T WANT QUERIED                      
         BNE   *+16                                                             
         TM    TSARIND2,TSAR2QRD   TEST QUERIED                                 
         BO    *+8                                                              
         MVI   DUB,INCLUDE                                                      
*&&UK                                                                           
         CLI   OPTCLRNG,INCLUDE    TEST WANT CLEARING                           
         BNE   *+16                                                             
         TM    TSARIND2,TSAR2CLR   TEST CLEARING                                
         BZ    *+8                                                              
         MVI   DUB,INCLUDE                                                      
         CLI   OPTCLRNG,EXCLUDE    TEST DON'T WANT CLEARING                     
         BNE   *+16                                                             
         TM    TSARIND2,TSAR2CLR   TEST CLEARING                                
         BO    *+8                                                              
         MVI   DUB,INCLUDE                                                      
*&&                                                                             
         CLI   DUB,INCLUDE         TEST RECORD WANTED                           
         BNE   ROU2H                                                            
*                                                                               
FILT30   CLI   OPTOFST,0           TEST OFFSET FILTER SET                       
         BE    FILT32                                                           
         CLI   OPTOFST,INCLUDE     TEST USER WANTS OFFSETS ONLY                 
         BNE   *+12                                                             
         TM    TSARINDS,TSARIOFS                                                
         BZ    ROU2H                                                            
         CLI   OPTOFST,EXCLUDE     TEST USER WANTS NOT OFFSETS                  
         BNE   *+12                                                             
         TM    TSARINDS,TSARIOFS                                                
         BNZ   ROU2H                                                            
*                                                                               
FILT32   DS    0H                  NEXT ONE-PART FILTER                         
*                                                                               
         OC    OPTDATE,OPTDATE     DATE FILTER                                  
         BZ    *+14                                                             
         CLC   TSARDAT,OPTDATE                                                  
         BNE   ROU2H                                                            
*                                                                               
         OC    OPTBIL,OPTBIL       BILL NUMBER FILTER                           
         BZ    FILT42                                                           
         IC    RF,OPTBILXQ                                                      
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD                  
         BNO   *+8                                                              
         LA    RF,L'TSARREF-1                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TSARREF(0),OPTBIL                                                
         BNE   ROU2H                                                            
*                                                                               
FILT42   OC    OPTACTX,OPTACTX     ACCOUNT FILTER                               
         BZ    *+14                                                             
         CLC   RECVCNDX,OPTACTX                                                 
         BNE   ROU2H                                                            
*                                                                               
         OC    OPTAMT,OPTAMT       AMOUNT FILTER                                
         BZ    FILT48                                                           
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE FOREIGN CURRENCY                 
         BZ    FILT44                                                           
         ZAP   DUB,TSARDR2         USE CURRENCY AMOUNTS                         
         SP    DUB,TSARCR2                                                      
         OC    DISC,DISC                                                        
         BZ    *+10                                                             
         AP    DUB,TSARDI2                                                      
         B     FILT46                                                           
*&&                                                                             
FILT44   ZAP   DUB,TSARDR          FILTERS ON BALANCE (DEBIT-CREDIT)            
         SP    DUB,TSARCR                                                       
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT KNOWN                  
         BZ    *+10                                                             
         AP    DUB,TSARDISC        YES - SUBTRACT DISCOUNT (-VE AMOUNT)         
FILT46   CP    DUB,OPTAMT                                                       
         BNE   ROU2H                                                            
*                                                                               
FILT48   OC    OPTCAC,OPTCAC       BILL SOURCE FILTER                           
         BZ    FILT50                                                           
         IC    RF,OPTCACXQ                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   OPTCAC(0),TSARCAC+3                                              
         BNE   ROU2H                                                            
*                                                                               
FILT50   OC    OPTOFF,OPTOFF       OFFICE FILTER                                
         BZ    FILT52                                                           
         CLC   TSAROFFC,OPTOFF                                                  
         BNE   ROU2H                                                            
*&&US                                                                           
FILT52   DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
FILT52   OC    OPTCUR,OPTCUR       CURRENCY FILTER                              
         BZ    FILT54                                                           
         CLC   TSARCUR,OPTCUR                                                   
         BE    FILT54                                                           
         OC    TSARCUR,TSARCUR     TEST CURRENCY KNOWN                          
         BNZ   ROU2H                                                            
         MVC   DUB(L'AGYCURR),CDEFCUR                                           
         OC    AGYCURR,AGYCURR                                                  
         BZ    *+10                                                             
         MVC   DUB(L'AGYCURR),AGYCURR                                           
         CLC   OPTCUR,DUB          TEST AGENCY CURRENCY                         
         BNE   ROU2H                                                            
*&&                                                                             
FILT54   DS    0H                  NEXT TWO-PART FILTER                         
*                                                                               
         B     ROU2E               TSAR RECORD PASSES                           
         EJECT                                                                  
***********************************************************************         
* MAINTAIN BILLING SOURCE RECORD FOR THIS COMPANY                     *         
***********************************************************************         
         SPACE 1                                                                
VALBSC   TM    TWAMODE2,TWA2BSRC   TEST SOURCE SCREEN LOADED                    
         BNZ   VALBSC10                                                         
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(2,0),TWAD                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWA2BSRC                                                
         MVC   TWASCRSS,TWASCROV                                                
         GOTO1 AOVRSCR,TWASCRBS    OVERLAY BILLING SOURCE SCREEN                
         LA    R2,KEY                                                           
         USING BSCRECD,R2                                                       
         XC    BSCKEY,BSCKEY                                                    
         MVI   BSCKTYP,BSCKTYPQ                                                 
         MVC   BSCKCPY,COMPANY                                                  
         GOTO1 AIOREAD                                                          
         BE    VALBSC2                                                          
         CLI   DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BE    VALBSC8                                                          
         DC    H'0'                                                             
*                                                                               
VALBSC2  L     R2,AIO1             R2=A(RECORD)                                 
         AH    R2,DATADISP                                                      
         USING BSCELD,R2                                                        
         LA    R1,SRCLIN1H                                                      
         SR    R0,R0                                                            
VALBSC4  CLI   BSCEL,0             TEST EOR                                     
         BE    VALBSC8                                                          
         CLI   BSCEL,BSCELQ        TEST BILLING SOURCE ELEMENT                  
         BNE   VALBSC6                                                          
         SR    RE,RE                                                            
         IC    RE,BSCLN                                                         
         SH    RE,=Y(BSCBSRC+1-BSCELD)                                          
         CHI   RE,11                                                            
         BNH   *+8                                                              
         LA    RE,11                                                            
         MVC   L'FVIHDR(0,R1),BSCBSRC                                           
         EX    RE,*-6                                                           
         IC    R0,FVTLEN-FVIHDR(R1)                                             
         AR    R1,R0               BUMP TO NEXT OUTPUT FIELD                    
VALBSC6  IC    R0,BSCLN            BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         B     VALBSC4                                                          
*                                                                               
VALBSC8  LA    R1,SRCLIN1H                                                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAENTCHA)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         OC    TWASAGN,TWASAGN     TEST OLD/NEW SECURITY                        
         BNZ   *+16                                                             
         TM    TWAAUTH,ACTIAUT2    TEST UPDATE AUTHORISATION                    
         BNZ   ROU2E                                                            
         B     VALBSC09                                                         
         MVI   BYTE,FLDSRCE        TEST FIELD WRITE PROTECTED                   
         LR    R0,R1                                                            
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECBLK),BYTE                            
         LR    R1,R0                                                            
         BE    ROU2E                                                            
*                                                                               
VALBSC09 SR    RE,RE               NO - PROTECT ALL INPUT FIELDS                
         LA    RF,4095(R1)                                                      
         OI    FVATRB-FVIHDR(R1),FVAPROT                                        
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+8                                                              
         BXLE  R1,RE,*-12                                                       
         LA    R1,RECACTH          POSITION CURSOR TO ACTION FIELD              
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IASRCDIS)                                           
         B     ROU2E                                                            
*                                                                               
VALBSC10 TM    TWAAUTH,ACTIAUT2    TEST UPDATE AUTHORISATION                    
         BZ    VALBSC38                                                         
         LA    R1,SRCLIN1H                                                      
         SR    R0,R0                                                            
VALBSC12 CLI   FVTLEN-FVIHDR(R1),L'SRCLIN1H+L'SRCLIN1                           
         BNE   VALBSC38                                                         
         TM    FVIIND-FVIHDR(R1),FVITHIS                                        
         BNZ   *+14                                                             
         IC    R0,FVTLEN-FVIHDR(R1)                                             
         AR    R1,R0                                                            
         B     VALBSC12                                                         
*                                                                               
         LA    R2,KEY                                                           
         USING BSCRECD,R2                                                       
         XC    BSCKEY,BSCKEY                                                    
         MVI   BSCKTYP,BSCKTYPQ                                                 
         MVC   BSCKCPY,COMPANY                                                  
         GOTO1 AIOREAD                                                          
         BNE   *+12                                                             
         MVI   UPDMODE,0           SET WRITE RECORD                             
         B     VALBSC14                                                         
         CLI   DMCB+8,X'10'        TEST NOT FOUND                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDMODE,1           SET ADD RECORD                               
*                                                                               
VALBSC14 L     R2,AIO1             BUILD VIRGIN RECORD (NO ELEMENTS)            
         XC    BSCRECD(256),BSCRECD                                             
         MVC   BSCKEY,KEY                                                       
         LH    R1,DATADISP                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BSCRLEN                                                     
*                                                                               
         LA    R3,SRCLIN1H                                                      
         SR    R0,R0                                                            
VALBSC16 CLI   FVTLEN-FVIHDR(R3),L'SRCLIN1H+L'SRCLIN1                           
         BNE   VALBSC26                                                         
         GOTO1 AFVAL,(R3)                                                       
         BNE   VALBSC24                                                         
*                                                                               
         LA    RF,TEMP             BUILD BILLING SOURCE ELEMENT                 
         USING BSCELD,RF                                                        
         MVI   BSCEL,BSCELQ                                                     
         MVI   BSCLN,BSCLNQ                                                     
         MVC   BSCBSRC,FVIFLD                                                   
         OC    BSCBSRC,SPACES                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         LA    RE,BSCBSRC-BSCELD(RE)                                            
         BCTR  RE,0                CHECK FOR DUPLICATES                         
         CHI   RE,11                                                            
         BNH   *+8                                                              
         LA    RE,11                                                            
*                                                                               
         LA    R1,BSCRECD                                                       
         AH    R1,DATADISP                                                      
VALBSC18 CLI   BSCEL-BSCELD(R1),0  TEST EOR                                     
         BE    VALBSC22                                                         
         CLI   BSCEL-BSCELD(R1),BSCELQ                                          
         BNE   VALBSC20                                                         
         EX    RE,*+8              MATCH ELEMENTS                               
         BNE   VALBSC20                                                         
         CLC   BSCELD(0),BSCEL-BSCELD(R1)                                       
         MVC   FVMSGNO,=AL2(EADUPFLD)                                           
         B     ROU2H                                                            
VALBSC20 IC    R0,BSCLN-BSCELD(R1) BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     VALBSC18                                                         
*                                                                               
VALBSC22 GOTO1 VHELLO,DMCB,(C'P',ACCFIL),BSCRECD,BSCELD,0,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VALBSC24 IC    R0,FVTLEN-FVIHDR(R3)                                             
         AR    R3,R0                                                            
         B     VALBSC16                                                         
         DROP  RF                                                               
*                                                                               
VALBSC26 LA    R0,DMWRT            WRITE RECORD BACK                            
         CLI   UPDMODE,0           TEST RECORD FOUND                            
         BE    *+8                                                              
         LA    R0,DMADD            ADD A NEW RECORD                             
         GOTO1 VDATAMGR,DMCB,(0,(R0)),ACCFIL,BSCRECD,BSCRECD                    
         BE    *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
*                                                                               
         AH    R2,DATADISP         RE-DISPLAY RECORD                            
         USING BSCELD,R2                                                        
         LA    R1,SRCLIN1H                                                      
         SR    R0,R0                                                            
VALBSC28 CLI   BSCEL,0             TEST EOR                                     
         BE    VALBSC32                                                         
         CLI   BSCEL,BSCELQ        TEST BILLING SOURCE ELEMENT                  
         BNE   VALBSC30                                                         
         XC    L'FVIHDR(L'SRCLIN1,R1),L'FVIHDR(R1)                              
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         MVC   L'FVIHDR(L'BSCBSRC,R1),BSCBSRC                                   
         IC    R0,FVTLEN-FVIHDR(R1)                                             
         AR    R1,R0               BUMP TO NEXT OUTPUT FIELD                    
VALBSC30 IC    R0,BSCLN            BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         B     VALBSC28                                                         
*                                                                               
VALBSC32 CLI   FVTLEN-FVIHDR(R1),L'SRCLIN1H+L'SRCLIN1                           
         BNE   VALBSC34                                                         
         XC    L'FVIHDR(L'SRCLIN1,R1),L'FVIHDR(R1)                              
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         IC    R0,FVTLEN-FVIHDR(R1)                                             
         AR    R1,R0                                                            
         B     VALBSC32                                                         
*                                                                               
VALBSC34 LA    R1,SRCLIN1H         OUTPUT MESSAGE                               
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IACHACHA)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     ROU2E                                                            
         DROP  R2                                                               
*                                                                               
VALBSC38 ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y((SAVEAREA+SAVEAREL)-RECOLAYH)                            
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(2,0),ATIA,,(RF)                    
         LA    R0,RECOLAYH                                                      
         LH    R1,=Y((SAVEAREA+SAVEAREL)-RECOLAYH)                              
         L     RE,ATIA                                                          
         MVC   RECMSG,RECMSG-TWAD(RE)                                           
         LA    RE,RECOLAYH-TWAD(RE)                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 AXMTSCR                                                          
         MVC   TWASCROV,TWASCRSS                                                
         NI    TWAMODE2,255-TWA2BSRC                                            
         MVC   RECACT,ACTNAME                                                   
         OI    RECACTH+(FVOIND-FVIHDR),FVOCUR                                   
         MVC   ACTION,ACTIONL                                                   
         B     ROU2L                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION MEMO SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
PROMEM   TM    TWAMODE2,TWA2MEMO   TEST MEMO MODE                               
         BO    PROMEM20                                                         
         MVC   STSARTYP,TSARTYPE   SAVE RECORD TYPE                             
         L     R1,ATSARBLK         SAVE CURRENT TSAR RECORD NUMBER              
         USING TSARD,R1                                                         
         MVC   STSRNUM,TSRNUM                                                   
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(2,0),TWAD                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWA2MEMO                                                
         MVC   TWASCRSS,TWASCROV                                                
         GOTO1 AOVRSCR,TWASCRMM    OVERLAY MEMO SCREEN                          
*                                                                               
         TM    TSARIND2,TSAR2QRD+TSAR2HLD+TSAR2CLR                              
         BNZ   *+10                                                             
         XC    MEMSTTX,MEMSTTX     CLEAR STATUS FIELD TEXT                      
         TM    TSARIND2,TSAR2QRD                                                
         BZ    *+10                                                             
         MVC   MEMSTAT(L'AC@QUERD),AC@QUERD                                     
         TM    TSARIND2,TSAR2HLD   SET HELD                                     
         BZ    *+10                                                             
         MVC   MEMSTAT(L'AC@HELD),AC@HELD                                       
*&&UK                                                                           
         TM    TSARIND2,TSAR2CLR   SET CLEARING                                 
         BZ    *+10                                                             
         MVC   MEMSTAT(L'AC@CLRNG),AC@CLRNG                                     
*&&                                                                             
         OC    TSARXSEQ,TSARXSEQ   TEST EXTENSION RECORD EXISTS                 
         BZ    *+14                                                             
         MVC   STS2SEQ,TSARXSEQ                                                 
         B     PROMEM14                                                         
         MVC   TSARLEN,=AL2(TS2LEN)                                             
         XC    TSARKEY(TSARKEYL),TSARKEY                                        
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         MVC   TS2TYP,=AL2(FFFF)                                                
         L     R1,ATSARBLK         BUILD EXTENSION TSAR RECORD                  
         MVI   TSACTN,TSARDH                                                    
         GOTO1 VTSAR                                                            
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         TM    TSERRS,TSEEOF       TEST EOF                                     
         BO    PROMEM02                                                         
         CLC   TS2TYP,=AL2(FFFF)   TEST EXTENSION RECORD FOUND                  
         BNE   PROMEM02                                                         
         ICM   RF,3,TS2SEQ         YES - TAKE LAST SEQUENCE#                    
         BCTR  RF,0                                                             
         STCM  RF,3,TS2SEQ         SET NEXT SEQUENCE#                           
         B     PROMEM04                                                         
PROMEM02 XC    TSARKEY(TSARKEYL),TSARKEY                                        
         MVC   TS2TYP,=AL2(FFFF)                                                
         MVC   TS2SEQ,=AL2(FFFF)   FIRST EXTENSION RECORD                       
*                                                                               
PROMEM04 CLI   STSARTYP,TSARTTRN   TEST TRANSACTION ON FILE                     
         BNE   PROMEM12                                                         
         LA    R2,KEY                                                           
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,RECVCACT                                                
         MVC   TRNKCULC,TSARCAC                                                 
         TM    COMPSTA4,CPYSOFF2   TEST TWO CHARACTER OFFICES IN USE            
         BZ    *+18                                                             
         CLI   FILEFORM,ISDAQ      TEST NEW FILE IN USE                         
         BNE   *+10                                                             
         MVC   TRNKOFF,TSAROFFC                                                 
         MVC   TRNKDATE,TSARDAT                                                 
         MVC   TRNKREF,TSARREF                                                  
         MVC   TRNKSBR,TSARSUB                                                  
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),ACCFIL,TRNRECD,AIO1                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,TRNRECD+ACCORFST                                              
         USING NOTELD,R3                                                        
         LA    R4,TS2ELE           R4=A(FIRST TSAR DATA ELEMENT)                
         USING TS2ELE,R4                                                        
PROMEM06 CLI   NOTEL,0             TEST NO MORE NOTE ELEMENTS                   
         BE    PROMEM12                                                         
         CLI   NOTEL,NOTELQ        TEST NOTE ELEMENT                            
         BNE   PROMEM10                                                         
         MVC   TS2DATE,NOTDATE                                                  
         MVC   TS2REF,NOTREF                                                    
         SR    RF,RF                                                            
         IC    RF,NOTLN                                                         
         SH    RF,=Y(NOTLN1Q+1)    RF=L'TEXT-1                                  
         BM    PROMEM08            NO TEXT                                      
         CH    RF,=Y(L'TS2NOTE)                                                 
         BNH   *+8                                                              
         LH    RF,=Y(L'TS2NOTE-1)                                               
         MVC   TS2NOTE(0),NOTNOTE                                               
         EX    RF,*-6                                                           
PROMEM08 LA    R4,TS2ELEL(R4)      BUMP TO NEXT TSAR DATA ELEMENT               
         DROP  R4                                                               
PROMEM10 SR    R0,R0               BUMP TO MEXT ELEMENT                         
         IC    R0,NOTLN                                                         
         AR    R3,R0                                                            
         B     PROMEM06                                                         
         DROP  R3                                                               
PROMEM12 L     R1,ATSARBLK         BUILD EXTENSION TSAR RECORD                  
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   STS2SEQ,TS2SEQ      SAVE SEQUENCE NUMBER                         
         GOTO1 ATSARGET,STSRNUM    READ SAVED TSAR RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TSARXSEQ,STS2SEQ                                                 
         L     R1,ATSARBLK                                                      
         MVI   TSACTN-TSARD(R1),TSAPUT                                          
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROMEM14 L     R1,ATSARBLK         READ EXTENSION TSAR RECORD                   
         MVI   TSACTN,TSARDH                                                    
         MVC   TSARLEN,=Y(TSARMAXL)                                             
         XC    TSARKEY(TSARKEYL),TSARKEY                                        
         MVC   TS2TYP,=AL2(FFFF)                                                
         MVC   TS2SEQ,STS2SEQ                                                   
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,MEMDAT1H                                                      
         USING MEMDAT1H,R4                                                      
         LA    R3,TS2ELE                                                        
         USING TS2ELE,R3                                                        
         LA    R0,TS2ELEN                                                       
PROMEM16 OC    TS2DATE,TS2DATE     TEST DATE PRESENT                            
         BZ    PROMEM18                                                         
         GOTO1 VDATCON,DMCB,(2,TS2DATE),(17,MEMDAT1)                            
         MVI   MEMDAT1H+(FVILEN-FVIHDR),L'MEMDAT1                               
PROMEM18 MVC   MEMREF1,TS2REF                                                   
         MVC   MEMMEM1,TS2NOTE                                                  
         LA    R4,MEMDAT2H         BUMP TO NEXT SCREEN LINE                     
         DROP  R4                                                               
         LA    R3,TS2ELEL(R3)      BUMP TO NEXT TSAR DATA ELEMENT               
         DROP  R3                                                               
         BCT   R0,PROMEM16                                                      
         OI    MEMIND1,MEMIACTV    SET ACTIVITY THIS TIME (NO ERROR)            
         XC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
         B     PROMEM80                                                         
         DROP  R2                                                               
*                                                                               
PROMEM20 MVC   TSARLEN,=Y(TSARMAXL)                                             
         XC    TSARKEY(TSARKEYL),TSARKEY                                        
         MVC   TS2TYP,=AL2(FFFF)                                                
         MVC   TS2SEQ,STS2SEQ                                                   
         L     R1,ATSARBLK                                                      
         MVI   TSACTN,TSARDH                                                    
         GOTO1 VTSAR               READ EXTENSION TSAR RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MEMIND1,0                                                        
         GOTO1 AXMTSCR                                                          
         LA    R4,MEMDAT1H         VALIDATE MEMORANDUM SCREEN                   
         USING MEMDAT1H,R4                                                      
         LA    R3,TS2ELE                                                        
         USING TS2ELE,R3                                                        
         LA    R0,TS2ELEN                                                       
         XC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
*                                                                               
PROMEM22 TM    MEMDAT1H+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    PROMEM30                                                         
         OI    MEMIND1,MEMIACTV    SET ACTIVITY THIS TIME                       
         XC    TS2DATE,TS2DATE                                                  
         GOTO1 AFVAL,MEMDAT1H                                                   
         BL    PROMEM30                                                         
         BE    PROMEM24                                                         
         OC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
         BNZ   PROMEM30                                                         
         MVC   TEMP(L'FVADDR),FVADDR                                            
         MVC   TEMP+L'FVADDR(L'FVMSGNO),FVMSGNO                                 
         B     PROMEM30                                                         
PROMEM24 MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   PROMEM26                                                         
         LA    R2,WORK             EDIT OUT DATE                                
         USING PERVALD,R2                                                       
         CLC   PVALPSTA,TODAYP     CAN'T BE HIGHER THAN TODAY                   
         BH    PROMEM26                                                         
         MVC   TS2DATE,PVALCSTA    PACKED START DATE                            
         GOTO1 VDATCON,DMCB,(2,TS2DATE),(17,MEMDAT1)                            
         B     PROMEM30                                                         
PROMEM26 OC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
         BNZ   PROMEM30                                                         
         MVC   TEMP(L'FVADDR),FVADDR                                            
         MVC   TEMP+L'FVADDR(L'FVMSGNO),=AL2(EGDATINV)                          
*                                                                               
PROMEM30 TM    MEMREF1H+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    PROMEM40                                                         
         OI    MEMIND1,MEMIACTV    SET ACTIVITY THIS TIME                       
         XC    TS2REF,TS2REF                                                    
         GOTO1 AFVAL,MEMREF1H                                                   
         BL    PROMEM40                                                         
         BE    PROMEM34                                                         
         OC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
         BNZ   PROMEM40                                                         
         MVC   TEMP(L'FVADDR),FVADDR                                            
         MVC   TEMP+L'FVADDR(L'FVMSGNO),FVMSGNO                                 
         B     PROMEM40                                                         
PROMEM34 MVC   TS2REF,FVIFLD                                                    
         MVC   MEMREF1,TS2REF                                                   
*                                                                               
PROMEM40 TM    MEMMEM1H+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    PROMEM50                                                         
         OI    MEMIND1,MEMIACTV    SET ACTIVITY THIS TIME                       
         XC    TS2NOTE,TS2NOTE                                                  
         GOTO1 AFVAL,MEMMEM1H                                                   
         BL    PROMEM50                                                         
         BE    PROMEM44                                                         
         OC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
         BNZ   PROMEM50                                                         
         MVC   TEMP(L'FVADDR),FVADDR                                            
         MVC   TEMP+L'FVADDR(L'FVMSGNO),FVMSGNO                                 
         B     PROMEM50                                                         
PROMEM44 MVC   TS2NOTE,FVIFLD                                                   
         MVC   MEMMEM1,TS2NOTE                                                  
*                                                                               
PROMEM50 OC    TS2REF(L'TS2REF+L'TS2NOTE),TS2REF                                
         BZ    PROMEM52                                                         
         OC    TS2DATE,TS2DATE     TEST DATE PRESENT                            
         BNZ   PROMEM52                                                         
         MVC   TS2DATE,TODAYC      DEFAULT TO TODAY                             
         GOTO1 VDATCON,DMCB,(2,TS2DATE),(17,MEMDAT1)                            
         OI    MEMIND1,MEMIACTV    SET ACTIVITY THIS TIME                       
PROMEM52 LA    R4,MEMDAT2H         BUMP TO NEXT SCREEN LINE                     
         DROP  R4                                                               
         LA    R3,TS2ELEL(R3)      BUMP TO NEXT TSAR DATA ELEMENT               
         DROP  R3                                                               
         BCT   R0,PROMEM22                                                      
*                                                                               
PROMEM80 L     R1,ATSARBLK         WRITE EXTENSION RECORD                       
         MVI   TSACTN,TSAWRT                                                    
         GOTO1 VTSAR                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    TEMP(L'FVADDR+L'FVMSGNO),TEMP                                    
         BZ    PROMEM82                                                         
         MVC   FVADDR,TEMP         EXIT IN ERROR                                
         MVC   FVMSGNO,TEMP+L'FVADDR                                            
         B     ROU2H                                                            
*                                                                               
PROMEM82 TM    MEMIND1,MEMIACTV    TEST ACTIVITY THIS TIME                      
         BZ    PROMEM86                                                         
         LA    R0,TS2ELEN          SET A(FIRST EMPTY LINE)                      
         LA    R1,MEMDAT1H                                                      
         USING MEMDAT1H,R1                                                      
         CLI   MEMDAT1H+FVILEN-FVIHDR,0                                         
         BE    PROMEM84                                                         
         LA    R1,MEMDAT2H                                                      
         BCT   R0,*-12                                                          
         DROP  R1                                                               
         SH    R1,=Y(MEMDAT2H-MEMDAT1H)                                         
PROMEM84 ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAENTCHA)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     ROU2E                                                            
         DROP  R2                                                               
*                                                                               
PROMEM86 ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y((SAVEAREA+SAVEAREL)-RECOLAYH)                            
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(2,0),ATIA,,(RF)                    
         LA    R0,RECOLAYH                                                      
         LH    R1,=Y((SAVEAREA+SAVEAREL)-RECOLAYH)                              
         L     RE,ATIA                                                          
         MVC   RECMSG,RECMSG-TWAD(RE)                                           
         LA    RE,RECOLAYH-TWAD(RE)                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 AXMTSCR                                                          
         MVC   TWASCROV,TWASCRSS                                                
         NI    TWAMODE2,255-TWA2MEMO                                            
         XC    STS2SEQ,STS2SEQ                                                  
         XC    TSARDATA(TSARDATL),TSARDATA                                      
         GOTO1 ATSARGET,STSRNUM    READ SAVED MAIN TSAR RECORD                  
         MVI   ANYMARK,1           SET SOMETHING MARKED THIS TIME               
         MVI   ANYUPDAT,1          SET UPDATE FLAG                              
         BE    ROU2L                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TRANSMIT ALL FIELDS ON SCREEN & SET BEFORE & AFTER INDICATORS       *         
***********************************************************************         
         SPACE 1                                                                
XMTSCR   LA    R1,RECMSGH          RE-TRANSMIT HEADER SCREEN                    
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,0(R1)                                                       
         BZ    *+8                                                              
         BXLE  R1,RE,*-12                                                       
         MVI   1(R1),1             SET INDICS                                   
         MVI   2(R1),1                                                          
         B     ROU2E                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD TOTALS LINES ON A SCREEN                           *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF FIRST LINE)                             *         
***********************************************************************         
         SPACE 1                                                                
BLDTOT   LR    R2,R1               R2=A(HEADER OF 1ST TWA FIELD)                
         CLC   TOTPROF,TOTPROFS    TEST PROFILE CHANGED                         
         BE    BLDTOT16                                                         
         XC    TOTDISP(TOTDISPL),TOTDISP                                        
         MVC   L'FVIHDR(L'TOTHEAD,R2),SPACES                                    
*                                                                               
BLDTOT02 ICM   R1,8,TOTPROF        R1=TOTALS MASK                               
         BZ    BLDTOTX                                                          
         SR    RE,RE                                                            
         LA    RF,8                                                             
BLDTOT04 SR    R0,R0               COUNT NUMBER OF BITS ON IN MASK              
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         BCT   RF,BLDTOT04                                                      
*                                                                               
         LA    R1,L'TOTHEAD                                                     
         SR    R0,R0                                                            
         DR    R0,RE               R1=WIDTH OF EACH ENTRY                       
         CH    R1,=Y(TOTMINW)      TEST ENTRIES WILL FIT ON LINE                
         BNL   BLDTOT08                                                         
*                                                                               
         IC    R0,TOTPROF                                                       
         LA    RE,X'01'                                                         
         LA    RF,8                                                             
BLDTOT06 SR    R1,R1               TURN LEAST SIGNIFICANT MASK BIT OFF          
         SRDL  R0,1                                                             
         LTR   R1,R1                                                            
         BNZ   *+12                                                             
         SLL   RE,1                                                             
         BCT   RF,BLDTOT06                                                      
         STC   RE,WORK             RE=MASK BIT TO TURN OFF                      
         XI    WORK,X'FF'          INVERT MASK                                  
         NC    TOTPROF,WORK        AND LEAVE ALL OTHER BITS ON                  
         B     BLDTOT02            NOW TRY TO FIT AGAIN                         
*                                                                               
BLDTOT08 LA    RE,L'FVIHDR         BUILD DISPLACEMENTS IN DISPLAY ORDER         
         LA    R0,TOTTABN          R0=NUMBER OF ENTRIES IN TABLE                
         L     R1,ATOTTAB          R1=A(DISPLAY SEQUENCE TABLE)                 
         USING TOTTABD,R1                                                       
BLDTOT10 MVC   WORK(1),TOTMASK     TEST TABLE BIT ON IN MASK                    
         NC    WORK(1),TOTPROF                                                  
         BZ    BLDTOT12                                                         
         SR    RF,RF                                                            
         IC    RF,TOTADDR                                                       
         LA    RF,TOTDISP(RF)      POINT TO DISPLACEMENT VALUE                  
         STC   RE,0(RF)            SET DISPLACEMENT TO VALUE                    
         AH    RE,=Y(TOTMINW)      INCREMENT DISPLACEMENT VALUE                 
BLDTOT12 LA    R1,TOTTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,BLDTOT10         DO FOR NUMBER OF ENTRIES                     
         DROP  R1                                                               
*                                                                               
         LA    RE,TOTALS           RE=A(HEADING LIST)                           
         LA    RF,TOTDISP          RF=A(DISPLACEMENT TABLE)                     
         LA    R0,TOTDISPL         R0=NUMBER OF ENTRIES IN TABLE                
BLDTOT14 SR    R1,R1                                                            
         ICM   R1,1,0(RF)          GET DISPLACEMENT TO HEADING                  
         BZ    *+12                                                             
         AR    R1,R2               ADDRESS & SET HEADING VALUE                  
         MVC   0(TOTMINW-1,R1),0(RE)                                            
         LA    RE,TOTMINW-1(RE)    BUMP TO NEXT HEADING ENTRY                   
         LA    RF,1(RF)            BUMP TO NEXT DISPLACEMENT VALUE              
         BCT   R0,BLDTOT14                                                      
         MVC   TOTHEAD,L'FVIHDR(R2)                                             
*                                                                               
BLDTOT16 MVC   TOTPROFS,TOTPROF                                                 
         MVC   L'FVIHDR(L'TOTHEAD,R2),TOTHEAD                                   
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         SR    R1,R1               BUMP TO NEXT TWA FIELD                       
         IC    R1,FVTLEN-FVIHDR(R2)                                             
         AR    R2,R1               R2=A(TOTALS LINE)                            
         TM    FVATRB-FVIHDR(R2),FVAXTND                                        
         BZ    BLDTOT18                                                         
         IC    R1,FVTLEN-FVIHDR(R2)                                             
         AR    R1,R2                                                            
         SH    R1,=Y(L'FVIHDR)                                                  
         OI    FVATRB-FVIHDR(R2),FVAHIGH                                        
         MVI   FLDXXATT-FLDXNUM(R1),FXATCGRN                                    
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT18                                                         
         NI    FVATRB-FVIHDR(R2),255-(FVAHIGH)                                  
         MVI   FLDXXATT-FLDXNUM(R1),FXATCRED                                    
BLDTOT18 MVC   L'FVIHDR(L'TOTHEAD,R2),SPACES                                    
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
*                                                                               
         OC    TOTCHQD,TOTCHQD     DISPLAY CHEQUE AMOUNT                        
         BZ    BLDTOT22                                                         
         ZAP   DUB,CHQAMT                                                       
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
*&&UK                                                                           
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT20                                                         
         ZAP   DUB,CHQAM2          DISPLAY LOCAL CURRENCY CHEQUE AMOUNT         
         MVC   TEMP(L'AGYCURT),FORCURT                                          
*&&                                                                             
BLDTOT20 SR    RF,RF                                                            
         IC    RF,TOTCHQD                                                       
         AR    RF,R2                                                            
         CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOT22 OC    TOTMRKD,TOTMRKD     DISPLAY MARKED (ALLOCATED) AMOUNT            
         BZ    BLDTOT28                                                         
         ZAP   DUB,MRKTOT                                                       
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
*&&UK                                                                           
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT24                                                         
         ZAP   DUB,MRKTO2                                                       
         MVC   TEMP(L'AGYCURT),FORCURT                                          
*&&                                                                             
BLDTOT24 SR    RF,RF                                                            
         IC    RF,TOTMRKD                                                       
         AR    RF,R2                                                            
         CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOT28 OC    TOTDIFD,TOTDIFD     DISPLAY DIFFERENCES (OVERS) AMOUNT           
         BZ    BLDTOT34                                                         
         ZAP   DUB,DIFTOT                                                       
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
*&&UK                                                                           
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT30                                                         
         ZAP   DUB,DIFTO2                                                       
         MVC   TEMP(L'AGYCURT),FORCURT                                          
*&&                                                                             
BLDTOT30 SR    RF,RF                                                            
         IC    RF,TOTDIFD                                                       
         AR    RF,R2                                                            
         CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOT34 OC    TOTDSCD,TOTDSCD     DISPLAY DISCOUNT AMOUNT                      
         BZ    BLDTOT40                                                         
         ZAP   DUB,DISCAMT                                                      
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
*&&UK                                                                           
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT36                                                         
         ZAP   DUB,DISCAM2                                                      
         MVC   TEMP(L'AGYCURT),FORCURT                                          
*&&                                                                             
BLDTOT36 SR    RF,RF                                                            
         IC    RF,TOTDSCD                                                       
         AR    RF,R2                                                            
         CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOT40 SR    RF,RF               DISPLAY WRITE-OFF AMOUNT                     
         ICM   RF,1,TOTWOFD                                                     
         BZ    BLDTOT46                                                         
         AR    RF,R2                                                            
         ZAP   DUB,WOFFAMT                                                      
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
*&&UK                                                                           
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT42                                                         
         ZAP   DUB,WOFFAM2                                                      
         MVC   TEMP(L'AGYCURT),FORCURT                                          
*&&                                                                             
BLDTOT42 CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOT46 SR    RF,RF               DISPLAY OFFSET TOTAL                         
         ICM   RF,1,TOTOFSD                                                     
         BZ    BLDTOT52                                                         
         AR    RF,R2                                                            
         ZAP   DUB,OFSTOT                                                       
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
*&&UK                                                                           
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT48                                                         
         ZAP   DUB,OFSTO2                                                       
         MVC   TEMP(L'AGYCURT),FORCURT                                          
*&&                                                                             
BLDTOT48 CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOT52 SR    RF,RF               DISPLAY VAT (OR TRANSFERRED) TOTAL           
         ICM   RF,1,TOTVATD                                                     
         BZ    BLDTOT60                                                         
         AR    RF,R2                                                            
*&&UK                              VAT TOTAL GBR/GER ETC.                       
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BO    BLDTOT60            VAT TOTAL IS MEANINGLESS                     
         CURED VATAMT,(TOTMINW-1,(RF)),AGYCURT,MINUS=YES,ALIGN=LEFT             
*&&                                                                             
*&&US                              TRANSFERRED TOTAL USA/CAN                    
         CURED XFRTOT,(TOTMINW-1,(RF)),AGYCURT,MINUS=YES,ALIGN=LEFT             
*&&                                                                             
BLDTOT60 OC    TOTBALD,TOTBALD     DISPLAY BALANCE                              
         BZ    BLDTOTX                                                          
         MVC   TEMP(L'AGYCURT),AGYCURT                                          
         ZAP   DUB,CHQAMT                                                       
         SP    DUB,MRKTOT                                                       
         SP    DUB,DIFTOT                                                       
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    DUB,DISCAMT                                                      
*&&UK                                                                           
         AP    DUB,VATAMT                                                       
         TM    TWAMODE2,TWA2ALTO   TEST ALTERNATIVE TOTALS                      
         BZ    BLDTOT62                                                         
         ZAP   DUB,CHQAM2                                                       
         SP    DUB,MRKTO2                                                       
         SP    DUB,DIFTO2                                                       
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    DUB,DISCAM2                                                      
         MVC   TEMP(L'AGYCURT),FORCURT                                          
         CP    VATAMT,PZERO        SINGLE CURRENCY, CANNOT HAVE VAT             
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
BLDTOT62 SR    RF,RF                                                            
         IC    RF,TOTBALD                                                       
         AR    RF,R2                                                            
         CURED DUB,(TOTMINW-1,(RF)),TEMP,MINUS=YES,ALIGN=LEFT                   
*                                                                               
BLDTOTX  B     ROU2E                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PF KEY LINE ON A SCREEN                            *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF PF KEY LINE)                            *         
***********************************************************************         
         SPACE 1                                                                
BLDPFK   LR    R2,R1               R2=A(HEADER OF PF KEY TWA FIELD)             
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         MVC   L'FVIHDR(L'RECPFK,R2),TEMP                                       
         LA    RF,TEMP             RF=A(PFKEY BUILD AREA)                       
         LR    R3,RF               SAVE A(START OF STRING)                      
         L     R5,AAPFTAB                                                       
         TM    TWAMODE,TWAMALTP                                                 
         BZ    *+8                                                              
         L     R5,ASPFTAB                                                       
*                                                                               
         USING APFTABD,R5                                                       
BLDPFK02 TM    TWAMODE,TWAMALTP    TEST SCROLL OR ACTION PFKEYS                 
         BNZ   BLDPFK16                                                         
         SR    R0,R0                                                            
         ICM   R0,1,APFTPFK        R0=PFKEY NUMBER (ZERO=EOT)                   
         BZ    BLDPFK22                                                         
         CLC   APFTACT,ACTION      MATCH ON ACTION CODE                         
         BNE   BLDPFK12                                                         
*                                                                               
         L     R4,AACTTAB          ENSURE ACTION VALID AT THIS TIME             
         USING ACTTABD,R4                                                       
BLDPFK04 CLI   ACTTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTTNUM,APFTACT2    MATCH ON ACTION NUMBER                       
         BE    *+12                                                             
         LA    R4,ACTTABL(R4)                                                   
         B     BLDPFK04                                                         
*                                                                               
         CLI   ACTTNUM,ACTHEAD     TEST PF KEY FOR HEADER                       
         BNE   *+12                                                             
         TM    TWAMODE,TWAMHDRS    TEST HEADER ALREADY SAVED                    
         BNZ   BLDPFK12            HEADER IS NOT VALID                          
         TM    ACTTIND1,ACTISHDR   DOES ACTION REQUIRE SAVED HEADER             
         BZ    BLDPFK06                                                         
         TM    TWAMODE,TWAMHDRS    TEST SAVED HEADER AVAILABLE                  
         BZ    BLDPFK12                                                         
*                                                                               
BLDPFK06 OC    TWASAGN,TWASAGN     TEST NEW SECURITY IN USE                     
         BNZ   BLDPFK08                                                         
         TM    ACTTIND1,ACTIAUTH   TEST ACTION REQUIRES AUTHORISATION           
         BZ    BLDPFK08                                                         
         MVC   WORK(1),ACTTIND1    TEST USER AUTHORISATION                      
         NI    WORK,ACTIAUTH                                                    
         MVC   WORK+1(1),WORK                                                   
         NC    WORK(1),TWAAUTH                                                  
         CLC   WORK(1),WORK+1                                                   
         BNE   BLDPFK12                                                         
*                                                                               
BLDPFK08 TM    ACTTIND2,ACTISECY   TEST CALL SECRET                             
         BZ    BLDPFK10                                                         
         OC    TWASAGN,TWASAGN     TEST USING NEW SECURITY                      
         BZ    BLDPFK10                                                         
         LR    R0,RF               SAVE RF                                      
         GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',ACTTNUM)             
         LR    RF,R0               RESTORE RF                                   
         BNE   BLDPFK12                                                         
*                                                                               
BLDPFK10 TM    ACTTIND2,ACTISINC   TEST SINGLE CURRENCY ACTION                  
         BZ    *+12                                                             
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    BLDPFK12                                                         
         TM    ACTTIND1,ACTINALL   TEST ALLOCATION ACTION (SPECIAL)             
         BZ    BLDPFK14                                                         
         TM    TWAMODE2,TWA2NALL   TEST ALLOCATION ALLOWED                      
         BZ    BLDPFK14                                                         
*                                                                               
BLDPFK12 LA    R5,APFTABL(R5)      BUMP TO NEXT TABLE ENTRY                     
         B     BLDPFK02                                                         
*                                                                               
BLDPFK14 SR    R0,R0                                                            
         IC    R0,APFTPFK          R0=PFKEY NUMBER                              
         BAS   RE,SETPFK           SET PFNN=                                    
         MVC   LAREADDR,APFTADDM   SET SCON ADDRESS (MIXED CASE)                
         LA    R5,APFTABL(R5)      SET A(NEXT TABLE ENTRY)                      
         B     BLDPFK18                                                         
*                                                                               
         USING SPFTABD,R5                                                       
BLDPFK16 SR    R0,R0                                                            
         ICM   R0,1,SPFTPFK        R0=PFKEY NUMBER (ZERO=EOT)                   
         BZ    BLDPFK22                                                         
         BAS   RE,SETPFK           SET PFNN=                                    
         CLI   SPFTUPDN,0          SET DIRECTION IF DEFINED IN TABLE            
         BE    *+14                                                             
         MVC   0(L'SPFTUPDN,RF),SPFTUPDN                                        
         LA    RF,L'SPFTUPDN(RF)                                                
         MVC   LAREADDR,SPFTADDM   SET SCON ADDRESS (MIXED CASE)                
         LA    R5,SPFTABL(R5)      SET A(NEXT TABLE ENTRY)                      
*&&US                                                                           
BLDPFK18 DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
BLDPFK18 TM    ACTTIND2,ACTISINC                                                
         BZ    BLDPFK20                                                         
         TM    CURIND1,CUR1SINC                                                 
         BZ    BLDPFK20                                                         
         MVC   0(L'CURTCUR,RF),FORCURT+(CURTCUR-CURTABD)                        
         TM    TWAMODE2,TWA2ALTO                                                
         BZ    *+10                                                             
         MVC   0(L'CURTCUR,RF),AGYCURT+(CURTCUR-CURTABD)                        
         LA    RF,L'CURTCUR+1(RF)                                               
*&&                                                                             
BLDPFK20 EX    0,LARE              GET A(ACTION/SCROLL WORD)                    
         MVC   0(L'AC@PAGE,RF),0(RE)                                            
         LA    RF,L'AC@PAGE-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)            POINT ONE PAST END OF WORD                   
         B     BLDPFK02                                                         
*                                                                               
BLDPFK22 BAS   RE,SETPFK           SET ALTPF KEY IF ROOM                        
*                                                                               
BLDPFKX  B     ROU2E                                                            
         DROP  R5,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO BUILD PFKEY KEYWORD                                  *         
***********************************************************************         
         SPACE 1                                                                
SETPFK   LTR   R0,R0               TEST LAST TIME CALL                          
         BZ    SETPFK2                                                          
         CR    RF,R3               TEST "PF" DISPLAYED                          
         BNE   *+14                                                             
         MVC   0(2,RF),AC@PFK      NO - FORMAT PFNN= (ELSE NN=)                 
         LA    RF,2(RF)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,RF),DUB                                                      
         CLI   0(RF),C'0'          TEST PFKEY 1 THRU 9                          
         BNE   *+12                                                             
         MVC   0(2,RF),1(RF)       YES - SQUASH OUT ZERO                        
         BCTR  RF,0                                                             
         MVI   2(RF),C'='                                                       
         LA    RF,3(RF)                                                         
         BR    RE                                                               
SETPFK2  SR    RF,R3               SET PF13=ALTPFS IF ENOUGH ROOM               
         BZ    *+12                                                             
         CH    RF,=Y(L'RECPFK-L'AC@ALTPF-5)                                     
         BH    SETPFK4                                                          
         AR    RF,R3               POINT BACK TO OUTPUT AREA                    
         CR    RF,R3                                                            
         BNE   *+14                                                             
         MVC   0(2,RF),AC@PFK                                                   
         LA    RF,2(RF)                                                         
         MVC   0(2,RF),=C'13'      SET ALTPF KEY VALUE                          
         MVI   2(RF),C'='                                                       
         MVC   3(L'AC@ALTPF,RF),AC@ALTPF                                        
SETPFK4  OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         LA    RF,TEMP+L'RECPFK                                                 
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-12                                                          
         MVC   L'FVIHDR(L'RECPFK,R2),TEMP                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD SCREEN DISPLAY LINE                                           *         
*                                                                     *         
* NTRY - R1=A(TWA LINE HEADER)                                        *         
***********************************************************************         
         SPACE 1                                                                
BLDLIN   LR    R2,R1                                                            
         USING DISLINED,R2         R2=A(FIRST TWA LINE)                         
         XC    DISLLINE,DISLLINE                                                
         OI    DISLHDR1+(FVOIND-FVIHDR),FVOXMT                                  
         NI    DISLHDR1+(FVATRB-FVIHDR),255-FVAHIGH                             
         XC    DISLAMNT,DISLAMNT                                                
         OI    DISLHDR2+(FVOIND-FVIHDR),FVOXMT                                  
         NI    DISLHDR2+(FVATRB-FVIHDR),255-(FVAHIGH+FVAPROT)                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DISACTD        DISPLAY RECEIVABLE ACCOUNT                   
         BZ    *+14                                                             
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'RECVCACT-3,RF),RECVCACT+3                                    
*                                                                               
         ICM   RE,1,DISCACD        DISPLAY BILLING SOURCE                       
         BZ    *+14                                                             
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARCAC-3,RF),TSARCAC+3                                      
*                                                                               
         ICM   RE,1,DISREFD        DISPLAY BILL NUMBER                          
         BZ    *+14                                                             
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARREF,RF),TSARREF                                          
*                                                                               
         ICM   RE,1,DISDATD        DISPLAY BILL DATE                            
         BZ    BLDLIN02                                                         
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(17,(RF))                               
*                                                                               
BLDLIN02 SR    RE,RE                                                            
         ICM   RE,1,DISBLAD        DISPLAY BILL AMOUNT                          
         BZ    BLDLIN04                                                         
         LA    RF,DISLINED(RE)                                                  
         CURED TSARDR,(L'AC@BILAM,(RF)),AGYCURT,MINUS=YES                       
*                                                                               
BLDLIN04 SR    RE,RE                                                            
         ICM   RE,1,DISPRCD        DISPLAY PRIOR CREDITS                        
         BZ    BLDLIN06                                                         
         LA    RF,DISLINED(RE)                                                  
         CURED TSARCR,(L'AC@PRRCR,(RF)),AGYCURT,MINUS=YES                       
*                                                                               
BLDLIN06 SR    RE,RE                                                            
         ICM   RE,1,DISALCD        DISPLAY ALLOCATED THIS TIME                  
         BZ    BLDLIN08                                                         
         LA    RF,DISLINED(RE)                                                  
         CURED TSARPOST,(L'AC@ALCTD,(RF)),AGYCURT,MINUS=YES                     
*                                                                               
BLDLIN08 SR    RE,RE                                                            
         ICM   RE,1,DISDSCD        DISPLAY DISCOUNT                             
         BZ    BLDLIN10                                                         
         LA    RF,DISLINED(RE)                                                  
         CURED TSARDISC,(L'AC@DISSL,(RF)),AGYCURT,MINUS=YES                     
*                                                                               
BLDLIN10 SR    RE,RE                                                            
         ICM   RE,1,DISOFFD        DISPLAY OFFICE                               
         BZ    BLDLIN12                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSAROFFC,RF),TSAROFFC                                        
*                                                                               
BLDLIN12 SR    RE,RE                                                            
         ICM   RE,1,DISDUED        DISPLAY DUE DATE                             
         BZ    BLDLIN14                                                         
         MVC   WORK(L'TSARDUED),TSARDUED                                        
         OC    TSARDUE2,TSARDUE2   TEST NEW DUE DATE SET                        
         BZ    *+10                                                             
         MVC   WORK(L'TSARDUE2),TSARDUE2                                        
         OC    WORK(L'TSARDUED),WORK                                            
         BZ    BLDLIN14            NO DUE DATE                                  
         LA    RF,DISLINED(RE)                                                  
         OC    TSARDUE2,TSARDUE2   TEST NEW DUE DATE SET                        
         BZ    *+8                                                              
         MVI   0(RF),C'*'          INDICATE CHANGED THIS TIME                   
         LA    RF,1(RF)                                                         
         GOTO1 VDATCON,DMCB,(2,WORK),(17,(RF))                                  
*&&UK                                                                           
BLDLIN14 SR    RE,RE                                                            
         ICM   RE,1,DISVATD        DISPLAY VAT (ADJUSTMENT) AMOUNT              
         BZ    BLDLIN18                                                         
         LA    RF,DISLINED(RE)                                                  
         ZAP   DUB,PZERO                                                        
         OC    VAT,VAT             TEST VAT ACCOUNT PRESENT                     
         BZ    BLDLIN16                                                         
         TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BZ    BLDLIN16                                                         
         CLI   PROFWVAT,C'Y'       TEST IF THE PROFILE IS ON OR OFF             
         BNE   BLDLIN16                                                         
         SR    R0,R0                                                            
         ICM   R0,3,VATRATE        CALCULATE VAT AMOUNT                         
         CVD   R0,DUB                                                           
         AP    DUB,=P'10000'                                                    
         ZAP   WORK(16),TSARPOST                                                
         MP    WORK(16),=P'1000000'                                             
         DP    WORK(16),DUB                                                     
         SRP   WORK(16-L'DUB),64-2,5                                            
         ZAP   DUB,TSARPOST                                                     
         SP    DUB,WORK(16-L'DUB)                                               
BLDLIN16 CURED DUB,(L'AC@VATAM,(RF)),AGYCURT,MINUS=YES                          
*&&                                                                             
*&&US                                                                           
BLDLIN14 SR    RE,RE                                                            
         ICM   RE,1,DISGSTD        DISPLAY GST AMOUNT                           
         BZ    BLDLIN18                                                         
         LA    RF,DISLINED(RE)                                                  
         ZAP   DUB,TSARGST                                                      
         CURED DUB,(L'AC@GSTAM,(RF)),AGYCURT,MINUS=YES                          
*&&                                                                             
BLDLIN18 SR    R1,R1               DISPLAY ALLOCATION STATUS                    
         ICM   R1,1,DISSTAD                                                     
         BZ    BLDLIN20                                                         
         LA    R1,DISLINED(R1)     R1=A(STATUS DISPLAY AREA)                    
         GOTO1 ABLDSTA                                                          
*                                                                               
BLDLIN20 SR    R1,R1               DISPLAY TRANSFER ACCOUNT                     
         ICM   R1,1,DISXFRD                                                     
         BZ    BLDLIN22                                                         
         LA    R1,DISLINED(R1)                                                  
         MVC   0(L'TSARTRFA,R1),TSARTRFA                                        
*                                                                               
BLDLIN22 SR    RE,RE               DISPLAY NET AMOUNT                           
         ICM   RE,1,DISNETD                                                     
         BZ    BLDLIN24                                                         
         LA    RF,DISLINED(RE)                                                  
         ZAP   DUB,TSARDR                                                       
         SP    DUB,TSARCR                                                       
         AP    DUB,TSARDISC                                                     
         CURED DUB,(L'AC@NETAM,(RF)),AGYCURT,MINUS=YES                          
*&&US                                                                           
BLDLIN24 SR    RE,RE               DISPLAY PST AMOUNT                           
         ICM   RE,1,DISPSTD                                                     
         BZ    BLDLIN26                                                         
         LA    RF,DISLINED(RE)                                                  
         ZAP   DUB,TSARPST                                                      
         CURED DUB,(L'AC@PSTAM,(RF)),AGYCURT,MINUS=YES                          
*                                                                               
BLDLIN26 DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
BLDLIN24 OC    TSARCUR,TSARCUR     TEST CURRENCY KNOWN                          
         BZ    BLDLIN40            NO CURRENCY AMOUNTS                          
         MVC   WORK(L'FORCURT),FORCURT                                          
         OC    WORK(L'FORCURT),WORK                                             
         BNZ   BLDLIN26            SINGLE RESOLVED CURRENCY                     
         GOTO1 VBLDCUR,DMCB,(0,TSARCUR),(0,WORK),ACOM                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDLIN26 SR    RE,RE                                                            
         ICM   RE,1,DISDR2D        DISPLAY BILL AMOUNT (CURRENCY)               
         BZ    BLDLIN28                                                         
         LA    RF,DISLINED(RE)                                                  
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BO    BLDLIN27                                                         
         CURED TSARDR2,(L'AC@BILAM,(RF)),WORK,MINUS=YES                         
         B     BLDLIN28                                                         
BLDLIN27 CURED TSARDR2,(L'AC@BILAM,(RF)),WORK,MINUS=YES,CURSYMB=Y               
*                                                                               
BLDLIN28 SR    RE,RE                                                            
         ICM   RE,1,DISCR2D        DISPLAY PRIOR CREDITS (CURRENCY)             
         BZ    BLDLIN30                                                         
         LA    RF,DISLINED(RE)                                                  
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BO    BLDLIN29                                                         
         CURED TSARCR2,(L'AC@PRRCR,(RF)),WORK,MINUS=YES                         
         B     BLDLIN30                                                         
BLDLIN29 CURED TSARCR2,(L'AC@PRRCR,(RF)),WORK,MINUS=YES,CURSYMB=Y               
*                                                                               
BLDLIN30 SR    RE,RE                                                            
         ICM   RE,1,DISDI2D        DISPLAY DISCOUNT (CURRENCY)                  
         BZ    BLDLIN32                                                         
         LA    RF,DISLINED(RE)                                                  
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BO    BLDLIN31                                                         
         CURED TSARDI2,(L'AC@DISSL,(RF)),WORK,MINUS=YES                         
         B     BLDLIN32                                                         
BLDLIN31 CURED TSARDI2,(L'AC@DISSL,(RF)),WORK,MINUS=YES,CURSYMB=Y               
*                                                                               
BLDLIN32 SR    RE,RE               DISPLAY NET AMOUNT (CURRENCY)                
         ICM   RE,1,DISNE2D                                                     
         BZ    BLDLIN34                                                         
         LA    RF,DISLINED(RE)                                                  
         ZAP   DUB,TSARDR2                                                      
         SP    DUB,TSARCR2                                                      
         AP    DUB,TSARDI2                                                      
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BO    BLDLIN33                                                         
         CURED DUB,(L'AC@NETAM,(RF)),WORK,MINUS=YES                             
         B     BLDLIN34                                                         
BLDLIN33 CURED DUB,(L'AC@NETAM,(RF)),WORK,MINUS=YES,CURSYMB=Y                   
*                                                                               
BLDLIN34 SR    RE,RE               DISPLAY ALLOCATED (CURRENCY)                 
         ICM   RE,1,DISAL2D                                                     
         BZ    BLDLIN36                                                         
         LA    RF,DISLINED(RE)                                                  
         TM    CURIND1,CUR1ALLC    TEST SHOWING ALL CURRENCIES                  
         BO    BLDLIN35                                                         
         CURED TSARPO2,(L'AC@ALCTD,(RF)),WORK,MINUS=YES                         
         B     BLDLIN36                                                         
BLDLIN35 CURED TSARPO2,(L'AC@ALCTD,(RF)),WORK,MINUS=YES,CURSYMB=Y               
*                                                                               
BLDLIN36 DS    0H                  NEXT CURRENCY AMOUNT                         
*                                                                               
BLDLIN38 DS    0H                  NEXT CURRENCY AMOUNT                         
*                                                                               
BLDLIN40 OC    DISEXDD,DISEXDD     DISPLAY EXCHANGE DIFFERENCE                  
         BZ    BLDLIN42                                                         
         TM    CURIND1,CUR1SINC                                                 
         BZ    BLDLIN42                                                         
         ZAP   DUB,PZERO                                                        
         TM    TSARINDS,TSARIWOF+TSARITRF+TSARIOFS                              
         BNZ   BLDLIN41            ALLOCATED AT ORIGINAL RATE                   
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         MVC   AFCX,ATLX                                                        
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         LA    RF,TEMP+L'AFCX                                                   
         MVC   AFCX,TSARAFCX                                                    
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP TSARPO2,TEMP                                                     
         EXCHP TSARPO2,TEMP+L'AFCX,DUB=DUB2                                     
         SP    DUB,DUB2                                                         
BLDLIN41 SR    RE,RE                                                            
         IC    RE,DISEXDD                                                       
         LA    RF,DISLINED(RE)                                                  
         CURED DUB,(L'AC@DFRNC,(RF)),AGYCURT,MINUS=YES                          
*                                                                               
BLDLIN42 DS    0H                  NEXT NON-CURRENCY COLUMN                     
BLDLIN60 DS    0H                  ** AMOUNT FIELD **                           
         TM    CURIND1,CUR1SINC    SINGLE CURRENCY - SWAP AMOUNTS               
         BZ    BLDLIN62                                                         
         XC    TSARP6(TSARP6L),TSARP62                                          
         XC    TSARP62(TSARP6L),TSARP6                                          
         XC    TSARP6(TSARP6L),TSARP62                                          
*&&                                                                             
BLDLIN62 TM    TSARIND2,TSAR2QRD   TEST QUERIED                                 
         BZ    *+14                                                             
         MVC   DISLMARK,AC@QUERD                                                
         B     BLDLIN63                                                         
         TM    TSARIND2,TSAR2HLD   TEST HELD                                    
         BZ    *+14                                                             
         MVC   DISLMARK,AC@HELD                                                 
         B     BLDLIN63                                                         
*&&UK                                                                           
         TM    TSARIND2,TSAR2CLR   TEST CLEARING                                
         BZ    *+14                                                             
         MVC   DISLMARK,AC2CLRNG                                                
         B     BLDLIN63                                                         
*&&                                                                             
         MVI   DISLMARK,C' '       SET NO PART PAYMENTS                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARCR2,PZERO       TEST AGENCY CURRENCY TOO                     
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARCR,PZERO        ANY PRIOR CREDITS?                           
         BE    *+8                                                              
         MVI   DISLMARK,DISLMPPD   SET PART PAYMENTS                            
         B     BLDLIN64                                                         
*                                                                               
BLDLIN63 MVC   FVIFLD(1),DISLMARK                                               
         GOTO1 ATSTSEC             TEST SECURED ACTION AND PROTECT              
         BE    *+8                 IF USER NOT AUTHORISED                       
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
BLDLIN64 ZAP   DUB,TSARDR          TAKE ORIGINAL DEBITS                         
         TM    TSARINDS,TSARITRF                                                
         BZ    *+14                                                             
         AP    DUB,TSARCR                                                       
         B     *+10                                                             
         SP    DUB,TSARCR                                                       
         TM    TSARINDS,TSARIGRS   TEST GROSS ALLOCATION                        
         BNZ   *+10                                                             
         AP    DUB,TSARDISC        YES - SUBTRACT DISCOUNT (-VE AMOUNT)         
         SP    DUB,TSARPOST        SUBTRACT ANY THIS TIME ALLOCATION            
         LA    RF,AGYCURT          AGENCY DEFAULT                               
*&&UK                                                                           
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         LA    RF,FORCURT                                                       
*&&                                                                             
         CURED DUB,(L'DISLPOST,DISLPOST),(RF),ALIGN=LEFT,FLOAT=-                
*MN                                                                             
*&&US                                                                           
         TM    TSARIND2,TSARPRTC                                                
         BZ    *+8                                                              
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
*&&                                                                             
*MN                                                                             
         TM    TSARIND2,TSAR2QRD+TSAR2HLD+TSAR2CLR                              
         BNZ   BLDLIN66                                                         
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARPO2,PZERO       TEST AGENCY CURRENCY TOO                     
         BNE   *+14                                                             
*&&                                                                             
         CP    TSARPOST,PZERO      TEST THIS ITEM ALLOCATED                     
         BE    *+12                                                             
BLDLIN66 OI    DISLHDR1+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAHIGH                                 
*&&UK                                                                           
         TM    CURIND1,CUR1SINC    SINGLE CURRENCY - SWAP AMOUNTS BACK          
         BZ    BLDLIN68                                                         
         XC    TSARP6(TSARP6L),TSARP62                                          
         XC    TSARP62(TSARP6L),TSARP6                                          
         XC    TSARP6(TSARP6L),TSARP62                                          
*&&                                                                             
BLDLIN68 DS    0H                                                               
*                                                                               
BLDLINX  B     ROU2E                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TEST LINE ACTION SECURITY                                           *         
*                                                                     *         
* NTRY - FVIFLD(1) CONTAINS LINE ACTION                               *         
***********************************************************************         
         SPACE 1                                                                
         USING LACTABD,R2                                                       
TSTSEC   OC    TWASAGN,TWASAGN     TEST USING NEW SECURITY                      
         BZ    TSTSECY                                                          
         CLC   FVIFLD(1),AC@NO     'NO' MEANS NO ACTION                         
         BE    TSTSECY                                                          
         LA    R2,LACTAB           LINE ACTION TABLE                            
         CLC   FVIFLD(1),AC@YES    'YES' MEANS ALLOCATE                         
         BE    TSTSEC04                                                         
         CLI   FVIFLD,C'-'         NEGATIVE SIGN SHIFTED MEANS ALLOCATE         
         BE    TSTSEC04                                                         
         CLI   FVIFLD,C'1'         NUMERAL SHIFTED MEANS ALLOCATE               
         BH    TSTSEC04                                                         
         LA    R0,LACTABN-1        SKIP FIRST TABLE ENTRY (ALLOCATE)            
TSTSEC02 LA    R2,LACTABL(R2)                                                   
         SR    R1,R1                                                            
         ICM   R1,3,LACTDSP                                                     
         LA    R1,DSLISTU(R1)                                                   
         CLC   FVIFLD(1),0(R1)     ATTEMPT TO MATCH ACTION                      
         BE    TSTSEC04                                                         
         BCT   R0,TSTSEC02                                                      
         CLI   FVIFLD,X'80'        UNRESOLVED ACTION MEANS ALLOCATE             
         BNH   TSTSECY                                                          
         LA    R2,LACTAB           FIRST TABLE ENTRY IS ALLOCATE                
*                                                                               
TSTSEC04 GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',LACTNUM)             
         BNE   TSTSECN                                                          
*                                                                               
TSTSECY  B     ROU2E                                                            
*                                                                               
TSTSECN  B     ROU2H                                                            
         DROP  R2                                                               
         SPACE 1                                                                
LACTAB   DS    0X                  ** LINE ACTION TABLE (LACTABD) **            
         DC    AL2(0),AL1(ACTALOC)                                              
*&&UK*&& DC    AL2(AC2CLRNG-DSLISTU),AL1(ACTCLRNG)                              
         DC    AL2(AC@QUERD-DSLISTU),AL1(ACTQURY)                               
         DC    AL2(AC@HELD-DSLISTU),AL1(ACTHOLD)                                
         DC    AL2(AC@XFR-DSLISTU),AL1(ACTXFER)                                 
         DC    AL2(AC@DUEDT-DSLISTU),AL1(ACTDUED)                               
         DC    AL2(AC@OFFST-DSLISTU),AL1(ACTCTRA)                               
         DC    AL2(AC@RPLC-DSLISTU),AL1(ACTREPL)                                
         DC    AL2(AC@WRTF-DSLISTU),AL1(ACTWOFF)                                
LACTABN  EQU   (*-LACTAB)/LACTABL                                               
*                                                                               
LACTABD  DSECT                     ** LINE ACTION TABLE **                      
LACTDSP  DS    AL2                 DISPLACEMENT OF NAME INTO DSLISTU            
LACTNUM  DS    XL1                 LINE ACTION NUMBER FOR SECRET                
LACTABL  EQU   *-LACTABD                                                        
RECALL   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SECURE THE HEADER SCREEN                                            *         
***********************************************************************         
         SPACE 1                                                                
HDRSEC   NI    TWAMODE2,FF-(TWA2NCUR)                                           
         OC    TWASAGN,TWASAGN     TEST USING NEW SECURITY                      
         BZ    ROU2X                                                            
         SR    R2,R2                                                            
         MVI   BYTE,ACTWOFF                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',BYTE)                
         BE    HDRSEC02                                                         
         ICM   R2,3,SRECWACT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWAC                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECVATT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECVAT                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWDTT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWDT                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWRFT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWRF                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWNRT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECWNR                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
*                                                                               
HDRSEC02 MVI   BYTE,ACTCTRA                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',SECBLK),('RECDEBT',BYTE)                
         BE    HDRSEC04                                                         
         ICM   R2,3,SRECODTT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECODT                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
*                                                                               
HDRSEC04 MVI   BYTE,FLDDSCA                                                     
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECBLK),BYTE                            
         BE    HDRSEC06                                                         
         ICM   R2,3,SRECDSCT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECDSC                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECDAMT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECDAM                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
*                                                                               
HDRSEC06 MVI   BYTE,FLDVWOF                                                     
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECBLK),BYTE                            
         BE    HDRSEC08                                                         
         ICM   R2,3,SRECVATT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECVAT                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
*                                                                               
HDRSEC08 MVI   BYTE,FLDCODT                                                     
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECBLK),BYTE                            
         BE    HDRSEC10                                                         
         ICM   R2,3,SRECODTT                                                    
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
         ICM   R2,3,SRECODT                                                     
         BZ    *+8                                                              
         BAS   RE,HDRSECLR                                                      
*                                                                               
HDRSEC10 MVI   BYTE,FLDCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPFLDP',SECBLK),BYTE                            
         BE    HDRSEC12                                                         
         OI    TWAMODE2,TWA2NCUR   CURRENCY IS NOT VALID FOR USER               
         OC    SRECCUCT,SRECCUCT   TEST S(CURRENCY FIELDS) KNOWN                
         BZ    HDRSEC12                                                         
         LA    R3,SRECCF           CURRENCY FIELDS                              
         LA    R4,SRECCFN                                                       
         ICM   R2,3,0(R3)                                                       
         BAS   RE,HDRSECLR                                                      
         LA    R3,L'SRECCF(R3)                                                  
         BCT   R4,*-12                                                          
*                                                                               
HDRSEC12 DS    0H                                                               
         B     ROU2X                                                            
*                                                                               
HDRSECLR LA    R1,RECOLAYH(R2)     CLEAR/PROTECT/TRANSMIT FIELD                 
         SR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR+1)                                                
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
         EX    RF,*-6                                                           
         OI    FVATRB-FVIHDR(R1),FVAPROT                                        
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CTRYTAB  DS    0X                  ** COUNTRY TABLE (CTRYTABD) **               
*                                                                               
         DC    AL1(CTRYGBR,0),X'F101'                                           
         DC    C'GBP'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYGBR,1),X'E111'                                           
         DC    C'GBP'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYIRE,0),X'F101'                                           
         DC    C'IEP'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYIRE,1),X'E111'                                           
         DC    C'IEP'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYUSA,0),X'F202'                                           
         DC    C'USD'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYGER,0),X'F303'                                           
         DC    C'DEM'                                                           
         DC    AL1(4,3,2,1,5,6),AL1(4,3,5,6,7,0,0)                              
         DC    AL1(1,4,3,2,5,6),AL1(1,4,3,5,6,7,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYGER,1),X'E313'                                           
         DC    C'DEM'                                                           
         DC    AL1(4,3,2,1,5,6),AL1(4,3,5,6,7,0,0)                              
         DC    AL1(1,4,3,2,5,6),AL1(1,4,3,5,6,7,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYCAN,0),X'F404'                                           
         DC    C'CAD'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYHOL,0),X'F505'                                           
         DC    C'NLG'                                                           
         DC    AL1(4,3,2,1,5,6),AL1(4,3,5,6,7,0,0)                              
         DC    AL1(1,4,3,2,5,6),AL1(1,4,3,5,6,7,0)                              
         DC    AL1(255)                                                         
*                                                                               
         DC    AL1(CTRYHOL,1),X'F101'                                           
         DC    C'NLG'                                                           
         DC    AL1(1,2,3,4,5,6),AL1(2,3,4,5,6,0,0)                              
         DC    AL1(1,2,3,4,5,6),AL1(1,2,3,4,5,0,0)                              
         DC    AL1(255)                                                         
*                                                                               
CTRYTABX DC    AL1(EOT)                                                         
         EJECT                                                                  
KEYTAB   DS    0H                  ** KEY BUILD TABLE (KEYTABD) **              
         DC    X'80',AL1(L'RECVNDX)                                             
         DC    X'40',AL1(L'TSARCAC)                                             
         DC    X'20',AL1(L'TRNDATE)                                             
         DC    X'10',AL1(L'TRNREF)                                              
         DC    X'08',AL1(L'TRNOFFC)                                             
         DC    X'04',AL1(L'DUEDATE)                                             
KEYTABN  EQU   (*-KEYTAB)/KEYTABL                                               
         SPACE 1                                                                
TOTTAB   DS    0X                  ** TOTALS SEQUENCE (TOTTABD) **              
         DC    AL1(PROFTOFS,TOTOFSD-TOTDISP)                                    
         DC    AL1(PROFTWOF,TOTWOFD-TOTDISP)                                    
         DC    AL1(PROFTDSC,TOTDSCD-TOTDISP)                                    
         DC    AL1(PROFTCHQ,TOTCHQD-TOTDISP)                                    
         DC    AL1(PROFTDIF,TOTDIFD-TOTDISP)                                    
         DC    AL1(PROFTVAT,TOTVATD-TOTDISP)                                    
         DC    AL1(PROFTMRK,TOTMRKD-TOTDISP)                                    
         DC    AL1(PROFTBAL,TOTBALD-TOTDISP)                                    
TOTTABN  EQU   (*-TOTTAB)/TOTTABL                                               
         EJECT                                                                  
DISTAB   DS    0H                  ** DISPLAY TABLE (DISTABD) **                
*                                                                               
         DC    X'8000000000'                                                    
         DC    AL1(L'AC@ACC,0,0)                                                
         DC    S(AC@ACC,0)                                                      
*                                                                               
         DC    X'4000000000'                                                    
         DC    AL1(L'AC@SRC,0,0)                                                
         DC    S(AC@SRC,0)                                                      
*                                                                               
         DC    X'2000000000'                                                    
         DC    AL1(L'AC@DATE,0,0)                                               
         DC    S(AC@DATE,0)                                                     
*                                                                               
         DC    X'1000000000'                                                    
         DC    AL1(L'AC@BIL,0,0)                                                
         DC    S(AC@BIL,0)                                                      
*                                                                               
         DC    X'0800000000'                                                    
         DC    AL1(L'AC@BILAM,0,0)                                              
         DC    S(AC@BILAM,0)                                                    
*                                                                               
         DC    X'0400000000'                                                    
         DC    AL1(L'AC@PRRCR,0,0)                                              
         DC    S(AC@PRRCR,0)                                                    
*                                                                               
         DC    X'0200000000'                                                    
         DC    AL1(L'AC@ALCTD,0,0)                                              
         DC    S(AC@ALCTD,0)                                                    
*                                                                               
         DC    X'0100000000'                                                    
         DC    AL1(L'AC@DISSL,0,0)                                              
         DC    S(AC@DISSL,0)                                                    
*                                                                               
         DC    X'0080000000'                                                    
         DC    AL1(L'AC@OFFL,0,0)                                               
         DC    S(AC@OFFL,0)                                                     
*                                                                               
         DC    X'0040000000'                                                    
         DC    AL1(L'AC@DUEDT,0,0)                                              
         DC    S(AC@DUEDT,0)                                                    
*&&UK                                                                           
         DC    X'0020000000'                                                    
         DC    AL1(L'AC@VATAM,0,0)                                              
         DC    S(AC@VATAM,0)                                                    
*&&                                                                             
*&&US                                                                           
         DC    X'0020000000'                                                    
         DC    AL1(L'AC@GSTAM,0,0)                                              
         DC    S(AC@GSTAM,0)                                                    
*&&                                                                             
         DC    X'0010000000'                                                    
         DC    AL1(L'AC@STT,0,0)                                                
         DC    S(AC@STT,0)                                                      
*                                                                               
         DC    X'0008000000'                                                    
         DC    AL1(L'AC@XFRAC,0,0)                                              
         DC    S(AC@XFRAC,0)                                                    
*                                                                               
         DC    X'0004000000'                                                    
         DC    AL1(L'AC@NETAM,0,0)                                              
         DC    S(AC@NETAM,0)                                                    
*&&US                                                                           
         DC    X'0002000000'                                                    
         DC    AL1(L'AC@PSTAM,0,0)                                              
         DC    S(AC@PSTAM,0)                                                    
*&&                                                                             
*&&UK                                                                           
         DC    X'0002000000'                                                    
         DC    AL1(L'AC@BILAM,0,DISTICUR)                                       
         DC    S(AC@BILAM,AC@CURRY)                                             
*                                                                               
         DC    X'0001000000'                                                    
         DC    AL1(L'AC@PRRCR,0,DISTICUR)                                       
         DC    S(AC@PRRCR,AC@CURRY)                                             
*                                                                               
         DC    X'0000800000'                                                    
         DC    AL1(L'AC@DISSL,0,DISTICUR)                                       
         DC    S(AC@DISSL,AC@CURRY)                                             
*                                                                               
         DC    X'0000400000'                                                    
         DC    AL1(L'AC@NETAM,0,DISTICUR)                                       
         DC    S(AC@NETAM,AC@CURRY)                                             
*                                                                               
         DC    X'0000200000'                                                    
         DC    AL1(L'AC@ALCTD,0,DISTICUR)                                       
         DC    S(AC@ALCTD,AC@CURRY)                                             
*                                                                               
         DC    X'0000100000'                                                    
         DC    AL1(L'AC@DFRNC,0,0)                                              
         DC    S(AC@DFRNC,0)                                                    
*&&                                                                             
DISTABN  EQU   (*-DISTAB)/DISTABL                                               
         EJECT                                                                  
DCLISTU  DS    0X                  ** UPPER CASE DICTIONARY **                  
         DCDDL AC#HEADR,8                                                       
         DCDDL AC#HEADR,3                                                       
*&&UK*&& DCDDL AC#INP,8                                                         
*&&US*&& DCDDL AC#MARK,8                                                        
*&&UK*&& DCDDL AC#INP,3                                                         
*&&US*&& DCDDL AC#MARK,3                                                        
         DCDDL AC#SPCL,8                                                        
         DCDDL AC#SPCL,3                                                        
         DCDDL AC#RSR,8                                                         
         DCDDL AC#RSR,3                                                         
         DCDDL AC#QUIT,8                                                        
         DCDDL AC#QUIT,3                                                        
         DCDDL AC#UPD,8                                                         
         DCDDL AC#UPD,3                                                         
         DCDDL AC#DRAFT,8                                                       
         DCDDL AC#DRAFT,3                                                       
         DCDDL AC#FLT,8                                                         
         DCDDL AC#FLT,3                                                         
         DCDDL AC#PAGE,8                                                        
         DCDDL AC#HALF,8                                                        
         DCDDL AC#FIRST,8                                                       
         DCDDL AC#LAST,8                                                        
*&&UK*&& DCDDL AC#SWTCH,8                                                       
*&&US*&& DCDDL AC#RVRS,8                                                        
*&&UK*&& DCDDL AC#SWTCH,3                                                       
*&&US*&& DCDDL AC#RVRS,3                                                        
         DCDDL AC#MARKD,8                                                       
         DCDDL AC#MARKD,3                                                       
         DCDDL AC#BAL,8                                                         
         DCDDL AC#BAL,3                                                         
         DCDDL AC#DATE,8                                                        
         DCDDL AC#DATE,3                                                        
         DCDDL AC#BILC,8                                                        
         DCDDL AC#BILC,3                                                        
         DCDDL AC#ACC,8                                                         
         DCDDL AC#ACC,3                                                         
         DCDDL AC#SRC,8                                                         
         DCDDL AC#SRC,3                                                         
         DCDDL AC#OFF,8                                                         
         DCDDL AC#OFF,3                                                         
         DCDDL AC#DSP,8                                                         
         DCDDL AC#DSP,3                                                         
         DCDDL AC#YES,3                                                         
         DCDDL AC#NO,3                                                          
         DCDDL AC#UALCX,3                                                       
         DCDDL AC#WRTF,8                                                        
         DCDDL AC#DISS,8                                                        
         DCDDL AC#AMT,8                                                         
         DCDDL AC#PRINT,8                                                       
         DCDDL AC#CHKC,13                                                       
         DCDDL AC#DATED,5                                                       
         DCDDL AC#DPSON,12                                                      
         DCDDL AC#OFFST,8                                                       
         DCDDL AC#NET,3                                                         
*&&UK*&& DCDDL AC#GROSS,3                                                       
*&&US*&& DCDDL AC#DEDT,3                                                        
         DCDDL AC#ALCTD,3                                                       
         DCDDL AC#RPLC,3                                                        
         DCDDL AC#ALTPF,8                                                       
         DCDDL AC#XFR,8                                                         
         DCDDL AC#SRC,8                                                         
         DCDDL AC#LATE,3                                                        
         DCDDL AC#QUERD,8                                                       
         DCDDL AC#HELD2,8                                                       
         DCDDL AC#MEMO,3                                                        
*        DCDDL AC#DUEDT,3                                                       
         DCDDL AC#X,3                                                           
         DCDDL AC#CURRY,8                                                       
         DCDDL AC#LEFT,8                                                        
         DCDDL AC#RIGHT,8                                                       
         DCDDL AC#CLRNG,8                                                       
         DCDDL AC#CLRNG,3                                                       
         DCDDL AC#CLI,6                                                         
DCLISTUX DC    AL1(EOT)                                                         
         SPACE 1                                                                
DCLISTL  DS    0X                  ** LOWER CASE DICTIONARY **                  
         DCDDL AC#ACC,12                                                        
         DCDDL AC#SRC,12                                                        
         DCDDL AC#DATE,8                                                        
         DCDDL AC#BIL,6                                                         
         DCDDL AC#BILAM,11,R                                                    
         DCDDL AC#PRRCR,11,R                                                    
         DCDDL AC#ALCTD,11,R                                                    
         DCDDL AC#DSF,11,R                                                      
         DCDDL AC#OFF,3                                                         
         DCDDL AC#DUEDT,9                                                       
*&&UK*&& DCDDL AC#VATAM,11,R                                                    
*&&US*&& DCDDL AC#GSTAM,11,R                                                    
*&&US*&& DCDDL AC#PSTAM,11,R                                                    
         DCDDL AC#STT,4                                                         
         DCDDL AC#XFRAC,12                                                      
         DCDDL AC#PFK,3                                                         
         DCDDL AC#ALTPF,6                                                       
         DCDDL AC#HEADR,8                                                       
*&&UK*&& DCDDL AC#INP,8                                                         
*&&US*&& DCDDL AC#MARK,8                                                        
         DCDDL AC#SPCL,8                                                        
         DCDDL AC#RSR,8                                                         
         DCDDL AC#QUIT,8                                                        
         DCDDL AC#UPD,8                                                         
         DCDDL AC#DRAFT,8                                                       
         DCDDL AC#FLT,8                                                         
         DCDDL AC#PAGE,8                                                        
         DCDDL AC#HALF,8                                                        
         DCDDL AC#FIRST,8                                                       
         DCDDL AC#LAST,8                                                        
*&&UK*&& DCDDL AC#NETA1,11,R                                                    
*&&US*&& DCDDL AC#AMTCD,11,R                                                    
         DCDDL AC#SRC,8                                                         
         DCDDL AC#PGDEB,11                                                      
         DCDDL AC#CURRY,11,R                                                    
         DCDDL AC#AMT,8,LU                                                      
         DCDDL AC#DFRNC,11,R                                                    
         DCDDL AC#TOTLS,8                                                       
         DCDDL AC#LEFT,8                                                        
         DCDDL AC#RIGHT,8                                                       
DCLISTLX DC    AL1(EOT)                                                         
         EJECT                                                                  
APFTAB   DS    0H                  ** ACTION PFK TABLE (APFTABD) **             
         DC    AL1(PFK01,0,ACTINIT,ACTHEAD),S(AC8HEADR,AC@HEADR)                
         DC    AL1(PFK14,0,ACTINIT,ACTBSRC),S(AC8SRCE,AC@SRCE)                  
         DC    AL1(PFK01,0,ACTHEAD,ACTINPT),S(AC8INP,AC@INP)                    
         DC    AL1(PFK02,0,ACTHEAD,ACTOVER),S(AC8SPCL,AC@SPCL)                  
         DC    AL1(PFK03,0,ACTHEAD,ACTQUIT),S(AC8QUIT,AC@QUIT)                  
         DC    AL1(PFK04,0,ACTHEAD,ACTDRFT),S(AC8DRAFT,AC@DRAFT)                
         DC    AL1(PFK05,0,ACTHEAD,ACTFILT),S(AC8FLT,AC@FLT)                    
         DC    AL1(PFK06,0,ACTHEAD,ACTUPDT),S(AC8UPD,AC@UPD)                    
         DC    AL1(PFK14,0,ACTHEAD,ACTBSRC),S(AC8SRCE,AC@SRCE)                  
         DC    AL1(PFK01,0,ACTINPT,ACTREST),S(AC8RSR,AC@RSR)                    
         DC    AL1(PFK02,0,ACTINPT,ACTOVER),S(AC8SPCL,AC@SPCL)                  
         DC    AL1(PFK03,0,ACTINPT,ACTQUIT),S(AC8QUIT,AC@QUIT)                  
         DC    AL1(PFK04,0,ACTINPT,ACTDRFT),S(AC8DRAFT,AC@DRAFT)                
         DC    AL1(PFK05,0,ACTINPT,ACTFILT),S(AC8FLT,AC@FLT)                    
         DC    AL1(PFK06,0,ACTINPT,ACTUPDT),S(AC8UPD,AC@UPD)                    
         DC    AL1(PFK14,0,ACTINPT,ACTBSRC),S(AC8SRCE,AC@SRCE)                  
         DC    AL1(PFK15,0,ACTINPT,ACTALTO),S(0,AC@TOTLS)                       
         DC    AL1(PFK01,0,ACTOVER,ACTREST),S(AC8RSR,AC@RSR)                    
         DC    AL1(PFK02,0,ACTOVER,ACTINPT),S(AC8INP,AC@INP)                    
         DC    AL1(PFK03,0,ACTOVER,ACTQUIT),S(AC8QUIT,AC@QUIT)                  
         DC    AL1(PFK04,0,ACTOVER,ACTDRFT),S(AC8DRAFT,AC@DRAFT)                
         DC    AL1(PFK05,0,ACTOVER,ACTFILT),S(AC8FLT,AC@FLT)                    
         DC    AL1(PFK06,0,ACTOVER,ACTUPDT),S(AC8UPD,AC@UPD)                    
         DC    AL1(PFK14,0,ACTOVER,ACTBSRC),S(AC8SRCE,AC@SRCE)                  
         DC    AL1(PFK15,0,ACTOVER,ACTALTO),S(0,AC@TOTLS)                       
         DC    AL1(PFK01,0,ACTREST,ACTINPT),S(AC8INP,AC@INP)                    
         DC    AL1(PFK02,0,ACTREST,ACTOVER),S(AC8SPCL,AC@SPCL)                  
         DC    AL1(PFK03,0,ACTREST,ACTQUIT),S(AC8QUIT,AC@QUIT)                  
         DC    AL1(PFK04,0,ACTREST,ACTDRFT),S(AC8DRAFT,AC@DRAFT)                
         DC    AL1(PFK05,0,ACTREST,ACTFILT),S(AC8FLT,AC@FLT)                    
         DC    AL1(PFK06,0,ACTREST,ACTUPDT),S(AC8UPD,AC@UPD)                    
         DC    AL1(PFK14,0,ACTREST,ACTBSRC),S(AC8SRCE,AC@SRCE)                  
APFTABX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
SPFTAB   DS    0H                  ** SCROLL PFK TABLE (SPFTABD) **             
         DC    AL1(PFK07,SCRUP,0,0),S(SCR@HALF,AC@HALF)                         
         DC    AL1(PFK08,SCRUP,0,0),S(SCR@PAGE,AC@PAGE)                         
         DC    AL1(PFK09,0,0,0),S(SCR@FRST,AC@FRST)                             
         DC    AL1(PFK10,SCRDN,0,0),S(SCR@HALF,AC@HALF)                         
         DC    AL1(PFK11,SCRDN,0,0),S(SCR@PAGE,AC@PAGE)                         
         DC    AL1(PFK12,0,0,0),S(SCR@LAST,AC@LAST)                             
         DC    AL1(PFK16,0,0,0),S(SCR@LEFT,AC@LEFT)                             
         DC    AL1(PFK17,0,0,0),S(SCR@RGHT,AC@RGHT)                             
SPFTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
ACTTAB   DS    0H                  ** ACTION TABLE (ACTTABD) **                 
         DC    AL1(ACTHEAD,0)                                                   
         DC    AL1(0,0)                                                         
         DC    S(AC8HEADR,AC3HEADR)                                             
*                                                                               
         DC    AL1(ACTINPT,ACTISHDR)                                            
         DC    AL1(0,0)                                                         
         DC    S(AC8INP,AC3INP)                                                 
*                                                                               
         DC    AL1(ACTOVER,ACTINALL+ACTISHDR)                                   
         DC    AL1(ACTISECY,0)                                                  
         DC    S(AC8SPCL,AC3SPCL)                                               
*                                                                               
         DC    AL1(ACTREST,ACTISHDR)                                            
         DC    AL1(0,0)                                                         
         DC    S(AC8RSR,AC3RSR)                                                 
*                                                                               
         DC    AL1(ACTQUIT,ACTIXSET+ACTISHDR)                                   
         DC    AL1(0,0)                                                         
         DC    S(AC8QUIT,AC3QUIT)                                               
*                                                                               
         DC    AL1(ACTUPDT,ACTIXSET+ACTISHDR)                                   
         DC    AL1(ACTISECY,0)                                                  
         DC    S(AC8UPD,AC3UPD)                                                 
*                                                                               
         DC    AL1(ACTDRFT,ACTIXSET+ACTISHDR)                                   
         DC    AL1(0,0)                                                         
         DC    S(AC8DRAFT,AC3DRAFT)                                             
*                                                                               
         DC    AL1(ACTFILT,ACTIXSET+ACTISHDR)                                   
         DC    AL1(0,0)                                                         
         DC    S(AC8FLT,AC3FLT)                                                 
*                                                                               
         DC    AL1(ACTAPFK,0)                                                   
         DC    AL1(0,0)                                                         
         DC    S(AC8ALTPF,AC8ALTPF)                                             
*                                                                               
         DC    AL1(ACTBSRC,ACTIAUT4)                                            
         DC    AL1(ACTISECY,0)                                                  
         DC    S(AC8SRCE,AC8SRCE)                                               
*                                                                               
         DC    AL1(ACTALTO,0)                                                   
         DC    AL1(ACTISINC,0)                                                  
         DC    S(0,0)                                                           
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
SCRTAB   DS    0H                  ** SCROLL TABLE (SCRTABD) **                 
         DC    S(SCR@PAGE),AL2(SCRPAGE),AL1(0,0)                                
         DC    S(SCR@HALF),AL2(SCRHALF),AL1(0,0)                                
         DC    S(SCR@FRST),AL2(SCRMAXI),AL1(SCRUP,0)                            
         DC    S(SCR@LAST),AL2(SCRMAXI),AL1(SCRDN,0)                            
         DC    S(SCR@LEFT),AL2(SCRLEFT),AL1(0,SCRTIHRZ)                         
         DC    S(SCR@RGHT),AL2(SCRRGHT),AL1(0,SCRTIHRZ)                         
SCRTABX  DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACRECWRK                                                       
         SPACE 1                                                                
* FAFACTS                                                                       
* BMONVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
*                                                                               
SAVEVALS DSECT                     ** SAVE CONTRA/DATE/REF **                   
SAVECAC  DS    CL(L'TRNKCULC)      CONTRA-ACCOUNT                               
SAVEDAT  DS    XL(L'TRNKDATE)      DATE                                         
SAVEREF  DS    CL(L'TRNKREF)       BILL NUMBER/REFERENCE                        
SAVEVALL EQU   *-SAVEVALS                                                       
SAVETBLE DS    CL1                 SAVE TABLE ENTRY                             
SAVETBLY EQU   C'Y'                SAVE TABLE ENTRY - YES                       
SAVETBLN EQU   C'N'                SAVE TABLE ENTRY - NO                        
*                                                                               
TBLENTRY DS    CL(TBLEMAXQ*TABLVALL)  SPACE FOR 76 NUMBER OF ENTRIES            
*                                                                               
WORKDR   DS    PL6                 DEBIT AMOUNT                                 
WORKCR   DS    PL6                 CREDIT AMOUNT                                
WORKDISC DS    PL6                 DISCOUNT (OR SURCHARGE) AMOUNT               
WORKPOST DS    PL6                 POSTING AMOUNT                               
WORKGSTB DS    PL6                 GST BASIS                                    
WORKGST  DS    PL6                 GST AMOUNT                                   
WORKPSTB DS    PL6                 PST BASIS                                    
WORKPST  DS    PL6                 PST AMOUNT                                   
*                                                                               
TABLVALD DSECT ,                   TABLE TO SAVE PROCESSED REF NUMBER           
TABLVALS DS    0C                  ** SAVE CONTRA/DATE/REF IN TABLE **          
TABLCAC  DS    CL(L'TRNKCULC)      CONTRA-ACCOUNT                               
TABLDAT  DS    XL(L'TRNKDATE)      DATE                                         
TABLREF  DS    CL(L'TRNKREF)       BILL NUMBER/REFERENCE                        
TABLVALL EQU   *-TABLVALD                                                       
TBLEMAXQ EQU   76                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACREC00   02/25/20'                                      
         END                                                                    
