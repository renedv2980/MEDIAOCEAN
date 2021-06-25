*          DATA SET GERLP00    AT LEVEL 041 AS OF 09/29/20                      
*PHASE TF2D00B                                                                  
*INCLUDE REQRFP                                                                 
RLP00    TITLE '- REQUEST FILE PROGRAM ROOT'                                    
RLP00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**RLP0**,CLEAR=YES,RR=RE                                   
                                                                                
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
                                                                                
         ST    RB,PCBASER1                                                      
         ST    RE,PCRELO                                                        
         ST    RD,PCSVRD                                                        
                                                                                
         MVI   PCOVSYS,X'0F'                                                    
         MVI   PCPRGNO,X'2D'                                                    
                                                                                
         MVC   AINP,00(R1)         A(TIOB)                                      
         MVC   ATWA,04(R1)         A(TWA)                                       
         MVC   ASYS,08(R1)         A(SYSTEM FACILITY LIST)                      
         MVC   ATIA,12(R1)         A(TIA)                                       
         MVC   ACOM,16(R1)         A(COMMON FACILITY LIST)                      
         L     R1,20(R1)           R1=A(FACPAK EXTRA INFORMATION)               
         USING XTRAINFD,R1                                                      
         MVC   CUCTRY,XIAGCTRY     SET AGENCY COUNTRY                           
         MVC   CUCTRY2,XIAGCTRY                                                 
         MVC   CULANG,XILANG                                                    
         MVC   ASXFLAG1,XIFLAG1                                                 
                                                                                
***********************************************************************         
* INITIALIZE GLOBAL ADDRESSES                                         *         
***********************************************************************         
                                                                                
INITAD   L     R1,ACOM                                                          
         USING COMFACSD,R1                                                      
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VCOLY,CCALLOV                                                    
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VMINIO,CMINIO                                                    
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VDICTAT,CDICTATE                                                 
         MVC   VGETPROF,CGETPROF                                                
         MVC   VSECRET,CSECRET                                                  
         MVC   VGETHELP,CGETHELP                                                
         MVC   VPROTON,CPROTON                                                  
         MVC   VPROTOFF,CPROTOFF                                                
         MVC   VSOFDAT,CSOFDAT                                                  
         MVC   VGETRET,CGETRET                                                  
         MVC   VPERVERT,CPERVERT                                                
                                                                                
         BASR  R2,0                                                             
         AHI   R2,PHASES-*                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,PCPARM                                                        
         L     RF,VCOLY                                                         
INITAD02 CLI   0(R2),FF                                                         
         BE    INITAD04                                                         
         ICM   R0,1,0(R2)                                                       
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,1                                                             
         AHI   R3,4                                                             
         B     INITAD02                                                         
                                                                                
INITAD04 LHI   R0,ADDRS#           SET CONTROLLER ADDRESSES 1                   
         SR    RE,RE               RE=INDEX TO ADDRESS VALUE                    
         BASR  R2,0                                                             
         AHI   R2,ADDRS-*                                                       
         BASR  RF,0                                                             
         L     R1,0(R2,RE)                                                      
         A     R1,PCBASER1         RELOCATE AND STORE IN W/S                    
         ST    R1,AADDRS(RE)                                                    
         AHI   RE,L'ADDRS          BUMP ADDRESS INDEX                           
         BCTR  R0,RF                                                            
                                                                                
         L     RF,=V(REQRFP)       RELOCATE REQRFP                              
         A     RF,PCRELO                                                        
         ST    RF,VREQRFP                                                       
                                                                                
         LHI   R0,AROUTSN          SET ROUTINE ADDRESSES - 1                    
         SR    RE,RE                                                            
         LA    R1,AROUTS                                                        
         BASR  RF,0                                                             
         MVC   0(4,R1),AROUT       SET A(ROUTS)                                 
         STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         AHI   RE,1                                                             
         AHI   R1,4                                                             
         BCTR  R0,RF                                                            
                                                                                
INITAD06 BASR  R2,0                SET SUNDRY STORAGE ADDRESSES                 
         AHI   R2,ANAWS-*                                                       
         LHI   R0,ANAWSN                                                        
         BASR  R1,0                                                             
         ICM   RE,15,0(R2)         RE=AL2(AREA),AL2(ADDRESS)                    
         SRDL  RE,16                                                            
         SRL   RF,16                                                            
         LA    RE,WORKD(RE)                                                     
         LA    RF,WORKD(RF)                                                     
         STCM  RE,15,0(RF)         SET AREA ADDRESS                             
         AHI   R2,L'ANAWS                                                       
         BCTR  R0,R1                                                            
                                                                                
         LHI   RE,SECBLK-TWAD      SET A(SECURITY ACCESS BLOCK)                 
         A     RE,ATWA                                                          
         ST    RE,ASECBLK                                                       
         EJECT                                                                  
***********************************************************************         
* INITIALISE GENERAL VALUES                                           *         
***********************************************************************         
                                                                                
INITGV   L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         MVI   PCEFFS,FF                                                        
         MVC   PCEFFS+1(L'PCEFFS-1),PCEFFS                                      
         MVI   PCSPACES,SPACE                                                   
         MVC   PCSPACES+1(L'PCSPACES-1),PCSPACES                                
         MVC   CUACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   CUUSER,TWAUSRID     USER-ID NUMBER                               
         MVC   CUAUTH,TWAAUTH      AUTHORIZATION CODE                           
         MVC   CUAALF,TWAAGY       AGENCY ALPHA-ID                              
         MVC   CUUALF,TWAAGY       AGENCY ALPHA-ID (ACTUAL)                     
         GOTOR VGETFACT,PCPARM,0                                                
         L     R2,0(R1)                                                         
         USING FACTSD,R2           R2=A(SYSTEM DEFINITION BLOCK)                
         MVC   ASYSLST,FASYSLST                                                 
         MVC   CUTSYM,FASYM                                                     
         MVC   ASSDAT,FADATE       SYSTEM DATES (VARIOUS FORMATS)               
         MVC   ASBDAT,FADATEB                                                   
         CLI   TWAOFFC,C'*'        TEST DDS OFFICE CODE                         
         BNE   *+8                                                              
         MVI   CUSTAT,CUSDDS       SET DDS STATUS INDICATOR                     
                                                                                
         OC    CUPASS,FAPASSWD     TEST CONNECTED WITH PASSWORD                 
         BZ    INITGV02                                                         
         TM    FATFLAG,X'08'       YES - TEST PERSONAL PASSWORD                 
         BZ    *+8                                                              
         OI    CUSTAT,CUSPER                                                    
                                                                                
INITGV02 MVC   ASTIME,FATIME       SYSTEM TIME (STANDARD FORMAT)                
         MVC   ASSYSN,FASYS        SYSTEM NUMBER                                
         MVC   ASSYSO,FAOVSYS      SYSTEM NUMBER (FOR CALLOV)                   
         MVC   ASSIN,FASIN         SYSTEM INPUT NUMBER                          
         MVC   ASIOASTR,FAIOASTR   SYSTEM EXTRA AREA ADDRESS                    
         MVC   ASIOALEN,FAIOALEN   SYSTEM EXTRA AREA LENGTH                     
         MVC   ACTRY,FAACTRY       SET A(COUNTRY & LANGUAGE TABLES)             
         MVC   ALANG,FAALANG                                                    
         MVI   ASONOFF,ASON        SET ONLINE OR OFFLINE SWITCH                 
                                                                                
         L     R1,ASYSLST          LOOK UP SYSTEM IN SYSTEM LIST                
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SYSLSTD,R1                                                       
         CLC   SYSLNUM,ASSYSO                                                   
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   ASSYSNAM,SYSLNAME   EXTRACT SYSTEM NAME                          
*&&UK*&& MVC   ASSYSL,SYSLUSLT     AND SYSTEM LETTER                            
*&&US                                                                           
         MVC   ASSYSL,SYSLNAME                                                  
         CLI   ASSYSO,STRNUMQ      TEST SPOT TRAFFIC                            
         BNE   *+8                                                              
         MVI   ASSYSL,RFPFSSTL     YES - FUDGE SYSTEM LETTER                    
*&&                                                                             
         MVC   CUSYSL,ASSYSL       COPY INTO CUSYSL                             
                                                                                
         MVC   PCPARM(4),PCEFFS    SET A(UTL ENTRY)                             
         GOTOR VSWITCH,PCPARM                                                   
         MVC   PCAUTL,0(R1)        A(UTL) FOR OFFLINE USE ONLY                  
                                                                                
         MVC   PCPARM(4),PCEFFS    SET A(SYSFACS)                               
         MVI   PCPARM,X'FE'                                                     
         GOTOR VSWITCH,PCPARM                                                   
         MVC   ASYSFAC,0(R1)                                                    
         DROP  R2                                                               
                                                                                
         GOTOR VGETFACT,PCPARM,(X'80',0),F#TCBD                                 
         L     R1,0(R1)                                                         
         USING F@TCBD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,F@BSWNUM       R0=N'ENTRIES IN TCBSWTAB                     
         BZ    INITGV04                                                         
         CHI   R0,SYSSWMAX                                                      
         BNH   *+8                                                              
         LHI   R0,SYSSWMAX                                                      
         LA    R1,F@BSWTAB                                                      
         USING F@BSWTAB,R1         R1=A(TCB SWITCH TABLE)                       
         L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(LOCAL SWITCH TABLE)                     
         BASR  RF,0                                                             
         MVC   SYSSWSYS,F@BSWSYS                                                
         MVC   SYSSWSOV,F@BSWSOV                                                
         MVC   SYSSWAGB,F@BSWAGB                                                
         MVC   SYSSWACS,F@BSWACS                                                
         MVC   SYSSWAC2,F@BSWAC2                                                
         AHI   R1,F@BSWLEN         BUMP TO NEXT                                 
         AHI   RE,SYSSWLEN                                                      
         BCTR  R0,RF               DO FOR NUMBER OF ENTRIES                     
         DROP  R1,RE                                                            
                                                                                
INITGV04 L     R1,AINP             EXTRACT PFKEY NUMBER                         
         USING TIOBD,R1                                                         
         ICM   RE,1,TIOBAID                                                     
         BZ    *+12                                                             
         OI    PCINDS1,PCIANYPF    SET USER ENTERED PF KEY THIS TIME            
         STC   RE,PCPFKEY          AND SET PFKEY NUMBER                         
         SR    RE,RE                                                            
         ICM   RE,3,TIOBCURD                                                    
         LA    RE,TWAD(RE)                                                      
         ST    RE,PCACUR           SET A(CURSOR)                                
         DROP  R1                                                               
                                                                                
         CLI   CULANG,0            SET DEFAULT LANGUAGE                         
         BNE   *+8                                                              
*&&UK*&& MVI   CULANG,LANGEUK                                                   
*&&US*&& MVI   CULANG,LANGEUS                                                   
         SR    R0,R0                                                            
         IC    R0,CULANG                                                        
         BCTR  R0,0                                                             
         MHI   R0,L'LANGCHAR                                                    
         BASR  R1,0                                                             
         AHI   R1,LANGCHAR-*                                                    
         AR    R1,R0                                                            
         MVC   PCCHARS,0(R1)       SET SPECIAL CHARACTERS                       
         CLI   CUCTRY,0            SET DEFAULT COUNTRY                          
         BNE   *+8                                                              
*&&UK*&& MVI   CUCTRY,CTRYGBR                                                   
*&&US*&& MVI   CUCTRY,CTRYUSA                                                   
                                                                                
         MVC   PCSWSYSN,ASSYSO     SET SWITCHED SYSTEM INFO                     
         MVC   PCSWSYSC,ASSYSO                                                  
         MVC   PCSWSYSP,ASSYSO                                                  
                                                                                
         GOTOR VDATCON,PCPARM,(3,ASBDAT),(1,ASPDAT)                             
         GOTOR (RF),(R1),(3,ASBDAT),(2,ASCDAT)                                  
         GOTOR (RF),(R1),(3,ASBDAT),(0,ASEDAT)                                  
         GOTOR (RF),(R1),(3,ASBDAT),(15,ASJDAT)                                 
                                                                                
         L     R1,AFILTAB          FIND FILE TABLE ENTRY (FOR IO)               
         SR    RE,RE                                                            
INITGV06 CLI   0(R1),EOT           TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,R1),ASSYSO      MATCH ON OVSYS NUMBER                        
         BE    *+16                                                             
         ICM   RE,3,4(R1)                                                       
         LA    R1,5(RE,R1)                                                      
         B     INITGV06                                                         
         AHI   R1,6                                                             
         ST    R1,AFILNTRY         SAVE A(FILE TABLE ENTRY)                     
                                                                                
         TM    TWAINDS1,TWAIINIT   TEST VALUES SAVED LAST TIME                  
         BZ    INITGV08                                                         
         LA    R0,PCVALS           MOVE SAVE VALUES TO GLOBAL W/S               
         LHI   R1,PCVALSL                                                       
         LA    RE,TWAD                                                          
         AHI   RE,PSVALS-TWAD                                                   
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     TEST                                                             
                                                                                
INITGV08 OI    TWAINDS1,TWAIINIT   INITIALISE DICTIONARY                        
                                                                                
         GOTOR VDICTAT,PCPARM,C'LU  ',ADICUPR,PCUWORDS                          
         GOTOR (RF),(R1),C'LC  ',ADICMIX,PCMWORDS                               
                                                                                
         GOTOR ATSTPID,TWAUSRID    TEST CONNECTED USER IS A PRINCIPAL           
         BNE   *+8                                                              
         OI    TWAINDS1,TWAIUPID                                                
                                                                                
         XC    PCWORK(16),PCWORK   GET PROGRAM PROFILE (CONTROL SYSTEM)         
         MVI   PCWORK,CONLETQ-X'40'                                             
         MVC   PCWORK+1(3),PRGNAM                                               
         MVC   PCWORK+4(2),TWAAGY                                               
         GOTOR VGETPROF,PCPARM,PCWORK,PCPRGPRF,VDMGR                            
                                                                                
         OC    CUPASS,CUPASS       TEST PASSWORD SET                            
         BZ    TEST                                                             
         TM    CUSTAT,CUSPER       TEST CONNECTED WITH PASSWORD                 
         BZ    TEST                                                             
         LA    RF,SECBLKL                                                       
         L     R2,ASECBLK                                                       
         USING SECD,R2                                                          
         GOTOR VSECRET,PCPARM,('SECPINIT',SECD),(RF)                            
         ORG   *-2                                                              
         TM    SECINDS,SECIINIT    TEST SECRET BLOCK INITIALISED                
         BNZ   *+12                                                             
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
                                                                                
TEST     L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         L     R1,AINP             EXTRACT CURSOR POSITION                      
         MVC   CSCURDSP,TIOBCURS-TIOBD(R1)                                      
                                                                                
         CLI   RLPRECH+(FVILEN-FVIHDR),0                                        
         BNE   VALPFK                                                           
         CLI   RLPACTH+(FVILEN-FVIHDR),0                                        
         BNE   VALPFK                                                           
         TM    PCINDS1,PCIANYPF                                                 
         BNZ   VALPFK                                                           
         LA    R0,RLPRECH          NO RECORD/ACTION OR PFKEY THIS TIME          
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EDATA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     SETMSG                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PFKEY                                                      *         
***********************************************************************         
                                                                                
VALPFK   NI    TWAINDS1,FF-TWAINTRL                                             
         TM    TWAINDS2,TWAIRDFR+TWAIDPFK TEST RECORD DISPLAYED FOR             
         BNZ   VALREC                     RENAME OR PFKEYS DISABLED             
         MVC   PCWORK(L'CSRECACT),CSRECACT                                      
         TM    PCINDS1,PCIANYPF    TEST USER ENTERED PFKEY THIS TIME            
         BNZ   VALPFK02                                                         
         TM    TWAINDS2,TWAIAPPF   TEST ONLY APPLICATION PFKEYS ACTIVE          
         BO    VALREC                                                           
         CLI   CSNEXTPF,0          TEST NEXT TIME PFKEY SET                     
         BE    VALREC                                                           
         TM    RLPRECH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   VALREC                                                           
         TM    RLPACTH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   VALREC                                                           
         MVC   PCPFKEY,CSNEXTPF    SET NEXT TIME PFKEY & PROCESS                
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
         B     VALPFK04                                                         
                                                                                
VALPFK02 CLI   PCWORK+1,0                                                       
         BNE   *+16                                                             
         MVI   PCWORK+0,FF                                                      
         MVI   PCWORK+1,FF                                                      
         B     VALPFK04                                                         
                                                                                
         TM    RLPRECH+(FVIIND-FVIHDR),FVIVAL                                   
         BZ    VALREC                                                           
         TM    RLPACTH+(FVIIND-FVIHDR),FVIVAL                                   
         BZ    VALREC                                                           
                                                                                
VALPFK04 L     R2,APFKTAB                                                       
         USING PFKTABD,R2          R2=A(PFKEY TABLE)                            
         SR    R0,R0                                                            
                                                                                
         CLI   PCPFKEY,PFKSPFKQ    TEST PFK SCROLL (RIGHT)                      
         BNE   VALPFK06                                                         
                                                                                
         TM    TWAINDS3,TWAISPFK   TEST PFKEY SCROLLING ACTIVE                  
         BNZ   *+12                                                             
         MVI   PCPFKEY,0           NO - RESET PFKEY AND EXIT                    
         B     VALREC                                                           
                                                                                
         L     R1,AINP             SET CURSOR FROM WHENCE IT CAME               
         MVC   TIOBCURS-TIOBD(,R1),CSCURDSP                                     
         XC    TIOBCURD-TIOBD(,R1),TIOBCURD-TIOBD(R1)                           
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         SR    R2,R2                                                            
         B     SETMSG16                                                         
                                                                                
VALPFK06 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    VALREC                                                           
         CLC   PFKKEY,PCWORK       MATCH ON HEADER RECORD/ACTION                
         BE    *+14                                                             
         ICM   R0,3,PFKLEN                                                      
         AR    R2,R0               POINT TO NEXT SUB-TABLE HEADER               
         B     VALPFK06                                                         
                                                                                
         AHI   R2,PFKHEADL         BUMP TO FIRST DATA ENTRY                     
VALPFK08 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    VALPFK38                                                         
         CLC   PFKNUMB,PCPFKEY     MATCH ON PFKEY NUMBER                        
         BE    *+12                                                             
VALPFK10 AHI   R2,PFKDATAL         POINT TO NEXT DATA ENTRY                     
         B     VALPFK08                                                         
                                                                                
         TM    PFKINDS1,PFKIAPPL   TEST FOR APPLICATION USE                     
         BZ    VALPFK14                                                         
         CLI   PFKCTRY,0           TEST 'ALL' COUNTRIES VALID                   
         BE    VALREC                                                           
         TM    PFKCTRY,CTRYNOT     TEST 'NOT' COUNTRY                           
         BNZ   VALPFK12                                                         
         CLC   PFKCTRY,CUCTRY      MATCH ON COUNTRY CODE                        
         BE    VALREC                                                           
         B     VALPFK10                                                         
VALPFK12 MVC   PCWORK(L'PFKCTRY),PFKCTRY                                        
         NI    PCWORK,FF-CTRYNOT                                                
         CLC   CUCTRY,PCWORK                                                    
         BNE   VALREC                                                           
         B     VALPFK10                                                         
                                                                                
VALPFK14 TM    TWAINDS2,TWAIAPPF   TEST ONLY APPLICATION PFKEYS ACTIVE          
         BO    VALREC                                                           
                                                                                
         TM    PFKINDS1,PFKIKAPA   TEST FOR KNOWN APPLICATION ACTION            
         BNZ   VALREC                                                           
                                                                                
         TM    PFKINDS1,PFKIACTN+PFKISUBA                                       
         BZ    VALPFK30                                                         
                                                                                
         LA    RE,TWASESRA         RE=A(SESSION RECORD/ACTION TABLE)            
         SR    R1,R1                                                            
         ICM   R1,1,TWASESNL       R1=NUMBER OF ENTERED SESSIONS                
         BZ    VALPFK16                                                         
         BASR  RF,0                                                             
         CLC   PFKRECA,0(RE)       TEST RECORD/ACTION ALREADY USED              
         BE    VALREC                                                           
         AHI   RE,L'TWASESRA       BUMP TO NEXT TABLE ENTRY                     
         BCTR  R1,RF                                                            
                                                                                
VALPFK16 TM    PFKCSIL1,CSIUSELC   TEST PASSING LIST KEY                        
         BZ    VALPFK22                                                         
         CLI   PFKRECN,0           TEST RECORD(/ACTION) SET                     
         BE    VALPFK18                                                         
         GOTOR ATSTMIX,PFKRECN     TEST VALID RECORD/ACTION COMBO               
         BNE   VALPFK40                                                         
         TM    PFKINDS1,PFKISUBA   TEST LINE ACTION                             
         BNZ   VALPFK18                                                         
         XC    CSLSTCUR(L'PCLSTCUR),CSLSTCUR                                    
         L     RF,AMIXNTRY                                                      
         SR    RE,RE                                                            
         ICM   RE,1,MIXNTREC-MIXTABD(RF)                                        
         BZ    VALPFK18                                                         
         MHI   RE,L'PCLSTCUR                                                    
         LA    RE,PCLSTCUR-L'PCLSTCUR(RE)                                       
         OC    CSLSTCUR(L'PCLSTCUR),0(RE)                                       
         BNZ   VALPFK18                                                         
         OI    PCINDS1,PCINREC+PCINACT                                          
         GOTOR ARECACT,PFKRECN                                                  
         B     VALREC                                                           
                                                                                
VALPFK18 TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BZ    VALPFK22                                                         
         MVC   PCHALF,CSINDSL2     TEST ALLOWED TO                              
         NC    PCHALF,PFKMASK                                                   
         CLC   PCHALF,PFKMASK                                                   
         BNE   VALPFK40                                                         
         CLI   TWASESNL,TWASESMX   TEST ANY SESSIONS AVAILABLE                  
         BE    VALPFK40                                                         
                                                                                
VALPFK22 TM    PFKINDS1,PFKISUBA   TEST LINE ACTION                             
         BNZ   VALREC              YES - OVERLAY WILL DEAL WITH IT              
                                                                                
         SR    R1,R1                                                            
         CLI   CSQRTN,0            TEST RETURN ROUTINE FROM QUIT                
         BE    VALPFK26                                                         
         LA    R1,PCWORK                                                        
         USING SELTPARM,R1                                                      
         XC    SELTPARM,SELTPARM                                                
         MVC   SELTRTN,CSQRTN      SET RETURN ROUTINE FROM QUIT                 
                                                                                
VALPFK26 TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BNZ   *+12                                                             
         OI    PCINDS1,PCINREC+PCINACT                                          
         B     VALPFK28                                                         
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         OI    SELTINDS,SELTICLR                                                
         GOTOR ANTRSES,(R1)        ENTER NEW RECORD/ACTION SESSION              
         DROP  R1                                                               
                                                                                
VALPFK28 MVC   CSINDSL,PFKCSIL     SET NEXT SESSION INDICATORS                  
         MVC   CSNEXTPF,PFKNEXT    SET NEXT TIME AUTO PFKEY                     
         GOTOR ARECACT,PFKRECN                                                  
         B     VALREC                                                           
                                                                                
VALPFK30 TM    PFKINDS1,PFKIQUIT+PFKINEXT                                       
         BZ    VALPFK36                                                         
         CLI   TWASESNL,0          TEST NESTED                                  
         BE    VALREC                                                           
         MVC   PCHALF,CSINDSL2     TEST ALLOWED TO QUIT                         
         NC    PCHALF,PFKMASK                                                   
         CLC   PCHALF,PFKMASK                                                   
         BNE   VALREC                                                           
                                                                                
         GOTOR AXITSES             RESTORE PREVIOUS SESSION                     
                                                                                
         TM    PFKINDS1,PFKIQUIT   TEST QUIT KEY ENTERED                        
         BZ    *+8                                                              
         NI    CSLTINDS,FF-(CSLTIEOL+CSLTIEOP)                                  
         TM    TWAINDS1,TWAIXITS   TEST XITSES ISSUED                           
         BZ    VALPFK34                                                         
         NI    TWAINDS1,FF-TWAIXITS                                             
         B     GO04                CALL SUB-CONTROLLER                          
                                                                                
VALPFK34 GOTOR ARECACT,CSREC       RESTORE RECORD/ACTION                        
         B     SETMSG              EXIT TO USER                                 
                                                                                
VALPFK36 TM    PFKINDS1,PFKISCRL   TEST SCROLL PFKEY                            
         BZ    VALREC                                                           
         MVC   PCSCROLL,PFKINDS2   SAVE SCROLL INDICATORS                       
         B     VALREC                                                           
                                                                                
VALPFK38 CLI   CSNEXTPF,0          TEST NEXT TIME PFKEY SET                     
         BE    VALREC                                                           
         CLC   PCPFKEY,CSNEXTPF    TEST THIS IS THE ONE JUST USED               
         BE    VALREC                                                           
         MVC   PCPFKEY,CSNEXTPF                                                 
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
         B     VALPFK04                                                         
                                                                                
VALPFK40 MVI   PCPFKEY,0           SET NO PFKEY INPUT THIS TIME                 
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD TYPE FIELD                                          *         
***********************************************************************         
                                                                                
VALREC   MVI   FVMINL,1            SET FIELD IS REQUIRED                        
         GOTOR AFVAL,RLPRECH                                                    
         BNE   SETMSG                                                           
         L     R2,ARECTAB                                                       
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
                                                                                
VALREC02 CLI   RECTABD,EOT         TEST END OF TABLE                            
         BNE   VALREC04                                                         
         MVC   FVMSGNO,=AL2(GE$RRCNR)                                           
         LHI   R0,RECNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(RECNAMLQ),FVIFLD                                       
         B     SETMSG                                                           
                                                                                
VALREC04 SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         SR    R1,R1                                                            
         ICM   R1,3,RECNAMEU                                                    
         LA    R1,WORKD(R1)                                                     
         EX    RE,*+8                                                           
         BE    VALREC06                                                         
         CLC   FVIFLD(0),0(R1)     MATCH AGAINST UPPER CASE NAME                
         SR    R1,R1                                                            
         ICM   R1,3,RECNAMEL                                                    
         LA    R1,WORKD(R1)                                                     
         EX    RE,*+8                                                           
         BNE   VALREC08                                                         
         CLC   FVIFLD(0),0(R1)     MATCH AGAINST MIXED CASE NAME                
VALREC06 TM    RECINDS1,RECIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALREC08                                                         
         GOTOR ATSTREC,RECNUMB     TEST RECORD VALID                            
         BE    VALREC10                                                         
VALREC08 AHI   R2,RECTABL          BUMP TO NEXT TABLE ENTRY                     
         B     VALREC02                                                         
                                                                                
VALREC10 ST    R2,ARECNTRY         SET A(RECORD TYPE TABLE ENTRY)               
         MVC   PCHALF+0(1),RECNUMB                                              
         CLC   RLPREC,CSRECNAM                                                  
         BE    *+14                                                             
         MVC   RLPREC,CSRECNAM                                                  
         OI    RLPRECH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RLPRECH+(FVIIND-FVIHDR),FVIVAL                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION FIELD & RECORD/ACTION COMBINATION                   *         
***********************************************************************         
                                                                                
VALACT   MVI   FVMINL,1            SET FIELD IS REQUIRED                        
         MVI   FVMAXL,ACTNAMLQ                                                  
         GOTOR AFVAL,RLPACTH                                                    
         BNE   SETMSG                                                           
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2          R2=A(ACTION TABLE)                           
                                                                                
VALACT02 CLI   ACTTABD,EOT         TEST EOT                                     
         BNE   VALACT04                                                         
         MVC   FVMSGNO,=AL2(GE$ACTNR)                                           
         LHI   R0,ACTNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(ACTNAMLQ),FVIFLD                                       
         B     SETMSG                                                           
                                                                                
VALACT04 SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         SR    R1,R1                                                            
         ICM   R1,3,ACTNAMEU                                                    
         LA    R1,WORKD(R1)                                                     
         EX    RE,*+8                                                           
         BE    VALACT06                                                         
         CLC   FVIFLD(0),0(R1)     MATCH AGAINST UPPER CASE NAME                
         SR    R1,R1                                                            
         ICM   R1,3,ACTNAMEL                                                    
         LA    R1,WORKD(R1)                                                     
         EX    RE,*+8                                                           
         BNE   VALACT10                                                         
         CLC   FVIFLD(0),0(R1)     MATCH AGAINST MIXED CASE NAME                
                                                                                
VALACT06 TM    ACTINDS1,ACTIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALACT10                                                         
         MVC   PCHALF+1(1),ACTNUMB                                              
         GOTOR ATSTMIX,PCHALF      TEST AUTHORISED FOR RECORD/ACTION            
         BNE   VALACT10                                                         
         TM    ACTINDS1,ACTIUPD    TEST UPDATIVE ACTION                         
         BZ    VALACT08                                                         
         TM    ASXFLAG1,XIROSYS+XIROMODE                                        
         BZ    VALACT08                                                         
         MVC   FVMSGNO,=AL2(GE$UPDNA)                                           
         B     SETMSG                                                           
                                                                                
VALACT08 L     R1,AMIXNTRY                                                      
         USING MIXTABD,R1          R1=A(MIX TABLE ENTRY)                        
         TM    MIXINDS1,MIXIINT    TEST INTERNAL ACTION                         
         BZ    *+12                                                             
         TM    TWAINDS1,TWAINTRL   YES - TEST NTRSES ISSUED                     
         BZ    VALACT10                                                         
         TM    MIXINDS1,MIXISEL    TEST SELECT ACTION ONLY                      
         BZ    VALACT12                                                         
         TM    CSINDSL1,CSIUSELC   YES - TEST NESTED CALL                       
         BZ    VALACT10                                                         
         TM    MIXINDS1,MIXISUB    TEST SUB ACTION                              
         BZ    VALACT12                                                         
         TM    TWAINDS1,TWAINTRL   YES - TEST NTRSES ISSUED                     
         BNZ   VALACT12                                                         
VALACT10 AHI   R2,ACTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     VALACT02                                                         
         DROP  R1                                                               
                                                                                
VALACT12 ST    R2,AACTNTRY         SET A(ACTION TABLE ENTRY)                    
         SR    R1,R1                                                            
         ICM   R1,3,ACTNAMEL                                                    
         LA    R1,WORKD(R1)                                                     
         MVC   CSACTNAM,0(R1)                                                   
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3          R3=A(MIX TABLE ENTRY)                        
         GOTOR ASETSEL,MIXKEY      SET A(SELECT TABLE)                          
         MVC   CSSCRN,MIXSCRN      EXTRACT MIXTAB VALUES                        
         MVC   CSOVER,MIXOVER                                                   
         MVC   CSMIX1,MIXINDS1                                                  
         MVC   CSMIX2,MIXINDS2                                                  
         CLC   CSREC,MIXRECB       TEST CHANGE OF RECORD TYPE                   
         MVC   CSREC,MIXRECB       SET RECORD NUMBER                            
         BE    *+8                                                              
         OI    PCINDS1,PCINREC+PCINACT                                          
         CLC   CSACT,MIXACTB       TEST CHANGE OF ACTION                        
         MVC   CSACT,MIXACTB       SET ACTION NUMBER                            
         BE    *+8                                                              
         OI    PCINDS1,PCINACT                                                  
         CLC   RLPACT,CSACTNAM                                                  
         BE    *+14                                                             
         MVC   RLPACT,CSACTNAM                                                  
         OI    RLPACTH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RLPACTH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   CSQRTN,MIXQRTN      SAVE RETURN ROUTINE FROM QUIT                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SCROLL FIELD                                               *         
***********************************************************************         
                                                                                
VALSCR   TM    RLPSCRH+(FVIIND-FVIHDR),FVIVAL                                   
         BNZ   VALSCRX                                                          
         CLI   RLPSCRH+(FVILEN-FVIHDR),0                                        
         BNE   VALSCR02                                                         
         MVC   RLPSCR(L'PCMPAGE),PCMPAGE                                        
                                                                                
VALSCR02 GOTOR AFVAL,RLPSCRH                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FVIIND,FVINUM                                                    
         BNZ   VALSCR06                                                         
                                                                                
         LA    RE,PCMPAGE                                                       
         LHI   RF,PFKIMAXN                                                      
         CLC   FVIFLD(1),PCUMAX    M(AXIMUM)                                    
         BE    VALSCR04                                                         
         LHI   RF,PFKIPAGE                                                      
         CLC   FVIFLD(1),PCUPAGE   P(AGE)                                       
         BE    VALSCR04                                                         
         LA    RE,PCMHALF                                                       
         LHI   RF,PFKIHALF                                                      
         CLC   FVIFLD(1),PCUHALF   H(ALF)                                       
         BNE   VALSCR08                                                         
                                                                                
VALSCR04 XC    RLPSCR,RLPSCR                                                    
         MVC   RLPSCR(L'PCMMAX),0(RE)                                           
         OI    RLPSCRH+(FVOIND-FVIHDR),FVOXMT                                   
         STC   RF,PCSCRNUM                                                      
         B     VALSCRX                                                          
                                                                                
VALSCR06 OC    PCFULL,PCFULL       VALIDATE SCROLL AMOUNT                       
         BZ    VALSCR08                                                         
         OC    PCFULL(3),PCFULL                                                 
         BNZ   VALSCR08                                                         
         CLI   PCFULL+3,15                                                      
         BH    VALSCR08                                                         
         MVC   PCSCRNUM,PCFULL+3                                                
         B     VALSCRX                                                          
                                                                                
VALSCR08 MVC   FVMSGNO,=AL2(GE$ISCRL)                                           
         B     SETMSGNI                                                         
                                                                                
VALSCRX  TM    PCSCRNUM,PFKIMAXN                                                
         BNZ   *+8                                                              
         OI    RLPSCRH+(FVIIND-FVIHDR),FVIVAL                                   
         EJECT                                                                  
***********************************************************************         
* GO TO SUB-CONTROLLER                                                *         
***********************************************************************         
                                                                                
GO       MVI   PCBYTE1,0                                                        
         TM    PCINDS1,PCINREC+PCINACT                                          
         BZ    GO04                                                             
                                                                                
         XC    CSHIRECN,CSHIRECN                                                
         XC    CSPSRECN,CSPSRECN                                                
         XC    CSINDSL,CSINDSL                                                  
         XC    CSINDSG,CSINDSG                                                  
         XC    CSINITRA,CSINITRA   CLEAR INITIAL RECORD/ACTION                  
         LA    R0,TWAD                                                          
         AHI   R0,OSVALS-TWAD                                                   
         LHI   R1,OSVALSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TM    TWAINDS1,TWAINTRL                                                
         BNZ   GO04                                                             
         MVI   TWASESNL,0                                                       
                                                                                
GO04     GOTOR AOVRPHS,CSOVER      LOAD SUB-CONTROLLER                          
         BNE   SETMSG                                                           
         MVC   PCNTRYA(1),PCBYTE1                                               
         L     RF,AMIXNTRY                                                      
         SR    RE,RE                                                            
         ICM   RE,1,MIXNTREC-MIXTABD(RF)                                        
         BZ    GO06                                                             
         MHI   RE,L'PCLSTCUR                                                    
         LA    RE,PCLSTCUR-L'PCLSTCUR(RE)                                       
         MVC   CSLSTCUR(L'PCLSTCUR),0(RE)                                       
                                                                                
GO06     GOTOR PCNTRYA                                                          
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    *+12                                                             
         NI    TWAINDS1,FF-(TWAINTRS+TWAIXITS)                                  
         B     SETMSG                                                           
         TM    TWAINDS1,TWAINTRS   TEST NTRSES ISSUED                           
         BZ    *+16                                                             
         NI    TWAINDS1,FF-TWAINTRS                                             
         OI    TWAINDS1,TWAINTRL                                                
         B     VALREC                                                           
                                                                                
         TM    TWAINDS1,TWAIXITS   TEST XITSES ISSUED                           
         BZ    SETMSG                                                           
         NI    TWAINDS1,FF-(TWAIXITS+TWAINTRL)                                  
         B     GO04                                                             
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
* NTR AT SETMSGNI TO SET MULTIPLE FIELD INDEX VALUES TO ZERO          *         
*        SETMSG FOR REGULAR MESSAGE BUILDING                          *         
*        SETMSG08 ONLY TO SET CURSOR TO FIELD ADDRESSED BY FVADDR     *         
***********************************************************************         
                                                                                
SETMSGNI MVI   FVINDX,0            ENTRY POINT FOR NO INDEX INFO                
         MVI   FVSUBX,0                                                         
                                                                                
SETMSG   SR    R2,R2                                                            
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    *+12                                                             
         NI    CSINDSG1,FF-(CSINDUNW)                                           
         LHI   R2,1                                                             
         CLC   FVMSGNO,=AL2(FVFSET) TEST USER HAS SUPPLIED MESSAGE              
         BE    SETMSG08                                                         
         MVC   CSMSGNUM,FVMSGNO                                                 
         MVC   CSMSGTYP,FVOMTYP                                                 
         LA    R1,PCPARM           DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R1                                                       
         CLC   FVMSGNO,=AL2(FVFGTSET)   TEST APPL SET GETTXT BLOCK              
         BE    SETMSG06                                                         
                                                                                
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FVINDX                                                    
         MVC   GTSUBX,FVSUBX                                                    
         MVC   GTMSGNO,FVMSGNO                                                  
         MVC   GTMSYS,FVOSYS       OVERRIDE SYSTEM (IF SET)                     
         CLI   GTMSYS,0            TEST OVERRIDE SYSTEM SET                     
         BNE   *+8                                                              
         MVI   GTMSYS,FF           NO - SET GENERAL SYSTEM                      
         MVC   GTMTYP,FVOMTYP      OVERRIDE MESSAGE TYPE (IF SET)               
         CLI   GTMSGNO,FF          STD CONTROLLER MSG                           
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF           GENERAL SYSTEM MESSAGE                       
         OC    GTMSGNO,GTMSGNO     MESSAGE 0 = DATAMGR ERR                      
         BNZ   SETMSG02                                                         
         LA    R0,PCPARM                                                        
         STCM  R0,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
                                                                                
SETMSG02 LA    R3,FVXTRA+L'FVXTRA-1                                             
         LHI   R0,L'FVXTRA                                                      
         BASR  RF,0                                                             
         CLI   0(R3),SPACE                                                      
         BH    *+12                                                             
         BCTR  R3,0                                                             
         BCTR  R0,RF                                                            
         B     SETMSG04                                                         
         LA    R3,FVXTRA                                                        
         STCM  R3,7,GTATXT         SET LENGTH & ADDRESS OF EXTRA TEXT           
         STCM  R0,1,GTLTXT                                                      
                                                                                
SETMSG04 LA    R1,PCPARM           PCPARM DEFINED INTERNALLY                    
         CLI   GTMSGNO,FF          CHECK FOR GENERAL MESSAGES                   
         BNE   SETMSG06                                                         
         MVI   GTMSYS,FF           FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
                                                                                
SETMSG06 LA    R0,FVPARMS          SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         GOTOR VGETTXT,(R1)        RESOLVE MESSAGE                              
         DROP  R1                                                               
                                                                                
SETMSG08 OI    RLPMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         L     R1,AINP             TEST IF OVERLAY SET CURSOR                   
         TM    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         BNZ   SETMSG12                                                         
         ICM   R1,15,OVCURSOR      TEST CURSOR ADDRESS SET                      
         BNZ   *+12                                                             
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    SETMSG12                                                         
                                                                                
         CLI   FVERRNDX,0          TEST FIELD INDEX VALUE SET                   
         BE    SETMSG10                                                         
         L     RE,AINP                                                          
         USING TIOBD,RE                                                         
         LA    R0,TWAD                                                          
         LR    RF,R1                                                            
         SR    RF,R0               RF=DISPLACEMENT TO FIELD                     
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FVERRNDX                                                
         OI    TIOBINDS,TIOBSETC                                                
         DROP  RE                                                               
                                                                                
SETMSG10 OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         AR    R1,RE               POINT TO NEXT FIELD HEADER                   
         LA    RF,TWALAST          RF=END OF LARGEST SCREEN                     
         ICM   RE,1,FVTLEN-FVIHDR(R1) TURN OFF CURSORS TO BOTTOM OF TWA         
         BZ    SETMSG12                                                         
         NI    FVOIND-FVIHDR(R1),FF-FVOCUR                                      
         BXLE  R1,RE,*-12                                                       
                                                                                
SETMSG12 GOTOR ATSARIO,TSASAV      SAVE TSAR BUFFER IF NECESSARY                
                                                                                
         SR    RE,RE               SAVE CURRENT RECORD ENTRY                    
         ICM   RE,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    SETMSG14                                                         
         MHI   RE,L'PCLSTCUR                                                    
         LA    RE,PCLSTCUR-L'PCLSTCUR(RE)                                       
         MVC   0(L'PCLSTCUR,RE),CSLSTCUR                                        
                                                                                
SETMSG14 LA    R0,TWAD             SAVE GLOBAL VALUES IN TWA                    
         AHI   R0,PSVALS-TWAD                                                   
         LHI   R1,PSVALSL                                                       
         LA    RE,PCVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
SETMSG16 TM    TWAINDS1,TWAIINIT   TEST INITIALISED                             
         BZ    SETMSG18                                                         
         GOTOR ABLDPFK             BUILD PFKEY DISPLAY LINE                     
                                                                                
SETMSG18 LTR   R2,R2               TEST UNWIND VIA $ABEND                       
         BZ    RLPX                                                             
         DC    H'0',C'$ABEND'      UNWIND TRANSACTION                           
                                                                                
RLPX     J     EXIT                                                             
         EJECT                                                                  
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITE    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                EXIT WITH CONDITION CODE SET                 
                                                                                
EXIT     XIT1  ,                   EXIT WITH CONDITION CODE SET                 
                                                                                
         LTORG                                                                  
                                                                                
PRGNAM   DC    C'RLP'              PROGRAM NAME FOR GETPROF CALL                
         EJECT                                                                  
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
                                                                                
ROUT     NMOD1 300,**ROU1**,R8,R7,R6,CLEAR=YES,RR=RE                            
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         SRL   RF,32-8                                                          
         SLL   RF,2                RF=ROUTINE#*4                                
         LA    RF,ROUTS(RF)                                                     
         BR    RF                  GO TO JUMP INSTRUCTION                       
                                                                                
ROUTS    DS    0XL4                                                             
         B     BLDPFK              BUILD PFKEY LINE                             
         B     TSTACS              TEST AUTHORISATION ACCESS                    
         B     TSTREC              LOOKUP RECORD & TEST AUTHORISATION           
         DC    AL4(0)              N/D (WAS TSTACT)                             
         B     TSTMIX              LOOKUP RECACT & TEST AUTHORISATION           
         B     VALOPT              VALIDATE OPTIONS                             
         B     GETGEN              GET GENERAL MESSAGE                          
         B     SETSEL              SET A(SELECT TABLE) FOR REC/ACT              
         B     GETUID              GET USER-ID CODE                             
         B     VALUID              VALIDATE USER-ID CODE                        
         B     VALFRQ              VALIDATE FREQUENCY                           
         B     GETFRQ              LOOK UP FREQUENCY NAME                       
         B     VALGID              VALIDATE USER-ID GROUP LIST                  
         B     VALDST              VALIDATE DESTINATION ID                      
         B     VALOUT              VALIDATE OUTPUT TYPE                         
         B     FLDSEC              TEST FIELD SECURITY                          
         B     OVRSCR              OVERLAY SCREEN                               
         B     OVRPHS              OVERLAY PHASE                                
         B     FVAL                VALIDATE INPUT FIELD                         
         B     IO                  I/O EXECUTIVE                                
         B     NTRSES              PUSH SESSION                                 
         B     XITSES              POP SESSION                                  
         B     RECACT              SET RECORD/ACTION                            
         B     TSARIO              INTERFACE TO TSAR                            
         B     TSTPID              TEST PRINCIPAL ID                            
         B     INIRFP              INITIALISE RFP BLOCK VARIABLES               
         B     VALAGY              VALIDATE AN AGENCY ALPHA ID                  
         B     GOSOFT              SOFDAT INTERFACE                             
         B     VALGRP              VALIDATE GROUP USING RFPIO                   
         B     RFPXFG              MOVE RFP VALUES TO XFILE RECORD              
         B     VALRUN              VALIDATE RUN OPTION                          
         B     GETNXT              GET NEXT RUN DATE FOR A GROUP                
         B     GETEND              GET END RUN DATE FOR A GROUP                 
         B     GETAGY              GET AGENCY PRINCIPAL ID NAME                 
         B     VALSYS              VALIDATE SYSTEM NAME                         
         B     GETSYS              GET SYSTEM NAME                              
         B     GETRSH              GET RUN SCHEDULE                             
         B     XFGRFP              MOVE XFILE VALUES TO RFP BLOCK               
         B     GETWRK              TEST (& GET NEXT/PREVIOUS) WORK DAY          
         B     SETKEY              BUILD NEXT I/O KEY                           
         B     FLTKEY              FILTER I/O KEY                               
         B     GETLST              GET LAST RUN DATE FOR A GROUP                
         B     GETPER              GET PERSON DETAILS                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PFKEY LINE                                         *         
***********************************************************************         
                                                                                
         USING BPWORKD,RC                                                       
BLDPFK   LA    R2,RLPOLY1H         LOCATE PFKEY DISPLAY LINE IN TWA             
         LA    RF,TWALAST          RF=A(LAST POSSIBLE BYTE IN TWA)              
         SR    RE,RE                                                            
BLDPFK02 TM    FVATRB-FVIHDR(R2),FVAXTND                                        
         BZ    BLDPFK04                                                         
         IC    RE,FVTLEN-FVIHDR(R2)                                             
         LA    R1,0(R2,RE)                                                      
         SHI   R1,L'FVIHDR         R1=A(EXTENDED FIELD HEADER)                  
         CLI   0(R1),255                                                        
         BE    BLDPFK06                                                         
BLDPFK04 ICM   RE,1,FVTLEN-FVIHDR(R2)                                           
         BZ    BLDPFKX                                                          
         BXLE  R2,RE,BLDPFK02                                                   
         B     BLDPFKX                                                          
                                                                                
BLDPFK06 ICM   RE,1,FVTLEN-FVIHDR(R2)                                           
         SHI   RE,L'FVIHDR+L'FVIHDR+1                                           
         BASR  RF,0                                                             
         EX    RE,8(RF)                                                         
         B     *+10                                                             
         XC    L'FVIHDR(0,R2),L'FVIHDR(R2)                                      
         AHI   RE,-5               NEED 6 BYTES FOR ' NN=>>'                    
         STC   RE,BPLOUT                                                        
         OI    FVATRB-FVIHDR(R2),FVAHIGH                                        
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
                                                                                
         LHI   R5,PFKEYS-TWAD                                                   
         LA    R5,TWAD(R5)         R5=A(PFKEY LIST)                             
                                                                                
         MVC   BPRECACT,CSREC                                                   
         CLI   BPRECACT+1,0                                                     
         BNE   *+12                                                             
         MVI   BPRECACT+0,FF                                                    
         MVI   BPRECACT+1,FF                                                    
                                                                                
         L     R3,APFKTAB          LOCATE PFKEY SUB-TABLE                       
         USING PFKTABD,R3          R3=A(PFKEY TABLE)                            
         SR    R1,R1                                                            
BLDPFK08 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    BLDPFKX                                                          
         CLC   PFKKEY,BPRECACT     TEST CURRENT RECORD MATCHES                  
         BE    *+14                                                             
         ICM   R1,3,PFKLEN         BUMP TO NEXT TABLE HEADER                    
         AR    R3,R1                                                            
         B     BLDPFK08                                                         
         AHI   R3,PFKHEADL         BUMP TO FIRST DATA ENTRY                     
         STCM  R3,15,BPATAB        SAVE A(FIRST DATA ENTRY)                     
                                                                                
         AHI   R2,L'FVIHDR         BUMP TO PFKEY FIELD                          
         ST    R2,BPAOUT           SAVE A(OUTPUT FIELD)                         
                                                                                
         OC    CSRECACT,CSRECACT   TEST FIRST TIME                              
         BZ    BLDPFK12                                                         
         CLC   TWALRA,CSRECACT     TEST CHANGE OF RECORD/ACTION                 
         BNE   BLDPFK12                                                         
         CLI   PCPFKEY,PFKSPFKQ    TEST PFKEY SCROLL KEY ENTERED                
         BNE   BLDPFK32                                                         
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,TWAPFKN        RF=N'PFKEYS IN LIST                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,1,TWAPFKD        RE=N'PFKEYS DISPLAYED                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RE               RF=N'UNSEEN PFKEYS                           
         LA    R1,0(R5,RE)         R1=A(FIRST UNSEEN PFKEY)                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BPWORK(0),0(R1)     SET FIRST UNSEEN AS NEXT                     
         LA    R1,BPWORK+1(RF)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R5)       MOVE SEEN LIST TO END                        
         MVC   0(L'PFKEYS,R5),BPWORK                                            
         B     BLDPFK32                                                         
                                                                                
BLDPFK12 XC    0(L'PFKEYS,R5),0(R5)                                             
         MVI   TWAPFKN,0                                                        
         NI    TWAINDS3,FF-(TWAISPFK)                                           
                                                                                
BLDPFK14 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    BLDPFK32                                                         
         CLC   PFKKEY,CSNEXTPF     TEST AUTO PF KEY                             
         BE    BLDPFK30                                                         
         TM    PFKINDS1,PFKIKAPA   TEST FOR KNOWN APPLICATION ACTION            
         BNZ   BLDPFK28                                                         
                                                                                
BLDPFK16 TM    PFKINDS1,PFKIAPPL   TEST FOR APPLICATION USE                     
         BZ    BLDPFK20                                                         
         CLI   PFKCTRY,0           TEST 'ALL' COUNTRIES VALID                   
         BE    BLDPFK28                                                         
         TM    PFKCTRY,CTRYNOT     TEST 'NOT' COUNTRY                           
         BNZ   BLDPFK18                                                         
         CLC   PFKCTRY,CUCTRY      MATCH ON COUNTRY CODE                        
         BNE   BLDPFK30                                                         
         B     BLDPFK28                                                         
                                                                                
BLDPFK18 MVC   PCWORK(L'PFKCTRY),PFKCTRY                                        
         NI    PCWORK,FF-CTRYNOT                                                
         CLC   CUCTRY,PCWORK                                                    
         BE    BLDPFK30                                                         
         B     BLDPFK28                                                         
                                                                                
BLDPFK20 MVC   PCHALF,CSINDSL2     TEST PFKEY ALLOWED                           
         NC    PCHALF,PFKMASK                                                   
         CLC   PCHALF,PFKMASK                                                   
         BNE   BLDPFK30                                                         
         CLI   PFKRECN,0           TEST RECORD/ACTION SET                       
         BNE   *+12                                                             
         CLI   PFKACTN,0                                                        
         BE    BLDPFK30                                                         
         TM    PFKINDS1,PFKIQUIT                                                
         BZ    BLDPFK22                                                         
         CLI   TWASESNL,0          TEST NESTED                                  
         BE    BLDPFK30                                                         
         B     BLDPFK28                                                         
                                                                                
BLDPFK22 TM    PFKINDS1,PFKINEXT   TEST NEXT PFKEY                              
         BZ    BLDPFK24                                                         
         SR    RF,RF                                                            
         ICM   RF,1,TWASESNL                                                    
         BZ    BLDPFK30                                                         
         SLL   RF,1                                                             
         LA    RF,TWASESRA-L'TWASESRA(RF)                                       
         CLI   L'CSREC(RF),ACTLST  TEST PREVIOUS LEVEL IS A LIST                
         BNE   BLDPFK30                                                         
         B     BLDPFK28                                                         
                                                                                
BLDPFK24 TM    PFKINDS1,PFKISCRL                                                
         BNZ   BLDPFK28                                                         
                                                                                
         TM    PFKINDS1,PFKIACTN+PFKISUBA                                       
         BZ    BLDPFK30                                                         
                                                                                
         LA    RE,TWASESRA         RE=A(SESSION RECORD/ACTION TABLE)            
         SR    R1,R1                                                            
         ICM   R1,1,TWASESNL       R1=NUMBER OF ENTERED SESSIONS                
         BZ    BLDPFK26                                                         
         BASR  RF,0                                                             
         CLC   PFKRECA,0(RE)       TEST RECORD/ACTION ALREADY USED              
         BE    BLDPFK30                                                         
         AHI   RE,L'TWASESRA       BUMP TO NEXT TABLE ENTRY                     
         BCTR  R1,RF                                                            
                                                                                
BLDPFK26 GOTOR ATSTACS,PFKRECA     TEST RECORD/ACTION AUTHORISATION             
         BNE   BLDPFK30                                                         
         TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BZ    BLDPFK28                                                         
         CLI   TWASESNL,TWASESMX   TEST ANY SESSIONS AVAILABLE                  
         BE    BLDPFK30                                                         
                                                                                
BLDPFK28 MVC   0(1,R5),PFKNUMB     SET VALID PFKEY IN LIST                      
         AHI   R5,1                                                             
         IC    RE,TWAPFKN          BUMP N'KEYS IN LIST                          
         AHI   RE,1                                                             
         STC   RE,TWAPFKN                                                       
                                                                                
BLDPFK30 AHI   R3,PFKDATAL         BUMP TO NEXT DATA ENTRY                      
         B     BLDPFK14                                                         
                                                                                
BLDPFK32 MVI   TWAPFKD,0           RESET N'PFKEYS OUTPUT                        
         LHI   R5,PFKEYS-TWAD                                                   
         LA    R5,TWAD(R5)         R5=A(PFKEY LIST)                             
         MVI   BPLREC,0            INITIALISE LAST RECORD BYTE                  
         CLI   0(R5),0             EXIT IF NO PFKEYS TO BE DISPLAYED            
         BE    BLDPFKX                                                          
                                                                                
         L     R2,BPAOUT           POINT TO WORK AREA                           
         MVC   0(2,R2),PCUPFK                                                   
         AHI   R2,2                                                             
         LHI   R3,2                                                             
                                                                                
BLDPFK34 GOTOR BLDPRA              BUILD PFKEY EXPRESSION                       
                                                                                
         SR    R1,R1               TEST THIS ONE WILL FIT ON LINE               
         IC    R1,PCWORK                                                        
         LA    R3,2(R1,R3)                                                      
         CLM   R3,1,BPLOUT                                                      
         BH    BLDPFK36                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PCWORK+1                                                 
         LA    R2,2(R2,R1)                                                      
         IC    R1,TWAPFKD                                                       
         AHI   R1,1                                                             
         STC   R1,TWAPFKD                                                       
         AHI   R5,1                                                             
         CLI   0(R5),0                                                          
         BNE   BLDPFK34                                                         
         CLC   TWAPFKD,TWAPFKN     TEST ALL PFKEYS FIT IN DISPLAY               
         BE    BLDPFKX                                                          
                                                                                
BLDPFK36 OI    TWAINDS3,TWAISPFK   SET PFKEY SCROLLING ACTIVE                   
         L     R2,BPAOUT                                                        
         SR    R3,R3                                                            
         IC    R3,BPLOUT                                                        
         AR    R2,R3                                                            
         LHI   RE,PFKSPFKQ                                                      
         CVD   RE,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  1(2,R2),PCDUB                                                    
         MVC   3(L'PCEQUAL,R2),PCEQUAL                                          
         MVC   3+L'PCEQUAL(2,R2),=C'>>'                                         
                                                                                
BLDPFKX  MVC   TWALRA,CSRECACT     SAVE CURRENT RECORD/ACTION                   
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PFKEY DESCRIPTION (NN=RECORD/ACTION)               *         
*                                                                     *         
* NTRY - R5=A(PFKEY NUMBER)                                           *         
* EXIT - PCWORK=AL1(L'OUTPUT EXPRESSION),CLNN'OUTPUT EXPRESSION'      *         
***********************************************************************         
                                                                                
BLDPRA   NTR1  ,                                                                
         LA    R2,PCWORK+1         R2=A(OUTPUT AREA)                            
         SR    R1,R1               OUTPUT PFKEY NUMBER                          
         ICM   R1,1,0(R5)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,X'0F'                                            
         UNPK  0(2,R2),PCDUB                                                    
         CLI   0(R2),C'0'          TEST LEADING ZERO                            
         BE    *+12                                                             
         AHI   R2,2                                                             
         B     *+14                                                             
         MVC   0(2,R2),1(R2)       SUPPRESS LEADING ZERO                        
         AHI   R2,1                                                             
         MVC   0(L'PCEQUAL,R2),PCEQUAL                                          
                                                                                
         ICM   R3,15,BPATAB        POINT TO FIRST PFKEY TABLE ENTRY             
BLDPRA02 CLC   PFKNUMB,0(R5)       SEARCH FOR PFKEY NUMBER                      
         BE    BLDPRA04                                                         
         AHI   R3,PFKDATAL         BUMP TO NEXT ENTRY                           
         CLI   PFKNUMB,0                                                        
         BNE   BLDPRA02                                                         
         DC    H'0'                                                             
                                                                                
BLDPRA04 TM    PFKINDS1,PFKISCRL+PFKIAPPL                                       
         BNZ   BLDPRA14                                                         
         TM    PFKINDS1,PFKIQUIT+PFKINEXT                                       
         BNZ   BLDPRA10                                                         
         TM    PFKINDS1,PFKIKAPA                                                
         BNZ   BLDPRA10                                                         
                                                                                
         CLC   PFKRECN,BPLREC      TEST SAME RECORD AS PREVIOUS                 
         BE    BLDPRA08                                                         
         MVC   BPLREC,PFKRECN      SET LAST DISPLAYED RECORD TYPE               
         L     RF,ARECTAB          LOCATE RECORD TABLE ENTRY                    
         USING RECTABD,RF                                                       
BLDPRA06 CLI   RECTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RECNUMB,PFKRECN     MATCH ON RECORD NUMBER                       
         BE    *+12                                                             
         AHI   RF,RECTABL          BUMP TO NEXT TABLE ENTRY                     
         B     BLDPRA06                                                         
         SR    RE,RE               OUTPUT RECORD NAME                           
         ICM   RE,3,RECNAMEL                                                    
         LA    RE,WORKD(RE)                                                     
         MVC   1(RECNAMLQ,R2),0(RE)                                             
         AHI   R2,RECNAMLQ                                                      
                                                                                
BLDPRA08 BASR  RF,0                OUTPUT SLASH BETWEEN RECORD & ACTION         
         CLI   0(R2),SPACE                                                      
         BH    *+6                                                              
         BCTR  R2,RF                                                            
         MVC   1(L'PCSLASH,R2),PCSLASH                                          
         AHI   R2,L'PCSLASH                                                     
                                                                                
BLDPRA10 L     RF,AACTTAB          LOCATE ACTION TABLE ENTRY                    
         USING ACTTABD,RF                                                       
BLDPRA12 CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTNUMB,PFKACTN     MATCH ON ACTION NUMBER                       
         BE    *+12                                                             
         AHI   RF,ACTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     BLDPRA12                                                         
         SR    RE,RE               OUTPUT ACTION NAME                           
         ICM   RE,3,ACTNAMEL                                                    
         LA    RE,WORKD(RE)                                                     
         B     BLDPRA16                                                         
                                                                                
BLDPRA14 SR    RE,RE               OUTPUT SCROLL EXPRESSION                     
         ICM   RE,3,PFKSCRLL                                                    
         LA    RE,WORKD(RE)                                                     
         TM    PFKINDS1,PFKIAPPL                                                
         BNZ   BLDPRA16                                                         
         TM    PFKINDS2,PFKIMAXN+PFKINPFX                                       
         BNZ   BLDPRA16                                                         
         MVI   1(R2),C'+'          NO - OUTPUT DIRECTION                        
         TM    PFKINDS2,PFKIUPDN                                                
         BZ    *+8                                                              
         MVI   1(R2),C'-'                                                       
         AHI   R2,1                                                             
                                                                                
BLDPRA16 MVC   1(PFKSCRLQ,R2),0(RE)                                             
         AHI   R2,PFKSCRLQ                                                      
         TM    PFKINDS2,PFKILONG   TEST LONG PFKEY DESCRIPTION                  
         BZ    BLDPRA18                                                         
         CLI   PFKINDS1,PFKIAPPL   UNLESS PFKEY FOR APPLICATION USE             
         BE    BLDPRA18                                                         
         MVC   1+PFKSCRLQ(PFKSCRLQ,R2),PFKSCRLQ(RE)                             
         AHI   R2,PFKSCRLQ                                                      
                                                                                
BLDPRA18 BASR  RF,0                                                             
         CLI   0(R2),SPACE                                                      
         BH    *+6                                                              
         BCTR  R2,RF                                                            
         LA    RE,PCWORK+1                                                      
         SR    R2,RE                                                            
         STC   R2,PCWORK           SET L'OUTPUT IN PCWORK                       
                                                                                
BLDPRAX  J     EXIT                                                             
         DROP  R3,RF                                                            
                                                                                
BPWORKD  DSECT                     ** BLDPFK S/R LOCAL W/S **                   
BPATAB   DS    A                   A(PFKEY SUB-TABLE)                           
BPAOUT   DS    A                   A(OUTPUT AREA)                               
BPLOUT   DS    XL1                 L'OUTPUT AREA                                
BPWORK   DS    XL64                                                             
BPLREC   DS    XL(L'PFKRECN)                                                    
BPRECACT DS    XL2                                                              
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST AUTHORISATION FOR A RECORD OR RECORD/ACTION COMBO   *         
***********************************************************************         
                                                                                
TSTACS   LR    RF,R1                                                            
         OC    TWALEN,TWALEN       TEST NEW SECURITY IN USE                     
         JZ    EXITE                                                            
         CLI   L'CSREC(RF),0       TEST ACTION SPECIFIED                        
         BE    TSTACS02                                                         
         GOTOR VSECRET,PCPARM,('SECPRACT',ASECBLK),(0(RF),1(RF))                
         B     TSTACSX                                                          
                                                                                
TSTACS02 GOTOR VSECRET,PCPARM,('SECPRCD',ASECBLK),(RF)                          
                                                                                
TSTACSX  JNE   EXITH                                                            
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* LOOKUP RECORD TABLE & TEST AUTHORISATION                            *         
*                                                                     *         
* NTRY - R1=A(RECORD NUMBER)                                          *         
***********************************************************************         
                                                                                
TSTREC   L     R2,ARECTAB                                                       
         USING RECTABD,R2          LOCATE RECTAB ENTRY                          
         MVC   PCWORK(L'RECNUMB),0(R1)                                          
         MVI   PCWORK+L'RECNUMB,0  SET 'ALL' ACTION VALUE                       
         LA    R1,PCWORK                                                        
                                                                                
TSTREC02 CLI   RECTABD,EOT         TEST END OF TABLE                            
         JE    EXITH                                                            
         CLC   RECNUMB,PCWORK      MATCH ON RECORD NUMBER                       
         BNE   TSTREC06                                                         
         TM    RECINDS1,RECIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    TSTREC06                                                         
         GOTOR ATSTACS             TEST RECORD AUTHORISATION                    
         BNE   TSTREC06                                                         
                                                                                
         CLI   RECCTRY,0           ANY COUNTRY SET?                             
         BE    TSTREC04                                                         
         CLC   RECCTRY,CUCTRY      MATCH ON COUNTRY?                            
         BE    TSTREC04                                                         
                                                                                
         TM    RECCTRY,CTRYNOT     VALID FOR ALL BUT THIS COUNTRY?              
         BZ    TSTREC06                                                         
                                                                                
         MVC   PCBYTE1,RECCTRY                                                  
         NI    PCBYTE1,FF-(CTRYNOT)                                             
         CLC   PCBYTE1,CUCTRY                                                   
         BE    TSTREC06                                                         
                                                                                
TSTREC04 ST    R2,ARECNTRY         SAVE A(RECORD TABLE ENTRY)                   
         SR    RE,RE                                                            
         ICM   RE,3,RECNAMEL       EXTRACT LOWER CASE RECORD WORD               
         LA    RE,WORKD(RE)                                                     
         MVC   CSRECNAM,0(RE)                                                   
         J     EXITE                                                            
                                                                                
TSTREC06 AHI   R2,RECTABL          BUMP TO NEXT TABLE ENTRY                     
         B     TSTREC02                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOOKUP MIX TABLE & TEST AUTHORISATION                               *         
*                                                                     *         
* NTRY - R1=A(RECORD/ACTION COMBO)                                    *         
***********************************************************************         
                                                                                
TSTMIX   L     R2,AMIXTAB                                                       
         USING MIXTABD,R2          LOCATE MIXTAB ENTRY                          
TSTMIX02 CLI   MIXTABD,EOT         TEST END OF TABLE                            
         JE    EXITH                                                            
         CLC   MIXKEY,0(R1)        MATCH RECORD                                 
         BNE   TSTMIX04                                                         
         GOTOR ATSTACS             TEST AUTHORISATION                           
         JNE   EXITH                                                            
         TM    MIXINDS1,MIXIDDS    TEST DDS ONLY COMBO                          
         BZ    TSTMIXX                                                          
         TM    CUSTAT,CUSDDS       YES - TEST IF A DDS TERMINAL                 
         BNZ   TSTMIXX                                                          
TSTMIX04 AHI   R2,MIXTABL          BUMP TO NEXT TABLE ENTRY                     
         B     TSTMIX02                                                         
                                                                                
TSTMIXX  ST    R2,AMIXNTRY         SAVE A(COMBO TABLE ENTRY)                    
         J     EXITE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN OPTIONS FIELD                                *         
*                                                                     *         
* NTRY - FVAL MUST HAVE BEEN CALLED TO EXTRACT TWA FIELD INTO FVIHDR  *         
*        AND FVIFLD.                                                  *         
*        R1=A(OPTIONS VALIDATION TABLE)                               *         
*                                                                     *         
* EXIT - CC=EQUAL IF OPTIONS FIELD IS OK                              *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET                       *         
***********************************************************************         
                                                                                
         USING VOWORKD,RC          RC=A(LOCAL W/S)                              
VALOPT   ST    R1,VOATAB                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   FVILEN,0            TEST ANY INPUT                               
         BE    VALOPT34                                                         
         MVC   PCPARM+08(2),=C',='                                              
         MVC   PCPARM+10(2),PCCHARS                                             
         GOTOR VSCANNER,PCPARM,('VORHSL',FVIHDR),VOAREA                         
         MVC   VOAREAN(1),4(R1)    SAVE NUMBER OF LINES INPUT                   
         CLI   VOAREAN,0           TEST FOR INVALID INPUT                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     VALOPTX                                                          
         MVI   FVINDX,1            SET FIRST FIELD INDEX NUMBER                 
         LA    R3,VOAREA           R3=A(SCANNER TABLE)                          
         B     VALOPT04                                                         
                                                                                
VALOPT02 SR    R1,R1                                                            
         IC    R1,FVINDX           BUMP MULTIPLE FIELD INDEX                    
         AHI   R1,1                                                             
         CLM   R1,1,VOAREAN        TEST ALL OPTIONS PROCESSED                   
         BH    VALOPT34                                                         
         STC   R1,FVINDX                                                        
         AHI   R3,VOWDTH                                                        
                                                                                
VALOPT04 CLI   0(R3),0             TEST VALID KEYWORD LENGTH                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         B     VALOPTX                                                          
         CLI   0(R3),OPTNAMLQ                                                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ILTL)                                            
         B     VALOPTX                                                          
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=KEYWORD LENGTH-1                          
         L     R2,VOATAB           SAVE A(OPTIONS VALIDATION TABLE)             
         USING OPTTABD,R2          R2=A(OPTIONS VALIDATION TABLE)               
                                                                                
VALOPT06 CLI   OPTTABD,EOT         SEARCH TABLE FOR KEYWORD                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOKWD)                                           
         B     VALOPTX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,OPTNAME                                                     
         LA    RE,WORKD(RE)                                                     
         MVC   PCWORK(OPTNAMLQ),0(RE)                                           
         SR    RE,RE                                                            
         ICM   RE,3,OPTSHRT                                                     
         LA    RE,WORKD(RE)                                                     
         MVC   PCWORK+OPTNAMLQ(OPTSHTLQ),0(RE)                                  
VALOPT08 LA    RE,1(R1)                                                         
         CLM   RE,1,OPTMINKL       TEST L'INPUT LESS THAN MIN ALLOWED           
         BL    VALOPT14                                                         
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         BE    VALOPT10                                                         
         CLC   PCWORK(0),VOHDRL(R3) MATCH ON FULL OPTION NAME                   
         CLI   0(R3),OPTSHTLQ      TEST > SHORT KEYWORD                         
         BH    VALOPT14                                                         
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         BNE   VALOPT14                                                         
         CLC   PCWORK+OPTNAMLQ(0),VOHDRL(R3)                                    
VALOPT10 TM    OPTINDS,OPTIDDS     TEST DDS ONLY OPTION                         
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST THIS IS A DDS TERMINAL            
         BZ    VALOPT14                                                         
         MVC   PCWORK(2),OPTAUTH                                                
         NC    PCWORK(2),CUAUTH                                                 
         CLC   PCWORK(2),OPTAUTH                                                
         BNE   VALOPT14                                                         
                                                                                
VALOPT12 CLI   OPTRECB,0           TEST VALID FOR ALL RECORDS                   
         BE    *+14                                                             
         CLC   CSREC,OPTRECB       NO - MATCH ON RECORD NUMBER                  
         BNE   VALOPT14                                                         
         CLI   OPTACTB,0           TEST VALID FOR ALL ACTIONS                   
         BE    *+14                                                             
         CLC   CSACT,OPTACTB       NO - MATCH ON ACTION NUMBER                  
         BNE   VALOPT14                                                         
         CLI   OPTSECN,0           TEST SECURITY NUMBER GIVEN                   
         BE    VALOPT16                                                         
         GOTOR VSECRET,PCPARM,('SECPOPTP',ASECBLK),OPTSECN                      
         BE    VALOPT16                                                         
                                                                                
VALOPT14 AHI   R2,OPTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT06                                                         
                                                                                
VALOPT16 SR    R0,R0               CHECK FOR DUPLICATED OPTION KEYWORD          
         LHI   R1,1                SET LOW ORDER BIT ON                         
         SR    RE,RE                                                            
         IC    RE,OPTOPTN                                                       
         SLDL  R0,0(RE)            SHIFT BY UNIQUE OPTION NUMBER 1-63           
         STM   R0,R1,VODUB1                                                     
         MVC   VODUB2,VOMASK                                                    
         NC    VODUB2,VODUB1       AND NEW BIT POSITION WITH SAVED MASK         
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$DUPOP)                                           
         B     VALOPTX                                                          
         OC    VOMASK,VODUB1       OR NEW BIT POSITION INTO SAVED MASK          
         MVI   4(R3),0             CLEAR SPECIAL CHARACTERS INDICATOR           
         TM    OPTINDS2,OPTLEQ                                                  
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'<'     TEST LESS THAN CHARACTER                     
         BNE   *+8                                                              
         OI    4(R3),OPTLEQ                                                     
         TM    OPTINDS2,OPTGEQ                                                  
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'>'     TEST GREATER THAN CHARACTER                  
         BNE   *+8                                                              
         OI    4(R3),OPTGEQ                                                     
         TM    OPTINDS2,OPTNEQ                                                  
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'*'     TEST NOT EQUALS CHARACTER                    
         BNE   *+8                                                              
         OI    4(R3),OPTNEQ                                                     
         TM    OPTINDS2,OPTPLQ                                                  
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'+'     TEST PLUS CHARACTER                          
         BNE   *+8                                                              
         OI    4(R3),OPTPLQ                                                     
         TM    OPTINDS2,OPTMIQ                                                  
         BZ    *+16                                                             
         CLI   VOLHSL(R3),C'-'     TEST MINUS CHARACTER                         
         BNE   *+8                                                              
         OI    4(R3),OPTMIQ                                                     
         ICM   RE,1,4(R3)          TEST SPECIAL CHARACTER INPUT                 
         BZ    VALOPT18                                                         
         CLI   1(R3),1             TEST DATA INPUT AFTER SPECIAL                
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         B     VALOPTX                                                          
         BASR  RF,0                                                             
         EX    RE,8(RF)                                                         
         BNZ   VALOPT18                                                         
         TM    OPTINDS2,0          TEST SPECIAL CHARACTER ALLOWED               
         MVC   FVMSGNO,=AL2(GE$IDQUL)                                           
         B     VALOPTX                                                          
VALOPT18 SR    RF,RF                                                            
         ICM   RF,1,1(R3)          TEST DATA INPUT AFTER EQUALS SIGN            
         BNZ   VALOPT20                                                         
         CLI   OPTMINDL,0          NO - TEST IF THAT'S OK                       
         BE    VALOPT20                                                         
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         MVI   FVSUBX,1                                                         
         B     VALOPTX                                                          
                                                                                
VALOPT20 MVI   FVIFLD,SPACE                                                     
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         LTR   RF,RF               TEST ZERO LENGTH FIELD                       
         BZ    VALOPT22                                                         
         BCTR  RF,0                                                             
         LA    R1,VOLHSL(R3)                                                    
         TM    4(R3),FF-(OPTMIQ)                                                
         BZ    *+10                                                             
         BCTR  RF,0                DROP FIRST CHARACTER (UNLESS MINUS)          
         AHI   R1,1                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         B     VALOPT22                                                         
         MVC   FVIFLD(0),0(R1)                                                  
VALOPT22 XC    FVIHDR,FVIHDR                                                    
         LA    RE,L'FVIHDR+1(RF)                                                
         STC   RE,FVTLEN                                                        
         LR    R0,RF               R0=LENGTH OF FIELD-1                         
         GOTOR AFVAL,0                                                          
                                                                                
         CLC   FVILEN,OPTMINDL     TEST L'DATA IS VALID                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ILTS)                                            
         B     VALOPTX                                                          
         CLC   FVILEN,OPTMAXDL                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ILTL)                                            
         B     VALOPTX                                                          
                                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,3,OPTIADDR                                                    
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION RTN                
         BZ    *+16                                                             
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         ICM   RF,7,AOVERVAL+1                                                  
         B     VALOPT26                                                         
                                                                                
         TM    OPTINDS,OPTATAB                                                  
         BZ    VALOPT24                                                         
         A     RF,AOVERVAL                                                      
         B     VALOPT28                                                         
                                                                                
VALOPT24 TM    OPTINDS,OPTARTN     TEST A(VALIDATION ROUTINE)                   
         BNZ   *+6                                                              
         DC    H'0'                NO - TABLE IS SCREWY                         
         A     RF,AOVERVAL                                                      
                                                                                
VALOPT26 XC    PCWORK,PCWORK                                                    
         BASR  RE,RF               GO TO ROUTINE                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALOPT32                                                         
         MVI   FVFLAG,X'01'        INDICATE A USER ERROR MESSAGE                
         BNE   VALOPTX                                                          
                                                                                
VALOPT28 SR    R0,R0                                                            
         IC    R0,0(RF)            R0=L'LHS OF TABLE                            
         SR    R1,R1                                                            
         IC    R1,1(RF)            R1=L'RHS OF TABLE                            
         AHI   RF,2                POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'TABLE                                   
         SR    RE,RE                                                            
         IC    RE,FVXLEN           RE=L'DATA-1                                  
VALOPT30 CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IODV)                                            
         B     VALOPTX                                                          
         BASR  R4,0                                                             
         EX    RE,8(R4)                                                         
         B     *+10                                                             
         CLC   FVIFLD(0),0(RF)     MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0               BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT30                                                         
         AR    RF,R0               EXTRACT RHS OF TABLE INTO WORK               
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         BASR  R4,0                                                             
         EX    R1,8(R4)                                                         
         B     VALOPT32                                                         
         MVC   PCWORK(0),0(RF)                                                  
                                                                                
VALOPT32 GOTOR SETOPT              MOVE DATA TO OPTION AREA                     
         B     VALOPT02                                                         
                                                                                
VALOPT34 MVI   FVINDX,0            RESET INDEX/SUB-INDEX                        
         MVI   FVSUBX,0                                                         
         L     R2,VOATAB           APPLY ANY DEFAULT OPTION VALUES              
         LA    R3,VOAREA                                                        
         XC    0(12,R3),0(R3)      TEST ALL REQUIRED OPTIONS WERE INPUT         
                                                                                
VALOPT36 CLI   OPTTABD,EOT         APPLY ANY DEFAULT OPTION VALUES              
         BE    VALOPTX                                                          
         TM    OPTINDS,OPTDFLTO+OPTDFLTI                                        
         BZ    VALOPT42                                                         
         CLI   OPTRECB,0           TEST RECORD/ACTION FILTERS                   
         BE    *+14                                                             
         CLC   CSREC,OPTRECB                                                    
         BNE   VALOPT42                                                         
         CLI   OPTACTB,0           AND ACTION IS VALID                          
         BE    *+14                                                             
         CLC   CSACT,OPTACTB                                                    
         BNE   VALOPT42                                                         
         SR    R0,R0               CHECK FOR OPTION KEYWORD USED                
         LHI   R1,1                SET LOW ORDER BIT ON                         
         SR    RE,RE                                                            
         IC    RE,OPTOPTN                                                       
         SLDL  R0,0(RE)            SHIFT BY UNIQUE OPTION NUMBER 1-63           
         STM   R0,R1,VODUB1                                                     
         MVC   VODUB2,VOMASK                                                    
         NC    VODUB2,VODUB1       AND NEW BIT POSITION WITH SAVED MASK         
         BNZ   VALOPT42                                                         
         TM    OPTINDS,OPTDFLTO    AND THERE IS A DEFAULT VALUE (WHEW)          
         BZ    VALOPT38                                                         
         MVC   PCWORK(L'OPTDVAL),OPTDVAL                                        
         B     VALOPT40                                                         
                                                                                
VALOPT38 MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    FVIHDR,FVIHDR                                                    
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'OPTDVAL),OPTDVAL                                        
         LA    RE,FVIFLD+L'OPTDVAL-1                                            
         BASR  RF,0                                                             
         CLI   0(RE),SPACE                                                      
         BH    *+6                                                              
         BCTR  RE,RF                                                            
         LA    R0,FVIFLD                                                        
         SR    RE,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RF,L'FVIHDR+1(RE)                                                
         STC   RF,FVTLEN                                                        
         LR    R0,RE               R0=LENGTH OF FIELD-1                         
         GOTOR AFVAL,0                                                          
         XC    PCWORK,PCWORK                                                    
         SR    RF,RF               VALIDATE DATA VALUE                          
         ICM   RF,1,OPTIADDR+1                                                  
         TM    OPTINDS,OPTNRTN     TEST NUMBER OF VALIDATION ROUTINE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,24               PASS ROUTINE NUMBER IN HOB                   
         ICM   RF,7,AOVERVAL+1                                                  
         BASR  RE,RF               CALL APPLICATION VALIDATION ROUTINE          
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    *+6                                                              
         DC    H'0'                DIE IF DEFAULT VALUE IS INVALID              
                                                                                
VALOPT40 GOTOR SETOPT              SET DEFAULT OPTION VALUE                     
                                                                                
VALOPT42 AHI   R2,OPTTABL          BUMP TO NEXT TABLE ENTRY                     
         B     VALOPT36                                                         
                                                                                
VALOPTX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
         DROP  RC                                                               
                                                                                
VOWORKD  DSECT                     ** VALOPT S/R LOCAL W/S **                   
VODUB1   DS    D                                                                
VODUB2   DS    D                                                                
VOATAB   DS    A                   A(OPTIONS VALIDATION TABLE)                  
VOMASK   DS    XL8                 OPTION BIT MASK                              
VOMAXN   EQU   20                  MAXIMUM NUMBER OF SCANNER ENTRIES            
VOHDRL   EQU   12                  LENGTH OF HEADER                             
VOLHSL   EQU   VOHDRL+10           LENGTH OF LHS OF ENTRY                       
VORHSL   EQU   30                  LENGTH OF RHS OF ENTRY                       
VOWDTH   EQU   VOLHSL+VORHSL       WIDTH OF SCANNER ENTRY                       
VOAREAN  DS    XL1                 NUMBER OF SCANNER TABLE ENTRIES              
VOAREA   DS    (VOMAXN)XL(VOWDTH)  SCANNER TABLE                                
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MOVE OPTION VALUE FROM WORK INTO OPTION/FORMAT AREA      *         
*                                                                     *         
* NTRY - R2=A(OPTION TABLE ENTRY)                                     *         
*        R3=A(SCANNER BLOCK ENTRY)                                    *         
*        SCWORK=OPTION VALUE TO BE SET                                *         
***********************************************************************         
                                                                                
SETOPT   SR    RF,RF                                                            
         ICM   RF,3,OPTOADDR                                                    
         A     RF,AOVEROUT                                                      
         SR    R1,R1                                                            
         IC    R1,OPTOUTDL         R1=L'DATA (EXCLUDING QUALIFIER)              
         BCTR  R1,0                                                             
         TM    OPTINDS2,OPTGEQ+OPTLEQ+OPTNEQ                                    
         BZ    *+14                                                             
         MVC   0(1,RF),4(R3)       MOVE DATA QUALIFIER                          
         AHI   RF,1                                                             
         TM    OPTINDS,OPTBOOL     TEST IF A BIT VALUE                          
         BZ    SETOPT02                                                         
         BASR  R4,0                                                             
         EX    R1,6(R4)            OR VALUE INTO OUTPUT AREA                    
         BR    RE                                                               
         OC    0(0,RF),PCWORK                                                   
                                                                                
SETOPT02 BASR  R4,0                                                             
         EX    R1,6(R4)            MOVE VALUE TO OUTPUT AREA                    
         BR    RE                                                               
         MVC   0(0,RF),PCWORK                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A TEXT RECORD                                        *         
***********************************************************************         
                                                                                
GETGEN   LA    R1,PCWORK                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,FVMSGNO                                                  
         LA    R0,RLPMSG                                                        
         CLI   FVOMTYP,GTMTXT                                                   
         BNE   *+8                                                              
         LA    R0,PCWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         OI    GT1INDS,GT1OWRK                                                  
         MVI   GTMAXL,L'RLPMSG                                                  
         MVC   GTMTYP,FVOMTYP                                                   
         GOTOR VGETTXT                                                          
                                                                                
GETGENX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* LOOKUP SELECT TABLE FOR RECORD/ACTION AND SET AOVERSEL              *         
*                                                                     *         
* NTRY - R1=A(RECORD/ACTION COMBO)                                    *         
***********************************************************************         
                                                                                
SETSEL   L     R2,ASELTAB          RESOLVE RECORD/ACTION SELECT TABLE           
         SR    RE,RE                                                            
         XC    AOVERSEL,AOVERSEL                                                
         USING SELTABD,R2                                                       
SETSEL02 CLI   SELTABD,EOT         TEST EOT                                     
         JE    EXITE                                                            
         CLC   SELKEY,0(R1)        MATCH ON NEW RECORD/ACTION                   
         BE    *+14                                                             
         ICM   RE,3,SELLEN         NO - BUMP TO NEXT TABLE ENTRY                
         AR    R2,RE                                                            
         B     SETSEL02                                                         
         AHI   R2,SELHEADL         BUMP TO FIRST ENTRY                          
         STCM  R2,15,AOVERSEL      SET A(SUB-TABLE)                             
                                                                                
SETSELX  J     EXITE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET USER-ID CODE INTO PCWORK                             *         
*                                                                     *         
* NTRY - R1=A(USER-ID NUMBER)                                         *         
***********************************************************************         
                                                                                
AU       USING GIDTABD,PCWORK                                                   
GETUID   XC    AU.GIDTABD(GIDLNGL),AU.GIDTABD                                   
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
         CVD   R0,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,X'0F'                                            
         MVI   AU.GIDCODE,C'#'                                                  
         UNPK  AU.GIDCODE+1(L'GIDCODE-1),PCDUB                                  
         MVC   AU.GIDNAME(L'PCQUEST),PCQUEST                                    
         MVC   AU.GIDNAME+L'PCQUEST(L'GIDNAME-L'PCQUEST),AU.GIDNAME             
         OC    AU.GIDNUM,0(R1)                                                  
         BNZ   ALLUID                                                           
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE USER-ID CODE - NUMBER RETURNED IN PCWORK        *         
*                                                                     *         
* NTRY - R1=A(USER-ID CODE)                                           *         
***********************************************************************         
                                                                                
VALUID   XC    AU.GIDTABD(GIDLNGL),AU.GIDTABD                                   
         MVC   AU.GIDNAME(L'PCQUEST),PCQUEST                                    
         MVC   AU.GIDNAME+L'PCQUEST(L'GIDNAME-L'PCQUEST),AU.GIDNAME             
         OC    AU.GIDCODE,0(R1)                                                 
         BNZ   ALLUID                                                           
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO READ USER-ID RECORD AND EXTRACT VALUES FROM IT INTO      *         
* PCWORK (SEE GIDTABD IN GERLPWORK - LONG ENTRY IS CREATED)           *         
***********************************************************************         
                                                                                
         USING AUWORKD,RC                                                       
ALLUID   MVC   AUIOKEY,IOKEY       SAVE CURRENT IOKEY                           
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         OC    AU.GIDNUM,AU.GIDNUM TEST USER-ID NUMBER GIVEN                    
         BZ    *+14                                                             
         MVC   CTIKNUM,AU.GIDNUM                                                
         B     *+10                                                             
         MVC   CTIKID,AU.GIDCODE   NO - MUST BE A CODE THEN                     
         L     R2,AIO3                                                          
                                                                                
         CLC   CTIKEY,IOKEY        TEST RECORD IN IO3 ALREADY                   
         BE    ALLUID02                                                         
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         MVC   IOKEY,AUIOKEY                                                    
         JE    ALLUID02                                                         
         MVC   AU.GIDNAME,=CL33'RECORD NOT FOUND'                               
         J     EXITL                                                            
*                                                                               
ALLUID02 MVC   IOKEY,AUIOKEY                                                    
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTDSCD,R1                                                        
ALLUID04 CLI   CTDSCEL,0           TEST E-O-R                                   
         BE    ALLUIDX                                                          
         CLI   CTDSCEL,CTDSCELQ    TEST DESCRIPTION ELEMENT                     
         BE    ALLUID08                                                         
         CLI   CTDSCEL,CTAGYELQ    TEST AGENCY DEFINITION ELEMENT               
         BE    ALLUID10                                                         
         CLI   CTDSCEL,CTDSTELQ    TEST DESTINATION ID ELEMENT                  
         BE    ALLUID12                                                         
         CLI   CTDSCEL,CTSYSELQ    TEST SYSTEM ELEMENT                          
         BE    ALLUID14                                                         
ALLUID06 IC    R0,CTDSCLEN         BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     ALLUID04                                                         
                                                                                
ALLUID08 OC    AU.GIDNUM,AU.GIDNUM TEST NUMBER SET                              
         BNZ   *+14                                                             
         MVC   AU.GIDNUM,CTDSC     NO - SET NUMBER                              
         B     ALLUID06                                                         
         MVC   AU.GIDCODE,CTDSC    YES - SET CODE                               
         B     ALLUID06                                                         
                                                                                
         USING CTAGYD,R1                                                        
ALLUID10 MVC   AU.GIDALPH,CTAGYID  EXTRACT AGENCY ALPHA ID                      
         B     ALLUID06                                                         
                                                                                
         USING CTDSTD,R1                                                        
ALLUID12 MVC   AU.GIDNAME,CTDSTNAM EXTRACT DESTINATION ID NAME                  
         B     ALLUID06                                                         
                                                                                
         USING CTSYSD,R1                                                        
ALLUID14 LA    RE,AU.GIDSEL        EXTRACT SE NUMBER IN LIST OF SE'S            
         LHI   R0,GIDSELN                                                       
         CLI   0(RE),0             LOOK FOR FREE SLOT                           
         BE    *+16                                                             
         AHI   RE,L'GIDSEL                                                      
         BCT   R0,*-12             TOO MANY SE'S - IGNORE                       
         B     ALLUID06                                                         
         MVC   0(L'GIDSEL,RE),CTSYSSE                                           
         B     ALLUID06                                                         
                                                                                
ALLUIDX  OC    AU.GIDALPH,AU.GIDALPH                                            
         JZ    EXITH                                                            
         J     EXITE                                                            
         DROP  R1,R2,AU                                                         
                                                                                
AUWORKD  DSECT                     ** ALLUID S/R LOCAL W/S **                   
AUIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE A FREQUENCY CODE                                           *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER), IF HOB OF R1 X'80' BIT ON 'ALL EXCEPT'   *         
*        VALIDATION IS PERFORMED                                      *         
* EXIT - CC=LOW IF NO INPUT                                           *         
*        CC=EQUAL IF FOUND - PCWORK(1) CONTAINS FREQUENCY CODE        *         
*           FIELD IS REFRESHED WITH FULL NAME IF NECESSARY            *         
*        CC=HIGH IF INVALID                                           *         
***********************************************************************         
                                                                                
         USING VFWORKD,RC                                                       
VALFRQ   STCM  R1,8,VFINDS                                                      
         LA    R1,0(R1)            CLEAR HOB                                    
         LA    R0,FVIHDR                                                        
         CR    R1,R0               ARE WE POINTING TO FVIHDR                    
         BE    VALFRQ02            YES - AFVAL ALREADY CALLED                   
         MVI   FVMINL,1                                                         
         GOTOR AFVAL,(R1)                                                       
         JNE   EXITL                                                            
                                                                                
VALFRQ02 L     RF,AFRQTAB          RF=A(FREQUENCY TABLE)                        
         USING FRQTABD,RF                                                       
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         MVC   FVMSGNO,=AL2(GE$IFREQ)                                           
                                                                                
         TM    VFINDS,VFIEXCPT     TEST ALL EXCEPT VALID                        
         BZ    VALFRQ04                                                         
         CLC   PCEXCEPT,FVIFLD     TEST PREFIXED WITH ALL ALL EXCEPT            
         BNE   VALFRQ04                                                         
         MVC   FVIFLD(L'FVIFLD-L'PCEXCEPT),FVIFLD+(L'PCEXCEPT)                  
         MVC   FVIFLD+L'FVIFLD-L'PCEXCEPT(L'PCEXCEPT),PCSPACES                  
         OI    VFFLAG,VFFEXCPT                                                  
         SHI   RE,L'PCEXCEPT                                                    
                                                                                
VALFRQ04 CLI   FRQTABD,FRQTEOTQ    TEST END OF TABLE                            
         JE    EXITH                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FRQTNAMU                                                    
         LA    R1,WORKD(R1)        R1=A(UPPER CASE NAME)                        
         EX    RE,*+8                                                           
         BE    VALFRQ08                                                         
         CLC   0(0,R1),FVIFLD      MATCH NAME TO INPUT                          
         SR    R1,R1                                                            
         ICM   R1,3,FRQTNAML                                                    
         LA    R1,WORKD(R1)        R1=A(MIXED CASE NAME)                        
         EX    RE,*+8                                                           
         BE    VALFRQ08                                                         
         CLC   0(0,R1),FVIFLD      MATCH NAME TO INPUT                          
         LTR   RE,RE               TEST ONE BYTE INPUT                          
         BNZ   VALFRQ06                                                         
         CLC   FRQTCODE,FVIFLD     MATCH ON ONE CHARACTER AS WELL               
         BE    VALFRQ08                                                         
VALFRQ06 AHI   RF,FRQTABL          BUMP TO NEXT TABLE ENTRY                     
         B     VALFRQ04                                                         
                                                                                
VALFRQ08 CLI   FRQTCODE,FRQRDAY    TEST RUNDAYS FREQUENCY                       
         BNE   *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST USING A DDS TERMINAL              
         JZ    EXITH               NO - DON'T ALLOW                             
                                                                                
         MVC   PCWORK(L'FRQTCODE),FRQTCODE                                      
         MVC   PCWORK+FRQNAMLQ(L'FRQTIND),FRQTIND                               
         TM    VFFLAG,VFFEXCPT     TEST 'ALL EXCEPT' INPUT                      
         BZ    *+8                                                              
         NI    PCWORK,FF-SPACE     YES - TURN OFF X'40' BIT                     
                                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    R1,R1                                                            
         ICM   R1,3,FRQTNAML                                                    
         LA    R1,WORKD(R1)        R1=A(LOWER CASE NAME)                        
         CLC   0(FRQNAMLQ,R1),FVIFLD                                            
         JE    EXITE                                                            
         L     RF,FVADDR                                                        
         LA    RE,FVIFLD-FVIHDR(RF)                                             
         TM    VFFLAG,VFFEXCPT     TEST 'ALL EXCEPT' INPUT                      
         BZ    *+14                                                             
         MVC   0(L'PCEXCEPT,RE),PCEXCEPT                                        
         AHI   RE,L'PCEXCEPT                                                    
         MVC   0(FRQNAMLQ,RE),0(R1)                                             
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
                                                                                
VALFREQX J     EXITE                                                            
         DROP  RF                                                               
                                                                                
VFWORKD  DSECT                     ** VALFRQ S/R LOCAL W/S **                   
VFINDS   DS    XL1                 INDICATOR BYTE                               
VFIEXCPT EQU   X'80'                                                            
VFFLAG   DS    XL1                 FLAG BYTE                                    
VFFEXCPT EQU   X'80'               ALL EXCEPT INPUT                             
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LOOK UP FREQUENCY NAME IN TABLE                                     *         
*                                                                     *         
* NTRY - R1=A(FREQUENCY CODE), IF HOB BYTE OF R1 HAS X'80' BIT ON THE *         
*        RETURNED FREQUENCY NAME WILL BE IN UPPER CASE                *         
* EXIT - PCWORK=CL10'FREQUENCY NAME',AL1(FREQUENCY INDICATOR)         *         
***********************************************************************         
                                                                                
         USING GFWORKD,RC                                                       
GETFRQ   STCM  R1,8,GFINDS                                                      
         MVC   PCWORK(L'PCQUEST),PCQUEST                                        
         MVC   PCWORK+L'PCQUEST(FRQNAMLQ-L'PCQUEST),PCQUEST                     
                                                                                
         L     RE,AFRQTAB          RE=A(FREQUENCY TABLE)                        
         USING FRQTABD,RE                                                       
GETFRQ02 CLI   FRQTABD,FRQTEOTQ    TEST END OF TABLE                            
         JE    EXITE                                                            
         CLC   FRQTCODE,0(R1)      MATCH INPUT CODE TO TABLE                    
         BE    *+12                                                             
         AHI   RE,FRQTABL                                                       
         B     GETFRQ02                                                         
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,FRQTNAML                                                    
         TM    GFINDS,GFIUCASE                                                  
         BZ    *+8                                                              
         ICM   RF,3,FRQTNAMU                                                    
         LA    RF,WORKD(RF)        RF=A(FREQUENCY NAME)                         
         MVC   PCWORK(FRQNAMLQ),0(RF)                                           
         MVC   PCWORK+FRQNAMLQ(L'FRQTIND),FRQTIND                               
                                                                                
GETFREQX J     EXITE                                                            
         DROP  RE                                                               
                                                                                
GFWORKD  DSECT                     ** GETFRQ S/R LOCAL W/S **                   
GFINDS   DS    XL1                 INDICATOR BYTE                               
GFIUCASE EQU   X'80'               RETURN UPPER CASE NAME                       
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ USER-ID GROUP LIST RECORD AND BUILD (OR ADD TO) A   *         
* LIST OF USER-IDS                                                    *         
*                                                                     *         
* NTRY - R1 POINTS TO A PARAMETER LIST AS FOLLOWS:                    *         
*        P1=A(AGENCY ALPHA ID)                                        *         
*        P2=A(LIST CODE)                                              *         
*        P3=A(OUTPUT AREA) - MAXNUM(1),CURNUM(1),TABLE(GIDTABL)       *         
*                                                                     *         
* EXIT - CC=LOW IF LIST RECORD NOT FOUND                              *         
*        CC=EQUAL IF LIST READ AND ENTRIES FOUND - LIST SORTED INTO   *         
*           ASCENDING USER-ID NUMBER SEQUENCE                         *         
*        CC=HIGH IF LIST FULL                                         *         
***********************************************************************         
                                                                                
         USING VGWORKD,RC                                                       
VALGID   MVC   VGIOKEY,IOKEY       SAVE CURRENT IOKEY                           
         LM    RE,RF,0(R1)         RE=A(AGENCY ALPHA), RF=A(LIST CODE)          
         L     R3,8(R1)            R3=A(OUTPUT TABLE)                           
         LA    R2,IOKEY                                                         
         USING CTWREC,R2           R2=A(LIST RECORD KEY)                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKAGY,0(RE)                                                    
         MVI   CTWKREC,CTWKRIDG                                                 
         MVC   CTWKID,0(RF)                                                     
         GOTOR AIO,'IOREAD+IOCTFILE+IO4'                                        
         MVC   IOKEY,VGIOKEY       RESTORE SAVED KEY VALUE                      
         JNE   EXITL                                                            
                                                                                
         SR    R4,R4                                                            
         IC    R4,1(R3)            RF=CURRENT N'ENTRIES IN TABLE                
         SR    R0,R0                                                            
         IC    R0,0(R3)                                                         
         SR    R0,R4               R0=NUMBER OF FREE SLOTS IN TABLE             
         JNP   EXITH                                                            
         MHI   R4,GIDTABL                                                       
         LA    R4,2(R4,R3)                                                      
         USING GIDTABD,R4          R4=A(NEXT AVAILABLE SLOT)                    
                                                                                
         L     R2,AIO4                                                          
         LA    R2,CTWDATA                                                       
         USING CTLSTD,R2                                                        
         SR    RE,RE                                                            
VALGID02 CLI   CTLSTEL,0           TEST E-O-R                                   
         BE    VALGID06                                                         
         CLI   CTLSTEL,CTLSTELQ    TEST LIST ELEMENT                            
         BNE   VALGID04                                                         
         CLI   CTLSTLEN,CTGIDLNQ   TEST CORRECT LENGTH                          
         BNE   VALGID04                                                         
         GOTOR AGETUID,CTGIDNUM    VALIDATE USER-ID                             
         BNE   VALGID04                                                         
         MVC   GIDTABD(GIDTABL),PCWORK                                          
         IC    RE,1(R3)            INCREMENT N'ENTRIES IN TABLE                 
         AHI   RE,1                                                             
         STC   RE,1(R3)                                                         
         AHI   R0,-1                                                            
         JZ    EXITH               TABLE TOO BIG                                
                                                                                
VALGID04 SR    RE,RE               BUMP TO NEXT LIST ELEMENT                    
         IC    RE,CTLSTLEN                                                      
         AR    R2,RE                                                            
         B     VALGID02                                                         
                                                                                
VALGID06 LA    R1,2(R3)            SORT GIDTAB INTO CODE SEQUENCE               
         SR    R0,R0                                                            
         ICM   R0,1,1(R3)          R0=NUMBER OF TABLE ENTRIES                   
         JZ    EXITL               RETURN CC=LOW IF EMPTY LIST                  
         SH    R0,=H'1'                                                         
         JZ    EXITE               GOOD EXIT IF ONE ENTRY ONLY                  
         BASR  R2,0                                                             
VALGID08 LA    RF,GIDTABL(R1)                                                   
         LR    RE,R0                                                            
         BASR  R3,0                                                             
VALGID10 CLC   GIDNUM-GIDTABD(L'GIDNUM,R1),GIDNUM-GIDTABD(RF)                   
         BNH   *+22                                                             
         XC    0(GIDTABL,R1),0(RF)                                              
         XC    0(GIDTABL,RF),0(R1)                                              
         XC    0(GIDTABL,R1),0(RF)                                              
         AHI   RF,GIDTABL                                                       
         BCTR  RE,R3                                                            
         AHI   R1,GIDTABL                                                       
         BCTR  R0,R2                                                            
                                                                                
VALGIDX  J     EXITE                                                            
         DROP  R2,R4                                                            
                                                                                
VGWORKD  DSECT                     ** VALGUID S/R LOCAL W/S **                  
VGIOKEY  DS    XL(L'IOKEY)         SAVED IOKEY VALUE                            
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DESTINATION FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(DESTINATION TWA FIELD HEADER)                           *         
*        HOB OF R1 CONTAINS X'80' IF A SPOOF REQUEST - IF ON ALLOW    *         
*        INPUT OF A FAX ID IN THE DESTINATION FIELD (FX=FAXID)        *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE PCWORK(2) CONTAINS ID NUMBER      *         
*           PCWORK+2(7) OPTIONALLY CONTAINS FAX ID IF INPUT           *         
***********************************************************************         
                                                                                
         USING VDWORKD,RC                                                       
VALDST   STCM  R1,8,VDMODE         SET CALLING MODE                             
         LA    R1,0(R1)                                                         
         ST    R1,VDSVFLDA         SET A(INPUT FIELD)                           
         MVC   VDIOKEY,IOKEY       SAVE CURRENT IOKEY VALUE                     
         XC    PCWORK(L'CUUSER+L'CTFXCODE),PCWORK                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
                                                                                
         LA    R1,IOKEY                                                         
         USING CTIKEY,R1           BUILD ID RECORD KEY                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         LA    R0,VDREC                                                         
         ST    R0,IOADDR                                                        
         GOTOR AIO,'IORD+IOCTFILE'                                              
         BE    *+6                                                              
         DC    H'0'                CAN'T READ USER ID RECORD                    
         MVC   IOKEY,VDIOKEY       RESTORE SAVED KEY VALUE                      
                                                                                
         L     R1,VDSVFLDA                                                      
         LA    R0,FVIHDR                                                        
         CR    R1,R0               TEST R1 POINTING TO FVIHDR                   
         BE    VALDST04                                                         
         MVI   FVMINL,1                                                         
         GOTOR AFVAL               NO - CALL FVAL TO VALIDATE FIELD             
         BNE   VALDSTX                                                          
                                                                                
VALDST04 TM    VDMODE,VDMSPOOF     TEST VALIDATING FOR SPOOF REQUEST            
         BZ    *+14                                                             
         CLC   PCUFAXP,FVIFLD      YES - TEST FAX DESTINATION                   
         BE    VALDST10                                                         
                                                                                
         GOTOR VGETIDS,VDPARM,(C'M',IOADDR),0,(C'A',VDMGR),FVIFLD,0             
         CLI   12(R1),0                                                         
         BE    VALDSTE1                                                         
         L     R1,4(R1)            R1=A(LIST OF VALID DESTINATIONS)             
         CLC   FVIFLD(8),0(R1)                                                  
         JNE   *+2                                                              
         MVC   PCWORK(L'CTIKNUM),L'CTIKID(R1)                                   
         B     VALDSTX                                                          
                                                                                
VALDST10 LA    R1,VDREC+(CTIDATA-CTIREC)                                        
         SR    R0,R0                                                            
         USING CTDSCD,R1                                                        
VALDST12 CLI   CTDSCEL,0           LOCATE USER-ID CODE ELEMENT                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         B     VALDST12                                                         
                                                                                
         MVC   VDUSER,CTDSC        EXTRACT USER-ID CODE                         
                                                                                
         LA    R1,IOKEY                                                         
         USING EDIKEYD,R1          READ FAX AUTHORIZATION RECORD                
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ                                                 
         MVI   EDITYPE,EDITYPEQ                                                 
         MVC   EDINAME,VDUSER                                                   
         GOTOR AIO,'IOREAD+IOCTFILE'                                            
         MVC   IOKEY,VDIOKEY       RESTORE SAVED KEY VALUE                      
         BNE   VALDSTE2                                                         
                                                                                
         LA    R1,IOKEY                                                         
         USING CTFXREC,R1          READ FAX RECORD                              
         XC    CTFXKEY,CTFXKEY                                                  
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,CUAALF                                                   
         MVC   CTFXCODE,FVIFLD+3                                                
         GOTOR AIO,'IOREAD+IOCTFILE'                                            
         MVC   IOKEY,VDIOKEY                                                    
         BNE   VALDSTE3                                                         
         MVC   PCWORK(L'CUUSER),CUUSER                                          
         MVC   PCWORK+L'CUUSER(L'CTFXCODE),FVIFLD+3                             
                                                                                
VALDSTX  CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         J     EXITE                                                            
                                                                                
VALDSTE1 MVC   FVMSGNO,=AL2(GE$IDEST)                                           
         J     EXITH                                                            
                                                                                
VALDSTE2 MVC   FVMSGNO,=AL2(GE$NATSF)                                           
         J     EXITH                                                            
                                                                                
VALDSTE3 MVC   FVMSGNO,=AL2(GE$FAXNF)                                           
         J     EXITH                                                            
         DROP  R1                                                               
                                                                                
VDWORKD  DSECT                     ** VALDEST S/R LOCAL W/S **                  
VDSVFLDA DS    A                   SAVED INPUT FIELD ADDRESS                    
VDIOKEY  DS    XL(L'IOKEY)         SAVE AREA FOR IOKEY                          
VDPARM   DS    6F                  GETIDS PARAMETER LIST                        
VDUSER   DS    CL(L'EDINAME)       USER-ID                                      
VDMODE   DS    X                   ** CALLING MODE **                           
VDMSPOOF EQU   X'80'               SPOOF REQUEST                                
VDREC    DS    XL2000              OUTPUT TYPE RECORD I/O AREA                  
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE OUTPUT TYPE FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(OUTPUT TYPE TWA FIELD HEADER)                           *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE PCWORK CONTAINS OUTPUT TYPE       *         
***********************************************************************         
                                                                                
         USING VOTWORKD,RC                                                      
VALOUT   LA    R0,FVIHDR                                                        
         CR    R1,R0               TEST R1 POINTING TO FVIHDR                   
         BE    VALOUT02                                                         
         MVI   FVMINL,1                                                         
         GOTOR AFVAL               NO - CALL FVAL TO VALIDATE INPUT             
         JNE   EXITL                                                            
                                                                                
VALOUT02 MVC   PCWORK(L'CTOKID),FVIFLD                                          
         CLI   FVIFLD,C'@'         TEST FIELD STARTS WITH @ SIGN                
         JE    EXITE               YES - ALLOW IT                               
                                                                                
         MVC   VOTIOKEY,IOKEY      SAVE CURRENT IOKEY VALUE                     
         LA    R2,IOKEY                                                         
         USING CTOREC,R2                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKTYP,CTOKTYPQ                                                 
         MVC   CTOKID,FVIFLD                                                    
         GOTOR AIO,'IOREAD+IOCTFILE+IO4'                                        
         MVC   IOKEY,VOTIOKEY      RESTORE SAVED KEY VALUE                      
         BNE   VALOUTER                                                         
         TM    CUSTAT,CUSDDS       DDS CAN REQUEST ANY OUTPUT TYPE              
         JNZ   EXITE                                                            
                                                                                
         L     R2,AIO4             LOOK FOR OUTPUT DETAIL ELEMENT               
         LA    R2,CTODATA                                                       
         USING CTOUTD,R2                                                        
         SR    R0,R0                                                            
VALOUT04 CLI   CTOUTEL,0           TEST E-O-R                                   
         BE    VALOUTER                                                         
         CLI   CTOUTEL,CTOUTELQ    TEST OUTPUT DETAIL ELEMENT                   
         BE    VALOUTX                                                          
         IC    R0,CTOUTLEN                                                      
         AR    R2,R0                                                            
         B     VALOUT04                                                         
                                                                                
VALOUTX  TM    CTOUTSTA,X'80'      TEST VALID TO BE REQUESTED                   
         JNZ   EXITE                                                            
                                                                                
VALOUTER MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         J     EXITH                                                            
         DROP  R2                                                               
                                                                                
VOTWORKD DSECT                     ** VALOUT S/R LOCAL W/S **                   
VOTIOKEY DS    XL(L'IOKEY)         SAVE AREA FOR IOKEY                          
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST SECURITY FOR A FIELD                                           *         
*                                                                     *         
* NTRY - R1=A(FIELD SECURITY NUMBER)                                  *         
* EXIT - CC=EQUAL IF FIELD VALID FOR READ AND WRITE                   *         
*        CC=HIGH IF FIELD VALID FOR READ ONLY                         *         
*        CC=LOW IF FIELD INVALID FOR READ AND WRITE                   *         
***********************************************************************         
                                                                                
FLDSEC   CLI   0(R1),0                                                          
         JE    EXITE                                                            
         TM    CUSTAT,CUSPER                                                    
         JZ    EXITE                                                            
         LR    RF,R1                                                            
         GOTOR VSECRET,PCPARM,('SECPFLDP',ASECBLK),(RF)                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY A SCREEN INTO TWA                                *         
*                                                                     *         
* NTRY - P1=AL1(SCREEN OVERLAY NUMBER),AL3(LOAD POINT)                *         
***********************************************************************         
                                                                                
OVRSCR   ICM   R1,15,0(R1)         SET A(LOAD POINT)                            
         STCM  R1,8,PCBYTE1                                                     
         XC    PCPARM(16),PCPARM                                                
         STCM  R1,7,PCPARM+1       SET LOAD POINT                               
         MVI   PCPARM+4,C'R'                                                    
         MVC   PCPARM+5(2),PCOVSYS                                              
         MVC   PCPARM+7(1),PCBYTE1 SET SCREEN OVERLAY NUMBER                    
         GOTOR VCOLY,PCPARM                                                     
         GOTOR CHKOLY              TEST SCREEN LOADED OK                        
         JNE   EXITH                                                            
         LA    R1,RLPMSGH                                                       
         LA    RF,TWALAST                                                       
         SR    RE,RE                                                            
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                SCREEN TOO LARGE                             
         CLC   TWASCRN,PCBYTE1     TEST SCREEN SAME AS LAST                     
         BNE   *+10                                                             
         XC    1(2,R1),1(R1)       YES - DON'T SET BEFORE/AFTER                 
         MVC   TWASCRN,PCBYTE1                                                  
         MVI   TWASCRF,0                                                        
                                                                                
OVRSCRX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOAD APPLICATION OVERLAY                                 *         
*                                                                     *         
* NTRY - R1=A(PHASE NUMBER TO BE LOADED)                              *         
***********************************************************************         
                                                                                
OVRPHS   CLI   0(R1),0             ENSURE OVERLAY NUMBER IS RESOLVED            
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CSOVER,0(R1)                                                     
         GOTOR VCOLY,PCPARM,(CSOVER,0),0,0                                      
         GOTOR CHKOLY              TEST OVERLAY LOADED OK                       
         JNE   EXITH                                                            
         MVC   PCNTRYA,0(R1)       SET OVERLAY ADDRESS                          
                                                                                
OVRPHSX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IF OVERLAY IS LOADED OK AND BUILD ERROR MESSAGE    *         
*                                                                     *         
* NTRY - R1=A(CALL OVERLAY PARAMETER LIST)                            *         
* EXIT - CC=EQUAL IF LOADED OK, CC=NOT EQUAL IF NOT OK AND ERROR SET  *         
***********************************************************************         
                                                                                
CHKOLY   CLI   4(R1),FF            TEST PHASE LOADED OK                         
         BE    *+8                                                              
         CR    RE,RE                                                            
         BR    RE                                                               
         MVC   FVMSGNO,=AL2(GE$CLOCD)                                           
         LTR   RE,RE                                                            
                                                                                
CHKOLYX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT AND PRE-VALIDATE A TWA INPUT FIELD               *         
*                                                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
*        FVMINL=MINIMUM INPUT FIELD LENGTH (ZERO=OPTIONAL FIELD)      *         
*        FVMAXL=MAXIMUM INPUT FIELD LENGTH (ZERO=MAXIMUM LENGTH)      *         
*        FVXTRA=NARRATIVE TO BE ATTACHED TO ERROR IF FIELD IS INVALID *         
*                                                                     *         
* EXIT - FVADDR=A(TWA FIELD HEADER)                                   *         
*        FVINDX=ZERO                                                  *         
*        FVSUBX=ZERO                                                  *         
*        FVMINL=ZERO                                                  *         
*        FVMAXL=ZERO                                                  *         
*        FVXTRA=SPACES                                                *         
*        FVIHDR=EXTRACTED INPUT FIELD HEADER (SEE FVIHDR IN WORKD)    *         
*        FVIFLD=EXTRACTED & SPACE FILLED INPUT FIELD                  *         
*        FVMSGNO=SET TO STANDARD ERROR NUMBER (SEE FVMSGNO EQUATES)   *         
*        CC=LOW IF FIELD IS NOT INPUT                                 *         
*        CC=EQUAL IF FIELD IS INPUT AND VALID                         *         
*        CC=HIGH IF INPUT TOO SHORT/LONG ETC.                         *         
***********************************************************************         
                                                                                
FVAL     LTR   R1,R1               TEST A(TWA FIELD HEADER) PASSED              
         BNZ   *+10                                                             
         LR    RF,R0               SET FIELD LENGTH IN RF                       
         B     FVAL04                                                           
         ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVI   FVOMTYP,0           RESET MESSAGE TYPE                           
         MVI   FVIFLD,SPACE                                                     
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
                                                                                
         MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         XC    FVIXHDR,FVIXHDR                                                  
         SR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         LHI   R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    FVAL02                                                           
         LA    RE,1(R1,RF)                                                      
         SR    RE,R0                                                            
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
         LHI   R0,L'FVIHDR+L'FVIHDR+1                                           
                                                                                
FVAL02   SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         BASR  RE,0                                                             
         EX    RF,8(RE)            EXTRACT FIELD DATA                           
         B     FVAL04                                                           
         MVC   FVIFLD(0),L'FVIHDR(R1)                                           
                                                                                
FVAL04   LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         AHI   RF,1                RF=LOOP COUNT                                
         BASR  R2,0                                                             
         CLI   0(R1),SPACE         LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FVAL08                                                           
         MVI   0(R1),SPACE         SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCTR  RF,R2                                                            
FVAL08   STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(GE$ILTS)                                            
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         CLM   RF,1,FVMINL                                                      
         BL    FVAL20                                                           
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(GE$ILTL)                                            
         CLM   RF,1,FVMAXL                                                      
         BH    FVAL20                                                           
         NI    FVIIND,FF-FVINUM-FVIALF-FVIHEX                                   
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     FVAL18                                                           
                                                                                
         MVC   FVMSGNO,=AL2(FVFOK) INDICATE FIELD IS OK                         
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BASR  RE,0                                                             
         TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FVAL14                                                           
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,FF-FVINUM-FVIALF-FVIHEX                                   
         B     FVAL12                                                           
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,FF-FVIALF                                                 
         B     FVAL12                                                           
         NI    FVIIND,FF-FVINUM                                                 
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,FF-FVIHEX                                                 
FVAL12   BCTR  R1,0                                                             
         BCTR  RF,RE                                                            
FVAL14   IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         TM    FVIIND,FVINUM                                                    
         BZ    FVAL16                                                           
         CLI   FVILEN,8            TEST INPUT NOT LONGER THAN 8 BYTES           
         BNH   *+12                                                             
         NI    FVIIND,FF-FVINUM                                                 
         B     FVAL16                                                           
         BASR  RE,0                                                             
         EX    RF,8(RE)            SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  PCDUB,FVIFLD(0)                                                  
         CVB   R0,PCDUB                                                         
         ST    R0,PCFULL                                                        
                                                                                
FVAL16   MVC   FVMSGNO,=AL2(FVFOK)                                              
                                                                                
FVAL18   MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVXTRA,SPACE                                                     
         MVC   FVXTRA+1(L'FVXTRA-1),FVXTRA                                      
                                                                                
FVAL20   CLC   FVMSGNO,=AL2(GE$MIF)                                             
         JE    EXITH                                                            
         CLI   FVILEN,0                                                         
         JE    EXITL                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         JE    EXITE                                                            
         J     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO ANY STANDARD SYSTEM FILE                 *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES (LOW ORDER 2 BYTES) SET FROM IO EQUATES *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSAV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)           *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSAV *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
                                                                                
         USING IOWORKD,RC                                                       
IO       ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOFLAG,0            RESET I/O FLAG BYTE                          
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IOCTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         TM    IOCTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
                                                                                
         LHI   R1,IO1+IO2          ESTABLISH I/O AREA ADDRESS (IO1-IO3)         
         N     R1,IOCTRL                                                        
         BZ    IO02                                                             
         SRL   R1,6                R1=I/O AREA NUMBER                           
         B     IO04                                                             
                                                                                
IO02     TM    IOCTRL+2,X'F0'      ESTABLISH I/O AREA ADDRESS (IO4-IOA)         
         BZ    IO08                                                             
         IC    R1,IOCTRL+2                                                      
         SRL   R1,4                                                             
                                                                                
IO04     STC   R1,IODUB                                                         
         CLI   IODUB,10            ONLY 10 I/O AREAS SUPPORTED HERE             
         BH    IO06                                                             
         BCTR  R1,0                                                             
         MHI   R1,IOAREALN                                                      
         AHI   R1,IOAREA1-WORKD                                                 
         LA    R1,WORKD(R1)                                                     
         ST    R1,IOAREAD          SAVE I/O AREA ADDRESS                        
                                                                                
         AHI   R1,L'IODA+L'IOWORK                                               
         STCM  R1,15,IOADDR        SET REAL I/O ADDRESS                         
         B     IO08                                                             
                                                                                
IO06     CLI   IODUB,11            SPECIAL FOR IOREC                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOREC                                                        
         ST    R1,IOADDR           I/O AREA REAL ADDRESS                        
                                                                                
         SHI   R1,L'IODA+L'IOWORK                                               
         STCM  R1,15,IOAREAD       I/O AREA WORK AREA                           
                                                                                
IO08     LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IO10                                                             
         OC    IOFILE,IOFILE       CALLER MUST SUPPLY FILE NAME                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IO38                                                             
                                                                                
IO10     SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILNTRY         POINT TO LOCAL SYSTEM FILES                  
         LHI   R0,10                                                            
         CR    R1,R0                                                            
         BNH   *+8                                                              
         L     RE,ASYSTAB          POINT TO GLOBAL SYSTEM FILES                 
                                                                                
         USING FILTABD,RE                                                       
IO12     CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         AHI   RE,FILTABL                                                       
         B     IO12                                                             
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IOFILNM,IOFILNM     TEST NATIVE SYSTEM FILE                      
         BZ    IO14                                                             
         LA    R1,PCSWSYSN                                                      
         GOTOR IOSWITCH                                                         
         BNE   IO40                                                             
         B     IO20                                                             
                                                                                
IO14     MVC   IOSWSYS(L'IOSWSYS+L'IOSWFIL),FILSYSN                             
         L     RE,AFILTAB                                                       
         SR    R1,R1                                                            
IO16     CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,RE),IOSWSYS     MATCH ON SYSTEM SWITCH NUMBER                
         BE    *+16                                                             
         ICM   R1,3,4(RE)                                                       
         LA    RE,5(R1,RE)                                                      
         B     IO16                                                             
         MVC   IOSWSYSN,1(RE)      SAVE SWITCH-TO SYSTEM NAME                   
         AHI   RE,6                POINT TO FIRST FILE ENTRY                    
IO18     CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLC   FILNUM,IOSWFIL      MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         AHI   RE,FILTABL                                                       
         B     IO18                                                             
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IOFILNM,IOFILNM     TEST NATIVE FILE TO THIS SYSTEM              
         BNZ   *+6                                                              
         DC    H'0'                NO - KILL THE APPLICATION                    
         LA    R1,IOSWSYS                                                       
         GOTOR IOSWITCH                                                         
         BE    IO20                                                             
         LA    R1,PCSWSYSN                                                      
         GOTOR IOSW                CAN'T SWITCH - SWITCH BACK TO NATIVE         
         MVI   IOERR,FF            SET SWITCH FAILURE ERROR BITS                
         B     IO42                                                             
                                                                                
IO20     L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IO22                                                             
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IO38                                                             
                                                                                
IO22     CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)                                                   
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         AHI   RE,4                                                             
         B     IO24                                                             
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IO22                                                             
                                                                                
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
IO24     CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         AHI   RE,CMDTABL                                                       
         B     IO24                                                             
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
                                                                                
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IO32                                                             
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IO34                                                             
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IO26                                                             
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         XC    IODAOVER,IODAOVER                                                
         B     IO32                                                             
                                                                                
IO26     ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    IO30                                                             
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    0(L'IODA,R1),0(R1)                                               
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         AHI   R1,L'IODA                                                        
                                                                                
IO28     OC    0(L'IOWORK,R1),0(R1)                                             
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
         AHI   R1,L'IOWORK                                                      
                                                                                
IO30     OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IO32                                                             
                                                                                
         TM    IOFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         SR    R1,R1                                                            
         IC    R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTOR AIO                 RECURSE FOR DIRECTORY I/O                    
         BE    IO32                SUCCESSFUL I/O                               
         BL    IO40                EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IO40                                                             
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IO40                                                             
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
                                                                                
IO32     ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO I/O AREA ADDRESS                          
         MVC   IOFILE,IOFILNM      SET FILE NAME IN WORK AREA                   
         GOTOR VDMGR,PCPARM,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK              
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IO40                                                             
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IO40                                                             
         MVC   0(L'IODA,R1),IODA                                                
         AHI   R1,L'IODA           YES - BUMP BY D/A LENGTH                     
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IO40                EXIT TO CALLER                               
                                                                                
IO34     TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IO38                                                             
         MVC   IOFILE,IOFILNM      SET FILE NAME IN WORK AREA                   
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,FILIID      TEST I/S FILE HAS D/A ATTACHED               
         BZ    *+12                YES - MUST READ INTO IOAREA                  
         TM    IOFILI,FILIVL                                                    
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDMGR,PCPARM,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IO40                                                             
         TM    IOFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IO36                                                             
         SR    R1,R1                                                            
         IC    R1,IOFILKL          YES - EXTRACT DISK ADDRESS                   
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)        POINT TO DISK ADDRESS                        
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IO40                                                             
         MVC   0(L'IODA,R1),IODA                                                
         B     IO40                                                             
                                                                                
IO36     ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IO40                                                             
         AHI   R1,L'IODA           YES - BUMP BY D/A LENGTH                     
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IO40                                                             
                                                                                
IO38     ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDMGR,PCPARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IO40                                                             
                                                                                
IO40     TM    IOINDS1,IOISWAUT    TEST AUTO SWITCH BACK AFTER I/O              
         BZ    IO42                                                             
         TM    IOFLAG,IOFSWTCH     TEST SYSTEM SWITCH OCCURRED                  
         BZ    IO42                                                             
         LA    R1,PCSWSYSP                                                      
         GOTOR IOSWITCH            SWICTH TO PREVIOUS SYSTEM                    
                                                                                
IO42     MVI   IOQ,1               SET I/O COMPLETED OK                         
         TM    IOERR,IOERRS                                                     
         BZ    IOX                                                              
         MVI   IOQ,2               SET LOGICAL I/O ERROR                        
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   IOX                                                              
         MVI   IOQ,0               SET IRRECOVERABLE ERROR                      
                                                                                
IOX      CLI   IOQ,1               SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SWITCH TO A SYSTEM                                       *         
*                                                                     *         
* NTRY - R1=A(LOGICAL SYSTEM NUMBER)                                  *         
* EXIT - CC=LOW   - USER NOT AUTHORISED FOR SYSTEM                    *         
*        CC=EQUAL - SWITCH SUCCESSFUL                                 *         
*        CC=HIGH  - SYSTEM NOT AVAILABLE (ONLINE ONLY)                *         
* NOTE - IF ERROR OCCURRS IOERR IS SET TO X'FF' WHICH WILL RETURN A   *         
*        CC OF HIGH FROM I/O ROUTINE WITH FVMSGNO SET TO FVFIOER. IT  *         
*        IS THE CALLER'S RESPONSIBILITY TO DEAL WITH THIS OTHERWISE   *         
*        A RANDOM DATAMGR ERROR WILL BE REPORTED.                     *         
***********************************************************************         
                                                                                
IOSWITCH CLC   PCSWSYSC,0(R1)      TEST SWITCHED TO CORRECT SYSTEM              
         BER   RE                                                               
IOSW     LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   IOBYTE,0(R1)        SAVE SYSTEM NUMBER                           
         CLI   0(R1),10            TEST SWITCH TO CONTROL SYSTEM                
         BE    IOSW04                                                           
         L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LHI   RF,SYSSWMAX                                                      
         BASR  R2,0                                                             
IOSW02   CLC   SYSSWSOV,IOBYTE     MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   *+12                                                             
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         B     IOSW04                                                           
         AHI   RE,SYSSWLEN         BUMP TO NEXT SWITCH TABLE ENTRY              
         BCTR  RF,R2                                                            
         MVI   IOBYTE,0            SET CC=LOW FOR INVALID SYSTEM                
         B     IOSW12                                                           
                                                                                
IOSW04   CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   IOSW06                                                           
         ICM   RF,15,PCAUTL        YES - MOVE SE NUMBER TO UTL                  
         MVC   TSYS-UTLD(,RF),0(R1)                                             
         B     IOSW08                                                           
                                                                                
IOSW06   MVC   PCPARM(1),0(R1)     SWITCH TO A SYSTEM                           
         MVC   PCPARM+1(3),PCEFFS                                               
         XC    PCPARM+4(4),PCPARM+4                                             
         GOTOR VSWITCH,PCPARM                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    IOSW08                                                           
         MVI   IOBYTE,2            SET CC=HIGH FOR CAN'T SWITCH                 
         B     IOSW12                                                           
                                                                                
IOSW08   MVC   PCSWSYSP,PCSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   PCSWSYSC,IOBYTE     SAVE CURRENT SYSTEM NUMBER                   
         OI    IOFLAG,IOFSWTCH     SET SYSTEM SWITCH OCCURRED                   
                                                                                
IOSW10   MVI   IOBYTE,1            SET CC=EQUAL FOR OK                          
                                                                                
IOSW12   CLI   IOBYTE,1            SET CC FOR CALLER                            
         BE    IOSWX                                                            
         MVI   IOERR,FF            SET ALL ERROR BITS ON                        
                                                                                
IOSWX    LR    RE,R0                                                            
         BR    RE                  RETURN TO CALLER                             
         DROP  RC,RE                                                            
                                                                                
IOWORKD  DSECT                     ** IO S/R LOCAL W/S **                       
IODUB    DS    D                   GENERAL WORK AREA                            
IOAREAD  DS    A                   I/O AREA ADDRESS                             
IOCTRL   DS    F                   I/O COMMAND WORD                             
IOBYTE   DS    X                   I/O BYTE                                     
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILNM  DS    CL7                 FILE NAME                                    
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOSWSYS  DS    XL1                 SWITCH SYSTEM NUMBER                         
IOSWFIL  DS    XL1                 SWITCH FILE NUMBER                           
IOSWSYSN DS    CL3                 SWITCH SYSTEM NAME                           
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE CURRENT SESSION TO NEXT SESSION SAVE AREA                      *         
*                                                                     *         
* NTRY - TWASESNL=CURRENT LEVEL OF NESTING                            *         
*      - R1=A(SESSION PARAMETERS)                                     *         
* EXIT - TWASESNL=NEXT LEVEL                                          *         
***********************************************************************         
                                                                                
NTRSES   LR    R3,R1               SAVE SESSION PARAMETERS                      
         USING SELTPARM,R3         R3=A(NTRSES PARAMETERS)                      
         MVI   PCBYTE2,1           SET DIFFERENT RECORD TYPE FLAG               
         CLC   CSREC,SELTREC       TEST CHANGE OF RECORD                        
         BNE   *+8                                                              
         MVI   PCBYTE2,0           NO - SET SAME RECORD TYPE FLAG               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,TWASESNL                                                      
         AHI   RE,1                INCREMENT NEXT LEVEL                         
         CHI   RE,TWASESMX                                                      
         BNH   *+6                                                              
         DC    H'0'                MAXIMUM NEST LEVEL EXCEEDED                  
         STC   RE,TWASESNL                                                      
         AHI   RE,1                                                             
         SRDL  RE,1                                                             
         STC   RE,PCBYTE1          SAVE ABSOLUTE TEMPSTR PAGE NUMBER            
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   0(L'TWASESRA,RE),CSRECACT                                        
         OC    CSINITRA,CSINITRA                                                
         BZ    *+10                                                             
         MVC   0(L'TWASESRA,RE),CSINITRA                                        
                                                                                
         XC    PCHALF,PCHALF                                                    
         LTR   RF,RF                                                            
         BNZ   NTRSES02                                                         
         L     R0,ATIA             CLEAR TEMPSTR PAGE                           
         LHI   R1,TWAMAXRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     NTRSES04                                                         
                                                                                
NTRSES02 LHI   RF,TWAMAXRL/2                                                    
         STCM  RF,3,PCHALF                                                      
         LHI   RF,TWAMAXRL                                                      
         ICM   RF,12,=C'L='                                                     
         GOTOR VDMGR,PCPARM,DMREAD,TEMPSTR,(PCBYTE1,0),ATIA,,(RF)               
         BE    NTRSES04                                                         
         DC    H'0'                                                             
                                                                                
NTRSES04 L     R2,ATIA             SAVE START ADDRESS INTO TIA                  
         AH    R2,PCHALF                                                        
         USING SESD,R2             R2=A(SESSION SAVE AREA)                      
         MVI   SESINDS,0                                                        
         TM    SELTINDS,SELTICLR   SET CLEAR OSVALS                             
         BNZ   *+12                                                             
         CLI   PCBYTE2,0           TEST NEW RECORD TYPE                         
         BE    *+8                                                              
         OI    SESINDS,SESISAVE    YES - OSAVLS MUST BE SAVED/RESTORED          
                                                                                
         MVI   SESROUT,0                                                        
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    *+10                                                             
         MVC   SESROUT,SELTRTN     SET EXIT ROUTINE NUMBER                      
                                                                                
         MVC   SESSCRN,TWASCRN     SAVE GLOBAL VALUES                           
         MVC   SESSCRF,TWASCRF                                                  
                                                                                
         LHI   RE,PFKEYS-TWAD      SAVE PFKEY LIST                              
         LA    RE,TWAD(RE)                                                      
         MVC   SESPFKS,0(RE)                                                    
         MVC   SESLRA,TWALRA                                                    
         MVC   SESPFKD,TWAPFKD                                                  
         MVC   SESPFKN,TWAPFKN                                                  
         MVI   SESPFKI,0                                                        
         TM    TWAINDS3,TWAISPFK                                                
         BZ    *+8                                                              
         OI    SESPFKI,TWAISPFK                                                 
         NI    TWAINDS3,FF-(TWAISPFK)                                           
                                                                                
         LA    R0,SESCSSV          SAVE CURRENT SESSION VALUES                  
         LHI   R1,SESCSSVL                                                      
         LA    RE,CSVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,SESTWSV          SAVE ENTIRE SCREEN                           
         LHI   R1,SESTWSVL                                                      
         LA    RE,RLPOPTH                                                       
         LHI   RF,TWALAST-RLPOPTH                                               
         MVCL  R0,RE                                                            
                                                                                
         XC    CSINITRA,CSINITRA   CLEAR INITIATOR RECORD/ACTION                
         MVI   CSLTINDS,0          CLEAR LIST INDICATORS                        
         SR    RF,RF               SAVE CURRENT SESSION LSTTAB ENTRY            
         ICM   RF,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    NTRSES06                                                         
         MHI   RF,LSTTABL                                                       
         LA    RF,PCLSTCUR-LSTTABL(RF)                                          
         MVC   0(LSTTABL,RF),CSLSTCUR                                           
                                                                                
NTRSES06 TM    SESINDS,SESISAVE    TEST SAVING OSVALS TOO                       
         BZ    NTRSES08                                                         
         LA    R0,SESOSSV          SAVE CURRENT OVERLAY SAVE VALUES             
         LHI   R1,SESOSSVL                                                      
         LA    RE,TWAD                                                          
         AHI   RE,OSVALS-TWAD                                                   
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,TWAD                                                          
         AHI   R0,OSVALS-TWAD      AND CLEAR THEM FOR NEW RECORD                
         LHI   R1,OSVALSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NTRSES08 GOTOR VDMGR,PCPARM,DMWRITE,TEMPSTR,(PCBYTE1,0),ATIA                    
                                                                                
         OC    RLPOPT,RLPOPT       TEST ANYTHING IN OPTIONS FIELD               
         BZ    *+14                                                             
         XC    RLPOPT,RLPOPT                                                    
         OI    RLPOPTH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         MVC   CSPSRECN,CSHIRECN   SET NEXT SESSION LOW TSAR RECORD#            
                                                                                
         LTR   R3,R3               TEST SESSION PARAMETERS PASSED               
         BZ    NTRSESX                                                          
         MVC   CSOIND1,SELTNSI1    SET NEXT SESSION INDICATORS                  
         MVC   CSOIND2,SELTNSI2                                                 
         CLI   SELTREC,0           TEST RECORD/ACTION PASSED                    
         BNE   *+12                                                             
         CLI   SELTACT,0                                                        
         BE    NTRSESX                                                          
                                                                                
         L     R0,AOVERWRK         CLEAR OVERWRK                                
         LHI   R1,OVERWRKL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR ARECACT,SELTREC     SET ENTRY RECORD/ACTION NAMES                
                                                                                
NTRSES10 CLI   SELTNXPF,0          TEST AUTO RETURN PFKEY SET                   
         BE    *+10                                                             
         MVC   CSNEXTPF,SELTNXPF   YES - SET VALUE                              
         OI    TWAINDS1,TWAINTRS   SET NTRSES ISSUED                            
         XC    CSINDSL,CSINDSL                                                  
         MVI   CSINDSL1,CSIUSELC                                                
         XC    CSINDSG,CSINDSG                                                  
         L     RD,PCSVRD                                                        
         L     RD,8(RD)                                                         
                                                                                
NTRSESX  J     EXITE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RESTORE PREVIOUS SESSION                                            *         
*                                                                     *         
* NTRY - TWASESNL=CURRENT NESTING LEVEL                               *         
* EXIT - TWASESNL=PREVIOUS NESTING LEVEL                              *         
***********************************************************************         
                                                                                
         USING XSWORKD,RC                                                       
XITSES   MVC   XSNXRECN,CSHIRECN                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,TWASESNL                                                      
         BCTR  RE,0                DECREMENT NEXT LEVEL                         
         LTR   RE,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                MINIMUM NEST LEVEL EXCEEDED                  
         STC   RE,TWASESNL                                                      
         AHI   RE,2                                                             
         SRDL  RE,1                                                             
         STC   RE,PCBYTE1          SAVE ABSOLUTE TEMPSTR PAGE NUMBER            
         L     R2,ATIA                                                          
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         AHI   R2,TWAMAXRL/2                                                    
         USING SESD,R2             R2=A(SESSION SAVE AREA)                      
                                                                                
         LHI   RF,TWAMAXRL                                                      
         ICM   RF,12,=C'L='        READ SAVED TEMPSTR PAGE                      
         GOTOR VDMGR,PCPARM,DMREAD,TEMPSTR,(PCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    RF,RF               SAVE CURRENT SESSION LSTTAB ENTRY            
         ICM   RF,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    XITSES02                                                         
         MHI   RF,LSTTABL                                                       
         LA    RF,PCLSTCUR-LSTTABL(RF)                                          
         MVC   0(LSTTABL,RF),CSLSTCUR                                           
                                                                                
XITSES02 LA    R0,CSVALS           RESTORE SESSION VALUES                       
         LHI   R1,SESCSSVL                                                      
         LA    RE,SESCSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    SESINDS,SESISAVE    TEST OSVALS WERE SAVED ON NTRSES             
         BZ    XITSES04            NO - DON'T RESTORE THEM                      
         LA    R0,TWAD                                                          
         AHI   R0,OSVALS-TWAD                                                   
         LHI   R1,SESOSSVL                                                      
         LA    RE,SESOSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
XITSES04 MVC   CSNXRECN,XSNXRECN   SET NEXT SESSION HIGH RECORD                 
                                                                                
         MVC   TWASCRN,SESSCRN     RESTORE GLOBAL VALUES                        
         MVC   TWASCRF,SESSCRF                                                  
                                                                                
         LHI   RE,PFKEYS-TWAD      RESTORE PFKEY LIST                           
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'PFKEYS,RE),SESPFKS                                           
         MVC   TWALRA,SESLRA                                                    
         MVC   TWAPFKD,SESPFKD                                                  
         MVC   TWAPFKN,SESPFKN                                                  
         NI    TWAINDS3,FF-(TWAISPFK)                                           
         TM    SESPFKI,TWAISPFK    TEST PFKEY SCROLLING ALLOWED                 
         BZ    *+8                                                              
         OI    TWAINDS3,TWAISPFK   YES - SET PFKEY SCROLLING ON                 
                                                                                
         L     RF,AINP                                                          
         USING TIOBD,RF                                                         
         MVC   TIOBCNT(1),SESSCRN  SET SCREEN NUMBER IN TIO BLOCK               
         OI    TIOBINDS,TIOBSCRN                                                
         DROP  RF                                                               
                                                                                
         LA    R0,RLPOPTH          RESTORE TWA                                  
         LHI   R1,TWALAST-RLPOPTH                                               
         LA    RE,SESTWSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,RLPACTH          POSITION CURSOR TO ACTION FIELD              
         ST    R0,FVADDR                                                        
         LA    R0,RLPOLY1H         R0=A(OVERLAY SCREEN START)                   
         LA    R1,RLPMSGH          TRANSMIT SCREEN & TURN OFF CURSORS           
         LA    RF,TWALAST-1                                                     
         SR    RE,RE                                                            
XITSES06 CLI   FVTLEN-FVIHDR(R1),0                                              
         BE    XITSES10                                                         
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         NI    FVOIND-FVIHDR(R1),FF-FVOCUR                                      
         CR    R1,R0               TEST INTO THE OVERLAY SCREEN AREA            
         BL    XITSES08                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   *+8                                                              
         NI    FVIIND-FVIHDR(R1),FF-FVIVAL                                      
         C     R0,FVADDR           TEST A(OVERLAY SCREEN FIELD) SET             
         BNH   XITSES08                                                         
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BNZ   XITSES08                                                         
         ST    R1,FVADDR           A(FIRST UNPROT OVERLAY SCREEN FIELD)         
XITSES08 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+8                                                              
         BXLE  R1,RE,XITSES06                                                   
XITSES10 MVI   1(R1),1             SET CLEAR BEFORE AND AFTER                   
         MVI   2(R1),1                                                          
                                                                                
XITSES12 MVI   PCBYTE1,0                                                        
         CLI   SESROUT,0           TEST RETURN POINT GIVEN                      
         BE    XITSES16                                                         
         GOTOR ARECACT,CSREC                                                    
         OI    TWAINDS1,TWAIXITS   SET XITSES ISSUED                            
         MVC   PCBYTE1,SESROUT     RETURN ROUTINE NUMBER                        
                                                                                
XITSES14 L     RD,PCSVRD                                                        
         L     RD,8(RD)                                                         
         B     XITSESX                                                          
                                                                                
XITSES16 MVC   FVMSGNO,=AL2(GI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
                                                                                
XITSESX  J     EXITE                                                            
         DROP  R2                                                               
                                                                                
XSWORKD  DSECT                     ** XITSES S/R LOCAL W/S **                   
XSNXRECN DS    XL2                 SAVED CSHIRECN TO SET CSNXRECN               
XSWORKX  EQU   *                                                                
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SET NEW RECORD/ACTION WORDS                                         *         
*                                                                     *         
* NTRY - R1=AL1(RECORD NUMBER,ACTION NUMBER)                          *         
***********************************************************************         
                                                                                
RECACT   GOTOR ATSTMIX             TEST VALID RECORD/ACTION COMBO               
         JNE   EXITH                                                            
         GOTOR ASETSEL             SET A(SELECT TABLE)                          
         MVC   CSREC(L'CSREC+L'CSACT),0(R1)                                     
         L     RE,AMIXNTRY                                                      
         MVC   CSQRTN,MIXQRTN-MIXTABD(RE)                                       
         MVC   CSOVER,MIXOVER-MIXTABD(RE)                                       
                                                                                
         L     RE,ARECTAB                                                       
         USING RECTABD,RE                                                       
RECACT02 CLI   RECTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RECNUMB,CSREC                                                    
         BE    *+12                                                             
         AHI   RE,RECTABL                                                       
         B     RECACT02                                                         
         SR    RF,RF                                                            
         ICM   RF,3,RECNAMEL                                                    
         LA    RF,WORKD(RF)                                                     
         MVC   RLPREC,0(RF)        OUTPUT RECORD WORD AND TRANSMIT              
         OI    RLPRECH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RLPRECH+(FVIIND-FVIHDR),FVIVAL                                   
                                                                                
         L     RE,AACTTAB                                                       
         USING ACTTABD,RE                                                       
RECACT04 CLI   ACTTABD,EOT         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTNUMB,CSACT                                                    
         BE    *+12                                                             
         AHI   RE,ACTTABL                                                       
         B     RECACT04                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ACTNAMEL                                                    
         LA    RF,WORKD(RF)                                                     
         MVC   RLPACT,0(RF)        OUTPUT ACTION WORD AND TRANSMIT              
         OI    RLPACTH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RLPACTH+(FVIIND-FVIHDR),FVIVAL                                   
                                                                                
RECACTX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR                                      *         
*                                                                     *         
* NTRY - R1=TSAR ACTION VALUE (TSAR WILL BE INITIALISED IF NECESSARY) *         
*     OR R1=A(TSAR ACTION VALUE,A(RECORD))                            *         
***********************************************************************         
                                                                                
         USING TIWORKD,RC                                                       
TSARIO   ST    R1,TIPARM           SAVE ACTION OR A(PARAMETER)                  
         MVC   TITSACTN,TIPARM+3   SAVE CALLER'S ACTION                         
         USING LSTTABD,R2          R2=A(CURRENT LIST ENTRY)                     
         LA    R2,CSLSTCUR                                                      
         OC    TIPARM(3),TIPARM    TEST ACTION,A(RECORD) PASSED                 
         BZ    *+14                                                             
         MVC   TITSACTN,0(R1)                                                   
         ICM   R2,7,1(R1)                                                       
         L     R3,ATSABLK                                                       
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
         TM    PCTSINDS,PCTSIRES   TEST ALREADY RESTORED                        
         BNZ   TSARIO04                                                         
         LA    R0,LSTTABD                                                       
         ST    R0,TSAREC           SET A(RECORD)                                
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,L'LSTTRECN   SET KEY LENGTH                               
         LHI   R0,LSTTABL                                                       
         STCM  R0,3,TSRECL         SET RECORD LENGTH                            
         MVI   TSPAGN,TSPEXPN      SET NUMBER OF TEMPEST PAGES                  
         MVI   TSACTN,TSAINI       SET INITIALISE                               
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         CLI   TITSACTN,TSASAV     TEST SAVE                                    
         BE    TSARIOX                                                          
         TM    PCTSINDS,PCTSIINI   TEST TEMPEST BUFFER INITIALISED              
         BZ    TSARIO02                                                         
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVC   TSPAGL,PCTSLOWP     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,PCTSNUMP     SET NUMBER OF PAGES ALLOCATED                
                                                                                
TSARIO02 GOTOR VTSAR,TSARD         CALL TO INITIALISE/RESTORE                   
         BNE   TSARIOAB            ABEND                                        
         MVC   PCTSLOWP,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   PCTSNUMP,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         OI    PCTSINDS,PCTSIINI+PCTSIRES                                       
                                                                                
TSARIO04 MVC   TSACTN,TITSACTN     SET ACTION NUMBER                            
         CLI   TSACTN,TSAINI       TEST EXPLICIT INITIALISE                     
         BE    TSARIOX                                                          
         CLI   TSACTN,TSARES       TEST EXPLICIT RESTORE                        
         BE    TSARIOX                                                          
         CLI   TSACTN,TSASAV       TEST SAVE                                    
         BNE   *+12                                                             
         NI    PCTSINDS,FF-PCTSIRES                                             
         B     TSARIO08                                                         
         MVC   TSRNUM,LSTTRECN     SET RECORD NUMBER (FOR TSAGET)               
         CLI   TSACTN,TSAADD       TEST ADD/PUT/WRITE                           
         BE    TSARIO06                                                         
         CLI   TSACTN,TSAPUT                                                    
         BE    TSARIO06                                                         
         CLI   TSACTN,TSAWRT                                                    
         BNE   TSARIO08                                                         
                                                                                
TSARIO06 CLC   CSHIRECN,TSRNUM     SET HIGH RECORD NUMBER IF REQUIRED           
         BNL   *+10                                                             
         MVC   CSHIRECN,TSRNUM                                                  
         MVI   TSACTN,TSAPUT       SET TO PUT RECORD                            
         CLC   TSRNUM,PCTSHIGH     TEST NEW RECORD                              
         BNH   TSARIO08                                                         
         MVC   PCTSHIGH,TSRNUM     SET HIGH RECORD NUMBER - GLOBAL              
         MVI   TSACTN,TSAADD       SET TO ADD RECORD                            
                                                                                
TSARIO08 GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PCTSHIGH,TSPRECN    CHECK HIGH NUMBERS AGREE                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
TSARIOX  J     EXITE                                                            
                                                                                
TSARIOAB CLI   TSACTN,TSARES       TEST RESTORE                                 
         BNE   *+6                                                              
         DC    H'0'                YES - DIE ON ERROR                           
         LA    R1,RLPACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$TSFCT)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,PCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
         DROP  R2,R3                                                            
                                                                                
TIWORKD  DSECT                     ** TSARIO S/R LOCAL W/S **                   
TIPARM   DS    A                   CALLING PARAMETER                            
TITSACTN DS    XL1                 SAVED TSAR ACTION NUMBER                     
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IF USER-ID IS AN RLP PRINCIPAL ID                  *         
*                                                                     *         
* NTRY - R1=A(2 BYTE BINARY USER-ID NUMBER)                           *         
* EXIT - CC=EQUAL IF PRINCIPAL, NOT EQUAL IF NOT                      *         
***********************************************************************         
                                                                                
         USING TPWORKD,RC                                                       
TSTPID   MVC   PCHALF,0(R1)        SAVE USER-ID NUMBER                          
         MVC   TPIOKEY,IOKEY       SAVE CURRENT IOKEY VALUE                     
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           READ AGENCY ACCESS RECORD                    
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,TPIOKEY       RESTORE SAVED IOKEY VALUE                    
         L     R2,AIO3                                                          
                                                                                
         LA    R1,CT5DATA                                                       
         USING CTRPID,R1                                                        
         SR    R0,R0                                                            
TSTPID02 CLI   CTRPIEL,0           TEST E-O-R                                   
         JE    EXITH                                                            
         CLI   CTRPIEL,CTRPIELQ    TEST RPI ELEMENT                             
         BE    TSTPIDX                                                          
         IC    R0,CTRPILEN                                                      
         AR    R1,R0                                                            
         B     TSTPID02                                                         
                                                                                
TSTPIDX  CLC   CTRPINUM,PCHALF     MATCH USER-ID TO PRINCIPAL                   
         JE    EXITE                                                            
         J     EXITH                                                            
         DROP  R1,R2                                                            
                                                                                
TPWORKD  DSECT                     ** TSTPID S/R LOCAL W/S **                   
TPIOKEY  DS    XL(L'IOKEY)                                                      
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RFP BLOCK VALUES                                         *         
***********************************************************************         
                                                                                
INIRFP   L     R2,ARFPIOB                                                       
         USING RFPD,R2                                                          
         MVC   RFPACOMF,ACOM                                                    
         MVC   RFPAMIN,AMINBUF                                                  
         MVC   RFPFUID,CUUSER                                                   
         MVC   RFPFAGY,CUAALF                                                   
         MVC   RFPFSYS,CUSYSL                                                   
         MVI   RFPINIT,0                                                        
         GOTOR VRFPIO,PCPARM,RFPD  CALL RFPIO TO INITIALISE                     
                                                                                
INIRFPX  J     EXITE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN AGENCY ALPHA ID                              *         
*                                                                     *         
* NTRY - R1=A(2 BYTE AGENCY ALPHA ID)                                 *         
* EXIT - CC=EQUAL IF VALID, NOT EQUAL IF INVALID                      *         
***********************************************************************         
                                                                                
         USING VAWORKD,RC                                                       
VALAGY   MVC   VAIOKEY,IOKEY       SAVE CURRENT IOKEY VALUE                     
         LA    R2,IOKEY                                                         
         USING CT5REC,R2           READ AGENCY ACCESS RECORD                    
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R1)                                                   
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         MVC   IOKEY,VAIOKEY                                                    
                                                                                
VALAGYX  JNE   EXITH                                                            
         J     EXITE                                                            
         DROP  R2                                                               
                                                                                
VAWORKD  DSECT                     ** VALAGY S/R LOCAL W/S **                   
VAIOKEY  DS    XL(L'IOKEY)                                                      
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE TO SOFDAT                                      *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST AS FOLLOWS):-                            *         
*        P1/B0  =SOFDAT INPUT TYPE (SOFITYPE)                         *         
*        P1/B1-3=A(INPUT AREA)                                        *         
*        P2/B0  =SOFDAT OUTPUT TYPE (SOFOTYPE)                        *         
*        P2/B1-3=A(OUTPUT AREA)                                       *         
*        P3/B0  =SOFDAT INPUT INDICATORS (SOFIINDS)                   *         
*        P3/B1-3=A(MAXIMUM PERIOD TYPE/VALUE) OR ZERO                 *         
* EXIT - CC=SET BY SOFDAT                                             *         
***********************************************************************         
                                                                                
         USING GSWORKD,RC                                                       
GOSOFT   LR    RE,R1               RE=A(PARAMETER LIST)                         
         LA    R1,GSSOFBLK         R1=A(SOFDAT BLOCK)                           
         ST    R1,12(RE)           RETURN A(SOFDATD TO CALLER)                  
         USING SOFDATD,R1                                                       
         MVC   SOFITYPE,0(RE)                                                   
         MVC   SOFAINP+1(3),1(RE)                                               
         MVC   SOFOTYPE,4(RE)                                                   
         MVC   SOFAOUT+1(3),5(RE)                                               
         MVC   SOFIINDS,8(RE)                                                   
         ICM   RF,7,9(RE)          GET A(MAXIMUM VALUE)                         
         BZ    *+10                                                             
         MVC   SOFMAXPT(L'SOFMAXPT+L'SOFMAXPV),0(RF)                            
                                                                                
         MVC   GSBYTE,SOFITYPE                                                  
         NI    GSBYTE,DIGIT                                                     
         CLI   GSBYTE,SOFITYM      TEST VALIDATION CALL                         
         BE    *+12                                                             
         CLI   GSBYTE,SOFITYMD                                                  
         BNE   GOSOFT02                                                         
         LA    R0,FVIHDR                                                        
         C     R0,SOFAINP          TEST A(INPUT) IF FVIFLD                      
         BE    GOSOFT02                                                         
         MVC   FVADDR,SOFAINP      NO - POINT FVADDR TO INPUT FIELD             
                                                                                
GOSOFT02 MVC   SOFTODAY,ASEDAT                                                  
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,CONNUMQ                                                  
         MVC   SOFLANG,CULANG                                                   
         OI    SOFITYPE,SOFITXTN   SET EXTENSION BLOCK PASSED                   
         MVC   SOFABUFF,ASOFBUFF   SET A(OPTIMISATION BUFFER)                   
         GOTOR VSOFDAT,(R1)                                                     
         BE    GOSOFT04                                                         
         MVC   FVMSGNO,SOFERROR    SET MESSAGE NUMBER ON ERROR                  
         MVI   FVOSYS,CONNUMQ                                                   
         J     EXIT                                                             
                                                                                
GOSOFT04 TM    SOFOTYPE,SOFOTMIX   TEST MIXED CASE OUTPUT SET                   
         BZ    GOSOFT06                                                         
         ICM   RF,15,SOFAINP       POINT TO INPUT FIELD                         
         CLI   GSBYTE,SOFITYM      TEST IF A VAIDATION CALL                     
         BE    *+8                                                              
         CLI   GSBYTE,SOFITYMD                                                  
         BE    *+8                                                              
         ICM   RF,15,SOFAOUT       NO - POINT TO OUTPUT FIELD                   
         TM    FVATRB-FVIHDR(RF),FVAPROT                                        
         BNZ   *+8                                                              
         OI    FVATRB-FVIHDR(RF),FVAMODF                                        
                                                                                
GOSOFT06 CLI   SOFMAXPT,0          TEST MAXIMUM PERIOD VALUE SET                
         JE    EXITE                                                            
         OC    SOFOVDIP,SOFOVDIP   TEST PERIOD SET                              
         JNZ   EXITE                                                            
         MVC   FVMSGNO,=AL2(GE$MIF)                                             
         J     EXITH                                                            
         DROP  R1,RC                                                            
                                                                                
GSWORKD  DSECT                     ** GOSOFT S/R LOCAL W/S **                   
GSBYTE   DS    XL(L'SOFITYPE)                                                   
GSSOFBLK DS    XL(SOFXTNL)         SOFDAT BLOCK                                 
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN RFPGROUP USING RFPIO                         *         
*                                                                     *         
* NTRY - R1=AL1(OPTIONS),AL3(GROUP CODE)                              *         
* EXIT - CC=EQUAL IF GROUP FOUND, HIGH IF NOT FOUND, LOW IF DELETED   *         
***********************************************************************         
                                                                                
         USING VGRWORKD,RC                                                      
VALGRP   STCM  R1,8,VGRWFLAG       SAVE CALLING FLAGS                           
         MVC   VGRWGRP,0(R1)       EXTRACT GROUP CODE                           
         L     R2,ARFPIOB          R2=A(RFP BLOCK)                              
         USING RFPD,R2                                                          
                                                                                
         MVC   VGRSFREQ,RFPFRQID   SAVE REQUEST KEY                             
         MVC   VGRSSORT,RFPFSORT                                                
         MVC   VGRSSEQN,RFPFSEQN                                                
                                                                                
         CLI   RFPINIT,0           TEST RFP INITIALISED                         
         BNE   VALGRP02                                                         
         GOTOR AINIRFP             NO - CALL INITIALISATION ROUTINE             
                                                                                
VALGRP02 XC    RFPFFREQ(RFPMODE-RFPFFREQ),RFPFFREQ                              
         MVC   RFPFGRP,VGRWGRP                                                  
         MVC   RFPFUID,CUUSER                                                   
         MVC   RFPFAGY,CUAALF                                                   
         MVC   RFPFSYS,CUSYSL                                                   
         MVI   RFPMODE,RFPVALGP                                                 
         MVC   RFPFLAGS,VGRWFLAG   SET FLAG BYTE                                
         OI    RFPFLAGS,RFPXSYMS                                                
         CLI   VGRWFLAG,0          SET DEFAULT IF NOTHING PASSED                
         BNE   *+8                                                              
         MVI   RFPFLAGS,RFPXRDUP+RFPNOSYM+RFPNOREQ+RFPXSYMS                     
         NI    RFPFLAGS,FF-(RFPRDDEL)                                           
                                                                                
         GOTOR VRFPIO,PCPARM,RFPD  CALL RFP TO CHECK GROUP                      
                                                                                
         MVI   RFPFLAGS,0                                                       
         CLI   RFPERROR,RFPNOGRP   TEST GROUP EXISTS                            
         JE    EXITH                                                            
                                                                                
         CLI   RFPERROR,RFPNOERR   TEST OTHER ERRORS                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   RFPFRQID,VGRSFREQ   RESTORE SAVED BLOCK VALUES                   
         MVC   RFPFSORT,VGRSSORT                                                
         MVC   RFPFSEQN,VGRSSEQN                                                
                                                                                
         TM    RFPVGSTA,RFPVGSDQ   TEST GROUP IS DELETED                        
         JZ    EXITE                                                            
                                                                                
         MVC   FVMSGNO,=AL2(GE$RID)                                             
         J     EXITL               YES - EXIT WITH CC LOW AND MESSAGE           
         DROP  R2,RC                                                            
                                                                                
VGRWORKD DSECT                     ** VALGRP S/R LOCAL W/S **                   
VGRWFLAG DS    XL1                 CALLING FLAGS                                
VGRWGRP  DS    CL(L'RFPFGRP)       GROUP CODE                                   
VGRSFREQ DS    XL(L'RFPFRQID)      SAVED FREQUENCY                              
VGRSSORT DS    CL(L'RFPFSORT)      SAVED SORT                                   
VGRSSEQN DS    XL(L'RFPFSEQN)      SAVED SEQUENCE NUMBER                        
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* MOVE RFPV VALUES INTO XFILE GROUP RECORD                            *         
* NTRY - AIO1 POINTS TO AN XFILE GROUP RECORD                         *         
* EXIT - RECORD IS UPDATED WITH NEW VALUES                            *         
***********************************************************************         
                                                                                
RFPXFG   L     R2,ARFPIOB                                                       
         USING RFPD,R2                                                          
         L     R1,AIO1                                                          
         AHI   R1,XFFRSTEL-XFILED                                               
         USING XFPHD,R1            R1=A(FIRST ELEMENT ON XFILE RECORD)          
         SR    R0,R0                                                            
RFPXFG02 CLI   XFPHCD,XFPHCDQ      TEST RECORD HEADER ELEMENT                   
         BE    RFPXFG06                                                         
         CLI   XFPHCD,XFDRCDEQ     TEST DATES ELEMENT                           
         BE    RFPXFG08                                                         
RFPXFG04 IC    R0,XFPHLN           BUMP TO NEXT ELEENT ON RECORD                
         AR    R1,R0                                                            
         CLI   XFPHCD,0            TEST END OF RECORD                           
         BNE   RFPXFG02                                                         
         B     RFPXFGX                                                          
                                                                                
RFPXFG06 MVC   XFPHFREQ,RFPVFREQ                                                
         MVC   XFPHNXTR,RFPVNXTR                                                
         MVC   XFPHEND,RFPVENDR                                                 
         B     RFPXFG04                                                         
                                                                                
         USING XFDRD,R1                                                         
RFPXFG08 MVC   XFDRDAY,RFPVDAYS                                                 
         MVC   XFDRTS,RFPVRNGE                                                  
         MVC   XFDRST,RFPVNXTR                                                  
         MVC   XFDREN,RFPVENDR                                                  
         B     RFPXFG04                                                         
                                                                                
RFPXFGX  J     EXITE                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REQUEST RUN OPTION                              *         
*                                                                     *         
* NTRY - R1=A(INPUT FIELD)                                            *         
* EXIT - CC=EQUAL IF VALID INPUT - PCWORK CONTAINS OPTION VALUE -     *         
*           FIELD IS RE-DISPLAYED IF FULL OPTION WORD NOT INPUT       *         
*        CC=LOW IF NO INPUT - PCWORK DEFAULTS TO 'YES' OPTION         *         
*        CC=HIGH IF INPUT IS INVALID                                  *         
***********************************************************************         
                                                                                
VALRUN   MVI   PCWORK,SPACE        SET OUTPUT OPTION TO 'YES'                   
         LA    R0,FVIHDR                                                        
         CR    R1,R0               TEST POINTING TO FVIHDR                      
         BE    VALRUN02                                                         
         MVI   FVMINL,1                                                         
         GOTOR AFVAL,(R1)          NO - CALL FVAL TO VALIDATE INPUT             
         JNE   EXITL                                                            
         SR    R0,R0                                                            
                                                                                
VALRUN02 LA    RE,PCMYES                                                        
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    VALRUN04                                                         
         CLC   FVIFLD(0),0(RE)                                                  
         EX    RF,*+8                                                           
         BE    VALRUN04                                                         
         CLC   FVIFLD(0),PCUYES                                                 
         MVI   PCWORK,REQLRSRD                                                  
         LA    RE,PCMNO                                                         
         EX    RF,*+8                                                           
         BE    VALRUN04                                                         
         CLC   FVIFLD(0),0(RE)                                                  
         EX    RF,*+8                                                           
         BE    VALRUN04                                                         
         CLC   FVIFLD(0),PCUNO                                                  
         MVC   FVMSGNO,=AL2(GE$IRUN)                                            
         J     EXITH                                                            
                                                                                
VALRUN04 LTR   R0,R0               TEST POINTING TO FVIHDR                      
         JNZ   EXITE                                                            
         CLC   FVIFLD(L'PCUNO),0(RE)                                            
         JE    EXITE                                                            
         L     RF,FVADDR                                                        
         MVC   L'FVIHDR(L'PCUNO,RF),0(RE)                                       
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
                                                                                
VALRUNX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT RUN DATE FOR A GROUP                            *         
*                                                                     *         
* NTRY - INITIALISED RFP BLOCK                                        *         
* EXIT - CC=EQUAL IF NEXT RUN DATE SET (PCWORK(4) CONTAINS RUN DATE   *         
*           IN JULIAN FORMAT) ,CC=NOT EQUAL IF NO NEXT RUN DATE       *         
*           (PCWORK(4) SET TO BINARY ZEROES)                          *         
***********************************************************************         
                                                                                
         USING GNWORKD,RC                                                       
GETNXT   L     R2,ARFPIOB          R2=A(RFP BLOCK)                              
         USING RFPD,R2                                                          
                                                                                
         OC    GNPSDJ,RFPVNXTR     EXTRACT AND TEST NEXT RUN DATE               
         BZ    GETNXTN                                                          
         MVC   GNCDDJ,RFPVNXTR                                                  
         OC    RFPVENDR,RFPVENDR   TEST END RUN DATE SET                        
         BNZ   GETNXT02                                                         
         CLC   GNCDDJ,ASJDAT       NEXT RUN AFTER TODAY                         
         BNL   GETNXTY                                                          
         B     GETNXTN                                                          
                                                                                
GETNXT02 CLC   ASJDAT,RFPVENDR     TEST TODAY AFTER END DATE                    
         BH    GETNXTN                                                          
         MVC   GNCDDJ,ASJDAT       START FROM TODAY                             
         CLC   GNCDDJ,GNPSDJ       TEST TODAY LOWER THAN PERIOD START           
         BNL   *+10                                                             
         ZAP   GNCDDJ,GNPSDJ       YES - START FROM PERIOD START DATE           
         ZAP   GNPEDJ,RFPVENDR                                                  
         CLC   GNCDDJY,GNPEDJY     TEST START/END IN SAME YEAR                  
         BE    GETNXT04                                                         
         MVC   GNPEDJY,GNPSDJY                                                  
         ZAP   GNPEDJD,MAXDAYS                                                  
                                                                                
GETNXT04 CP    GNCDDJ,GNPEDJ       TEST EXCEEDED PERIOD END                     
         BH    GETNXT06                                                         
         ZAP   GNDUB,GNCDDJD       EXTRACT DAY NUMBER                           
         CVB   RE,GNDUB                                                         
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)     RE=A(BYTE FOR THIS DAY)                      
         IC    RF,DAYBITS(RF)      RF=BIT VALUE FOR THIS DAY                    
         EX    RF,*+8              TEST BIT ON IN RUN MASK                      
         BNZ   GETNXTY             YES - FOUND THE NEXT RUN DATE                
         TM    0(RE),0                                                          
         AP    GNCDDJD,PONE        NO - ADVANCE TO NEXT DAY                     
         B     GETNXT04                                                         
                                                                                
GETNXT06 CLC   GNCDDJY,RFPVENDR    TEST BOTH YEARS SEARCHED                     
         BE    GETNXTN                                                          
         MVC   GNPSDJY,RFPVENDR    NO - SEARCH SECOND YEAR                      
         ZAP   GNPSDJD,PONE        START AT END YEAR/DAY 001                    
         MVC   GNPEDJ,RFPVENDR     END AT END YEAR/END DAY                      
         MVC   GNCDDJ,GNPSDJ       SEARCH FROM END YEAR/DAY 001                 
         B     GETNXT04                                                         
                                                                                
GETNXTY  MVC   PCWORK(L'GNCDDJ),GNCDDJ                                          
         OI    PCWORK+(L'RFPVNXTR-1),DIGIT                                      
         J     EXITE               EXIT WITH CC=EQUAL                           
                                                                                
GETNXTN  XC    PCWORK(L'GNCDDJ),PCWORK                                          
         J     EXITH               EXIT WITH CC=NOT EQUAL                       
         DROP  R2,RC                                                            
                                                                                
GNWORKD  DSECT                     ** GETNXT S/R LOCAL W/S **                   
GNDUB    DS    D                                                                
                                                                                
GNPSDJ   DS    0XL(L'RFPVNXTR)     PERIOD START DATE                            
GNPSDJY  DS    XL2                                                              
GNPSDJD  DS    PL2                                                              
                                                                                
GNPEDJ   DS    0XL(L'RFPVNXTR)     PERIOD END DATE                              
GNPEDJY  DS    XL2                                                              
GNPEDJD  DS    PL2                                                              
                                                                                
GNCDDJ   DS    0XL(L'RFPVNXTR)     CURRENT SEARCH DATE                          
GNCDDJY  DS    XL2                                                              
GNCDDJD  DS    PL2                                                              
                                                                                
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET LAST RUN DATE FOR A GROUP                            *         
*                                                                     *         
* NTRY - INITIALISED RFP BLOCK                                        *         
* EXIT - CC=EQUAL IF LAST RUN DATE SET (PCWORK(4) CONTAINS RUN DATE   *         
*           IN JULIAN FORMAT) ,CC=NOT EQUAL IF NO LAST RUN DATE       *         
*           (PCWORK(4) SET TO BINARY ZEROES)                          *         
***********************************************************************         
                                                                                
         USING GLWORKD,RC                                                       
GETLST   L     R2,ARFPIOB          R2=A(RFP BLOCK)                              
         USING RFPD,R2                                                          
                                                                                
         MVC   GLLDDJ,RFPVLSTR     EXTRACT LAST RUN DATE                        
         OC    RFPVNXTR,RFPVNXTR   TEST NEXT RUN DATE SPECIFIED                 
         BZ    GETLSTX                                                          
         CLC   ASJDAT,RFPVNXTR     TEST NEXT RUN DATE IN THE FUTURE             
         BNH   GETLSTX                                                          
         MVC   GLLDDJ,RFPVNXTR                                                  
         OC    RFPVENDR,RFPVENDR   TEST END RUN DATE SET                        
         BZ    GETLSTX                                                          
                                                                                
         MVC   GLPSDJ,RFPVNXTR     START AT FIRST RUN DATE                      
         MVC   GLPEDJ,ASJDAT                                                    
         SP    GLPEDJD,PONE        AND END AT YESTERDAY                         
         CLC   RFPVENDR,ASJDAT     TEST END RUN DATE IN THE PAST                
         BNL   GETLST02                                                         
         MVC   GLPEDJ,RFPVENDR     YES - SET AS END DATE                        
                                                                                
GETLST02 MVC   GLLDDJ,GLPEDJ       SET DATE FROM END DATE                       
                                                                                
GETLST04 CP    GLLDDJ,GLPSDJ       TEST DATE LOWER THAN START DATE              
         BL    GETLSTN                                                          
         ZAP   GLDUB,GLLDDJD       EXTRACT DAY NUMBER                           
         CVB   RE,GLDUB                                                         
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)     RE=A(BYTE FOR THIS DAY)                      
         IC    RF,DAYBITS(RF)      RF=BIT VALUE FOR THIS DAY                    
         EX    RF,*+8              TEST BIT ON IN RUN MASK                      
         BNZ   GETLSTX             YES - FOUND THE LAST RUN DATE                
         TM    0(RE),0                                                          
         SP    GLLDDJD,PONE        NO - BACK UP TO PREVIOUS DAY                 
         BNZ   GETLST04                                                         
         CLC   GLPSDJY,GLPEDJY     TEST START/END IN SAME YEAR                  
         BE    GETLSTN                                                          
         MVC   GLLDDJY,GLPSDJY     NO - SEARCH FIRST YEAR                       
         ZAP   GLLDDJD,MAXDAYS     FROM THE LAST DAY                            
         B     GETLST04                                                         
                                                                                
GETLSTN  XC    GLLDDJ,GLLDDJ       CLEAR THE DATE                               
                                                                                
GETLSTX  OC    GLLDDJ,GLLDDJ       TEST LAST RUN DATE SET                       
         JZ    EXITH               NO - EXIT WITH ERROR                         
         MVC   PCWORK(L'GLLDDJ),GLLDDJ                                          
         OI    PCWORK+(L'RFPVNXTR-1),DIGIT                                      
         J     EXITE               EXIT WITH CC=EQUAL                           
         DROP  R2,RC                                                            
                                                                                
GLWORKD  DSECT                     ** GETNXT S/R LOCAL W/S **                   
GLDUB    DS    D                                                                
                                                                                
GLPSDJ   DS    0XL(L'RFPVLSTR)     PERIOD START DATE                            
GLPSDJY  DS    XL2                                                              
GLPSDJD  DS    PL2                                                              
                                                                                
GLPEDJ   DS    0XL(L'RFPVLSTR)     PERIOD END DATE                              
GLPEDJY  DS    XL2                                                              
GLPEDJD  DS    PL2                                                              
                                                                                
GLLDDJ   DS    0XL(L'RFPVLSTR)     LAST RUN DATE                                
GLLDDJY  DS    XL2                                                              
GLLDDJD  DS    PL2                                                              
                                                                                
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET END RUN DATE FOR A GROUP                             *         
*                                                                     *         
* NTRY - INITIALISED RFP BLOCK                                        *         
* EXIT - CC=EQUAL IF LAST RUN DATE SET (PCWORK(4) CONTAINS RUN DATE   *         
*           IN JULIAN FORMAT) ,CC=NOT EQUAL IF NO LAST RUN DATE       *         
*           (PCWORK(4) SET TO BINARY ZEROES)                          *         
***********************************************************************         
                                                                                
         USING GEWORKD,RC                                                       
GETEND   L     R2,ARFPIOB          R2=A(RFP BLOCK)                              
         USING RFPD,R2                                                          
         XC    PCWORK(L'GEPSDJ),PCWORK                                          
                                                                                
         OC    GEPSDJ,RFPVNXTR     EXTRACT AND TEST NEXT RUN DATE               
         BZ    GETENDX                                                          
         OC    GEPEDJ,RFPVENDR     EXTRACT AND TEST NEXT RUN DATE               
         BZ    GETENDX                                                          
         MVC   GECDDJ,GEPSDJ       START FROM PERIOD START DATE                 
         CLC   GEPSDJY,GEPEDJY     TEST START/END IN SAME YEAR                  
         BE    GETEND04                                                         
         MVC   GEPEDJY,GEPSDJY     NO - SEARCH FIRST YEAR FIRST                 
         ZAP   GEPEDJD,MAXDAYS                                                  
                                                                                
GETEND04 CLC   GECDDJ,GEPEDJ       TEST EXCEEDED PERIOD END                     
         BH    GETEND06                                                         
         ZAP   GEDUB,GECDDJD       EXTRACT DAY NUMBER                           
         CVB   RE,GEDUB                                                         
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)     RE=A(BYTE FOR THIS DAY)                      
         IC    RF,DAYBITS(RF)      RF=BIT VALUE FOR THIS DAY                    
         EX    RF,*+14             TEST BIT ON IN RUN MASK                      
         BZ    *+10                YES - FOUND THE NEXT RUN DATE                
         MVC   PCWORK(L'GECDDJ),GECDDJ                                          
         TM    0(RE),0                                                          
         AP    GECDDJD,PONE        NO - ADVANCE TO NEXT DAY                     
         B     GETEND04                                                         
                                                                                
GETEND06 CLC   GECDDJY,RFPVENDR    TEST BOTH YEARS SEARCHED                     
         BE    GETENDX                                                          
         MVC   GEPSDJY,RFPVENDR    NO - SEARCH SECOND YEAR                      
         ZAP   GEPSDJD,PONE        START AT END YEAR/DAY 001                    
         MVC   GEPEDJ,RFPVENDR     END AT END YEAR/END DAY                      
         MVC   GECDDJ,GEPSDJ       SEARCH FROM END YEAR/DAY 001                 
         B     GETEND04                                                         
                                                                                
GETENDX  OC    PCWORK(L'GECDDJ),PCWORK                                          
         JZ    EXITH               CC=NOT EQUAL IF END DATE NOT SET             
         OI    PCWORK+(L'RFPVNXTR-1),DIGIT                                      
         J     EXITE               CC=EQUAL IF END DATE SET                     
         DROP  R2,RC                                                            
                                                                                
GEWORKD  DSECT                     ** GETEND S/R LOCAL W/S **                   
GEDUB    DS    D                                                                
                                                                                
GEPSDJ   DS    0XL(L'RFPVNXTR)     PERIOD START DATE                            
GEPSDJY  DS    XL2                                                              
GEPSDJD  DS    PL2                                                              
                                                                                
GEPEDJ   DS    0XL(L'RFPVNXTR)     PERIOD END DATE                              
GEPEDJY  DS    XL2                                                              
GEPEDJD  DS    PL2                                                              
                                                                                
GECDDJ   DS    0XL(L'RFPVNXTR)     CURRENT SEARCH DATE                          
GECDDJY  DS    XL2                                                              
GECDDJD  DS    PL2                                                              
                                                                                
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AGENCY NAME                                          *         
*                                                                     *         
* NTRY - R1=A(2 CHARACTER AGENCY ALPHA ID)                            *         
* EXIT - CC=EQUAL WITH PCWORK CONTAINING THE USER ID NAME IF PRINCI-  *         
*        PAL ID RECORD FOUND, CC=NOT EQUAL WITH PCWORK CONTAINING     *         
*        QUESTION MARKS IF NOT FOUND                                  *         
***********************************************************************         
                                                                                
         USING GAWORKD,RC                                                       
GETAGY   MVC   GAIOKEY,IOKEY       SAVE CURRENT IOKEY VALUE                     
         MVC   PCWORK(L'PCQUEST),PCQUEST                                        
         MVC   PCWORK+L'PCQUEST(L'CTDSTNAM-L'PCQUEST),PCWORK                    
                                                                                
         LA    R2,IOKEY                                                         
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R1)                                                   
                                                                                
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         BNE   GETAGYN                                                          
                                                                                
         L     R2,AIO3             LOCATE PRINCIPAL ID ON AGENCY RECORD         
         LA    R1,CT5DATA                                                       
         USING CTDSCD,R1                                                        
         SR    R0,R0                                                            
GETAGY02 CLI   CTDSCEL,0           TEST END OF RECORD                           
         BE    GETAGYN                                                          
         CLI   CTDSCEL,CTDSCELQ    TEST DESCRIPTION ELEMENT                     
         BE    *+14                                                             
         IC    R0,CTDSCLEN         NO - BUMP TO NEXT ELEMENT                    
         AR    R1,R0                                                            
         B     GETAGY02                                                         
                                                                                
         LA    R2,IOKEY            READ PRINCIPAL USER-ID RECORD                
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTDSC                                                    
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         BNE   GETAGYN                                                          
                                                                                
         L     R2,AIO3             LOCATE DESTINATION ID NAME                   
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTDSTD,R1                                                        
         SR    R0,R0                                                            
GETAGY04 CLI   CTDSTEL,0           TEST END OF RECORD                           
         BE    GETAGYX                                                          
         CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION DETAILS ELEMENT             
         BE    *+14                                                             
         IC    R0,CTDSTLEN         NO - BUMP TO NEXT ELEMENT                    
         AR    R1,R0                                                            
         B     GETAGY04                                                         
         MVC   PCWORK(L'CTDSTNAM),CTDSTNAM                                      
                                                                                
GETAGYY  CLI   *+1,0               SET CC=EQUAL IF ALL OKAY                     
         B     GETAGYX                                                          
                                                                                
GETAGYN  CLI   *+0,0               SET CC=NOT EQUAL ON ERROR                    
                                                                                
GETAGYX  MVC   IOKEY,GAIOKEY       RESTORE SAVED IOKEY VALUE                    
         J     EXIT                                                             
         DROP  R1,R2                                                            
                                                                                
GAWORKD  DSECT                     ** GETAGY S/R LOCAL W/S **                   
GAIOKEY  DS    XL(L'IOKEY)         SAVED IOKEY VALUE                            
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SYSTEM NAME                                     *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF SYSTEM FIELD)                           *         
* EXIT - CC=EQUAL WITH PCWORK CONTAINING THE SYSTEM LETTER - SYSTEM   *         
*        NAME IS REFRESHED IF NECESSARY, CC=NOT EQUAL ON ERROR WITH   *         
*        FVMSGNO SET                                                  *         
***********************************************************************         
                                                                                
VALSYS   MVI   PCWORK,0            SET INVALID OUTPUT VALUE                     
         MVI   FVMINL,1            SYSTEM IS REQUIRED INPUT FIELD               
         GOTOR AFVAL                                                            
         JNE   EXIT                                                             
                                                                                
         L     R1,ASYSLST          LOOK UP SYSTEM NAME IN SYSTEM LIST           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6+SYSLLEN                                                     
         USING SYSLSTD,R1                                                       
         SR    R2,R2                                                            
         IC    R2,FVXLEN                                                        
VALSYS02 EX    R2,*+8              COMPARE NAME FOR INPUT LENGTH                
         BE    VALSYS04                                                         
         CLC   SYSLNAME(0),FVIFLD                                               
         BXLE  R1,RE,VALSYS02                                                   
         MVC   FVMSGNO,=AL2(GE$ISYST)                                           
         J     EXITH                                                            
                                                                                
VALSYS04 CLC   SYSLNAME,FVIFLD     TEST SYSTEM NAME DISPLYED IN FULL            
         BE    VALSYS06                                                         
         L     RF,FVADDR                                                        
         MVC   L'FVIHDR(L'SYSLNAME,RF),SYSLNAME                                 
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
                                                                                
VALSYS06 DS    0H                                                               
*&&UK*&& MVC   PCWORK(L'SYSLUSLT),SYSLUSLT                                      
*&&US*&& MVC   PCWORK(L'SYSLUSLT),SYSLNAME                                      
         J     EXITE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET SYSTEM NAME                                          *         
*                                                                     *         
* NTRY - R1=A(SYSTEM LETTER OR SYSTEM NUMBER)                         *         
* EXIT - PCWORK CONTAINS SYSTEM NAME AND SYSTEM LETTER                *         
***********************************************************************         
                                                                                
GETSYS   LR    R2,R1               R2=A(SYSTEM LETTER)                          
         L     R1,ASYSLST          LOOK UP SYSTEM NAME IN SYSTEM LIST           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6+SYSLLEN                                                     
         USING SYSLSTD,R1                                                       
GETSYS02 CLC   SYSLNUM,0(R2)       OR SYSTEM NUMBER                             
         BE    GETSYS04                                                         
*&&UK*&& CLC   SYSLUSLT,0(R2)      MATCH SYSTEM LETTER TO TABLE                 
*&&US                                                                           
         CLI   0(R2),RFPFSSTL      TEST SPOT TRAFFIC                            
         BNE   *+12                                                             
         CLI   SYSLNUM,STRNUMQ     YES - MATCH SYSTEM NUMBER                    
         BE    GETSYS04                                                         
         CLC   SYSLNAME(1),0(R2)   MATCH SYSTEM LETTER TO TABLE                 
*&&                                                                             
         BE    GETSYS04                                                         
         BXLE  R1,RE,GETSYS02                                                   
         DC    H'0'                                                             
GETSYS04 MVC   PCWORK(L'SYSLNAME),SYSLNAME                                      
*&&UK*&& MVC   PCWORK+L'SYSLNAME(L'SYSLUSLT),SYSLUSLT                           
*&&US*&& MVC   PCWORK+L'SYSLNAME(L'SYSLUSLT),SYSLNAME                           
         MVC   PCWORK+L'SYSLNAME+L'SYSLUSLT(L'SYSLNUM),SYSLNUM                  
         J     EXITE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET RUN SCHEDULE                                         *         
*                                                                     *         
* NTRY - R1=A(RUN DAYS), IF HOB OF R1 HAS X'80' BIT ON THE SCHEDULE   *         
*        WILL BE RETURNED IN MIXED-CASE                               *         
* EXIT - PCWORK CONTAINS RUN SCHEDULE                                 *         
***********************************************************************         
                                                                                
         USING GRWORKD,RC                                                       
GETRSH   STCM  R1,8,GRINDS         R2=A(SYSTEM LETTER)                          
         MVC   GRDAYS,0(R1)        R2=A(SYSTEM LETTER)                          
         MVI   PCWORK,SPACE                                                     
         MVC   PCWORK+1(L'PCWORK-1),PCWORK                                      
                                                                                
         CLI   GRDCON,GRDCONQ      TEST CONVERTED TO NEW FORMAT                 
         BE    GETRSH04                                                         
                                                                                
         CLI   GRDAYS+5,C'E'       TEST SPECIAL FORMAT (31)                     
         BNE   *+14                                                             
         MVC   GRDAYS,DAYS31                                                    
         B     GETRSH04                                                         
                                                                                
         CLI   GRDAYS+6,C'S'       TEST SPECIAL FORMAT (15/31)                  
         BNE   *+14                                                             
         MVC   GRDAYS,DAYS1531                                                  
         B     GETRSH04                                                         
                                                                                
         LA    RE,GRDAYS           ELSE CONVERT DAYS TO DAY MASK                
         LHI   RF,5                                                             
         SR    R1,R1                                                            
GETRSH02 CLI   0(RE),0                                                          
         BE    *+8                                                              
         AHI   R1,1                                                             
         SLL   R1,1                                                             
         AHI   RE,1                                                             
         BCT   RF,GETRSH02                                                      
         SLL   R1,2                SHIFT REMAINING BITS                         
         XC    GRDAYS,GRDAYS                                                    
         MVI   GRDCON,GRDCONQ                                                   
         MVI   GRDTYP,GRDTDAYQ     SET TYPE TO DAY LIST                         
         STC   R1,GRDDAYS                                                       
                                                                                
GETRSH04 LA    R2,PCWORK           R2=A(OUTPUT FIELD)                           
         CLI   GRDTYP,GRDTDAYQ     TEST LIST OF DAYS                            
         BNE   GETRSH10                                                         
         SR    RF,RF                                                            
         ICM   RF,8,GRDDAYS                                                     
         LA    R1,PCUMON           R1=A(LIST OF DAY NAMES)                      
         TM    GRINDS,GRILCASE     TEST LOWER CASE REQUESTED                    
         BZ    *+8                                                              
         LA    R1,PCMMON                                                        
         LHI   R0,DAYSINWQ         R0=N'DAYS IN WEEK                            
GETRSH06 SR    RE,RE               SHIFT DAY BIT INTO RE                        
         SLDL  RE,1                                                             
         LTR   RE,RE               TEST DAY BIT ON                              
         BZ    GETRSH08                                                         
         MVC   0(L'PCUMON,R2),0(R1)                                             
         AHI   R2,L'PCUMON                                                      
         MVC   0(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA                                                     
GETRSH08 AHI   R1,L'PCUMON         BUMP TO NEXT DAY NAME                        
         BCT   R0,GETRSH06         DO FOR NUMBER OF DAYS                        
         BCTR  R2,0                                                             
         MVI   0(R2),SPACE         REMOVE TRAILING COMMA                        
         B     GETRSHX                                                          
                                                                                
GETRSH10 CLI   GRDTYP,GRDTDATQ     TEST LIST OF DATES                           
         BNE   GETRSH16                                                         
         ICM   RF,15,GRDDATS                                                    
         LHI   R1,1                                                             
         LHI   R0,DAYSINMQ                                                      
GETRSH12 SR    RE,RE                                                            
         SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    GETRSH14                                                         
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  0(2,R2),PCDUB                                                    
         AHI   R2,2                                                             
         MVC   0(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA                                                     
GETRSH14 AHI   R1,1                ADD 1 TO DAY NUMBER                          
         BCT   R0,GETRSH12                                                      
         BCTR  R2,0                                                             
         MVI   0(R2),SPACE         REMOVE TRAILING COMMA                        
         B     GETRSHX                                                          
                                                                                
GETRSH16 CLI   GRDTYP,GRDTNOTQ     TEST NO RUN SCHEDULE (IRREGULAR)             
         BE    GETRSHX                                                          
                                                                                
         DC    H'0'                TYPE NOT SUPPORTED                           
                                                                                
GETRSHX  J     EXITE                                                            
         DROP  RC                                                               
                                                                                
GRWORKD  DSECT                     ** GETRSH S/R LOCAL W/S **                   
GRINDS   DS    XL1                 INDICATOR BYTE                               
GRILCASE EQU   X'80'               RETURN SCHEDULE IN MIXED CASE                
GRDAYS   DS    0XL(L'RFPVDAYS)     RUN SCHEDULE                                 
GRDCON   DS    XL1                 CONTROL BYTE                                 
GRDCONQ  EQU   X'FF'               CONVERTED TO NEW FORMAT                      
GRDTYP   DS    XL1                 LIST TYPE                                    
GRDTDAYQ EQU   1                   LIST OF DAYS                                 
GRDTDATQ EQU   2                   LIST OF DATES                                
GRDTNOTQ EQU   3                   LIST NOT PRESENT (IRREGULAR)                 
GRDDAYS  DS    0XL1                LIST OF DAYS (X'80'=MON ETC.)                
GRDDATS  DS    XL4                 LIST OF DATES (X'80'=1,X'40'=2 ETC.)         
         DS    XL2                 N/D                                          
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* MOVE GROUP/XFILE GROUP VALUES INTO RFP CONTROL BLOCK                *         
* NTRY - AIO1 POINTS TO AN XFILE GROUP RECORD                         *         
* EXIT - RFP CONTROL BLOCK VALUES SET OR CLEARED                      *         
***********************************************************************         
                                                                                
XFGRFP   L     R2,ARFPIOB                                                       
         USING RFPD,R2                                                          
         XC    RFPVDAYS,RFPVDAYS                                                
         XC    RFPVRNGE,RFPVRNGE                                                
         XC    RFPVNXTR,RFPVNXTR                                                
         XC    RFPVENDR,RFPVENDR                                                
         XC    RFPVLSTR,RFPVLSTR                                                
         XC    RFPXFILE,RFPXFILE                                                
                                                                                
         L     R1,AIO1                                                          
         USING GRPKEYD,R1                                                       
         SR    RE,RE               RE=0 FOR XFILE GROUP                         
         CLI   GRPKSYS,GRPKSYSQ                                                 
         BNE   *+8                                                              
         CLI   GRPKSTYP,GRPKSTYQ                                                
         BNE   *+8                                                              
         LHI   RE,1                RE=1 FOR RFP GROUP                           
                                                                                
         MVC   RFPVSYST,GRPKSYST                                                
         MVC   RFPVGRP,GRPKGRP                                                  
         MVC   RFPVAGY,GRPKAGY                                                  
         MVC   RFPVUSER,GRPKUSER                                                
                                                                                
         MVI   RFPVGSTA,0          SET GROUP STATUS                             
         TM    GRPSTAT,X'80'                                                    
         BZ    *+8                                                              
         OI    RFPVGSTA,RFPVGSDQ                                                
                                                                                
         LA    R1,GRPFSTEL                                                      
         USING GRPHD,R1            R1=A(FIRST ELEMENT ON XFILE RECORD)          
         SR    R0,R0                                                            
XFGRFP02 CLI   GRPHCD,GRPHCDQ      TEST RECORD HEADER ELEMENT                   
         BE    XFGRFP06                                                         
         CLI   GRPHCD,GRPDCDQ      TEST DATES ELEMENT                           
         BE    XFGRFP08                                                         
XFGRFP04 IC    R0,GRPHLN           BUMP TO NEXT ELEMENT ON RECORD               
         AR    R1,R0                                                            
         CLI   GRPHCD,0            TEST END OF RECORD                           
         BNE   XFGRFP02                                                         
         B     XFGRFPX                                                          
                                                                                
XFGRFP06 MVC   RFPVDESC,GRPHDESC   EXTRACT GROUP VALUES                         
         MVC   RFPVFREQ,GRPHFREQ                                                
         MVC   RFPVOTYP,GRPHOTYP                                                
         MVC   RFPVDEST,GRPHDEST                                                
         MVC   RFPVNAME,GRPHNAME                                                
         MVC   RFPVNXTR,GRPHNXTR                                                
         MVC   RFPVENDR,GRPHEND                                                 
         MVC   RFPVLSTR,GRPHLSTR                                                
         MVC   RFPVHSTA,GRPHSTAT                                                
         MVC   RFPVPERS,GRPHPERS                                                
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   RFPXFILE,GRPHXFIL                                                
         CLC   RFPXFILE,PCSPACES                                                
         BH    *+10                                                             
         XC    RFPXFILE,RFPXFILE                                                
         B     XFGRFP04                                                         
                                                                                
         USING GRPDD,R1                                                         
XFGRFP08 MVC   RFPVDAYS,GRPDDAY    EXTRACT RUN SCHEDULE/DATES                   
         MVC   RFPVRNGE,GRPDTS                                                  
         B     XFGRFP04                                                         
                                                                                
XFGRFPX  J     EXITE                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR NON-WORKING DAY AND GET NEXT WORKING DAY        *         
*                                                                     *         
* NTRY - R1=A(JULIAN DATE TO BE TESTED) - HOB ON IF VALIDATING A      *         
*           MANUAL GROUP SUBMISSION (DIFFERENT RULES APPLY)           *         
* EXIT - CC=EQUAL IF INPUT DATE IS OKAY                               *         
*        CC=LOW IF A WEEKEND DATE & OVWORK1/OVFULL1 CONTAINING EBCDIC *         
*           AND JULIAN ADJUSTED (FRIDAY/MONDAY) DATE RESPECTIVELY     *         
*        CC=HIGH IF A DDS HOLIDAY, NO PRODUCTION RUN                  *         
***********************************************************************         
                                                                                
         USING GWWORKD,RC                                                       
GETWRK   STCM  R1,8,GWMODE         SET CALLING MODE                             
         MVC   GWCDDJ,0(R1)                                                     
         MVC   PCWORK(L'GWCDDJ),GWCDDJ                                          
         GOTOR VDATCON,GWPARM,(6,GWCDDJ),(0,GWCDDE)                             
         CLC   =C'1231',GWCDDEMM   ALWAYS ALLOW DEC/31                          
         JE    EXITE                                                            
*        CLC   =C'1224',GWCDDEMM                                                
*        JE    EXITH                                                            
         CLC   =C'1225',GWCDDEMM                                                
         JE    EXITH                                                            
         GOTOR VGETDAY,GWPARM,GWCDDE,GWCDDD                                     
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),6                                                          
         JL    EXITE                                                            
         TM    GWMODE,X'80'        TEST MANUAL SUBMISSION                       
         BZ    *+12                                                             
         CLI   0(R1),7             YES - ALLOW SUNDAY                           
         JE    EXITE                                                            
         CLI   PCPWKEND,PCPWKYQ    TEST SUNDAY SUBMISSION ALLOWED               
         JNE   *+12                                                             
         CLI   0(R1),7             YES - ALLOW SUNDAY                           
         JE    EXITE                                                            
                                                                                
         CLI   0(R1),6             ADJUST TO NEXT/PREVIOUS WORK DAY             
         BNE   GETWRK02                                                         
         LHI   R0,1                SATURDAY - FORWARD TO SUNDAY                 
         CLI   PCPWKEND,PCPWKYQ    BUT ONLY IF ALLOWED                          
         JE    *+8                                                              
         LHI   R0,2                OR MONDAY IF NOT                             
         CLI   PCPDRCTN,PCPDRFQ                                                 
         BE    GETWRK04                                                         
         LHI   R0,-1               OR BACK TO FRIDAY                            
         B     GETWRK04                                                         
                                                                                
GETWRK02 LHI   R0,1                SUNDAY - FORWARD TO MONDAY                   
         CLI   PCPDRCTN,PCPDRFQ                                                 
         BE    GETWRK04                                                         
         LHI   R0,-2               OR BACK TO FRIDAY                            
                                                                                
GETWRK04 GOTOR VADDAY,GWPARM,GWCDDE,GWCDDE2,(R0)                                
         GOTOR VDATCON,GWPARM,(0,GWCDDE2),(15,PCWORK)                           
         J     EXITL                                                            
         DROP  RC                                                               
                                                                                
GWWORKD  DSECT                     ** GETWRK S/R LOCAL W/S **                   
GWPARM   DS    6F                  PARAMETER LIST                               
GWMODE   DS    X                   CALLING MODE                                 
GWCDDJ   DS    XL(L'ASJDAT)        INPUT JULIAN DATE                            
GWCDDE   DS    0CL6                INPUT EBCDIC DATE                            
GWCDDEYY DS    CL2                                                              
GWCDDEMM DS    CL2                                                              
GWCDDEDD DS    CL2                                                              
GWCDDE2  DS    CL6                 COMPUTED EBCDIC DATE                         
GWCDDD   DS    CL3                 DAY                                          
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET NEXT I/O KEY AND FILTER I/O KEY                      *         
***********************************************************************         
                                                                                
         USING SKWORKD,RC                                                       
SETKEY   MVI   SKACTN,SKASET       SET ACTION TO 'SET NEXT KEY'                 
         B     SETKEY00                                                         
                                                                                
FLTKEY   MVI   SKACTN,SKAFLT       SET ACTION TO 'FILTER'                       
                                                                                
SETKEY00 L     R3,AKEYTAB          R3=A(KEY TABLE)                              
                                                                                
         ST    R3,SKATAB           SAVE A(KEY TABLE)                            
         USING KEYTABD,R3                                                       
         MVI   SKFUNC,SKFTSTKY     SET TO TEST KEY COMPONENT                    
         MVI   SKACTV,SKANO        SET NO KEY CHANGES                           
SETKEY02 CLI   SKFUNC,SKFTSTKY                                                  
         BE    *+8                                                              
         MVI   SKACTV,SKAYES       SET KEY CHANGED                              
         BNL   SETKEY08                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
                                                                                
         SR    RE,RE                                                            
         IC    RE,KEYTKDSP                                                      
         LA    RE,IOKEY(RE)        RE=DISPLACEMENT TO KEY FIELD                 
         LA    R1,IOKEY+L'IOKEY-1                                               
         SR    R1,RE               R1=L'REMAINING KEY                           
         TM    KEYTIND1,KEYTITAB+KEYTIRNG+KEYTIFIX                              
         BZ    SETKEY06                                                         
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,KEYTFDSP                                                    
         LA    RF,TWAD(RF)                                                      
         TM    KEYTIND1,KEYTIATB   TEST ATTRIBUTE IN FRONT OF DATA              
         BZ    SETKEY04                                                         
         CLC   KEYTATRB,0(RF)      MATCH TO DATA                                
         BNE   SETKEYNT                                                         
         AHI   RF,L'KEYTATRB                                                    
                                                                                
SETKEY04 EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)       CLEAR REMAINDER OF KEY                       
         IC    R1,KEYTKLEN                                                      
         EX    R1,*+8                                                           
         B     SETKEYNT            NEXT TABLE ENTRY                             
         MVC   0(0,RE),0(RF)       SET LOWEST KEY FILTER VALUE                  
                                                                                
SETKEY06 TM    KEYTIND1,KEYTILIT   TEST LITERAL VALUE                           
         BO    *+6                                                              
         DC    H'0'                UNKNOWN KEY COMPONENT TYPE                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)       CLEAR REMAINDER OF KEY                       
         MVC   0(1,RF),KEYTKLIT                                                 
         ICM   R1,1,KEYTKLEN                                                    
         BZ    SETKEYNT            NEXT TABLE ENTRY                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     SETKEYNT            NEXT TABLE ENTRY                             
         MVC   1(0,RF),0(RF)                                                    
                                                                                
SETKEY08 BE    SETKEY12                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         SR    RE,RE                                                            
         IC    RE,KEYTKDSP                                                      
         LA    RE,IOKEY(RE)                                                     
         SR    R1,R1                                                            
         ICM   R1,1,KEYTKLEN                                                    
         LA    RF,0(R1,RE)                                                      
         AHI   R1,1                                                             
         CLI   0(RF),FF                                                         
         BNE   SETKEY10                                                         
         MVI   0(RF),0                                                          
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
         B     SETKEYPT                                                         
                                                                                
SETKEY10 IC    RE,0(RF)                                                         
         AHI   RE,1                                                             
         STC   RE,0(RF)                                                         
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES IN REMAINDER OF KEY           
                                                                                
SETKEY12 SR    RE,RE                                                            
         IC    RE,KEYTKDSP                                                      
         LA    RE,IOKEY(RE)                                                     
         SR    R1,R1                                                            
         IC    R1,KEYTKLEN                                                      
         TM    KEYTIND1,KEYTITAB+KEYTIRNG+KEYTIFIX                              
         BZ    SETKEY30                                                         
         SR    RF,RF                                                            
         ICM   RF,3,KEYTFDSP                                                    
         LA    RF,TWAD(RF)                                                      
         TM    KEYTIND1,KEYTIATB   TEST ATTRIBUTE IN FRONT OF DATA              
         BZ    SETKEY14                                                         
         CLC   KEYTATRB,0(RF)      MATCH TO DATA                                
         BNE   SETKEYNT                                                         
         AHI   RF,L'KEYTATRB                                                    
                                                                                
SETKEY14 SR    R0,R0                                                            
         IC    R0,KEYTFNUM                                                      
         TM    KEYTIND1,KEYTITAB   TEST TABULAR                                 
         BZ    SETKEY20                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       TEST ANY VALUE PRESENT                       
         BE    SETKEYNT            NEXT TABLE ENTRY                             
                                                                                
SETKEY16 EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       TEST EOT                                     
         BZ    SETKEYPT                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST VALUE                                   
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEY18                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         MVI   SKACTV,SKAYES                                                    
         B     SETKEYNT            NEXT TABLE ENTRY                             
                                                                                
SETKEY18 LA    RF,1(RF,R1)         NEXT FILTER TABLE ENTRY                      
         BCT   R0,SETKEY16                                                      
         B     SETKEYPT                                                         
                                                                                
SETKEY20 TM    KEYTIND1,KEYTIRNG   TEST RANGE                                   
         BZ    SETKEY24                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST AGAINST RANGE START VALUE               
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEY22                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         B     SETKEYNT            NEXT TABLE ENTRY                             
                                                                                
SETKEY22 LA    RF,1(RF,R1)         RF=A(RANGE END VALUE)                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST AGAINST RANGE END VALUE                 
         BH    SETKEYPT                                                         
         B     SETKEYNT            NEXT TABLE ENTRY                             
                                                                                
SETKEY24 TM    KEYTIND1,KEYTIFIX                                                
         BO    *+6                                                              
         DC    H'0'                INVALID KEY COMPONENT TYPE                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       TEST VALUE                                   
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEYPT                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       SET VALUE                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         B     SETKEYNT            NEXT TABLE ENTRY                             
                                                                                
SETKEY30 TM    KEYTIND1,KEYTILIT                                                
         BO    *+6                                                              
         DC    H'0'                INVALID KEY COMPONENT TYPE                   
         MVC   SKFILL(L'KEYTKLIT),KEYTKLIT                                      
         MVC   SKFILL+L'KEYTKLIT(L'SKFILL-1),SKFILL                             
         SR    R1,R1                                                            
         IC    R1,KEYTKLEN                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SKFILL      TEST VALUE                                   
         BE    SETKEYNT            NEXT TABLE ENTRY                             
         BH    SETKEYPT                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         EX    R1,*+8                                                           
         B     *+10                NEXT TABLE ENTRY                             
         MVC   0(0,RE),SKFILL      SET VALUE                                    
         MVI   SKFUNC,SKFSETLO     SET LOW VALUES                               
         B     SETKEYNT            NEXT TABLE ENTRY                             
                                                                                
SETKEYPT CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYN                                                          
         MVI   SKFUNC,SKFSETNX     SET NEXT KEY VALUE                           
SETKEYP2 SHI   R3,KEYTABL          BACK UP TO PREVIOUS KEYTAB ENTRY             
         C     R3,SKATAB                                                        
         BL    SETKEYN                                                          
         TM    KEYTIND1,KEYTIATB   TEST ATTRIBUTE IN FRONT OF DATA              
         BZ    SETKEYP4                                                         
         SR    RF,RF                                                            
         ICM   RF,3,KEYTFDSP                                                    
         LA    RF,TWAD(RF)                                                      
         CLC   KEYTATRB,0(RF)      MATCH TO DATA                                
         BNE   SETKEYP2                                                         
SETKEYP4 TM    KEYTIND1,KEYTILIT+KEYTIFIX                                       
         BNZ   SETKEYN                                                          
         B     SETKEY02                                                         
                                                                                
SETKEYNT CLI   KEYTABD+KEYTABL,KEYTEOTQ  TEST END OF TABLE IS NEXT              
         BE    *+12                                                             
         AHI   R3,KEYTABL          BUMP TO NEXT KEYTAB ENTRY                    
         B     SETKEY02                                                         
         CLI   SKACTN,SKAFLT       TEST APPLYING FILTERS                        
         BE    SETKEYY                                                          
         CLI   SKACTV,SKANO        TEST KEY CHANGED                             
         BNE   SETKEYY                                                          
         LA    RE,IOKEY+L'GRPKEY-1                                              
         CLI   0(RE),FF                                                         
         BNE   *+12                                                             
         MVI   0(RE),0                                                          
         BCT   RE,*-12                                                          
         IC    RF,0(RE)                                                         
         AHI   RF,1                                                             
         STC   RF,0(RE)                                                         
         B     SETKEYY                                                          
                                                                                
SETKEYN  MVI   SKFUNC,2                                                         
         B     SETKEYX                                                          
                                                                                
SETKEYY  MVI   SKFUNC,1                                                         
                                                                                
SETKEYX  CLI   SKFUNC,1                                                         
         J     EXIT                                                             
         DROP  R3,RC                                                            
                                                                                
SKWORKD  DSECT                     ** SETKEY LOCAL W/S **                       
SKATAB   DS    A                   A(KEY TABLE)                                 
SKACTN   DS    XL1                 ACTION NUMBER                                
SKAFLT   EQU   1                   APPLY KEY FILTERS                            
SKASET   EQU   2                   SET NEXT KEY                                 
SKFUNC   DS    XL1                 KEY FUNCTION                                 
SKFSETLO EQU   0                   SET LOW VALUE                                
SKFTSTKY EQU   1                   TEST KEY COMPONENT                           
SKFSETNX EQU   2                   SET NEXT KEY VALUE                           
SKACTV   DS    XL1                 KEY CHANGE ACTIVE                            
SKANO    EQU   0                   NO KEY CHANGES                               
SKAYES   EQU   1                   KEY CHANGED                                  
SKFILL   DS    XL16                LITERAL VALUE                                
SKWORKX  EQU   *                                                                
RLP00    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PERSON-ID                                            *         
*                                                                     *         
* NTRY - R1=A(AGENCY ALPHA/PERSON CODE)                               *         
* EXIT - BCWORK=PUBLIC-ID                                             *         
***********************************************************************         
                                                                                
         USING GPWORKD,RC                                                       
GETPER   MVC   GPIOKEY,IOKEY                                                    
         MVC   GPAGY,0(R1)                                                      
         MVC   GPNUM,L'GPAGY(R1)                                                
         MVI   PCWORK,C'*'                                                      
         MVC   PCWORK+1(L'SAPALPID-1),PCWORK                                    
                                                                                
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           BUILD KEY OF RECORD                          
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,GPAGY                                                    
         MVC   SA0KNUM,GPNUM                                                    
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         BE    GETPER06                                                         
                                                                                
         USING CT5REC,R2           READ ACCESS RECORD AND CHECK                 
         XC    CT5KEY,CT5KEY       IF DIFFERENT SECURITY AGENCY                 
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,GPAGY                                                   
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,CT5DATA                                                       
         SR    R0,R0                                                            
         USING CTSEAD,R2                                                        
GETPER02 CLI   CTSEAEL,0           TEST END OF RECORD                           
         BE    GETPERX                                                          
         CLI   CTSEAEL,CTSEAELQ                                                 
         BE    *+14                                                             
         IC    R0,CTSEALEN                                                      
         AR    R2,R0                                                            
         B     GETPER02                                                         
         CLC   GPAGY,CTSEAAID      TEST JUST LOOKED FOR THIS                    
         BE    GETPERX                                                          
         MVC   GPAGY,CTSEAAID      SET SECURITY AGENCY & READ AGAIN             
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           BUILD KEY OF RECORD                          
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,GPAGY                                                    
         MVC   SA0KNUM,GPNUM                                                    
         GOTOR AIO,'IOREAD+IOCTFILE+IO3'                                        
         BNE   GETPERX                                                          
                                                                                
GETPER06 L     R2,AIO3                                                          
         AHI   R2,SA0DATA-SA0REC                                                
         USING SAPALD,R2                                                        
         SR    R0,R0                                                            
GETPER08 CLI   SAPALEL,0           TEST EOR                                     
         BE    GETPERX                                                          
         CLI   SAPALEL,SAPALELQ    TEST PERSON PERSONAL-ID ELEMENT              
         BE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    R2,R0                                                            
         B     GETPER08                                                         
         MVC   PCWORK(L'SAPALPID),SAPALPID                                      
                                                                                
GETPERX  MVC   IOKEY,GPIOKEY                                                    
         J     EXITE                                                            
         DROP  R2                                                               
                                                                                
GPWORKD  DSECT                     ** GETPER S/R LOCAL W/S **                   
GPAGY    DS    CL(L'SA0KAGY)                                                    
GPNUM    DS    XL(L'SA0KNUM)                                                    
GPIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
RLP00    CSECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
                                                                                
ANAWS    DS    0XL4                ** A(NON-ADDRESSABLE W/S AREAS) **           
         DC    AL2(IOAREA1+L'IODA+L'IOWORK-WORKD,AIO1-WORKD)                    
         DC    AL2(IOAREA2+L'IODA+L'IOWORK-WORKD,AIO2-WORKD)                    
         DC    AL2(IOAREA3+L'IODA+L'IOWORK-WORKD,AIO3-WORKD)                    
         DC    AL2(IOAREA4+L'IODA+L'IOWORK-WORKD,AIO4-WORKD)                    
         DC    AL2(SWSTAB-WORKD,ASWSTAB-WORKD)                                  
         DC    AL2(TSABLK-WORKD,ATSABLK-WORKD)                                  
         DC    AL2(RFPIOB-WORKD,ARFPIOB-WORKD)                                  
         DC    AL2(MINBUF-WORKD,AMINBUF-WORKD)                                  
         DC    AL2(RFPBUFF-WORKD,ARFPBUFF-WORKD)                                
         DC    AL2(SOFBUFF-WORKD,ASOFBUFF-WORKD)                                
         DC    AL2(OVERWRK-WORKD,AOVERWRK-WORKD)                                
         DC    AL2(WORKX-WORKD,AWORKX-WORKD)                                    
ANAWSN   EQU   (*-ANAWS)/L'ANAWS                                                
                                                                                
LANGCHAR DS    0CL8                ** LANGUAGE DEPENDANT CHARACTERS **          
         DC    CL8',=?/#.**'       ENGLISH                                      
         DC    CL8',=?/#.**'       AMERICAN                                     
         DC    CL8'#=?/#,**'       GERMAN                                       
         DC    CL8',=?/#,**'       FRENCH                                       
         DC    CL8',=?/#,**'       SPANISH                                      
         DC    CL8',=?/#,**'       ITALIAN                                      
         DC    CL8',=?/#,**'       DUTCH                                        
         DC    CL8',=?/#,**'       ???                                          
         DC    CL8',=?/#,**'       ???                                          
         DC    CL8',=?/#,**'       ???                                          
         DC    CL8',=?/#,**'       ???                                          
         DC    CL8',=?/#,**'       ???                                          
                                                                                
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QTSAR)                                                       
         DC    AL1(QRFPIO)                                                      
         DC    AL1(QREPORT)                                                     
         DC    AL1(QGETIDS)                                                     
PHASESX  DC    AL1(FF)                                                          
                                                                                
ADDRS    DS    0A                  ** CONTROLLER ADDRESSES 1 **                 
         DC    AL4(SYSTAB-RLP00)                                                
         DC    AL4(CMDTAB-RLP00)                                                
         DC    AL4(FILTAB-RLP00)                                                
         DC    AL4(FRQTAB-RLP00)                                                
         DC    AL4(ROUT-RLP00)                                                  
         DC    AL4(RECTAB-RLP00)                                                
         DC    AL4(ACTTAB-RLP00)                                                
         DC    AL4(MIXTAB-RLP00)                                                
         DC    AL4(PFKTAB-RLP00)                                                
         DC    AL4(SELTAB-RLP00)                                                
         DC    AL4(DICUPR-RLP00)                                                
         DC    AL4(DICMIX-RLP00)                                                
ADDRS#   EQU   (*-ADDRS)/L'ADDRS                                                
                                                                                
***********************************************************************         
* SYSTEM FILE NAMES TABLE (FILE NUMBERS 1 THRU 9)                     *         
***********************************************************************         
                                                                                
FILTAB   DS    0X                                                               
                                                                                
FILACC   DC    AL1(ACCNUMQ),C'ACC',AL2(FILACCX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILACCX  DC    AL1(EOT)                                                         
                                                                                
*&&US                                                                           
FILSPT   DC    AL1(SPTNUMQ),C'SPT',AL2(FILSPTX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILSPTX  DC    AL1(EOT)                                                         
                                                                                
FILNET   DC    AL1(NETNUMQ),C'NET',AL2(FILNETX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILNETX  DC    AL1(EOT)                                                         
                                                                                
FILPRT   DC    AL1(PRTNUMQ),C'PRT',AL2(FILPRTX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILPRTX  DC    AL1(EOT)                                                         
                                                                                
FILTAL   DC    AL1(TALNUMQ),C'TAL',AL2(FILTALX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILTALX  DC    AL1(EOT)                                                         
                                                                                
FILREP   DC    AL1(REPNUMQ),C'REP',AL2(FILREPX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILREPX  DC    AL1(EOT)                                                         
                                                                                
FILSTR   DC    AL1(STRNUMQ),C'STR',AL2(FILSTRX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENDIR/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),XL7'00'                                        
         DC    AL1(CONNUMQ,IOGENFIL/256)                                        
         DC    AL1(0,0,0),AL2(00)                                               
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILSTRX  DC    AL1(EOT)                                                         
*&&                                                                             
                                                                                
CONLETQ  EQU   C'C'                                                             
FILCON   DC    AL1(CONNUMQ),C'CON',AL2(FILCONX-*)                               
                                                                                
         DC    AL1(IOGENDIS/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIS/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
FILCONX  DC    AL1(EOT)                                                         
                                                                                
FILTABX  DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* TABLE OF GLOBAL SYSTEM FILES (FILE NUMBERS 10 THRU 15)              *         
***********************************************************************         
                                                                                
SYSTAB   DS    0X                                                               
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(15),C'CTFILE '                                               
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
SYSTABX  DS    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* SYSTEM FILE COMMANDS TABLE                                          *         
***********************************************************************         
                                                                                
CMDTAB   DS    0X                                                               
                                                                                
*                                  ** INDEX SEQUENTIAL COMMANDS **              
CMDIS    DC    AL1(FILIVL+FILIIS,0),AL2(CMDISX-*)                               
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
CMDISX   DC    AL1(EOT)                                                         
                                                                                
*                                  ** DIRECT ACCESS COMMANDS **                 
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX-*)                                      
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
CMDDAX   DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* TABLE OF FREQUENCY CODES                                            *         
***********************************************************************         
                                                                                
FRQTAB   DS    0X                                                               
                                                                                
         DC    AL1(FRQWEEK),AL2(PCUWKLY-WORKD,PCMWKLY-WORKD)                    
         DC    AL1(FRQTIWKQ)                                                    
         DC    AL1(FRQMNTH),AL2(PCUMTHLY-WORKD,PCMMTHLY-WORKD)                  
         DC    AL1(FRQTIMNQ+1)                                                  
         DC    AL1(FRQQRTR),AL2(PCUQTRLY-WORKD,PCMQTRLY-WORKD)                  
         DC    AL1(FRQTIMNQ+3)                                                  
         DC    AL1(FRQYEAR),AL2(PCUYRLY-WORKD,PCMYRLY-WORKD)                    
         DC    AL1(FRQTIMNQ+12)                                                 
         DC    AL1(FRQNOTR),AL2(PCUIRREG-WORKD,PCMIRREG-WORKD)                  
         DC    AL1(FRQTINOQ)                                                    
         DC    AL1(FRQNOTR),AL2(PCUNOTRG-WORKD,PCMNOTRG-WORKD)                  
         DC    AL1(FRQTINOQ)                                                    
         DC    AL1(FRQRDAY),AL2(PCURDAYS-WORKD,PCMRDAYS-WORKD)                  
         DC    AL1(0)              NO INCREMENTS FOR RUNDAYS FREQUENCY          
                                                                                
FRQTABX  DC    AL1(FRQTEOTQ)                                                    
         EJECT                                                                  
DICUPR   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
                                                                                
         DCDDL GE#PFK,L'PCUPFK                                                  
         DCDDL GE#HELP,L'PCUHELP                                                
         DCDDL GE#YES,L'PCUYES                                                  
         DCDDL GE#NO,L'PCUNO                                                    
         DCDDL GE#PAGE,L'PCUPAGE                                                
         DCDDL GE#HALF,L'PCUHALF                                                
         DCDDL GE#MAX,L'PCUMAX                                                  
         DCDDL GE#WKLY,L'PCUWKLY                                                
         DCDDL GE#MTHLY,L'PCUMTHLY                                              
         DCDDL GE#QTRLY,L'PCUQTRLY                                              
         DCDDL GE#YRLY,L'PCUYRLY                                                
         DCDDL GE#IRREG,L'PCUIRREG                                              
         DCDDL GE#NOTRG,L'PCUNOTRG                                              
         DCDDL GE#GROUP,L'PCUGROUP                                              
         DCDDL GE#REQST,L'PCUREQST                                              
         DCDDL GE#LIS,L'PCULIS                                                  
         DCDDL GE#SEL,L'PCUSEL                                                  
         DCDDL GE#DIS,L'PCUDIS                                                  
         DCDDL GE#UPDTE,L'PCUUPDTE                                              
         DCDDL GE#USRID,L'PCUUSRID                                              
         DCDDL GE#SYSTM,L'PCUSYSTM                                              
         DCDDL GE#AGY,L'PCUAGY                                                  
         DCDDL GE#ALL,L'PCUALL                                                  
         DCDDL GE#ADD,L'PCUADD                                                  
         DCDDL GE#CHA,L'PCUCHA                                                  
         DCDDL GE#IDGRP,L'PCUIDGRL                                              
         DCDDL GE#IDGRP,L'PCUIDGRS                                              
         DCDDL GE#DEL,L'PCUDEL                                                  
         DCDDL GE#MOVE,L'PCUMOVE                                                
         DCDDL GE#COPY,L'PCUCOPY                                                
         DCDDL GE#RUNQ,L'PCURUNQ                                                
         DCDDL GE#GLOBL,L'PCUGLOBL                                              
         DCDDL GE#CALDR,L'PCUCALDR                                              
         DCDDL GE#MON,L'PCUMON                                                  
         DCDDL GE#TUE,L'PCUTUE                                                  
         DCDDL GE#WED,L'PCUWED                                                  
         DCDDL GE#THU,L'PCUTHU                                                  
         DCDDL GE#FRI,L'PCUFRI                                                  
         DCDDL GE#SAT,L'PCUSAT                                                  
         DCDDL GE#SUN,L'PCUSUN                                                  
         DCDDL GE#XFILE,L'PCUXFILE                                              
         DCDDL GE#SBMIT,L'PCUSBMIT                                              
         DCDDL GE#REP,L'PCUREP                                                  
         DCDDL GE#KEYWD,L'PCUKEYWD                                              
         DCDDL GE#DATES,L'PCUDATES                                              
         DCDDL GE#REAL,L'PCUREAL                                                
         DCDDL GE#SENAM,L'PCUSENAM                                              
         DCDDL GE#ONLY,L'PCUONLY                                                
         DCDDL GE#PRVT,L'PCUPRVT                                                
         DCDDL GE#FAXP,L'PCUFAXP                                                
         DCDDL GE#FAX,L'PCUFAX                                                  
         DCDDL GE#SOFTD,L'PCUSOFTD                                              
         DCDDL GE#NORUN,L'PCUNORUN                                              
         DCDDL GE#NOSET,L'PCUNOSET                                              
         DCDDL GE#BUILD,L'PCUBUILD                                              
         DCDDL GE#NONE,L'PCUNONE                                                
         DCDDL GE#RUNDS,L'PCURUND                                               
         DCDDL GE#RDAYS,L'PCURDAYS                                              
                                                                                
DICUPRX  DC    AL1(EOT)                                                         
                                                                                
DICMIX   DS    0XL4                ** MIXED CASE DICTIONARY LIST **             
                                                                                
         DCDDL GE#WKLY,L'PCMWKLY                                                
         DCDDL GE#MTHLY,L'PCMMTHLY                                              
         DCDDL GE#QTRLY,L'PCMQTRLY                                              
         DCDDL GE#YRLY,L'PCMYRLY                                                
         DCDDL GE#IRREG,L'PCMIRREG                                              
         DCDDL GE#NOTRG,L'PCMNOTRG                                              
         DCDDL GE#GROUP,L'PCMGROUP                                              
         DCDDL GE#REQST,L'PCMREQST                                              
         DCDDL GE#LIS,L'PCMLIS                                                  
         DCDDL GE#SEL,L'PCMSEL                                                  
         DCDDL GE#DIS,L'PCMDIS                                                  
         DCDDL GE#UPDTE,L'PCMUPDTE                                              
         DCDDL GE#DSKAD,L'PCMDSKAD                                              
         DCDDL GE#RNUM,L'PCMRNUM                                                
         DCDDL GE#SYSTM,L'PCMSYSTM                                              
         DCDDL GE#AGY,L'PCMAGY                                                  
         DCDDL GE#USER#,L'PCMUSER#                                              
         DCDDL GE#DEST#,L'PCMDEST#                                              
         DCDDL GE#USRID,L'PCMUSRID                                              
         DCDDL GE#DSTID,L'PCMDSTID                                              
         DCDDL GE#FRQCY,L'PCMFRQCY                                              
         DCDDL GE#OTYP,128+L'PCMOTYPC                                           
         DCDDL GE#GNAME,128+L'PCMGNAMC                                          
         DCDDL GE#NRUND,128+L'PCMNRUNC                                          
         DCDDL GE#LRUND,128+L'PCMLRUNC                                          
         DCDDL GE#ERUND,128+L'PCMERUNC                                          
         DCDDL GE#XFILE,L'PCMXFILE                                              
         DCDDL GE#UP,L'PCMUP                                                    
         DCDDL GE#DOWN,L'PCMDOWN                                                
         DCDDL GE#LEFT,L'PCMLEFT                                                
         DCDDL GE#RIGHT,L'PCMRIGHT                                              
         DCDDL GE#ALTPF,L'PCMALTPF                                              
         DCDDL GE#DESC,L'PCMDESC                                                
         DCDDL GE#ADD,L'PCMADD                                                  
         DCDDL GE#NEXT,L'PCMNEXT                                                
         DCDDL GE#QUIT,L'PCMQUIT                                                
         DCDDL GE#CHA,L'PCMCHA                                                  
         DCDDL GE#DEL,L'PCMDEL                                                  
         DCDDL GE#OTYP,L'PCMOTYP                                                
         DCDDL GE#AGY,L'PCMAGY2                                                 
         DCDDL GE#YES,L'PCMYES                                                  
         DCDDL GE#NO,L'PCMNO                                                    
         DCDDL GE#RUNQ,L'PCMRUNQ                                                
         DCDDL GE#ACTV,L'PCMACTV                                                
         DCDDL GE#GLOBL,L'PCMGLOBL                                              
         DCDDL GE#CALDR,L'PCMCALDR                                              
         DCDDL GE#JAN,L'PCMJAN                                                  
         DCDDL GE#FEB,L'PCMFEB                                                  
         DCDDL GE#MAR,L'PCMMAR                                                  
         DCDDL GE#APR,L'PCMAPR                                                  
         DCDDL GE#MAY,L'PCMMAY                                                  
         DCDDL GE#JUN,L'PCMJUN                                                  
         DCDDL GE#JUL,L'PCMJUL                                                  
         DCDDL GE#AUG,L'PCMAUG                                                  
         DCDDL GE#SEP,L'PCMSEP                                                  
         DCDDL GE#OCT,L'PCMOCT                                                  
         DCDDL GE#NOV,L'PCMNOV                                                  
         DCDDL GE#DEC,L'PCMDEC                                                  
         DCDDL GE#SBMIT,L'PCMSBMIT                                              
         DCDDL GE#NRUND,L'PCMNRUND                                              
         DCDDL GE#EFDT,L'PCMEFFDT                                               
         DCDDL GE#RUNSH,L'PCMRUNSH                                              
         DCDDL GE#MON,L'PCMMON                                                  
         DCDDL GE#TUE,L'PCMTUE                                                  
         DCDDL GE#WED,L'PCMWED                                                  
         DCDDL GE#THU,L'PCMTHU                                                  
         DCDDL GE#FRI,L'PCMFRI                                                  
         DCDDL GE#SAT,L'PCMSAT                                                  
         DCDDL GE#SUN,L'PCMSUN                                                  
         DCDDL GE#VACTS,L'PCMVACTS                                              
         DCDDL GE#REP,L'PCMREP                                                  
         DCDDL GE#KEYWD,L'PCMKEYWD                                              
         DCDDL GE#RQDET,L'PCMRQDET                                              
         DCDDL GE#REQR,L'PCMREQR                                                
         DCDDL GE#GROUP,128+L'PCMGRPC                                           
         DCDDL GE#XFILE,128+L'PCMXFGC                                           
         DCDDL GE#STAT,L'PCMSTAT                                                
         DCDDL GE#DEL,L'PCM3DEL                                                 
         DCDDL GE#PRVT,L'PCM3PRVT                                               
         DCDDL GE#PRSN,128+L'PCMPRSNC                                           
         DCDDL GE#PAGE,L'PCMPAGE                                                
         DCDDL GE#HALF,L'PCMHALF                                                
         DCDDL GE#MAX,L'PCMMAX                                                  
         DCDDL GE#CPY,L'PCMCPY                                                  
         DCDDL GE#SOFTD,L'PCMSOFTD                                              
         DCDDL GE#RUNDS,L'PCMRUND                                               
         DCDDL GE#RDAYS,L'PCMRDAYS                                              
                                                                                
DICMIXX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD TYPE TABLE                                                   *         
***********************************************************************         
                                                                                
RECTAB   DS    0X                                                               
                                                                                
         DC    AL1(RECGRP)                                                      
         DC    AL2(PCUGROUP-WORKD,PCMGROUP-WORKD)                               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RECREQ)                                                      
         DC    AL2(PCUREQST-WORKD,PCMREQST-WORKD)                               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(RECXFG)                                                      
         DC    AL2(PCUXFILE-WORKD,PCMXFILE-WORKD)                               
         DC    AL1(0,0)                                                         
         DC    AL1(0)                                                           
                                                                                
RECTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ACTION TABLE                                                        *         
***********************************************************************         
                                                                                
ACTTAB   DS    0X                                                               
                                                                                
         DC    AL1(ACTLST)                                                      
         DC    AL2(PCULIS-WORKD,PCMLIS-WORKD)                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTDIS)                                                      
         DC    AL2(PCUDIS-WORKD,PCMDIS-WORKD)                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTCHA)                                                      
         DC    AL2(PCUCHA-WORKD,PCMCHA-WORKD)                                   
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTADD)                                                      
         DC    AL2(PCUADD-WORKD,PCMADD-WORKD)                                   
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTDEL)                                                      
         DC    AL2(PCUDEL-WORKD,PCMDEL-WORKD)                                   
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTMOV)                                                      
         DC    AL2(PCUMOVE-WORKD,PCUMOVE-WORKD)                                 
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTCPY)                                                      
         DC    AL2(PCUCOPY-WORKD,PCUCOPY-WORKD)                                 
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTGLO)                                                      
         DC    AL2(PCUGLOBL-WORKD,PCMGLOBL-WORKD)                               
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTCAL)                                                      
         DC    AL2(PCUCALDR-WORKD,PCMCALDR-WORKD)                               
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTSUB)                                                      
         DC    AL2(PCUSBMIT-WORKD,PCMSBMIT-WORKD)                               
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTSOF)                                                      
         DC    AL2(PCUSOFTD-WORKD,PCMSOFTD-WORKD)                               
         DC    AL1(ACTIUPD,0)                                                   
                                                                                
         DC    AL1(ACTREP)                                                      
         DC    AL2(PCUREP-WORKD,PCMREP-WORKD)                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTAPF)                                                      
         DC    AL2(PCMALTPF-WORKD,PCMALTPF-WORKD)                               
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTNXT)                                                      
         DC    AL2(PCMNEXT-WORKD,PCMNEXT-WORKD)                                 
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTQUI)                                                      
         DC    AL2(PCMQUIT-WORKD,PCMQUIT-WORKD)                                 
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTBLD)                                                      
         DC    AL2(PCUBUILD-WORKD,PCUBUILD-WORKD)                               
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(ACTRUN)                                                      
         DC    AL2(PCURUND-WORKD,PCMRUND-WORKD)                                 
         DC    AL1(0,0)                                                         
                                                                                
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD/ACTION COMBO TABLE                                           *         
***********************************************************************         
                                                                                
MIXTAB   DS    0X                                                               
                                                                                
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(0,0,0)                                                       
                                                                                
         DC    AL1(RECGRP,ACTADD)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(1,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(2,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(3,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,SUBOVERQ)                                                  
         DC    AL1(3,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTDEL)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(4,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTBLD)                                               
         DC    AL1(MIXIINT,0)                                                   
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(5,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,SUBOVERQ)                                                  
         DC    AL1(0,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTRUN)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,CALOVERQ)                                                  
         DC    AL1(0,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,CALOVERQ)                                                  
         DC    AL1(0,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTREP)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,REPOVERQ)                                                  
         DC    AL1(0,RECGRP,0)                                                  
                                                                                
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,REQOVERQ)                                                  
         DC    AL1(0,0,0)                                                       
                                                                                
         DC    AL1(RECREQ,ACTDIS)                                               
         DC    AL1(MIXISEL+MIXISUB,0)                                           
         DC    AL1(0,REQOVERQ)                                                  
         DC    AL1(1,RECREQ,0)                                                  
                                                                                
         DC    AL1(RECREQ,ACTDEL)                                               
         DC    AL1(MIXISEL+MIXISUB,0)                                           
         DC    AL1(0,REQOVERQ)                                                  
         DC    AL1(2,RECREQ,0)                                                  
                                                                                
         DC    AL1(RECREQ,ACTMOV)                                               
         DC    AL1(MIXISEL+MIXISUB,0)                                           
         DC    AL1(0,REQOVERQ)                                                  
         DC    AL1(3,RECREQ,0)                                                  
                                                                                
         DC    AL1(RECREQ,ACTCPY)                                               
         DC    AL1(MIXISEL+MIXISUB,0)                                           
         DC    AL1(0,REQOVERQ)                                                  
         DC    AL1(4,RECREQ,0)                                                  
                                                                                
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(MIXISEL,0)                                                   
         DC    AL1(0,REQOVERQ)                                                  
         DC    AL1(5,RECGRP,4)                                                  
                                                                                
         DC    AL1(RECXFG,ACTLST)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(0,0,0)                                                       
                                                                                
         DC    AL1(RECXFG,ACTADD)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(1,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECXFG,ACTCHA)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(2,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECXFG,ACTDIS)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(3,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECXFG,ACTDEL)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(4,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECGRP,ACTBLD)                                               
         DC    AL1(MIXIINT,0)                                                   
         DC    AL1(0,GRPOVERQ)                                                  
         DC    AL1(5,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECXFG,ACTSUB)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,SUBOVERQ)                                                  
         DC    AL1(3,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,CALOVERQ)                                                  
         DC    AL1(0,RECXFG,0)                                                  
                                                                                
         DC    AL1(RECXFG,ACTREP)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,REPOVERQ)                                                  
         DC    AL1(0,RECXFG,0)                                                  
                                                                                
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PROGRAM FUNCTION KEY TABLE                                          *         
***********************************************************************         
                                                                                
PFKTAB   DS    0X                                                               
                                                                                
PFIRSTS  DC    AL1(FF,FF)                                                       
         DC    AL2(PFIRSTSX+1-PFIRSTS)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKXLSTQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECXFG,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKCHARQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKADDRQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECGRP,ACTADD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PFIRSTSX DC    AL1(EOT)                                                         
                                                                                
PGRPLST  DC    AL1(RECGRP,ACTLST)                                               
         DC    AL2(PGRPLSTX+1-PGRPLST)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKADDRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTADD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKCHARQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,0,0,0)                               
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSBMTQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDELRQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTDEL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKXLSTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSOFTQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(PCMUP-WORKD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(PCMDOWN-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(PCMLEFT-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(PCMRIGHT-WORKD)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PGRPLSTX DC    AL1(EOT)                                                         
                                                                                
PGRPSUB  DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL2(PGRPSUBX+1-PGRPSUB)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKCHARQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(PCMUP-WORKD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(PCMDOWN-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PGRPSUBX DC    AL1(EOT)                                                         
                                                                                
PGRPSOF  DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL2(PGRPSOFX+1-PGRPSOF)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKRUNDQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTRUN)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKCHARQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(PCMUP-WORKD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(PCMDOWN-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PGRPSOFX DC    AL1(EOT)                                                         
                                                                                
PGRPDIS  DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL2(PGRPDISX+1-PGRPDIS)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKSOFTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSBMTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PGRPDISX DC    AL1(EOT)                                                         
                                                                                
PGRPADD  DC    AL1(RECGRP,ACTADD)                                               
         DC    AL2(PGRPADDX+1-PGRPADD)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLST2Q)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKSOFTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSBMTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PGRPADDX DC    AL1(EOT)                                                         
                                                                                
PGRPCHA  DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL2(PGRPCHAX+1-PGRPCHA)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKSOFTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSBMTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PGRPCHAX DC    AL1(EOT)                                                         
                                                                                
PGRPDEL  DC    AL1(RECGRP,ACTDEL)                                               
         DC    AL2(PGRPDELX+1-PGRPDEL)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PGRPDELX DC    AL1(EOT)                                                         
                                                                                
PGRPCAL  DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL2(PGRPCALX+1-PGRPCAL)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKRUNDQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTRUN)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTDIS)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PGRPCALX DC    AL1(EOT)                                                         
                                                                                
PGRPRUN  DC    AL1(RECGRP,ACTRUN)                                               
         DC    AL2(PGRPRUNX+1-PGRPRUN)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PGRPRUNX DC    AL1(EOT)                                                         
                                                                                
PGRPREP  DC    AL1(RECGRP,ACTREP)                                               
         DC    AL2(PGRPREPX+1-PGRPREP)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PGRPREPX DC    AL1(EOT)                                                         
                                                                                
PXFGLST  DC    AL1(RECXFG,ACTLST)                                               
         DC    AL2(PXFGLSTX+1-PXFGLST)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKADDRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTADD)                                               
         DC    AL1(PFKQUITQ,0,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKCHARQ)                                                    
         DC    AL1(PFKISUBA,0)                                                  
         DC    AL1(RECXFG,ACTCHA)                                               
         DC    AL1(PFKQUITQ,0,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKISUBA,0)                                                  
         DC    AL1(RECXFG,ACTDIS)                                               
         DC    AL1(PFKQUITQ,0,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKISUBA,0)                                                  
         DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKSBMTQ)                                                    
         DC    AL1(PFKISUBA,0)                                                  
         DC    AL1(RECXFG,ACTSUB)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKDELRQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTDEL)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,0,0,0)                               
                                                                                
         DC    AL1(PFKREPTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTREP)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(PCMUP-WORKD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(PCMDOWN-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(PCMLEFT-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(PCMRIGHT-WORKD)                                              
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PXFGLSTX DC    AL1(EOT)                                                         
                                                                                
PXFGSUB  DC    AL1(RECXFG,ACTSUB)                                               
         DC    AL2(PXFGSUBX+1-PXFGSUB)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTDIS)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(PCMUP-WORKD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(PCMDOWN-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PXFGSUBX DC    AL1(EOT)                                                         
                                                                                
PXFGADD  DC    AL1(RECXFG,ACTADD)                                               
         DC    AL2(PXFGADDX+1-PXFGADD)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKXLSTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PXFGADDX DC    AL1(EOT)                                                         
                                                                                
PXFGDIS  DC    AL1(RECXFG,ACTDIS)                                               
         DC    AL2(PXFGDISX+1-PXFGDIS)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PXFGDISX DC    AL1(EOT)                                                         
                                                                                
PXFGDEL  DC    AL1(RECXFG,ACTDEL)                                               
         DC    AL2(PXFGDELX+1-PXFGDEL)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PXFGDELX DC    AL1(EOT)                                                         
                                                                                
PXFGCHA  DC    AL1(RECXFG,ACTCHA)                                               
         DC    AL2(PXFGCHAX+1-PXFGCHA)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PXFGCHAX DC    AL1(EOT)                                                         
                                                                                
PXFGCAL  DC    AL1(RECXFG,ACTCAL)                                               
         DC    AL2(PXFGCALX+1-PXFGCAL)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKDISRQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECXFG,ACTDIS)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PXFGCALX DC    AL1(EOT)                                                         
                                                                                
PXFGREP  DC    AL1(RECXFG,ACTREP)                                               
         DC    AL2(PXFGREPX+1-PXFGREP)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PXFGREPX DC    AL1(EOT)                                                         
                                                                                
PREQLST  DC    AL1(RECREQ,ACTLST)                                               
         DC    AL2(PREQLSTX+1-PREQLST)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKGLOBQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKDIS2Q)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTDIS)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,0,0,0)                               
                                                                                
         DC    AL1(PFKDELRQ)                                                    
         DC    AL1(PFKISUBA,PFKISAVS)                                           
         DC    AL1(RECREQ,ACTDEL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(PCMUP-WORKD)                                                 
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(PCMDOWN-WORKD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKCHARQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCHA)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSOFTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSOF)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKVSCHQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTCAL)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
         DC    AL1(PFKSBMTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECGRP,ACTSUB)                                               
         DC    AL1(0,CSIUSELC,0,0,0,0,0,0)                                      
                                                                                
PREQLSTX DC    AL1(EOT)                                                         
                                                                                
PREQDIS  DC    AL1(RECREQ,ACTDIS)                                               
         DC    AL2(PREQDISX+1-PREQDIS)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PREQDISX DC    AL1(EOT)                                                         
                                                                                
PREQGLO  DC    AL1(RECREQ,ACTGLO)                                               
         DC    AL2(PREQGLOX+1-PREQGLO)                                          
         DC    AL1(0,0,0,0)                                                     
                                                                                
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
         DC    AL1(PFKLSTRQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECGRP,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
                                                                                
PREQGLOX DC    AL1(EOT)                                                         
                                                                                
PFKTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SELECT FIELD ACTION TABLE                                           *         
***********************************************************************         
                                                                                
SELTAB   DS    0X                                                               
                                                                                
SGRPLST  DC    AL1(RECGRP,ACTLST)                                               
         DC    AL2(SGRPLSTX+1-SGRPLST)                                          
                                                                                
         DC    AL2(PCUDIS-WORKD)                                                
         DC    AL2(PCMDIS-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIDEF)                                                    
         DC    AL2(0)                                                           
         DC    AL1(PFKDISRQ,0)                                                  
         DC    AL1(RECGRP,ACTDIS,1,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUSEL-WORKD)                                                
         DC    AL2(PCMSEL-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIDEF)                                                    
         DC    AL2(0)                                                           
         DC    AL1(PFKDISRQ,0)                                                  
         DC    AL1(RECGRP,ACTDIS,1,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUCHA-WORKD)                                                
         DC    AL2(PCMCHA-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKCHARQ,0)                                                  
         DC    AL1(RECGRP,ACTCHA,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUDEL-WORKD)                                                
         DC    AL2(PCMDEL-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKDELRQ,0)                                                  
         DC    AL1(RECGRP,ACTDEL,2,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCULIS-WORKD)                                                
         DC    AL2(PCMLIS-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKLSTRQ,SELTICLR)                                           
         DC    AL1(RECREQ,ACTLST,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUGLOBL-WORKD)                                              
         DC    AL2(PCMGLOBL-WORKD)                                              
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKGLOBQ,SELTICLR)                                           
         DC    AL1(RECREQ,ACTGLO,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUCALDR-WORKD)                                              
         DC    AL2(PCMCALDR-WORKD)                                              
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKVSCHQ,SELTICLR)                                           
         DC    AL1(RECGRP,ACTCAL,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUSBMIT-WORKD)                                              
         DC    AL2(PCMSBMIT-WORKD)                                              
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(GRPMSUBQ)                                                    
         DC    AL1(PFKSBMTQ,SELTICLR)                                           
         DC    AL1(RECGRP,ACTSUB,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUREP-WORKD)                                                
         DC    AL2(PCMREP-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,SELTICLR)                                                  
         DC    AL1(RECGRP,ACTREP,1,0)                                           
         DC    AL1(CSIUSELC,0)                                                  
                                                                                
         DC    AL2(PCUSOFTD-WORKD)                                              
         DC    AL2(PCMSOFTD-WORKD)                                              
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKSOFTQ,SELTICLR)                                           
         DC    AL1(RECGRP,ACTSOF,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
SGRPLSTX DC    AL1(EOT)                                                         
                                                                                
SXFGLST  DC    AL1(RECXFG,ACTLST)                                               
         DC    AL2(SXFGLSTX+1-SXFGLST)                                          
                                                                                
         DC    AL2(PCUDIS-WORKD)                                                
         DC    AL2(PCMDIS-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIDEF)                                                    
         DC    AL2(0)                                                           
         DC    AL1(PFKDISRQ,0)                                                  
         DC    AL1(RECXFG,ACTDIS,1,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUSEL-WORKD)                                                
         DC    AL2(PCMSEL-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIDEF)                                                    
         DC    AL2(0)                                                           
         DC    AL1(PFKDISRQ,0)                                                  
         DC    AL1(RECXFG,ACTDIS,1,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUCHA-WORKD)                                                
         DC    AL2(PCMCHA-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKCHARQ,0)                                                  
         DC    AL1(RECXFG,ACTCHA,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUDEL-WORKD)                                                
         DC    AL2(PCMDEL-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKDELRQ,0)                                                  
         DC    AL1(RECXFG,ACTDEL,2,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUCALDR-WORKD)                                              
         DC    AL2(PCMCALDR-WORKD)                                              
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKVSCHQ,SELTICLR)                                           
         DC    AL1(RECXFG,ACTCAL,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUSBMIT-WORKD)                                              
         DC    AL2(PCMSBMIT-WORKD)                                              
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(GRPMSUBQ)                                                    
         DC    AL1(PFKSBMTQ,SELTICLR)                                           
         DC    AL1(RECXFG,ACTSUB,1,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUREP-WORKD)                                                
         DC    AL2(PCMREP-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0,SELTICLR)                                                  
         DC    AL1(RECXFG,ACTREP,1,0)                                           
         DC    AL1(CSIUSELC,0)                                                  
                                                                                
SXFGLSTX DC    AL1(EOT)                                                         
                                                                                
SREQLST  DC    AL1(RECREQ,ACTLST)                                               
         DC    AL2(SREQLSTX+1-SREQLST)                                          
                                                                                
         DC    AL2(PCUDIS-WORKD)                                                
         DC    AL2(PCMDIS-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIDEF)                                                    
         DC    AL2(0)                                                           
         DC    AL1(PFKDIS2Q,0)                                                  
         DC    AL1(RECREQ,ACTDIS,3,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUSEL-WORKD)                                                
         DC    AL2(PCMSEL-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIDEF)                                                    
         DC    AL2(0)                                                           
         DC    AL1(PFKDIS2Q,0)                                                  
         DC    AL1(RECREQ,ACTDIS,3,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUDEL-WORKD)                                                
         DC    AL2(PCMDEL-WORKD)                                                
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKDELRQ,0)                                                  
         DC    AL1(RECREQ,ACTDEL,2,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUMOVE-WORKD)                                               
         DC    AL2(PCUMOVE-WORKD)                                               
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIRHS)                                                    
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECREQ,ACTMOV,2,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUCOPY-WORKD)                                               
         DC    AL2(PCUCOPY-WORKD)                                               
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(SELTIRHS)                                                    
         DC    AL2(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(RECREQ,ACTCPY,1,PFKNEXTQ)                                    
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUGLOBL-WORKD)                                              
         DC    AL2(PCMGLOBL-WORKD)                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKGLOBQ,SELTICLR)                                           
         DC    AL1(RECREQ,ACTGLO,4,0)                                           
         DC    XL2'00'                                                          
                                                                                
         DC    AL2(PCUSBMIT-WORKD)                                              
         DC    AL2(PCMSBMIT-WORKD)                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(PFKSBMTQ,SELTICLR)                                           
         DC    AL1(RECGRP,ACTSUB,4,0)                                           
         DC    XL2'00'                                                          
                                                                                
SREQLSTX DC    AL1(EOT)                                                         
                                                                                
SELTABX  DC    AL1(EOT)                                                         
                                                                                
DAYSINWQ EQU   7                   N'DAYS IN WEEK                               
DAYSINMQ EQU   31                  N'DAYS IN MONTH                              
                                                                                
DMREAD   DC    C'DMREAD'                                                        
DMWRITE  DC    C'DMWRT  '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
                                                                                
MAXDAYS  DC    P'366'                                                           
DAYBITS  DC    X'8040201008040201'                                              
PONE     DC    P'1'                                                             
                                                                                
DAYS31   DC    AL1(GRDCONQ,GRDTDATQ),X'00000002',X'0000'                        
DAYS1531 DC    AL1(GRDCONQ,GRDTDATQ),X'00020002',X'0000'                        
         EJECT                                                                  
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
                                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
                                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
                                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
                                                                                
* GEDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGETRETD                                                      
GETRETL  EQU   *-GETRETD                                                        
         PRINT ON                                                               
         EJECT                                                                  
* GERLPWORK                                                                     
       ++INCLUDE GERLPWORK                                                      
                                                                                
CTLSTD   DSECT                     ** DATA FIELDS FOR LIST ELEMENT **           
         ORG   CTLSTDTA                                                         
CTGIDCOD DS    CL(L'GIDCODE)                                                    
CTGIDNUM DS    CL(L'GIDNUM)                                                     
CTGIDLNQ EQU   *-CTLSTD                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041GERLP00   09/29/20'                                      
         END                                                                    
