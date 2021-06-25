*          DATA SET ACBAT00B   AT LEVEL 049 AS OF 12/27/12                      
*PHASE T61B00BA                                                                 
*INCLUDE AMTVALA                                                                
*INCLUDE CONVMOS                                                                
*INCLUDE BMONVAL                                                                
*INCLUDE VATICAN                                                                
*INCLUDE ACJOBCOL                                                               
*INCLUDE CATCALL                                                                
*INCLUDE EXCEL                                                                  
*&&US                                                                           
*INCLUDE PRORATA                                                                
*&&                                                                             
BAT00    TITLE '- BATCH PROGRAM ROOT'                                           
BAT00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**BAT0**,R8,R7,R6,CLEAR=YES,RR=RE                          
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ADDRESSES                                                *         
***********************************************************************         
         SPACE 1                                                                
INITAD   ST    RB,BCBASER1         SAVE CONTROLLER BASE REGISTERS               
         ST    R8,BCBASER2                                                      
         ST    R7,BCBASER3                                                      
         ST    R6,BCBASER4                                                      
         MVC   CUABIN,0(R1)        COMPANY CODE                                 
         MVC   AINP,00(R1)         A(TIOB)                                      
         MVC   ATWA,04(R1)         A(TWA)                                       
         MVC   ASYS,08(R1)         A(SYSTEM FACILITY LIST)                      
         MVC   ATIA,12(R1)         A(TIA)                                       
         MVC   ACOM,16(R1)         A(COMMON FACILITY LIST)                      
         L     R1,20(R1)           R1=A(FACPAK EXTRA INFORMATION)               
         MVC   CUCTRY,1(R1)        SET AGENCY COUNTRY                           
         MVC   CULANG,3(R1)                                                     
         MVC   FACFLAG,7(R1)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(R1)       AND UPDATIVE FACPAK ID                       
*                                                                               
         ST    RE,BCRELO                                                        
         ST    RD,BCSVRD                                                        
*                                                                               
         L     R1,ACOM                                                          
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
         MVC   VSCUNKEY,CSCUNKEY                                                
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VMINIO,CMINIO                                                    
         MVC   VPARSNIP,CPARSNIP                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   VDICTAT,CDICTATE                                                 
         MVC   VGETPROF,CGETPROF                                                
         MVC   VEDITOR,CEDITOR                                                  
         MVC   VSECRET,CSECRET                                                  
         MVC   VGETHELP,CGETHELP                                                
*&&UK*&& MVC   VCONVERT,CCONVERT                                                
*&&UK*&& MVC   VPRORATA,CPRORATA                                                
*&&US                                                                           
         L     RF,=V(PRORATA)                                                   
         A     RF,BCRELO                                                        
         ST    RF,VPRORATA                                                      
*&&                                                                             
         MVC   VGETCUR,CGETCUR                                                  
         MVC   VBLDCUR,CBLDCUR                                                  
         DROP  R1                                                               
*                                                                               
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BCRELO                                                        
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAXIMUM NUMBER OF PHASES                  
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,BCPARM                                                        
         L     RF,VCOLY                                                         
INITAD02 CLI   0(R2),FF                                                         
         BE    INITAD04                                                         
         ICM   R0,1,0(R2)                                                       
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,INITAD02                                                      
*                                                                               
INITAD04 LA    R0,ADDRSN           SET CONTROLLER ADDRESSES                     
         SR    RE,RE               RE=INDEX TO ADDRESS VALUE                    
         L     RF,=A(ADDRS)                                                     
         A     RF,BCRELO                                                        
         L     R1,0(RF,RE)                                                      
         A     R1,BCRELO           RELOCATE AND STORE IN W/S                    
         ST    R1,AADDRESS(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         LA    R0,AROUTS1N         SET ROUTINE ADDRESSES - 1                    
         L     RF,VROUT1                                                        
         SR    RE,RE                                                            
         LA    R1,AROUTS1                                                       
INITAD06 ST    RF,0(R1)            SET A(ROUTS)                                 
         STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INITAD06                                                      
*                                                                               
         LA    R0,AROUTS2N         SET ROUTINE ADDRESSES - 2                    
         L     RF,VROUT2                                                        
         SR    RE,RE                                                            
         LA    R1,AROUTS2                                                       
INITAD08 ST    RF,0(R1)            SET A(ROUTS)                                 
         STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INITAD08                                                      
*                                                                               
         LA    R0,AROUTS3N         SET ROUTINE ADDRESSES - 3                    
         L     RF,VROUT3                                                        
         SR    RE,RE                                                            
         LA    R1,AROUTS3                                                       
INITAD10 ST    RF,0(R1)            SET A(ROUTS)                                 
         STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INITAD10                                                      
*                                                                               
         L     R1,=A(ANAWS)        SET SUNDRY STORAGE ADDRESSES                 
         A     R1,BCRELO                                                        
         LA    R0,ANAWSN                                                        
INITAD12 ICM   RE,15,0(R1)         RE=AL2(AREA),AL2(ADDRESS)                    
         SRDL  RE,16                                                            
         SRL   RF,16                                                            
         LA    RE,WORKD(RE)                                                     
         LA    RF,WORKD(RF)                                                     
         STCM  RE,15,0(RF)         SET AREA ADDRESS                             
         LA    R1,L'ANAWS(R1)                                                   
         BCT   R0,INITAD12                                                      
*                                                                               
         LH    RE,=Y(OSVALS2-TWAD) SET A(OVERLAY SECONDARY STORAGE)             
         A     RE,ATWA                                                          
         ST    RE,BOSVALS2                                                      
*                                                                               
         LH    RE,=Y(SECBLK-TWAD)  SET A(SECURITY ACCESS BLOCK)                 
         A     RE,ATWA                                                          
         ST    RE,ASECBLK                                                       
*                                                                               
         LA    R0,COLTABL          SET TABLE LENGTHS                            
         ST    R0,LCOLTAB                                                       
         LA    R0,OPVTABL                                                       
         ST    R0,LOPVTAB                                                       
*                                                                               
INITADX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GENERAL VALUES                                           *         
***********************************************************************         
         SPACE 1                                                                
INITGV   L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         LH    R5,=Y(BSVALS-TWAD)                                               
         LA    R5,TWAD(R5)                                                      
         USING BSVALS,R5           R5=A(TWA SAVE AREA)                          
         MVI   BCOVSYS,X'06'                                                    
         MVI   BCPRGNO,X'1B'                                                    
         MVC   BCDSPREC,=AL2(BASRECH-TWAD)                                      
         MVC   BCDSPACT,=AL2(BASACTH-TWAD)                                      
         MVC   BCDSPSCR,=AL2(BASSCRH-TWAD)                                      
         MVC   BCDSPREP,=AL2(BASREPH-TWAD)                                      
         MVC   BCDSPSAV,=AL2(BASOPTH-TWAD)                                      
         MVC   BCDTADSP,=Y(ACCORFST)                                            
*&&UK*&& MVC   BCSYSTEM,=C'UK'                                                  
*&&US*&& MVC   BCSYSTEM,=C'US'                                                  
         MVI   BCEFFS,FF                                                        
         MVC   BCEFFS+1(L'BCEFFS-1),BCEFFS                                      
         MVI   BCSPACES,C' '                                                    
         MVC   BCSPACES+1(L'BCSPACES-1),BCSPACES                                
         ZAP   BCPZERO,=P'0'                                                    
         MVC   BCDSPOPT,=AL2(BASOPTH-TWAD)                                      
         MVC   BCDSPOVR,=AL2(BASOLY1H-TWAD)                                     
         MVI   BCHELPSO,HELPSCRN                                                
         MVC   CUACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   CUUSER,TWAUSRID     USER-ID NUMBER                               
         MVC   CUAUTH,TWAAUTH      AUTHORIZATION CODE                           
         MVC   CUAALF,TWAAGY       AGENCY ALPHA-ID                              
         GOTO1 VGETFACT,BCPARM,0                                                
         L     R2,0(R1)                                                         
         USING FACTSD,R2           R1=A(SYSTEM DEFINITION BLOCK)                
         MVC   ACTRY,FAACTRY       SET A(COUNTRY & LANGUAGE TABLES)             
         MVC   ALANG,FAALANG                                                    
         MVI   ASONOFF,ASON        SET ONLINE OR OFFLINE SWITCH                 
*                                                                               
         MVC   CUTSYM,FASYM                                                     
         MVC   ASEDAT,FADATE       SYSTEM DATES (VARIOUS FORMATS)               
         MVC   ASBDAT,FADATEB                                                   
         CLI   TWAOFFC,C'*'        TEST DDS OFFICE CODE                         
         BNE   *+8                                                              
         MVI   CUSTAT,CUSDDS       SET DDS STATUS INDICATOR                     
         OC    CUPASS,FAPASSWD     TEST CONNECTED WITH PASSWORD                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    FATFLAG,X'08'       YES - TEST PERSONAL PASSWORD                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    CUSTAT,CUSPER                                                    
INITGV02 MVC   ASTIME,FATIME       SYSTEM TIME (STANDARD FORMAT)                
         MVC   ASSYSN,FASYS        SYSTEM NUMBER                                
         MVC   ASSYSO,FAOVSYS      SYSTEM NUMBER (FOR CALLOV)                   
         MVC   ASSIN,FASIN         SYSTEM INPUT NUMBER                          
         MVC   ASIOASTR,FAIOASTR   SYSTEM EXTRA AREA ADDRESS                    
         MVC   ASIOALEN,FAIOALEN   SYSTEM EXTRA AREA LENGTH                     
*                                                                               
         MVC   BCPARM(4),BCEFFS    SET A(UTL ENTRY)                             
         GOTO1 VSWITCH,BCPARM                                                   
         MVC   BCAUTL,0(R1)                                                     
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6SCRP    IS SCRIPT RUNNING?                   
         BZ    INITGV03                                                         
         L     RF,4(RD)            GO BACK IN D-CHAIN                           
         SH    RD,=Y(WORKXTRA)     MODIFY D-CHAIN                               
         ST    RF,4(RD)            RE-ESTABLISH BACK POINTER                    
         XC    8(4,RD),8(RD)       CLEAR FORWARD POINTER                        
         ST    RD,8(RF)            SAVE NEW PREV FORWARD POINTER                
         ST    RD,BCSVRD           HAVE TO RESET THIS                           
*                                                                               
INITGV03 MVC   BCPARM(4),BCEFFS    SET A(SYSFACS)                               
         MVI   BCPARM,X'FE'                                                     
         GOTO1 VSWITCH,BCPARM                                                   
         MVC   ASYSFAC,0(R1)                                                    
         DROP  R2                                                               
*                                                                               
         L     R1,BCAUTL           AGENCY ALPHA VALUE FOR SECURITY              
         MVC   CUASEC,TAGYSEC-UTLD(R1)                                          
*                                                                               
         GOTO1 VGETFACT,BCPARM,(X'80',0),F#TCBD                                 
         L     R1,0(R1)                                                         
         USING F@TCBD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,F@BSWNUM       R0=N'ENTRIES IN TCBSWTAB                     
         BZ    INITGV06                                                         
         CLM   R0,1,=AL1(SYSSWMAX)                                              
         BNH   *+8                                                              
         LA    R0,SYSSWMAX                                                      
         LA    R1,F@BSWTAB                                                      
         USING F@BSWTAB,R1         R1=A(TCB SWITCH TABLE)                       
         L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(LOCAL SWITCH TABLE)                     
INITGV04 MVC   SYSSWSYS,F@BSWSYS                                                
         MVC   SYSSWSOV,F@BSWSOV                                                
         MVC   SYSSWAGB,F@BSWAGB                                                
         MVC   SYSSWACS,F@BSWACS                                                
         MVC   SYSSWAC2,F@BSWAC2                                                
         LA    R1,F@BSWLEN(R1)     BUMP TO NEXT                                 
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,INITGV04         DO FOR NUMBER OF ENTRIES                     
         DROP  R1,RE                                                            
*                                                                               
INITGV06 L     R1,AINP             EXTRACT PFKEY NUMBER                         
         USING TIOBD,R1                                                         
         ICM   RE,1,TIOBAID                                                     
         BZ    *+12                                                             
         OI    BCINDS1,BCIANYPF    SET USER ENTERED PF KEY THIS TIME            
         STC   RE,BCPFKEY          AND SET PFKEY NUMBER                         
         SR    RE,RE                                                            
         ICM   RE,3,TIOBCURD                                                    
         LA    RE,TWAD(RE)                                                      
         ST    RE,BCACUR           SET A(CURSOR)                                
         DROP  R1                                                               
*                                                                               
         CLI   CULANG,0            SET DEFAULT LANGUAGE                         
         BNE   *+8                                                              
*&&UK*&& MVI   CULANG,LANGEUK                                                   
*&&US*&& MVI   CULANG,LANGEUS                                                   
         SR    R1,R1                                                            
         IC    R1,CULANG                                                        
         SLL   R1,3                                                             
         L     R0,=A(LANGCHAR-L'LANGCHAR)                                       
         A     R0,BCRELO                                                        
         AR    R1,R0                                                            
         MVC   BCCHARS,0(R1)       SET SPECIAL CHARACTERS                       
         CLI   CUCTRY,0            SET DEFAULT COUNTRY                          
         BNE   *+8                                                              
*&&UK*&& MVI   CUCTRY,CTRYGBR                                                   
*&&US*&& MVI   CUCTRY,CTRYUSA                                                   
         CLI   CUCTRY,CTRYSCA      CONVERT SCANDINAVIA INTO UK                  
         BNE   *+8                                                              
         MVI   CUCTRY,CTRYGBR                                                   
*                                                                               
         SR    RF,RF               SET COUNTRY/LANGUAGE ATTRIBUTES              
         IC    RF,CUCTRY                                                        
         SLL   RF,3                                                             
         L     R1,=A(CTRYATB-L'CTRYATB)                                         
         A     R1,BCRELO                                                        
         AR    R1,RF                                                            
         MVC   BCCTRYAT,0(R1)      SET COUNTRY GLOBAL ATTRIBUTES                
         SR    RF,RF                                                            
         IC    RF,CULANG                                                        
         SLL   RF,3                                                             
         L     R1,=A(LANGATB-L'LANGATB)                                         
         A     R1,BCRELO                                                        
         AR    R1,RF                                                            
         OC    BCCTRYAT,0(R1)      MERGE IN LANGUAGE GLOBAL ATTRIBUTES          
*                                                                               
         MVC   BCSWSYSN,ASSYSO     SET SWITCHED SYSTEM INFO                     
         MVC   BCSWSYSC,ASSYSO                                                  
         MVC   BCSWSYSP,ASSYSO                                                  
*                                                                               
         GOTO1 VDATCON,BCPARM,(3,ASBDAT),(1,ASPDAT)                             
         GOTO1 (RF),(R1),(3,ASBDAT),(2,ASCDAT)                                  
*                                                                               
         L     R1,AFILTAB          FIND FILE TABLE ENTRY (FOR IOEX)             
         SR    RE,RE                                                            
INITGV10 CLI   0(R1),EOT           TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,R1),ASSYSO      MATCH ON OVSYS NUMBER                        
         BE    *+16                                                             
         ICM   RE,3,4(R1)                                                       
         LA    R1,5(RE,R1)                                                      
         B     INITGV10                                                         
         LA    R1,6(R1)                                                         
         ST    R1,AFILNTRY         SAVE A(FILE TABLE ENTRY)                     
*                                                                               
         TM    TWAINDS1,TWAIINIT   TEST VALUES SAVED LAST TIME                  
         BZ    INITGV12                                                         
         LA    R0,BCVALS           MOVE SAVE VALUES TO GLOBAL W/S               
         LA    RE,BSVALS                                                        
         LA    R1,BCVALSL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     INITGV50                                                         
*                                                                               
INITGV12 DS    0H                                                               
*NITGV12 GOTO1 VDMGR,BCPARM,=C'DTFADD',=C'ACCFIL'                               
*        L     R1,12(R1)                                                        
*        TM    ISFTYPE-ISDTF(R1),ISFTEMU                                        
*        BNZ   *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$FNCNA)                                           
*        B     SETMSG0                                                          
*                                                                               
*                                                                               
         GOTO1 VDICTAT,BCPARM,C'LU  ',ADICUPR,BSDICTU                           
         GOTO1 (RF),(R1),C'LU  ',ADICGEN,BC@YES                                 
         GOTO1 (RF),(R1),C'LL  ',ADICMIX,BSDICTL                                
*                                                                               
         GOTO1 VDATCON,(R1),(7,0),('FF',0)                                      
         SR    RE,RE                                                            
         ICM   RE,7,5(R1)                                                       
         MVC   BCMONTHS,0(RE)                                                   
*                                                                               
         LA    R2,IOKEY            READ COMPANY RECORD & EXTRACT VALUES         
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         GOTO1 AIO,IOREAD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ COMPANY RECORD                    
*                                                                               
         L     R2,AIO1             LOCATE COMPANY ELEMENT                       
         LA    R1,CPYRFST                                                       
         USING CPYEL,R1                                                         
         SR    R0,R0                                                            
INITGV14 CLI   CPYEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    R1,R0                                                            
         B     INITGV14                                                         
         SR    RE,RE               EXTRACT COMPANY ELEMENT                      
         ICM   RE,1,CPYLN                                                       
         LA    R0,L'BCCPYEL                                                     
         CR    RE,R0                                                            
         BNH   *+6                                                              
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BCCPYEL(0),CPYELD                                                
*                                                                               
         TM    CPYSTAT1,CPYSOROE   SET LENGTH OF OFFICE CODE                    
         BZ    *+8                                                              
         MVI   BCOFFLEN,1                                                       
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+8                                                              
         MVI   BCOFFLEN,2                                                       
*                                                                               
         MVI   BCDPTLEN,2          SET LENGTH OF DEPARTMENT CODE                
         CLI   CPYDEPTL,0                                                       
         BE    *+10                                                             
         MVC   BCDPTLEN,CPYDEPTL                                                
*                                                                               
         LA    R2,IOKEY            READ PRODUCTION LEDGER RECORD                
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         GOTO1 AGETLDG             GET PRODUCTION LEDGER TABLE ENTRY            
         BE    INITGV15                                                         
         MVC   FVMSGNO,=AL2(AE$MPLR)                                            
         B     SETMSG0             CAN'T FIND PRODUCTION LEDGER                 
*                                                                               
INITGV15 OI    TWAINDS1,TWAIINIT   INITIALIZE DICTIONARY                        
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           BUILD KEY OF ID RECORD                       
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         GOTO1 AIO,IORD+IOCTFILE+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R1,CTIDATA                                                       
         USING CTDSCD,R1           LOCATE DESCRIPTION (CODE) ELEMENT            
INITGV16 CLI   CTDSCEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         B     INITGV16                                                         
         MVC   BCUSERID,CTDSC                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING CTWREC,R2           BUILD KEY OF GROUP RECORD                    
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKAGY,CUAALF                                                   
         MVI   CTWKREC,CTWKRIDG                                                 
         MVC   CTWKID,BCUSERID     USE CODE FROM USER-ID RECORD                 
         GOTO1 AIO,IORD+IOCTFILE+IO1                                            
         BNE   *+8                                                              
         OI    TWAINDS1,TWAIUGID   SET CONNECTED USERID IS A GROUPID            
         CLC   CUUSER,BCCPYEL+(CPYUID-CPYELD)                                   
         BNE   *+8                                                              
         OI    TWAINDS1,TWAIUPID   SET CONNECTED USERID IS PRINCIPAL ID         
*                                                                               
         GOTO1 VDATCON,BCPARM,(5,0),(0,BCWORK)                                  
         MVC   BCTMON+0(1),BCWORK+1                                             
         MVC   BCTMON+1(1),BCWORK+3                                             
         CLI   BCWORK+2,C'1'                                                    
         BNE   INITGV20                                                         
         MVI   BCTMON+1,C'A'                                                    
         CLI   BCWORK+3,C'0'                                                    
         BE    INITGV20                                                         
         MVI   BCTMON+1,C'B'                                                    
         CLI   BCWORK+3,C'1'                                                    
         BE    INITGV20                                                         
         MVI   BCTMON+1,C'C'                                                    
*                                                                               
INITGV20 GOTO1 (RF),(R1),,(1,BCTODAYP)                                          
         MVC   BCTMONP,BCTODAYP                                                 
         GOTO1 (RF),(R1),,(2,BCTODAYC)                                          
         GOTO1 (RF),(R1),,(3,BCTODAYB)                                          
*&&UK*&& LA    R0,1                                                             
*&&US*&& LA    R0,2                                                             
         GOTO1 VADDAY,BCPARM,(C'Y',BCWORK),BCWORK+6,(R0)                        
         LNR   R0,R0                                                            
         GOTO1 (RF),(R1),,BCWORK+12,(R0)                                        
         GOTO1 VDATCON,BCPARM,(0,BCWORK+6),(1,BCTDATH)                          
         GOTO1 (RF),(R1),(0,BCWORK+12),(1,BCTDATL)                              
         TM    BCCPYST4,CPYSOV12                                                
         BZ    *+10                                                             
         XC    BCTDATL,BCTDATL     ANY DATE BACKWARDS ALLOWABLE                 
*                                                                               
         XC    BCWORK,BCWORK       SAVE AGENCY LEVEL PROGRAM PROFILE            
         MVI   BCWORK+00,C'A'-X'40'                                             
*&&US*&& MVC   BCWORK+01(3),=C'BAT'                                             
*&&UK*&& MVC   BCWORK+01(3),=C'PM1'                                             
         MVC   BCWORK+12(2),TWAAGY                                              
         GOTO1 VGETPROF,BCPARM,(X'C0',BCWORK),BCBPROF1,VDMGR                    
         MVI   BCWORK+03,C'2'      BA2                                          
         GOTO1 (RF),(R1),,BCBPROF2                                              
*                                                                               
INITGV50 L     R1,AOFFBLK                                                       
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
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BCOFFSAV,OFFASAV    SAVE OFFAL VALUES                            
         DROP  R1                                                               
*                                                                               
         L     R1,AINP             EXTRACT CURSOR POSITION                      
         MVC   CSCURDSP,TIOBCURS-TIOBD(R1)                                      
*                                                                               
         LA    RF,SECBLKL                                                       
         L     R2,ASECBLK                                                       
         USING SECD,R2                                                          
         GOTO1 VSECRET,BCPARM,('SECPINIT',SECD),(RF)                            
         ORG   *-2                                                              
         TM    SECINDS,SECIINIT    TEST SECRET BLOCK INITIALIZED                
         BNZ   *+12                                                             
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CSBPID,SECPID       EXTRACT USER PUBLIC-ID                       
         DROP  R2                                                               
*                                                                               
         MVC   AREP,AWORKX         SET A(REPORT BLOCK)                          
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         LA    RF,WORKD                                                         
         A     RF,=A(PQBUFF-WORKD)                                              
         ST    RF,REPAPQB                                                       
         MVC   REPACOM,ACOM        INITIALIZE REPBLK VALUES                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         OI    REPIND2,REPILOW                                                  
         OI    REPHEADI,REPHSPAC+REPHCLRA                                       
         OI    REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVC   REPDATE,ASBDAT                                                   
         MVC   REPSYSID,=C'AC'                                                  
         MVC   REPPRGID,=C'IN'                                                  
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
*                                                                               
         CLI   BASRECH+(FVILEN-FVIHDR),0                                        
         BNE   INITGV60                                                         
         CLI   BASACTH+(FVILEN-FVIHDR),0                                        
         BNE   INITGV60                                                         
         TM    BCINDS1,BCIANYPF                                                 
         BNZ   INITGV60                                                         
         LA    R0,BASRECH          NO RECORD/ACTION OR PFKEY THIS TIME          
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AS$ERF)                                             
         MVI   FVOMTYP,GTMSCR                                                   
         B     SETMSG                                                           
*                                                                               
INITGV60 LA    R3,BCWORK                                                        
         USING GLVXCTLD,R3                                                      
         XC    GLVXCODE(24),GLVXCODE  TEST GLOBBER CALL                         
         L     R4,ACOM                                                          
         USING COMFACSD,R4                                                      
         GOTO1 CGLOBBER,BCPARM,=C'GETD',GLVXFRSY,22,GLVXCTL                     
         TM    8(R1),X'10'                                                      
         BO    INITGVX                 NOT A GLOBBER CALL                       
         GOTO1 CGLOBBER,BCPARM,=C'DELE'  DELETE XCTL ELEMENT                    
         DROP  R3,R4                                                            
*                                                                               
         LA    RF,BASMSGH          RE-TRANSMIT SCREEN                           
         SR    R1,R1                                                            
INITGV65 CLI   0(RF),0                                                          
         BE    INITGV69                                                         
         OI    6(RF),X'80'                                                      
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         B     INITGV65                                                         
INITGV69 MVC   0(3,RF),=X'000100'  ERASE BEFORE                                 
         LA    R0,BASRECH          SEND 'SESSION RESTORED'                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$PSRES)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     SETMSG                                                           
*                                                                               
INITGVX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* RESTORE SESSION AT END OR USER TERMINATION OF OPTION HELP           *         
***********************************************************************         
         SPACE 1                                                                
HELP     XC    BOWORK1,BOWORK1     CLEAR WORK AREA                              
         CLC   BCQUEST,BASOPT      TEST FOR ? IN OPTIONS FIELD                  
         BNE   HELP02                                                           
         CLI   BASOPT+L'BCQUEST,C' '                                            
         BH    HELP04                                                           
         XC    TWAOSAVE,TWAOSAVE                                                
         B     HELP04                                                           
HELP02   CLI   BASOPT,C' '                                                      
         BNH   HELP04                                                           
         MVC   TWAOSAVE,BASOPT     NO - SAVE FIRST BYTE OF OPTIONS              
*                                                                               
HELP04   TM    TWAINDS1,TWAIOHIP   TEST OPTION HELP IN PROGRESS                 
         BZ    HELP06              NO - VALIDATE PFKEYS OR AUTO-RECALL          
         TM    BASRECH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   HELP08                                                           
         TM    BASACTH+(FVIIND-FVIHDR),FVITHIS                                  
         BNZ   HELP08                                                           
         TM    BCINDS1,BCIANYPF                                                 
         BNZ   HELP08                                                           
         CLC   BCQUEST,BASOPT      TEST QUESTION MARK REMOVED                   
         BE    HELP06              NO - VALIDATE PFKEYS OR AUTO RECALL          
         CLC   BASOPT,BCSPACES     TEST OPTIONS INPUT DURING HELP               
         BNH   HELP08                                                           
         MVC   BOWORK1(L'BASOPT),BASOPT                                         
         B     HELP08                                                           
*                                                                               
HELP06   TM    TWAINDS1,TWAIOHRE   TEST OPTION AUTO RECALL REQUIRED             
         BZ    VALPFK                                                           
*                                                                               
HELP08   NI    TWAINDS1,FF-(TWAIOHRE+TWAIOHIP)                                  
         GOTO1 AXITSES                                                          
         MVC   BASOPT(L'TWAOSAVE),TWAOSAVE                                      
         XC    TWAOSAVE,TWAOSAVE                                                
         OC    BOWORK1,BOWORK1                                                  
         BZ    *+10                                                             
         MVC   BASOPT,BOWORK1                                                   
         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         B     VALPFK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PFKEY                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALPFK   TM    TWAINDS2,TWAIPFKD+TWAIDPFK  RECORD DISPLAYED FOR RENAME          
         BNZ   VALPFKX                     OR DISABLED?                         
         MVC   BCWORK(L'CSRECACT),CSRECACT                                      
         TM    BCINDS1,BCIANYPF    TEST USER ENTERED PFKEY THIS TIME            
         BNZ   VALPFK04                                                         
         TM    TWAINDS2,TWAIAPPF   ONLY APPLICATION KEYS ACTIVE?                
         BO    VALPFKX             YES                                          
         CLI   CSNEXTPF,0          TEST NEXT TIME PFKEY SET                     
         BE    VALPFKX                                                          
         MVC   BCPFKEY,CSNEXTPF    SET NEXT TIME PFKEY & PROCESS                
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
         B     VALPFK06                                                         
*                                                                               
VALPFK04 CLI   BCWORK+1,0                                                       
         BNE   *+16                                                             
         MVI   BCWORK+0,FF                                                      
         MVI   BCWORK+1,FF                                                      
         B     VALPFK06                                                         
         TM    BASRECH+(FVIIND-FVIHDR),FVIVAL                                   
         BZ    VALPFKX                                                          
         TM    BASACTH+(FVIIND-FVIHDR),FVIVAL                                   
         BZ    VALPFKX                                                          
*                                                                               
VALPFK06 TM    CSINDSL2,CSIPFAPP   TEST APPLICATION DOES PFKEYS                 
         BNZ   VALPFKX                                                          
         L     R2,APFKTAB                                                       
         USING PFKTABD,R2          R2=A(PFKEY TABLE)                            
         SR    R0,R0                                                            
*                                                                               
VALPFK07 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    VALPFKX                                                          
         CLC   PFKKEY,BCWORK       MATCH ON HEADER RECORD/ACTION                
         BE    *+14                                                             
         ICM   R0,3,PFKLEN                                                      
         AR    R2,R0               POINT TO NEXT SUB-TABLE HEADER               
         B     VALPFK07                                                         
*                                                                               
         LA    R2,PFKHEADL(R2)     BUMP TO FIRST DATA ENTRY                     
VALPFK08 CLI   PFKTABD,EOT         TEST END OF TABLE                            
         BE    VALPFK34                                                         
         CLC   PFKNUMB,BCPFKEY     MATCH ON PFKEY NUMBER                        
         BE    *+12                                                             
*                                                                               
VALPFK09 LA    R2,PFKDATAL(R2)     POINT TO NEXT DATA ENTRY                     
         B     VALPFK08                                                         
*                                                                               
         TM    PFKINDS1,PFKIAPPL   TEST FOR APPLICATION USE                     
         BZ    VALPFK12                                                         
*                                                                               
*&&US                                                                           
         CLC   CSBTYP,PFKBTYP      BATCH TYPES MATCH?                           
         BNE   VALPFK09            NO                                           
*&&                                                                             
         MVC   BCHALF,CSAPFMSK     TEST APPLICATION PFKEY MASK                  
         NC    BCHALF,PFKOMASK                                                  
         CLC   BCHALF,PFKOMASK                                                  
         BNE   VALPFK09                                                         
         CLI   PFKCTRY,0           'ALL' COUNTRIES VALID?                       
         BE    VALPFKX             YES                                          
         TM    PFKCTRY,CTRYNOT     NO, 'NOT' COUNTRIES?                         
         BNZ   VALPFK10            YES                                          
         CLC   PFKCTRY,CUCTRY      NO, DO COUNTRY CODES MATCH?                  
         BE    VALPFKX             YES, OK TO LEAVE                             
         B     VALPFK09                                                         
*                                                                               
VALPFK10 MVC   BCWORK(L'PFKCTRY),PFKCTRY                                        
         NI    BCWORK,FF-CTRYNOT                                                
         CLC   CUCTRY,BCWORK                                                    
         BNE   VALPFKX                                                          
         B     VALPFK09                                                         
*                                                                               
VALPFK12 TM    TWAINDS2,TWAIAPPF   APPLICATION PFKEYS ACTIVE?                   
         BO    VALPFKX             NO                                           
*                                                                               
         TM    PFKINDS1,PFKIKAPA   TEST FOR KNOWN APPLICATION ACTION            
         BNZ   VALPFKX                                                          
*                                                                               
         TM    PFKINDS1,PFKIACTN   TEST ACTION PFKEY                            
         BZ    VALPFK26                                                         
*                                                                               
         LA    RE,TWASESRA         RE=A(SESSION RECORD/ACTION TABLE)            
         SR    RF,RF                                                            
         ICM   RF,1,TWASESNL       RF=NUMBER OF ENTERED SESSIONS                
         BZ    VALPFK13                                                         
         CLC   PFKRECA,0(RE)       TEST RECORD/ACTION ALREADY USED              
         BE    VALPFKX                                                          
         LA    RE,L'TWASESRA(RE)   BUMP TO NEXT TABLE ENTRY                     
         BCT   RF,*-14                                                          
*                                                                               
VALPFK13 TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BNZ   *+12                                                             
         OI    BCINDS1,BCINREC+BCINACT                                          
         B     VALPFK24                                                         
         TM    PFKCSIL1,CSIUSELC   TEST PASSING LIST KEY                        
         BZ    VALPFK18                                                         
         GOTO1 ATSTMIX,PFKRECN     TEST VALID RECORD/ACTION COMBO               
         BNE   VALPFKX                                                          
         XC    CSLSTCUR(L'BCLSTCUR),CSLSTCUR                                    
         L     RF,AMIXNTRY                                                      
         SR    RE,RE                                                            
         ICM   RE,1,MIXNTREC-MIXTABD(RF)                                        
         BZ    VALPFK14                                                         
         MH    RE,=Y(L'BCLSTCUR)                                                
         LA    RE,BCLSTCUR-L'BCLSTCUR(RE)                                       
         OC    CSLSTCUR(L'BCLSTCUR),0(RE)                                       
         BNZ   VALPFK14                                                         
         OI    BCINDS1,BCINREC+BCINACT                                          
         GOTO1 ARECACT,PFKRECN                                                  
         B     VALPFKX                                                          
*                                                                               
VALPFK14 TM    PFKINDS2,PFKISAVS   TEST ENTER NEW SESSION                       
         BZ    VALPFK18                                                         
         LA    RF,CSLSTCUR                                                      
         CLC   PFKRTYPE,LSTTRTYP-LSTTABD(RF)                                    
         BE    VALPFK16                                                         
         SR    RE,RE                                                            
         IC    RE,PFKRTYPE                                                      
         MH    RE,=Y(LSTTABL)                                                   
         LA    RF,BCBATCUR-LSTTABL(RE)                                          
         CLC   PFKRTYPE,LSTTRTYP-LSTTABD(RF)                                    
         BNE   VALPFKX                                                          
VALPFK16 MVC   BCHALF,LSTTMASK-LSTTABD(RF)                                      
         NC    BCHALF,PFKRMASK                                                  
         CLC   BCHALF,PFKRMASK                                                  
         BNE   VALPFKX                                                          
*                                                                               
VALPFK18 TM    TWAINDS1,TWAIOHIP   TEST OPTION HELP IN PROGRESS                 
         BZ    VALPFK20                                                         
         NI    TWAINDS1,FF-(TWAIOHIP+TWAIOHRE)                                  
         GOTO1 AXITSES             EXIT FROM HELP SESSION                       
         CLC   BCQUEST,BASOPT      REMOVE THE ? IF NECESSARY                    
         BNE   *+14                                                             
         MVC   BASOPT(L'TWAOSAVE),TWAOSAVE                                      
         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    TWAOSAVE,TWAOSAVE                                                
*                                                                               
VALPFK20 SR    R1,R1                                                            
         CLI   CSQRTN,0            TEST RETURN ROUTINE FROM QUIT                
         BE    VALPFK22                                                         
         LA    R1,BCWORK                                                        
         USING SELTPARM,R1                                                      
         XC    SELTPARM,SELTPARM                                                
         MVC   SELTRTN,CSQRTN      SET RETURN ROUTINE FROM QUIT                 
         DROP  R1                                                               
*                                                                               
VALPFK22 GOTO1 ANTRSES,(R1)        ENTER NEW RECORD/ACTION SESSION              
*                                                                               
VALPFK24 MVC   CSINDSL,PFKCSIL     SET NEXT SESSION INDICATORS                  
         MVC   CSNEXTPF,PFKNEXT    SET NEXT TIME AUTO PFKEY                     
         GOTO1 ARECACT,PFKRECN                                                  
         B     VALPFKX                                                          
*                                                                               
VALPFK26 TM    PFKINDS1,PFKIALTP   TEST ALTERNATE PFKEY DISPLAY                 
         BZ    VALPFK28                                                         
         XI    TWAINDS3,TWAIPFKD   SWAP INDICATOR BIT                           
         L     R1,AINP             SET CURSOR FROM WHENCE IT CAME               
         MVC   TIOBCURS-TIOBD(,R1),CSCURDSP                                     
         XC    TIOBCURD-TIOBD(,R1),TIOBCURD-TIOBD(R1)                           
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         SR    R2,R2                                                            
         B     BATX04                                                           
*                                                                               
VALPFK28 TM    PFKINDS1,PFKIQUIT+PFKINEXT                                       
         BZ    VALPFK32                                                         
         CLI   TWASESNL,0          TEST NESTED                                  
         BE    VALPFKX                                                          
         TM    PFKINDS1,PFKITSEC   TEST BATCH TYPE SECURITY                     
         BZ    VALPFK29                                                         
         CLI   CSBTYP,0            TEST BATCH TYPE RESOLVED                     
         BE    VALPFKX                                                          
         GOTO1 ATSTBTY,PFKACTN     TEST ACTION VALID                            
         BNE   VALPFKX                                                          
*                                                                               
VALPFK29 TM    CSINDSL2,CSIDPFQN   TEST QUIT/NEXT DISABLED                      
         BNZ   VALPFKX                                                          
         GOTO1 AXITSES             RESTORE PREVIOUS SESSION                     
         TM    TWAINDS1,TWAIXITS   TEST XITSES ISSUED                           
         BZ    VALPFK30                                                         
         NI    TWAINDS1,FF-TWAIXITS                                             
         B     GO10                CALL SUB-CONTROLLER                          
*                                                                               
VALPFK30 GOTO1 ARECACT,CSREC       RESTORE RECORD/ACTION                        
         B     SETMSG              EXIT TO USER                                 
*                                                                               
VALPFK32 TM    PFKINDS1,PFKISCRL   TEST SCROLL PFKEY                            
         BZ    VALPFKX                                                          
         MVC   BCSCROLL,PFKINDS2   SAVE SCROLL INDICATORS                       
         B     VALPFKX                                                          
*                                                                               
VALPFK34 CLI   CSNEXTPF,0          TEST NEXT TIME PFKEY SET                     
         BE    VALPFKX                                                          
         CLC   BCPFKEY,CSNEXTPF    TEST THIS IS THE ONE JUST USED               
         BE    VALPFKX                                                          
         MVC   BCPFKEY,CSNEXTPF                                                 
         MVI   CSNEXTPF,0          RESET NEXT TIME PFKEY                        
         B     VALPFK06                                                         
*                                                                               
VALPFKX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALREC   TM    BASRECH+(FVIIND-FVIHDR),FVIVAL                                   
         BZ    *+12                                                             
         TM    BASACTH+(FVIIND-FVIHDR),FVIVAL                                   
         BNZ   VALREC02                                                         
         LA    R0,CSVALS                                                        
         LHI   R1,L'CSVALS                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                  PRESERVE THESE BITS                          
         MVC   BCBYTE1,TWAINDS1                                                 
         NI    BCBYTE1,TWAIINIT+TWAIUPID+TWAIUGID                               
         XC    TWAVALS,TWAVALS                                                  
         MVC   TWAINDS1,BCBYTE1                                                 
                                                                                
VALREC02 MVI   FVMINL,1            SET FIELD IS REQUIRED                        
         MVI   FVFIELD,FVFREC                                                   
         LA    R0,CSRECNAM-WORKD                                                
         STCM  R0,3,FVSAVE         SET DISPLACEMENT TO LAST RECORD              
         GOTO1 AFVAL,BASRECH                                                    
         BNE   SETMSG                                                           
         MVC   CSRECNAM,FVIFLD                                                  
         L     R2,ARECTAB                                                       
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
                                                                                
VALREC04 CLI   RECTABD,EOT         TEST END OF TABLE                            
         BNE   VALREC06                                                         
         MVC   FVMSGNO,=AL2(AE$INREC)                                           
         LA    R0,RECNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(RECNAMLQ),FVIFLD                                       
         B     SETMSG                                                           
                                                                                
VALREC06 SR    R1,R1                                                            
         ICM   R1,3,RECNAMEU                                                    
         LA    R1,TWAD(R1)                                                      
         MVC   BCWORK(RECNAMLQ),0(R1)                                           
         ICM   R1,1,FVXLEN                                                      
         EX    R1,*+8                                                           
         BNE   VALREC08                                                         
         CLC   BCWORK(0),FVIFLD    MATCH INPUT AGAINST FULL NAME                
         TM    RECINDS1,RECIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALREC08                                                         
         GOTO1 ATSTREC,RECNUMB     TEST RECORD VALID                            
         BE    VALREC10                                                         
                                                                                
VALREC08 LA    R2,RECTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALREC04                                                         
                                                                                
VALREC10 ST    R2,ARECNTRY         SET A(RECORD TYPE TABLE ENTRY)               
         MVC   BCHALF+0(1),RECNUMB                                              
         CLC   BASREC,BCWORK+RECNAMLQ                                           
         BE    *+14                                                             
         MVC   BASREC,BCWORK+RECNAMLQ                                           
         OI    BASRECH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    BASRECH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   CSRECNAM,BCWORK+RECNAMLQ                                         
*                                                                               
VALRECX  DS    0H                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION FIELD & RECORD/ACTION COMBINATION                   *         
***********************************************************************         
         SPACE 1                                                                
VALACT   MVI   FVMINL,1            SET FIELD IS REQUIRED                        
         MVI   FVFIELD,FVFACT                                                   
         MVI   FVMAXL,ACTNAMLQ                                                  
         LA    R0,CSACTNAM-WORKD                                                
         STCM  R0,3,FVSAVE         SET DISPLACEMENT TO LAST RECORD              
         GOTO1 AFVAL,BASACTH                                                    
         BNE   SETMSG                                                           
         MVC   CSACTNAM,FVIFLD                                                  
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2          R2=A(ACTION TABLE)                           
*                                                                               
VALACT02 CLI   ACTTABD,EOT         TEST EOT                                     
         BNE   VALACT04                                                         
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         LA    R0,ACTNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(ACTNAMLQ),FVIFLD                                       
         B     SETMSG                                                           
*                                                                               
VALACT04 SR    R1,R1                                                            
         ICM   R1,3,ACTNAMEU                                                    
         LA    R1,TWAD(R1)                                                      
         MVC   BCWORK(ACTNAMLQ),0(R1)                                           
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         BNE   VALACT10                                                         
         CLC   BCWORK(0),FVIFLD    MATCH ON INPUT NAME                          
*                                  TEST IF THIS ENTRY IS VALID                  
         TM    ACTINDS1,ACTIDDS    TEST DDS ONLY RECORD TYPE                    
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       YES - TEST DDS TERMINAL                      
         BZ    VALACT10                                                         
         TM    ACTINDS1,ACTIUPD    IS THIS AN UPDATIVE ACTION?                  
         BZ    VALACT08            NO, CONTINUE                                 
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    VALACT08                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(360)                                                
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    VALACT06            YES                                          
*                                                                               
         MVC   FVMSGNO,=AL2(357)                                                
         MVC   FVXTRA(L'FACUPD),FACUPD                                          
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    VALACT06            YES                                          
*                                                                               
         MVC   FVMSGNO,=AL2(358)   CONNECTED TO READ ONLY SYSTEM                
         MVC   FVXTRA,BCSPACES                                                  
*                                                                               
VALACT06 LA    R0,ACTNAMLQ+1                                                    
         STC   R0,FVPARMS                                                       
         MVC   FVPARMS+1(ACTNAMLQ),FVIFLD                                       
         B     SETMSG                                                           
*                                                                               
VALACT08 MVC   BCHALF+1(1),ACTNUMB                                              
         GOTO1 ATSTMIX,BCHALF      TEST AUTHORISED FOR RECORD/ACTION            
         BNE   VALACT10                                                         
         L     R1,AMIXNTRY                                                      
         USING MIXTABD,R1          R1=A(MIX TABLE ENTRY)                        
         TM    MIXINDS1,MIXISEL    TEST SELECT ACTION ONLY                      
         BZ    VALACT12                                                         
         TM    CSINDSL1,CSIUSELC   YES - TEST NESTED CALL                       
         BZ    VALACT10                                                         
         B     VALACT12                                                         
*                                                                               
VALACT10 LA    R2,ACTTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     VALACT02                                                         
         DROP  R1                                                               
*                                  EXTRACT ACTION VALUES INTO W/S               
VALACT12 ST    R2,AACTNTRY         SET A(ACTION TABLE ENTRY)                    
         GOTO1 ASETSEL             SET A(SELECT TABLE)                          
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3          R3=A(MIX TABLE ENTRY)                        
         MVC   CSSCRN,MIXSCRN      EXTRACT MIXTAB VALUES                        
         MVC   CSOVER,MIXOVER                                                   
         MVC   CSMIX1,MIXINDS1                                                  
         MVC   CSMIX2,MIXINDS2                                                  
         CLC   CSREC,MIXRECB       TEST CHANGE OF RECORD TYPE                   
         MVC   CSREC,MIXRECB       SET RECORD NUMBER                            
         BE    *+8                                                              
         OI    BCINDS1,BCINREC+BCINACT                                          
         CLC   CSACT,MIXACTB       TEST CHANGE OF ACTION                        
         MVC   CSACT,MIXACTB       SET ACTION NUMBER                            
         BE    *+8                                                              
         OI    BCINDS1,BCINACT                                                  
         CLC   BASACT,BCWORK                                                    
         BE    *+14                                                             
         MVC   BASACT,BCWORK                                                    
         OI    BASACTH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    BASACTH+(FVIIND-FVIHDR),FVIVAL                                   
         MVC   CSACTNAM,BCWORK     SAVE FULL ACTION NAME                        
         MVC   CSQRTN,MIXQRTN      SAVE RETURN ROUTINE FROM QUIT                
*                                                                               
VALACTX  DS    0H                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SCROLL FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSCR   CLI   BASSCRH+(FVILEN-FVIHDR),0                                        
         BNE   VALSCR02                                                         
         LH    RE,=Y(UC@PAGE-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         MVC   BASSCR(L'UC@PAGE),0(RE)                                          
*                                                                               
VALSCR02 GOTO1 AFVAL,BASSCRH                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    FVIIND,FVINUM                                                    
         BNZ   VALSCR06                                                         
*                                                                               
         LH    R1,=Y(UC@MAX-TWAD)  VALIDATE SCROLL KEYWORDS                     
         LA    R1,TWAD(R1)                                                      
         LH    RE,=Y(UC@PAGE-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         LA    RF,PFKIMAXN                                                      
         CLC   FVIFLD(1),0(R1)     M(AXIMUM)                                    
         BE    VALSCR04                                                         
         LA    RF,PFKIPAGE                                                      
         CLC   FVIFLD(1),0(RE)     P(AGE)                                       
         BE    VALSCR04                                                         
         LH    RE,=Y(UC@HALF-TWAD)                                              
         LA    RE,TWAD(RE)                                                      
         LA    RF,PFKIHALF                                                      
         CLC   FVIFLD(1),0(RE)     H(ALF)                                       
         BNE   VALSCRE1                                                         
*                                                                               
VALSCR04 XC    BASSCR,BASSCR                                                    
         MVC   BASSCR(L'UC@MAX),0(RE)                                           
         OI    BASSCRH+(FVOIND-FVIHDR),FVOXMT                                   
         STC   RF,BCSCRNUM                                                      
         B     VALSCRX                                                          
*                                                                               
VALSCR06 OC    BCFULL,BCFULL       VALIDATE SCROLL AMOUNT                       
         BZ    VALSCRE1                                                         
         OC    BCFULL(3),BCFULL                                                 
         BNZ   VALSCRE1                                                         
         CLI   BCFULL+3,15                                                      
         BH    VALSCRE1                                                         
         MVC   BCSCRNUM,BCFULL+3                                                
         B     VALSCRX                                                          
*                                                                               
VALSCRE1 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     SETMSG0                                                          
*                                                                               
VALSCRX  TM    BCSCRNUM,PFKIMAXN                                                
         BNZ   *+8                                                              
         OI    BASSCRH+(FVIIND-FVIHDR),FVIVAL                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT FIELD AND SET VALUE IN CSREPID/REPSUBID             *         
***********************************************************************         
         SPACE 1                                                                
VALREP   L     R2,AREP                                                          
         USING REPD,R2                                                          
         XC    CSREPID,CSREPID     CLEAR CURRENT REPORT ID                      
         CLI   BASREPH+FHILD,0                                                  
         BE    VALREPX                                                          
         MVC   CSREPID,BASREP                                                   
         TM    BASREPH+FHIID,FHIIVA                                             
         BO    VALREPX                                                          
         OI    BASREPH+FHIID,FHIIVA                                             
         OI    BASREPH+FHOID,FHOITR                                             
VALREPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* GO TO SUB-CONTROLLER                                                *         
***********************************************************************         
         SPACE 1                                                                
GO       MVI   BCBYTE1,0                                                        
         TM    BCINDS1,BCINREC+BCINACT                                          
         BZ    GO04                                                             
         TM    TWAINDS1,TWAIOHIP                                                
         BZ    GO02                                                             
         GOTO1 AXITSES                                                          
         NI    TWAINDS1,FF-(TWAIOHIP+TWAIOHRE)                                  
         CLC   BCQUEST,BASOPT                                                   
         BNE   *+14                                                             
         MVC   BASOPT(L'TWAOSAVE),TWAOSAVE                                      
         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    TWAOSAVE,TWAOSAVE                                                
*                                                                               
GO02     XC    CSHIRECN,CSHIRECN                                                
         XC    CSPSRECN,CSPSRECN                                                
         XC    CSINDSL,CSINDSL                                                  
         XC    CSINDSG,CSINDSG                                                  
         MVI   TWASESNL,0                                                       
         XC    CSINITRA,CSINITRA   CLEAR INITIAL RECORD/ACTION                  
         LA    R0,TWAD                                                          
         AH    R0,=Y(OSVALS-TWAD)                                               
         LA    R1,OSVALSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,TWAD                                                          
         AH    R0,=Y(OSVALS2-TWAD)                                              
         LA    R1,OSVALS2L                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
GO04     TM    TWAINDS1,TWAIOHIP   TEST HELP IN PROGRESS                        
         BNZ   GO06                                                             
         CLC   BCQUEST,BASOPT      TEST HELP FOR OPTIONS REQUESTED              
         BNE   GO10                                                             
         L     RF,AMIXNTRY                                                      
         CLI   MIXOHELP-MIXTABD(RF),0                                           
         BE    GO10                                                             
         GOTO1 ANTRSES,0           ENTER HELP SESSION                           
*                                                                               
GO06     LA    R1,BCPARM                                                        
         USING GETHELPD,R1         R1=A(GETHELP CONTROL BLOCK)                  
         XC    GETHELPD(24),GETHELPD                                            
         LA    RF,BCWORK                                                        
         USING HELPKEYD,RF                                                      
         XC    HELPKEYD(10),HELPKEYD                                            
         MVI   GTHSCRN,X'FF'                                                    
         L     RE,AMIXNTRY                                                      
         MVC   GTHFLDN,MIXOHELP-MIXTABD(RE)                                     
         MVI   GTHFLAGS,GHMKEYQ                                                 
         TM    TWAINDS1,TWAIOHIP                                                
         BZ    *+8                                                              
         OI    GTHFLAGS,GHSAVEQ                                                 
         OI    TWAINDS1,TWAIOHIP   SET HELP IN PROGRESS                         
         LA    R0,HELPKEYD                                                      
         STCM  R0,7,GTHAKEY                                                     
         LA    R0,BASOPTH                                                       
         STCM  R0,7,GTHQHDR                                                     
         LA    R0,OSVALS                                                        
         STCM  R0,7,GTHASVE                                                     
         GOTO1 VGETHELP                                                         
         TM    GTHRET,GHCTRLQ+GHNONEQ                                           
         BZ    GO08                                                             
         GOTO1 AXITSES                                                          
         NI    TWAINDS1,FF-(TWAIOHIP+TWAIOHRE)                                  
         MVC   BASOPT(L'TWAOSAVE),TWAOSAVE                                      
         OI    BASOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    TWAOSAVE,TWAOSAVE                                                
         B     GO10                                                             
*                                                                               
GO08     LA    R0,BASOPTH                                                       
         ST    R0,FVADDR                                                        
         MVI   FVOMTYP,GTMSCR                                                   
         MVC   FVMSGNO,=AL2(AS$HLPD2)                                           
         TM    GTHRET,GHPAGEQ      TEST MORE PAGES TO COME                      
         BNZ   *+14                                                             
         OI    TWAINDS1,TWAIOHRE   SET AUTO RECALL NEXT TIME                    
         MVC   FVMSGNO,=AL2(AS$HLPD1)                                           
         B     SETMSG                                                           
*                                                                               
GO10     GOTO1 AOVRPHS,CSOVER      LOAD SUB-CONTROLLER                          
         BNE   SETMSG                                                           
         MVC   BCNTRYA(1),BCBYTE1                                               
         GOTO1 BCNTRYA                                                          
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    *+12                                                             
         NI    TWAINDS1,FF-(TWAINTRS+TWAIXITS)                                  
         B     SETMSG                                                           
         TM    TWAINDS1,TWAINTRS   TEST NTRSES ISSUED                           
         BZ    *+12                                                             
         NI    TWAINDS1,FF-TWAINTRS                                             
         B     VALREC                                                           
*                                                                               
         TM    TWAINDS1,TWAIXITS   TEST XITSES ISSUED                           
         BZ    SETMSG                                                           
         NI    TWAINDS1,FF-TWAIXITS                                             
         B     GO10                                                             
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
* NTR AT SETMSG0 TO SET MULTIPLE FIELD INDEX VALUES TO ZERO           *         
*        SETMSG FOR REGULAR MESSAGE BUILDING                          *         
*        SETMSG10 ONLY TO SET CURSOR TO FIELD ADDRESSED BY FVADDR     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SETMSG0  MVI   FVINDX,0            ENTRY POINT FOR NO INDEX INFO                
         MVI   FVSUBX,0                                                         
SETMSG   SR    R2,R2                                                            
         TM    CSINDSG1,CSINDUNW   TEST UNWIND VIA $ABEND                       
         BZ    *+12                                                             
         NI    CSINDSG1,FF-(CSINDUNW)                                           
         LA    R2,1                                                             
         CLC   FVMSGNO,=AL2(FVFSET) TEST USER HAS SUPPLIED MESSAGE              
         BE    SETMSG10                                                         
         MVC   CSMSGNUM,FVMSGNO                                                 
         MVC   CSMSGTYP,FVOMTYP                                                 
         LA    R1,BCPARM           DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R1                                                       
         CLC   FVMSGNO,=AL2(FVFGTSET)   TEST APPL SET GETTXT BLOCK              
         BNE   *+12                                                             
         LA    R1,BOPARM           APPLICATION HAS DEFINED BLOCK                
         B     SETMSG8                                                          
*                                                                               
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FVINDX                                                    
         MVC   GTSUBX,FVSUBX                                                    
         MVC   GTMSGNO,FVMSGNO                                                  
         MVC   GTMSYS,FVOSYS       OVERRIDE SYSTEM (IF SET)                     
         CLI   GTMSYS,0            TEST OVERRIDE SYSTEM SET                     
         BNE   *+10                                                             
         MVC   GTMSYS,ASSYSO       NO - SET NATIVE SYSTEM                       
         MVC   GTMTYP,FVOMTYP      OVERRIDE MESSAGE TYPE (IF SET)               
         CLI   GTMSGNO,FF          STD CONTROLLER MSG                           
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF           GENERAL SYSTEM MESSAGE                       
         OC    GTMSGNO,GTMSGNO     MESSAGE 0 = DATAMGR ERR                      
         BNZ   SETMSG2                                                          
         LA    R0,BOPARM                                                        
         STCM  R0,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
*                                                                               
SETMSG2  LA    RF,FVXTRA+L'FVXTRA-1                                             
         LA    R0,L'FVXTRA                                                      
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     SETMSG6                                                          
         LA    RF,FVXTRA                                                        
         STCM  RF,7,GTATXT         SET LENGTH & ADDRESS OF EXTRA TEXT           
         STCM  R0,1,GTLTXT                                                      
*                                                                               
SETMSG6  LA    R1,BCPARM           BCPARM DEFINED INTERNALLY                    
         CLI   GTMSGNO,FF          CHECK FOR GENERAL MESSAGES                   
         BNE   SETMSG8                                                          
         MVI   GTMSYS,FF           FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
*                                                                               
SETMSG8  LA    R0,FVPARMS          SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         DROP  R1                                                               
*                                                                               
SETMSG10 OI    BASMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         L     R1,AINP             TEST IF OVERLAY SET CURSOR                   
         TM    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         BNZ   BATX                                                             
         ICM   R1,15,BOCURSOR      TEST CURSOR ADDRESS SET                      
         BNZ   *+12                                                             
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    BATX                                                             
*                                                                               
         CLI   FVERRNDX,0          TEST FIELD INDEX VALUE SET                   
         BE    SETMSG12                                                         
         L     RE,AINP                                                          
         USING TIOBD,RE                                                         
         LA    R0,TWAD                                                          
         LR    RF,R1                                                            
         SR    RF,R0               RF=DISPLACEMENT TO FIELD                     
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FVERRNDX                                                
         OI    TIOBINDS,TIOBSETC                                                
         DROP  RE                                                               
*                                                                               
SETMSG12 OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         AR    R1,RE               POINT TO NEXT FIELD HEADER                   
         LA    RF,OSVALS-1         RF=END OF LARGEST SCREEN                     
         TM    CSBIND8,TYPIXOVL    TEST EXTRA AREA FOR SCREEN                   
         BNO   *+8                                                              
         LA    RF,OSSAVE-1                                                      
         ICM   RE,1,FVTLEN-FVIHDR(R1) TURN OFF CURSORS TO BOTTOM OF TWA         
         BZ    BATX                                                             
         NI    FVOIND-FVIHDR(R1),FF-FVOCUR                                      
         BXLE  R1,RE,*-12                                                       
*                                                                               
BATX     GOTO1 ATSARIO,TSASAV      SAVE TSAR BUFFER IF NECESSARY                
*                                                                               
         SR    RE,RE               SAVE CURRENT RECORD ENTRY                    
         ICM   RE,1,CSLSTCUR+(LSTTRTYP-LSTTABD)                                 
         BZ    BATX02                                                           
         MH    RE,=Y(L'BCLSTCUR)                                                
         LA    RE,BCLSTCUR-L'BCLSTCUR(RE)                                       
         MVC   0(L'BCLSTCUR,RE),CSLSTCUR                                        
*                                                                               
BATX02   LA    R0,BSVALS           SAVE GLOBAL W/S VALUES IN TWA0               
         LA    RE,BCVALS                                                        
         LA    R1,BCVALSL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
BATX04   TM    TWAINDS1,TWAIINIT   TEST INITIALIZED                             
         BZ    BATXX                                                            
         GOTO1 ABLDPFK             BUILD PFKEY DISPLAY LINE                     
BATXX    LTR   R2,R2               TEST UNWIND VIA $ABEND                       
         BZ    BATXXX                                                           
         DC    H'0',C'$ABEND'      UNWIND TRANSACTION                           
BATXXX   XIT1  ,                                                                
         EJECT                                                                  
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
ANAWS    DS    0XL4                ** A(NON-ADDRESSABLE W/S AREAS) **           
         DC    AL2(IOAREA1+L'IODA+L'IOWORK-WORKD,AIO1-WORKD)                    
         DC    AL2(IOAREA2+L'IODA+L'IOWORK-WORKD,AIO2-WORKD)                    
         DC    AL2(IOAREA3+L'IODA+L'IOWORK-WORKD,AIO3-WORKD)                    
         DC    AL2(IOAREA4+L'IODA+L'IOWORK-WORKD,AIO4-WORKD)                    
         DC    AL2(IOAREA5+L'IODA+L'IOWORK-WORKD,AIO5-WORKD)                    
         DC    AL2(IOAREA6+L'IODA+L'IOWORK-WORKD,AIO6-WORKD)                    
         DC    AL2(IOAREA7+L'IODA+L'IOWORK-WORKD,AIO7-WORKD)                    
         DC    AL2(IOAREA8+L'IODA+L'IOWORK-WORKD,AIO8-WORKD)                    
         DC    AL2(IOAREA9+L'IODA+L'IOWORK-WORKD,AIO9-WORKD)                    
         DC    AL2(IOAREAA+L'IODA+L'IOWORK-WORKD,AIOA-WORKD)                    
         DC    AL2(SWSTAB-WORKD,ASWSTAB-WORKD)                                  
         DC    AL2(GOPBLK-WORKD,AGOPBLK-WORKD)                                  
         DC    AL2(GOXBLK-WORKD,AGOXBLK-WORKD)                                  
         DC    AL2(OFFBLK-WORKD,AOFFBLK-WORKD)                                  
         DC    AL2(VATBLK-WORKD,AVATBLK-WORKD)                                  
         DC    AL2(ADTBLK-WORKD,AADTBLK-WORKD)                                  
         DC    AL2(JOBBLK-WORKD,AJOBBLK-WORKD)                                  
         DC    AL2(COLTAB-WORKD,ACOLTAB-WORKD)                                  
         DC    AL2(OPVTAB-WORKD,AOPVTAB-WORKD)                                  
         DC    AL2(TSABLK-WORKD,ATSABLK-WORKD)                                  
         DC    AL2(OVERWRK-WORKD,AOVERWRK-WORKD)                                
         DC    AL2(WORKX-WORKD,AWORKX-WORKD)                                    
ANAWSN   EQU   (*-ANAWS)/L'ANAWS                                                
*                                                                               
DICUPR   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
         DCDDL AC#PFK,L'UC@PFKEY+1                                              
         DCDDL AC#ALTPF,L'UC@ALTPF                                              
         DCDDL AC#HELP,L'UC@HELP                                                
         DCDDL AC#BAT,RECNAMLQ                                                  
         DCDDL AC#ITEM,RECNAMLQ                                                 
         DCDDL AC#PSTG,RECNAMLQ                                                 
         DCDDL AC#CLOSE,ACTNAMLQ                                                
         DCDDL AC#DSP,ACTNAMLQ                                                  
         DCDDL AC#LIST,ACTNAMLQ                                                 
         DCDDL AC#UPD,ACTNAMLQ                                                  
         DCDDL AC#OPEN,ACTNAMLQ                                                 
         DCDDL AC#CHG,ACTNAMLQ                                                  
         DCDDL AC#INP,ACTNAMLQ                                                  
         DCDDL AC#RECAL,ACTNAMLQ                                                
         DCDDL AC#RETRN,ACTNAMLQ                                                
         DCDDL AC#FIRST,ACTNAMLQ                                                
         DCDDL AC#APRV,ACTNAMLQ                                                 
         DCDDL AC#SAVE,ACTNAMLQ                                                 
         DCDDL AC#MANBL,L'UC@MANBL                                              
         DCDDL AC#DELD,L'UC@DELD                                                
         DCDDL AC#CLSD,L'UC@CLSD                                                
         DCDDL AC#UPDTD,L'UC@UPDTD                                              
         DCDDL AC#INSUP,L'UC@INSUP                                              
         DCDDL AC#APRVD,L'UC@APRVD                                              
         DCDDL AC#SAVED,L'UC@SAVED                                              
         DCDDL AC#OPEN,L'UC4OPEN                                                
         DCDDL AC#DELD,L'UC3DELD                                                
         DCDDL AC#CLSD,L'UC3CLSD                                                
         DCDDL AC#UPDTD,L'UC3UPDTD                                              
         DCDDL AC#INSUP,L'UC3INSUP                                              
         DCDDL AC#APRVD,L'UC3APRVD                                              
         DCDDL AC#SAVED,L'UC3SAVED                                              
         DCDDL AC#DATOP,L'UC@CRTD                                               
         DCDDL AC#EFF,L'UC@EFF                                                  
         DCDDL AC#IDUSR,OPTNAMLQ                                                
         DCDDL AC#IDUSR,OPTSHTLQ                                                
         DCDDL AC#PRSN,OPTNAMLQ                                                 
         DCDDL AC#PRSN,OPTSHTLQ                                                 
         DCDDL AC#GROUP,OPTNAMLQ                                                
         DCDDL AC#GROUP,OPTSHTLQ                                                
         DCDDL AC#REF,OPTNAMLQ                                                  
         DCDDL AC#REF,OPTSHTLQ                                                  
         DCDDL AC#OFF,OPTNAMLQ                                                  
         DCDDL AC#OFF,OPTSHTLQ                                                  
         DCDDL AC#DSP,OPTNAMLQ                                                  
         DCDDL AC#DSP,OPTSHTLQ                                                  
         DCDDL AC#APRVR,OPTNAMLQ                                                
         DCDDL AC#APRVR,OPTSHTLQ                                                
         DCDDL AC#DATE,OPTNAMLQ                                                 
         DCDDL AC#DATE,OPTSHTLQ                                                 
         DCDDL AC#AMT,OPTNAMLQ                                                  
         DCDDL AC#AMT,OPTSHTLQ                                                  
         DCDDL AC#ACC,OPTNAMLQ                                                  
         DCDDL AC#ACC,OPTSHTLQ                                                  
         DCDDL AC#COPY,ACTNAMLQ                                                 
         DCDDL AC#RVRS,ACTNAMLQ                                                 
         DCDDL AC#SEL,ACTNAMLQ                                                  
         DCDDL AC#ANL1,ACTNAMLQ                                                 
         DCDDL AC#NXT,ACTNAMLQ                                                  
         DCDDL AC#YES,L'UC@YES                                                  
         DCDDL AC#NO,L'UC@NO                                                    
         DCDDL AC#DEL,ACTNAMLQ                                                  
         DCDDL AC#UAPRV,ACTNAMLQ                                                
         DCDDL AC#IDGRP,OPTNAMLQ                                                
         DCDDL AC#IDGRP,OPTSHTLQ                                                
         DCDDL AC#IDALL,OPTNAMLQ                                                
         DCDDL AC#IDALL,OPTSHTLQ                                                
         DCDDL AC#ACRL,OPTNAMLQ                                                 
         DCDDL AC#RVRSL,L'UC@RVRSL                                              
         DCDDL AC#ONLY,L'UC@ONLY                                                
         DCDDL AC#MAX,L'UC@MAX                                                  
         DCDDL AC#PAGE,L'UC@PAGE                                                
         DCDDL AC#HALF,L'UC@HALF                                                
         DCDDL AC#ACT,OPTNAMLQ                                                  
         DCDDL AC#ACT,OPTSHTLQ                                                  
         DCDDL AC#MKALL,ACTNAMLQ                                                
         DCDDL AC#CNTRL,RECNAMLQ                                                
         DCDDL AC#NONE,L'UC@NONE                                                
         DCDDL AC#OFF,L'UC2OFF                                                  
         DCDDL AC#LGR,L'UC2LGR                                                  
         DCDDL AC#NAME,OPTNAMLQ                                                 
         DCDDL AC#NAME,OPTSHTLQ                                                 
         DCDDL AC#TYPE,RECNAMLQ                                                 
         DCDDL AC#MAINT,ACTNAMLQ                                                
         DCDDL AC#GENER,ACTNAMLQ                                                
         DCDDL AC#SCRN,RECNAMLQ                                                 
         DCDDL AC#RFRSH,ACTNAMLQ                                                
         DCDDL AC#RVRSL,ACTNAMLQ                                                
         DCDDL AC#NCMSN,L'UC$NCMSN                                              
         DCDDL AC#GENED,ACTNAMLQ                                                
         DCDDL AC#STT,OPTNAMLQ                                                  
         DCDDL AC#OK,L'UC@OK                                                    
         DCDDL AC#EXPAN,L'UC@EXPAN                                              
         DCDDL AC#ANY,L'UC@ANY                                                  
         DCDDL AC#ORDC,L'UC@ORDC                                                
         DCDDL AC#COPY2,L'UC@COPY2                                              
         DCDDL AC#OPEN2,L'UC@OPEN2                                              
         DCDDL AC#OPEN2,L'UC4OPEN2                                              
         DCDDL AC#NOORD,L'UC@NOORD                                              
         DCDDL AC#ORGL,OPTNAMLQ                                                 
         DCDDL AC#ORGL,OPTSHTLQ                                                 
         DCDDL AC#SUBIT,RECNAMLQ                                                
         DCDDL AC#RPT,ACTNAMLQ                                                  
         DCDDL AC#PRINT,ACTNAMLQ                                                
         DCDDL AC#GLU,ACTNAMLQ                                                  
         DCDDL AC#GLUED,ACTNAMLQ                                                
DICUPRX  DC    AL1(EOT)                                                         
*                                                                               
DICMIX   DS    0XL4                ** MIXED CASE DICTIONARY LIST **             
         DCDDL AC#CLINT,L'LC@CLI                                                
         DCDDL AC#PRO,L'LC@PRO                                                  
         DCDDL AC#JOB,L'LC@JOB                                                  
         DCDDL AC#BAT,RECNAMLQ                                                  
         DCDDL AC#ITEM,RECNAMLQ                                                 
         DCDDL AC#PSTG,RECNAMLQ                                                 
         DCDDL AC#CLOSE,ACTNAMLQ                                                
         DCDDL AC#DSP,ACTNAMLQ                                                  
         DCDDL AC#LIST,ACTNAMLQ                                                 
         DCDDL AC#UPD,ACTNAMLQ                                                  
         DCDDL AC#OPEN,ACTNAMLQ                                                 
         DCDDL AC#CHG,ACTNAMLQ                                                  
         DCDDL AC#INP,ACTNAMLQ                                                  
         DCDDL AC#RECAL,ACTNAMLQ                                                
         DCDDL AC#RETRN,ACTNAMLQ                                                
         DCDDL AC#FIRST,ACTNAMLQ                                                
         DCDDL AC#SAVE,ACTNAMLQ                                                 
         DCDDL AC#REF,L'BATRHD1                                                 
         DCDDL AC#NAME,L'BATRHD1                                                
         DCDDL AC#INPTY,L'BATRHD1                                               
         DCDDL AC#MOA,L'BATRHD1                                                 
         DCDDL AC#EFFDT,L'BATRHD1                                               
         DCDDL AC#INSUP,L'BATRHD1                                               
         DCDDL AC#PRSN,L'BATRHD1                                                
         DCDDL AC#DATOP,L'BATRHD1                                               
         DCDDL AC#STT,L'BATRHD1                                                 
         DCDDL AC#ITMCT,L'BATRHD1                                               
         DCDDL AC#ITMSA,L'BATRHD1                                               
         DCDDL AC#BATTO,L'BATRHD1                                               
         DCDDL AC#AMTIN,L'BATRHD1                                               
         DCDDL AC#DELD,L'LC@DELD                                                
         DCDDL AC#CLSD,L'LC@CLSD                                                
         DCDDL AC#UPDTD,L'LC@UPDTD                                              
         DCDDL AC#INSUP,L'LC8INSUP                                              
         DCDDL AC#SAVED,L'LC@SAVED                                              
         DCDDL AC#OPEN,L'LC4OPEN                                                
         DCDDL AC#DELD,L'LC3DELD                                                
         DCDDL AC#CLSD,L'LC3CLSD                                                
         DCDDL AC#UPDTD,L'LC3UPDTD                                              
         DCDDL AC#INSUP,L'LC3INSUP                                              
         DCDDL AC#SAVED,L'LC3SAVED                                              
         DCDDL AC#USRID,L'LC$USRID                                              
         DCDDL AC#REF,L'LC$REF                                                  
         DCDDL AC#BATN,L'LC$BATN                                                
         DCDDL AC#NAME,L'LC$NAME                                                
         DCDDL AC#INPTY,128+L'LC$INPTY                                          
         DCDDL AC#MOA,L'LC$MOA                                                  
         DCDDL AC#EFFDT,L'LC$EFFDT                                              
         DCDDL AC#INSUP,128+L'LC$INSUP                                          
         DCDDL AC#PRSN,L'LC$PRSN                                                
         DCDDL AC#DATOP,L'LC$CRTDT                                              
         DCDDL AC#STT,L'LC$STT                                                  
         DCDDL AC#ITMCT,128+L'LC$ITMCT                                          
         DCDDL AC#ITMSA,128+L'LC$ITMSA                                          
         DCDDL AC#BATTO,L'LC$BATTO,R                                            
         DCDDL AC#AMTIN,L'LC$AMTIN,R                                            
         DCDDL AC#OFF,L'LC$OFF                                                  
         DCDDL AC#CMTS,L'LC$CMTS                                                
         DCDDL AC#APRVR,L'LC$APRVR                                              
         DCDDL AC#GROUP,L'LC$GROUP                                              
         DCDDL AC#LUID,L'LC$LUID                                                
         DCDDL AC#NUM,L'LC$NUM                                                  
         DCDDL AC#ADR,L'LC$ADR                                                  
         DCDDL AC#PAGE,L'LC@PAGE                                                
         DCDDL AC#HALF,L'LC@HALF                                                
         DCDDL AC#ALTPF,L'LC@ALTPF                                              
         DCDDL AC#FRBAT,L'BATRHD1                                               
         DCDDL AC#TOBAT,L'BATRHD1                                               
         DCDDL AC#COPY,ACTNAMLQ                                                 
         DCDDL AC#RVRS,ACTNAMLQ                                                 
         DCDDL AC#SEL,ACTNAMLQ                                                  
         DCDDL AC#ANL1,ACTNAMLQ                                                 
         DCDDL AC#NXT,ACTNAMLQ                                                  
         DCDDL AC#DRTON,L'LC$DRTON                                              
         DCDDL AC#LIVON,L'LC$LIVON                                              
         DCDDL AC#UPDON,L'LC$UPDON                                              
         DCDDL AC#RVRON,L'LC$RVRON                                              
         DCDDL AC#MAUON,L'LC$MAUON                                              
         DCDDL AC#PELON,L'LC$PELON                                              
         DCDDL AC#LSTON,L'LC$LSTON                                              
         DCDDL AC#FEEAJ,L'LC$FEEAJ                                              
         DCDDL AC#BEN,L'LC$BEN                                                  
         DCDDL AC#CMN,L'LC$CMN                                                  
         DCDDL AC#DPR,L'LC$DPR                                                  
         DCDDL AC#CSHDS,L'LC$CSHDS                                              
         DCDDL AC#XFRAM,L'LC$XFRAM                                              
         DCDDL AC#GRSCK,L'LC$GRSCK                                              
         DCDDL AC#FEE,L'LC$FEE                                                  
         DCDDL AC#GRSBD,L'LC$GRSBD                                              
         DCDDL AC#HOURS,L'LC$HOURS                                              
         DCDDL AC#INCM,L'LC$INCM                                                
         DCDDL AC#HRSDC,L'LC$HRSDC                                              
         DCDDL AC#FLTX,L'LC$FLTX                                                
         DCDDL AC#RCVCL,L'LC$RCVCL                                              
         DCDDL AC#GRSMV,L'LC$GRSMV                                              
         DCDDL AC#FLYMT,L'LC$FLYMT                                              
         DCDDL AC#PTYMT,L'LC$PTYMT                                              
         DCDDL AC#CSTRT,L'LC$CSTRT                                              
         DCDDL AC#TCHKS,L'LC$TCHKS                                              
         DCDDL AC#TAXPD,L'LC$TAXPD                                              
         DCDDL AC#VCLS,L'LC$VCLS                                                
         DCDDL AC#VAT,L'LC$VAT                                                  
         DCDDL AC#NOCHD,L'LC$NOCHD                                              
         DCDDL AC#ASBOF,L'LC$ASBOF                                              
         DCDDL AC#CHKOC,L'LC$CHKOC                                              
         DCDDL AC#CODEX,L'LC$CODEX                                              
         DCDDL AC#TOLAM,L'LC$TOLAM                                              
         DCDDL AC#TOLPC,L'LC$TOLPC                                              
         DCDDL AC#MAXCB,L'LC$MAXCB                                              
         DCDDL AC#MAXDB,L'LC$MAXDB                                              
         DCDDL AC#RVRSD,L'LC@RVRSD                                              
         DCDDL AC#PELED,L'LC@PELED                                              
         DCDDL AC#USED,L'LC@USED                                                
         DCDDL AC#AUTHD,L'LC@AUTHD                                              
         DCDDL AC#NRTV,L'LC@NRTV                                                
         DCDDL AC#DRAFT,L'LC@DRAFT                                              
         DCDDL AC#DEL,ACTNAMLQ                                                  
         DCDDL AC#WC,L'LC@WC                                                    
         DCDDL AC#CHKC,L'LC$CHKC                                                
         DCDDL AC#WRTRF,L'LC$WRTRF                                              
         DCDDL AC#WRTFA,L'LC$WRTFA                                              
         DCDDL AC#XFRFR,L'LC$XFRFR                                              
         DCDDL AC#XFRTO,L'LC$XFRTO                                              
         DCDDL AC#EFFON,L'LC$EFFON                                              
         DCDDL AC#PSTGM,L'LC$PSTGM                                              
         DCDDL AC#RVRMA,L'LC$RVRMA                                              
         DCDDL AC#MAUMA,L'LC$MAUMA                                              
         DCDDL AC#CHKDT,L'LC$CHKDT                                              
         DCDDL AC#WRTFO,L'LC$WRTFO                                              
         DCDDL AC#CHKDP,L'LC$CHKDP                                              
         DCDDL AC#OFFSO,L'LC$OFFSO                                              
         DCDDL AC#XFRON,L'LC$XFRON                                              
         DCDDL AC#XFRMA,L'LC$XFRMA                                              
         DCDDL AC#VALAC,L'LC@VALAC                                              
         DCDDL AC#YES,L'LC@YES                                                  
         DCDDL AC#NO,L'LC@NO                                                    
         DCDDL AC#APRV,ACTNAMLQ                                                 
         DCDDL AC#UAPRV,ACTNAMLQ                                                
         DCDDL AC#APRVD,L'LC@APRVD                                              
         DCDDL AC#APRVD,L'LC3APRVD                                              
         DCDDL AC#USRID,L'BATRHD1                                               
         DCDDL AC#APRVR,L'BATRHD1                                               
         DCDDL AC#ACRL,L'BATRHD1                                                
         DCDDL AC#RVRSL,L'LC@RVRSL                                              
         DCDDL AC#SUBR,L'LC@SUBR                                                
         DCDDL AC#LEFT,ACTNAMLQ                                                 
         DCDDL AC#RIGHT,ACTNAMLQ                                                
         DCDDL AC#UP,ACTNAMLQ                                                   
         DCDDL AC#DOWN,ACTNAMLQ                                                 
         DCDDL AC#OTHRI,L'LC@OTHRI                                              
         DCDDL AC#CTR,L'LC@CTR                                                  
         DCDDL AC#URG,L'LC@URG                                                  
         DCDDL AC#NCOM,L'LC@NCOM                                                
         DCDDL AC#MKALL,ACTNAMLQ                                                
         DCDDL AC#UPDGL,L'LC@UPDGL                                              
         DCDDL AC#BLD,L'LC@BLD                                                  
         DCDDL AC#PAID,L'LC@PAID                                                
         DCDDL AC#CTRD,L'LC@CTRD                                                
         DCDDL AC#PTYMO,L'LC$PTYMO                                              
         DCDDL AC#FLYMO,L'LC$FLYMO                                              
         DCDDL AC#BLDON,L'LC$BLDON                                              
         DCDDL AC#PAION,L'LC$PAION                                              
         DCDDL AC#CTRON,L'LC$CTRON                                              
         DCDDL AC#BLGMA,L'LC$BLGMA                                              
         DCDDL AC#PAIMA,L'LC$PAIMA                                              
         DCDDL AC#CTRMA,L'LC$CTRMA                                              
         DCDDL AC#UPGON,L'LC$UPGON                                              
         DCDDL AC#CNTRL,RECNAMLQ                                                
         DCDDL AC#LIVE,L'LC@LIVE                                                
         DCDDL AC#TYPE,L'LC@TYPE                                                
         DCDDL AC#TIME,L'LC@TIME                                                
         DCDDL AC#MSNG,L'LC@MSNG                                                
         DCDDL AC#ADJ,L'LC@ADJ                                                  
         DCDDL AC#TAX,L'LC@TAX                                                  
         DCDDL AC#CHK,L'LC@CHK                                                  
         DCDDL AC#INV,L'LC@INV                                                  
         DCDDL AC#RFRSH,ACTNAMLQ                                                
         DCDDL AC#1ST,L'LC@1ST                                                  
         DCDDL AC#2ND,L'LC@2ND                                                  
         DCDDL AC#ERASE,L'LC@ERASE                                              
         DCDDL AC#DR,L'LC$DR,R                                                  
         DCDDL AC#CR,L'LC$CR,R                                                  
         DCDDL AC#MAINT,ACTNAMLQ                                                
         DCDDL AC#ACC1,L'LC@ACC1                                                
         DCDDL AC#ACC2,L'LC@ACC2                                                
         DCDDL AC#AMT,L'LC@AMT,R                                                
         DCDDL AC#SEQNO,L'LC@SEQNO                                              
         DCDDL AC#GENER,ACTNAMLQ                                                
         DCDDL AC#SCRN,RECNAMLQ                                                 
         DCDDL AC#GRSFI,L'LC$GRSFI                                              
         DCDDL AC#MILGE,L'LC$MILGE                                              
         DCDDL AC#INSAM,L'LC$INSAM                                              
         DCDDL AC#FEEAM,L'LC$FEEAM                                              
         DCDDL AC#AMPEX,L'LC$AMPEX                                              
         DCDDL AC#RCNAM,L'LC$RCNAM                                              
         DCDDL AC#INVTO,L'LC$INVTO                                              
         DCDDL AC#MEMOT,L'LC$MEMOT                                              
         DCDDL AC#VSCHN,L'LC$VSCHN                                              
         DCDDL AC#HELD2,L'LC@HELD                                               
         DCDDL AC#SELED,L'LC@SELED                                              
         DCDDL AC#COPY,L'LC3COPY                                                
         DCDDL AC#RVRSL,L'LC3RVRSL                                              
         DCDDL AC#GENED,L'LC@GENED                                              
         DCDDL AC#GENED,L'LC3GENED                                              
         DCDDL AC#RCND,L'LC@RCND                                                
         DCDDL AC#ACRRV,L'LC@ACRRV                                              
         DCDDL AC#UAPR,L'LC@UAPR                                                
         DCDDL AC#UAPR,L'LC3UAPR                                                
         DCDDL AC#BLBCS,L'LC@BLBCS                                              
         DCDDL AC#DUEON,L'LC$DUEON                                              
         DCDDL AC#VATAM,L'LC$VATAM                                              
         DCDDL AC#CURRY,L'LC@CURRY,R                                            
         DCDDL AC#BLB,L'LC@BLB                                                  
         DCDDL AC#NBLB,L'LC@NBLB                                                
         DCDDL AC#COPY2,L'LC@COPY2                                              
         DCDDL AC#COPY2,L'LC3COPY2                                              
         DCDDL AC#OPEN2,L'LC@OPEN2                                              
         DCDDL AC#OPEN2,L'LC4OPEN2                                              
         DCDDL AC#CTAX,L'LC@CTAX                                                
         DCDDL AC#MEMEX,L'LC$MEMEX                                              
         DCDDL AC#CURBT,L'LC@CURBT                                              
         DCDDL AC#ORGBT,L'LC@ORGBT                                              
         DCDDL AC#CURRY,L'LC@CURRL                                              
         DCDDL AC#VPB,L'LC@VPB                                                  
         DCDDL AC#SUBIT,RECNAMLQ                                                
         DCDDL AC#DEBSC,ACTNAMLQ                                                
         DCDDL AC#TOP,ACTNAMLQ                                                  
         DCDDL AC#CBOTT,ACTNAMLQ                                                
         DCDDL AC#RETRN,ACTNAMLQ                                                
         DCDDL AC#ADD,ACTNAMLQ                                                  
         DCDDL AC#RPT,ACTNAMLQ                                                  
         DCDDL AC#PRINT,ACTNAMLQ                                                
         DCDDL AC#VNDR,ACTNAMLQ                                                 
         DCDDL AC#DTL,ACTNAMLQ                                                  
         DCDDL AC#ZOOM,ACTNAMLQ                                                 
         DCDDL AC#MEMO,ACTNAMLQ                                                 
         DCDDL AC#TMSTT,L'LC@TMSTT                                              
         DCDDL AC#POST,ACTNAMLQ                                                 
         DCDDL AC#MULT,ACTNAMLQ                                                 
         DCDDL AC#FIS,L'LC@FIS                                                  
         DCDDL AC#GLU,ACTNAMLQ                                                  
         DCDDL AC#GLU,L'LC3GLU                                                  
         DCDDL AC#GLUED,L'LC@GLUED                                              
         DCDDL AC#GLUED,L'LC3GLUED                                              
DICMIXX  DC    AL1(EOT)                                                         
*                                                                               
DICGEN   DS    0XL4                ** UPPER CASE DICTIONARY LIST **             
         DCDDL AC#YES,4                                                         
         DCDDL AC#NO,4                                                          
         DCDDL AC#DR,3                                                          
         DCDDL AC#CR,3                                                          
DICGENX  DC    AL1(EOT)                                                         
         EJECT                                                                  
HDRTAB   DS    0X                  ** HEADER DISPLAY TABLE **                   
*                                                                               
HDRTAB1  DC    AL2(0,0)            BATCH/(NOMINATE)                             
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2DP)                                              
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DISP)                                                    
HDRTAB1X DS    0X                                                               
*                                                                               
HDRTAB2  DC    AL2(0,0)            BATCH/DISPLAY FROM ITEM/INPUT                
         DC    AL2(LC@USRID-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2DHP)                                             
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DHP+HDR2PROT)                                            
         DC    AL1(HDR1DHP)                                                     
HDRTAB2X DS    0X                                                               
*                                                                               
HDRTAB3  DC    AL2(0,0)            BATCH/UPDATE FROM ITEM/INPUT                 
         DC    AL2(LC@USRID-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DP)                                                      
HDRTAB3X DS    0X                                                               
*                                                                               
HDRTAB4  DC    AL2(0,0)            BATCH/DISPLAY                                
         DC    AL2(LC@USRID-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2DP)                                              
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DH+HDR2PROT)                                             
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DH+HDR2PROT)                                             
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DP)                                                      
HDRTAB4X DS    0X                                                               
*                                                                               
HDRTAB5  DC    AL2(LC@FRBAT-TWAD,LC@TOBAT-TWAD)  BATCH/COPY ENTRY               
         DC    AL2(LC@USRID-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2HIGH)                                             
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DP+HDR2DISP)                                             
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DISP)                                                    
HDRTAB5X DS    0X                                                               
*                                                                               
HDRTAB6  DC    AL2(0,0)            BATCH/CHANGE                                 
         DC    AL2(LC@USRID-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DH+HDR2PROT)                                             
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DH+HDR2PROT)                                             
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DH)                                                      
HDRTAB6X DS    0X                                                               
*                                                                               
HDRTAB7  DC    AL2(0,0)            BATCH/OPEN, ENTRY SCREEN                     
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2DP)                                              
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL1(HDR1DISP)                                                    
HDRTAB7X DS    0X                                                               
*                                                                               
HDRTAB8  DC    AL2(0,0)            BATCH/OPEN, AFTER SUCCESSFUL OPEN            
         DC    AL2(0)                                                           
         DC    AL1(HDR1PROT+HDR2PROT)                                           
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2DP)                                              
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDRH1DH+HDR2PROT)                                            
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DISP+HDR2PROT)                                           
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DISP)                                                    
HDRTAB8X DS    0X                                                               
*                                                                               
HDRTAB9  DC    AL2(LC@FRBAT-TWAD,LC@TOBAT-TWAD)  BATCH/GENL UPDATE              
         DC    AL2(LC@USRID-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@REF-TWAD)                                                 
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@NAME-TWAD)                                                
         DC    AL1(HDRH1DP+HDR2DH)                                              
         DC    AL2(LC@INPTY-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@MOA-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@CRTDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@EFFDT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@INSUP-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@PRSN-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@STT-TWAD)                                                 
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ITMCT-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@ITMSA-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@BATTO-TWAD)                                               
         DC    AL1(HDR1DP+HDR2DP)                                               
         DC    AL2(LC@AMTIN-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@ACRL-TWAD)                                                
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL2(LC@APRVR-TWAD)                                               
         DC    AL1(HDR1DP+HDR2PROT)                                             
         DC    AL1(HDR1DH)                                                      
HDRTAB9X DS    0X                                                               
*                                                                               
HDRTABX  DS    0X                                                               
         EJECT                                                                  
LANGCHAR DS    0CL8                ** LANGUAGE DEPENDANT CHARACTERS **          
         DC    CL8',=?/#'          ENGLISH                                      
         DC    CL8',=?/#'          AMERICAN                                     
         DC    CL8'#=?/#'          GERMAN                                       
         DC    CL8',=?/#'          FRENCH                                       
         DC    CL8',=?/#'          SPANISH                                      
         DC    CL8',=?/#'          ITALIAN                                      
         DC    CL8';=?/#'          DUTCH                                        
         DC    CL8',=?/#'          ???                                          
         DC    CL8',=?/#'          ???                                          
         DC    CL8',=?/#'          ???                                          
         DC    CL8',=?/#'          ???                                          
         DC    CL8',=?/#'          ???                                          
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QADDTRN)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QREPORT)                                                     
         DC    AL1(QREQTWA)                                                     
         DC    AL1(QSPOON)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QBATFAC1)                                                    
         DC    AL1(QBATFAC2)                                                    
         DC    AL1(QBATFAC3)                                                    
         DC    AL1(QQSORT)                                                      
         DC    AL1(QGETIDS)                                                     
PHASESN  EQU   *-PHASES                                                         
         SPACE 1                                                                
ADDRS    DS    0F                  ** CONTROLLER ADDRESSES **                   
         DC    V(AMTVAL)                                                        
         DC    V(CONVMOS)                                                       
         DC    V(BMONVAL)                                                       
         DC    V(VATICAN)                                                       
         DC    V(ACJOBCOL)                                                      
         DC    V(CATCALL)                                                       
         DC    V(EXCEL)                                                         
         DC    A(0)                                                             
         DC    A(SYSTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    A(FILTAB)                                                        
         DC    A(RECTAB)                                                        
         DC    A(ACTTAB)                                                        
         DC    A(MIXTAB)                                                        
         DC    A(PFKTAB)                                                        
         DC    A(SELTAB)                                                        
         DC    A(TYPTAB)                                                        
         DC    A(DICUPR)                                                        
         DC    A(DICMIX)                                                        
         DC    A(DICGEN)                                                        
         DC    A(HDRTAB)                                                        
ADDRSN   EQU   (*-ADDRS)/L'ADDRS                                                
*                                                                               
CTRYATB  DS    0XL8                ** COUNTRY GLOBAL ATTRIBUTES **              
         DC    XL8'31025CA800000000'  1 - GBR                                   
         DC    XL8'3103A11000000000'  2 - USA                                   
         DC    XL8'4EF4466800000000'  3 - GERMANY                               
         DC    XL8'00'                4 - N/D FRANCE                            
         DC    XL8'00'                5 - N/D SPAIN                             
         DC    XL8'00'                6 - N/D ITALY                             
         DC    XL8'4AF4466800000000'  7 - HOLLAND                               
         DC    XL8'3103A11000000000'  8 - CANADA                                
         DC    XL8'00'                9 - N/D IRELAND                           
*                                                                               
LANGATB  DS    0XL8                ** LANGUAGE GLOBAL ATTRIBUTES **             
         DC    XL8'0000000000000000'  1 - UK ENGLISH                            
         DC    XL8'0000000000000000'  2 - US ENGLISH                            
         DC    XL8'8008000000000000'  3 - GERMAN                                
         DC    XL8'00'                4 - N/D FRENCH                            
         DC    XL8'00'                5 - N/D SPANISH                           
         DC    XL8'00'                6 - N/D ITALIAN                           
         DC    XL8'8008000000000000'  7 - DUTCH                                 
         EJECT                                                                  
***********************************************************************         
* SYSTEM FILE NAMES TABLE (FILE NUMBERS 1 THRU 9)                     *         
***********************************************************************         
         SPACE 1                                                                
FILTAB   DS    0X                                                               
*                                  ** ACCOUNT SYSTEM FILES **                   
FILACC   DC    AL1(6),C'ACC',AL2(FILACCX-*)                                     
*                                                                               
         DC    AL1(IOACCFIL/256),C'ACCFIL '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,42,49),AL2(1000)                                           
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCDIR/256),C'ACCDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOACCMST/256,42,08),AL2(54)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCMST/256),C'ACCMST '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOACCDIR/256,42,56),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOACCARC/256),C'ACCARC '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOACCDIR/256,42,56),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
FILACCX  DC    AL1(EOT)                                                         
*                                  ** CONTROL SYSTEM FILES **                   
FILCON   DC    AL1(10),C'CON',AL2(FILCONX-*)                                    
*                                                                               
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,42),AL2(2000)                                
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(IOCONFIL/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,28),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
FILCONX  DC    AL1(EOT)                                                         
*                                                                               
FILTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF GLOBAL SYSTEM FILES (FILE NUMBERS 10 THRU 15)              *         
***********************************************************************         
         SPACE 1                                                                
SYSTAB   DS    0X                                                               
*                                                                               
         DC    AL1(15),C'CTFILE '                                               
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
SYSTABX  DS    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SYSTEM FILE COMMANDS TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
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
*                                                                               
CMDTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD TYPE TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RECTAB   DS    0X                                                               
*                                                                               
         DC    AL1(RECBAT)                                                      
         DC    AL2(UC@BAT-TWAD,LC@BAT-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(2002)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(RECITE)                                                      
         DC    AL2(UC@ITEM-TWAD,LC@ITEM-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(2003)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(RECPST)                                                      
         DC    AL2(UC@PSTG-TWAD,LC@PSTG-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(2004)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(RECCON)                                                      
         DC    AL2(UC@CNTRL-TWAD,LC@CNTRL-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(2005)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(RECTYP)                                                      
         DC    AL2(UC@TYPE-TWAD,LC@TYPE-TWAD)                                   
         DC    AL1(RECIDDS+RECINOH)                                             
         DC    AL1(0,0,0)                                                       
         DC    AL2(2006)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(RECSCR)                                                      
         DC    AL2(UC@SCRN-TWAD,LC@SCRN-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(2007)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(RECSIT)                                                      
         DC    AL2(UC@SUBIT-TWAD,LC@SUBIT-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(2008)                                                        
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
RECTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ACTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
ACTTAB   DS    0X                                                               
*                                                                               
         DC    AL1(ACTOPN)                                                      
         DC    AL2(UC@OPEN-TWAD,LC@OPEN-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTINP)                                                      
         DC    AL2(UC@INP-TWAD,LC@INP-TWAD)                                     
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTDSP)                                                      
         DC    AL2(UC@DSP-TWAD,LC@DSP-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTCHA)                                                      
         DC    AL2(UC@CHG-TWAD,LC@CHG-TWAD)                                     
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTLST)                                                      
         DC    AL2(UC@LIST-TWAD,LC@LIST-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTUPD)                                                      
         DC    AL2(UC@UPD-TWAD,LC@UPD-TWAD)                                     
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTCLO)                                                      
         DC    AL2(UC@CLOSE-TWAD,LC@CLOSE-TWAD)                                 
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTREC)                                                      
         DC    AL2(UC@RECAL-TWAD,LC@RECAL-TWAD)                                 
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTQUI)                                                      
         DC    AL2(UC@QUIT-TWAD,LC@QUIT-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTAPF)                                                      
         DC    AL2(UC@ALTPF-TWAD,LC@ALTPF-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTFST)                                                      
         DC    AL2(UC@FIRST-TWAD,LC@FIRST-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTSAV)                                                      
         DC    AL2(UC@SAVE-TWAD,LC@SAVE-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTCOP)                                                      
         DC    AL2(UC@COPY-TWAD,LC@COPY-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTREV)                                                      
         DC    AL2(UC@RVRS-TWAD,LC@RVRS-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTSEL)                                                      
         DC    AL2(UC@SEL-TWAD,LC@SEL-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTANL)                                                      
         DC    AL2(UC@ANL1-TWAD,LC@ANL1-TWAD)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTNXT)                                                      
         DC    AL2(UC@NXT-TWAD,LC@NXT-TWAD)                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTDEL)                                                      
         DC    AL2(UC@DEL-TWAD,LC@DEL-TWAD)                                     
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTAPR)                                                      
         DC    AL2(UC@APRV-TWAD,LC@APRV-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTUAP)                                                      
         DC    AL2(UC@UAPRV-TWAD,LC@UAPRV-TWAD)                                 
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTAUT)                                                      
         DC    AL2(UC@AUTO-TWAD,LC@AUTO-TWAD)                                   
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTMNT)                                                      
         DC    AL2(UC@MAINT-TWAD,LC@MAINT-TWAD)                                 
         DC    AL1(ACTIDDS+ACTINOH+ACTIUPD)                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTGEN)                                                      
         DC    AL2(UC@GENER-TWAD,LC@GENER-TWAD)                                 
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTRFS)                                                      
         DC    AL2(UC@RFRSH-TWAD,LC@RFRSH-TWAD)                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTRPT)                                                      
         DC    AL2(UC@RPT-TWAD,LC@RPT-TWAD)                                     
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTPRT)                                                      
         DC    AL2(UC@PRINT-TWAD,LC@PRINT-TWAD)                                 
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(ACTGLU)                                                      
         DC    AL2(UC@GLU-TWAD,LC@GLU-TWAD)                                     
         DC    AL1(ACTIUPD)                                                     
         DC    AL1(0,0,0)                                                       
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* RECORD/ACTION COMBO TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
MIXTAB   DS    0X                                                               
*                                                                               
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0),AL1(BATROVER)                                     
         DC    AL2(2108)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTDSP)                                               
         DC    AL1(0,0,0,0,0),AL1(BATROVER)                                     
         DC    AL2(2109)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL1(RECBAT,0,2)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0),AL1(BATROVER)                                     
         DC    AL2(2110)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,MIXIMARK,0,0,0),AL1(BATROVER)                              
         DC    AL2(2111)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(3)                                                           
         DC    AL1(RECBAT,0,2)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,MIXIMARK,0,0,0),AL1(BATROVER)                              
         DC    AL2(2112)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(4)                                                           
         DC    AL1(RECBAT,0,2)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTCHA)                                               
         DC    AL1(0,0,0,0,0),AL1(BATROVER)                                     
         DC    AL2(2113)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(5)                                                           
         DC    AL1(RECBAT,0,2)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,MIXIUNDO,0,0,0),AL1(BATROVER)                              
         DC    AL2(2114)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(6)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTSAV)                                               
         DC    AL1(0,0,0,0,0),AL1(BATROVER)                                     
         DC    AL2(2115)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(7)                                                           
         DC    AL1(RECBAT,0,2)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTDEL)                                               
         DC    AL1(0,MIXIUNDO,0,0,0),AL1(BATROVER)                              
         DC    AL2(2116)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(8)                                                           
         DC    AL1(RECBAT,0,2)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTAPR)                                               
         DC    AL1(MIXISEL,0,0,0,0),AL1(BATROVER)                               
         DC    AL2(2117)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTUAP)                                               
         DC    AL1(MIXISEL,0,0,0,0),AL1(BATROVER)                               
         DC    AL2(2118)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(10)                                                          
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECITE,ACTINP)                                               
         DC    AL1(0,MIXIMARK,0,0,0),AL1(ITEMOVER)                              
         DC    AL2(2119)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(RECBAT,0,10)                                                 
*                                                                               
         DC    AL1(RECITE,ACTDSP)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2120)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD3)                                                    
         DC    AL1(1)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2121)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(2)                                                           
         DC    AL1(RECBAT,201,0)                                                
*                                                                               
         DC    AL1(RECITE,ACTSEL)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2122)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(3)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTCOP)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2123)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(4)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTREV)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2124)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(5)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECITE,ACTCHA)                                               
         DC    AL1(MIXISEL+MIXINOH,MIXIMARK,0,0,0),AL1(ITEMOVER)                
         DC    AL2(2125)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD3)                                                    
         DC    AL1(6)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECITE,ACTDEL)                                               
         DC    AL1(MIXISEL+MIXINOH,MIXIUNDO,0,0,0),AL1(ITEMOVER)                
         DC    AL2(2126)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD3)                                                    
         DC    AL1(7)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTGEN)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2135)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(8)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECSCR,ACTSEL)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2136)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(9)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECPST,ACTLST)                                               
         DC    AL1(MIXISEL,0,0,0,0),AL1(POSTOVER)                               
         DC    AL2(2127)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL1(RECITE,202,0)                                                
*                                                                               
         DC    AL1(RECPST,ACTDSP)                                               
         DC    AL1(MIXISEL,0,0,0,0),AL1(POSTOVER)                               
         DC    AL2(2128)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECPST,0,0)                                                  
*                                                                               
         DC    AL1(RECPST,ACTANL)                                               
         DC    AL1(MIXISEL,0,0,0,0),AL1(POSTOVER)                               
         DC    AL2(2129)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECPST,0,0)                                                  
*                                                                               
         DC    AL1(RECCON,ACTDSP)                                               
         DC    AL1(0,0,0,0,0),AL1(CTRLOVER)                                     
         DC    AL2(2130)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECCON,0,0)                                                  
*                                                                               
         DC    AL1(RECCON,ACTCHA)                                               
         DC    AL1(0,0,0,0,0),AL1(CTRLOVER)                                     
         DC    AL2(2131)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL1(RECCON,0,0)                                                  
*                                                                               
         DC    AL1(RECTYP,ACTMNT)                                               
         DC    AL1(MIXIDDS,0,0,0,0),AL1(TYPEOVER)                               
         DC    AL2(2134)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECTYP,0,0)                                                  
*                                                                               
         DC    AL1(RECSIT,ACTINP)                                               
         DC    AL1(0,MIXIMARK,0,0,0),AL1(ITEMOVER)                              
         DC    AL2(2135)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD2)                                                    
         DC    AL1(0)                                                           
         DC    AL1(RECBAT,0,10)                                                 
*                                                                               
         DC    AL1(RECSIT,ACTDSP)                                               
         DC    AL1(0,0,0,0,0),AL1(ITEMOVER)                                     
         DC    AL2(2136)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD3)                                                    
         DC    AL1(1)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECSIT,ACTCHA)                                               
         DC    AL1(MIXISEL+MIXINOH,MIXIMARK,0,0,0),AL1(ITEMOVER)                
         DC    AL2(2137)                                                        
         DC    AL2(AS$BATD1)                                                    
         DC    AL2(AS$BATD3)                                                    
         DC    AL1(6)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTRPT)                                               
         DC    AL1(0,0,0,0,0),AL1(RPRTOVER)                                     
         DC    AL2(2138)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTPRT)                                               
         DC    AL1(MIXISEL+MIXINOH,0,0,0,0),AL1(RPRTOVER)                       
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
         DC    AL1(RECBAT,ACTGLU)                                               
         DC    AL1(0,MIXIMARK,0,0,0),AL1(ITEMOVER)                              
         DC    AL2(2140)                                                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(10)                                                          
         DC    AL1(RECBAT,0,0)                                                  
*                                                                               
MIXTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* PROGRAM FUNCTION KEY TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
PFKTAB   DS    0X                                                               
*                                                                               
PFIRSTS  DC    AL1(FF,FF)                                                       
         DC    AL2(PFIRSTSX+1-PFIRSTS)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCDISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTDSP)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCCHAQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTCHA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRPRTQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTRPT)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PFIRSTSX DC    AL1(EOT)                                                         
*                                                                               
PBATOPN  DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL2(PBATOPNX+1-PBATOPN)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKSAVEQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTSAV)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRECLQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATOPNX DC    AL1(EOT)                                                         
*                                                                               
PBATCLO  DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL2(PBATCLOX+1-PBATCLO)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&UK                                                                           
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*&&US                                                                           
         DC    AL1(PFKSAVEQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTSAV)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATSAV)                       
*&&                                                                             
         DC    AL1(PFKRECLQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATRCL)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*                                                                               
         DC    AL1(PFKCHNGQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCHA)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCHA)                       
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATCLOX DC    AL1(EOT)                                                         
*                                                                               
PBATUPD  DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL2(PBATUPDX+1-PBATUPD)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&UK                                                                           
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&                                                                             
*&&US                                                                           
         DC    AL1(PFKSAVEQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTSAV)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATSAV)                       
*&&                                                                             
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKRVRSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTREV)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITEREV)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATUPDX DC    AL1(EOT)                                                         
*                                                                               
PBATDSP  DC    AL1(RECBAT,ACTDSP)                                               
         DC    AL2(PBATDSPX+1-PBATDSP)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRECLQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATRCL)                       
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATDSPX DC    AL1(EOT)                                                         
*                                                                               
PBATSAV  DC    AL1(RECBAT,ACTSAV)                                               
         DC    AL2(PBATSAVX+1-PBATSAV)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRECLQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATRCL)                       
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATSAVX DC    AL1(EOT)                                                         
*                                                                               
PBATREC  DC    AL1(RECBAT,ACTREC)                                               
         DC    AL2(PBATRECX+1-PBATREC)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATRECX DC    AL1(EOT)                                                         
*                                                                               
PBATDEL  DC    AL1(RECBAT,ACTDEL)                                               
         DC    AL2(PBATDELX+1-PBATDEL)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATDELX DC    AL1(EOT)                                                         
*                                                                               
PBATCHA  DC    AL1(RECBAT,ACTCHA)                                               
         DC    AL2(PBATCHAX+1-PBATCHA)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRECLQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATRCL)                       
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATCHAX DC    AL1(EOT)                                                         
*                                                                               
PBATLST  DC    AL1(RECBAT,ACTLST)                                               
         DC    AL2(PBATLSTX+1-PBATLST)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKALTPQ)                                                    
         DC    AL1(PFKIALTP,0)                                                  
         DC    AL1(0,ACTAPF)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATLSTX DC    AL1(EOT)                                                         
*                                                                               
PITELST  DC    AL1(RECITE,ACTLST)                                               
         DC    AL2(PITELSTX+1-PITELST)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRECLQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTREC)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATRCL)                       
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKALTPQ)                                                    
         DC    AL1(PFKIALTP,0)                                                  
         DC    AL1(0,ACTAPF)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRFSHQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTRFS)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PITELSTX DC    AL1(EOT)                                                         
*                                                                               
PITESEL  DC    AL1(RECITE,ACTSEL)                                               
         DC    AL2(PITESELX+1-PITESEL)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKDISPQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTDSP)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,RECBAT),AL2(LMBATDSP)                
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKSAVEQ)                                                    
         DC    AL1(PFKIQUIT+PFKITSEC,0)                                         
         DC    AL1(0,ACTSAV)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIQUIT+PFKITSEC,0)                                         
         DC    AL1(0,ACTCLO)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIQUIT+PFKITSEC,0)                                         
         DC    AL1(0,ACTUPD)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKALTPQ)                                                    
         DC    AL1(PFKIALTP,0)                                                  
         DC    AL1(0,ACTAPF)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PITESELX DC    AL1(EOT)                                                         
*                                                                               
PITEINP  DC    AL1(RECITE,ACTINP)                                               
         DC    AL2(PITEINPX+1-PITEINP)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKDISPQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTDSP)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,RECBAT),AL2(LMBATDSP)                
*                                                                               
         DC    AL1(PFKSAVEQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTSAV)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATSAV)                       
*                                                                               
         DC    AL1(PFKCLOSQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTCLO)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATCLO)                       
*                                                                               
         DC    AL1(PFKUPDTQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTUPD)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMBATUPD)                       
*&&US                                                                           
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT01,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT03,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT06,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT21,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT22,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT26,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT33,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT46,0,0,0,0,0,0,0)       CANADIAN SCREEN                    
*&&                                                                             
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*&&UK                                                                           
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT17,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT17,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT45,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT45,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT49,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT49,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT75,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT75,0,0,0,0,0,0,0)                                          
*&&                                                                             
*&&US                                                                           
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN+CTRYNOT)                                    
         DC    AL2(LC@TAX-TWAD)                                                 
         DC    AL1(BT01,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT01,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT01,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN+CTRYNOT)                                    
         DC    AL2(LC@TAX-TWAD)                                                 
         DC    AL1(BT03,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT03,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT03,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT01,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT03,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT06,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@FIS-TWAD)                                                 
         DC    AL1(BT10,0,0,0,0,0),AL2(BPFKFIS)                                 
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT10,0,0,0,0,0),AL2(BPFKDTL)                                 
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@MULT-TWAD)                                                
         DC    AL1(BT10,0,0,0,0,0),AL2(BPFKMUL)                                 
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TAX-TWAD)                                                 
         DC    AL1(BT10,0,0,0,0,0),AL2(BPFKTAX)                                 
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT10,0,0,0,0,0),AL2(BPFKINV)                                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT21,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT22,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT26,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT33,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT46,0,0,0,2,0,0,0)         CANADIAN TAX SCREEN              
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN+CTRYNOT)                                    
         DC    AL2(LC@TAX-TWAD)                                                 
         DC    AL1(BT49,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT49,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TIME-TWAD)                                                
         DC    AL1(BT49,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT34,0,0,0,0,0,0,0)         DETAIL SCREEN                    
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT34,0,0,0,2,0,0,0)         JOB TRNSFR SCREEN                
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT34,0,0,0,2,0,0,0)         JOB TRNSFR SCREEN                
*                                                                               
         DC    AL1(PFK13)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ALTPF-TWAD)                                               
         DC    AL1(BT34,0,0,0,2,0,0,0)         JOB TRNSFR SCREEN                
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT34,0,0,0,2,0,0,0)         JOB TRNSFR SCREEN                
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT60,0,0,0,0,0,0,0)         DETAIL SCREEN                    
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT60,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT72,0,0,0,0,0,0,0)         DETAIL SCREEN                    
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT72,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@2ND-TWAD)                                                 
         DC    AL1(BT61,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@1ST-TWAD)                                                 
         DC    AL1(BT61,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT61,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT61,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN+CTRYNOT)                                    
         DC    AL2(LC@TAX-TWAD)                                                 
         DC    AL1(BT62,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RFRSH-TWAD)                                               
         DC    AL1(BT62,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@QUIT-TWAD)                                                
         DC    AL1(BT62,0,0,0,1,0,0,0)                                          
*                                                                               
*&&                                                                             
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(1,0,0,0,0,0,0,0)                                             
*                                                                               
PITEINPX DC    AL1(EOT)                                                         
*                                                                               
PITECHA  DC    AL1(RECITE,ACTCHA)                                               
         DC    AL2(PITECHAX+1-PITECHA)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&US                                                                           
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT01,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT03,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT06,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT21,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT22,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT26,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT33,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@CTAX-TWAD)                                                
         DC    AL1(BT46,0,0,0,0,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT01,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT03,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT06,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT21,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT22,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT26,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT33,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK15)                                                       
         DC    AL1(PFKIAPPL,CTRYCAN)                                            
         DC    AL2(LC@INV-TWAD)                                                 
         DC    AL1(BT46,0,0,0,2,0,0,0)      CANADIAN TAX SCREEN                 
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT60,0,0,0,0,0,0,0)         DETAIL SCREEN                    
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT72,0,0,0,0,0,0,0)         DETAIL SCREEN                    
*&&                                                                             
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PITECHAX DC    AL1(EOT)                                                         
*                                                                               
PITEDEL  DC    AL1(RECITE,ACTDEL)                                               
         DC    AL2(PITEDELX+1-PITEDEL)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PITEDELX DC    AL1(EOT)                                                         
*                                                                               
PBATCOP  DC    AL1(RECBAT,ACTCOP)                                               
         DC    AL2(PBATCOPX+1-PBATCOP)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKAUTOQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTAUT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATCOPX DC    AL1(EOT)                                                         
*                                                                               
PBATREV  DC    AL1(RECBAT,ACTREV)                                               
         DC    AL2(PBATREVX+1-PBATREV)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKAUTOQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTAUT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATREVX DC    AL1(EOT)                                                         
*                                                                               
PBATGEN  DC    AL1(RECBAT,ACTGEN)                                               
         DC    AL2(PBATGENX+1-PBATGEN)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKILISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECITE,ACTLST)                                               
         DC    AL1(0,CSIUSELC,0,0,0,RECBAT),AL2(LMITELST)                       
*                                                                               
         DC    AL1(PFKAUTOQ)                                                    
         DC    AL1(PFKIKAPA,0)                                                  
         DC    AL1(0,ACTAUT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATGENX DC    AL1(EOT)                                                         
*                                                                               
PBATRPT  DC    AL1(RECBAT,ACTRPT)                                               
         DC    AL2(PBATRPTX+1-PBATRPT)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCDISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTDSP)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCCHAQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTCHA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATRPTX DC    AL1(EOT)                                                         
*                                                                               
PITEDSP  DC    AL1(RECITE,ACTDSP)                                               
         DC    AL2(PITEDSPX+1-PITEDSP)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*&&UK                                                                           
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DEBSC-TWAD)                                               
         DC    AL1(BT70,0,0,0,0,0,0,0)                                          
*&&                                                                             
*&&US                                                                           
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT60,0,0,0,0,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DTL-TWAD)                                                 
         DC    AL1(BT72,0,0,0,0,0,0,0)                                          
*&&                                                                             
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PITEDSPX DC    AL1(EOT)                                                         
*                                                                               
PPSTLST  DC    AL1(RECPST,ACTLST)                                               
         DC    AL2(PPSTLSTX+1-PPSTLST)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIPAGE+PFKINPFX)                         
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIPAGE+PFKINPFX)                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKALTPQ)                                                    
         DC    AL1(PFKIALTP,0)                                                  
         DC    AL1(0,ACTAPF)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PPSTLSTX DC    AL1(EOT)                                                         
*                                                                               
PPSTDSP  DC    AL1(RECPST,ACTDSP)                                               
         DC    AL2(PPSTDSPX+1-PPSTDSP)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKPANAQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECPST,ACTANL)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,RECPST),AL2(LMPSTANL)                
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PPSTDSPX DC    AL1(EOT)                                                         
*                                                                               
PPSTANL  DC    AL1(RECPST,ACTANL)                                               
         DC    AL2(PPSTANLX+1-PPSTANL)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKPDISQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECPST,ACTDSP)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,RECPST),AL2(LMPSTDSP)                
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(BCPFXITS)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PPSTANLX DC    AL1(EOT)                                                         
*                                                                               
PCONDIS  DC    AL1(RECCON,ACTDSP)                                               
         DC    AL2(PCONDISX+1-PCONDIS)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCCHAQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTCHA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRPRTQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTRPT)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PCONDISX DC    AL1(EOT)                                                         
*                                                                               
PCONCHA  DC    AL1(RECCON,ACTCHA)                                               
         DC    AL2(PCONCHAX+1-PCONCHA)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCDISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTDSP)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRPRTQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTRPT)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PCONCHAX DC    AL1(EOT)                                                         
*                                                                               
PTYPMNT  DC    AL1(RECTYP,ACTMNT)                                               
         DC    AL2(PTYPMNTX+1-PTYPMNT)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKBLISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTLST)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKOPENQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECBAT,ACTOPN)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCDISQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTDSP)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKCCHAQ)                                                    
         DC    AL1(PFKIACTN,0)                                                  
         DC    AL1(RECCON,ACTCHA)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PTYPMNTX DC    AL1(EOT)                                                         
*                                                                               
PSCRSEL  DC    AL1(RECSCR,ACTSEL)                                               
         DC    AL2(PSCRSELX+1-PSCRSEL)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKDISPQ)                                                    
         DC    AL1(PFKIACTN,PFKISAVS)                                           
         DC    AL1(RECBAT,ACTDSP)                                               
         DC    AL1(PFKQUITQ,CSIUSELC,0,0,0,RECBAT),AL2(LMBATDSP)                
*                                                                               
         DC    AL1(PFKBKWDQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKINPFX)                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKFRWDQ)                                                    
         DC    AL1(PFKISCRL,PFKINPFX)                                           
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKLEFTQ)                                                    
         DC    AL1(PFKISCRL,PFKIUPDN+PFKIHORZ+PFKINPFX)                         
         DC    AL2(LC@LEFT-TWAD)                                                
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKRGHTQ)                                                    
         DC    AL1(PFKISCRL,PFKIHORZ+PFKINPFX)                                  
         DC    AL2(LC@RIGHT-TWAD)                                               
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKINPTQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTINP)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKALTPQ)                                                    
         DC    AL1(PFKIALTP,0)                                                  
         DC    AL1(0,ACTAPF)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PSCRSELX DC    AL1(EOT)                                                         
*                                                                               
PSITINP  DC    AL1(RECSIT,ACTINP)                                               
         DC    AL2(PSITINPX+1-PSITINP)                                          
         DC    AL1(0,0,0,0)                                                     
*&&UK                                                                           
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*&&                                                                             
         DC    AL1(PFK05)                  TYPE 60                              
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK05)                  TYPE 72                              
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK09)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ZOOM-TWAD)                                                
         DC    AL1(BT72,0,0,0,1,0,0,0)         ZOOM SCREEN                      
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT72,0,0,0,2,0,0,0)                                          
*                                                                               
PSITINPX DC    AL1(EOT)                                                         
*                                                                               
PSITDSP  DC    AL1(RECSIT,ACTDSP)                                               
         DC    AL2(PSITDSPX+1-PSITDSP)                                          
         DC    AL1(0,0,0,0)                                                     
*&&UK                                                                           
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*&&                                                                             
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@MEMO-TWAD)                                                
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
PSITDSPX DC    AL1(EOT)                                                         
*                                                                               
PSITCHA  DC    AL1(RECSIT,ACTCHA)                                               
         DC    AL2(PSITCHAX+1-PSITCHA)                                          
         DC    AL1(0,0,0,0)                                                     
*&&UK                                                                           
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT70,0,0,0,1,0,0,0)                                          
*&&                                                                             
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT60,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK05)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@TOP-TWAD)                                                 
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK06)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@CBOTT-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK07)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@UP-TWAD)                                                  
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK08)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@DOWN-TWAD)                                                
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK10)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ADD-TWAD)                                                 
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK11)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@ERASE-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
         DC    AL1(PFK12)                                                       
         DC    AL1(PFKIAPPL,0)                                                  
         DC    AL2(LC@RETRN-TWAD)                                               
         DC    AL1(BT72,0,0,0,1,0,0,0)                                          
*                                                                               
PSITCHAX DC    AL1(EOT)                                                         
*                                                                               
PBATGLU  DC    AL1(RECBAT,ACTGLU)                                               
         DC    AL2(PBATGLUX+1-PBATGLU)                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL1(PFKNEXTQ)                                                    
         DC    AL1(PFKINEXT,0)                                                  
         DC    AL1(0,ACTNXT)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
         DC    AL1(PFKQUITQ)                                                    
         DC    AL1(PFKIQUIT,0)                                                  
         DC    AL1(0,ACTQUI)                                                    
         DC    AL1(0,0,0,0,0,0,0,0)                                             
*                                                                               
PBATGLUX DC    AL1(EOT)                                                         
*                                                                               
PFKTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SELECT FIELD ACTION TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
SELTAB   DS    0X                                                               
*                                                                               
SBATLST  DC    AL1(RECBAT,ACTLST)                                               
         DC    AL2(SBATLSTX+1-SBATLST)                                          
*                                                                               
         DC    AL2(UC@DSP-TWAD)                                                 
         DC    AL2(LC@DSP-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMBATDSP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTDSP,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CLOSE-TWAD)                                               
         DC    AL2(LC@CLOSE-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATCLO)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTCLO,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@UPD-TWAD)                                                 
         DC    AL2(LC@UPD-TWAD)                                                 
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATUPD)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTUPD,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@SAVE-TWAD)                                                
         DC    AL2(LC@SAVE-TWAD)                                                
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATSAV)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTSAV,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL2(LC@DEL-TWAD)                                                 
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATDEL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTDEL,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@APRV-TWAD)                                                
         DC    AL2(LC@APRV-TWAD)                                                
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATAPR)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTAPR,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@UAPRV-TWAD)                                               
         DC    AL2(LC@UAPRV-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATUAP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTUAP,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@RECAL-TWAD)                                               
         DC    AL2(LC@RECAL-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATRCL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTREC,1,0)                                           
         DC    XL2'00'                                                          
*&&UK                                                                           
         DC    AL2(UC@INP-TWAD)                                                 
         DC    AL2(LC@INP-TWAD)                                                 
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATRCL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTREC,1,0)                                           
         DC    XL2'00'                                                          
*&&                                                                             
         DC    AL2(UC@CHG-TWAD)                                                 
         DC    AL2(LC@CHG-TWAD)                                                 
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATCHA)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTCHA,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@LIST-TWAD)                                                
         DC    AL2(LC@LIST-TWAD)                                                
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMITELST)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECITE,ACTLST,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@COPY-TWAD)                                                
         DC    AL2(LC@COPY-TWAD)                                                
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMITECOP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTCOP,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@RVRS-TWAD)                                                
         DC    AL2(LC@RVRS-TWAD)                                                
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMITEREV)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTREV,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@GENER-TWAD)                                               
         DC    AL2(LC@GENER-TWAD)                                               
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMITEGEN)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTGEN,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@PRINT-TWAD)                                               
         DC    AL2(LC@PRINT-TWAD)                                               
         DC    AL1(SELTIEOP+SELTINTR)                                           
         DC    AL1(0)                                                           
         DC    AL2(LMBATPRT)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTPRT,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@GLU-TWAD)                                                 
         DC    AL2(LC@GLU-TWAD)                                                 
         DC    AL1(SELTINTR)                                                    
         DC    AL1(0)                                                           
         DC    AL2(LMBATGLU)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECBAT,ACTGLU,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SBATLSTX DC    AL1(EOT)                                                         
*                                                                               
SITELST  DC    AL1(RECITE,ACTLST)                                               
         DC    AL2(SITELSTX+1-SITELST)                                          
*                                                                               
         DC    AL2(UC@ANL1-TWAD)                                                
         DC    AL2(LC@ANL1-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTANL)                                                    
         DC    AL1(LSTBBCBG,LSTI1AUT)                                           
         DC    AL1(RECPST,ACTANL,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DSP-TWAD)                                                 
         DC    AL2(LC@DSP-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTDSP)                                                    
         DC    AL1(LSTBBCBG,LSTI1AUT)                                           
         DC    AL1(RECPST,ACTDSP,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CHG-TWAD)                                                 
         DC    AL2(LC@CHG-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMITECHA)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECITE,ACTCHA,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DSP-TWAD)                                                 
         DC    AL2(LC@DSP-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMITEDSP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECITE,ACTDSP,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DEL-TWAD)                                                 
         DC    AL2(LC@DEL-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMITEDEL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECITE,ACTDEL,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@LIST-TWAD)                                                
         DC    AL2(LC@LIST-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTLST)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECPST,ACTLST,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SITELSTX DC    AL1(EOT)                                                         
*                                                                               
SITESEL  DC    AL1(RECITE,ACTSEL)                                               
         DC    AL2(SITESELX+1-SITESEL)                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(LMISESEL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(0,0,2,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@NO-TWAD)                                                  
         DC    AL2(LC@NO-TWAD)                                                  
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(LMISEDES)                                                    
         DC    XL2'00'                                                          
         DC    AL1(0,0,3,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@CHG-TWAD)                                                 
         DC    AL2(LC@CHG-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(LMISECHA)                                                    
         DC    XL2'00'                                                          
         DC    AL1(0,0,4,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DSP-TWAD)                                                 
         DC    AL2(LC@DSP-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMITEDSP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECITE,ACTDSP,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@LIST-TWAD)                                                
         DC    AL2(LC@LIST-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTLST)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECPST,ACTLST,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SITESELX DC    AL1(EOT)                                                         
*                                                                               
SPSTLST  DC    AL1(RECPST,ACTLST)                                               
         DC    AL2(SPSTLSTX+1-SPSTLST)                                          
*                                                                               
         DC    AL2(UC@ANL1-TWAD)                                                
         DC    AL2(LC@ANL1-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTANL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECPST,ACTANL,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DSP-TWAD)                                                 
         DC    AL2(LC@DSP-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTDSP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECPST,ACTDSP,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
SPSTLSTX DC    AL1(EOT)                                                         
*                                                                               
SSCRSEL  DC    AL1(RECSCR,ACTSEL)                                               
         DC    AL2(SSCRSELX+1-SSCRSEL)                                          
*                                                                               
         DC    AL2(UC@YES-TWAD)                                                 
         DC    AL2(LC@YES-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(LMISESEL)                                                    
         DC    XL2'00'                                                          
         DC    AL1(0,0,2,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@NO-TWAD)                                                  
         DC    AL2(LC@NO-TWAD)                                                  
         DC    AL1(SELTIEOL+SELTIEOP)                                           
         DC    AL1(0)                                                           
         DC    AL2(LMISEDES)                                                    
         DC    XL2'00'                                                          
         DC    AL1(0,0,3,0)                                                     
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@DSP-TWAD)                                                 
         DC    AL2(LC@DSP-TWAD)                                                 
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMITEDSP)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECITE,ACTDSP,1,BCPFXITS)                                    
         DC    XL2'00'                                                          
*                                                                               
         DC    AL2(UC@LIST-TWAD)                                                
         DC    AL2(LC@LIST-TWAD)                                                
         DC    AL1(SELTIEOL+SELTIEOP+SELTINTR)                                  
         DC    AL1(0)                                                           
         DC    AL2(LMPSTLST)                                                    
         DC    XL2'00'                                                          
         DC    AL1(RECPST,ACTLST,1,0)                                           
         DC    XL2'00'                                                          
*                                                                               
SSCRSELX DC    AL1(EOT)                                                         
*                                                                               
SELTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* BATCH TYPE TABLE                                                    *         
***********************************************************************         
         SPACE 1                                                                
TYPTAB   DS    0X                                                               
*&&US                                                                           
         DC    AL1(BT01,BTO01,BTS01),AL1(TBAGPRDQ)                              
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF)                                                     
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIT200)                                                    
         DC    AL1(TYPIHRTX+TYPINODC)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT001,15                                                      
*                                                                               
         DC    AL1(BT01,BTO01,BTS01C),AL1(TBAGPRDQ)                             
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF)                                                     
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIT200)                                                    
         DC    AL1(TYPIHRTX+TYPINODC)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT001,15                                                      
*                                                                               
         DC    AL1(BT03,BTO03U,BTS03),AL1(TBAGPRDQ)                             
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIT200)                                                    
         DC    AL1(TYPIHRTX+TYPINODC)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT003,15                                                      
*                                                                               
         DC    AL1(BT03,BTO03U,BTS03C),AL1(TBAGPRDQ)                            
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIT200)                                                    
         DC    AL1(TYPIHRTX+TYPINODC)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT003,15                                                      
*                                                                               
         DC    AL1(BT05,BTO05,BTS05),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT005,15                                                      
*                                                                               
         DC    AL1(BT06,BTO06,BTS06C),AL1(TBAGPRDQ)                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIANAR+TYPIMANB+TYPIT250)                                  
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT006,15                                                      
*                                                                               
         DC    AL1(BT06,BTO06,BTS06),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIANAR+TYPIMANB+TYPIT250)                                  
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT006,15                                                      
*                                                                               
         DC    AL1(BT07,0,0),AL1(TBAGPRDQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT007,15                                                      
*                                                                               
         DC    AL1(BT08,BTO08,BTS08),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT008,15                                                      
*                                                                               
         DC    AL1(BT10,BTO10,BTS10),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF+TYPIDDS)               DDS ONLY                      
         DC    AL1(TYPIMSCR+TYPIT250)                                           
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIFCUR+TYPIOVSS+TYPINODC)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIXOVL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(BPFKDTL+BPFKTAX)                                             
         DC    AL2(0)                                                           
         DCDDL AC#TT010,15                                                      
*                                                                               
         DC    AL1(BT10,BTO10,BTS10D),AL1(TBAGGENQ)  DETAIL                     
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF+TYPIDDS)                 DDS ONLY                    
         DC    AL1(TYPIMSCR+TYPIT250)                                           
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIFCUR+TYPIOVSS+TYPINODC)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIXOVL+TYPIDETL)                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(BPFKMUL+BPFKTAX)                                             
         DC    AL2(0)                                                           
         DCDDL AC#TT010,15                                                      
*                                                                               
         DC    AL1(BT10,BTO10,BTS10C),AL1(TBAGGENQ)                             
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF+TYPIDDS)               DDS ONLY                      
         DC    AL1(TYPIMSCR+TYPIT250)                                           
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIFCUR+TYPIOVSS+TYPINODC)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIXOVL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT010,15                                                      
*                                                                               
         DC    AL1(BT10,BTO10,BTS10CD),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF+TYPIDDS)               DDS ONLY                      
         DC    AL1(TYPIMSCR+TYPIT250)                                           
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIFCUR+TYPIOVSS+TYPINODC)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIXOVL+TYPIDETL)                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT010,15                                                      
*                                                                               
         DC    AL1(BT14,BTO14,BTS14),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT014,15                                                      
*                                                                               
         DC    AL1(BT15,BTO15,BTS15),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT015,15                                                      
*                                                                               
         DC    AL1(BT19,BTO19,BTS19),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD+TYPIDDS)                                             
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT019,15                                                      
*                                                                               
         DC    AL1(BT20,BTO20,BTS20),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT020,15                                                      
*                                                                               
         DC    AL1(BT21,BTO21,BTS21),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT021,15                                                      
*                                                                               
         DC    AL1(BT21,BTO21,BTS21C),AL1(TBAGGENQ)                             
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT021,15                                                      
*                                                                               
         DC    AL1(BT22,BTO22,BTS22),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT022,15                                                      
*                                                                               
         DC    AL1(BT22,BTO22,BTS22C),AL1(TBAGGENQ)                             
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT022,15                                                      
*                                                                               
         DC    AL1(BT25,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(TYPICUMU)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIDISP)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT025,15                                                      
*                                                                               
         DC    AL1(BT26,BTO26,BTS26),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIMANB+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT026,15                                                      
*                                                                               
         DC    AL1(BT26,BTO26,BTS26C),AL1(TBAGGENQ)                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIMANB+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT026,15                                                      
*                                                                               
*   TYPE 27 REMOVED 3/10/98                                                     
*        DC    AL1(BT27,BTO27,BTS27),AL1(TBAGGENQ)                              
*        DC    AL1(0)                                                           
*        DC    AL1(TYPIMLT)                                                     
*        DC    AL1(TYPIMSCR+TYPIT250)                                           
*        DC    AL1(TYPIT250)                                                    
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(CTRYALL)                                                     
*        DCDDL AC#TT027,15                                                      
*                                                                               
         DC    AL1(BT30,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR+TYPIT250)                                           
         DC    AL1(TYPIBGIC+TYPIBACO)                                           
         DC    AL1(TYPIASIR)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT030,15                                                      
*                                                                               
         DC    AL1(BT33,BTO33,BTS33),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT033,15                                                      
*                                                                               
         DC    AL1(BT33,BTO33,BTS33C),AL1(TBAGGENQ)                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT033,15                                                      
*                                                                               
         DC    AL1(BT34,BTO34,BTS34),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIOPF)                                                     
         DC    AL1(TYPIMSCR+TYPIMITE+TYPIT250)                                  
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV+TYPIT250)                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT034,15                                                      
*                                                                               
         DC    AL1(BT36,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT036,15                                                      
*                                                                               
         DC    AL1(BT37,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT037,15                                                      
*                                                                               
         DC    AL1(BT41,BTO41,BTS41),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPITIME)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT041,15                                                      
*                                                                               
         DC    AL1(BT45,BTO45,BTS45),AL1(TBAGGENQ)                              
         DC    AL1(TYPICUMU)                                                    
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(0,0)                                                         
         DCDDL AC#TT045,15                                                      
*                                                                               
         DC    AL1(BT46,BTO46,BTS46),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT046,15                                                      
*                                                                               
         DC    AL1(BT46,BTO46,BTS46C),AL1(TBAGPRDQ)                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPINODC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT046,15                                                      
*                                                                               
         DC    AL1(BT47,BTO47,BTS47),AL1(TBAGPRDQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPINOD)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIDORU)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT047,15                                                      
*                                                                               
         DC    AL1(BT48,0,0),AL1(TBAGPRDQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT048,15                                                      
*                                                                               
         DC    AL1(BT49,BTO49U,BTS49),AL1(TBAGGENQ)                             
         DC    AL1(0)                                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMSCR+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIHRTX)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIHOUR)                                                    
         DC    AL1(TYPITIME)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT049,15                                                      
*                                                                               
         DC    AL1(BT51,BTO51,BTS51),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT051,15                                                      
*                                                                               
         DC    AL1(BT53,BTO53,BTS53),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL+TYPICUMU)                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(BT54)                                                        
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT053,15                                                      
*                                                                               
         DC    AL1(BT54,BTO53,BTS53),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRV+TYPICUMU)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT054,15                                                      
*                                                                               
         DC    AL1(BT55,BTO55,BTS55),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT100)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(BT56)                                                        
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT055,15                                                      
*                                                                               
         DC    AL1(BT55,BTO55,BTS55C),AL1(TBAGGENQ)                             
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIT100)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(BT56)                                                        
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT055,15                                                      
*                                                                               
         DC    AL1(BT56,BTO55,BTS55),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRV)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT056,15                                                      
*                                                                               
         DC    AL1(BT56,BTO55,BTS55C),AL1(TBAGGENQ)                             
         DC    AL1(TYPIACRV)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT056,15                                                      
*                                                                               
         DC    AL1(BT57,0,0),AL1(TBAGPRDQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT057,15                                                      
*                                                                               
         DC    AL1(BT58,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT058,15                                                      
*                                                                               
         DC    AL1(BT60,BTO60,BTS60),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF)                                                     
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR+TYPIOVSS)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIDCSA)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT060,15                                                      
*                                                                               
         DC    AL1(BT61,BTO61,BTS61),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMITE+TYPIMSCR+TYPIT250+TYPIMSIC)                         
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT061,15                                                      
*                                                                               
         DC    AL1(BT62,BTO62,BTS62),AL1(TBAGPRDQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMSCR+TYPIT250)                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIHRTX)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIHOUR)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT062,15                                                      
*                                                                               
         DC    AL1(BT72,BTO72,BTS72),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF+TYPIDDS)              DDS ONLY                       
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR+TYPIOVSS+TYPINODC)                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIDCSA)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT072,15                                                      
*                                                                               
         DC    AL1(BT72,BTO72,BTS72C),AL1(TBAGGENQ)                             
         DC    AL1(TYPIORDS+TYPIOMEM)                                           
         DC    AL1(TYPIOPF+TYPIDDS)               DDS ONLY                      
         DC    AL1(TYPIMSCR+TYPIMSIC+TYPIT250)                                  
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR+TYPIOVSS+TYPINODC)                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIDCSA)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYCAN)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT072,15                                                      
*                                                                               
         DC    AL1(BT97,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD)                                                     
         DC    AL1(0)                                                           
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT097,15                                                      
*                                                                               
         DC    AL1(BT98,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT098,15                                                      
*                                                                               
         DC    AL1(BT99,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT099,15                                                      
*&&                                                                             
*&&UK                                                                           
         DC    AL1(BT01,BTO01,BTS01),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT001,15                                                      
*                                                                               
         DC    AL1(BT01,BTO01G,BTS01G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT001,15                                                      
*                                                                               
         DC    AL1(BT02,BTO02,BTS02),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT002,15                                                      
*                                                                               
         DC    AL1(BT03,BTO03,BTS03),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT003,15                                                      
*                                                                               
         DC    AL1(BT03,BTO03G,BTS03G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT004,15                                                      
*                                                                               
         DC    AL1(BT05,BTO05,BTS05),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT005,15                                                      
*                                                                               
         DC    AL1(BT06,BTO06,BTS06),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIANAR+TYPIMANB+TYPIT250)                                  
         DC    AL1(0)                                                           
         DC    AL1(TYPIFCUR)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT006,15                                                      
*                                                                               
         DC    AL1(BT07,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR+TYPIT250)                                           
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT007,15                                                      
*                                                                               
         DC    AL1(BT08,BTO08,BTS08),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT008,15                                                      
*                                                                               
         DC    AL1(BT08,BTO08,BTS08G),AL1(TBAGGENQ)                             
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT008,15                                                      
*                                                                               
         DC    AL1(BT11,BTO11,BTS11),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT011,15                                                      
*                                                                               
         DC    AL1(BT14,BTO14,BTS14),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT014,15                                                      
*                                                                               
         DC    AL1(BT17,BTO17,BTS17),AL1(TBAGGENQ)                              
         DC    AL1(TYPIXLOK)                                                    
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIPAUT+TYPIT150)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPITIME)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT017,15                                                      
*                                                                               
         DC    AL1(BT18,BTO18,BTS18),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT018,15                                                      
*                                                                               
         DC    AL1(BT18,BTO18,BTS18G),AL1(TBAGGENQ)                             
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGBR)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT018,15                                                      
*                                                                               
         DC    AL1(BT19,BTO19,BTS19),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT019,15                                                      
*                                                                               
         DC    AL1(BT19,BTO19,BTS19),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGBR)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT019,15                                                      
*                                                                               
         DC    AL1(BT21,BTO21,BTS21),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT021,15                                                      
*                                                                               
         DC    AL1(BT21,BTO21G,BTS21G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT021,15                                                      
*                                                                               
         DC    AL1(BT22,BTO22,BTS22),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT022,15                                                      
*                                                                               
         DC    AL1(BT22,BTO22G,BTS22G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT022,15                                                      
*                                                                               
         DC    AL1(BT26,BTO26,BTS26),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIMANB+TYPIT250)                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIFCUR)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT026,15                                                      
*                                                                               
         DC    AL1(BT26,BTO26G,BTS26G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIMANB+TYPIT250)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGBR)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT026,15                                                      
*                                                                               
*   TYPE 27 REMOVED 3/10/98                                                     
*        DC    AL1(BT27,BTO27,BTS27),AL1(TBAGGENQ)                              
*        DC    AL1(0)                                                           
*        DC    AL1(TYPIMLT+TYPIOSC)                                             
*        DC    AL1(TYPIMITE+TYPIMSCR+TYPIT250)                                  
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(TYPITIME)                                                    
*        DC    AL1(0)                                                           
*        DC    AL1(CTRYGER)                                                     
*        DCDDL AC#TT027,15                                                      
*                                                                               
         DC    AL1(BT30,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR+TYPIT400)                                           
         DC    AL1(TYPIBGIC+TYPIBACO)                                           
         DC    AL1(TYPIFCUR+TYPIASIR)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT030,15                                                      
*                                                                               
         DC    AL1(BT33,BTO33,BTS33),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPITIME)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT033,15                                                      
*                                                                               
         DC    AL1(BT34,BTO34,BTS34),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMSCR+TYPIMITE+TYPIT250)                                  
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT034,15                                                      
*                                                                               
         DC    AL1(BT36,BTO36,BTS36),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYNOT+CTRYGER)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT036,15                                                      
*                                                                               
         DC    AL1(BT36,BTO36G,BTS36G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIADVP)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT036,15                                                      
*                                                                               
         DC    AL1(BT37,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT037,15                                                      
*                                                                               
         DC    AL1(BT42,BTO42,BTS42),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT042,15                                                      
*                                                                               
         DC    AL1(BT42,BTO42,BTS42),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT042,15                                                      
*                                                                               
         DC    AL1(BT43,BTO43,BTS43),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(BT44)                                                        
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT043,15                                                      
*                                                                               
         DC    AL1(BT43,BTO43,BTS43),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(BT44)                                                        
         DC    AL1(CTRYNOT+CTRYGBR)                                             
         DC    AL2(0,0)                                                         
         DCDDL AC#TT043,15                                                      
*                                                                               
         DC    AL1(BT44,BTO43,BTS43),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRV)                                                    
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT044,15                                                      
*                                                                               
         DC    AL1(BT45,BTO45,BTS45),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL+TYPICUMU)                                           
         DC    AL1(TYPIMLT+TYPIOSC)                                             
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIPAUT+TYPIT100)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT045,15                                                      
*                                                                               
         DC    AL1(BT46,BTO46,BTS46),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMSCR+TYPIMITE+TYPIT250)                                  
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT046,15                                                      
*                                                                               
         DC    AL1(BT49,BTO49,BTS49),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(TYPIOSC+TYPIMLT)                                             
         DC    AL1(TYPIMITE+TYPIMSCR+TYPIT250)                                  
         DC    AL1(TYPIPAUT+TYPIT150)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPITIME)                                                    
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT049,15                                                      
*                                                                               
         DC    AL1(BT52,BTO52,BTS52),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT052,15                                                      
*                                                                               
         DC    AL1(BT52,BTO52,BTS52),AL1(TBAGGENQ)                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(TYPIT250)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT052,15                                                      
*                                                                               
         DC    AL1(BT57,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT057,15                                                      
*                                                                               
         DC    AL1(BT58,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT058,15                                                      
*                                                                               
         DC    AL1(BT59,BTO59,BTS59),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL+TYPICUMU)                                           
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIBGIC+TYPIPAUT)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT059,15                                                      
*                                                                               
         DC    AL1(BT70,BTO70,BTS70),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT070,15                                                      
*                                                                               
         DC    AL1(BT70,BTO70G,BTS70G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIOVTY)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT070,15                                                      
*                                                                               
         DC    AL1(BT70,BTO70,BTS70),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIBACO)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYHOL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT070,15                                                      
*                                                                               
         DC    AL1(BT71,BTO71,BTS71),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT071,15                                                      
*                                                                               
         DC    AL1(BT71,BTO71G,BTS71G),AL1(TBAGGENQ)                            
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(TYPIOVTY)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT071,15                                                      
*                                                                               
         DC    AL1(BT71,BTO71,BTS71),AL1(TBAGGENQ)                              
         DC    AL1(TYPIORDS)                                                    
         DC    AL1(TYPIOSC)                                                     
         DC    AL1(TYPIT250)                                                    
         DC    AL1(TYPIPAUT)                                                    
         DC    AL1(TYPIOVTY+TYPIFCUR)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYHOL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT071,15                                                      
*                                                                               
         DC    AL1(BT75,BTO75,BTS75),AL1(TBAGGENQ)                              
         DC    AL1(TYPIACRL+TYPICUMU)                                           
         DC    AL1(TYPIMLT)                                                     
         DC    AL1(TYPIMITE+TYPIT250)                                           
         DC    AL1(TYPIPAUT+TYPIT100)                                           
         DC    AL1(TYPIFCUR)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT075,15                                                      
*                                                                               
         DC    AL1(BT79,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR+TYPIT250)                                           
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT079,15                                                      
*                                                                               
         DC    AL1(BT97,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPINOD)                                                     
         DC    AL1(0)                                                           
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT097,15                                                      
*                                                                               
         DC    AL1(BT98,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT098,15                                                      
*                                                                               
         DC    AL1(BT99,0,0),AL1(TBAGGENQ)                                      
         DC    AL1(0)                                                           
         DC    AL1(TYPICBG)                                                     
         DC    AL1(TYPIANAR)                                                    
         DC    AL1(TYPIBGIC+TYPIBACO+TYPIBARV)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
         DC    AL2(0,0)                                                         
         DCDDL AC#TT099,15                                                      
*&&                                                                             
TYPTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* DMDTFIS                                                                       
*********PRINT OFF                                                              
*********INCLUDE DMDTFIS                                                        
*********PRINT ON                                                               
         EJECT                                                                  
* ACBATWORK                                                                     
       ++INCLUDE ACBATWORK                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACBAT00B  12/27/12'                                      
         END                                                                    
