*          DATA SET ACMRK00    AT LEVEL 101 AS OF 06/12/18                      
*PHASE T61600B                                                                  
*INCLUDE CONVMOS                                                                
*INCLUDE BMONVAL                                                                
*INCLUDE PUBVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE ACSRCHC                                                                
*INCLUDE SRCHCALL                                                               
*&&US                                                                           
*INCLUDE PRORATA                                                                
*&&                                                                             
MARKER   TITLE '- TRANSACTION MARKING'                                          
***********************************************************************         
*                                                                     *         
* NMAL 101 12JUN18  INCLUDED ACDCPAYJD IN ACMRKWRK                    *         
* NMAL 100 17OCT17  PAY2JOB PAYMENT SEEDING TO SJ            SPEC-9021*         
***********************************************************************         
* JFOX 049 COLUMNS FOR DIFFERENCE/ADVANCE AMOUNTS. SET A(FIRST TRPEL)           
* JFOX 050 WIDEN AND USE SOFT L'RPTAMNT & L'RPTAFCA IN REPTRN CUREDS            
* JFOX 051 DON'T SET SAVED VALUES IN ACCELS ROUTINE IF GETINOSV ON              
* JFOX 052 SUPER SAVED AREA KEPT AND RESTORED WHEN SAVED CLEARED                
* JFOX 053 ADD EARLIEST PAYMENT DATE TO ZOOMFAC PROCESSING AND OPTIONS          
* JFOX 054 NOP SOME CODE                                                        
* JFOX 055 ENABLE EARLIEST PAYMENT DATE FILTER                                  
* JFOX 056 MOVE TWO NEW U/C DICTIONARY EXPRESSIONS                              
* JFOX 057 CHANGE CHARACTER FOR DIFFERENCE COLUMN                               
* ECLI 058 ADD A NEW L/C DICTIONARY EXPRESSION                                  
* JFOX 059 FIX UNDERLINE BUG WHEN DRAFTING REPORT                               
* JNEW 060 REMOVE DTF REFERENCES.  JFOX - RENAME DSECT                          
* JNEW 061 SET LEDGTST2 FROM LDGSTAT2                                           
* JFOX 062 CHANGES FOR U.S. COMPATIBILITY                                       
* JFOX 063 PROTECTION COMPLIANCE                                                
* JFOX 064 U.K./U.S. COMPATIBILITY                                              
* ECLI 065 ALLOW GENERAL CONTRA IN GERMANY                                      
* JNEW 066 FIX TABLE ENTRY FOR SUB-REFERENCE COLUMN                             
* JFOX 067 SET ASORPROD FOR ACCOUNTING PRODUCTION SOREL                         
* JFOX 068 SET ASPACOSG (COSTING ACCOUNT/GROUP).  EXTEND AELEMS                 
* ECLI 069 ALLOW GENERAL CONTRA IN UK                                           
* JNEW 070 EXTRACT COMPANY STATUS BYTE 9                                        
* JFOX 071 OPTIONAL ACCOUNT CLOSED TEST IN GETACC                               
* JFOX 072 BUILD SPAEL IF TAX ACCOUNT APEEL FOUND                               
* JFOX 073 SET V(EUREKA).  EXTRACT CPYCURRS/CPYSCMOA                            
* JFOX 074 SECONDARY CURRENCY SUPPORT                                           
* JFOX 075 ZOOMFAC - INFO MESSAGE ON 'SWITCH', NOT ERROR                        
* JFOX 076 FURTHER SECONDARY CURRENCY SUPPORT                                   
* JFOX 077 MOVE "ROUV" ROUTINES INTO ACMRK42                                    
* JFOX 078 MOVE "ROUT" ROUTINES INTO ACMRK40 & ACMRK41                          
* JFOX 079 GET BIG TEMPEST PAGES.  GET V(TOBACCO) FROM COMFACS                  
* JFOX 080 SET MAXIMUM NUMBER OF BIG TEMPEST PAGES                              
* JFOX 081 FIX DICTIONARY CALLS FOR OP3CTD/OP3CTR & OP8CTD/OP8CTR               
* JFOX 082 OPTIONS - USED (CONTRA'D) DATE FILTER                                
* DCUR 083 MADE THE UK DEFAULT CONTROL DISPLAY PROFILE FOR BANK/REC THE         
*          DEFAULT FOR US AND MADE THE UK BANK/REC TOTAL TABLE                  
*          (BRTOTTAB) THE TABLE FOR US ALSO.                                    
* JFOX 084 US/UK COMPATIBILITY                                                  
* JFOX 085 SUPPORT DDS-ONLY OPTION (REV FOR WIP/HOLD IN U.S.)                   
* AWIL 093 SUPPORT FOR GRIDS                                                    
MARKER   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL (WORKX-WORKD),**MRK0**,RA,R9,RR=RE,CLEAR=YES                     
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
                                                                                
         L     RF,8(R1)            RF=A(ACCFACS)                                
         USING ACCFACSD,RF                                                      
         MVC   VACCEMU,AACCEMU                                                  
                                                                                
         L     RF,20(R1)           RF=A(EXTRA INFO BLOCK)                       
         MVC   AGYOPTS,0(R1)                                                    
         MVC   AGYCTRY,1(RF)                                                    
         MVC   AGYLANG,3(RF)                                                    
         MVC   AGYCURR,4(RF)                                                    
         CLI   AGYCTRY,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   AGYCTRY,CTRYGBR                                                  
*&&US*&& MVI   AGYCTRY,CTRYUSA                                                  
         CLI   AGYCTRY,CTRYSCA                                                  
         BNE   *+8                                                              
         MVI   AGYCTRY,CTRYGBR                                                  
         CLI   AGYLANG,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   AGYLANG,LANGEUK                                                  
*&&US*&& MVI   AGYLANG,LANGEUS                                                  
         MVC   N31BMB,23(RF)       SAVE # MEGABYTES OF 31 BIT W/S               
         MVC   A31BWS,24(RF)       SAVE A(31 BIT W/S FOR FACPAK TASK)           
                                                                                
                                                                                
         L     R1,16(R1)                                                        
         ST    R1,ACOM             EXTRACT A(COMFACS) FROM FAPARMS              
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VBLDCUR,CBLDCUR                                                  
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VCUREDIT,CCUREDIT                                                
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETCUR,CGETCUR                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETPROF,CGETPROF                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VDLFLD,CDLFLD                                                    
         MVC   VXTRAINF,CXTRAINF                                                
*&&UK*&& MVC   VPRORATA,CPRORATA                                                
*&&US                                                                           
         L     RF,=V(PRORATA)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRORATA                                                      
*&&                                                                             
         MVC   VREPORT,CREPORT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VSECRET,CSECRET                                                  
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VEUREKA,CEUREKA                                                  
*&&UK*&& MVC   VTOBACCO,CTOBACCO                                                
         DROP  R1                                                               
                                                                                
         GOTO1 VSWITCH,DMCB,X'FFFFFFFF'                                         
         MVC   AUTL,0(R1)          SAVE A(UTL ENTRY)                            
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'40',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            SAVE A(ROUTINES 1 ENTRY POINT)               
         LA    R1,ROU40            SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,ROU40N                                                        
         XR    RE,RE                                                            
INIT10   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT10                                                        
                                                                                
         GOTO1 VCALLOV,DMCB,(X'41',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            SAVE A(ROUTINES 2 ENTRY POINT)               
         LA    R1,ROU41            SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,ROU41N                                                        
         XR    RE,RE                                                            
INIT12   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT12                                                        
                                                                                
         GOTO1 VCALLOV,DMCB,(X'42',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            SAVE A(ROUTINES 3 ENTRY POINT)               
         LA    R1,ROU42            SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,ROU42N                                                        
         XR    RE,RE                                                            
INIT14   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT14                                                        
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'43',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            SAVE A(ROUTINES 3 ENTRY POINT)               
         LA    R1,ROU43            SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,ROU43N                                                        
         XR    RE,RE                                                            
INIT15   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT15                                                        
*                                                                               
         LA    R0,TABLESN          SET A(ROOT TABLES IN W/S)                    
         LHI   R1,TABLES-MARKER                                                 
         LA    R1,MARKER(R1)                                                    
         LA    RE,ATABLES                                                       
INIT16   LH    RF,0(R1)                                                         
         LA    RF,MARKER(RF)                                                    
         ST    RF,0(RE)            SET TABLE ADDRESS                            
         LA    R1,2(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,INIT16                                                        
                                                                                
         LA    R1,WORKD                                                         
         AHI   R1,OFFBLK-WORKD                                                  
         ST    R1,AOFFBLK                                                       
         LA    R1,WORKD                                                         
         AHI   R1,TSARBLK-WORKD                                                 
         ST    R1,ATSARBLK                                                      
         LA    R1,WORKD                                                         
         AHI   R1,PQBUFF-WORKD                                                  
         ST    R1,APQBUFF                                                       
         LA    R1,WORKD                                                         
         AHI   R1,OVERWORK-WORKD                                                
         ST    R1,AOVERWRK                                                      
         LA    R1,WORKD                                                         
         AHI   R1,REPWORK-WORKD                                                 
         ST    R1,AREPWRK                                                       
         LA    R1,WORKD                                                         
         AHI   R1,IOAS-WORKD                                                    
         ST    R1,AIOAS                                                         
         LA    R1,SAVED                                                         
         AHI   R1,CURRTAB-SAVED                                                 
         ST    R1,ACURRTAB                                                      
         LA    R1,SAVED                                                         
         AHI   R1,LEDGTAB-SAVED                                                 
         ST    R1,ALEDGTAB                                                      
         LA    R1,SAVED                                                         
         AHI   R1,USEDTAB-SAVED                                                 
         ST    R1,AUSEDTAB                                                      
         LA    R1,SAVED                                                         
         AHI   R1,TSBINFO-SAVED                                                 
         ST    R1,ATSBINFO                                                      
         LA    R1,SAVED                                                         
         AHI   R1,SECBLK-SAVED                                                  
         ST    R1,ASECBLK                                                       
*                                                                               
         LA    R1,WORKD                                                         
         AHI   R1,DLCB-WORKD                                                    
         ST    R1,ADLCB                                                         
         LA    R1,WORKD                                                         
         AHI   R1,DUMLINE-WORKD                                                 
         ST    R1,ADUMLINE                                                      
                                                                                
         LA    R0,IOAMAX           R0=IO AREA COUNT                             
         L     R1,AIOAS            R1=A(1ST IOAREA IN GWS)                      
         LA    RF,AIOSAVE1         RF=A(1ST A(IOSAVE) IN OVRWORK)               
INIT18   ST    R1,0(RF)            SAVE A(IOSAVE)                               
         LA    RE,IOBUFF-IOAS(R1)  RE=A(IO BUFFER)                              
         ST    RE,4(RF)            SAVE A(IOBUFF)                               
         LA    RF,8(RF)            RF=NEXT A(IOSAVE)                            
         LA    R1,IOALQ(R1)        R1=NEXT IOAREA                               
         BCT   R0,INIT18                                                        
*                                                                               
*&&UK                                                                           
         LA    R1,SAVED                                                         
         AHI   R1,SCUTOTS-SAVED                                                 
         ST    R1,ASCUTOTS                                                      
*&&                                                                             
         L     R1,=V(CONVMOS)                                                   
         A     R1,RELO                                                          
         ST    R1,VCONVMOS                                                      
         L     R1,=V(BMONVAL)                                                   
         A     R1,RELO                                                          
         ST    R1,VBMONVAL                                                      
         L     R1,=V(PUBVAL)                                                    
         A     R1,RELO                                                          
         ST    R1,VPUBVAL                                                       
         L     R1,=V(GETBROAD)                                                  
         A     R1,RELO                                                          
         ST    R1,VGETBRD                                                       
         L     R1,=V(ACSRCHC)                                                   
         A     R1,RELO                                                          
         ST    R1,VACSRCHC                                                      
                                                                                
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
*                                                                               
         GOTO1 (RF),(R1),0,X'C1000A0D'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSQASHER,0(R1)        GET SQASHER ADDRESS - CORERES              
*                                                                               
         GOTO1 (RF),(R1),0,X'D9000A63'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VADDTRN,0(R1)       GET ADDTRN ADDRESS - CORERES                 
                                                                                
         GOTO1 (RF),(R1),0,X'D9000A84'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETOPT,0(R1)       GET GETOPT ADDRESS - CORERES                 
                                                                                
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVI   UNDERLN,C'-'                                                     
         MVC   UNDERLN+1(L'UNDERLN-1),UNDERLN                                   
         MVC   LARE,=X'41E0'                                                    
         MVC   LARF,=X'41F0'                                                    
                                                                                
         TM    TWAMODE,TWAMINIT    FIRST TIME?                                  
         BNZ   INIT40                                                           
         MVC   TEMP(SSAVEL),SSAVE  KEEP SUPER SAVED AREA                        
         MVC   LACCOUNT,ACCOUNT    KEEP LAST ACCOUNT                            
         LA    R0,SAVEAREA         CLEAR SAVED VALUES                           
         LHI   R1,SAVEDLEN                                                      
         CHI   R1,SAVEAREL                                                      
         BNH   *+6                                                              
         DC    H'0'                ENSURE WE HAVEN'T OVERSTEPPED                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
**NOP    LA    R0,TWASAVAR         CLEAR SAVED VALUES                           
*        LHI   R1,TWASAVAL                                                      
*        XR    RE,RE                                                            
*        XR    RF,RF                                                            
*        MVCL  R0,RE                                                            
*                                                                               
         MVC   SSAVE(SSAVEL),TEMP  RESTORE SUPER SAVED AREA                     
         NI    TSA2MODE,FF-TSA2MINI    SET TO RE-INITIALISE TSAR 2              
         MVI   SSCRCOLL,1              RESET VERTICAL SCROLL                    
         MVI   SSCRCOLL+1,127          RESET VERTICAL SCROLL                    
                                                                                
         GOTO1 VDICTATE,DMCB,C'LU  ',ADCLISTU,DSLISTU                           
         GOTO1 (RF),(R1),C'LL  ',ADCLISTL,DSLISTL                               
                                                                                
*        MVI   FILEFORM,VLISQ      ESTABLISH ACCOUNT FILE FORMAT                
*        GOTO1 VDATAMGR,DMCB,=C'DTFAD',=C'ACCFIL'                               
*        L     R1,12(R1)                                                        
*        TM    DTFTYPE-DTFPHD(R1),DTFTEMU                                       
*        BZ    *+8                                                              
         MVI   FILEFORM,ISDAQ                                                   
                                                                                
         USING SECD,R2                                                          
         L     R2,ASECBLK                                                       
         TM    SECINDS,SECIINIT    TEST SECRET BLOCK INITIALISED                
         BNZ   INIT22                                                           
         LA    RF,L'SECBLK                                                      
         GOTO1 VSECRET,DMCB,('SECPINIT',SECD),(RF)                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
INIT22   LA    R2,KEY              R2=A(KEY)                                    
         USING CPYRECD,R2          READ COMPANY RECORD                          
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,COMPANY                                                  
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    *+6                                                              
         DC    H'0'                NO COMPANY DIRECTORY RECORD                  
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                NO COMPANY DATA RECORD                       
         GOTO1 AACCELS                                                          
         ICM   R1,15,RECCPYEL                                                   
         BNZ   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
                                                                                
         USING CPYELD,R1                                                        
         MVC   COMPALFA,CPYALPHA   EXTRACT DATA FROM COMPANY ELEMENT            
         MVC   COMPSTAT,CPYSTAT1                                                
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         MVC   COCDPASS,CPYCDC                                                  
         MVC   VATRATES,CPYVATR                                                 
         MVC   TENO,CPYTENO                                                     
         MVC   ARTFUL,=C'ST'                                                    
         MVC   BANKUL,CPYBANK                                                   
         MVC   EXPSUL,=C'SE'                                                    
         MVC   FOLIUL,=C'SF'                                                    
         MVC   PRODUL,CPYPROD                                                   
         MVC   RECVUL,CPYRECV                                                   
         MVC   SUPPUL,CPYSUPP                                                   
         MVC   SUPXUL,=C'SX'                                                    
         MVC   TAXAUL,=C'SG'                                                    
         XC    COMPGMOA,COMPGMOA                                                
         CLI   CPYLN,CPYLN2Q       TEST LONG ELEMENT                            
         BL    INIT24                                                           
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
*&&UK                                                                           
         PUSH  USING                                                            
         USING CURTABD,COMPCURT                                                 
         MVC   CURTCUR,CPYCURR                                                  
*&&                                                                             
         CLI   CPYLN,CPYLN3Q                                                    
         BL    INIT24                                                           
         MVC   COMPSTA9,CPYSTAT9                                                
         MVC   COMPSTAA,CPYSTATA                                                
         MVC   COMPGMOA,CPYGLMOA                                                
*                                                                               
         CLI   CPYLN,CPYSTATD-CPYELD                                            
         JL    *+10                                                             
         MVC   COMPSTAD,CPYSTATD                                                
*                                                                               
*&&UK                                                                           
         OC    CPYCURRS,CPYCURRS   TEST SECONDARY CURRENCY                      
         BZ    INIT24                                                           
S        USING CURTABD,SCNDCURT                                                 
         MVC   S.CURTCUR,CPYCURRS  SET CURRENCY CODE IN SCNDCURT                
         MVC   COMPSCMO,CPYSCMOA   SET SECOND CURRENCY MONTH                    
         GOTO1 VBLDCUR,DMCB,S.CURTCUR,S.CURTABD,ACOM                            
         DROP  S                                                                
*&&                                                                             
*                                                                               
INIT24   DS    0H                                                               
*&&UK                                                                           
         OC    CURTCUR,CURTCUR     TEST CURRENCY RESOLVED                       
         BNZ   INIT26                                                           
         LA    R1,CTRYTAB                                                       
         LA    R0,CTRYTABN                                                      
         CLC   AGYCTRY,0(R1)                                                    
         BE    *+14                                                             
         LA    R1,CTRYTABL(R1)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                THIS ACGCTRY IS MISSING FROM CTRYTAB         
         MVC   CURTCUR,1(R1)       SET DEFAULT CURRENCY                         
INIT26   GOTO1 VBLDCUR,DMCB,CURTCUR,CURTABD,ACOM                                
         POP   USING                                                            
*&&                                                                             
         USING LDGRECD,R2          READ PRODUCTION LEDGER                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(L'PRODUL),PRODUL                                         
         DROP  R2                                                               
         GOTO1 AGETLDG,0                                                        
         BE    INIT30              LEDGER IS VALID                              
         LA    R1,MRKTYPH                                                       
         ST    R1,FVADDR                                                        
         B     FVERR               INVALID LEDGER/SECURITY LOCKOUT              
INIT30   ICM   R1,15,RECALDGT                                                   
         MVC   PRODLEVS,LEDGTLVA-LEDGTABD(R1)                                   
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         GOTO1 (RF),(R1),(0,WORK),(1,TODAYP)                                    
         GOTO1 (RF),(R1),(0,WORK),(2,TODAYC)                                    
         GOTO1 (RF),(R1),(0,WORK),(3,TODAYB)                                    
                                                                                
         L     RF,ACOM             EXTRACT USER PASSWORD NUMBER                 
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         XC    STWAPAS#,STWAPAS#                                                
         TM    FATFLAG-FACTSD(RF),X'08'                                         
         BZ    *+10                                                             
         MVC   STWAPAS#,FAPASSWD-FACTSD(RF)                                     
         TM    FATSTAT6-FACTSD(RF),TST6STRO                                     
         BNO   *+8                                                              
         OI    PCDRIVEN,PCMODEQ    RUNNING UNDER STEREO                         
*                                                                               
INIT40   L     R1,ATSARBLK                                                      
         USING TSARD,R1            R1=A(TSAR BLOCK)                             
         MVC   TSABUF,ATSBINFO     SET A(TSAR BUFFER INFORMATION)               
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC           SET A(RECORD)                                
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,L'TSARKEY    SET KEY LENGTH                               
         MVC   TSRECL,=Y(TSARMAXL) ALLOW FOR VERY BIG RECORDS                   
         MVI   TSRECI,TSRVAR       SET VARIABLE LENGTH RECORDS                  
         MVI   TSINDS,TSIALLOC+TSIXTTWA  TEMPEST ALLOCATION/BIG PAGES           
         MVI   TSIND2,TSI2DATA     SET TO SAVE BUFFER DATA AT TSABUF            
         MVI   TSNBUF,2            SET NUMBER OF CORE BUFFERS                   
         MVI   TSPAGN,TSNMAX       SET MAX NUMBER OF BIG PAGES                  
         TM    TWAMODE,TWAMRSRV    TEST TEMPEST PREVIOUSLY RESERVED             
         BZ    INIT50                                                           
                                                                                
         MVC   TSINDS,TWATSARI     SET INDICATORS                               
         OI    TSINDS,TSIREUSE     SET TO RE-USE PREVIOUS ALLOCATION            
         MVC   TSPAGL,TWALOWPG     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWANUMPG     SET NUMBER OF PAGES ALLOCATED                
                                                                                
INIT50   MVI   TSACTN,TSAINI       SET INITIALISE                               
         TM    TWAMODE,TWAMINIT                                                 
         BZ    *+8                                                              
         MVI   TSACTN,TSARES       SET RESTORE                                  
         GOTO1 VTSAR               CALL TO INITIALISE/RESTORE                   
         BE    INIT60                                                           
         TM    TWAMODE,TWAMRSRV    TEST FIRST TIME ALLOCATION                   
         BZ    *+6                                                              
         DC    H'0'                KILL IF RESTORE (INITIALISED)                
         NI    TSINDS,255-TSIALLOC RESET TEMPEST ALLOCATION                     
         MVI   TSPAGL,2            SET TO USE TEMPSTR PAGES 2-4                 
         MVI   TSPAGN,3                                                         
         BASR  RE,RF               INITIALISE TEMPSTR PAGES                     
         BE    INIT60                                                           
         DC    H'0'                KILL IF CAN'T INITIALISE TEMPSTR             
                                                                                
INIT60   MVC   TWALOWPG,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   TWANUMPG,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         MVC   TWATSARI,TSINDS     SAVE TEMPSTR/TEMPEST INDICATOR               
         NI    TWATSARI,TSIALLOC+TSIXTTWA                                       
         OI    TWAMODE,TWAMINIT+TWAMRSRV                                        
         DROP  R1                                                               
                                                                                
         L     R1,AOFFBLK          INITIALISE OFFAL FOR OFFICE ACCESS           
         USING OFFALD,R1                                                        
         MVI   OFFACTRL,OFFACCNV   NEW STYLE RECORDS                            
         MVC   OFFACOMF,ACOM                                                    
         MVC   OFFATSAR,VTSAR                                                   
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTAT                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),TWAOFFSV  RESTORE SAVED VALUES                 
         BZ    *+8                 NO SAVED VALUES - FIRST TIME                 
         MVI   OFFAACT,OFFARES                                                  
         OI    OFFAINDS,OFFAISEC+OFFAIOFF                                       
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T INITIALISE OFFAL                
         MVC   TWAOFFSV,OFFASAV                                                 
         DROP  R1                                                               
                                                                                
         LH    R1,DISPHED          ESTABLISH INPUT SCREEN ADDRESSES             
         AR    R1,R6                                                            
         ST    R1,ADISHEAD                                                      
         LH    R1,DISPHED2                                                      
         AR    R1,R6                                                            
         ST    R1,ADISHEA2                                                      
         LH    R1,DISPDET                                                       
         AR    R1,R6                                                            
         ST    R1,ADISDET1                                                      
         LH    R1,DISPTOT                                                       
         AR    R1,R6                                                            
         ST    R1,ADISTOTS                                                      
         LH    R1,DISPPFK                                                       
         AR    R1,R6                                                            
         ST    R1,ADISPFKS                                                      
                                                                                
**NOP    OI    MRKSCRH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
*        L     R1,ACOM                                                          
*        L     R2,CXTRAINF-COMFACSD(R1)                                         
*        USING XTRAINFD,R2                                                      
*        TM    XIFLAG1,XIROSYS     CONNECTED TO READ ONLY SYS                   
*        BNO   INIT90                                                           
*        MVC   FVMSGNO,=AL2(AE$UPDNO)    "ADV NOT ALLOWING UPDS"                
*        B     FVERR                                                            
*NIT90   TM    XIFLAG1,XIROMODE    CONNECTED IN READ ONLY MODE                  
*        BNO   INIT92                                                           
*        MVC   FVMSGNO,=AL2(AE$NAUPD)    "NOT AUTHORIZED TO UPD"                
*        B     FVERR                                                            
*NIT92   TM    XIFLAG1,XIWRONGF    CONNECTED TO WRONG FACPAK                    
*        BNO   INIT99                                                           
*        MVC   FVMSGNO,=AL2(AE$HMADV)    "NOT CONNECTED TO HOME ADV"            
*        MVC   FVXTRA,XIUPDFAC     GET THE UPDATIVE ADV NAME                    
*        B     FVERR                                                            
*        DROP  R2                                                               
*NIT99   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE GRID                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALCOM   LA    R2,MRKTYPH          R2=A(ACTION TYPE SCREEN FIELD HEAD)          
         ST    R2,FVADDR                                                        
         USING FLDHDRD,R2                                                       
         TM    FLDIIND,FINPVAL                                                  
         BNZ   VALCOMX                                                          
         TM    PCDRIVEN,PCMODEQ    TEST RUNNING UNDER STEREO?                   
         BZ    VALCOMX                                                          
         CLC   =C'GRID',MRKTYP     TEST CONNECT TO GRID?                        
**NOP    BE    VALCOM02                                                         
*        CLC   =C'GR01',MRKTYP     TEST CONNECT TO GRID?                        
         BNE   *+12                                                             
         OI    PCDRIVEN,PCGDATQ                                                 
         B     VALCOM02                                                         
         CLC   =C'GOFF',MRKTYP     TEST DISCONNECT FROM GRID?                   
         BNE   VALCOMX                                                          
         NI    PCDRIVEN,X'FF'-(PCGDATQ+PCGRIDQ)                                 
                                                                                
VALCOM02 MVC   MRKTYP,SPACES                                                    
         MVI   SACTION,0           REBUILD SCREEN                               
         OI    FLDOIND,FOUTTRN+FOUTMOD                                          
         NI    FLDIIND,X'FF'-FINPVAL                                            
         MVC   MRKMSG,SPACES       CLEAR HEADER FIELD                           
         B     FVERRX                                                           
                                                                                
VALCOMX  DS    0H                                                               
***********************************************************************         
* VALIDATE TYPE                                                       *         
***********************************************************************         
VALTYP   TM    TWAMODE3,TWAM3ZOO   TEST ZOOM FACILITY REQUIRED                  
         BZ    VALTYP01                                                         
         GOTO1 AZOOMFAC                                                         
         BE    FVERR               STILL PROCESSING ZOOM SCREEN                 
                                                                                
VALTYP01 LA    R1,MRKTYPH          SET CURSOR TO TYPE FIELD                     
         ST    R1,FVADDR                                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,MRKTYPH                                                  
         BNE   FVERR                                                            
         L     R1,ATYPTAB                                                       
         USING TYPTABD,R1                                                       
         MVI   XTYPE,TYPINIT       CLEAR TYPE                                   
VALTYP02 XR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         CLI   FVILEN,TYPADSQ      DOES L'INPUT EXCEED L'SHORT NAME?            
         BH    VALTYP04            YES - TRY LONG NAME                          
         MVC   LAREADDR,TYPADDRS                                                
         EX    0,LARE                                                           
         CLC   0(TYPADSQ,RE),SPACES  DO WE HAVE A SHORT NAME?                   
         BE    VALTYP04            NO - TRY LONG NAME                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH ACTION SHORT NAME           
         BNE   VALTYP04                                                         
         MVC   LAREADDR,TYPADDRL   IT MATCHES - TAKE FULL NAME                  
         EX    0,LARE                                                           
         B     VALTYP06                                                         
VALTYP04 MVC   LAREADDR,TYPADDRL                                                
         EX    0,LARE                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH ACTION NAME                 
         BNE   VALTYP10                                                         
VALTYP06 CLI   XTYPE,TYPINIT       DO WE ALREADY HAVE A MATCH?                  
         BE    VALTYP08                                                         
         MVI   XTYPE,TYPINIT       YES - ERROR.  CLEAR TYPE                     
         MVC   MRKTYP,FVIFLD       RE-TRANSMIT USER'S TYPE                      
         B     ERRSHRT             USER MUST BE MORE SPECIFIC                   
VALTYP08 MVC   XTYPE,TYPE          TAKE TYPE NUMBER                             
         MVC   XTYPINDS,TYPINDS    TAKE TYPE INDICATORS                         
         XR    RF,RF                                                            
         ICM   RF,3,TYPAACT                                                     
         AH    RF,=Y(TABLES-MARKER)                                             
         LA    RF,MARKER(RF)                                                    
         ST    RF,AACTTAB          SAVE A(ACTION TABLE FOR THIS TYPE)           
         MVC   XTYPNAME,0(RE)      SAVE TYPE NAME                               
         MVC   MRKTYP,0(RE)        SET FULL TYPE NAME                           
         OI    MRKTYPH+(FVOIND-FVIHDR),FVOXMT                                   
         B     VALTYPX                                                          
VALTYP10 LA    R1,TYPTABL(R1)                                                   
         CLI   0(R1),EOT           FINISHED WITH TABLE?                         
         BNE   VALTYP02            NO - NEXT TYPE                               
VALTYPX  CLI   XTYPE,TYPINIT                                                    
         BE    ERRNOTV                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION                                                     *         
***********************************************************************         
                                                                                
VALACT   TM    TWAMODE,TWAMCFRM    TEST ACTION CONFIRMATION REQUIRED            
         BZ    VALACT04                                                         
         LH    R1,XACTYTOT         Y(ACTION TOTALS TABLE)                       
         LA    R1,MARKER(R1)                                                    
         ST    R1,AACTTOT          SET A(ACTION TOTALS TABLE)                   
                                                                                
         BAS   RE,CONFIRM          ** BEWARE - MAY NOT RETURN **                
         B     GOLAY               IF CONFIRMED GO DIRECTLY TO OVERLAY          
                                                                                
                                                                                
VALACT04 LA    R1,MRKACTH          SET CURSOR TO ACTION FIELD                   
         ST    R1,FVADDR                                                        
         TM    TWAMODE,TWAMHDRS    HEADER ACTIONS/INPUT ACTIONS                 
         BNZ   VALACT30            VALIDATE 'TEMPORARY' PFK ACTIONS             
         MVI   FVMINL,1            VALIDATE 'REAL' ACTION                       
         GOTO1 AFLDVAL,MRKACTH                                                  
         BNE   FVERR                                                            
         L     R1,AACTTAB                                                       
         USING ACTTABD,R1                                                       
         MVI   XACTION,ACTINIT     AND CLEAR ACTION                             
         CLI   0(R1),EOT           TEST ANY VALID ACTION                        
         BE    ERRNOTV             NO - ERROR                                   
VALACT06 XR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         CLI   FVILEN,ACTADSQ      DOES L'INPUT EXCEED L'SHORT NAME?            
         BH    VALACT08            YES - TRY LONG NAME                          
         MVC   LAREADDR,ACTADDRS                                                
         EX    0,LARE                                                           
         CLC   0(ACTADSQ,RE),SPACES                                             
         BE    VALACT08            NO SHORT NAME - TRY LONG NAME                
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH ACTION SHORT NAME           
         BNE   VALACT08                                                         
         MVC   LAREADDR,ACTADDRL   IT MATCHES - TAKE FULL NAME                  
         EX    0,LARE                                                           
         B     VALACT10                                                         
VALACT08 MVC   LAREADDR,ACTADDRL                                                
         EX    0,LARE                                                           
         EX    RF,*+8              TEST INPUT MATCHES ACTION NAME               
         BNE   VALACT20            NO - GET NEXT ACTION TABLE ENTRY             
         CLC   0(0,RE),FVIFLD                                                   
                                                                                
VALACT10 TM    ACTINDS,ACTIDDSQ    TEST DDS ONLY ACTION                         
         BNO   *+12                                                             
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BNE   VALACT20            NO - GET NEXT ACTION TABLE ENTRY             
         XR    RF,RF               TEST COUNTRY VALIDITY                        
         ICM   RF,3,ACTYCTR        TEST COUNTRY LIST                            
         BZ    VALACT14            NO - ALL COUNTRIES VALID                     
         LA    RF,MARKER(RF)       RF=A(COUNTRY LIST FOR TYPE/ACTION)           
VALACT12 CLI   0(RF),EOT           EOT - ACTION NOT VALID FOR COUNTRY           
         BE    VALACT20            GET NEXT ACTION TABLE ENTRY                  
         CLC   AGYCTRY,0(RF)       TEST AGENCY COUNTRY IN LIST                  
         BE    VALACT14            YES - OK TO CONTINUE                         
         LA    RF,L'AGYCTRY(RF)    NEXT COUNTRY LIST ENTRY                      
         B     VALACT12                                                         
                                                                                
VALACT14 OC    TWASAGN,TWASAGN     TEST USING OLD SECURITY                      
         BNZ   VALACT16                                                         
         MVC   BYTE,TWAAUTH        TEST OLD SECURITY AUTHORISATION              
         NC    BYTE,ACTSECY                                                     
         CLC   BYTE,ACTSECY        TEST AUTHORISED FOR TYPE/ACTION              
         BNE   VALACT20            NO - NEXT ACTION TABLE ENTRY                 
* - - - - - - -  - - - - - - - - - -                                            
VALACT15 TM    ACTIND2,ACTI2GRO    GRIDS ONLY ACTION                            
         BNO   VALACT16                                                         
         TM    PCDRIVEN,PCGDATQ    WANTS GRID DATA                              
         BZ    VALACT20                                                         
* - - - - - - -  - - - - - - - - - -                                            
VALACT16 CLI   ACTION,ACTAUTH      TEST AUTHORISING                             
         BNE   *+12                                                             
         TM    COMPSTA4,CPYSIREG   TEST COMPANY ON INVOICE REGISTER             
         BNO   VALACT20            NO - NEXT ACTION TABLE ENTRY                 
         CLI   XACTION,ACTINIT     DO WE ALREADY HAVE A MATCH?                  
         BE    VALACT18                                                         
         MVI   XACTION,ACTINIT     YES - ERROR.  CLEAR ACTION NUMBER            
         MVC   MRKACT,FVIFLD       RE-TRANSMIT USER'S ACTION                    
         B     ERRSHRT             USER MUST BE MORE SPECIFIC                   
                                                                                
VALACT18 MVC   MRKACT,0(RE)        SET FULL ACTION NAME                         
         OI    MRKACTH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   XACTNAME,0(RE)      SET THIS TIME ACTION NAME                    
         MVC   XACTION,ACTION      NUMBER                                       
         MVC   XACTINDS,ACTINDS    INDICATOR                                    
         MVC   XACTIND2,ACTIND2    INDICATOR 2                                  
         MVC   SACTNAME,XACTNAME   SAVE NAME                                    
         MVC   SACTION,XACTION     SAVE NUMBER                                  
         MVC   SACTINDS,XACTINDS   SAVE INDICATOR                               
         MVC   SACTIND2,XACTIND2   SAVE INDICATOR                               
         MVC   XACTOLAY,ACTOLAY    OVERLAY NUMBER                               
         MVC   XACTSCR1,ACTSCR1    HEADER SCREEN NUMBER                         
         MVC   XACTSCR2,ACTSCR2    INPUT SCREEN NUMBER                          
         MVC   XACTALFA,ACTALFA    ALPHA CODE FOR PROFILE LOOK-UP               
         MVC   XACTOPEV,ACTOPEV    BIT VALUE FOR OPTION VALIDATION              
         MVC   XACTYTOT,ACTYTOT    Y(ACTION TOTAL TABLE-MARKER)                 
         MVC   XACTBTYP,ACTBTYP    ACTION BATCH TYPE                            
                                                                                
VALACT20 LA    R1,ACTTABL(R1)      TRY NEXT ACTION TABLE ENTRY                  
         CLI   0(R1),EOT           FINISHED WITH TABLE?                         
         BNE   VALACT06            NO - NEXT ACTION                             
         CLI   XACTION,ACTINIT                                                  
         BE    ERRNOTV                                                          
         OC    TWASTYAC,TWASTYAC   TEST FOR SAVED 'REAL' ACTION                 
         BZ    VALACT22                                                         
         CLC   TWASTYP,XTYPE       TEST IF 'REAL' ACTION HAS CHANGED            
         BNE   *+14                                                             
         CLC   TWASACT,XACTION     TEST IF 'REAL' ACTION HAS CHANGED            
         BE    VALACT24                                                         
         XC    TWASKEY,TWASKEY     CLEAR SAVED ACCOUNT IN TWA                   
         NI    TWAMODE2,255-TWAM2HED  SET TO LOAD NEW HEADER SCREEN             
*&&US                                                                           
         XC    MRKOPT,MRKOPT       CLEAR OPTIONS                                
         MVI   MRKOPTH+(FVILEN-FVIHDR),0                                        
         OI    MRKOPTH+(FVOIND-FVIHDR),FVOXMT                                   
*MN      XC    OPTIONS(OPTIONSL),OPTIONS                                        
         LA    RE,OPTIONS          SET MAXIMUM END DATE                         
         LA    RF,OPTIONSL                                                      
         XCEF                                                                   
*MN                                                                             
         XC    SOPTDIS,SOPTDIS     CLEAR SAVED CHARACTER DISPLAY OPTION         
*&&                                                                             
*                                                                               
VALACT22 NI    PCDRIVEN,X'FF'-PCGRIDQ                                           
         TM    PCDRIVEN,PCGDATQ    WANTS GRID DATA                              
         BZ    VALACT23                                                         
         TM    XACTIND2,ACTI2GRD   ACTION IS ENABLED FOR GRIDS                  
         BZ    VALACT23                                                         
         OI    PCDRIVEN,PCGRIDQ    SET WE ARE RUNNING UNDER GRID                
         OI    MRKSCRLH+(FVATRB-FVIHDR),FVAZERO                                 
         OI    MRKSCRLH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    MRKSCRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKSCRH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    MRKSCRH+(FVATRB-FVIHDR),X'FF'-FVAMODF                            
*                                                                               
         USING PROFKEYD,RF                                                      
VALACT23 XC    WORK,WORK           READ AGENCY LEVEL PROFILE PAGE 2             
         LA    RF,WORK             R1=A(PROFILE KEY)                            
         MVI   PROFKSYS,C'A'-X'40' LOWER CASE SYSTEM                            
         MVC   PROFKAGY,TWAAGY     AGENCY                                       
         MVC   PROFKXAC,XACTALFA   TYPE/ACTION                                  
         MVI   PROFKPAG,C'2'       PAGE 2                                       
         GOTO1 VGETPROF,DMCB,PROFKEYD,PROFILE2,VDATAMGR                         
         DROP  RF                                                               
         MVC   LTYPACT,TWASTYAC    SET LAST TYPE/ACTION                         
                                                                                
VALACT24 MVC   TWASTYP,XTYPE       SAVE THIS TYPE/ACTION                        
         MVC   TWASACT,XACTION                                                  
*&&US                                                                           
         MVI   BATCHSEC,FF         ASSUME ALL BATCH SECURITY                    
         OC    XACTBTYP,XACTBTYP   TEST SPECIFIC BATCH TYPE                     
         BZ    VALACT28                                                         
         XC    TEMP(16),TEMP       DUMMY FIELD NO HEADER/NO DATE                
         GOTO1 VBMONVAL,DMCB,(0,TEMP),(XACTBTYP,ACOM),(AGYLANG,WORK),  X        
               (COMPANY,0)                                                      
         MVC   BATCHSEC,0(R1)      BATCH SECURITY LEVEL                         
*&&                                                                             
VALACT28 L     RF,AINP             TEST PF1 OR PF13 HIT                         
         MVC   BYTE,TIOBAID-TIOBD(RF)  EXTRACT PF KEY VALUE                     
         MVI   TIOBAID-TIOBD(RF),0                                              
         CLI   BYTE,PFK01                                                       
         BE    *+12                                                             
         CLI   BYTE,PFK13          ALLOW PF13 TOO                               
         BNE   VALACTX                                                          
         OI    TWAMODE2,TWAM2NXA   SET NEXT ACCOUNT SEEKING MODE                
         B     VALACTX                                                          
         DROP  R1                                                               
         EJECT                                                                  
VALACT30 L     RF,AINP             VALIDATE 'TEMPORARY' PFK ACTIONS             
         XR    RE,RE                                                            
         ICM   RE,1,TIOBAID-TIOBD(RF)                                           
         BZ    VALACTX                                                          
         TM    TWAMODE3,TWAM3ASC   TEST APPLICATION SCREEN PROCESSING           
         BO    VALACTX             LEAVE APPLICATION TO VALIDATE PFKEY          
         STC   RE,WORK                                                          
         TM    PCDRIVEN,PCGRIDQ    TEST WE ARE RUNNING UNDER GRIDS              
         BNZ   VALACT32                                                         
         CLI   WORK,PFK01          TEST ALTERNATE PF KEY ENTERED                
         BE    *+12                                                             
         CLI   WORK,PFK13          ALLOW PF13 AS WELL AS PF1                    
         BNE   VALACT32                                                         
         MVI   TIOBAID-TIOBD(RF),0                                              
         XI    TWAMODE,TWAMALTP    SWITCH TO/FROM ALTERNATE PFKEYS              
         GOTO1 ABLDPFK             BUILD PFKEY DISPLAY LINE                     
         LA    R1,MRKSCRH          SET A(SCROLL FIELD)                          
         ST    R1,FVADDR                                                        
         B     FVERRX                                                           
                                                                                
VALACT32 L     R1,AAPFTAB                                                       
         MVI   SSCROLL,0           RESET VERTICAL SCROLL INDICATORS             
         USING APFTABD,R1          R1=A(ACTION TABLE)                           
VALACT34 CLI   APFTPFK,EOT         TEST E-O-T                                   
         BE    VALACTX                                                          
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST WE ARE RUNNING UNDER GRIDS              
         BZ    VALACT36                                                         
         TM    APFTIND2,APFT2GRD   IS PFKEY VALID FOR GRIDS                     
         BZ    VALACT40            NO                                           
*                                                                               
VALACT36 CLC   APFTPFK,WORK        MATCH ON PFKEY NUMBER                        
         BNE   VALACT40                                                         
         TM    APFTIND1,APFTINUQ   TEST OBSERVE TWAMODE2,TWAM2NUQ               
         BZ    *+12                                                             
         TM    TWAMODE2,TWAM2NUQ                                                
         BO    VALACT40                                                         
         CLI   APFTOVR,0           TEST OVERLAY RESTRICTION                     
         BE    VALACT42            NO RESTRICTION                               
         CLC   APFTOVR,XACTOLAY    TEST CORRECT OVERLAY                         
         BNE   VALACT40            NO - TRY AGAIN                               
         MVC   TWAPFK,WORK         SAVE PFKEY FOR OVERLAY                       
         B     VALACTX             SKIP SETTING ACTION VALUES                   
                                                                                
VALACT40 LA    R1,APFTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         B     VALACT34                                                         
                                                                                
VALACT42 ICM   R0,3,APFTADDR       TEST NO ACTION NAME                          
         BNZ   VALACT70                                                         
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    TEST IF RUNNING UNDER GRIDS                  
         BNZ   VALACTX             YES - DO NOT ALLOW SCROLL                    
*                                                                               
         TM    APFTIND1,APFTILFT+APFTIRGH                                       
         BZ    VALACT54                                                         
         OI    SSCROLL,SSCROLLV    SET VERTICAL SCROLL ON                       
         ST    R1,FULL2                                                         
         MVI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         MVC   TEMP(L'MRKSCR),MRKSCR                                            
         CLI   MRKSCR,SCRDOWN      TEST VALID DIRECTION                         
         BE    *+12                                                             
         CLI   MRKSCR,SCRUP                                                     
         BNE   *+14                                                             
         MVC   MRKSCR(L'MRKSCR-1),MRKSCR+1                                      
         MVI   MRKSCR+(L'MRKSCR-1),C' '                                         
         GOTO1 AFLDVAL,MRKSCRH                                                  
         MVC   MRKSCR,TEMP                                                      
         L     R1,FULL2                                                         
         OC    FULL,FULL                                                        
         BNZ   VALACT52                                                         
         XR    RF,RF                                                            
         IC    RF,SSCRCOLL                                                      
         BCTR  RF,0                                                             
         XR    R0,R0                                                            
         IC    R0,SSCRCOLL+1                                                    
         SR    R0,RF               NUMBER OF COLUMNS DISPLAYED                  
         IC    RF,MRKSCRH+(FVILEN-FVIHDR)                                       
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BM    VALACT52                                                         
         EX    RF,*+8                                                           
         BE    VALACT50                                                         
         CLC   MRKSCR+1(0),SCR@PAGE                                             
         EX    RF,*+8                                                           
         BNE   VALACT52                                                         
         CLC   MRKSCR+1(0),SCR@HALF                                             
         SRA   R0,1                                                             
VALACT50 ST    R0,FULL             SCROLL & BUILD FROM RIGHT                    
         TM    APFTIND1,APFTILFT                                                
         BNO   VALACT52                                                         
         IC    R0,SSCRCOLL+1                                                    
         S     R0,FULL                                                          
         STC   R0,SSCRCOLL+1                                                    
         MVI   SSCRCOLL,1                                                       
         CH    R0,=H'1'                                                         
         BNH   VALACT54                                                         
         OI    SSCROLL,SSCROLLR                                                 
         B     VALACT54                                                         
                                                                                
VALACT52 IC    R0,SSCRCOLL         SCROLL BY MAGNITUDE                          
         S     R0,FULL                                                          
         TM    APFTIND1,APFTIRGH                                                
         BNO   *+12                                                             
         A     R0,FULL                                                          
         A     R0,FULL                                                          
         STC   R0,SSCRCOLL                                                      
         MVI   SSCRCOLL+1,127                                                   
                                                                                
VALACT54 DS    0H                                                               
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE FC FUNCTIONS                     
         BZ    VALACT58                                                         
                                                                                
         TM    APFTIND1,APFTIALT   TEST PFK TOTALS                              
         BNO   *+12                                                             
         XI    SAFCIND1,SAFCIALT   TOGGLE DISCOUNT/EXCHANGE DIFFERENCE          
         OI    SSCROLL,SSCROLLV                                                 
                                                                                
         TM    APFTIND1,APFTICOD   TEST TOGGLE TOTALS PFKEY                     
         BNO   VALACT40                                                         
         TM    SCURIND1,SCURITOT   UNLESS SHOWING SECOND TOTALS                 
         BO    VALACT58                                                         
         XI    SAFCIND1,SAFCI1ON   TOGGLE DISPLAY AFC/NORMAL TOTALS             
         OI    SSCROLL,SSCROLLV                                                 
                                                                                
VALACT58 TM    APFTIND1,APFTICOD   TEST TOGGLE TOTALS PFKEY                     
         BNO   VALACT40                                                         
         TM    SAFCIND1,SAFCI1ON   UNLESS SHOWING AFC TOTALS                    
         BO    VALACT40                                                         
         OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY                      
         BZ    VALACT40                                                         
         XI    SCURIND1,SCURITOT   TOGGLE SECOND/NORMAL TOTALS                  
         OI    SSCROLL,SSCROLLV    HOLD SCROLLING                               
*&&                                                                             
         B     VALACT40                                                         
                                                                                
VALACT70 MVC   LAREADDR,APFTADDR                                                
         EX    0,LARE                   GET A(ACTION WORD)                      
         MVC   XACTNAME(ACTADLQ),0(RE)  SET THIS TIME ACTION NAME               
         MVC   XACTION,APFTACT          NUMBER                                  
         MVC   XACTINDS,APFTACTI        INDICATORS                              
         DROP  R1                                                               
                                                                                
         OI    MRKACTH+(FVOIND-FVIHDR),FVOXMT                                   
         XR    R1,R1                                                            
         IC    R1,MRKACTH+(FVTLEN-FVIHDR)                                       
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    MRKACTH+(FVATRB-FVIHDR),FVAXTND                                  
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    MRKACT(0),MRKACT                                                 
         MVC   MRKACT(ACTADLQ),XACTNAME                                         
                                                                                
VALACTX  LH    R1,XACTYTOT         Y(ACTION TOTALS TABLE)                       
         LA    R1,MARKER(R1)                                                    
         ST    R1,AACTTOT          RESET A('REAL' ACTION TOTALS TABLE)          
         CLI   XACTION,ACTQUIT     TEST QUITTING                                
         BE    GO                  SKIP ANY FURTHER VALIDATION                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE NEW SECURITY                                               *         
***********************************************************************         
                                                                                
VALSEC   OC    TWASAGN,TWASAGN     TEST NEW SECURITY                            
         BZ    VALSECX                                                          
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),(XTYPE,XACTION)                
         BNE   ERRSECLO            SECURITY LOCKOUT                             
VALSECX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT-ID                                                  *         
***********************************************************************         
                                                                                
VALREP   XC    PRTSUB,PRTSUB       CLEAR REPORT-ID                              
         CLI   XACTION,ACTDRFT                                                  
         BNE   *+8                                                              
         MVI   FVMINL,1            COMPULSORY FOR DRAFT                         
         GOTO1 AFLDVAL,MRKREPH                                                  
         BH    ERRNONE                                                          
         BL    VALREPX                                                          
         MVC   PRTSUB,FVIFLD       SET REPORT-ID                                
         OI    MRKREPH+(FVOIND-FVIHDR),FVOXMT                                   
VALREPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SCROLL AMOUNT                                              *         
***********************************************************************         
                                                                                
VALSCR   TM    PCDRIVEN,PCGRIDQ    TEST RUNNING UNDER GRIDS                     
         BO    VALSCRX             YES - CAN'T SCROLL                           
         TM    SSCROLL,SSCROLLV    TEST VERTICAL SCROLL ON                      
         BO    VALSCRX                                                          
         XC    SCRVALS(SCRVALSL),SCRVALS                                        
         MVC   TEMP(L'MRKSCR),MRKSCR                                            
         L     RF,AINP             TEST PFKEY SCROLL COMMAND                    
         MVC   WORK(1),TIOBAID-TIOBD(RF)                                        
         CLI   WORK,0                                                           
         BE    VALSCR04                                                         
         TM    TWAMODE3,TWAM3ASC   TEST APPLICATION SCREEN PROCESSING           
         BO    VALSCR04            LEAVE APPLICATION TO VALIDATE PFKEY          
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
         XC    MRKSCR,MRKSCR       CLEAR & BUILD SCROLL FIELD                   
         MVI   MRKSCRH+(FVILEN-FVIHDR),L'SCR@PAGE                               
         LA    RF,MRKSCR                                                        
         CLI   SPFTDIR,0           TEST EXPLICIT DIRECTION                      
         BE    VALSCR03                                                         
         MVC   0(L'SPFTDIR,RF),SPFTDIR                                          
         MVI   MRKSCRH+(FVILEN-FVIHDR),L'SCR@PAGE+L'SPFTDIR                     
         LA    RF,L'SPFTDIR(RF)                                                 
VALSCR03 MVC   0(L'SCR@PAGE,RF),0(RE)                                           
         OI    MRKSCRH+(FVOIND-FVIHDR),FVOXMT                                   
         DROP  R1                                                               
                                                                                
VALSCR04 CLI   MRKSCRH+(FVILEN-FVIHDR),0                                        
         BE    VALSCR30                                                         
         MVI   SCRLDIR,SCRDOWN     PRESET TO SCROLL DOWN                        
         CLI   MRKSCR,SCRDOWN      TEST VALID DIRECTION                         
         BE    *+12                                                             
         CLI   MRKSCR,SCRUP                                                     
         BNE   VALSCR06                                                         
         MVC   SCRLDIR,MRKSCR      SET DIRECTION & REMOVE IT                    
         MVC   MRKSCR(L'MRKSCR-1),MRKSCR+1                                      
         MVI   MRKSCR+(L'MRKSCR-1),C' '                                         
                                                                                
VALSCR06 MVI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         GOTO1 AFLDVAL,MRKSCRH                                                  
         BNE   FVERR                                                            
         TM    FVIIND,FVINUM       IS INPUT NUMERIC                             
         BZ    VALSCR08                                                         
         L     R0,FULL                                                          
         CH    R0,=Y(SCRPAGE)      UP TO A PAGE ALLOWED                         
         BH    ERRISCRL                                                         
         STCM  R0,3,SCRLAMT        SAVE SCROLL AMOUNT                           
                                                                                
         XC    MRKSCR,MRKSCR       RE-DISPLAY SCROLL AMOUNT                     
         MVC   MRKSCR(1),SCRLDIR                                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MRKSCR+1(2),DUB                                                  
         CLI   MRKSCR+1,C'0'                                                    
         BNE   *+10                                                             
         MVC   MRKSCR+1(2),MRKSCR+2                                             
         B     VALSCR30                                                         
                                                                                
VALSCR08 CLI   FVILEN,L'SCR@PAGE   TEST INPUT GR MAXIMUM LEN                    
         BH    ERRLONG                                                          
         L     R1,ASCRTAB          LOOK-UP SCROLL TABLE                         
         USING SCRTABD,R1                                                       
VALSCR10 XR    RF,RF                                                            
         IC    RF,FVXLEN           EXECUTE LENGTH OF I/P                        
         MVC   LAREADDR,SCRTSCR                                                 
         EX    0,LARE                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FVIFLD      COMPARE I/P WITH SCROLL NAME                 
         BNE   VALSCR16                                                         
                                                                                
VALSCR12 OC    SCRLAMT,SCRLAMT     DO WE ALREADY HAVE A MATCH?                  
         BZ    VALSCR14                                                         
         MVC   MRKSCR,TEMP         RESTORE ORIGINAL SCROLL FIELD                
         B     ERRSHRT                                                          
                                                                                
VALSCR14 XC    MRKSCR,MRKSCR       RE-DISPLAY SCROLL                            
         MVC   MRKSCR(1),SCRLDIR                                                
         MVC   MRKSCR+1(L'SCR@PAGE),0(RE)                                       
         MVC   SCRLAMT,SCRTAMT     TAKE SCROLL MAGNITUDE FROM TABLE             
         CLI   SCRTDIR,0           TEST DIRECTION IS EXPLICIT                   
         BE    *+10                                                             
         MVC   SCRLDIR,SCRTDIR     YES - SET DIRECTION                          
                                                                                
VALSCR16 LA    R1,SCRTABL(R1)                                                   
         CLI   0(R1),EOT           FINISHED WITH TABLE?                         
         BNE   VALSCR10            NO - NEXT SCROLL KEYWORD                     
         OC    SCRLAMT,SCRLAMT                                                  
         BZ    ERRNOTV             ERROR - NO MATCH                             
                                                                                
VALSCR18 CLC   SCRLAMT,=AL2(SCRMAXI)  TEST MAXIMUM SCROLL REQUEST               
         BNE   VALSCR30                                                         
         XC    MRKSCR,MRKSCR       SET SCROLL TO +PAGE OR -PAGE                 
         MVI   MRKSCR,SCRDOWN                                                   
         CLI   SCRLDIR,SCRUP                                                    
         BE    *+8                                                              
         MVI   MRKSCR,SCRUP                                                     
         MVC   MRKSCR+1(L'SCR@PAGE),SCR@PAGE                                    
                                                                                
VALSCR30 OC    SCRVALS(SCRVALSL),SCRVALS                                        
         BNZ   VALSCRX                                                          
         MVC   SCRLAMT,=Y(SCRPAGE)  SET DEFAULT SCROLL (+PAGE)                  
         MVI   SCRLDIR,SCRDOWN                                                  
         XC    MRKSCR,MRKSCR                                                    
         MVC   MRKSCR(1),SCRLDIR                                                
         MVC   MRKSCR+1(L'SCR@PAGE),SCR@PAGE                                    
VALSCRX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS                                                    *         
***********************************************************************         
                                                                                
VALOPT   NI    DISIND,255-DISINCOL  CLEAR NEW COLUMN DISPLAY                    
         OI    INDS1,INDSVOPT      SET VALIDATING OPTIONS                       
*MN      MVC   TEMP(OPTIONSL),OPTIONS                                           
         LA    R0,TEMP                                                          
         LA    R1,L'TEMP                                                        
         LA    RE,OPTIONS                                                       
         LA    RF,OPTIONSL                                                      
         MVCL  R0,RE               MOVE TIA SCREEN INTO TWA                     
*MN                                                                             
*MN      XC    OPTIONS(OPTIONSL),OPTIONS                                        
         LA    RE,OPTIONS          SET MAXIMUM END DATE                         
         LA    RF,OPTIONSL                                                      
         XCEF                                                                   
*MN                                                                             
         XC    SOPTDIS,SOPTDIS     CLEAR SAVED CHARACTER DISPLAY OPTION         
         CLI   MRKOPTH+(FVILEN-FVIHDR),0                                        
         BE    VALOPTX                                                          
         LA    R1,MRKOPTH                                                       
         ST    R1,FVADDR           TAKE FIELD ADDRESS FOR ERRORS                
         LA    R2,WORKD                                                         
         AH    R2,=Y(SCANOUT-WORKD)                                             
         USING SCANOUTD,R2         R2=A(SCANNER OUTPUT BLOCK)                   
         LR    R0,R2               CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,MRKOPTH),('SCANMAXN',(R2)),   *        
               C',=  '                                                          
         MVC   BYTE,4(R1)                                                       
         CLI   BYTE,0                                                           
         BE    ERRNOTV                                                          
         MVI   FVINDX,1                                                         
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
         XR    RF,RF                                                            
         ICM   RF,1,SCANLEN1                                                    
         BZ    ERRNONE                                                          
         CLM   RF,1,=AL1(OPTLONG)                                               
         BH    ERRLONG             OPTION KEYWORD TOO LONG                      
VALOPT22 CLM   RF,1,=AL1(OPTSHRT)                                               
         BH    VALOPT24                                                         
         MVC   LAREADDR,OPTOPSH    TRY SHORT KEYWORD                            
         B     *+10                                                             
VALOPT24 MVC   LAREADDR,OPTOPT     FULL KEYWORD                                 
         EX    0,LARE                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCANTXT1(0),0(RE)                                                
         BE    VALOPT30                                                         
         CLC   LAREADDR,OPTOPT     HAVE WE TRIED FULL KEYWORD YET               
         BE    VALOPT26                                                         
         IC    RF,SCANLEN1                                                      
         B     VALOPT24            NO - TRY IT                                  
                                                                                
VALOPT26 LA    R1,OPTTABL(R1)                                                   
         IC    RF,SCANLEN1         REFRESH LENGTH                               
         CLI   0(R1),EOT           AND IF THERE'S ANOTHER ENTRY                 
         BNE   VALOPT22            GO BACK TO TRY IT                            
         B     ERRNOTV             NO MATCH - ERROR                             
                                                                                
VALOPT30 MVC   THREE,OPTVACM       TEST OPTION/ACTION VALIDITY                  
         NC    THREE,XACTOPEV                                                   
         CLC   THREE,XACTOPEV                                                   
         BNE   VALOPT26            NOT VALID WITH THIS ACTION                   
*                                                                               
         TM    OPTIND,OPTIGRD      TEST NOT ALLOWED ON GRID                     
         BZ    *+12                NO                                           
         TM    PCDRIVEN,PCGRIDQ    YES - ARE WE RUNNING UNDER GRID              
         BO    VALOPT26            YES - REJECT THIS ENTRY                      
*                                                                               
         TM    OPTIND,OPTIDDS      TEST DDS-ONLY OPTION                         
         BNO   *+12                                                             
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BNE   VALOPT26            NO - GET NEXT ACTION TABLE ENTRY             
         CLI   FLAG,1              TEST 'NOT' INPUT                             
         BNE   *+12                                                             
         TM    OPTIND,OPTINOT      TEST 'NOT' IS ALLOWED                        
         BZ    VALOPT26                                                         
         TM    OPTIND,OPTI2PT                                                   
         BO    VALOPT40                                                         
         CLI   SCANLEN2,0          ONE-PART MUST NOT HAVE 2ND PART              
         BNE   VALOPT26                                                         
         MVC   LARFADDR,OPTADDR                                                 
         EX    0,LARF                                                           
         CLI   0(RF),0             TEST OPTION ALREADY INPUT                    
         BNE   ERRKDUP             DUPLICATE OPTION                             
         MVI   0(RF),INCLUDE                                                    
         TM    FLAG,1              TEST 'NOT' INPUT                             
         BZ    *+8                                                              
         MVI   0(RF),EXCLUDE                                                    
         B     VALOPT60                                                         
                                                                                
VALOPT40 CLI   SCANLEN2,0          TWO-PART REQUIRE 2ND PART                    
         BE    VALOPT26                                                         
         CLC   SCANLEN2,OPTRMAX    CHECK MAXIMUM LENGTH ALLOWED                 
         BH    ERRLONG                                                          
         MVC   LARFADDR,OPTADDR                                                 
         EX    0,LARF                                                           
         BR    RF                  BRANCH TO MINI-ROUTINE                       
                                                                                
VALOPT60 IC    R1,FVINDX           BUMP TO NEXT SCANNER ENTRY                   
         LA    R1,1(R1)                                                         
         CLM   R1,1,BYTE                                                        
         BH    VALOPTX                                                          
         STC   R1,FVINDX                                                        
         LA    R2,SCANOUTL(R2)     TAKE NEXT OPTION                             
         B     VALOPT10                                                         
                                                                                
VALOPTX  CLC   OPTIONS(KEYOPTSL),TEMP  TEST KEY OPTION(S) CHANGED               
         BE    *+16                                                             
         OI    DISIND,DISIRST          YES - SET RESTART FROM BEGINNING         
         NI    DISIND,X'FF'-DISINIT                                             
         NI    TWAMODE2,255-TWAM2SKP   AND RESET SKIP VALINP                    
         CLC   OPTDIS,TEMP+(OPTDIS-OPTIONS) TEST NEW COLUMN DISPLAY             
         BE    *+16                                                             
         OI    DISIND,DISINCOL         SET NEW COLUMN DISPLAY                   
         MVI   SSCRCOLL,1              RESET VERTICAL SCROLL                    
         MVI   SSCRCOLL+1,127          RESET VERTICAL SCROLL                    
         CLC   OPTALPG,TEMP+(OPTALPG-OPTIONS)  TEST ALL=/PAGE= CHANGED          
         BE    *+8                                                              
         NI    TWAMODE2,255-TWAM2SKP   YES - RESET SKIP VALINP                  
         OI    MRKOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    INDS1,255-INDSVOPT      RESET VALIDATING OPTIONS                 
         B     GO                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIFIC VALIDATION FOR TWO-PART OPTIONS.                           *         
* NTRY - R2=A(SCANNER BLOCK ELEMENT)                                  *         
*      - FVMSGNO=EGOPTDUP                                             *         
* EXIT - TO VALOPT60, IF VALID                                        *         
*      - TO ONE OF THE COMMON ERROR EXITS, IF INVALID                 *         
***********************************************************************         
                                                                                
VOPDAT   OC    OPTDATR,OPTDATR     DATE RANGE                                   
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPDAT2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSDT,PVALPSTA     ELSE TAKE START DATE                         
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPDAT4             IF SO, ASSUME END DATE IS TODAY              
VOPDAT2  MVC   OPTEDT,PVALPEND     ELSE TAKE END DATE                           
         B     VOPEXIT                                                          
VOPDAT4  MVI   OPTEDT,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTEDT+1(L'OPTEDT-1),OPTEDT                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
                                                                                
VOPADA   OC    OPTADAR,OPTADAR     ACTIVITY DATES (COMPRESSED)                  
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPADA2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTADS,PVALCSTA     ELSE TAKE START DATE                         
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPADA4             IF SO, ASSUME END DATE IS TODAY              
VOPADA2  MVC   OPTADE,PVALCEND     ELSE TAKE END DATE                           
         B     VOPEXIT                                                          
VOPADA4  MVI   OPTADE,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTADE+1(L'OPTADE-1),OPTADE                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
                                                                                
VOPREF   OC    OPTREFR,OPTREFR                                                  
         BNZ   ERRKDUP                                                          
         XR    R1,R1                                                            
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                R1=EXECUTE L'FIELD                           
         LA    RF,SCANTXT2(R1)     RF=A(LAST CHARACTER)                         
         CLI   SCANTXT2,C'-'       TEST -END                                    
         BE    *+12                                                             
         CLI   0(RF),C'-'          TEST START-                                  
         BNE   VOPREF04                                                         
                                                                                
         CLM   R1,1,=AL1(L'OPTSRF) REFERENCE MUST BE <=6 CHARS                  
         BH    ERRLONG                                                          
         CLI   SCANLEN2,2          REFERENCE MUST BE >=1 CHAR                   
         BL    ERRSHRT                                                          
         BCTR  R1,0                DROP THE HYPHEN                              
         CLI   SCANTXT2,C'-'       TEST -END                                    
         BE    VOPREF02                                                         
         STC   R1,OPTSRFXL         SET START REFERENCE                          
         EX    R1,*+4                                                           
         MVC   OPTSRF(0),SCANTXT2                                               
         MVI   OPTERF,C'9'         SET MAXIMUM END REF                          
         MVC   OPTERF+1(L'OPTERF-1),OPTERF                                      
         MVI   OPTERFXL,L'OPTERF-1 SET EXECUTE LENGTH                           
         B     VOPEXIT             AND THAT WILL DO                             
                                                                                
VOPREF02 STC   R1,OPTERFXL         SET END REFERENCE                            
         EX    R1,*+8                                                           
         B     VOPEXIT                                                          
         MVC   OPTERF(0),SCANTXT2+1                                             
                                                                                
VOPREF04 LTR   R1,R1               TEST MORE THAN ONE CHARACTER                 
         BZ    VOPREF06                                                         
         CLI   0(RF),C'-'          SEARCH FOR A HYPHEN                          
         BE    VOPREF08                                                         
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         CLI   SCANLEN2,L'OPTSRF   REFERENCE MUST BE <=6 CHARS                  
         BH    ERRLONG                                                          
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
VOPREF06 STC   R1,OPTSRFXL         SET START REFERENCE                          
         EX    R1,*+8                                                           
         B     VOPEXIT                                                          
         MVC   OPTSRF(0),SCANTXT2                                               
                                                                                
VOPREF08 XR    RE,RE               EXTRACT BOTH SIDES                           
         IC    RE,SCANLEN2                                                      
         BCTR  RE,0                                                             
         SR    RE,R1                                                            
         CLM   R1,1,=AL1(L'OPTSRF)                                              
         BH    ERRLONG                                                          
         CLM   RE,1,=AL1(L'OPTERF)                                              
         BH    ERRLONG                                                          
         BCTR  R1,0                                                             
         STC   R1,OPTSRFXL                                                      
         EX    R1,*+4                                                           
         MVC   OPTSRF(0),SCANTXT2                                               
         BCTR  RE,0                                                             
         STC   RE,OPTERFXL                                                      
         EX    RE,*+4                                                           
         MVC   OPTERF(0),1(RF)                                                  
         CLC   OPTSRF,OPTERF       YES - CHECK START DOESN'T EXCEED END         
         BH    ERRRANGE                                                         
         B     VOPEXIT                                                          
                                                                                
VOPMOS   OC    OPTMOSR,OPTMOSR     MONTH OF SERVICE RANGE                       
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPMOS2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSMO,PVALPSTA     ELSE TAKE START DATE                         
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPMOS4             IF SO, ASSUME MAX. END DATE                  
VOPMOS2  MVC   OPTEMO,PVALPEND     ELSE TAKE END DATE                           
         B     VOPEXIT                                                          
VOPMOS4  MVI   OPTEMO,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTEMO+1(L'OPTEMO-1),OPTEMO                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
                                                                                
VOPCTR   OC    OPTCON,OPTCON                                                    
         BNZ   ERRKDUP                                                          
         MVC   OPTCON,SCANTXT2                                                  
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         CLI   OPTCON,C'*'         TEST NOT PREFIX                              
         BNE   VOPCTR2                                                          
         MVC   OPTCON,SCANTXT2+1                                                
         OI    OPTCONI,OPTINOT     SET NEGATIVE FILTER                          
         BCTR  R1,0                                                             
VOPCTR2  STC   R1,OPTCONXL                                                      
         B     VOPEXIT                                                          
                                                                                
VOPSRC   OC    OPTSRC,OPTSRC                                                    
         BNZ   ERRKDUP                                                          
         MVC   OPTSRC,SCANTXT2                                                  
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         CLI   OPTSRC,C'*'         TEST NOT PREFIX                              
         BNE   VOPSRC2                                                          
         MVC   OPTSRC,SCANTXT2+1                                                
         OI    OPTSRCI,OPTINOT     SET NEGATIVE FILTER                          
         BCTR  R1,0                                                             
VOPSRC2  STC   R1,OPTSRCXL                                                      
         B     VOPEXIT                                                          
                                                                                
VOPAMT   OC    OPTAMT,OPTAMT                                                    
         BNZ   ERRKDUP                                                          
         XR    R0,R0                                                            
         IC    R0,SCANLEN2                                                      
         MVI   OPTAMTM,X'40'       SET AMOUNT MODIFIER TO BL                    
         CLI   SCANTXT2,C'>'       TEST AMOUNT IS A MINIMUM                     
         BE    VOPAMT2                                                          
         MVI   OPTAMTM,X'20'       SET AMOUNT MODIFIER TO BH                    
         CLI   SCANTXT2,C'<'       TEST AMOUNT IS A MAXIMUM                     
         BNE   *+12                                                             
VOPAMT2  MVI   SCANTXT2,C' '       CLEAR > OR < SIGN                            
         B     VOPAMT3                                                          
         MVI   OPTAMTM,X'70'       SET AMOUNT MODIFIER TO BNE                   
         CLI   SCANTXT2,C'+'       TEST EXPLICIT POSITIVE                       
         BE    VOPAMT3                                                          
         CLI   SCANTXT2,C'-'       TEST EXPLICIT NEGATIVE                       
         BE    VOPAMT3                                                          
         OI    OPTAMTM,X'08'       SET +/- INDICATOR IN MODIFIER                
         B     *+16                                                             
VOPAMT3  MVC   SCANTXT2,SCANTXT2+1                                              
         MVI   SCANTXT2+L'SCANTXT2-1,0                                          
         BCTR  R0,0                                                             
*&&UK                                                                           
         CLI   SCANTXT2,C'0'                                                    
         BNL   VOPAMT4                                                          
         CH    R0,=H'3'                                                         
         BL    VOPAMT4                                                          
         PUSH  USING                                                            
         USING CURTABD,WORK                                                     
         GOTO1 VBLDCUR,PARM,SCANTXT2,CURTABD,ACOM                               
         CLI   0(R1),0                                                          
         BNE   ERRIAMNT                                                         
         MVC   OPTAMTC,CURTCUR                                                  
         POP   USING                                                            
         MVC   SCANTXT2,SCANTXT2+3                                              
         XC    SCANTXT2+L'SCANTXT2-3(3),SCANTXT2+L'SCANTXT2-3                   
         SH    R0,=H'3'                                                         
         TM    OPTAMTM,X'08'       TEST +/- INDICATOR IN MODIFIER               
         BNO   VOPAMT4                                                          
         CLI   SCANTXT2,C'+'       TEST EXPLICIT POSITIVE                       
         BE    *+12                                                             
         CLI   SCANTXT2,C'-'       TEST EXPLICIT NEGATIVE                       
         BNE   VOPAMT4                                                          
         NI    OPTAMTM,X'F7'       RESET +/- INDICATOR IN MODIFIER              
*&&                                                                             
VOPAMT4  GOTO1 VCASHVAL,PARM,SCANTXT2,(R0)                                      
         CLI   0(R1),0                                                          
         BNE   ERRIAMNT                                                         
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         OC    DUB(2),DUB          ENSURE WILL PACK INTO 6 BYTES                
         BNZ   ERRIAMNT                                                         
         ZAP   OPTAMT,DUB          SET FILTER AMOUNT                            
         B     VOPEXIT                                                          
                                                                                
VOPOFF   OC    OPTOFF,OPTOFF                                                    
         BNZ   ERRKDUP                                                          
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    *+12                                                             
         CLI   SCANLEN2,2          TEST TWO CHARACTERS GIVEN                    
         BL    ERROFFN2                                                         
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVI   OFFAACT,OFFAVAL     VALIDATE INPUT OFFICE                        
         MVC   OFFAOFFC,SCANTXT2   SET INPUT OFFICE                             
         CLI   SCANTXT2,C'*'       TEST NOT PREFIX                              
         BNE   VOPOFF2                                                          
         CLI   SCANLEN2,1          CHECK FOR INPUT BEYOND THE *                 
         BE    ERRSHRT                                                          
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    *+12                                                             
         CLI   SCANLEN2,3          CHECK TWO CHARACTERS BEYOND THE *            
         BNE   ERROFFN2                                                         
         MVC   OFFAOFFC,SCANTXT2+1                                              
         OI    OPTOFFI,OPTINOT     SET NEGATIVE FILTER                          
VOPOFF2  GOTO1 VOFFAL                                                           
         BNE   ERRSECLO            SECURITY LOCKOUT                             
         MVC   OPTOFF,OFFAOFFC     SET OFFICE FILTER                            
         B     VOPEXIT                                                          
         DROP  R1                                                               
                                                                                
VOPCLI   IC    RF,PRODALEN                                                      
         B     VOPCPJ                                                           
                                                                                
VOPPRO   IC    RF,PRODBLEN                                                      
         B     VOPCPJ                                                           
                                                                                
VOPJOB   IC    RF,PRODCLEN                                                      
                                                                                
VOPCPJ   OC    OPTCPJ,OPTCPJ                                                    
         BNZ   ERRKDUP                                                          
         CLI   SCANTXT2,C'*'                                                    
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         OI    OPTCPJI,OPTINOT     SET NEGATIVE FILTER                          
         CLM   RF,1,SCANLEN2       TEST LENGTH                                  
         BL    ERRLONG                                                          
         MVC   OPTCPJ,SPACES                                                    
         LA    R1,OPTCPJ                                                        
*&&US                                                                           
         TM    ACCIND1,ACCIPROD    TEST PRODUCTION (CARRIES U/L)                
         BZ    *+14                                                             
*&&                                                                             
         MVC   OPTCPJ(L'PRODUL),PRODUL                                          
         LA    R1,L'PRODUL(R1)                                                  
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         LA    RE,SCANTXT2                                                      
         TM    OPTCPJI,OPTINOT     TEST NEGATIVE FILTER                         
         BZ    *+10                                                             
         LA    RE,1(RE)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),0(RE)                                                    
*&&US                                                                           
         TM    ACCIND1,ACCIPROD    TEST PRODUCTION (CARRIES U/L)                
         BZ    *+8                                                              
*&&                                                                             
         AH    RF,=Y(L'PRODUL)                                                  
         STC   RF,OPTCPJXL         SAVE EXECUTE L'ULCLI/PRO/JOB                 
         B     VOPEXIT                                                          
                                                                                
VOPWRK   OC    OPTWRK,OPTWRK                                                    
         BNZ   ERRKDUP                                                          
         CLC   SCANLEN2,=AL1(L'OPTWRK)                                          
         BL    ERRNOTV                                                          
         LA    RE,SCANTXT2                                                      
         BE    VOPWRK2                                                          
         CLI   SCANTXT2,C'*'       TEST NOT PREFIX                              
         BNE   ERRNOTV                                                          
         CLC   SCANLEN2,=AL1(L'OPTWRK+1)                                        
         BNE   ERRNOTV                                                          
         OI    OPTWRKI,OPTINOT     SET NEGATIVE FILTER                          
         LA    RE,1(RE)                                                         
VOPWRK2  MVC   OPTWRK,0(RE)                                                     
         B     VOPEXIT                                                          
                                                                                
VOPDSP   OC    OPTDIS,OPTDIS                                                    
         BNZ   ERRKDUP                                                          
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                SCANLEN2-1 FOR EXECUTE                       
         EX    RF,*+4                                                           
         MVC   SOPTDIS(0),SCANTXT2 SAVE AS FOUND FOR LATER REBUILD (CC)         
         LA    RF,L'OPTDIS         MUST TRANSLATE ALL OF OPTDIS                 
         MVC   OPTDIS,SCANTXT2     TAKE OPTION                                  
         LA    RE,OPTDIS                                                        
         MVI   CHAR,X'5D'          BASE LINE FOR USERS                          
         CLI   TWAOFFC,C'*'                                                     
         BNE   VOPDSP2                                                          
         MVI   CHAR,X'40'          EXTRA COLUMNS FOR DDS                        
VOPDSP2  CLI   0(RE),C'+'          TEST PROFILE OPTION                          
         BE    *+8                                                              
         CLI   0(RE),C' '          TEST VALID COLUMN INDEX CHARACTER            
         BE    *+14                                                             
         CLC   CHAR,0(RE)          TEST VALID COLUMN INDEX CHARACTER            
         BH    ERRNOTV                                                          
         NI    0(RE),X'FF'-X'40'   TURN OFF X'40' BIT FOR TRANTAB               
         LA    RE,1(RE)            NEXT COLUMN INDEX                            
         BCT   RF,VOPDSP2          LOOP THROUGH FIELD                           
         TR    OPTDIS,TRANTAB      PICK UP HEX VALUE FOR ALPHANUMERAL           
         B     VOPEXIT                                                          
                                                                                
VOPALL   OC    OPTALL,OPTPAG       TEST PAGE/GLOBAL MARKING ALREADY SET         
         BNZ   ERRKDUP                                                          
         CLI   XACTOLAY,CCOLAY     TEST CHEQUE OVERLAY                          
         BE    VOPALL2                                                          
         CLC   SCANTXT2(1),AC@YES                                               
         BE    VOPALL4                                                          
         CLC   SCANTXT2(1),AC@NO                                                
         BE    VOPALL4                                                          
         B     ERRNOTV                                                          
VOPALL2  CLC   SCANTXT2(1),AC3SELC                                              
         BNE   ERRNOTV                                                          
VOPALL4  MVC   OPTALL,SCANTXT2                                                  
         B     VOPEXIT                                                          
                                                                                
VOPPAG   OC    OPTALL,OPTPAG       TEST PAGE/GLOBAL MARKING ALREADY SET         
         BNZ   ERRKDUP                                                          
         CLI   XACTOLAY,BVOLAY     TEST BANK/VOID OVERLAY                       
         BE    VOPPAG2                                                          
         CLI   XACTOLAY,CCOLAY     TEST CREDITOR/CHEQUE OVERLAY                 
         BNE   *+12                                                             
         CLI   FILTCHQ,EXCLUDE     TEST INVOICE MODE                            
         BNE   VOPPAG4                                                          
         CLC   SCANTXT2(1),AC@YES                                               
         BE    VOPPAG6                                                          
         CLC   SCANTXT2(1),AC@NO                                                
         BE    VOPPAG6                                                          
         B     ERRNOTV                                                          
VOPPAG2  CLI   FILTCHQ,EXCLUDE     VOID - TEST INVOICE MODE                     
         BE    ERRNOTV             OPTION INVALID                               
         CLC   SCANTXT2(1),AC@YES                                               
         BE    VOPPAG6                                                          
         CLC   SCANTXT2(1),AC@NO                                                
         BE    VOPPAG6                                                          
VOPPAG4  CLC   SCANTXT2(1),AC3SELC CHEQUE MODE (OR HEADER) TEST SELECT          
         BNE   ERRNOTV                                                          
VOPPAG6  MVC   OPTPAG,SCANTXT2                                                  
         B     VOPEXIT                                                          
                                                                                
VOPBAT   OC    OPTBAT,OPTBAT                                                    
         BNZ   ERRKDUP                                                          
         TM    SCANIND2,X'80'      TEST NUMERIC INPUT                           
         BNO   *+12                                                             
         L     R1,SCANBIN2                                                      
         B     VOPBAT2                                                          
         CLI   SCANTXT2,C'*'       TEST NOT PREFIX                              
         BNE   ERRNOTV                                                          
         OI    OPTBATI,OPTINOT     SET NEGATIVE FILTER                          
         XR    R0,R0                                                            
         IC    R0,SCANLEN2                                                      
         MVI   SCANTXT2,C'0'       MAKE THIS NUMERIC                            
         LA    R1,SCANTXT2                                                      
         TM    0(R1),X'F0'         TEST ALL NUMERIC                             
         BZ    ERRNOTV                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCANTXT2(0)                                                  
         CVB   R1,DUB                                                           
VOPBAT2  STC   R1,OPTBAT                                                        
         B     VOPEXIT                                                          
                                                                                
VOPSBR   OC    OPTSUBR,OPTSUBR                                                  
         BNZ   ERRKDUP                                                          
         LA    R1,SCANTXT2                                                      
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BNE   VOPSBR2                                                          
         CLI   0(RF),C' '                                                       
         BH    VOPSBR6                                                          
         B     ERRSHRT                                                          
                                                                                
VOPSBR2  CLI   0(RF),C' '                                                       
         BE    VOPSBR4                                                          
         CLI   0(RF),C'-'                                                       
         BE    VOPSBR4                                                          
         LA    RF,1(RF)                                                         
         B     VOPSBR2                                                          
                                                                                
VOPSBR4  SR    RF,R1                                                            
         CLM   RF,1,=AL1(L'OPTSSR)                                              
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         STC   RF,OPTSSRXL                                                      
         EX    RF,*+4                                                           
         MVC   OPTSSR(0),0(R1)                                                  
         LA    R1,1(RF,R1)                                                      
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'          TEST START SUBREF ONLY                       
         BNE   VOPSBR8             YES - THAT WILL DO                           
         CLI   0(RF),C' '          TEST END SUBREF INPUT                        
         BH    VOPSBR6             YES EXTRACT IT                               
         MVI   OPTESR,C'9'         NO - SET MAXIMUM END SUBREF                  
         MVC   OPTESR+1(L'OPTESR-1),OPTESR                                      
         MVI   OPTESRXL,L'OPTESR-1 SET EXECUTE LENGTH                           
         B     VOPSBR8             AND THAT WILL DO                             
                                                                                
VOPSBR6  LR    R1,RF                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *-8                                                              
         SR    RF,R1                                                            
         CLM   RF,1,=AL1(L'OPTESR)                                              
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         STC   RF,OPTESRXL                                                      
         EX    RF,*+4                                                           
         MVC   OPTESR(0),0(R1)                                                  
                                                                                
VOPSBR8  OC    OPTESR,OPTESR       TEST END SUB-REFERENCE                       
         BZ    VOPEXIT                                                          
         CLC   OPTSSR,OPTESR       YES - CHECK START DOESN'T EXCEED END         
         BH    ERRRANGE                                                         
         B     VOPEXIT                                                          
         EJECT                                                                  
VOPBMO   GOTO1 VBMONVAL,DMCB,(SCANLEN2,SCANTXT2),(XACTBTYP,ACOM),      X        
               (AGYLANG,WORK),(COMPANY,0)                                       
         LA    R1,WORK                                                          
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                NO ERROR                                     
         MVC   FVMSGNO,BMOMSG      USE MESSAGE PASSED BY BMONVAL                
         B     ERRMSGON            ERROR EXIT MESSAGE ALREADY SET               
         CLC   HICMONP,BMOMOSP     TEST HIGHEST CHEQUE MOS V OPTION MOS         
         BH    ERRBMECM                                                         
         MVC   OBATMON,BMOMOSC     SET BATCH MONTH (EBCDIC FUNNY)               
         MVC   OBATMONP,BMOMOSP    SET BATCH MONTH (PWOS YYMM)                  
         B     VOPEXIT                                                          
                                                                                
VOPBRF   XR    R0,R0                                                            
         IC    R0,SCANLEN2                                                      
         LA    R1,SCANTXT2                                                      
         CLI   0(R1),C'A'                                                       
         BL    ERRNOTV                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         MVC   OBATREF,SCANTXT2                                                 
         B     VOPEXIT                                                          
                                                                                
VOPBNA   MVC   OBATNAM,SCANTXT2                                                 
         B     VOPEXIT                                                          
                                                                                
VOPBSD   OC    OPTBSDR,OPTBSDR     BANK STATEMENT DATE RANGE                    
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPBSD2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSBS,PVALCSTA     ELSE TAKE COMPRESSED START DATE              
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPBSD4             IF SO, ASSUME END DATE IS TODAY              
VOPBSD2  MVC   OPTEBS,PVALCEND     ELSE TAKE COMPRESSED END DATE                
         B     VOPEXIT                                                          
VOPBSD4  MVI   OPTEBS,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTEBS+1(L'OPTEBS-1),OPTEBS                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
         EJECT                                                                  
*MN                                                                             
VOPREC   OC    OPTRCDR,OPTRCDR     RECONCILED DATE RANGE                        
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPREC2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSREC,PVALPSTA    ELSE TAKE COMPRESSED START DATE              
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPREC4             IF SO, ASSUME END DATE IS TODAY              
VOPREC2  MVC   OPTEREC,PVALPEND    ELSE TAKE COMPRESSED END DATE                
         B     VOPEXIT                                                          
VOPREC4  MVI   OPTEREC,X'FF'       SET MAXIMUM END DATE                         
         MVC   OPTEREC+1(L'OPTEREC-1),OPTEREC                                   
         B     VOPEXIT                                                          
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
VOPCLR   OC    OPTCLDR,OPTCLDR     BANK CLEARED DATE RANGE                      
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPCLR2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSCLR,PVALPSTA    ELSE TAKE COMPRESSED START DATE              
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPCLR4             IF SO, ASSUME END DATE IS TODAY              
VOPCLR2  MVC   OPTECLR,PVALPEND    ELSE TAKE COMPRESSED END DATE                
         B     VOPEXIT                                                          
VOPCLR4  MVI   OPTECLR,X'FF'       SET MAXIMUM END DATE                         
         MVC   OPTECLR+1(L'OPTECLR-1),OPTECLR                                   
         B     VOPEXIT                                                          
         DROP  R1                                                               
         EJECT                                                                  
*MN                                                                             
*&&US                                                                           
VOPINV   OC    OPTINVR,OPTINVR                                                  
         BNZ   ERRKDUP                                                          
         LA    R1,SCANTXT2                                                      
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BNE   VOPINV2                                                          
         CLI   0(RF),C' '                                                       
         BH    VOPINV6                                                          
         B     ERRSHRT                                                          
                                                                                
VOPINV2  CLI   0(RF),C' '                                                       
         BE    VOPINV4                                                          
         CLI   0(RF),C'-'                                                       
         BE    VOPINV4                                                          
         LA    RF,1(RF)                                                         
         B     VOPINV2                                                          
                                                                                
VOPINV4  SR    RF,R1                                                            
         CLM   RF,1,=AL1(L'OPTSINV)                                             
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         STC   RF,OPTSINXL                                                      
         EX    RF,*+4                                                           
         MVC   OPTSINV(0),0(R1)                                                 
         LA    R1,1(RF,R1)                                                      
         LA    RF,1(R1)                                                         
         CLI   0(R1),C'-'          TEST START MEDIA INV# ONLY                   
         BNE   VOPINV8             YES - THAT WILL DO                           
         CLI   0(RF),C' '          TEST END MEDIA INV# INPUT                    
         BH    VOPINV6             YES EXTRACT IT                               
         MVI   OPTEINV,C'9'        NO - SET MAXIMUM END MEDIA INV#              
         MVC   OPTEINV+1(L'OPTEINV-1),OPTEINV                                   
         MVI   OPTEINXL,L'OPTEINV-1 SET EXECUTE LENGTH                          
         B     VOPINV8             AND THAT WILL DO                             
                                                                                
VOPINV6  LR    R1,RF                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *-8                                                              
         SR    RF,R1                                                            
         CLM   RF,1,=AL1(L'OPTEINV)                                             
         BH    ERRLONG                                                          
         BCTR  RF,0                                                             
         STC   RF,OPTEINXL                                                      
         EX    RF,*+4                                                           
         MVC   OPTEINV(0),0(R1)                                                 
                                                                                
VOPINV8  OC    OPTEINV,OPTEINV     TEST END MEDIA INV#                          
         BZ    VOPEXIT                                                          
         CLC   OPTSINV,OPTEINV     YES - CHECK START DOESN'T EXCEED END         
         BH    ERRRANGE                                                         
         B     VOPEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* VALID INPUT FORMATS FOR PUBLICATION OPTION ARE:                     *         
* PUB=PUB                                                             *         
* PUB=PUB/ALL                                                         *         
* PUB=PUB/ZONE                                                        *         
* PUB=PUB/ZONE/EDITION                                                *         
* PUB=PUB//EDITION                                                    *         
* IF ZONE IS ALL, PUBLICATION IS EXTRACTED, ZONE IS SET TO ZZZ AND    *         
* PUBVAL CALL IS OMITTED                                              *         
* IF ZONE IS OMITTED, THE SECOND SLASH IS SQUEEZED OUT                *         
* SLASHES ARE REPLACED WITH COMMAS BEFORE THE PUBVAL CALL             *         
***********************************************************************         
                                                                                
VOPPUB   OC    OPTPUB,OPTPUB       TEST PUBLICATION(ZONE)(EDITION) SET          
         BNZ   ERRKDUP                                                          
         MVC   OPTPUB,SPACES                                                    
         XR    R1,R1                                                            
         IC    R1,SCANLEN2                                                      
         LA    RF,SCANTXT2                                                      
         CLI   0(RF),C'/'          FIND FIRST SEPARATOR                         
         BE    VOPPUB2             ZONE FOLLOWS                                 
         LA    RF,1(RF)                                                         
         BCT   R1,*-12                                                          
         IC    R1,SCANLEN2         NO ZONE/EDITION                              
         BCTR  R1,0                                                             
         STC   R1,OPTPUBXL         SET EXECUTE L'PUBLICATION                    
         B     VOPPUB8             PUBLICATION                                  
                                                                                
VOPPUB2  CLC   OP3ALL,1(RF)        TEST ALL ZONES/EDITIONS                      
         BNE   VOPPUB4                                                          
         BCTR  RF,0                POINT TO LAST BYTE OF PUBLICATION            
         LA    R1,SCANTXT2                                                      
         SR    RF,R1                                                            
         EX    RF,*+4                                                           
         MVC   OPTPUB(0),SCANTXT2  EXTRACT PUBLICATION                          
         MVC   OPTPUB+L'OPTPUB-L'ZEDS(L'ZEDS),ZEDS                              
         MVI   OPTPUBXL,L'OPTPUB-1                                              
         B     VOPPUBX                                                          
                                                                                
VOPPUB4  MVI   0(RF),C','                                                       
         CLI   1(RF),C'/'          TEST ZONE OMITTED                            
         BNE   VOPPUB6                                                          
         LA    R1,SCANTXT2                                                      
         LR    RE,RF                                                            
         SR    RE,R1                                                            
         LA    R1,L'SCANTXT2-1                                                  
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),2(RF)       SQUEEZE OUT SECOND SLASH                     
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         STC   R1,SCANLEN2         ADJUST LENGTH FOR PUBVAL                     
         MVI   OPTPUBXL,L'OPTPUB-1                                              
         B     VOPPUB8             PUBLICATION,EDITION                          
                                                                                
VOPPUB6  LA    RF,1(RF)                                                         
         CLI   0(RF),C'/'                                                       
         BE    *+12                                                             
         BCT   R1,*-12                                                          
         B     VOPPUB8             PUBLICATION,ZONE                             
         MVI   0(RF),C','          PUBLICATION,ZONE,EDITION                     
                                                                                
VOPPUB8  GOTO1 VPUBVAL,DMCB,(SCANLEN2,SCANTXT2),(1,WORK)                        
         CLI   0(R1),X'FF'         TEST ERROR                                   
         BE    ERRNOTV                                                          
         MVC   OPTPUB,WORK                                                      
                                                                                
VOPPUBX  B     VOPEXIT                                                          
                                                                                
VOPMMD   OC    OPTMMDR,OPTMMDR     MEDIA MONTH DATE RANGE                       
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPMMD2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSMM,PVALPSTA     ELSE TAKE PWOS START DATE                    
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPMMD4             IF SO, ASSUME END DATE IS TODAY              
VOPMMD2  MVC   OPTEMM,PVALPEND     ELSE TAKE PWOS END DATE                      
         B     VOPEXIT                                                          
VOPMMD4  MVI   OPTEMM,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTEMM+1(L'OPTEMM-1),OPTEMM                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
*&&                                                                             
         EJECT                                                                  
VOPFXR   OC    OPTFXRR,OPTFXRR                                                  
         BNZ   ERRKDUP                                                          
         XR    R1,R1                                                            
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                R1=EXECUTE L'FIELD                           
         LA    RF,SCANTXT2(R1)     RF=A(LAST CHARACTER)                         
         CLI   SCANTXT2,C'-'       TEST -END                                    
         BE    *+12                                                             
         CLI   0(RF),C'-'          TEST START-                                  
         BNE   VOPFXR04                                                         
                                                                                
         CLM   R1,1,=AL1(L'OPTSFR) REFERENCE MUST BE <=6 CHARS                  
         BH    ERRLONG                                                          
         CLI   SCANLEN2,2          REFERENCE MUST BE >=1 CHAR                   
         BL    ERRSHRT                                                          
         BCTR  R1,0                DROP THE HYPHEN                              
         CLI   SCANTXT2,C'-'       TEST -END                                    
         BE    VOPFXR02                                                         
         STC   R1,OPTSFRXL         SET START REFERENCE                          
         EX    R1,*+4                                                           
         MVC   OPTSFR(0),SCANTXT2                                               
         MVI   OPTEFR,C'9'         SET MAXIMUM END REF                          
         MVC   OPTEFR+1(L'OPTEFR-1),OPTEFR                                      
         MVI   OPTEFRXL,L'OPTEFR-1 SET EXECUTE LENGTH                           
         B     VOPEXIT             AND THAT WILL DO                             
                                                                                
VOPFXR02 STC   R1,OPTEFRXL         SET END REFERENCE                            
         EX    R1,*+8                                                           
         B     VOPEXIT                                                          
         MVC   OPTEFR(0),SCANTXT2+1                                             
                                                                                
VOPFXR04 LTR   R1,R1               TEST MORE THAN ONE CHARACTER                 
         BZ    VOPFXR06                                                         
         CLI   0(RF),C'-'          SEARCH FOR A HYPHEN                          
         BE    VOPFXR08                                                         
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         CLI   SCANLEN2,L'OPTSFR   REFERENCE MUST BE <=6 CHARS                  
         BH    ERRLONG                                                          
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
VOPFXR06 STC   R1,OPTSFRXL         SET START REFERENCE                          
         EX    R1,*+8                                                           
         B     VOPEXIT                                                          
         MVC   OPTSFR(0),SCANTXT2                                               
                                                                                
VOPFXR08 XR    RE,RE               EXTRACT BOTH SIDES                           
         IC    RE,SCANLEN2                                                      
         BCTR  RE,0                                                             
         SR    RE,R1                                                            
         CLM   R1,1,=AL1(L'OPTSFR)                                              
         BH    ERRLONG                                                          
         CLM   RE,1,=AL1(L'OPTEFR)                                              
         BH    ERRLONG                                                          
         BCTR  R1,0                                                             
         STC   R1,OPTSFRXL                                                      
         EX    R1,*+4                                                           
         MVC   OPTSFR(0),SCANTXT2                                               
         BCTR  RE,0                                                             
         STC   RE,OPTEFRXL                                                      
         EX    RE,*+4                                                           
         MVC   OPTEFR(0),1(RF)                                                  
         CLC   OPTSFR,OPTEFR       YES - CHECK START DOESN'T EXCEED END         
         BH    ERRRANGE                                                         
         B     VOPEXIT                                                          
                                                                                
VOPDUE   OC    OPTDUER,OPTDUER     DUE DATES (COMPRESSED)                       
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPDUE2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSDU,PVALCSTA     ELSE TAKE START DATE                         
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPDUE4             IF SO, ASSUME END DATE IS TODAY              
VOPDUE2  MVC   OPTEDU,PVALCEND     ELSE TAKE END DATE                           
         B     VOPEXIT                                                          
VOPDUE4  MVI   OPTEDU,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTEDU+1(L'OPTEDU-1),OPTEDU                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
                                                                                
                                                                                
*&&UK                                                                           
VOPCUR   OC    OPTCUR,OPTCUR       CURRENCY                                     
         BNZ   ERRKDUP                                                          
         CLI   SCANLEN2,1          TEST SINGLE CHARACTER                        
         BNE   VOPCUR02                                                         
         CLI   SCANTXT2,C'*'       TEST ALL NON-COMPANY CURRENCIES              
         BNE   ERRNOTV                                                          
         MVI   OPTCUR,OPTCUALL     SET ALL NON-COMPANY CURRENCIES               
         B     VOPCUR10                                                         
                                                                                
VOPCUR02 LA    R0,CURRTABN         FIND CURRENCY IN OUR TABLE                   
         L     R1,ACURRTAB                                                      
         USING CURTABD,R1                                                       
         CLC   CURTCUR,SCANTXT2                                                 
         BE    VOPCUR04                                                         
         LA    R1,L'CURRTAB(R1)                                                 
         BCT   R0,*-14                                                          
         DROP  R1                                                               
         GOTO1 VBLDCUR,DMCB,SCANTXT2,WORK,ACOM                                  
         CLI   0(R1),0                                                          
         BNE   ERRNOTV             THIS IS NOT A VALID CURRENCY                 
VOPCUR04 MVC   OPTCUR,SCANTXT2                                                  
                                                                                
VOPCUR10 B     VOPEXIT                                                          
                                                                                
VOPCR    OC    OPTCR,OPTCR         CREDIT                                       
         BNZ   ERRKDUP                                                          
         XR    R1,R1                                                            
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         BM    ERRNOTV                                                          
                                                                                
         EX    R1,*+18             TEST YES                                     
         BNE   *+20                                                             
         MVC   OPTCR,AC@YES                                                     
         B     VOPEXIT                                                          
         CLC   SCANTXT2(0),AC@YES                                               
                                                                                
         EX    R1,*+18             TEST NO                                      
         BNE   *+20                                                             
         MVC   OPTCR,AC@NO                                                      
         B     VOPEXIT                                                          
         CLC   SCANTXT2(0),AC@NO                                                
                                                                                
         EX    R1,*+18             TEST ONLY                                    
         BNE   *+20                                                             
         MVC   OPTCR,AC@ONLY                                                    
         B     VOPEXIT                                                          
         CLC   SCANTXT2(0),AC@ONLY                                              
                                                                                
         B     ERRNOTV                                                          
*&&                                                                             
VOPEPD   OC    OPTEPDR,OPTEPDR     EARLIEST PAYMENT DATES (PWOS)                
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPEPD2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTSEPD,PVALPSTA    ELSE TAKE START DATE                         
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPEPD4             IF SO, ASSUME END DATE IS TODAY              
VOPEPD2  MVC   OPTEEPD,PVALPEND    ELSE TAKE END DATE                           
         B     VOPEXIT                                                          
VOPEPD4  MVI   OPTEEPD,X'FF'       SET MAXIMUM END DATE                         
         MVC   OPTEEPD+1(L'OPTEEPD-1),OPTEEPD                                   
         B     VOPEXIT                                                          
         DROP  R1                                                               
*&&UK                                                                           
VOPSCU   OC    OPTDIND,OPTDIND     TEST DUPLICATE                               
         BNZ   ERRKDUP                                                          
         OC    SCNDCURT,SCNDCURT   TEST SECONDARY CURRENCY IN USE               
         BZ    ERRNOTV                                                          
         SR    R1,R1                                                            
         IC    R1,SCANLEN2                                                      
         BCTR  R1,0                                                             
         BM    ERRNOTV                                                          
                                                                                
         EX    R1,*+8              TEST INSERT                                  
         BNE   VOPSCU02                                                         
         CLC   SCANTXT2(0),AC@INSRT                                             
         MVI   OPTDIND,OPTDISIQ                                                 
         B     VOPEXIT                                                          
                                                                                
VOPSCU02 EX    R1,*+8              TEST REPLACE                                 
         BNE   ERRNOTV                                                          
         CLC   SCANTXT2(0),AC@RPLC                                              
         MVI   OPTDIND,OPTDISRQ                                                 
         B     VOPEXIT                                                          
*&&                                                                             
                                                                                
VOPUDA   OC    OPTUDAR,OPTUDAR     USED/OFFSET DATES (COMPRESSED)               
         BNZ   ERRKDUP                                                          
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'                                                      
         BNZ   ERRINVDT                                                         
         LA    R1,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST NO START DATE INPUT                     
         BO    VOPUDA2             IF SO, TAKE END DATE ONLY                    
         MVC   OPTUDS,PVALCSTA     ELSE TAKE START DATE                         
         XR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VOPUDA4             IF SO, ASSUME END DATE IS TODAY              
VOPUDA2  MVC   OPTUDE,PVALCEND     ELSE TAKE END DATE                           
         B     VOPEXIT                                                          
VOPUDA4  MVI   OPTUDE,X'FF'        SET MAXIMUM END DATE                         
         MVC   OPTUDE+1(L'OPTUDE-1),OPTUDE                                      
         B     VOPEXIT                                                          
         DROP  R1                                                               
*&&DO                                                                           
VOPLREF  OC    OPTLREF,OPTLREF     LONG INVOICE?                                
         BNZ   ERRKDUP                                                          
         ZIC   R1,SCANLEN2         LENGTH OF INPUT                              
         LA    R0,1                MUST BE BETWEEN 1 AND 20                     
         CR    R1,R0                                                            
         BL    ERRSHRT                                                          
         LA    R0,20                                                            
         CR    R1,R0                                                            
         BH    ERRLONG                                                          
         MVC   OPTLREF,SPACES      SPACE OUT,                                   
         STC   R1,OPTLREFL         STORE LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   OPTLREF(0),SCANTXT2 AND SAVE OPTION VALUE                        
         B     VOPEXIT                                                          
                                                                                
*&&                                                                             
VOPEXIT  B     VALOPT60            RETURN TO MAIN ROUTINE                       
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD HEADER SCREEN/INTERACT WITH OVERLAY                            *         
***********************************************************************         
GO       MVI   FVINDX,0                                                         
         TM    TWAMODE2,TWAM2HED                                                
         BO    GO4                                                              
         NI    TWAMODE,TWAMRSRV                                                 
         OI    TWAMODE2,TWAM2HED                                                
         IC    R1,XACTSCR1                                                      
         GOTO1 AOVRSCR                                                          
         TM    XACTINDS,ACTIPRVL   TEST OVERLAY PRE-VALIDATES HEADER            
         BNO   GO2                                                              
         OC    ACCOUNT,ACCOUNT     TEST ACCOUNT HAS BEEN CLEARED                
         BZ    *+10                                                             
         MVC   LACCOUNT,ACCOUNT    SET LAST MAIN ACCOUNT                        
         XR    R0,R0                                                            
         IC    R0,XACTOLAY                                                      
         GOTO1 VCALLOV,DMCB,((R0),0),0,0                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OVERLAY,0(R1)       SAVE A(OVERLAY ENTRY POINT)                  
         MVI   BYTE,ACTIPRVL       SET TO PRE-VALIDATE                          
         GOTO1 OVERLAY,WORKD                                                    
GO2      MVI   FVOMTYP,GTMINF      SET MESSAGE AND EXIT                         
         MVC   FVMSGNO,=AL2(IAENTHDD)                                           
         B     FVERR                                                            
                                                                                
GO4      TM    XACTINDS,ACTICFRM   IF ACTION REQUIRES CONFIRMATION              
         BNO   GOLAY                                                            
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+12                                                             
         TM    TWAMODE2,TWAM2CHG   TEST ANY CHANGES                             
         BNO   GOLAY               NO - REGULAR OVERLAY CALL                    
         CLI   XACTION,ACTUPDT     TEST UPDATE                                  
         BNE   GO10                NO - TEST QUIT CONFIRMATION                  
         TM    SACTINDS,ACTICHUP   TEST CHECK BEFORE UPDATE                     
         BNO   GO6                 NO - TEST REPORT REQUIRED                    
         XR    R0,R0                                                            
         IC    R0,XACTOLAY                                                      
         GOTO1 VCALLOV,DMCB,((R0),0),0,0                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OVERLAY,0(R1)       SAVE A(OVERLAY ENTRY POINT)                  
         MVI   BYTE,ACTICHUP       CHECK WE'RE OK TO UPDATE                     
         GOTO1 OVERLAY,WORKD                                                    
         BNE   GO8                                                              
GO6      CLI   PROFREPT,C'Y'       TEST REPORT REQUIRED ON UPDATE               
         BNE   GO10                                                             
         OC    PRTSUB,PRTSUB       TEST REPORT REQUESTED                        
         BNZ   GO10                                                             
         LA    R1,MRKREPH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
GO8      MVC   MRKACT,SACTNAME     NOT OK - RESTORE ACTION NAME                 
         B     GO19                AND EXIT WITH MESSAGE SET BY OVERLAY         
                                                                                
GO10     CLI   PROFCFRM,C'N'       TEST ANY CONFIRMATION REQUIRED               
         BE    GOLAY               NO - CALL OVERLAY                            
         CLI   XACTION,ACTQUIT     TEST THIS IS QUIT                            
         BNE   GO12                                                             
         CLI   PROFCFRM,C'U'       QUIT - TEST CONFIRM UPDATE ONLY              
         BE    GOLAY                                                            
         B     CONFIRM                                                          
GO12     CLI   PROFCFRM,C'Q'       UPDATE - TEST CONFIRM QUIT ONLY              
         BE    GOLAY                                                            
         TM    TWAMODE3,TWAM3CFM   HAS CONFIRMATION ALREADY HAPPENED            
         BNZ   GOLAY               IF SO GO TO OVERLAY                          
         B     CONFIRM                                                          
*                                                                               
GOLAY    XR    R0,R0                                                            
         IC    R0,XACTOLAY                                                      
         GOTO1 VCALLOV,DMCB,((R0),0),0,0                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OVERLAY,0(R1)       SAVE A(OVERLAY ENTRY POINT)                  
         MVI   BYTE,0              STANDARD OVERLAY CALL                        
         ST    RD,SAVERD           SAVE RD PRIOR TO OLAY ENTRY                  
         GOTO1 OVERLAY,WORKD                                                    
*                                                                               
         CLI   XACTION,ACTUPDT     TEST UPDATE                                  
         BNE   GO14                NO - TEST QUIT CONFIRMATION                  
         TM    TWAMODE3,TWAM3WRN   HAVE WE SET WARNING                          
         BNZ   GO18                YES                                          
         TM    TWAMODE3,TWAM3WRM   HAVE WE SET WARNING                          
         BNZ   GO18                YES                                          
*                                                                               
GO14     TM    XACTINDS,ACTICFRM+ACTIXSET  CONFIRMED/INTERIM ACTION?            
         BNZ   GO15                                                             
         TM    PCDRIVEN,PCGRIDQ+PCGRMSG2                                        
         BO    FVERRX                                                           
         B     FVERR                                                            
*                                                                               
GO15     MVC   MRKACT,SACTNAME     RESTORE ACTION NAME                          
         TM    XACTINDS,ACTICFRM   CONFIRMED ACTION?                            
         BNO   GO18                NOT UPDATE/QUIT                              
         CLI   XACTION,ACTQUIT     TEST QUITTING                                
         BE    GO17                                                             
         CLI   FVOMTYP,GTMINF      TEST OVERLAY SET INFO MESSAGE                
         BNE   GO18                NO - BAD RETURN, RESTORE TOTALS              
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    GO17                NO                                           
         TM    TWAMODE3,TWAM3UPD   HAS THE UPDATE COMPLETED                     
         BZ    EXIT                NO - EXIT AND WAIT FOR PC RESPONSE           
         NI    TWAMODE3,X'FF'-(TWAM3UPD+TWAM3CFM+TWAM3INT)                      
*                                                                               
*O16     NI    TWAMODE,255-TWAMDOIT                                             
GO17     NI    TWAMODE,TWAMRSRV    ELSE SET TO RE-INITIALISE                    
         NI    TWAMODE3,X'FF'-(TWAM3WRN+TWAM3WRM)                               
         B     RESHEAD             RESTORE HEADER SCREEN                        
*                                                                               
GO18     TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    GO19                NO                                           
         TM    TWAMODE3,TWAM3UPD   HAS THE UPDATE COMPLETED                     
         BZ    FVERRX2             NO - EXIT AND WAIT FOR PC RESPONSE           
         NI    TWAMODE3,X'FF'-(TWAM3UPD+TWAM3CFM+TWAM3INT)                      
*                                                                               
GO19     MVC   XACTNAME,SACTNAME   RESTORE NAME                                 
         MVC   XACTION,SACTION     RESTORE NUMBER                               
         MVC   XACTINDS,SACTINDS   RESTORE INDICATORS                           
         MVC   XACTIND2,SACTIND2   RESTORE INDICATORS TWO                       
         L     R2,ADISTOTS                                                      
         GOTO1 ABLDTOT,(R2)        RESTORE TOTALS, TRANSMIT SCREEN              
         B     FVERR               EXIT WITH MESSAGE SET BY OVERLAY             
         EJECT                                                                  
***********************************************************************         
* RESTORE HEADER SCREEN SAVED BY OVERLAY                              *         
***********************************************************************         
                                                                                
RESHEAD  GOTO1 VDATAMGR,DMCB,=CL8'DMREAD',=CL8'TEMPSTR',(1,0),ATIA              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,MRKOLAYH                                                      
         LA    R1,SAVHEADL         MRKOLAY->TWAD+3072 SAVED                     
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE TIA SCREEN INTO TWA                     
         NI    MRKTYPH+(FVATRB-FVIHDR),255-FVAPROT                              
         NI    MRKACTH+(FVATRB-FVIHDR),255-FVAPROT                              
*&&US                                                                           
         CLI   XACTION,ACTUPDT     TEST UPDATE                                  
         BNE   RESHEA02                                                         
         XC    MRKOPT,MRKOPT       CLEAR OPTIONS                                
         MVI   MRKOPTH+(FVILEN-FVIHDR),0                                        
*MN      XC    OPTIONS(OPTIONSL),OPTIONS                                        
         LA    RE,OPTIONS          SET MAXIMUM END DATE                         
         LA    RF,OPTIONSL                                                      
         XCEF                                                                   
*MN                                                                             
         XC    SOPTDIS,SOPTDIS     CLEAR SAVED CHARACTER DISPLAY OPTION         
*&&                                                                             
RESHEA02 LA    R1,MRKMSGH          RE-TRANSMIT HEADER SCREEN                    
         XR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,0(R1)                                                       
         BZ    *+8                                                              
         BXLE  R1,RE,*-12                                                       
         MVI   1(R1),1             SET INDICS                                   
         MVI   2(R1),1                                                          
         MVC   TWASCROV,XACTSCR1                                                
         LA    R0,1                ASSUME FIRST UNPROTECTED FIELD               
         TM    XTYPINDS,TYPIC2ND                                                
         BZ    *+8                                                              
         LA    R0,2                SECOND UNPROTECTED FIELD                     
         TM    XTYPINDS,TYPIC3RD                                                
         BZ    *+8                                                              
         LA    R0,3                THIRD UNPROTECTED FIELD                      
         LA    R1,MRKOLAYH                                                      
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R1)                                             
         AR    R1,RF                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *-10                                                             
         BCT   R0,*-14                                                          
         ST    R1,FVADDR                                                        
         TM    SACTINDS,ACTIPRVL   TEST REAL ACTION TO BE PRE-VALIDATED         
         BNO   FVERR                                                            
         XC    LACCOUNT,LACCOUNT   CLEAR LAST MAIN ACCOUNT                      
         MVI   BYTE,ACTIPRVL       SET TO PRE-VALIDATE                          
         GOTO1 OVERLAY,WORKD       CALL THE OVERLAY                             
         B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* ACTION CONFIRMATION ROUTINE                                         *         
***********************************************************************         
                                                                                
CONFIRM  ST    RE,FULL              SAVE A(RETURN)                              
         L     R2,ADISTOTS          A(LOAD POINT) FOR CONFIRM SCREEN            
         USING TWACFRMD,R2                                                      
         TM    TWAMODE,TWAMCFRM     IF SCREEN ALREADY LOADED                    
         BNZ   CONFM10              CHECK USER RESPONSE                         
         TM    PCDRIVEN,PCGRIDQ     TEST WE ARE RUNNING UNDER GRIDS             
         BO    CONFM02              YES                                         
         MVC   DMCB+4(3),=X'D90616' ELSE LOAD IT                                
         MVI   DMCB+7,TWASCRCA                                                  
         GOTO1 VCALLOV,DMCB,(0,TWACFRMD)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,MRKMSGH                                                       
         XR    RE,RE                                                            
         LA    RF,TWACFRMD                                                      
         BCTR  RF,0                                                             
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,*-12                                                       
                                                                                
         LA    R1,PARM                                                          
         USING GETTXTD,R1          GET CONFIRMATION MESSAGE                     
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(SAENTCMA)                                           
         MVI   GTMTYP,GTMSCR                                                    
         XC    FVXTRA,FVXTRA                                                    
         MVI   FVXTRA+0,2          &1=CONFIRMATION CHARACTER                    
         MVC   FVXTRA+1(1),AC@YES                                               
         MVI   FVXTRA+2,ACTADLQ+1  &2=ACTION NAME                               
         MVC   FVXTRA+3(ACTADLQ),XACTNAME                                       
         LA    R0,FVXTRA                                                        
         STCM  R0,7,GTASUBST                                                    
         LA    R0,MKAMSGH                                                       
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT,GETTXTD                                                  
         OI    MKAINPH+(FVOIND-FVIHDR),FVOCUR                                   
         LA    R0,MKAINPH                                                       
         ST    R0,FVADDR                                                        
         OI    TWAMODE,TWAMCFRM                                                 
         B     EXIT                                                             
*                                                                               
CONFM02  LA    R2,GRDDAT1H         CLEAR SCREEN AND UNPROTECT FI                
         USING FLDHDRD,R2                                                       
         LA    RF,GRDDATLH         LAST FIELD ON SCREEN                         
         SR    RE,RE                                                            
         SR    R1,R1                                                            
CONFM04  IC    RE,FLDLEN                                                        
         LR    R1,RE                                                            
         SH    R1,=Y(FLDDATA-FLDLEN) R1=INPUT DATA LENGTH                       
         BZ    CONFM06                                                          
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    FLDDATA(0),FLDDATA  CLEAR DATA IN FEILDS                         
         NI    FLDATB,X'FF'-FATBPROT  REMOVE PROTECTION                         
         BXLE  R2,RE,CONFM04                                                    
*                                                                               
CONFM06  LA    R2,MRKMSGH                                                       
*        NI    FLDATB,X'FF'-FATBPROT  REMOVE PROTECTION                         
         XC    MRKMSG,MRKMSG                                                    
         MVC   MRKMSG(16),=C'<UNPROTECT=ROW4>'                                  
         MVC   MRKMSG+16(14),=C'<CONFIRM QUIT>'                                 
         CLI   XACTION,ACTQUIT     TEST ACTION IS QUIT                          
         BE    CONFM08                                                          
         MVC   MRKMSG+16(16),=C'<CONFIRM UPDATE>'                               
CONFM08  OI    TWAMODE,TWAMCFRM                                                 
         B     EXIT                                                             
                                                                                
         USING TWACFRMD,R2                                                      
CONFM10  NI    TWAMODE,255-TWAMCFRM                                             
         TM    PCDRIVEN,PCGRIDQ    TEST WE ARE RUNNING UNDER GRIDS              
         BZ    CONFM14             NO                                           
         CLC   GRDDAT1(1),AC@YES                                                
         BNE   CONFM18                                                          
         CLI   XACTION,ACTQUIT     TEST ACTION IS QUIT                          
         BE    CONFM16                                                          
         OI    TWAMODE3,TWAM3CFM   CONFIRMATION IS DONE - USED BY GRIDS         
         B     CONFM16                                                          
CONFM14  CLC   MKAINP(1),AC@YES    TEST ACTION CONFIRMED                        
         BNE   CONFM18                                                          
*        OI    TWAMODE,TWAMDOIT    USER SAYS DO IT                              
CONFM16  L     RE,FULL             RE=A(RETURN)                                 
         BR    RE                  RETURN TO ACTION VALIDATION                  
                                                                                
CONFM18  MVC   MRKACT,SACTNAME     ELSE RESET SCREEN ACTION NAME                
         MVC   XACTNAME,SACTNAME   RESTORE NAME                                 
         MVC   XACTION,SACTION     RESTORE NUMBER                               
         MVC   XACTINDS,SACTINDS   RESTORE INDICATORS                           
         MVC   XACTIND2,SACTIND2   RESTORE INDICATORS TWO                       
                                                                                
         GOTO1 ABLDTOT,(R2)        RESTORE TOTALS, TRANSMIT SCREEN              
                                                                                
CONFM20  LA    R1,MRKSCRH                                                       
         TM    PCDRIVEN,PCGRIDQ    TEST WE ARE RUNNING UNDER GRIDS              
         BZ    CONFM22             YES                                          
         LA    R1,MRKOPTH                                                       
CONFM22  ST    R1,FVADDR                                                        
         MVI   FVOMTYP,GTMINF      SET MESSAGE AND EXIT                         
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         B     FVERR                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON ERROR EXITS                                                  *         
***********************************************************************         
                                                                                
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
ERRSECLO MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     FVERR                                                            
ERROFFN2 MVC   FVMSGNO,=AL2(AE$OFFN2)                                           
         B     FVERR                                                            
ERRRANGE MVC   FVMSGNO,=AL2(EARNGINV)                                           
         B     FVERR                                                            
ERRBMECM MVC   FVMSGNO,=AL2(AE$BMECM)                                           
ERRMSGON B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE, FIELD INDEX INFO & EXTRA MESSAGE   *         
* NTRY - FVADDR=A(FIELD HEADER OF FIELD IN ERROR)                     *         
*        FVMSGNO=FIELD ERROR NUMBER                                   *         
*        FVFLAG=ZERO IF A STANDARD CONTROLLER ERROR MESSAGE REQUIRED  *         
*        FVOSYS=OVERRIDE SYSTEM FOR GETTXT CALL (ZERO=STANDARD)       *         
*        FVINDX=MULTIPLE FIELD INDEX NUMBER                           *         
*        FVSUBX=MULTIPLE FIELD SUB-INDEX NUMBER                       *         
*        FVXTRA=USER SUPPLIED MESSAGE TO TACK ONTO GENERAL MESSAGE    *         
* NTR AT FVERR  TO SET MULTIPLE FIELD INDEX VALUES TO ZERO            *         
*        FVERRX ONLY TO SET CURSOR TO FIELD ADDRESSED BY FVADDR       *         
***********************************************************************         
                                                                                
FVERR    TM    INDS1,INDSVOPT      TEST OPTION VALIDATION IN PROGRESS           
         BZ    FVERR05                                                          
*MN      MVC   OPTIONS(OPTIONSL),TEMP  RESTORE SAVED OPTIONS                    
         LA    R0,TEMP                                                          
         LA    R1,L'TEMP                                                        
         LA    RE,OPTIONS                                                       
         LA    RF,OPTIONSL                                                      
         MVCL  RE,R0               MOVE TIA SCREEN INTO TWA                     
*MN                                                                             
FVERR05  LA    R2,PARM             DEFINE GETTXT CONTROL BLOCK                  
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
         BNH   FVERR20                                                          
         LA    R1,FVXTRA                                                        
         STCM  R1,7,GTATXT                                                      
         LA    RF,L'FVXTRA-1(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    RF,1(RF)                                                         
         STC   RF,GTLTXT                                                        
                                                                                
FVERR20  LA    R2,PARM             PARM DEFINED INTERNALLY                      
         CLI   GTMSGNO,X'FF'       CHECK FOR GENERAL MESSAGES                   
         BNE   *+12                                                             
         MVI   GTMSYS,X'FF'        FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
         XC    MRKMSG,MRKMSG                                                    
         GOTO1 VGETTXT,GETTXTD                                                  
         DROP  R2                                                               
                                                                                
FVERRX   OI    MRKMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    PCDRIVEN,X'FF'-(PCGRMSG1+PCGRMSG2)                               
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    *+8                                                              
         OI    FVOIND-FVIHDR(R1),FVOCUR                                         
*                                                                               
FVERRX2  L     R1,ATSARBLK         SAVE TSAR BUFFER ON DISK                     
         USING TSARD,R1                                                         
         OC    TSABUF,TSABUF       TEST WE HAVE A TSAR BUFFER                   
         BZ    EXIT                                                             
         MVI   TSACTN,TSASAV       YES - SAVE IT                                
         GOTO1 VTSAR                                                            
         DROP  R1                                                               
*                                                                               
         TM    TWAMODE3,TWAM3UWD                                                
         BZ    EXIT                                                             
         NI    TWAMODE3,FF-TWAM3UWD                                             
         L     R5,AIOCNTL                                                       
         USING IOCNTLD,R5                                                       
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(0,0),TWAD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DC    H'0',C'$ABEND'                                                   
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
GLOBALS  DS    0X                                                               
         DC    C'ACCOUNT '                                                      
         DC    C'ACCDIR  '                                                      
         DC    C'ACCMST  '                                                      
         DC    P'0'                                                             
         DC    P'1'                                                             
         DC    P'-1'                                                            
         DC    XL(L'AFCX)'0'                                                    
         ORG   *-L'AFCX+(AFCXRATE-AFCX)                                         
         DC    PL5'10000'                                                       
         ORG   *-1                                                              
         DC    X'0'                REMOVE SIGN AND MULTIPLY BY 10               
         ORG   ,                                                                
         DC    C'***S'                                                          
         DC    C'GRID NEXTL DATA 15 '                                           
         DC    C'GRID NEXTL DOR 15 DATA 17 WR | EID '                           
         DC    C'GRID NEXTL PR'                                                 
         DC    C'PR'                                                            
GLOBALSL EQU   *-GLOBALS                                                        
                                                                                
ZEDS     DC    C'ZZZ'                                                           
                                                                                
                                                                                
*&&UK                                                                           
CTRYTABL EQU   4                                                                
CTRYTAB  DS    0XL(CTRYTABL)                                                    
       ++INCLUDE ACCURTAB                                                       
CTRYTABN EQU   (*-CTRYTAB)/CTRYTABL                                             
*&&                                                                             
                                                                                
                                                                                
* TRANSLATES ANY CHARACTER IN RANGE X'40'-X'FF', COLUMN INDEXES SHOWN           
*                                                      <  (  +                 
TRANTAB  DC    AL1(00,00,00,00,00,00,00,00,00,00,44,00,49,50,78,00)             
*                   &                             !  $  *  )                    
         DC    AL1(43,00,00,00,00,00,00,00,00,00,40,45,48,39,00,00)             
*                      /                                %                       
         DC    AL1(00,37,00,00,00,00,00,00,00,00,00,00,42,00,00,00)             
*                                                       @  '     "              
         DC    AL1(00,00,00,00,00,00,00,00,00,00,00,00,38,46,00,41)             
         DC    AL1(00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00)             
         DC    AL1(00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00)             
         DC    AL1(00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00)             
         DC    AL1(00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00)             
*                      A  B  C  D  E  F  G  H  I                                
         DC    AL1(00,10,11,12,13,14,15,16,17,18,00,00,00,00,00,00)             
*                      J  K  L  M  N  O  P  Q  R                                
         DC    AL1(00,19,20,21,22,23,24,25,26,27,00,00,00,00,00,00)             
*                  \      S  T  U  V  W  X  Y  Z                                
         DC    AL1(47,00,28,29,30,31,32,33,34,35,00,00,00,00,00,00)             
*                   0  1  2  3  4  5  6  7  8  9                                
         DC    AL1(36,01,02,03,04,05,06,07,08,09,00,00,00,00,00,00)             
                                                                                
                                                                                
OPTTAB   DS    0H                  ** OPTION TABLE (SEE OPTTABD) **             
                                                                                
         DC    S(OP8DSP),S(OP3DSP),S(VOPDSP)                                    
         DC    AL1(OPTI2PT+OPTIGRD),AL1(L'PROFDIS)                              
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8MARKD),S(OP3MARKD),S(OPTMRK)                                
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8HELD),S(OP3HELD),S(OPTHLD)                                  
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALWQ+CAOPEQ+CSOPEQ+CHOPEQ+CMOPEQ)                         
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8SEL),S(OP3SEL),S(OPTSEL)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(CAOPEQ+CSOPEQ+CHOPEQ+COOPEQ+CMOPEQ)                          
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8AUTH),S(OP3AUTH),S(OPTAUT)                                  
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALLQ-CDOPEQ-BROPEQ)                                       
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8NUM),S(OP3NUM),S(OPTMRK)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(WNOPEQ)                                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8CTD),S(OP3CTD),S(OPTCTD)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(COOPEQ)                                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8MAT),S(OP3MAT),S(OPTMAT)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(CCOPEQ)                                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8REC),S(OP3REC),S(OPTREC)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALBQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8REV),S(OP3REV),S(OPTREV)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(GROPEQ)                                                      
         DC    AL1(0)                                                           
*&&US                                                                           
         DC    S(OP8REV),S(OP3REV),S(OPTREV)                                    
         DC    AL1(OPTI1PT+OPTIDDS),AL1(0)                                      
         DC    AL3(WHOPEQ)                                                      
         DC    AL1(0)                                                           
*&&                                                                             
         DC    S(OP8CHGD),S(OP3CHGD),S(OPTCHG)                                  
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8PND),S(OP3PND),S(OPTPND)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(GROPEQ)                                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8DSC),S(OP3DSC),S(OPTDSC)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8VOI),S(OP3VOI),S(OPTVOI)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALCQ+BROPEQ)                                              
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8CMM),S(OP3CMM),S(OPTCMM)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALWQ+GROPEQ)                                              
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8URG),S(OP3URG),S(OPTURG)                                    
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8ALCTD),S(OP3ALCTD),S(OPTALC)                                
         DC    AL1(OPTI1PT+OPTINOT),AL1(0)                                      
         DC    AL3(DISALGQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8DATE),S(OP3DATE),S(VOPDAT)                                  
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8DATAD),S(OP3DATAD),S(VOPADA)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8REF),S(OP3REF),S(VOPREF)                                    
         DC    AL1(OPTI2PT),AL1((2*L'TRNKREF)+1)                                
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(AC8CHEQ),S(AC3CHEQ),S(VOPREF)                                  
         DC    AL1(OPTI2PT),AL1((2*L'TRNKREF)+1)                                
         DC    AL3(CCOPEQ+DISALBQ)                                              
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8MOA),S(OP3MOA),S(VOPMOS)                                    
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8CTR),S(OP3CTR),S(VOPCTR)                                    
         DC    AL1(OPTI2PT),AL1(L'TRNKCULC)                                     
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8SBR),S(OP3SBR),S(VOPSBR)                                    
         DC    AL1(OPTI2PT),AL1((2*L'OTHNUM)+1)                                 
         DC    AL3(DISALLQ-CCOPEQ-BROPEQ)                                       
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8SUP),S(OP3SUP),S(VOPCTR)                                    
         DC    AL1(OPTI2PT),AL1(L'TRNKCULC)                                     
         DC    AL3(DISALWQ+GROPEQ+BVOPEQ+CMOPEQ)                                
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8SRC),S(OP3SRC),S(VOPSRC)                                    
         DC    AL1(OPTI2PT),AL1(L'TRNKCULC)                                     
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8AMT),S(OP3AMT),S(VOPAMT)                                    
         DC    AL1(OPTI2PT),AL1(13)                                             
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8OFF),S(OP3OFF),S(VOPOFF)                                    
         DC    AL1(OPTI2PT),AL1(L'TRNOFFC+1)                                    
         DC    AL3(DISALLQ-DISALWQ)                                             
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8CLI),S(OP3CLI),S(VOPCLI)                                    
         DC    AL1(OPTI2PT),AL1(6)                                              
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8PRO),S(OP3PRO),S(VOPPRO)                                    
         DC    AL1(OPTI2PT),AL1(7)                                              
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8JOB),S(OP3JOB),S(VOPJOB)                                    
         DC    AL1(OPTI2PT),AL1(13)                                             
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0)                                                           
                                                                                
*&&UK*&& DC    S(OP8WRK),S(OP2WRK),S(VOPWRK)                                    
*&&US*&& DC    S(OP8WRK),S(OP3WRK),S(VOPWRK)                                    
         DC    AL1(OPTI2PT),AL1(L'TRNKWORK+1)                                   
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8ALL),S(OP3ALL),S(VOPALL)                                    
         DC    AL1(OPTI2PT),AL1(1)                                              
         DC    AL3(DISALLQ-WNOPEQ-GROPEQ-BVOPEQ-MROPEQ)                         
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8PAG),S(OP3PAG),S(VOPPAG)                                    
         DC    AL1(OPTI2PT+OPTIGRD),AL1(1)                                      
         DC    AL3(DISALLQ-WNOPEQ)                                              
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8TYP),S(OP3TYP),S(VOPBAT)                                    
         DC    AL1(OPTI2PT),AL1(4)                                              
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8BATRF),S(OP3BATRF),S(VOPBRF)                                
         DC    AL1(OPTI2PT),AL1(L'BATREF)                                       
         DC    AL3(BVOPEQ+CMOPEQ+MROPEQ+CBOPEQ)                                 
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8BATM),S(OP3BATM),S(VOPBMO)                                  
         DC    AL1(OPTI2PT),AL1(6)                                              
         DC    AL3(BVOPEQ+CMOPEQ+MROPEQ+CBOPEQ)                                 
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8NAME),S(OP3NAME),S(VOPBNA)                                  
         DC    AL1(OPTI2PT),AL1(15)                                             
         DC    AL3(BVOPEQ+CMOPEQ+MROPEQ+CBOPEQ)                                 
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8STMDT),S(OP3STMDT),S(VOPBSD)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(BROPEQ)                                                      
         DC    AL1(0)                                                           
*&&US                                                                           
         DC    S(OP8INV),S(OP3INV),S(VOPINV)                                    
         DC    AL1(OPTI2PT),AL1((2*L'TSARFINV)+1)                               
         DC    AL3(DISALCQ+BVOPEQ)                                              
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8PUB),S(OP3PUB),S(VOPPUB)                                    
         DC    AL1(OPTI2PT),AL1(20)                                             
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8EST),S(OP3EST),S(VOPJOB)                                    
         DC    AL1(OPTI2PT),AL1(13)                                             
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8MMODT),S(OP3MMODT),S(VOPMMD)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(DISALCQ+GROPEQ)                                              
         DC    AL1(0)                                                           
*&&                                                                             
*&&UK                                                                           
         DC    S(OP8FXDRF),S(OP3FXDRF),S(VOPFXR)                                
         DC    AL1(OPTI2PT),AL1((2*L'MPYNO)+1)                                  
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP3CURRY),S(OP3CURRY),S(VOPCUR)                                
         DC    AL1(OPTI2PT),AL1(L'CURTCUR)                                      
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP2CR),S(OP2CR),S(VOPCR)                                       
         DC    AL1(OPTI2PT),AL1(L'AC@ONLY)                                      
         DC    AL3(GROPEQ)                                                      
         DC    AL1(0)                                                           
*&&                                                                             
         DC    S(OP3DUEDT),S(OP3DUEDT),S(VOPDUE)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(DISALCQ+BVOPEQ)                                              
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP3ERPYD),S(OP3ERPYD),S(VOPEPD)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(DISALCQ+BVOPEQ)                                              
         DC    AL1(0)                                                           
*&&UK                                                                           
         DC    S(OP82CUR),S(OP32CUR),S(VOPSCU)                                  
         DC    AL1(OPTI2PT),AL1(L'AC@INSRT)                                     
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0)                                                           
*&&                                                                             
         DC    S(OP8CTRDD),S(OP3CTRDD),S(VOPUDA)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(COOPEQ+GOOPEQ)                                               
         DC    AL1(0)              CONTRA'D DATE=RANGE                          
         DC    S(OP8UDAT),S(OP3UDAT),S(VOPUDA)                                  
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(COOPEQ+GOOPEQ)                                               
         DC    AL1(0)              OR USED DATE=RANGE                           
                                                                                
*MN                                                                             
         DC    S(OP8RECDT),S(OP3RECDT),S(VOPREC)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(BROPEQ)                                                      
         DC    AL1(0)                                                           
                                                                                
         DC    S(OP8CLRDT),S(OP3CLRDT),S(VOPCLR)                                
         DC    AL1(OPTI2PT),AL1(L'PVALCPER)                                     
         DC    AL3(BROPEQ)                                                      
         DC    AL1(0)                                                           
*&&DO                                                                           
         DC    S(OP8LREF),S(OP3LREF),S(VOPLREF)                                 
         DC    AL1(OPTI2PT),AL1(20)                                             
         DC    AL3(DISALCQ+BVOPEQ+GROPEQ)                                       
         DC    AL1(0)              LONG INVOICE NUMBER                          
                                                                                
*&&                                                                             
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
TABLES   DS    0H                                                               
         DC    Y(KEYTAB-MARKER)                                                 
         DC    Y(DISTAB-MARKER)                                                 
         DC    Y(MIDTAB-MARKER)                                                 
         DC    Y(SUDTAB-MARKER)                                                 
         DC    Y(DCLISTU-MARKER)                                                
         DC    Y(DCLISTL-MARKER)                                                
         DC    Y(TYPTAB-MARKER)                                                 
         DC    Y(APFTAB-MARKER)                                                 
         DC    Y(ACTTABS-MARKER)                                                
         DC    Y(SPFTAB-MARKER)                                                 
         DC    Y(SCRTAB-MARKER)                                                 
         DC    Y(OPTTAB-MARKER)                                                 
         DC    Y(PROFDFT-MARKER)                                                
         DC    Y(REPSPEC-MARKER)                                                
         DC    Y(ZPFTAB-MARKER)                                                 
         DC    Y(IOCNTL-MARKER)                                                 
TABLESN  EQU   (*-TABLES)/L'TABLES                                              
         EJECT                                                                  
* TABLE DISPLACEMENTS                                                           
                                                                                
IOCNTL   DS    0H                  AREA LABELLED BY IOCNTLD DSECT               
         DC    C'ACCFIL  '         DATAMGR LITERALS                             
         DC    C'ACCDIR  '                                                      
         DC    C'ACCMST  '                                                      
         DC    C'ACCARC  '                                                      
         DC    C'TEMPSTR '                                                      
                                                                                
* I/O COMMANDS FOR ACCDIR/ACCMST/ACCARC FILES WITH ACCFIL EQUIVALENTS           
                                                                                
*                ACCDIR     ACCFIL      INDS                                    
                                                                                
ISCMNDS  DS    0CL16                                                            
         DC    C'DMRDHI  ',XL8'00'                                              
         DC    C'DMREAD  ',XL8'00'                                              
         DC    C'DMRSEQ  ',XL8'00'                                              
         DC    C'DMADD   ',XL8'00'                                              
         DC    C'DMWRT   ',XL8'00'                                              
                                                                                
*                ACCMST     ACCFIL      INDS                                    
                                                                                
DACMNDS  DS    0CL16                                                            
         DC    C'GETREC  ',XL8'00'                                              
         DC    C'PUTREC  ',XL8'00'                                              
         DC    C'ADDREC  ',XL8'00'                                              
         DC    C'ADFREC  ',XL8'00'                                              
                                                                                
                                                                                
DCLISTU  DS    0X                      **UPPER CASE DICTIONARY**                
         DCDDL AC#ACC,L'OP3ACC                                                  
         DCDDL AC#ALCTD,L'OP3ALCTD                                              
         DCDDL AC#ALL,L'OP3ALL                                                  
         DCDDL AC#AMT,L'OP3AMT                                                  
         DCDDL AC#ATH,L'AC3AUTH                                                 
         DCDDL AC#BAL,L'OP3BAL                                                  
         DCDDL AC#BATM,L'OP3BATM                                                
         DCDDL AC#BATRF,L'OP3BATRF                                              
         DCDDL AC#BNK,L'AC3BNK                                                  
         DCDDL AC#CHGD,L'AC3CHEQ                                                
         DCDDL AC#CHK,L'OP3CHGD                                                 
         DCDDL AC#CLI,L'OP3CLI                                                  
         DCDDL AC#CMN,L'OP3CMM                                                  
         DCDDL AC#CROR,L'AC3CRD                                                 
         DCDDL AC#VNDC,L'AC3VCA                                                 
         DCDDL AC#CTRD,L'OP3CTD                                                 
         DCDDL AC#CTR,L'OP3CTR                                                  
         DCDDL AC#CURRY,L'AC3CURRY                                              
         DCDDL AC#DATAD,L'OP3DATAD                                              
         DCDDL AC#DATE,L'OP3DATE                                                
         DCDDL AC#DISS,L'AC3DISC                                                
         DCDDL AC#DRAFT,L'AC3DRAFT                                              
         DCDDL AC#DSP,L'OP3DSP                                                  
         DCDDL AC#DUEDT,L'OP3DUEDT                                              
         DCDDL AC#EST,L'OP3EST                                                  
         DCDDL AC#FXDRF,L'OP3FXDRF                                              
         DCDDL AC#GEN,L'AC3GEN                                                  
         DCDDL AC#HELD,L'OP3HELD                                                
         DCDDL AC#HOLD,L'AC3HOLD                                                
*&&UK*&& DCDDL AC#INTNL,L'AC3INTNL                                              
         DCDDL AC#INV,L'AC3INV                                                  
         DCDDL AC#JOB,L'OP3JOB                                                  
         DCDDL AC#MARKD,L'OP3MARKD                                              
         DCDDL AC#MCHED,L'OP3MAT                                                
         DCDDL AC#MED,L'AC3MED                                                  
         DCDDL AC#MEMO,L'AC3MEMO                                                
         DCDDL AC#MMODT,L'OP3MMODT                                              
         DCDDL AC#MOA,L'OP3MOA                                                  
         DCDDL AC#NAME,L'OP3NAME                                                
         DCDDL AC#NUM,L'AC3NUMB                                                 
         DCDDL AC#OFF,L'OP3OFF                                                  
         DCDDL AC#OFFST,L'AC3OFFS                                               
         DCDDL AC#PAGE,L'OP3PAG                                                 
         DCDDL AC#PRO,L'OP3PRO                                                  
         DCDDL AC#PUBLN,L'OP3PUB                                                
         DCDDL AC#QUIT,L'AC3QUIT                                                
         DCDDL AC#RCN,L'AC3RECN                                                 
         DCDDL AC#REF,L'OP3REF                                                  
         DCDDL AC#RVRS2,L'AC3REVS                                               
         DCDDL AC#SUBR,L'OP3SBR                                                 
*&&UK*&& DCDDL AC#SEL3,L'AC3SEL                                                 
*&&US*&& DCDDL AC#SEL,L'AC3SEL                                                  
*&&UK*&& DCDDL AC#SEL3,L'AC3SELC                                                
*&&US*&& DCDDL AC#SEL,L'AC3SELC                                                 
         DCDDL AC#SRC,L'OP3SRC                                                  
         DCDDL AC#STMDT,L'OP3STMDT                                              
         DCDDL AC#SUP,L'OP3SUP                                                  
         DCDDL AC#TUNIT,L'AC3TUNIT                                              
*&&UK*&& DCDDL AC#TYPE,L'OP3TYP                                                 
*&&US*&& DCDDL AC#BATTY,L'OP3TYP                                                
         DCDDL AC#UPD,L'AC3UPD                                                  
         DCDDL AC#URG,L'AC3URG                                                  
         DCDDL AC#VOID,L'AC3VOID                                                
         DCDDL AC#WC,L'OP3WRK                                                   
         DCDDL AC#WIP,L'AC3WIP                                                  
         DCDDL AC#RECDT,L'OP3RECDT                                              
         DCDDL AC#CLRDT,L'OP3CLRDT                                              
         DCDDL AC#ACC,L'AC8ACC                                                  
         DCDDL AC#ALCTD,L'OP8ALCTD                                              
         DCDDL AC#ALL,L'OP8ALL                                                  
         DCDDL AC#AMT,L'OP8AMT                                                  
         DCDDL AC#ATH,L'OP8AUTH                                                 
         DCDDL AC#BAL,L'OP8BAL                                                  
         DCDDL AC#BATM,L'OP8BATM                                                
         DCDDL AC#BATRF,L'OP8BATRF                                              
         DCDDL AC#CHGD,L'AC8CHEQ                                                
         DCDDL AC#CHK,L'OP8CHGD                                                 
         DCDDL AC#CLI,L'OP8CLI                                                  
         DCDDL AC#CMN,L'OP8CMM                                                  
         DCDDL AC#CTRD,L'OP8CTD                                                 
         DCDDL AC#CTR,L'OP8CTR                                                  
*&&UK*&& DCDDL AC#DATAD,L'OP8DATAD                                              
*&&US*&& DCDDL AC#DATAD,L'OP8DATAD-4 ADTE IS THE EIGHT BYTE ENTRY               
*&&US*&& DCDDL AC#X40,4              SPACE-FILL OP8DATAD                        
         DCDDL AC#DATE,L'OP8DATE                                                
         DCDDL AC#DISS,L'OP8DSC                                                 
         DCDDL AC#DSP,L'OP8DSP                                                  
         DCDDL AC#EST,L'OP8EST                                                  
         DCDDL AC#FXDRF,L'OP8FXDRF                                              
         DCDDL AC#HELD,L'OP8HELD                                                
         DCDDL AC#INV,L'OP8INV                                                  
         DCDDL AC#JOB,L'OP8JOB                                                  
         DCDDL AC#MARKD,L'OP8MARKD                                              
         DCDDL AC#MCHED,L'OP8MAT                                                
         DCDDL AC#MMODT,L'OP8MMODT                                              
         DCDDL AC#MOA,L'OP8MOA                                                  
         DCDDL AC#NAME,L'OP8NAME                                                
         DCDDL AC#NUM,L'OP8NUM                                                  
         DCDDL AC#OFF,L'OP8OFF                                                  
         DCDDL AC#PAGE,L'OP8PAG                                                 
         DCDDL AC#PENDG,L'OP8PND                                                
         DCDDL AC#PRO,L'OP8PRO                                                  
         DCDDL AC#PUBLN,L'OP8PUB                                                
         DCDDL AC#RCN,L'OP8REC                                                  
         DCDDL AC#REF,L'OP8REF                                                  
         DCDDL AC#RVRS2,L'OP8REV                                                
         DCDDL AC#SUBR,L'OP8SBR                                                 
*&&UK*&& DCDDL AC#SEL3,L'OP8SEL                                                 
*&&US*&& DCDDL AC#APRV,L'OP8SEL                                                 
         DCDDL AC#SRC,L'OP8SRC                                                  
         DCDDL AC#STMDT,L'OP8STMDT                                              
         DCDDL AC#SUP,L'OP8SUP                                                  
         DCDDL AC#TYPE,L'OP8TYP                                                 
*&&DO                                                                           
*&&UK*&& DCDDL AC#TYPE,L'OP8TYP                                                 
*&&US*&& DCDDL AC#BATTY,L'OP8TYP                                                
*&&                                                                             
         DCDDL AC#URG,L'OP8URG                                                  
         DCDDL AC#VOID,L'OP8VOI                                                 
         DCDDL AC#WC,L'OP8WRK                                                   
         DCDDL AC#RECDT,L'OP8RECDT                                              
         DCDDL AC#CLRDT,L'OP8CLRDT                                              
         DCDDL AC#ATH,L'AC10ATH                                                 
         DCDDL AC#BNK,L'AC10BNK                                                 
         DCDDL AC#CHK,L'AC10CHQ                                                 
         DCDDL AC#CROR,L'AC10CRD                                                
         DCDDL AC#VNDC,L'AC10VCA                                                
         DCDDL AC#DISS,L'AC10DSC                                                
         DCDDL AC#DRAFT,L'AC10DRF                                               
         DCDDL AC#GEN,L'AC10GEN                                                 
         DCDDL AC#HOLD,L'AC10HLD                                                
         DCDDL AC#MANU,L'AC10MAN                                                
         DCDDL AC#MED,L'AC10MED                                                 
         DCDDL AC#NUM,L'AC10NUM                                                 
         DCDDL AC#OFFST,L'AC10OFS                                               
         DCDDL AC#QUIT,L'AC10QUI                                                
         DCDDL AC#RCN,L'AC10RCN                                                 
         DCDDL AC#RVRS2,L'AC10REV                                               
*&&UK*&& DCDDL AC#SEL3,L'AC10SEL                                                
*&&US*&& DCDDL AC#APRV,L'AC10SEL                                                
         DCDDL AC#UPD,L'AC10UPD                                                 
         DCDDL AC#VOID,L'AC10VOI                                                
         DCDDL AC#WIP,L'AC10WIP                                                 
         DCDDL AC#ACC,L'AC@ACC                                                  
         DCDDL AC#ALTPF,L'AC@ALTPF                                              
         DCDDL AC#AMT,L'AC@AMT,R                                                
         DCDDL AC#BAT,L'AC@BATCH                                                
         DCDDL AC#BOTH,L'AC@BOTH                                                
         DCDDL AC#CHKC,L'AC@CHKC                                                
         DCDDL AC#CR,L'AC@CR                                                    
         DCDDL AC#CRS,L'AC@CRS                                                  
         DCDDL AC#CTRA,L'AC@CTRA                                                
         DCDDL AC#DR,L'AC@DR                                                    
         DCDDL AC#DRS,L'AC@DRS                                                  
         DCDDL AC#END,L'AC@END                                                  
         DCDDL AC#FACC,L'AC@FACC                                                
         DCDDL AC#FIRST,L'SCR@FRST                                              
         DCDDL AC#GROSS,L'AC4GROSS                                              
         DCDDL AC#HALF,L'SCR@HALF                                               
         DCDDL AC#JOB,L'AC@JOB                                                  
         DCDDL AC#LAST,L'SCR@LAST                                               
         DCDDL AC#MARK,L'AC@MARK                                                
         DCDDL AC#MOS,L'AC@MOS                                                  
         DCDDL AC#MSNG,L'AC@MSNG                                                
         DCDDL AC#NO,L'AC@NO                                                    
         DCDDL AC#NXT,L'AC@NXT                                                  
         DCDDL AC#OFF,L'AC@OFF                                                  
         DCDDL AC#ONLY,L'AC@ONLY                                                
         DCDDL AC#PAGE,L'SCR@PAGE                                               
         DCDDL AC#PENDG,L'AC@PENDG                                              
         DCDDL AC#PFK,L'AC@PFK                                                  
         DCDDL AC#PRINT,L'AC@PRINT                                              
         DCDDL AC#REF,L'AC@REF                                                  
         DCDDL AC#SAVE,L'AC@SAVE                                                
         DCDDL AC#SRC,L'AC@SRC                                                  
         DCDDL AC#SUP,L'AC@SUP                                                  
*&&UK*&& DCDDL AC#TYPE,L'AC@TYPE                                                
*&&US*&& DCDDL AC#BATTY,L'AC@TYPE                                               
         DCDDL AC#WC,L'AC@WC                                                    
         DCDDL AC#WC,L'OP2WRK                                                   
         DCDDL AC#YES,L'AC@YES                                                  
         DCDDL AC#ZOOM,L'AC@ZOOM                                                
         DCDDL AC#ERPYD,L'OP3ERPYD                                              
         DCDDL AC#BAL,L'AC3BALN                                                 
         DCDDL AC#BAL,L'AC10BAL                                                 
         DCDDL AC#2CUR,L'OP82CUR                                                
         DCDDL AC#2CUR,L'OP32CUR                                                
         DCDDL AC#INSRT,L'AC@INSRT                                              
         DCDDL AC#RPLC,L'AC@RPLC                                                
         DCDDL AC#UDAT,L'OP8UDAT                                                
         DCDDL AC#UDAT,L'OP3UDAT                                                
         DCDDL AC#CTRDD,L'OP8CTRDD                                              
         DCDDL AC#CTRDD,L'OP3CTRDD                                              
DCLISTUX DC    AL1(EOT)                                                         
                                                                                
DCLISTL  DS    0X                  ** LOWER CASE DICTIONARY **                  
         DCDDL AC#ACC,L'LC8ACC                                                  
         DCDDL AC#ACRL,L'LC@ACRL                                                
         DCDDL AC#ACRRV,L'LC@ACRRV                                              
         DCDDL AC#ADR,L'LC@ADR                                                  
         DCDDL AC#ALTPF,L'LC@ALTPF                                              
         DCDDL AC#AMT,L'LC@AMT                                                  
         DCDDL AC#AMT,L'LC@AMTR,R                                               
         DCDDL AC#AUTHD,L'LC@ATHED                                              
         DCDDL AC#AUTHD,L'LC@AUTHD                                              
         DCDDL AC#BAL,L'LC@BAL                                                  
         DCDDL AC#BAT,L'LC@BATCH                                                
         DCDDL AC#BATTS,L'LC@BATTS                                              
*&&UK*&& DCDDL AC#BKGDT,L'LC@BKGDT                                              
         DCDDL AC#BLD,L'LC@BLD                                                  
         DCDDL AC#CALC,L'LC@CALC                                                
         DCDDL AC#CHK,L'LC13CHK                                                 
         DCDDL AC#CHK,L'LC@CHK                                                  
         DCDDL AC#CHKS,L'LC@CHKS                                                
         DCDDL AC#CLOSE,L'LC@CLOSE                                              
         DCDDL AC#CLSD,L'LC@CLSD                                                
         DCDDL AC#CR,L'LC@CR                                                    
         DCDDL AC#CRS,L'LC@CRS                                                  
         DCDDL AC#CRS,L'LC@CRSR,R                                               
         DCDDL AC#CRSMK,L'LC@CRSMK                                              
         DCDDL AC#CTRA,L'LC@CTRA                                                
         DCDDL AC#CTRD,L'LC@CTRD                                                
         DCDDL AC#CTRD,L'LC8CTRD                                                
         DCDDL AC#CURRY,L'LC@CURRY                                              
         DCDDL AC#CURRY,L'LC@CURYR,R                                            
         DCDDL AC#ACTY,L'LC@ACTY                                                
*        DCDDL AC#DATAD,L'LC@DATAD                                              
         DCDDL AC#DATE,L'LC@DATE                                                
         DCDDL AC#DATED,L'LC@DATED                                              
         DCDDL AC#DEF,L'LC@DEF                                                  
         DCDDL AC#DFRNC,L'LC@DFRNC                                              
         DCDDL AC#DISS,L'LC@DSC                                                 
         DCDDL AC#DISS,L'LC@DSCR,R                                              
         DCDDL AC#DR,L'LC@DR                                                    
         DCDDL AC#DRAFT,L'LC@DRFT                                               
         DCDDL AC#DRS,L'LC@DRS                                                  
         DCDDL AC#DRS,L'LC@DRSR,R                                               
         DCDDL AC#DRSMK,L'LC@DRSMK                                              
         DCDDL AC#DUEDT,L'LC@DUEDT                                              
         DCDDL AC#END,L'LC@END                                                  
*&&UK*&& DCDDL AC#ENTRD,L'LC@ENTRD                                              
         DCDDL AC#EXCHR,L'LC@EXCHR                                              
         DCDDL AC#EXDIF,L'LC@EXDIF                                              
         DCDDL AC#EXDIF,L'LC@EXDF                                               
         DCDDL AC#FIRST,L'LC@FRST                                               
         DCDDL AC#FLT,L'LC@FILT                                                 
         DCDDL AC#FTRAT,L'LC@FTRAT                                              
         DCDDL AC#FXDRF,L'LC@FXDRF                                              
         DCDDL AC#GROSS,L'LC@GRSR,R                                             
         DCDDL AC#HALF,L'LC@HALF                                                
         DCDDL AC#HELD,L'LC@HELD                                                
         DCDDL AC#INTRD,L'LC@INTRD                                              
         DCDDL AC#INV,L'LC20INV                                                 
         DCDDL AC#INV,L'LC3INV                                                  
         DCDDL AC#INVS,L'LC@INVS                                                
*&&UK*&& DCDDL AC#JOB,L'LC@JOB                                                  
*&&US*&& DCDDL AC#JOEST,L'LC@JOB                                                
         DCDDL AC#LAST,L'LC@LAST                                                
         DCDDL AC#LEFT,L'LC@LEFT                                                
         DCDDL AC#MARK,L'LC@MARK                                                
         DCDDL AC#MARKD,L'LC@MARKD                                              
         DCDDL AC#MCHED,L'LC@MCHED                                              
         DCDDL AC#MMODT,L'LC@MMODT                                              
         DCDDL AC#RSMOA,L'LC@MOA                                                
         DCDDL AC#MOS,L'LC@MOS                                                  
         DCDDL AC#MSNG,L'LC@MSNG                                                
         DCDDL AC#NCOM,L'LC@NCOM                                                
         DCDDL AC#NO,L'LC4NO                                                    
         DCDDL AC#NOTAK,L'LC@NOTAK                                              
         DCDDL AC#NRTV,L'LC@NRTV                                                
         DCDDL AC#RSCNT,L'LC@COUNT                                              
         DCDDL AC#NUM,L'LC@NUM                                                  
         DCDDL AC#NXT,L'LC@NXT                                                  
         DCDDL AC#OFF,L'LC@OFF3                                                 
         DCDDL AC#OFFST,L'LC@OFFST                                              
         DCDDL AC#PAGE,L'LC@PAGE                                                
         DCDDL AC#PAID2,L'LC@PAID                                               
         DCDDL AC#PELED,L'LC@PELED                                              
         DCDDL AC#PENDG,L'LC@PENDG                                              
         DCDDL AC#PSTGS,L'LC@PSTGS                                              
         DCDDL AC#PRD,L'LC@PRD                                                  
         DCDDL AC#QUIT,L'LC@QUIT                                                
         DCDDL AC#RCND,L'LC@RCND                                                
         DCDDL AC#REF,L'LC@REF                                                  
         DCDDL AC#RIGHT,L'LC@RIGHT                                              
         DCDDL AC#RVRSD,L'LC@RVRSD                                              
         DCDDL AC#RVRSD,L'LC5RVRSD                                              
         DCDDL AC#RVRSL,L'LC@RVRSL                                              
*&&UK*&& DCDDL AC#SEL3,L'LC@SEL                                                 
*&&US*&& DCDDL AC#SEL,L'LC@SEL                                                  
         DCDDL AC#SELED,L'LC@SELED                                              
         DCDDL AC#SELED,L'LC8SELED                                              
         DCDDL AC#SERNO,L'LC@SERNO                                              
         DCDDL AC#SRC,L'LC@SRC                                                  
         DCDDL AC#SUPN,L'LC@SUPN                                                
         DCDDL AC#STMDT,L'LC@STMDT                                              
         DCDDL AC#STT,L'LC@STAT                                                 
         DCDDL AC#STT,L'LC4STAT                                                 
         DCDDL AC#STT,L'LC8STAT                                                 
         DCDDL AC#SUBR,L'LC3SUBR                                                
         DCDDL AC#SUBR,L'LC@SUBR                                                
*&&UK*&& DCDDL AC#SUNCR,L'LC@SUNCR                                              
         DCDDL AC#SUP,L'LC@SUP                                                  
         DCDDL AC#SWTCH,L'LC@SWTCH                                              
         DCDDL AC#TAKEN,L'LC@TAKEN                                              
         DCDDL AC#TAXBS,L'LC@TAXBS                                              
         DCDDL AC#TOTCU,L'LC@TOTCU                                              
         DCDDL AC#TOTLS,L'LC@TOTLS                                              
         DCDDL AC#TYPE,L'LC@TYPE                                                
*&&UK*&& DCDDL AC#TYPE,L'LC3TYPE                                                
*&&US*&& DCDDL AC#BATTY,L'LC3TYPE                                               
         DCDDL AC#TYPE,L'LC8TYPE                                                
         DCDDL AC#UATH,L'LC@UATH                                                
         DCDDL AC#UDAT,L'LC@USDT                                                
         DCDDL AC#UMTCD,L'LC@UNM                                                
         DCDDL AC#UNITP,L'LC@UNPR,R                                             
         DCDDL AC#UNMKD,L'LC@UNMKD                                              
         DCDDL AC#UPD,L'LC@UPDT                                                 
         DCDDL AC#UPDGL,L'LC@UPDGL                                              
         DCDDL AC#URG,L'LC@URG                                                  
         DCDDL AC#USED,L'LC@USED                                                
         DCDDL AC#VOID,L'LC@VOID                                                
         DCDDL AC#VOID,L'LC4VOID                                                
         DCDDL AC#VSCHD,L'LC@VSCHD                                              
         DCDDL AC#VSCHN,L'LC3VSCH                                               
         DCDDL AC#VSCHN,L'LC@VSCHN                                              
         DCDDL AC#WC,L'LC@WC                                                    
         DCDDL AC#WRTF,L'LC3WRTF                                                
         DCDDL AC#WRTNF,L'LC@WRTNF                                              
         DCDDL AC#XFR,L'LC3XFER                                                 
         DCDDL AC#XFRD,L'LC@XFRD                                                
         DCDDL AC#YES,L'LC4YES                                                  
         DCDDL AC#EXCD,L'LC@EXCD                                                
         DCDDL AC#AVNPA,L'LC@AVNPA                                              
         DCDDL AC#SPLIT,L'LC@SPLIT                                              
         DCDDL AC#RECDT,L'LC@RECDT                                              
         DCDDL AC#CLRDT,L'LC@CLRDT                                              
*        DCDDL AC#BAREF,L'LC@BAREF                                              
*        DCDDL AC#MEDTE,L'LC@MEDTE                                              
*        DCDDL AC#MEREF,L'LC@MEREF                                              
*        DCDDL AC#MENOT,L'LC@MENOT                                              
         DCDDL AC#ERPYD,L'LC@ERPYD                                              
         DCDDL AC#ACC,L'LC@ACC                                                  
         DCDDL AC#ON,L'LC@ON                                                    
         DCDDL AC#AND,L'LC@AND                                                  
         DCDDL AC#UDET,L'LC@UDET                                                
         DCDDL AC#OPTS,L'LC@OPTS                                                
         DCDDL AC#NONE,L'LC@NONE                                                
         DCDDL AC#2CUR,L'LC@2CUR                                                
         DCDDL AC#CROR,L'LC@CROR                                                
         DCDDL AC#ATH,L'LC@ATH                                                  
         DCDDL AC#HOLD,L'LC@HOLD                                                
         DCDDL AC#TRN,L'LC@TRN                                                  
         DCDDL AC#APRVD,L'LC@APRVD                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -                 
         DCDDL AC#SYS,L'LC@SYSC                                                 
         DCDDL AC#MEDC,L'LC@MEDC                                                
         DCDDL AC#CLIC,L'LC@CLIC                                                
         DCDDL AC#PRO,L'LC@PRO                                                  
         DCDDL AC#EST,L'LC@EST                                                  
         DCDDL AC#MMOS,L'LC@MMOS                                                
         DCDDL AC#VNDR,L'LC@VNDR                                                
         DCDDL AC#INVC2,L'LC@INVC2                                              
         DCDDL AC#RSBNT,L'LC@RSBNT                                              
         DCDDL AC#CASH,L'LC@CASH                                                
         DCDDL AC#RCVNT,L'LC@RCVNT                                              
         DCDDL AC#DISB,L'LC@DISB                                                
         DCDDL AC#POSN,L'LC@POSN                                                
         DCDDL AC#TOTAL,L'LC@TOTAL                                              
         DCDDL AC#APG38,L'LC@APG38                                              
         DCDDL AC#CLRUI,L'LC@CLRUI                                              
         DCDDL AC#UDBTO,L'LC@UNDIS                                              
         DCDDL AC#AVAIL,L'LC@AVAIL                                              
         DCDDL AC#CODE,L'LC@CODE                                                
         DCDDL AC#NAME,L'LC@NAME                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -                 
*                                                                               
DCLISTLX DC    AL1(EOT)                                                         
         EJECT                                                                  
APFTAB   DS    0H                  ** ACTION PFK TABLE (SEE APFTABD) **         
         DC    AL1(PFK02),AL1(0)                                                
         DC    S(0),S(LC@NXT)                                                   
         DC    AL1(0),AL1(CCOLAY)                                               
*&&UK*&& DC    AL1(0,0)                                                         
*&&US*&& DC    AL1(EXCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK02),AL1(0)                                                
         DC    S(0),S(LC@NXT)                                                   
         DC    AL1(0),AL1(BVOLAY)                                               
*&&UK*&& DC    AL1(0,0)                                                         
*&&US*&& DC    AL1(EXCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
*&&UK                                                                           
         DC    AL1(PFK02),AL1(0)                                                
         DC    S(0),S(LC@VSCHN)                                                 
         DC    AL1(0),AL1(MROLAY)                                               
         DC    AL1(INCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK02),AL1(0)                                                
         DC    S(0),S(LC@NXT)                                                   
         DC    AL1(0),AL1(MROLAY)                                               
         DC    AL1(EXCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
*&&                                                                             
         DC    AL1(PFK02),AL1(0)                                                
         DC    S(0),S(LC@DFRNC)                                                 
         DC    AL1(0),AL1(CMOLAY)                                               
         DC    AL1(0,APFTILNG+APFTIP26)                                         
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK03),AL1(ACTQUIT)                                          
         DC    S(AC10QUI),S(LC@QUIT)                                            
         DC    AL1(ACTIXSET+ACTICFRM),AL1(0)                                    
         DC    AL1(0,APFTINUQ)                                                  
         DC    AL1(APFT2GRD,0)                                                  
                                                                                
         DC    AL1(PFK04),AL1(ACTDRFT)                                          
         DC    S(AC10DRF),S(LC@DRFT)                                            
         DC    AL1(ACTIXSET),AL1(0)                                             
         DC    AL1(0,APFTINUQ)                                                  
         DC    AL1(APFT2GRD,0)                                                  
*&&UK                                                                           
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@END)                                                   
         DC    AL1(0),AL1(CCOLAY)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@END)                                                   
         DC    AL1(0),AL1(BVOLAY)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@END)                                                   
         DC    AL1(0),AL1(MROLAY)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
*&&                                                                             
*&&US                                                                           
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@SEL)                                                   
         DC    AL1(0),AL1(CCOLAY)                                               
         DC    AL1(INCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@MARK)                                                  
         DC    AL1(0),AL1(CCOLAY)                                               
         DC    AL1(EXCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@SEL)                                                   
         DC    AL1(0),AL1(BVOLAY)                                               
         DC    AL1(INCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@MARK)                                                  
         DC    AL1(0),AL1(BVOLAY)                                               
         DC    AL1(EXCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
*&&                                                                             
         DC    AL1(PFK05),AL1(0)                                                
         DC    S(0),S(LC@AVNPA)                                                 
         DC    AL1(0),AL1(CMOLAY)                                               
         DC    AL1(0,APFTILNG+APFTIP26)                                         
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK06),AL1(ACTUPDT)                                          
         DC    S(AC10UPD),S(LC@UPDT)                                            
         DC    AL1(ACTIXSET+ACTICFRM),AL1(0)                                    
         DC    AL1(0,APFTINUQ)                                                  
         DC    AL1(APFT2GRD,0)                                                  
                                                                                
*&&UK                                                                           
         DC    AL1(PFK12),AL1(0)                                                
         DC    S(0),S(LC@QUIT)                                                  
         DC    AL1(0),AL1(MROLAY)                                               
         DC    AL1(EXCLUDE,0)                                                   
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK14),AL1(0)                                                
         DC    S(0),S(LC@SWTCH)                                                 
         DC    AL1(0),AL1(0)                                                    
         DC    AL1(0,APFTI1FC+APFTIALT)                                         
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK15),AL1(0)                                                
         DC    S(0),S(LC@TOTLS)                                                 
         DC    AL1(0),AL1(0)                                                    
         DC    AL1(0,APFTI1FC+APFTICOD)                                         
         DC    AL1(0,0)                                                         
*&&                                                                             
         DC    AL1(PFK16),AL1(0)                                                
         DC    S(0),S(LC@LEFT)                                                  
         DC    AL1(0),AL1(0)                                                    
         DC    AL1(0,APFTILFT)                                                  
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK17),AL1(0)                                                
         DC    S(0),S(LC@RIGHT)                                                 
         DC    AL1(0),AL1(0)                                                    
         DC    AL1(0,APFTIRGH)                                                  
         DC    AL1(0,0)                                                         
                                                                                
APFTABX  DC    AL1(EOT)                                                         
                                                                                
SPFTAB   DS    0H                  ** SCROLL PFK TABLE (SEE SPFTABD) **         
                                                                                
         DC    AL1(PFK07),AL1(SCRUP)                                            
         DC    S(SCR@HALF),S(LC@HALF)                                           
                                                                                
         DC    AL1(PFK08),AL1(SCRUP)                                            
         DC    S(SCR@PAGE),S(LC@PAGE)                                           
                                                                                
         DC    AL1(PFK09),AL1(0)                                                
         DC    S(SCR@FRST),S(LC@FRST)                                           
                                                                                
         DC    AL1(PFK10),AL1(SCRDOWN)                                          
         DC    S(SCR@HALF),S(LC@HALF)                                           
                                                                                
         DC    AL1(PFK11),AL1(SCRDOWN)                                          
         DC    S(SCR@PAGE),S(LC@PAGE)                                           
                                                                                
         DC    AL1(PFK12),AL1(0)                                                
         DC    S(SCR@LAST),S(LC@LAST)                                           
                                                                                
SPFTABX  DC    AL1(EOT)                                                         
                                                                                
ZPFTAB   DS    0H                                                               
         DC    AL1(PFK02),AL1(0)                                                
         DC    S(0),S(LC@SWTCH)                                                 
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PFK03),AL1(0)                                                
         DC    S(0),S(LC@QUIT)                                                  
         DC    AL1(0,0)                                                         
         DC    AL1(0,0)                                                         
                                                                                
ZPFTABX  DC    AL1(EOT)                                                         
                                                                                
SCRTAB   DS    0H                  ** SCROLL TABLE (SEE SCRTABD) **             
         DC    S(SCR@PAGE),AL2(SCRPAGE),AL1(0,0)                                
         DC    S(SCR@HALF),AL2(SCRHALF),AL1(0,0)                                
         DC    S(SCR@FRST),AL2(SCRMAXI),AL1(SCRUP,0)                            
         DC    S(SCR@LAST),AL2(SCRMAXI),AL1(SCRDOWN,0)                          
SCRTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
TYPTAB   DS    0H                  ** TYPE TABLE (SEE TYPTABD) **               
         DC    AL1(TYPWIP),AL1(TYPIC3RD),AL2(WIPACTS-TABLES)                    
         DC    S(AC10WIP),S(AC3WIP)                                             
         DC    AL1(TYPCRD),AL1(TYPIC2ND),AL2(CRDACTS-TABLES)                    
         DC    S(AC10CRD),S(AC3CRD)                                             
         DC    AL1(TYPBNK),AL1(0),AL2(BNKACTS-TABLES)                           
         DC    S(AC10BNK),S(AC3BNK)                                             
         DC    AL1(TYPGEN),AL1(TYPIC2ND),AL2(GENACTS-TABLES)                    
         DC    S(AC10GEN),S(AC3GEN)                                             
         DC    AL1(TYPMED),AL1(0),AL2(MEDACTS-TABLES)                           
         DC    S(AC10MED),S(AC3MED)                                             
         DC    AL1(TYPVCA),AL1(TYPIC2ND),AL2(VCAACTS-TABLES)                    
         DC    S(AC10VCA),S(AC3VCA)       VC/APPROVE                            
TYPTABX  DC    AL1(EOT)                                                         
                                                                                
                                                                                
ACTTABS  DS    0H                  ** ACTION TABLES (SEE ACTTABD) **            
                                                                                
                                                                                
WIPACTS  DC    S(AC10HLD),S(AC3HOLD)                                            
         DC    AL1(ACTHOLD)                                                     
         DC    AL1(WHOLAY,WHSCR1,WHSCR2)                                        
         DC    CL2'WH'                                                          
         DC    AL3(WHOPEQ)                                                      
         DC    AL1(SECWIPSQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(WHTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
*&&UK                                                                           
         DC    S(AC10NUM),S(AC3NUMB)                                            
         DC    AL1(ACTNUMB)                                                     
         DC    AL1(WNOLAY,WNSCR1,WNSCR2)                                        
         DC    CL2'WN'                                                          
         DC    AL3(WNOPEQ)                                                      
         DC    AL1(SECWIPSQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(WNCTRYS-MARKER)                                                
         DC    Y(WNTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10ATH),S(AC3AUTH)                                            
         DC    AL1(ACTAUTH)                                                     
         DC    AL1(WAOLAY,WASCR1,WASCR2)                                        
         DC    CL2'WH'                                                          
         DC    AL3(WAOPEQ)                                                      
         DC    AL1(SECWIPSQ)                                                    
         DC    AL1(ACTIDDSQ+ACTIPRVL)                                           
         DC    AL1(0)                                                           
         DC    Y(WACTRYS-MARKER)                                                
         DC    Y(WATOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10OFS),S(AC3OFFS)                                            
         DC    AL1(ACTOFFS)                                                     
         DC    AL1(WEOLAY,WESCR1,WESCR2)                                        
         DC    CL2'WE'                                                          
         DC    AL3(WEOPEQ)                                                      
         DC    AL1(SECWIPSQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(WETOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
*&&                                                                             
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
         DS    0H                                                               
CRDACTS  DS    0H                                                               
*&&UK                                                                           
         DC    S(AC10ATH),S(AC3AUTH)                                            
         DC    AL1(ACTAUTH)                                                     
         DC    AL1(CAOLAY,CASCR1,CASCR2)                                        
         DC    CL2'CA'                                                          
*        DC    CL2'VU'                                                          
         DC    AL3(CAOPEQ)                                                      
         DC    AL1(SECCRAUQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(CACTRYS-MARKER)                                                
         DC    Y(CATOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(ACTI2ZOO+ACTI2GRD)                                           
         DC    XL6'00'                                                          
*&&                                                                             
         DC    S(AC10SEL),S(AC3SEL)                                             
         DC    AL1(ACTSELC)                                                     
*        DC    AL1(CSOLAY,CSSCR1,GRDSCR)                                        
         DC    AL1(CSOLAY,CSSCR1,CSSCR2)                                        
*&&UK*&& DC    CL2'CS'                                                          
*&&US*&& DC    CL2'VA'                                                          
         DC    AL3(CSOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(CSTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(ACTI2ZOO)                                                    
*        DC    AL1(ACTI2ZOO+ACTI2GRD)                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10OFS),S(AC3OFFS)                                            
         DC    AL1(ACTOFFS)                                                     
         DC    AL1(COOLAY,COSCR1,COSCR2)                                        
*&&UK*&& DC    CL2'CO'                                                          
*&&US*&& DC    CL2'VO'                                                          
         DC    AL3(COOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(COTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(ACTI2ZOO)                                                    
*        DC    AL1(ACTI2ZOO+ACTI2GRD)                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10DSC),S(AC3DISC)                                            
         DC    AL1(ACTDISC)                                                     
         DC    AL1(CDOLAY,CDSCR1,CDSCR2)                                        
*&&UK*&& DC    CL2'CD'                                                          
*&&US*&& DC    CL2'VD'                                                          
         DC    AL3(CDOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(CDCTRYS-MARKER)                                                
         DC    Y(CDTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(ACTI2ZOO)                                                    
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10CHQ),S(AC3CHEQ)                                            
         DC    AL1(ACTCHEQ)                                                     
         DC    AL1(CCOLAY,CCSCR1,CCSCR2)                                        
*&&UK*&& DC    CL2'CC'                                                          
*&&US*&& DC    CL2'VC'                                                          
         DC    AL3(CCOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
         DC    AL1(ACTIPRVL+ACTICHUP)                                           
         DC    AL1(0)                                                           
         DC    Y(CCCTRYS-MARKER)                                                
         DC    Y(CCTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(ACTI2ZOO)                                                    
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10HLD),S(AC3HOLD)                                            
         DC    AL1(ACTHOLD)                                                     
         DC    AL1(CHOLAY,CHSCR1,CHSCR2)                                        
*&&UK*&& DC    CL2'CH'                                                          
*&&US*&& DC    CL2'VH'                                                          
         DC    AL3(CHOPEQ)                                                      
         DC    AL1(SECCRHOQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(CHTOTTAB-MARKER)                                               
         DC    XL1'00'                                                          
         DC    AL1(ACTI2ZOO)                                                    
*        DC    AL1(ACTI2ZOO+ACTI2GRD)                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10MAN),S(AC10MAN)                                            
         DC    AL1(ACTMANU)                                                     
         DC    AL1(CMOLAY,CMSCR1,CMSCR2)                                        
*&&UK*&& DC    CL2'CM'                                                          
*&&US*&& DC    CL2'VM'                                                          
         DC    AL3(CMOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
         DC    AL1(ACTIPRVL+ACTICHUP+ACTIPOST)                                  
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(CMTOTTAB-MARKER)                                               
         DC    AL1(BT36)                                                        
         DC    AL1(ACTI2ZOO)                                                    
         DC    XL6'00'                                                          
                                                                                
                                                                                
         DC    S(AC10BAL),S(AC3BALN)                                            
         DC    AL1(ACTBALN)                                                     
         DC    AL1(CBOLAY,CBSCR1,CBSCR2)                                        
         DC    CL2'CQ'                                                          
         DC    AL3(CBOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
         DC    AL1(ACTIPRVL+ACTICHUP+ACTIPOST)                                  
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(CBTOTTAB-MARKER)                                               
         DC    AL1(BT65)                                                        
         DC    AL1(ACTI2ZOO)                                                    
         DC    XL6'00'                                                          
         DC    AL1(EOT)                                                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
VCAACTS  DS    0H                                                               
         DC    S(AC10SEL),S(AC3SEL)                                             
         DC    AL1(ACTSELC)                                                     
         DC    AL1(VTOLAY,VTSCR1,GRDSCR)                                        
         DC    CL2'VT'                                                          
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(SECSODCQ)                                                    
*        DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(CSTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(ACTI2ZOO+ACTI2GRO)                                           
         DC    XL6'00'                                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
         DC    AL1(EOT)                                                         
*                                                                               
         DS    0H                                                               
BNKACTS  DC    S(AC10RCN),S(AC3RECN)                                            
         DC    AL1(ACTRECN)                                                     
         DC    AL1(BROLAY,BRSCR1,BRSCR2)                                        
         DC    CL2'BR'                                                          
         DC    AL3(BROPEQ)                                                      
         DC    AL1(SECBRECQ)                                                    
         DC    AL1(ACTIPRVL+ACTICHUP)                                           
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(BRTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10VOI),S(AC3VOID)                                            
         DC    AL1(ACTVOID)                                                     
         DC    AL1(BVOLAY,BVSCR1,BVSCR2)                                        
         DC    CL2'BV'                                                          
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(SECBVOIQ)                                                    
         DC    AL1(ACTIPRVL+ACTICHUP+ACTIPOST)                                  
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(BVTOTTAB-MARKER)                                               
         DC    AL1(BT37)                                                        
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
         DS    0H                                                               
                                                                                
GENACTS  DC    S(AC10REV),S(AC3REVS)                                            
         DC    AL1(ACTREVS)                                                     
         DC    AL1(GROLAY,GRSCR1,GRSCR2)                                        
         DC    CL2'GR'                                                          
         DC    AL3(GROPEQ)                                                      
         DC    AL1(SECGREVQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(0)                                                             
         DC    Y(GRTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10OFS),S(AC3OFFS)                                            
         DC    AL1(ACTOFFS)                                                     
         DC    AL1(GOOLAY,GOSCR1,GOSCR2)                                        
         DC    CL2'GO'                                                          
         DC    AL3(GOOPEQ)                                                      
         DC    AL1(SECGOFFQ)                                                    
         DC    AL1(ACTIPRVL)                                                    
         DC    AL1(0)                                                           
         DC    Y(GACTRYS-MARKER)                                                
         DC    Y(GOTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    S(AC10OFS),S(AC3OFFS)                                            
         DC    AL1(ACTOFFS)                                                     
         DC    AL1(GOOLAY,GOSCR1,GOSCR2)                                        
         DC    CL2'GO'                                                          
         DC    AL3(GOOPEQ)                                                      
         DC    AL1(SECGOFFQ)                                                    
         DC    AL1(ACTIPRVL+ACTIDDSQ)                                           
         DC    AL1(0)                                                           
         DC    Y(GDCTRYS-MARKER)                                                
         DC    Y(GOTOTTAB-MARKER)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
                                                                                
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
         DS    0H                                                               
MEDACTS  DS    0H                                                               
*&&UK                                                                           
         DC    S(AC10RCN),S(AC3RECN)                                            
         DC    AL1(ACTRECN)                                                     
         DC    AL1(MROLAY,MRSCR1,MRSCR2)                                        
         DC    CL2'MR'                                                          
         DC    AL3(MROPEQ)                                                      
         DC    AL1(SECMRECQ)                                                    
         DC    AL1(ACTIPRVL+ACTICHUP+ACTIPOST)                                  
         DC    AL1(0)                                                           
         DC    Y(MRCTRYS-MARKER)                                                
         DC    Y(MRTOTTAB-MARKER)                                               
         DC    AL1(BT64)                                                        
         DC    AL1(0)                                                           
         DC    XL6'00'                                                          
*&&                                                                             
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
ACTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
KEYTAB   DS    0H                  ** KEY ELEMENTS (SEE KEYTABD) **             
         DC    X'80',AL1(L'TRNKOFF)     OFFICE (OR WORKCODE IN SJ)              
         DC    X'40',AL1(L'TRNKCULC)    C/U/L/CONTRA                            
         DC    X'20',AL1(L'TRNKDATE)    DATE                                    
         DC    X'10',AL1(L'TRNKREF)     REFERENCE                               
         DC    X'08',AL1(L'TRNKSBR)     SUB-REFERENCE                           
         DC    X'04',AL1(L'TRNKSMOS)    MONTH OF SERVICE                        
         DC    X'02',AL1(L'TRNBTCH)     BATCH REFERENCE                         
         DC    X'01',AL1(L'TSARVAR)     VARIABLE BYTE (EG. STATUS)              
***********************************************************************         
* MARKER ID TABLE FOR GRIDS                                           *         
* COVERED BY MIDTABD                                                  *         
***********************************************************************         
MIDTAB   DS    0X                  MAR ID TABLE COVERED BY MIDTABD              
*                                                                               
MIDCRSL  DC    AL3(CSOPEQ),AL2(MIDCRSLQ)  CREDITOR SELECT                       
         DC    C'010   U',AL1(CTRYALLQ,MIDALDGQ)                                
*        DC    C'010 Z U',AL1(CTRYALLQ,MIDALDGQ)                                
MIDCRSLQ EQU   *-MIDCRSL                                                        
*                                                                               
MIDCRAT  DC    AL3(CAOPEQ),AL2(MIDCRATQ)  CREDITOR AUTHORISE                    
         DC    C'020   U',AL1(CTRYALLQ,MIDALDGQ)                                
*        DC    C'020 Z U',AL1(CTRYALLQ,MIDALDGQ)                                
MIDCRATQ EQU   *-MIDCRAT                                                        
*                                                                               
MIDCRHL  DC    AL3(CHOPEQ),AL2(MIDCRHLQ)  CREDITOR HOLD                         
         DC    C'030   U',AL1(CTRYALLQ,MIDALDGQ)                                
*        DC    C'030 Z U',AL1(CTRYALLQ,MIDALDGQ)                                
MIDCRHLQ EQU   *-MIDCRHL                                                        
*                                                                               
MIDCROF  DC    AL3(COOPEQ),AL2(MIDCROFQ)  CREDITOR OFFSET                       
         DC    C'040   U',AL1(CTRYALLQ,MIDALDGQ)                                
*        DC    C'040 Z U',AL1(CTRYALLQ,MIDALDGQ)                                
MIDCROFQ EQU   *-MIDCROF                                                        
*                                                                               
MIDVEML  DC    AL3(VTOPEQ),AL2(MIDVEMLQ)  VENDOR MATCH                          
         DC    C'050   U',AL1(CTRYALLQ,MIDALDGQ)                                
*        DC    C'050 Z U',AL1(CTRYALLQ,MIDALDGQ)                                
MIDVEMLQ EQU   *-MIDVEML                                                        
*                                                                               
MIDTABX  DC    AL3(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* A DISPLAY OPTION NEEDS AN ENTRY IN: DISTAB, DISDISP, COLEQUS        *         
* ADD A NEW USER OPTION IN FIRST SPARE POSITION                       *         
* ADD A NEW DDS ONLY OPTION IN LAST SPARE POSITION                    *         
***********************************************************************         
*                                                                               
DISTAB   DS    0H                                                               
*                                                                               
DIS0     DC    AL1(COLIDGQ,DIS0LNQ)      INTERNAL RECORD NUMBER - GRID          
         DC    AL1(L'LC@COUNT,0),S(LC@COUNT,0,TSARCNT)                          
         DC    AL1(L'TSARCNT,0,DISNBINY,0,0,0,DISCIOGR)                         
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0,0,DISI2NUM+DISI2NTO,0,DISI4ID+DISI4HDE,0)                  
DIS0LNQ  EQU   *-DIS0                                                           
*                                                                               
DIS1     DC    AL1(COLOFFQ,DIS1LNQ)     1 - OFFICE - TRANS OF                   
         DC    AL1(L'LC@TRN,L'LC@OFF3),S(LC@TRN,LC@OFF3,TSAROFF)                
*        DC    AL1(L'LC@OFF,0),S(LC@OFF,0,TSAROFF)                              
         DC    AL1(L'TSAROFF,0,0,0,0,0,0)                                       
         DC    AL3(DISALLQ-DISALWQ-VTOPEQ)                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS1LNQ  EQU   *-DIS1                                                           
                                                                                
DIS2     DC    AL1(COLWRKQ,DIS2LNQ)     2 - WORKCODE                            
         DC    AL1(L'LC@WC,0),S(LC@WC,0,TSARFWRK)                               
         DC    AL1(L'TSARFWRK,0,0,0,0,0,0)                                      
         DC    AL3(DISALWQ+GROPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS2LNQ  EQU   *-DIS2                                                           
                                                                                
DIS3     DC    AL1(COLCONQ,DIS3LNQ)     3 - CONTRA ACCOUNT                      
         DC    AL1(L'LC@CTRA,0),S(LC@CTRA,0,TSARCON+1)                          
         DC    AL1(L'TSARCON-1,0,0,0,0,0,0)                                     
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS3LNQ  EQU   *-DIS3                                                           
                                                                                
DIS4     DC    AL1(COLDATQ,DIS4LNQ)     4 - DATE                                
         DC    AL1(L'LC@TRN,L'LC@DATE),S(LC@TRN,LC@DATE,TSARDAT)                
         DC    AL1(DISDFRM1,0,0,0,0,0,0)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,0,0)                                 
DIS4LNQ  EQU   *-DIS4                                                           
                                                                                
DIS5     DC    AL1(COLREFQ,DIS5LNQ)     5 - REFERENCE - TRANS REF               
         DC    AL1(L'LC@TRN,L'LC@REF3),S(LC@TRN,LC@REF3,TSARREF)                
*        DC    AL1(L'LC@REF,0),S(LC@REF,0,TSARREF)                              
         DC    AL1(L'TSARREF,0,0,0,0,0,0)                                       
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS5LNQ  EQU   *-DIS5                                                           
                                                                                
DIS6     DC    AL1(COLMOSQ,DIS6LNQ)     6 - MONTH OF ACTIVITY                   
         DC    AL1(L'LC@MOA,0),S(LC@MOA,0,TSARMOS)                              
         DC    AL1(DISDFRM1,0,0,0,0,0,0)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,DISI2DTE+DISI2RGH,DISI3MOS,0,0)                          
DIS6LNQ  EQU   *-DIS6                                                           
                                                                                
DIS7     DC    AL1(COLBATQ,DIS7LNQ)     7 - BATCH REFERENCE                     
         DC    AL1(L'LC@BATCH,L'LC@REF3),S(LC@BATCH,LC@REF3,TSARBAT)            
         DC    AL1(L'TSARBAT,0,0,0,0,0,0)                                       
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS7LNQ  EQU   *-DIS7                                                           
                                                                                
DIS8     DC    AL1(COLAMTQ,DIS8LNQ)     8 - AMOUNT (DEBIT OR CREDIT)            
         DC    AL1(L'LC@AMTR,0),S(LC@AMTR,0,TSARAMNT)                           
         DC    AL1(L'TSARAMNT,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(DISALLQ-VTOPEQ-DISALCQ)                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH,0,0,0)                                 
DIS8LNQ  EQU   *-DIS8                                                           
                                                                                
DIS9     DC    AL1(COLDRSQ,DIS9LNQ)     9 - DEBIT AMOUNT                        
         DC    AL1(L'LC@DRSR,0),S(LC@DRSR,0,TSARAMNT)                           
         DC    AL1(L'TSARAMNT,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(DISALLQ-DISPPCQ-VTOPEQ-COOPEQ)                               
         DC    AL1(0,0,DISI2NUM+DISI2RGH,DISI3DR,0,0)                           
DIS9LNQ  EQU   *-DIS9                                                           
                                                                                
DIS10    DC    AL1(COLCRSQ,DIS10LNQ)    A - CREDIT AMOUNT                       
         DC    AL1(L'LC@CRSR,0),S(LC@CRSR,0,TSARAMNT)                           
         DC    AL1(L'TSARAMNT,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,DISI2NUM+DISI2RGH,DISI3CR,0,0)                           
DIS10LNQ EQU   *-DIS10                                                          
                                                                                
DIS11    DC    AL1(COLSUPQ,DIS11LNQ)    B - SUPPLIER                            
         DC    AL1(L'LC@SUP,0),S(LC@SUP,0,TSARCON)                              
         DC    AL1(L'TSARCON,0,0,0,0,0,0)                                       
         DC    AL3(DISALWQ+GROPEQ+BVOPEQ)                                       
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
DIS11LNQ EQU   *-DIS11                                                          
                                                                                
DIS12    DC    AL1(COLJOBQ,DIS12LNQ)    C - JOB                                 
         DC    AL1(L'LC@JOB,0),S(LC@JOB,0,TSARFSAC)                             
         DC    AL1(L'TSARFSAC,0,0,0,0,0,0)                                      
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS12LNQ EQU   *-DIS12                                                          
                                                                                
DIS13    DC    AL1(COLSRCQ,DIS13LNQ)    D - SOURCE                              
         DC    AL1(L'LC@SRC,0),S(LC@SRC,0,TSARFSAC)                             
         DC    AL1(L'TSARFSAC,0,0,0,0,0,0)                                      
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ+MROPEQ)                               
         DC    AL1(0,0,0,0,0,0)                                                 
DIS13LNQ EQU   *-DIS13                                                          
                                                                                
DIS14    DC    AL1(COLCHQQ,DIS14LNQ)    E - CHEQUE NUMBER                       
         DC    AL1(0,0),S(0,0,0)          SEE SUBSTITUTE TABLE                  
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(CCOPEQ+CBOPEQ+DISALBQ)                                       
         DC    AL1(0,0,0,0,0,0)                                                 
DIS14LNQ EQU   *-DIS14                                                          
                                                                                
DIS15    DC    AL1(COLTYPQ,DIS15LNQ)    F - TYPE DR/CR                          
         DC    AL1(L'LC@TYPE,0),S(LC@TYPE,0,TSARINDS)                           
         DC    AL1(L'TSARINDS,0,TSARDRQ,0,0,0,0)                                
         DC    AL3(DISALLQ-VTOPEQ-DISALCQ)                                      
         DC    AL1(0,0,0,0,DISI4STA+DISI4HDE,0)                                 
DIS15LNQ EQU   *-DIS15                                                          
                                                                                
DIS16    DC    AL1(COLFWKQ,DIS16LNQ)    G - SOURCE ACCOUNT WORKCODE             
         DC    AL1(0,0),S(0,0,0)          SEE SUBSTITUTE TABLE                  
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ+MROPEQ)                               
         DC    AL1(0,0,0,0,0,0)                                                 
DIS16LNQ EQU   *-DIS16                                                          
                                                                                
DIS17    DC    AL1(COLFOTQ,DIS17LNQ)    H - OTHER NUMBER (SUBREFERENCE)         
*        DC    AL1(L'LC@SUBR,0),S(LC@SUBR,0,TSARFOTH)                           
*        DC    AL1(L'TSARFOTH,0,0,0,0,0,0)                                      
         DC    AL1(L'LC@SUBR,0),S(LC@SUBR,0,TSARSBR)                            
         DC    AL1(L'TSARSBR,0,DISNBINY,0,0,0,0)                                
         DC    AL3(DISALLQ-BROPEQ-CBOPEQ-COOPEQ-CSOPEQ-CHOPEQ-VTOPEQ)           
*        DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
DIS17LNQ EQU   *-DIS17                                                          
                                                                                
DIS18    DC    AL1(COLNUMQ,DIS18LNQ)     I - INTERNAL RECORD NUMBER             
         DC    AL1(L'LC@COUNT,0),S(LC@COUNT,0,TSARCNT)                          
         DC    AL1(L'TSARCNT,0,DISNBINY,0,0,0,DISCIGRD)                         
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,DISI2NUM+DISI2NTO,0,DISI4HDE,0)                          
DIS18LNQ EQU   *-DIS18                                                          
                                                                                
DIS19    DC    AL1(COLDSCQ,DIS19LNQ)     J - DISCOUNT AMOUNT                    
         DC    AL1(0,0),S(0,0,0)   SEE SUBSTITUTE TABLE                         
         DC    AL1(0,0,0,0,0,0,DISCIPCQ)                                        
*&&UK*&& DC    AL3(DISALCQ+BVOPEQ+MROPEQ)                                       
*&&US*&& DC    AL3(DISALCQ+BVOPEQ+GROPEQ)                                       
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS19LNQ EQU   *-DIS19                                                          
                                                                                
DIS20    DC    AL1(COLOMOQ,DIS20LNQ)     K - CLOSED MONTH                       
         DC    AL1(L'LC@CLOSE,0),S(LC@CLOSE,0,TSARFUSE)                         
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(CCOPEQ+GOOPEQ+GROPEQ)                                        
         DC    AL1(0,0,DISI2DTE+DISI2RGH,DISI3MOS,DISI4HDE,0)                   
DIS20LNQ EQU   *-DIS20                                                          
                                                                                
DIS21    DC    AL1(COLSTAQ,DIS21LNQ)     L - MARKED STATUS                      
         DC    AL1(L'LC@STAT,0),S(LC@STAT,0,0)                                  
         DC    AL1(0,0,0,0,0,0,DISCIGRD)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS21LNQ EQU   *-DIS21                                                          
                                                                                
DIS22    DC    AL1(COLADAQ,DIS22LNQ)     M - DATE ADDED - ACTIVITY DATE         
         DC    AL1(L'LC@ACTY,L'LC@DATE),S(LC@ACTY,LC@DATE,TSARADAT)             
*        DC    AL1(L'LC@DATAD,0),S(LC@DATAD,0,TSARADAT)                         
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,0,0)                                 
DIS22LNQ EQU   *-DIS22                                                          
                                                                                
DIS23    DC    AL1(COLBTYQ,DIS23LNQ)     N - BATCH INPUT TYPE-TRANS TYP         
         DC    AL1(L'LC@TRN,L'LC8TYPE),S(LC@TRN,LC8TYPE,TSARBTY)                
*        DC    AL1(L'LC3TYPE,0),S(LC3TYPE,0,TSARBTY)                            
         DC    AL1(L'TSARBTY,0,DISNBINY,0,0,0,0)                                
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,DISI2NUM+DISI2NTO+DISI2RGH,0,0,0)                        
DIS23LNQ EQU   *-DIS23                                                          
                                                                                
DIS24    DC    AL1(COLRMOQ,DIS24LNQ)     O - REVERSING TRANSACTION MOS          
         DC    AL1(L'LC5RVRSD,0),S(LC5RVRSD,0,TSARFREV)                         
         DC    AL1(DISDFRM1,0,0,0,0,0,0)                                        
         DC    AL3(GROPEQ)                                                      
         DC    AL1(0,0,DISI2DTE+DISI2RGH,DISI3MOS,DISI4HDE,0)                   
DIS24LNQ EQU   *-DIS24                                                          
*                                                                               
DIS25    DC    AL1(COLDUEQ,DIS25LNQ)     P - PAYMENT DUE DATE                   
         DC    AL1(L'LC@DUEDT,0),S(LC@DUEDT,0,TSARFDUE)                         
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(DISALCQ-CCOPEQ+CBOPEQ)                                       
         DC    AL1(FLDDUEDT,0,DISI2DTE+DISI2RGH+DISI2EDT,0,0)                   
         DC    AL1(TSARZDUE)                                                    
DIS25LNQ EQU   *-DIS25                                                          
*&&DO                                                                           
DIS25    DC    AL1(COLDUEQ,DIS25LNQ)     P - PAYMENT DUE DATE                   
         DC    AL1(L'LC@DUEDT,0),S(LC@DUEDT,0,0)                                
         DC    AL1(DISDFRM2,DUEELQ,DUEDATE-DUEELD,DUELNQ,0,0,0)                 
         DC    AL3(DISALCQ-CCOPEQ+CBOPEQ)                                       
         DC    AL1(FLDDUEDT,0,DISI2DTE+DISI2RGH+DISI2EDT,0,0)                   
         DC    AL1(TSARZDUE)                                                    
DIS25LNQ EQU   *-DIS25                                                          
*&&                                                                             
*                                                                               
*&&DO                                                                           
**&&US                                                                          
DIS25    DC    AL1(25,DIS25LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS25LNQ EQU   *-DIS25                                                          
*&&                                                                             
DIS26    DC    AL1(COLXSTQ,DIS26LNQ)     Q - FILE STATUS (TRNSTAT)              
         DC    AL1(L'LC4STAT,0),S(LC4STAT,0,0)                                  
         DC    AL1(0,0,0,0,0,0,DISCIGRD)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS26LNQ EQU   *-DIS26                                                          
                                                                                
DIS27    DC    AL1(COLUSEQ,DIS27LNQ)     R - USED DATE                          
         DC    AL1(0,0),S(0,0,0)   SEE SUBSTITUTE TABLE                         
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(COOPEQ+CCOPEQ+CBOPEQ+GOOPEQ+BVOPEQ)                          
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
DIS27LNQ EQU   *-DIS27                                                          
*&&US                                                                           
DIS28    DC    AL1(COLINVQ,DIS28LNQ)     S - LONG INVOICE NUMBER                
*        DC    AL1(L'LC20INV,0),S(LC20INV,0,TSARFINV)                           
         DC    AL1(L'LC20INV,L'LC@NUM),S(LC20INV,LC@NUM,TSARFINV)               
         DC    AL1(L'TSARFINV,0,0,0,0,0,0)                                      
         DC    AL3(DISALCQ+BVOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS28LNQ EQU   *-DIS28                                                          
*&&                                                                             
*&&UK                                                                           
DIS28    DC    AL1(COLFXRQ,DIS28LNQ)     S - FIXED REFERENCE                    
         DC    AL1(L'LC@FXDRF,0),S(LC@FXDRF,0,TSARFDIS)                         
         DC    AL1(L'TSARFDIS,0,0,0,0,0,0)                                      
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
DIS28LNQ EQU   *-DIS28                                                          
*&&                                                                             
DIS29    DC    AL1(COLBSDQ,DIS29LNQ)     T - BANK STATEMENT DATE                
         DC    AL1(L'LC@STMDT,0),S(LC@STMDT,0,TSARBSDT)                         
         DC    AL1(2,0,0,0,0,0,0)                                               
         DC    AL3(BROPEQ)                                                      
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
DIS29LNQ EQU   *-DIS29                                                          
*&&US                                                                           
DIS30    DC    AL1(COLTAXQ,DIS30LNQ)     U - TAX/BASIS                          
         DC    AL1(L'LC@TAXBS,0),S(LC@TAXBS,0,TSARFTAX)                         
         DC    AL1(L'TSARFTAX,0,DISNPCKD,0,0,0,0)                               
         DC    AL3(DISALCQ+GROPEQ+BVOPEQ)                                       
         DC    AL1(0,0,DISI2NUM+DISI2NTO+DISI2RGH,0,0,0)                        
DIS30LNQ EQU   *-DIS30                                                          
                                                                                
DIS31    DC    AL1(COLMMDQ,DIS31LNQ)     V - MEDIA MOS                          
         DC    AL1(L'LC@MMODT,0),S(LC@MMODT,0,TSARFMMD)                         
         DC    AL1(DISDFRM1,0,0,0,0,0,DISCIXLG)                                 
         DC    AL3(DISALCQ+GROPEQ+BVOPEQ)                                       
         DC    AL1(0,0,DISI2DTE+DISI2RGH,DISI3DAY+DISI3MOS,0,0)                 
*        DC    C'SV',C'SX'                                                      
DIS31LNQ EQU   *-DIS31                                                          
*                                                                               
DIS32    DC    AL1(COLNARQ,DIS32LNQ)     W - NARRATIVE (ALL EXCEPT MR)          
         DC    AL1(L'LC@NRTV,0),S(LC@NRTV,0,0)                                  
         DC    AL1(120,TRNELQ,TRNNARR-TRNELD,0,0,0,0)                           
         DC    AL3(DISALLQ-MROPEQ-VTOPEQ)                                       
         DC    AL1(FLDNARR,0,DISI2EDT,0,DISI4VAR,0)                             
DIS32LNQ EQU   *-DIS32                                                          
*                                                                               
DIS33    DC    AL1(33,DIS33LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS33LNQ EQU   *-DIS33                                                          
                                                                                
DIS34    DC    AL1(34,DIS34LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS34LNQ EQU   *-DIS34                                                          
                                                                                
DIS35    DC    AL1(35,DIS35LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS35LNQ EQU   *-DIS35                                                          
                                                                                
DIS36    DC    AL1(36,DIS36LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS36LNQ EQU   *-DIS36                                                          
                                                                                
DIS37    DC    AL1(37,DIS37LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS37LNQ EQU   *-DIS37                                                          
*&&                                                                             
*&&UK                                                                           
DIS30    DC    AL1(COLNARQ,DIS30LNQ)     U - NARRATIVE                          
         DC    AL1(0,0),S(0,0,0)   SEE SUBSTITUTE TABLE                         
         DC    AL1(120,TRNELQ,TRNNARR-TRNELD,0,0,0,0)                           
         DC    AL3(DISALLQ)                                                     
         DC    AL1(FLDNARR,0,DISI2EDT,0,DISI4HDE,0)                             
DIS30LNQ EQU   *-DIS30                                                          
                                                                                
DIS31    DC    AL1(COLSERQ,DIS31LNQ)     V - BUY SERIAL NUMBER                  
         DC    AL1(L'LC@SERNO,0),S(LC@SERNO,0,TSARFSER)                         
         DC    AL1(L'TSARFSER,0,0,0,0,0,0)                                      
         DC    AL3(MROPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS31LNQ EQU   *-DIS31                                                          
                                                                                
DIS32    DC    AL1(COLUNPQ,DIS32LNQ)     W - UNIT PRICE                         
         DC    AL1(L'LC@UNPR,0),S(LC@UNPR,0,TSARFUNP)                           
         DC    AL1(L'TSARFUNP,0,DISNPCKD,0,0,0,0)                               
         DC    AL3(MROPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
DIS32LNQ EQU   *-DIS32                                                          
                                                                                
DIS33    DC    AL1(COLIDTQ,DIS33LNQ)     X - INSERTION DATE                     
         DC    AL1(L'LC@INTRD,0),S(LC@INTRD,0,TSARFIDT)                         
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(MROPEQ)                                                      
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
DIS33LNQ EQU   *-DIS33                                                          
                                                                                
DIS34    DC    AL1(COLSDCQ,DIS34LNQ)     Y - SUNDRY CREDITOR                    
         DC    AL1(L'LC@SUNCR,0),S(LC@SUNCR,0,0)                                
         DC    AL1(0,NAMELQ,NAMEREC-NAMELD,0,0,0,0)                             
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0,0,0,0,DISI4VAR+DISI4HDE,0)                                 
DIS34LNQ EQU   *-DIS34                                                          
                                                                                
DIS35    DC    AL1(COLAFCQ,DIS35LNQ)     Z - ACCOUNT FOREIGN CURRENCY           
         DC    AL1(L'LC@AMTR,L'LC@CURYR),S(LC@AMTR,LC@CURYR,TSARAFCA)           
         DC    AL1(L'TSARAFCA,0,DISNPCKD,0,0,0,DISCIFCQ)                        
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0,0,DISI2NUM+DISI2RGH,0,DISI4HDE,0)                          
DIS35LNQ EQU   *-DIS35                                                          
                                                                                
DIS36    DC    AL1(COLEXDQ,DIS36LNQ)    0 - EXCHANGE DIFFERENCE                 
         DC    AL1(L'LC@EXDF,0),S(LC@EXDF,0,0)                                  
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(CMOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
DIS36LNQ EQU   *-DIS36                                                          
                                                                                
DIS37    DC    AL1(COLRATQ,DIS37LNQ)    / - EXCHANGE RATE                       
         DC    AL1(L'LC@EXCHR,0),S(LC@EXCHR,0,TSARAFCX)                         
         DC    AL1(L'TSARAFCX,0,0,0,0,0,0)                                      
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,DISI3EDR,DISI4HDE,0)          
DIS37LNQ EQU   *-DIS37                                                          
*&&                                                                             
DIS38    DC    AL1(COLSNMQ,DIS38LNQ)    @ - SUPPLIER NAME                       
         DC    AL1(L'LC@SUPN,0),S(LC@SUPN,0,0)                                  
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(DISALBQ)                                                     
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
DIS38LNQ EQU   *-DIS38                                                          
                                                                                
DIS39    DC    AL1(COLADVQ,DIS39LNQ)    ) - ADVANCE                             
         DC    AL1(L'LC@AVNPA,0),S(LC@AVNPA,0,TSARCADV)                         
         DC    AL1(L'TSARCADV,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
DIS39LNQ EQU   *-DIS39                                                          
                                                                                
DIS40    DC    AL1(COLDIFQ,DIS40LNQ)    # - DIFFERENCE                          
         DC    AL1(L'LC@DFRNC,0),S(LC@DFRNC,0,TSARCDIF)                         
         DC    AL1(L'TSARCDIF,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
DIS40LNQ EQU   *-DIS40                                                          
*&&UK                                                                           
DIS41    DC    AL1(COLSAMQ,DIS41LNQ)    " - AMOUNT (DR/CR) 2ND CURRENCY         
         DC    AL1(L'LC@AMTR,L'LC@2CUR),S(LC@AMTR,LC@2CUR,TSARSCUA)             
         DC    AL1(L'TSARSCUA,0,DISNPCKD,0,0,0,DISCI2CQ)                        
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0,0,DISI2NUM+DISI2RGH,0,DISI4HDE,0)                          
DIS41LNQ EQU   *-DIS41                                                          
                                                                                
DIS42    DC    AL1(COLSDRQ,DIS42LNQ)   % - DEBIT AMOUNT SECOND CURRENCY         
         DC    AL1(L'LC@DRSR,L'LC@2CUR),S(LC@DRSR,LC@2CUR,TSARSCUA)             
         DC    AL1(L'TSARSCUA,0,DISNPCKD,0,0,0,DISCI2CQ)                        
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,DISI3DR,DISI4HDE,0)           
DIS42LNQ EQU   *-DIS42                                                          
                                                                                
DIS43    DC    AL1(COLSCRQ,DIS43LNQ)  & - CREDIT AMOUNT SECOND CURRENCY         
         DC    AL1(L'LC@CRSR,L'LC@2CUR),S(LC@CRSR,LC@2CUR,TSARSCUA)             
         DC    AL1(L'TSARSCUA,0,DISNPCKD,0,0,0,DISCI2CQ)                        
         DC    AL3(DISALLQ)                                                     
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,DISI3CR,DISI4HDE,0)           
DIS43LNQ EQU   *-DIS43                                                          
                                                                                
DIS44    DC    AL1(COLSDSQ,DIS44LNQ)        - DISCOUNT SECOND CURRENCY         
         DC    AL1(L'LC@DSCR,L'LC@2CUR),S(LC@DSCR,LC@2CUR,TSARSCUD)             
         DC    AL1(L'TSARSCUD,0,DISNPCKD,0,0,0,DISCI2CQ)                        
         DC    AL3(DISALCQ+BVOPEQ)                                              
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
DIS44LNQ EQU   *-DIS44                                                          
                                                                                
DIS45    DC    AL1(COLSDIQ,DIS45LNQ)   $  - DIFFERENCE PORTION 2ND CURR         
         DC    AL1(L'LC@DFRNC,L'LC@2CUR),S(LC@DFRNC,LC@2CUR,TSARCDIF)           
         DC    AL1(L'TSARCDIF,0,DISNPCKD,0,0,0,DISCI2CQ)                        
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,DISI3SEC,DISI4HDE,0)          
DIS45LNQ EQU   *-DIS45                                                          
                                                                                
DIS46    DC    AL1(COLSADQ,DIS46LNQ)   '  - ADVANCE PORTION 2ND CURR            
         DC    AL1(L'LC@AVNPA,L'LC@2CUR),S(LC@AVNPA,LC@2CUR,TSARCADV)           
         DC    AL1(L'TSARCADV,0,DISNPCKD,0,0,0,DISCI2CQ)                        
         DC    AL3(BVOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,DISI3SEC,DISI4HDE,0)          
DIS46LNQ EQU   *-DIS46                                                          
                                                                                
DIS47    DC    AL1(COLLREF,DIS47LNQ)     \ - LONG INVOICE NUMBER                
         DC    AL1(L'LC@LREF,0),S(LC@LREF,0,0)                                  
         DC    AL1(20,FFTELQ,FFTDATA-FFTELD,0,FFTTINVN,0,0)                     
         DC    AL3(DISALCQ+GROPEQ)                                              
         DC    AL1(0,0,0,0,DISI4VAR+DISI4HDE,0)                                 
DIS47LNQ EQU   *-DIS47                                                          
*&&                                                                             
*&&US                                                                           
DIS41    DC    AL1(41,DIS41LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS41LNQ EQU   *-DIS41                                                          
                                                                                
DIS42    DC    AL1(42,DIS42LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS42LNQ EQU   *-DIS42                                                          
                                                                                
DIS43    DC    AL1(43,DIS43LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS43LNQ EQU   *-DIS43                                                          
                                                                                
DIS44    DC    AL1(44,DIS44LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS44LNQ EQU   *-DIS44                                                          
                                                                                
DIS45    DC    AL1(45,DIS45LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS45LNQ EQU   *-DIS45                                                          
                                                                                
DIS46    DC    AL1(46,DIS46LNQ)           N/D                                   
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS46LNQ EQU   *-DIS46                                                          
                                                                                
DIS47    DC    AL1(47,DIS47LNQ)           N/D                                   
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS47LNQ EQU   *-DIS47                                                          
*&&                                                                             
DIS48    DC    AL1(COLHSTQ,DIS48LNQ)      *  (DDS)                              
         DC    AL1(L'LC@STAT,0),S(LC@STAT,0,0)                                  
         DC    AL1(0,0,0,0,0,0,DISCIGRD)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS48LNQ EQU   *-DIS48                                                          
                                                                                
DIS49    DC    AL1(COLSEQQ,DIS49LNQ)      <  (DDS)                              
         DC    AL1(L'LC3SUBR,0),S(LC3SUBR,0,0)                                  
         DC    AL1(0,0,0,0,0,0,DISCIGRD)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS49LNQ EQU   *-DIS49                                                          
                                                                                
DIS50    DC    AL1(COLDADQ,DIS50LNQ)      (  (DDS)                              
         DC    AL1(L'LC@ADR,0),S(LC@ADR,0,0)                                    
         DC    AL1(0,0,0,0,0,0,DISCIGRD)                                        
         DC    AL3(DISALLQ-VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS50LNQ EQU   *-DIS50                                                          
*&&UK                                                                           
DIS51    DC    AL1(COLMDTQ,DIS51LNQ)      GRID ONLY - MEMO DATE                 
         DC    AL1(L'LC@MEDTE,0),S(LC@MEDTE,0,0)                                
         DC    AL1(DISDFRM2,NOTELQ,NOTDATE-NOTELD,NOTLN1Q,0,0,DISCIOGR)         
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0,1,DISI2DTE+DISI2REL+DISI2EDT+DISI2RGH)                     
         DC    AL1(DISI3TRE+DISI3MUL,DISI4HDE,0)                                
DIS51LNQ EQU   *-DIS51                                                          
                                                                                
DIS52    DC    AL1(COLMRFQ,DIS52LNQ)      GRID ONLY - MEMO REFERENCE            
         DC    AL1(L'LC@MEREF,0),S(LC@MEREF,0,0)                                
         DC    AL1(L'NOTREF,NOTELQ,NOTREF-NOTELD,NOTLN1Q,0,0,DISCIOGR)          
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0,1,DISI2REL+DISI2EDT,DISI3MUL,DISI4HDE,0)                   
DIS52LNQ EQU   *-DIS52                                                          
                                                                                
DIS53    DC    AL1(COLMNTQ,DIS53LNQ)      GRID ONLY - MEMO NOTE                 
         DC    AL1(L'LC@MENOT,0),S(LC@MENOT,0,0)                                
         DC    AL1(50,NOTELQ,NOTNOTE-NOTELD,NOTLN1Q,0,0,DISCIOGR)               
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0,1,DISI2REL+DISI2EDT,DISI3MUL)                              
         DC    AL1(DISI4VAR+DISI4MST+DISI4HDE,0)                                
DIS53LNQ EQU   *-DIS53                                                          
                                                                                
DIS54    DC    AL1(COLERPQ,DIS54LNQ)      GRID ONLY - EARLIEST PAYMENT          
         DC    AL1(L'LC@ERPYD,0),S(LC@ERPYD,0,TSARERPD) DATE                    
         DC    AL1(DISDFRM1,0,0,0,0,0,DISCIOGR)                                 
         DC    AL3(DISALCQ)                                                     
         DC    AL1(FLDERPD,0,DISI2DTE+DISI2EDT+DISI2RGH,0)                      
         DC    AL1(DISI4HDE,TSARZEPD)                                           
DIS54LNQ EQU   *-DIS54                                                          
                                                                                
DIS55    DC    AL1(COLATHQ,DIS55LNQ)      GRID ONLY - AUTHORISED                
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,DISCIOGR)                                        
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0,0,0,0,0,0)                                                 
DIS55LNQ EQU   *-DIS55                                                          
*&&                                                                             
*&&US                                                                           
DIS51    DC    AL1(51,DIS51LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS51LNQ EQU   *-DIS51                                                          
                                                                                
DIS52    DC    AL1(52,DIS52LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS52LNQ EQU   *-DIS52                                                          
                                                                                
DIS53    DC    AL1(53,DIS53LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS53LNQ EQU   *-DIS53                                                          
                                                                                
DIS54    DC    AL1(54,DIS54LNQ)          N/D                                    
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS54LNQ EQU   *-DIS54                                                          
                                                                                
DIS55    DC    AL1(55,DIS55LNQ)           N/D                                   
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
DIS55LNQ EQU   *-DIS55                                                          
*&&                                                                             
DIS56    DC    AL1(COLSELQ,DIS56LNQ)      GRID ONLY - SELECTED                  
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,DISCIOGR)                                        
         DC    AL3(DISALCQ+VTOPEQ)                                              
         DC    AL1(0,0,0,0,0,0)                                                 
DIS56LNQ EQU   *-DIS56                                                          
                                                                                
DIS57    DC    AL1(COLHLDQ,DIS57LNQ)      GRID ONLY - HELD                      
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,DISCIOGR)                                        
         DC    AL3(DISALCQ)                                                     
         DC    AL1(0,0,0,0,0,0)                                                 
DIS57LNQ EQU   *-DIS57                                                          
*                                                                               
DIS58    DC    AL1(COLOFSQ,DIS58LNQ)      GRID ONLY - OFFSET                    
         DC    AL1(L'LC@OFFST,0),S(LC@OFFST,0,TSARINDS)                         
         DC    AL1(L'TSARINDS,0,0,0,0,0,DISCIOGR)                               
         DC    AL3(COOPEQ)                                                      
         DC    AL1(0,0,DISI2CKB,0,0,0)                                          
DIS58LNQ EQU   *-DIS58                                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*                                                                               
DIS59    DC    AL1(COLSYSQ,DIS59LNQ)                  SYSTEM                    
         DC    AL1(L'LC@SYSC,0),S(LC@SYSC,0,TSARAASY)                           
         DC    AL1(L'TSARAASY,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS59LNQ EQU   *-DIS59                                                          
*                                                                               
DIS60    DC    AL1(COLMEDQ,DIS60LNQ)                  MEDIA                     
         DC    AL1(L'LC@MEDC,0),S(LC@MEDC,0,TSARAAMD)                           
         DC    AL1(L'TSARAAMD,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS60LNQ EQU   *-DIS60                                                          
*                                                                               
DIS61    DC    AL1(COLCLIQ,DIS61LNQ)                  CLIENT                    
         DC    AL1(L'LC@CLIC,0),S(LC@CLIC,0,TSARAACL)                           
         DC    AL1(L'TSARAACL,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS61LNQ EQU   *-DIS61                                                          
*                                                                               
DIS62    DC    AL1(COLPROQ,DIS62LNQ)                  PRODUCT                   
         DC    AL1(L'LC@PRO,0),S(LC@PRO,0,TSARAAPR)                             
         DC    AL1(L'TSARAAPR,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS62LNQ EQU   *-DIS62                                                          
                                                                                
DIS63    DC    AL1(COLESTQ,DIS63LNQ)                  ESTIMATE                  
         DC    AL1(L'LC@EST,0),S(LC@EST,0,TSARAAES)                             
         DC    AL1(L'TSARAAES,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS63LNQ EQU   *-DIS63                                                          
                                                                                
DIS64    DC    AL1(COLALTQ,DIS64LNQ)                  MOS                       
         DC    AL1(L'LC@MMOS,0),S(LC@MMOS,0,TSARAALT)                           
         DC    AL1(L'TSARAALT,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS64LNQ EQU   *-DIS64                                                          
                                                                                
DIS65    DC    AL1(COLVNDQ,DIS65LNQ)                  VENDOR                    
         DC    AL1(L'LC@VNDR,L'LC@CODE),S(LC@VNDR,LC@CODE,TSARAAVN)             
         DC    AL1(L'TSARAAVN,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS65LNQ EQU   *-DIS65                                                          
                                                                                
DIS66    DC    AL1(COLVNIQ,DIS66LNQ)                  VENDOR INVOICE #          
         DC    AL1(L'LC@VNDR,L'LC@INVC2),S(LC@VNDR,LC@INVC2,TSARAAVI)           
         DC    AL1(L'TSARAAVI,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS66LNQ EQU   *-DIS66                                                          
                                                                                
DIS67    DC    AL1(COLAABQ,DIS67LNQ)                                            
         DC    AL1(6,L'LC@RSBNT),S(LC@AMT,LC@RSBNT,TSARAAAB)                    
         DC    AL1(L'TSARAAAB,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS67LNQ EQU   *-DIS67                                                          
                                                                                
DIS68    DC    AL1(COLACRQ,DIS68LNQ)                                            
         DC    AL1(L'LC@CASH,L'LC@RCVNT),S(LC@CASH,LC@RCVNT,TSARAACR)           
         DC    AL1(L'TSARAACR,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS68LNQ EQU   *-DIS68                                                          
                                                                                
DIS69    DC    AL1(COLACDQ,DIS69LNQ)                                            
         DC    AL1(6,L'LC@DISB),S(LC@CHKS,LC@DISB,TSARAACD)                     
         DC    AL1(L'TSARAACD,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS69LNQ EQU   *-DIS69                                                          
                                                                                
DIS70    DC    AL1(COLACPQ,DIS70LNQ)                                            
         DC    AL1(L'LC@CASH,L'LC@POSN),S(LC@CASH,LC@POSN,TSARAACP)             
         DC    AL1(L'TSARAACP,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS70LNQ EQU   *-DIS70                                                          
                                                                                
DIS71    DC    AL1(COLATCQ,DIS71LNQ)                                            
         DC    AL1(L'LC@TOTAL,L'LC@APG38),S(LC@TOTAL,LC@APG38,TSARAATC)         
         DC    AL1(L'TSARAATC,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS71LNQ EQU   *-DIS71                                                          
                                                                                
DIS72    DC    AL1(COLACUQ,DIS72LNQ)                                            
         DC    AL1(L'LC@CLRUI,L'LC@UNDIS),S(LC@CLRUI,LC@UNDIS,TSARAACU)         
         DC    AL1(L'TSARAACU,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS72LNQ EQU   *-DIS72                                                          
                                                                                
DIS73    DC    AL1(COLACAQ,DIS73LNQ)                                            
         DC    AL1(L'LC@CASH,L'LC@AVAIL),S(LC@CASH,LC@AVAIL,TSARAACA)           
         DC    AL1(L'TSARAACA,0,DISNPCKD,0,0,0,DISCIPCQ)                        
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
DIS73LNQ EQU   *-DIS73                                                          
                                                                                
DIS74    DC    AL1(COLVNMQ,DIS74LNQ)                                            
         DC    AL1(L'LC@VNDR,L'LC@NAME),S(LC@VNDR,LC@NAME,TSARVNME)             
         DC    AL1(L'TSARVNME,0,0,0,0,0,0)                                      
         DC    AL3(VTOPEQ)                                                      
         DC    AL1(0,0,0,0,0,0)                                                 
DIS74LNQ EQU   *-DIS74                                                          
*                                                                               
DISTABN  EQU   (*-DISTAB)/DISTABL                                               
DISTABX  DC    AL1(EOT)                                                         
                                                                                
                                                                                
* NOTE: THIS TABLE MUST BE COVERED BY DISTABD, SO ENTRIES HAVE THE SAME         
* STRUCTURE AS DISTAB ENTRIES.  THE TABLE IS USED WHEN THE DISTAB ENTRY         
* FOR A COLUMN HAS VALID ACTIONS, BUT DISLEN IS ZERO, TO INDICATE THAT          
* HEADING TEXT VARIES BY ACTION.  THE ENTRIES FOR A COLUMN IN SUDTAB            
* MUST COVER ALL ACTIONS ALLOWED BY THE MAIN DISTAB ENTRY.                      
                                                                                
SUDTAB   DS    0H                  SUBSTITUTE DISPLAY COLUMN TABLE              
SUD1     DC    AL1(COLCHQQ,SUD1LNQ)       BANK ACTIONS                          
         DC    AL1(L'LC@CHK,0),S(LC@CHK,0,TSARREF)                              
         DC    AL1(L'TSARREF,0,0,0,0,0,0)                                       
         DC    AL3(DISALBQ)                                                     
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
SUD1LNQ  EQU   *-SUD1                                                           
*&&UK                                                                           
SUD1A    DC    AL1(COLCHQQ,SUD1ALNQ)      BANK ACTIONS                          
         DC    AL1(L'LC@CHK,0),S(LC@CHK,0,TSARFCHN)                             
         DC    AL1(L'TSARFCHN,0,0,0,0,0,0)                                      
         DC    AL3(CCOPEQ)                                                      
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
SUD1ALNQ EQU   *-SUD1                                                           
                                                                                
SUD2     DC    AL1(COLCHQQ,SUD2LNQ)       CREDITOR BALANCE                      
         DC    AL1(L'LC@BAREF,0),S(LC@BAREF,0,TSARFCHN)                         
         DC    AL1(L'TSARFCHN,0,0,0,0,0,0)                                      
         DC    AL3(CBOPEQ)                                                      
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
SUD2LNQ  EQU   *-SUD2                                                           
*&&                                                                             
*&&US                                                                           
SUD1A    DC    AL1(COLCHQQ,SUD1ALNQ)         N/D                                
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD1ALNQ EQU   *-SUD1A                                                          
                                                                                
SUD2     DC    AL1(COLCHQQ,SUD2LNQ)         N/D                                 
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD2LNQ  EQU   *-SUD2                                                           
*&&                                                                             
SUD3     DC    AL1(COLUSEQ,SUD3LNQ)       CONTRA'D/OFFSET (CO,GO)               
         DC    AL1(L'LC8CTRD,L'LC@DATE),S(LC8CTRD,LC@DATE,TSARUSDT)             
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(COOPEQ+GOOPEQ)                                               
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,0,0)                                 
*        DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
SUD3LNQ  EQU   *-SUD3                                                           
                                                                                
SUD4     DC    AL1(COLUSEQ,SUD4LNQ)      BALANCED (CQ)                          
         DC    AL1(L'LC@BAL,0),S(LC@BAL,0,TSARUSDT)                             
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(CBOPEQ)                                                      
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
SUD4LNQ  EQU   *-SUD4                                                           
*&&UK                                                                           
SUD5     DC    AL1(COLUSEQ,SUD5LNQ)      PAID (CC,BV)                           
         DC    AL1(L'LC@PAID,0),S(LC@PAID,0,TSARUSDT)                           
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(CCOPEQ+BVOPEQ)                                               
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
SUD5LNQ  EQU   *-SUD5                                                           
*&&                                                                             
*&&US                                                                           
SUD5     DC    AL1(COLUSEQ,SUD5LNQ)      USED DATE (CC, BV)                     
         DC    AL1(L'LC@USDT,0),S(LC@USDT,0,TSARUSDT)                           
         DC    AL1(DISDFRM2,0,0,0,0,0,0)                                        
         DC    AL3(CCOPEQ+BVOPEQ)                                               
         DC    AL1(0,0,DISI2DTE+DISI2RGH,0,DISI4HDE,0)                          
SUD5LNQ  EQU   *-SUD5                                                           
*&&                                                                             
SUD6     DC    AL1(COLDSCQ,SUD6LNQ)      DISCOUNT (CREDITOR, BV)                
         DC    AL1(L'LC@DSC,L'LC@AMT),S(LC@DSC,LC@AMT,TSARFDIS)                 
         DC    AL1(L'TSARFDIS,0,DISNPCKD,0,0,0,0)                               
         DC    AL3(DISALCQ+BVOPEQ)                                              
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,0,0)                        
SUD6LNQ  EQU   *-SUD6                                                           
*&&UK                                                                           
SUD7     DC    AL1(COLDSCQ,SUD7LNQ)      GROSS (MR)                             
         DC    AL1(L'LC@GRSR,0),S(LC@GRSR,0,TSARFDIS)                           
         DC    AL1(L'TSARFDIS,0,DISNPCKD,0,0,0,0)                               
         DC    AL3(MROPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
SUD7LNQ  EQU   *-SUD7                                                           
*&&                                                                             
*&&US                                                                           
SUD7     DC    AL1(COLDSCQ,SUD7LNQ)      DISCOUNT (GENERAL/REVERSE)             
         DC    AL1(L'LC@DSCR,0),S(LC@DSCR,0,TSARFDIS)                           
         DC    AL1(L'TSARFDIS,0,DISNPCKD,0,0,0,0)                               
         DC    AL3(GROPEQ)                                                      
         DC    AL1(0,0,DISI2NUM+DISI2RGH+DISI2NTO,0,DISI4HDE,0)                 
SUD7LNQ  EQU   *-SUD7                                                           
*&&                                                                             
SUD8     DC    AL1(COLFWKQ,SUD8LNQ)    WORKCODE (CREDITOR, GENERAL, BV)         
         DC    AL1(L'LC@WC,0),S(LC@WC,0,TSARFWRK)                               
         DC    AL1(L'TSARFWRK,0,0,0,0,0,0)                                      
         DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
         DC    AL1(0,0,0,0,0,0)                                                 
SUD8LNQ  EQU   *-SUD8                                                           
*&&UK                                                                           
SUD9     DC    AL1(COLFWKQ,SUD9LNQ)    TYPE (MR)                                
         DC    AL1(L'LC@WC,0),S(LC@TYPE,0,TSARFWRK)                             
         DC    AL1(L'TSARFWRK,0,0,0,0,0,0)                                      
         DC    AL3(MROPEQ)                                                      
         DC    AL1(0,0,0,0,DISI4HDE,0)                                          
SUD9LNQ  EQU   *-SUD9                                                           
                                                                                
SUD10    DC    AL1(COLNARQ,SUD10LNQ)   NARRATIVE (ALL EXCEPT MR)                
         DC    AL1(L'LC@NRTV,0),S(LC@NRTV,0,0)                                  
         DC    AL1(120,TRNELQ,TRNNARR-TRNELD,0,0,0,0)                           
         DC    AL3(DISALLQ-MROPEQ)                                              
         DC    AL1(FLDNARR,0,DISI2EDT,0,DISI4VAR,0)                             
SUD10LNQ EQU   *-SUD10                                                          
                                                                                
SUD11    DC    AL1(COLNARQ,SUD11LNQ)   BOOKING DETAILS (MR)                     
         DC    AL1(L'LC@BKGDT,0),S(LC@BKGDT,0,0)                                
         DC    AL1(120,TRNELQ,TRNNARR-TRNELD,0,0,0,0)                           
         DC    AL3(MROPEQ)                                                      
         DC    AL1(0,0,0,0,DISI4VAR,0)                                          
SUD11LNQ EQU   *-SUD11                                                          
*&&                                                                             
*&&US                                                                           
SUD9     DC    AL1(33,SUD9LNQ)          N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD9LNQ  EQU   *-SUD9                                                           
                                                                                
SUD10    DC    AL1(34,SUD10LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD10LNQ EQU   *-SUD10                                                          
                                                                                
SUD11    DC    AL1(35,SUD11LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD11LNQ EQU   *-SUD11                                                          
*&&                                                                             
SUD12    DC    AL1(COLHLDQ,SUD13LNQ)      GRID ONLY - HELD                      
         DC    AL1(L'LC@HELD,0),S(LC@HELD,0,TSARINDS)                           
         DC    AL1(L'TSARINDS,0,0,0,0,0,DISCIOGR)                               
         DC    AL3(CHOPEQ)                                                      
         DC    AL1(0,0,DISI2CKB,0,0,0)                                          
SUD12LNQ EQU   *-SUD12                                                          
                                                                                
SUD13    DC    AL1(COLHLDQ,SUD13LNQ)      GRID ONLY - HELD                      
         DC    AL1(L'LC@HELD,0),S(LC@HELD,0,TSARSTA)                            
         DC    AL1(L'TSARSTA,0,TRNSHOLD,0,0,0,DISCIOGR)                         
         DC    AL3(DISALCQ-CHOPEQ)                                              
         DC    AL1(0,0,0,0,DISI4STA,0)                                          
SUD13LNQ EQU   *-SUD13                                                          
                                                                                
SUD14    DC    AL1(COLSELQ,SUD14LNQ)      GRID ONLY - SELECTED                  
         DC    AL1(L'LC@APRVD,0),S(LC@APRVD,0,TSARSTA)                          
*        DC    AL1(L'LC@SELED,0),S(LC@SELED,0,TSARSTA)                          
         DC    AL1(L'TSARSTA,0,TRNSAPPR,0,0,0,DISCIOGR)                         
         DC    AL3(DISALCQ-CSOPEQ)                                              
         DC    AL1(0,0,0,0,DISI4STA,0)                                          
SUD14LNQ EQU   *-SUD14                                                          
*                                                                               
SUD15    DC    AL1(COLSELQ,SUD15LNQ)      GRID ONLY - SELECTED                  
         DC    AL1(L'LC@APRVD,0),S(LC@APRVD,0,TSARINDS)                         
*        DC    AL1(L'LC@SELED,0),S(LC@SELED,0,TSARINDS)                         
         DC    AL1(L'TSARINDS,0,0,0,0,0,DISCIOGR)                               
         DC    AL3(CSOPEQ+VTOPEQ)                                               
         DC    AL1(0,0,DISI2CKB,0,0,0)                                          
SUD15LNQ EQU   *-SUD15                                                          
*&&UK                                                                           
SUD16    DC    AL1(COLATHQ,SUD16LNQ)      GRID ONLY - AUTHORISED                
         DC    AL1(L'LC@ATHED,0),S(LC@ATHED,0,TSARSTA)                          
         DC    AL1(L'TSARSTA,0,TRNSAUTH,0,0,0,DISCIOGR)                         
         DC    AL3(DISALCQ-CAOPEQ)                                              
         DC    AL1(0,0,0,0,DISI4STA,0)                                          
SUD16LNQ EQU   *-SUD16                                                          
                                                                                
SUD17    DC    AL1(COLATHQ,SUD17LNQ)      GRID ONLY - AUTHORISED                
         DC    AL1(L'LC@ATHED,0),S(LC@ATHED,0,TSARINDS)                         
         DC    AL1(L'TSARINDS,0,0,0,0,0,DISCIOGR)                               
         DC    AL3(CAOPEQ)                                                      
         DC    AL1(0,0,DISI2CKB,0,0,0)                                          
SUD17LNQ EQU   *-SUD17                                                          
*&&                                                                             
*&&US                                                                           
SUD16    DC    AL1(16,SUD16LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD16LNQ EQU   *-SUD16                                                          
                                                                                
SUD17    DC    AL1(17,SUD17LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD17LNQ EQU   *-SUD17                                                          
*&&                                                                             
SUD18    DC    AL1(42,SUD18LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD18LNQ EQU   *-SUD18                                                          
                                                                                
SUD19    DC    AL1(43,SUD19LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD19LNQ EQU   *-SUD19                                                          
                                                                                
SUD20    DC    AL1(44,SUD20LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD20LNQ EQU   *-SUD20                                                          
                                                                                
SUD21    DC    AL1(45,SUD21LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD21LNQ EQU   *-SUD21                                                          
                                                                                
SUD22    DC    AL1(46,SUD22LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD22LNQ EQU   *-SUD22                                                          
                                                                                
SUD23    DC    AL1(47,SUD23LNQ)         N/D                                     
         DC    AL1(0,0),S(0,0,0)                                                
         DC    AL1(0,0,0,0,0,0,0)                                               
         DC    AL3(0)                                                           
         DC    AL1(0,0,0,0,0,0)                                                 
SUD23LNQ EQU   *-SUD23                                                          
                                                                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* A DISPLAY OPTION NEEDS AN ENTRY IN: DISTAB, DISDISP, COLEQUS        *         
* ADD A NEW USER OPTION IN FIRST SPARE POSITION                       *         
* ADD A NEW DDS ONLY OPTION IN LAST SPARE POSITION                    *         
***********************************************************************         
*                                                                               
*ISTAB   DS    0H                                                               
*        DC    AL1(COLOFFQ)        1 - OFFICE                                   
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@OFF,0),S(LC@OFF,0)                                      
*        DC    AL3(DISALLQ-DISALWQ)                                             
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLWRKQ)        2 - WORKCODE                                 
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@WC,0),S(LC@WC,0)                                        
*        DC    AL3(DISALWQ+GROPEQ)                                              
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLCONQ)        3 - CONTRA ACCOUNT                           
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@CTRA,0),S(LC@CTRA,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLDATQ)        4 - DATE                                     
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@DATE,0),S(LC@DATE,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLREFQ)        5 - REFERENCE                                
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@REF,0),S(LC@REF,0)                                      
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLMOSQ)        6 - MONTH OF SERVICE                         
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@MOS,0),S(LC@MOS,0)                                      
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLBATQ)        7 - BATCH REFERENCE                          
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@BATCH,0),S(LC@BATCH,0)                                  
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLAMTQ)        8 - AMOUNT (DEBIT OR CREDIT)                 
*        DC    AL1(DISCIPCQ)                                                    
*        DC    AL1(L'LC@AMTR,0),S(LC@AMTR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLDRSQ)        9 - DEBIT AMOUNT                             
*        DC    AL1(DISCIPCQ)                                                    
*        DC    AL1(L'LC@DRSR,0),S(LC@DRSR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLCRSQ)        A - CREDIT AMOUNT                            
*        DC    AL1(DISCIPCQ)                                                    
*        DC    AL1(L'LC@CRSR,0),S(LC@CRSR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSUPQ)        B - SUPPLIER                                 
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@SUP,0),S(LC@SUP,0)                                      
*        DC    AL3(DISALWQ+GROPEQ+BVOPEQ)                                       
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLJOBQ)        C - JOB                                      
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@JOB,0),S(LC@JOB,0)                                      
*        DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSRCQ)        D - SOURCE ACCOUNT                           
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@SRC,0),S(LC@SRC,0)                                      
*        DC    AL3(DISALCQ+DISALGQ+BVOPEQ+MROPEQ)                               
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLCHQQ)        E - CHEQUE NUMBER                            
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@CHK,0),S(LC@CHK,0)                                      
*        DC    AL3(CCOPEQ+DISALBQ)                                              
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLTYPQ)        F - TYPE DR/CR                               
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@TYPE,0),S(LC@TYPE,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLFWKQ)        G - SOURCE ACCOUNT WORKCODE                  
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)         SEE SUBSTITUTE TABLE                     
*        DC    AL3(DISALCQ+DISALGQ+BVOPEQ+MROPEQ)                               
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLFOTQ)        H - OTHER NUMBER (SUBREFERENCE)              
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@SUBR,0),S(LC@SUBR,0)                                    
*        DC    AL3(DISALLQ-CCOPEQ-BROPEQ-CBOPEQ)                                
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLNUMQ)        I - INTERNAL RECORD NUMBER                   
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC4NUM,0),S(LC4NUM,0)                                      
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLDSCQ)        J - DISCOUNT AMOUNT                          
*        DC    AL1(DISCIPCQ+DISCIFCQ)                                           
*        DC    AL1(0,0),S(0,0)     SEE SUBSTITUTE TABLE                         
*&&UK*&& DC    AL3(DISALCQ+BVOPEQ+MROPEQ)                                       
*&&US*&& DC    AL3(DISALCQ+BVOPEQ+GROPEQ)                                       
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLOMOQ)        K - CLOSED MONTH                             
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@CLOSE,0),S(LC@CLOSE,0)                                  
*        DC    AL3(COOPEQ+CCOPEQ+GOOPEQ+GROPEQ)                                 
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSTAQ)        L - MARKED STATUS                            
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@STAT,0),S(LC@STAT,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLADAQ)        M - DATE ADDED                               
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@DATAD,0),S(LC@DATAD,0)                                  
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLBTYQ)        N - BATCH INPUT TYPE                         
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC3TYPE,0),S(LC3TYPE,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLRMOQ)        O - REVERSING TRANSACTION MOS                
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC5RVRSD,0),S(LC5RVRSD,0)                                  
*        DC    AL3(GROPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLDUEQ)        P - PAYMENT DUE DATE                         
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@DUEDT,0),S(LC@DUEDT,0)                                  
*        DC    AL3(DISALCQ-CCOPEQ+CBOPEQ)                                       
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLXSTQ)        Q - FILE STATUS (TRNSTAT)                    
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC4STAT,0),S(LC4STAT,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLUSEQ)        R - USED DATE                                
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)     SEE SUBSTITUTE TABLE                         
*        DC    AL3(COOPEQ+CCOPEQ+GOOPEQ+BVOPEQ)                                 
*        DC    AL1(0)                                                           
*&&US                                                                           
*        DC    AL1(COLINVQ)        S - MEDIA/LONG/REFERENCE NUMBER              
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC20INV,0),S(LC20INV,0)                                    
*        DC    AL3(DISALCQ+BVOPEQ)                                              
*        DC    AL1(0)                                                           
*&&                                                                             
*&&UK                                                                           
*        DC    AL1(COLFXRQ)        S - FIXED REFERENCE                          
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@FXDRF,0),S(LC@FXDRF,0)                                  
*        DC    AL3(BVOPEQ)                                                      
*        DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
*        DC    AL1(COLBSDQ)        T - BANK STATEMENT DATE                      
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@STMDT,0),S(LC@STMDT,0)                                  
*        DC    AL3(BROPEQ)                                                      
*        DC    AL1(0)                                                           
*&&US                                                                           
*        DC    AL1(COLTAXQ)        U - TAX/BASIS                                
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@TAXBS,0),S(LC@TAXBS,0)                                  
*        DC    AL3(DISALCQ+GROPEQ+BVOPEQ)                                       
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLMMDQ)        V - MEDIA MONTH/DATE                         
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@MMODT,0),S(LC@MMODT,0)                                  
*        DC    AL3(DISALCQ+GROPEQ+BVOPEQ)                                       
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLNARQ)        W - NARRATIVE                                
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@NRTV,0),S(LC@NRTV,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*MN                                                                             
*        DC    AL1(COLRCDT)        STATEMENT RECONCILED DATE                    
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@RECDT,0),S(LC@RECDT,0)                                  
*        DC    AL3(DISBKRC)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLCLDT)        BANK CLEARED DATE                            
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@CLRDT,0),S(LC@CLRDT,0)                                  
*        DC    AL3(DISBKRC)                                                     
*        DC    AL1(0)                                                           
*MN                                                                             
*                                                                               
*        DC    AL1(35)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(36)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(37)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*&&                                                                             
*&&UK                                                                           
*        DC    AL1(COLNARQ)        U - NARRATIVE                                
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)     SEE SUBSTITUTE TABLE                         
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSERQ)        V - BUY SERIAL NUMBER                        
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@SERNO,0),S(LC@SERNO,0)                                  
*        DC    AL3(MROPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLUNPQ)        W - UNIT PRICE                               
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@UNPR,0),S(LC@UNPR,0)                                    
*        DC    AL3(MROPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLIDTQ)        X - INSERTION DATE                           
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@INTRD,0),S(LC@INTRD,0)                                  
*        DC    AL3(MROPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSDCQ)        Y - SUNDRY CREDITOR                          
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@SUNCR,0),S(LC@SUNCR,0)                                  
*        DC    AL3(DISALCQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLAFCQ)        Z - ACCOUNT FOREIGN CURRENCY                 
*        DC    AL1(DISCIFCQ)                                                    
*        DC    AL1(L'LC@AMTR,L'LC@CURYR),S(LC@AMTR,LC@CURYR)                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLEXDQ)        0 - EXCHANGE DIFFERENCE                      
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@EXDF,0),S(LC@EXDF,0)                                    
*        DC    AL3(CMOPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLRATQ)        / - EXCHANGE RATE                            
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@EXCHR,0),S(LC@EXCHR,0)                                  
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*&&                                                                             
*        DC    AL1(COLSNMQ)        @ - SUPPLIER NAME                            
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@SUPN,0),S(LC@SUPN,0)                                    
*        DC    AL3(DISALBQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLADVQ)        ) - ADVANCE                                  
*        DC    AL1(DISCIPCQ)                                                    
*        DC    AL1(L'LC@AVNPA,0),S(LC@AVNPA,0)                                  
*        DC    AL3(BVOPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLDIFQ)        # - DIFFERENCE                               
*        DC    AL1(DISCIPCQ)                                                    
*        DC    AL1(L'LC@DFRNC,0),S(LC@DFRNC,0)                                  
*        DC    AL3(BVOPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*&&UK                                                                           
*        DC    AL1(COLSAMQ)        " - AMOUNT (DR/CR) 2ND CURRENCY              
*        DC    AL1(DISCI2CQ)                                                    
*        DC    AL1(L'LC@AMTR,0),S(LC@AMTR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSDRQ)        % - DEBIT AMOUNT SECOND CURRENCY             
*        DC    AL1(DISCI2CQ)                                                    
*        DC    AL1(L'LC@DRSR,0),S(LC@DRSR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSCRQ)        & - CREDIT AMOUNT SECOND CURRENCY            
*        DC    AL1(DISCI2CQ)                                                    
*        DC    AL1(L'LC@CRSR,0),S(LC@CRSR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSDSQ)          - DISCOUNT SECOND CURRENCY                
*        DC    AL1(DISCI2CQ)                                                    
*        DC    AL1(L'LC@DSCR,0),S(LC@DSCR,0)                                    
*        DC    AL3(DISALCQ+BVOPEQ)                                              
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSDIQ)        $  - DIFFERENCE PORTION 2ND CURR             
*        DC    AL1(DISCI2CQ)                                                    
*        DC    AL1(L'LC@DFRNC,0),S(LC@DFRNC,0)                                  
*        DC    AL3(BVOPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSADQ)        '  - ADVANCE PORTION 2ND CURR                
*        DC    AL1(DISCI2CQ)                                                    
*        DC    AL1(L'LC@AVNPA,0),S(LC@AVNPA,0)                                  
*        DC    AL3(BVOPEQ)                                                      
*        DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
*        DC    AL1(41)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(42)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(43)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(44)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(45)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(46)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*&&                                                                             
*        DC    AL1(47)             N/D                                          
*        DC    AL1(0)                                                           
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLHSTQ)        *  (DDS)                                     
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@STAT,0),S(LC@STAT,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLSEQQ)        <  (DDS)                                     
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC3SUBR,0),S(LC3SUBR,0)                                    
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLDADQ)        (  (DDS)                                     
*        DC    AL1(0)                                                           
*        DC    AL1(L'LC@ADR,0),S(LC@ADR,0)                                      
*        DC    AL3(DISALLQ)                                                     
*        DC    AL1(0)                                                           
*                                                                               
*ISTABN  EQU   (*-DISTAB)/DISTABL                                               
*ISTABX  DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
* NOTE: THIS TABLE MUST BE COVERED BY DISTABD, SO ENTRIES HAVE THE SAME         
* STRUCTURE AS DISTAB ENTRIES.  THE TABLE IS USED WHEN THE DISTAB ENTRY         
* FOR A COLUMN HAS VALID ACTIONS, BUT DISLEN IS ZERO, TO INDICATE THAT          
* HEADING TEXT VARIES BY ACTION.  THE ENTRIES FOR A COLUMN IN SUDTAB            
* MUST COVER ALL ACTIONS ALLOWED BY THE MAIN DISTAB ENTRY.                      
*                                                                               
*UDTAB   DS    0H                  SUBSTITUTE DISPLAY COLUMN TABLE              
*        DC    AL1(COLUSEQ,0)      CONTRA'D/OFFSET (CO,GO)                      
*        DC    AL1(L'LC8CTRD,0),S(LC8CTRD,0)                                    
*        DC    AL3(COOPEQ+GOOPEQ)                                               
*        DC    AL1(0)                                                           
*&&UK                                                                           
*        DC    AL1(COLUSEQ,0)      PAID (CC,BV)                                 
*        DC    AL1(L'LC@PAID,0),S(LC@PAID,0)                                    
*        DC    AL3(CCOPEQ+BVOPEQ)                                               
*        DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
*        DC    AL1(COLUSEQ,0)      USED DATE (CC, BV)                           
*        DC    AL1(L'LC@USDT,0),S(LC@USDT,0)                                    
*        DC    AL3(CCOPEQ+BVOPEQ)                                               
*        DC    AL1(0)                                                           
*&&                                                                             
*        DC    AL1(COLDSCQ,0)      DISCOUNT (CREDITOR, BV)                      
*        DC    AL1(L'LC@DSCR,0),S(LC@DSCR,0)                                    
*        DC    AL3(DISALCQ+BVOPEQ)                                              
*        DC    AL1(0)                                                           
*&&UK                                                                           
*        DC    AL1(COLDSCQ,0)        GROSS (MR)                                 
*        DC    AL1(L'LC@GRSR,0),S(LC@GRSR,0)                                    
*        DC    AL3(MROPEQ)                                                      
*        DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
*        DC    AL1(COLDSCQ,0)      DISCOUNT (GENERAL/REVERSE)                   
*        DC    AL1(L'LC@DSCR,0),S(LC@DSCR,0)                                    
*        DC    AL3(GROPEQ)                                                      
*        DC    AL1(0)                                                           
*&&                                                                             
*        DC    AL1(COLFWKQ,0)      WORKCODE (CREDITOR, GENERAL, BV)             
*        DC    AL1(L'LC@WC,0),S(LC@WC,0)                                        
*        DC    AL3(DISALCQ+DISALGQ+BVOPEQ)                                      
*        DC    AL1(0)                                                           
*&&UK                                                                           
*        DC    AL1(COLFWKQ,0)      TYPE (MR)                                    
*        DC    AL1(L'LC@WC,0),S(LC@TYPE,0)                                      
*        DC    AL3(MROPEQ)                                                      
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLNARQ,0)      NARRATIVE (ALL EXCEPT MR)                    
*        DC    AL1(L'LC@NRTV,0),S(LC@NRTV,0)                                    
*        DC    AL3(DISALLQ-MROPEQ)                                              
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(COLNARQ,0)      BOOKING DETAILS (MR)                         
*        DC    AL1(L'LC@BKGDT,0),S(LC@BKGDT,0)                                  
*        DC    AL3(MROPEQ)                                                      
*        DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
*MN                                                                             
*        DC    AL1(33,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(34,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*MN                                                                             
*                                                                               
*        DC    AL1(35,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(36,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(37,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*&&                                                                             
*        DC    AL1(38,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(39,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(40,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(41,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(42,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(43,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(44,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(45,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(46,0)           N/D                                          
*        DC    AL1(0,0),S(0,0)                                                  
*        DC    AL3(0)                                                           
*        DC    AL1(0)                                                           
*                                                                               
*        DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
PROFDFT  DS    0H                  **DEFAULT SORT & DISPLAY PROFILES**          
                                                                                
         DC    C'WH'                                     ACTALFA                
*&&UK*&& DC    AL1(02,04,03,01,06,05,00,00)              KEYDISPS               
*&&UK*&& DC    AL1(11,05,04,02,07,09,00,00,00,00,00,00)  DISDISPS               
*&&US*&& DC    AL1(01,02,04,03,06,05,00,00)                                     
*&&US*&& DC    AL1(02,11,05,04,07,09,00,00,00,00,00,00)                         
*&&UK                                                                           
         DC    C'WN'                                                            
         DC    AL1(08,02,04,03,01,05,00,00)                                     
         DC    AL1(11,05,04,02,07,09,08,00,00,00,00,00)                         
                                                                                
         DC    C'CA'                                                            
*        DC    C'VU'                                                            
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(13,05,04,16,07,10,00,00,00,00,00,00)                         
*&&                                                                             
*&&UK*&& DC    C'CS'                                                            
*&&US*&& DC    C'VA'                                                            
*        DC    AL1(00,00,00,00,00,00,00,00)                                     
*        DC    AL1(01,03,04,05,06,07,10,12,00,00,00,00)                         
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,00,00,00,00,00,00,00)                         
                                                                                
*&&UK*&& DC    C'CO'                                                            
*&&US*&& DC    C'VO'                                                            
*        DC    AL1(00,00,00,00,00,00,00,00)                                     
*        DC    AL1(01,03,04,05,06,07,10,12,00,00,00,00)                         
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,00,00,00,00,00,00,00)                         
                                                                                
*&&UK*&& DC    C'CD'                                                            
*&&US*&& DC    C'VD'                                                            
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,19,00,00,00,00,00,00)                         
                                                                                
*&&UK*&& DC    C'CC'                                                            
*&&US*&& DC    C'VC'                                                            
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,00,00,00,00,00,00,00)                         
                                                                                
*&&UK*&& DC    C'CH'                                                            
*&&US*&& DC    C'VH'                                                            
*        DC    AL1(00,00,00,00,00,00,00,00)                                     
*        DC    AL1(01,03,04,05,06,07,10,12,00,00,00,00)                         
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,00,00,00,00,00,00,00)                         
                                                                                
*&&UK*&& DC    C'CM'                                                            
*&&US*&& DC    C'VM'                                                            
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,00,00,00,00,00,00,00)                         
                                                                                
         DC    C'CQ'                                                            
         DC    AL1(04,03,07,05,00,00,00,00)                                     
         DC    AL1(05,04,13,07,10,35,00,00,00,00,00,00)                         
                                                                                
         DC    C'BR'                                                            
         DC    AL1(04,03,02,06,05,00,00,00)                                     
*MN      DC    AL1(14,04,03,07,09,10,00,00,00,00,00,00)                         
         DC    AL1(14,04,03,09,10,07,00,00,00,00,00,00)                         
                                                                                
         DC    C'BV'                                                            
         DC    AL1(04,03,02,06,05,00,00,00)                                     
         DC    AL1(14,04,13,07,10,00,00,00,00,00,00,00)                         
                                                                                
         DC    C'GR'                                                            
         DC    AL1(02,01,08,07,04,00,00,00)                                     
*&&UK*&& DC    AL1(03,23,05,04,07,09,10,00,00,00,00,00)                         
*&&US*&& DC    AL1(03,23,05,04,07,09,10,26,00,00,00,00)                         
                                                                                
         DC    C'GO'                                                            
         DC    AL1(02,04,03,06,05,00,00,00)                                     
         DC    AL1(03,05,04,07,09,10,00,00,00,00,00,00)                         
*&&UK                                                                           
         DC    C'MR'                                                            
         DC    AL1(02,03,04,05,00,00,00,00)                                     
         DC    AL1(31,16,03,04,05,10,00,00,00,00,00,00)                         
*&&                                                                             
         DC    C'WE'                                     ACTALFA                
*&&UK*&& DC    AL1(02,04,03,01,06,05,00,00)              KEYDISPS               
*&&UK*&& DC    AL1(11,05,04,02,07,09,00,00,00,00,00,00)  DISDISPS               
*&&US*&& DC    AL1(01,02,04,03,06,05,00,00)                                     
*&&US*&& DC    AL1(02,11,05,04,07,09,00,00,00,00,00,00)                         
* - - - - - - - - - - - - - - - - - - -  - - --                                 
         DC    C'VT'                                                            
         DC    AL1(00,00,00,00,00,00,00,00)                                     
         DC    AL1(59,60,61,62,63,64,65,66,00,00,00,00)                         
*                                                                               
PROFDFTX DC    AL1(EOT)                                                         
         EJECT                                                                  
WHTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@DRS)                                             
         DC    AL1(37,00),S(LC@HELD)                                            
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
*&&UK                                                                           
WNTOTTAB DS    0H                                                               
         DC    AL1(08,01),S(LC@DRS)                                             
         DC    AL1(37,00),S(LC@MARKD)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
WATOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@DRS)                                             
         DC    AL1(37,00),S(LC@ATHED)                                           
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
CATOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@CRS)                                             
         DC    AL1(37,00),S(LC@ATHED)                                           
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
*&&                                                                             
CSTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@CRS)                                             
         DC    AL1(37,00),S(LC@SELED)                                           
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
COTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@CRS)                                             
         DC    AL1(37,22),S(LC@MARKD)                                           
         DC    AL1(66,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
CBTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@CRS)                                             
         DC    AL1(37,22),S(LC@MARKD)                                           
         DC    AL1(255)                                                         
                                                                                
CDTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@CRS)                                             
         DC    AL1(28,00),S(LC@DSC)                                             
         DC    AL1(47,00),S(LC@TAKEN)                                           
         DC    AL1(66,00),S(LC@NOTAK)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
CCTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@INVS)                                            
         DC    AL1(23,00),S(LC@MARKD)                                           
         DC    AL1(38,00),S(LC@CHKS)                                            
         DC    AL1(53,00),S(LC@MARKD)                                           
         DC    AL1(68,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
CHTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@CRS)                                             
         DC    AL1(37,00),S(LC@HELD)                                            
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
CMTOTTAB DS    0H                                                               
*&&UK                                                                           
         DC    AL1(08,00),S(LC@INVS)                                            
         DC    AL1(23,00),S(LC@MARKD)                                           
         DC    AL1(TOTSALT+38,00),S(LC@EXDIF)                                   
         DC    AL1(38,00),S(LC@DSC)                                             
         DC    AL1(53,00),S(LC13CHK)                                            
         DC    AL1(68,00),S(LC@DFRNC)                                           
         DC    AL1(00,01),S(LC@MARKD)                                           
         DC    AL1(255)                                                         
*&&                                                                             
*&&US                                                                           
         DC    AL1(08,00),S(LC@INVS)                                            
         DC    AL1(28,00),S(LC@MARKD)                                           
         DC    AL1(47,00),S(LC13CHK)                                            
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,01),S(LC@MARKD)                                           
         DC    AL1(255)                                                         
*&&                                                                             
                                                                                
BRTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@DRS)                                             
         DC    AL1(28,00),S(LC@RCND)                                            
         DC    AL1(00,08),S(LC@DRSMK)                                           
         DC    AL1(00,28),S(LC@UNMKD)                                           
         DC    AL1(47,00),S(LC@CRS)                                             
         DC    AL1(66,00),S(LC@RCND)                                            
         DC    AL1(00,48),S(LC@CRSMK)                                           
         DC    AL1(00,68),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
BVTOTTAB DS    0H                                                               
*&&UK                                                                           
         DC    AL1(08,00),S(LC@CHKS)                                            
         DC    AL1(23,00),S(LC@VOID)                                            
         DC    AL1(38,00),S(LC@INVS)                                            
         DC    AL1(TOTSALT+53,00),S(LC@EXDIF)                                   
         DC    AL1(53,00),S(LC@DSC)                                             
         DC    AL1(68,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
*&&                                                                             
*&&US                                                                           
         DC    AL1(08,00),S(LC@CHKS)                                            
         DC    AL1(28,00),S(LC@VOID)                                            
         DC    AL1(47,00),S(LC@INVS)                                            
         DC    AL1(00,00),S(LC@DSC)        NOT DISPLAYED                        
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
*&&                                                                             
                                                                                
GRTOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@DRS)                                             
         DC    AL1(28,00),S(LC@DFRNC)                                           
         DC    AL1(00,08),S(LC@DRSMK)                                           
         DC    AL1(00,28),S(LC@UNMKD)                                           
         DC    AL1(47,00),S(LC@CRS)                                             
         DC    AL1(66,00),S(LC@DFRNC)                                           
         DC    AL1(00,48),S(LC@CRSMK)                                           
         DC    AL1(00,68),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
GOTOTTAB DS    0H                                                               
         DC    AL1(08,0),S(LC@AMT)                                              
         DC    AL1(37,0),S(LC@MARKD)                                            
         DC    AL1(66,0),S(LC@UNMKD)                                            
         DC    AL1(00,22),S(LC@MARKD)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
MRTOTTAB DS    0H                                                               
*&&UK                                                                           
         DC    AL1(08,00),S(LC@BAL)                                             
         DC    AL1(23,00),S(LC@RCND)                                            
         DC    AL1(38,00),S(LC@VSCHD)                                           
         DC    AL1(53,00),S(LC@WRTNF)                                           
         DC    AL1(68,00),S(LC@XFRD)                                            
         DC    AL1(00,22),S(LC@CRSMK)                                           
         DC    AL1(00,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
*                                                                               
WETOTTAB DS    0H                                                               
         DC    AL1(08,00),S(LC@DRS)                                             
         DC    AL1(37,22),S(LC@MARKD)                                           
         DC    AL1(66,54),S(LC@UNMKD)                                           
         DC    AL1(255)                                                         
                                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALID COUNTRY LISTS FOR TYPE/ACTIONS WITH COUNTRY RESTRICTIONS      *         
***********************************************************************         
                                                                                
*&&UK                                                                           
WNCTRYS  DC    AL1(CTRYGER)                                                     
         DC    AL1(0)              ** MAY BE PATCHED FOR TESTING **             
         DC    AL1(EOT)                                                         
                                                                                
WACTRYS  DC    AL1(CTRYGBR)                                                     
         DC    AL1(CTRYGER)                                                     
         DC    AL1(CTRYHOL)                                                     
         DC    AL1(EOT)                                                         
                                                                                
CACTRYS  DC    AL1(CTRYGBR)                                                     
         DC    AL1(CTRYGER)                                                     
         DC    AL1(CTRYHOL)                                                     
         DC    AL1(EOT)                                                         
*&&                                                                             
CDCTRYS  DC    AL1(CTRYGBR)                                                     
         DC    AL1(CTRYGER)                                                     
         DC    AL1(CTRYHOL)                                                     
         DC    AL1(EOT)                                                         
                                                                                
CCCTRYS  DC    AL1(CTRYGBR)                                                     
         DC    AL1(CTRYGER)                                                     
         DC    AL1(CTRYHOL)                                                     
         DC    AL1(EOT)                                                         
                                                                                
GDCTRYS  DC    AL1(CTRYHOL)                                                     
         DC    AL1(EOT)                                                         
                                                                                
GACTRYS  DC    AL1(CTRYGER)                                                     
         DC    AL1(CTRYGBR)                                                     
         DC    AL1(EOT)                                                         
*&&UK                                                                           
MRCTRYS  DC    AL1(CTRYHOL)                                                     
         DC    AL1(EOT)                                                         
*&&                                                                             
         EJECT                                                                  
REPSPEC  DS    0X                  ** REPORT SPECS **                           
         SPROG 0,1                                                              
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPROG 0                                                                
         SPEC  H1,18,AC#MUMTR,48,C                                              
         SPEC  H2,18,AC#MUMTR,48,CU                                             
         SPROG 1                                                                
         SPEC  H1,18,AC#DRMUM,48,C                                              
         SPEC  H2,18,AC#DRMUM,48,CU                                             
                                                                                
         DC    AL1(EOT)            ** SPEC END MARKER **                        
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACMRKWRK                                                       
       ++INCLUDE ACMRKCCD          CREDITOR/CHEQUE DSECT REQUIRED               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
* FAXTRAINF                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
* ACPAY2JD                                                                      
*        PRINT OFF                                                              
*      ++INCLUDE ACPAY2JD                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101ACMRK00   06/12/18'                                      
         END                                                                    
