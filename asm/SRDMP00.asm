*          DATA SET SRDMP00    AT LEVEL 025 AS OF 02/11/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE T15D00A                                                                  
*INCLUDE KHDUMMY                                                                
*&&      SET   NOP=N                                                            
         TITLE '$ND - ONLINE DUMP DISPLAY PROGRAM'                              
         PRINT NOGEN                                                            
DUMP     CSECT                                                                  
         NMODL WORKL,**$ND***,CLEAR=YES,RR=RE                                   
         USING WORKD,RC                                                         
HDR      USING DMPHDRD,DHDR                                                     
         ST    RE,RELO                                                          
         ST    R1,SAVER1           SAVE INCOMING PARAMETER LIST                 
         MVC   IPARMS,0(R1)                                                     
         ST    RD,SAVERD                                                        
*                                                                               
         L     R9,ATWA                                                          
         USING T15DFFD,R9          R9=A(TWA)                                    
         LHI   R8,COMMON-DUMP                                                   
         AR    R8,RB                                                            
         USING COMMON,R8           R8=A(COMMON CODE AND CONSTANTS)              
         BRAS  RE,INIT             INITIALISE PARAMETERS                        
*                                                                               
MAIN02   XC    START,START         RESET PARAMETERS                             
         XC    DISPLACE,DISPLACE                                                
*                                                                               
         BRAS  RE,VALP1            VALIDATE P1 FIELD                            
         BL    XMOD                                                             
         BRAS  RE,VALP2            VALIDATE P2 FIELD                            
         BL    XMOD                                                             
         BRAS  RE,VALP3            VALIDATE P3 FIELD                            
         BL    XMOD                                                             
         BRAS  RE,VALP4            VALIDATE P4 FIELD                            
         BL    XMOD                                                             
*                                                                               
         LA    R0,SRVP2H                                                        
         ST    R0,FADRH                                                         
*                                                                               
         CLC   DUMPNUM,=AL2(DUMPNUMA)                                           
         BNE   MAIN04                                                           
         BRAS  RE,DISALL                                                        
         BH    MAIN02              HIGH MEANS SOMETHING WAS SELECTED            
         B     XMOD                                                             
*                                                                               
MAIN04   BRAS  RE,GETHDR           EXTRACT DUMP HEADER                          
         BNE   XMOD                                                             
         BRAS  RE,RDTWAB           READ TWA 11 (SAVED STORAGE)                  
         BNE   XMOD                                                             
*                                                                               
         ICM   R3,15,AKEYWORD      KEYWORD SET?                                 
         BZ    MAIN06              NO                                           
         USING KEYTABD,R3                                                       
         TM    KEYSRCE,KEYSCMD     SPECIAL ACTION COMMAND?                      
         BZ    MAIN06              NO                                           
*                                                                               
         BRAS  RE,SVDUMP           ACTION IS SAVE/RELEASE DUMP                  
         BNH   XMOD                                                             
         BRAS  RE,WRFL             ACTION IS SET STATUS FLAG                    
         BNH   XMOD                                                             
         BRAS  RE,SUPPRESS         ACTION IS SUPPRESS/ALLOW DUMPS               
         BNH   XMOD                                                             
         BRAS  RE,SRXREGS          ACTION IS SET REGISTERS                      
         BNH   XMOD                                                             
         BRAS  RE,SXREGS           ACTION IS DISPLAY RD POINTER                 
         BNH   XMOD                                                             
         BRAS  RE,INDIRECT         ACTION IS GET INDIRECT START ADDRESS         
         BNH   XMOD                                                             
         BRAS  RE,PGOTO            ACTION IS GOTO PLIST ADDRESS                 
         BNH   XMOD                                                             
         BRAS  RE,GOTO             ACTION IS GOTO LIST ADDRESS                  
         BNH   XMOD                                                             
*NOP     BRAS  RE,GOPSW            ACTION IS GO PSW ADDRES                      
*NOP     BNH   XMOD                                                             
         BRAS  RE,RDCHAIN          ACTION IS DISPLAY RD CHAIN                   
         BE    MAIN02              SOMETHING SELECTED                           
         BL    XMOD                JUST EXIT                                    
         BRAS  RE,LIST             ACTION IS PROCESS LIST SCREEN                
         BE    MAIN02              SOMETHING SELECTED                           
         BL    XMOD                JUST EXIT                                    
         DROP  R3                                                               
*                                                                               
MAIN06   BRAS  RE,KWSTART          GET START FROM A KEYWORD (IF SET)            
         BL    XMOD                                                             
*                                                                               
MAIN10   ICM   R3,15,AKEYWORD      CHECK SPECIAL KEYWORDS                       
         BZ    MAIN12                                                           
         USING KEYTABD,R3                                                       
         ICM   RF,15,KEYSRTN       KEYWORD HAS STAND ALONE PROCESSING?          
         BZ    MAIN12              NO                                           
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     XMOD                                                             
*                                                                               
MAIN12   GOTO1 ACALLOV,DMCB,(X'FE',SRVTAGH),0                                   
         CLI   4(R1),255           ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FE'                                       
         CLI   NDLSSCRN-NDSAVED(RF),X'FE'                                       
         BNE   MAIN14                                                           
         CLI   DOMRGE,C'Y'         ALREADY MERGED FE SCREEN?                    
         BNE   MAIN14              YES                                          
         BRAS  RE,SCRMRGE          MERGE IT                                     
         MVI   DOMRGE,C'N'                                                      
*                                                                               
MAIN14   BRAS  RE,CHKFLDS          PROCESS INPUT TO FF SCREEN FIELDS            
         BH    MAIN02                                                           
         BL    XMOD                                                             
*                                                                               
         MVC   BYTE,DUMPDSPC                                                    
         MVI   DUMPDSPC,0          MAKE SURE HOME ADDRESS SPACE HERE            
         BRAS  RE,DISPREGS         DISPLAY DUMP REGISTERS                       
         MVC   DUMPDSPC,BYTE                                                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,DUMPREG        GET DUMP REGISTER IF SET                     
         BZ    MAIN16                                                           
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         LA    RF,SETREGS(RF)                                                   
         L     R0,START            ADJUST START POINT FOR REGISTER              
         A     R0,0(RF)                                                         
         ST    R0,START                                                         
         CLI   XREGS,C'Y'                                                       
         BE    MAIN16                                                           
*                                                                               
*        CLI   DUMPREG,X'0C'       RB NEVER IN XA                               
*        BNE   *+8                                                              
*        MVI   START,0                                                          
*        TM    START,X'80'         IF X'80' BIT ON LIKELY NOT XA                
*        BO    *+8                                                              
*        MVI   START,0                                                          
*                                                                               
MAIN16   CLI   PFKEY,0             SCROLL PFKEYS                                
         BE    *+8                                                              
         BRAS  RE,DOPFKEY                                                       
*                                                                               
         TM    DMPFLGS1,DMPDSCTD   CALL HABER'S DSECT DISPLAYER?                
         BZ    *+12                NO                                           
         BRAS  RE,DSCDISP                                                       
         B     XMOD                                                             
*                                                                               
         OC    START,START         START POINT SET?                             
         BNZ   MAIN18                                                           
         MVC   START,DISPLACE      USING PURE DISPLACEMENT - CAN SCROLL         
         XC    DISPLACE,DISPLACE                                                
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+(FHIL-FHD),0                                              
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+(FHIL-FHD),0                                              
*                                                                               
         GOTO1 AHEXOUT,DMCB,START,DUB,4                                         
         BRAS  RE,STRIP0                                                        
         XR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         BZ    MAIN18                                                           
         BCTR  RF,0                                                             
         MVC   SRVP2(0),DUB                                                     
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,SRVP2H+(FHIL-FHD)                                             
*                                                                               
MAIN18   BRAS  RE,SRCH             PROCESS SEARCH                               
         BRAS  RE,DISSCRN          DISPLAY DUMP REGISTERS                       
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION ROUTINE                                              *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVI   HDRN,1              SET DEFAULT MESSAGE                          
         MVI   DOMRGE,C'Y'         OK TO DO MERGE SO FAR                        
         MVC   SRVP1F,DIS1HMSG     SET DEFAULT OPTION FIELD HEADINGS            
         MVC   SRVP2F,DIS2HMSG                                                  
         MVC   SRVP3F,DIS3HMSG                                                  
         MVC   SRVP4F,DIS4HMSG                                                  
*                                                                               
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         ST    RF,AERRMSGS                                                      
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         ST    RF,AOKMSGS                                                       
         L     RF,=A(KEYTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,AKEYTAB                                                       
*                                                                               
         L     RF,ACOMFACS         EXTRACT COMFACS INFO                         
         USING COMFACSD,RF                                                      
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ADICTATE,CDICTATE                                                
         MVC   AXSORT,CXSORT                                                    
         MVC   ADATCON,CDATCON                                                  
         MVC   AGETHELP,CGETHELP                                                
         MVC   AGETTXT,CGETTXT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADATTIM,CDATTIM                                                  
         MVC   AGETFACT,CGETFACT                                                
         MVC   AHELLO,CHELLO                                                    
         MVC   CUREDIT,CCUREDIT                                                 
         DROP  RF                                                               
*                                                                               
         L     RF,ASYSFACS         EXTRACT SYSFACS INFO                         
         USING SYSFACD,RF                                                       
         MVC   ADMGR,VDATAMGR                                                   
         MVC   ASSB,VSSB                                                        
         MVC   ATCB,VTCB                                                        
         MVC   ADADDS,VDADDS                                                    
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ADARPT,VDARPT                                                    
         MVC   ARDID,VRDID                                                      
         MVC   AWTID,VWTID                                                      
         MVC   ADMPFILE,VDMPFILE                                                
         MVC   ASELIST,VSELIST                                                  
         MVC   ALNKTAB,VLNKTAB                                                  
         DROP  RF                                                               
*                                                                               
         USING SSBD,R1                                                          
         L     R1,ASSB                                                          
         MVC   MYSYSN4,SSBSYSN4    SYSTEM ID NAME (4 CHR)                       
         MVC   MYSYSN1,SSBSYSN1    SYSTEM ID NAME (1 CHR)                       
         MVC   MYSYSFL,SSBSYSFL    SYSTEM ID FLAGS                              
         MVC   MYSYSCH,SSBSYSCH    SYSTEM ID CHR FOR MSGS                       
         MVC   MYDMPCY,SSBDMPCY    CYLINDERS/DUMP                               
         MVC   MYDMPSV,SSBDMPSV    SAVE SSB STOP FLAG                           
         MVC   RECLEN,SSBTWAL                                                   
         MVC   AFACITAB,SSBAFID    A(FACIDTAB)                                  
         MVC   ACTRYTAB,SSBACTRY                                                
         MVC   ALANGTAB,SSBALANG                                                
         MVC   TBALET,SSBTBLET     TABS DATASPACE TBALET                        
         XC    TBOFFS,TBOFFS                                                    
         DROP  R1                                                               
*                                                                               
*&&US*&& CLC   =C'FQA ',MYSYSN4    FQA IS A TEST SYSTEM BUT IT IS               
*&&US*&& BNE   *+8                 FLAGGED LIVE FOR HISTORIC REASONS            
*&&US*&& OI    MYSYSFL,FACITST                                                  
*                                                                               
         TM    MYSYSFL,FACITST                                                  
         JNO   INIT20                                                           
         BRAS  RE,BLDFIDTT         BUILD A TEST FACIDTAL                        
         JNE   INIT20              -- OLD TABLE, NO OVERRIDE NEEDED             
         LAY   RE,FACIDTT                                                       
         ST    RE,AFACITAB         OVERRIDE A(FACITAB)                          
*                                                                               
INIT20   DS    0H                                                               
         GOTO1 ADATCON,DMCB,(5,0),(3,TODAY3)                                    
         GOTO1 (RF),(R1),(5,0),(2,TODAYB)                                       
*                                                                               
         L     RF,=A(INA-WORKD)                                                 
         AR    RF,RC                                                            
         ST    RF,AINA                                                          
         L     RF,=A(INBACK-WORKD)                                              
         AR    RF,RC                                                            
         ST    RF,AINBACK                                                       
         LHI   RF,SORTBLK-WORKD                                                 
         AR    RF,RC                                                            
         ST    RF,ASORTBLK                                                      
         LHI   RF,IO-WORKD                                                      
         AR    RF,RC                                                            
         ST    RF,AIO                                                           
         LHI   RF,SVBLOCK-WORKD                                                 
         AR    RF,RC                                                            
         ST    RF,ASVBLOCK                                                      
*                                                                               
         MVC   MAXTWAL,=H'6144'                                                 
*                                                                               
         USING TIOBD,R6                                                         
         L     R6,ATIOB            GET PFKEY (IF PRESSED)                       
         XR    RF,RF                                                            
         ICM   RF,1,TIOBAID                                                     
         CHI   RF,12               CONVERT PFK13-24 TO PFK1-12                  
         BNH   *+8                                                              
         AHI   RF,-12                                                           
         STC   RF,PFKEY            AND SAVE IT                                  
         ICM   RF,3,TIOBCURS       DISPLACEMENT TO FIELD FOR CURSOR             
         STCM  RF,3,SCURSOR                                                     
*                                                                               
         USING UTLD,R6                                                          
         L     R6,AUTL             EXTRACT UTL INFO                             
         MVC   TRMNUM,TNUM                                                      
         MVI   DDS,0                                                            
         TM    TSTAT,TSTATDDS      DDS TERMINAL?                                
         BZ    *+8                                                              
         MVI   DDS,DDSTRM          YES SET FLAG                                 
         MVI   TSVCREQ,X'00'       CLR SRV REQ SCREEN FLAG                      
*                                                                               
         USING DTFPHD,R6           TEMPLATE FOR DIRECT ACCESS FILES             
         L     R6,ADMPFILE         A(DMPFILE) OF DATAMGR SYSTEM FILES           
         XC    P1(24),P1           CLEAR OUT THE PARAMETERS (P?)                
         MVC   P1,ADARPT           DIRECT ACCESS REPORT? (FILE ATTRIB)          
         MVC   P3+2(2),=AL2(RLEN)  RECORD LENGTH                                
         ST    R6,P4               A(DUMP FILE)                                 
         GOTO1 ADADDS,P1           GET FILE ATTRIBUTES                          
         OC    P3+2(2),P3+2        ANY RECORDS ON TRACK?                        
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         MVC   ADDRMAX,=X'FFFFFFFF'                                             
*                                                                               
         MVC   RECTRK+2(2),P3+2    SAVE # OF RECS PER TRK                       
         L     RF,P2                                                            
         LH    RF,2(RF)                                                         
         ST    RF,TRKCYL           SAVE TRKS PER CYL                            
*                                                                               
         XC    DPARMS,DPARMS       SET UP DADDS FOR READING                     
         MVC   P1,ARDID            P1 = ARDID                                   
         L     RF,AINA             P2 = A(I/O AREA)                             
         ST    RF,P2                                                            
         XC    P3,P3               P3 = NOT NEEDED (SET BY DADDS)               
         ST    R6,P4               P4 = A(DMPFILE DTF)                          
         LA    RF,ADDR                                                          
         ST    RF,P5               P5 = A(D/A)                                  
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A LOCAL TABLE FOR ONLY TEST FACID ENTRIES                     *         
***********************************************************************         
BLDFIDTT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,AFACITAB                                                      
         LHY   RF,-2(,RE)                                                       
         CHI   RF,X'10'            > FACIDMAX (14 or 15)                        
         JL    EXITL               LOW - OLD TABLE, NO OVERRIDE NEEDED          
*                                                                               
         LY    RF,-4(,RE)          RF=DISPLACEMENT BACK TO INDEX TABLE          
         SR    RE,RF               RE=A(FACID INDEX TABLE)                      
         USING FACIDD,RE                                                        
*                                                                               
         LAY   R2,FACIDTT          R2=A(LOCAL TEST FACIDTAB)                    
         LHI   R0,FACIDTNQ                                                      
*                                  SKIP PRODUCTION DSPACE                       
BFT20    CLI   FACIDSPC,C'A'                                                    
         JE    BFT70                                                            
         CLI   FACIDSPC,C'R'                                                    
         JE    BFT70                                                            
*                                                                               
         USING FACITABD,R1                                                      
         L     R1,FACAID                                                        
BFT30    CLI   0(R1),X'FF'         END OF THIS FACID TABLE                      
         JE    BFT70                                                            
         CLI   FACISN4,C' '        1ST CHAR IS SPACE                            
         JE    BFT40                                                            
         MVC   0(L'FACITAB,R2),0(R1)                                            
         AHI   R2,L'FACITAB                                                     
         BCT   R0,BFT40                                                         
         DC    H'0'                NEED TO INCREASE TABLE SIZE                  
*                                                                               
BFT40    AHI   R1,L'FACITAB                                                     
         B     BFT30                                                            
         DROP  R1                                                               
*                                                                               
BFT70    AHI   RE,FACIDLNQ         NEXT FACID TABLE                             
         CLI   0(RE),X'FF'         END OF FACID INDEX TABLE                     
         JE    BFT90                                                            
         J     BFT20                                                            
         DROP  RE                                                               
*                                                                               
BFT90    DS    0H                  PUT IN 5X'FF',X'80',C'??'                    
         MVC   0(L'FACITAB,R2),=X'FFFFFFFFFF806F6F'                             
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE P1 (DUMP NUMBER) FIELD                                     *         
***********************************************************************         
VALP1    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SRVP1H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0              INPUT TO FIELD?                              
         BNE   VP106               YES                                          
*                                                                               
         LAM   AR0,ARF,ARZERO      GET LATEST DUMP NUMBER                       
         LAM   AR3,AR3,TBALET                                                   
         ICM   R3,15,TBOFFS                                                     
         SAC   512                                                              
         ICM   R3,15,TABSADDR-FATABSD(R3)                                       
         USING TORDUMPD,R3                                                      
         LHI   RF,TDLENQ                                                        
VP102    ICM   R0,15,TDSYSNA       END OF LIST                                  
         BNZ   *+6                                                              
         DC    H'0'                DATASPACE OR SSB CORRUPTED                   
*                                                                               
         CLC   MYSYSN4,TDSYSNA     IS THIS MY FACPAK?                           
         BE    VP104               YES                                          
         BXH   R3,RF,VP102                                                      
*                                                                               
VP104    ICM   R0,15,TDDUMPNO      SAVE START NUMBER                            
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO        GET OUT OF AR MODE                         
         DROP  R3                                                               
*                                                                               
         CURED (R0),(3,SRVP1),0,ALIGN=LEFT,ZERO=NOBLANK                         
         STC   R0,FHIL                                                          
*                                                                               
VP106    GOTO1 ASCANNER,DMCB,FHD,(X'84',SCANBLK)                                
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CHI   R0,4                                                             
         BNH   *+12                                                             
         MVI   FERN,3              TOO MANY PARAMETERS IN FIELD                 
         B     EXITL                                                            
*                                                                               
         LA    R3,SCANBLK                                                       
         USING SCANBLKD,R3                                                      
         CLI   SC2NDLEN,0          CAN'T HAVE SECOND PARAMETER                  
         BE    *+12                                                             
         MVI   FERN,2              INVALID INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         TM    SC1STVAL,SCNUMQ     VALIDATING A NUMBER?                         
         BZ    VP108                                                            
         ICM   RF,15,SC1STNUM                                                   
         STH   RF,DUMPNUM          SET DUMP NUMBER                              
         B     VP112                                                            
*                                                                               
VP108    CLI   SC1STFLD,C'A'       ALL?                                         
         BNE   VP110                                                            
         MVC   SRVP1,SPACES                                                     
         MVC   SRVP1(L'ALL4U),ALL4U                                             
         MVC   DUMPNUM,=AL2(DUMPNUMA)                                           
         B     EXITOK              ALL HAS NO PARAMETERS IN P1                  
*&&DO                                                                           
VP108    XR    RF,RF               ALL?                                         
         ICM   RF,1,SC1STLEN                                                    
         CHI   RF,3                                                             
         BH    VP110                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   VP110                                                            
         CLC   SC1STFLD(0),ALL4U                                                
         MVC   DUMPNUM,=AL2(DUMPNUMA)                                           
         B     EXITOK              ALL HAS NO PARAMETERS IN P1                  
*&&                                                                             
VP110    MVI   FERN,1              INVALID INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
VP112    AHI   R0,-1               NORMAL DISPLAY - JUST NUMBER                 
         BZ    EXITOK                                                           
*                                                                               
         CHI   R0,2                MUST HAVE DUMP#,SYS,LABEL                    
         BNL   VP113                                                            
         AHI   R3,SCBLKLQ          GO PAST DUMP # TO DSPACE                     
         CLI   SCONEFLD,C'D'                                                    
         JNE   *+12                                                             
         MVI   DUMPDSPC,C'D'       SET DSPACE DMGR                              
         J     EXITOK                                                           
*                                                                               
         CLI   SCONEFLD,C'T'                                                    
         JNE   *+12                                                             
         MVI   DUMPDSPC,C'T'       SET DSPACE TABS                              
         J     EXITOK                                                           
*                                                                               
         MVI   FERN,1              INVALID INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
VP113    AHI   R3,SCBLKLQ          GO PAST DUMP # TO SYSTEM                     
         OI    DMPFLGS1,DMPDSCTD   SET CALL HABER'S DSECT DISPLAYER             
*                                                                               
         MVI   DSSYS,0             DEFAULT SYSTEM IS FACPAK                     
         MVI   DSSYSCHR,C' '                                                    
         CLI   SC1STLEN,0          IF SOMETHING INPUT                           
         BE    *+12                                                             
         BRAS  RE,VP1SYS           VALIDATE SYSTEM                              
         BNE   EXITL                                                            
*                                                                               
         AHI   R3,SCBLKLQ          GO PAST SYSTEM TO LABEL                      
         AHI   R0,-1                                                            
         CLI   SC1STLEN,0          TEST AUTO-LOOKUP REQUESTED                   
         BE    VP116                                                            
         CLI   SC1STLEN,8          ENSURE NOT MORE THAN MAX LENGTH              
         BNH   VP114                                                            
         MVI   FERN,10                                                          
         MVC   FERRDSP,SC1STLEN                                                 
         B     EXITL                                                            
*                                                                               
VP114    XR    R1,R1                                                            
         ICM   R1,1,SC1STLEN                                                    
*                                                                               
         LA    RF,SC1STFLD+7       ELSE CLEAR TRAILING DOTS                     
         CLI   0(RF),C'.'                                                       
         BH    *+14                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*                                                                               
VP116    MVC   LABEL,SC1STFLD      SAVE LABEL                                   
         MVC   LLABEL,SC1STLEN     AND L'LABEL                                  
         MVC   DSPTOLBL,SC1STNUM   AND DISPLACEMENT TO LABEL                    
         CLI   DSSYSCHR,C'T'       IF SYSTEM IS TALENT                          
         BNE   VP118                                                            
         CLC   =C'TL',LABEL        TEST FOR RECORD LABEL                        
         BNE   *+8                                                              
         MVI   RECOREL,C'R'        PRESET INDICATOR                             
         CLC   =C'TA',LABEL        ELSE TEST FOR ELEMENT LABEL                  
         BNE   *+8                                                              
         MVI   RECOREL,C'E'        PRESET INDICATOR                             
*                                                                               
VP118    AHI   R0,-1               MAY HAVE (R)ECORD OR (E)LEMENT IND           
         BZ    EXITOK                                                           
         AHI   R3,SCBLKLQ          BUMP PAST SYSTEM TO INDICATOR                
         MVC   RECOREL,SC1STFLD                                                 
         CLI   SC1STFLD,C'R'       RECORD                                       
         BE    EXITOK                                                           
         CLI   SC1STFLD,C'E'       ELEMENT                                      
         BE    EXITOK                                                           
         MVI   FERN,2                                                           
         MVC   FERRDSP,SC1STLEN                                                 
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SYSTEM INPUT FIELD                                         *         
* NTRY: R3     = A(SCAN BLOCK ENTRY)                                  *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VP1SYS   NTR1  ,                                                                
         LA    R2,SYSLST           GET OVERLAY SYSTEM NAME                      
         LH    RE,0(R2)            SET FOR LOOP                                 
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SYSLSTD,R2                                                       
         CLC   SYSLRPLT,SC1STFLD   SEARCH SYSLIST FOR SYSTEM NAME               
         BE    VP1S02                                                           
         BXLE  R2,RE,*-10                                                       
         MVI   FERN,4              INVALID SYSTEM                               
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
VP1S02   MVC   DSSYSCHR,SYSLRPLT   SAVE CHAR. SYSTEM CODE                       
         MVC   DSSYS,SYSLNUM       SAVE BINARY SYSTEM NUMBER                    
*                                                                               
         GOTO1 AHELLO,DMCB,C'AFIL'                                              
         L     R2,0(R1)                                                         
         USING HELEND,R2                                                        
         LHI   RF,HELENL                                                        
VP1S04   CLI   HELEND,255                                                       
         BE    EXITOK                                                           
         CLC   DSSYSCHR,HELNAME    MATCH ON SYSTEM                              
         BE    VP1S06                                                           
         BXH   R1,RF,VP1S04                                                     
         DC    H'00'                                                            
*                                                                               
VP1S06   MVC   DATADISP,HELEDIS    SAVE DISP TO 1ST ELEMENT                     
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE P2                                                         *         
***********************************************************************         
VALP2    NTR1  BASE=*,LABEL=*                                                   
         XC    AKEYWORD,AKEYWORD   RESET A(KEYTAB ENTRY)                        
         MVI   ALLFLVL,0           RESET 'ALL' SCREEN FILTER INPUTS             
*                                                                               
         LA    R2,SRVP2H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
*                                                                               
         CLI   FHIL,0              INPUT OPTIONAL IN THIS FIELD                 
         BE    EXITOK                                                           
         CLC   DUMPNUM,=AL2(DUMPNUMA)                                           
         BNE   *+12                                                             
         BRAS  RE,VFLTALL          VALIDATE FAC/SYS/PRG FILTERS                 
         B     EXIT                                                             
*                                                                               
         CLI   FHDA,C'?'           ASKED FOR HELP?                              
         BNE   VP202               NO                                           
         BRAS  RE,P2HELP                                                        
         BNH   EXITL                                                            
*                                                                               
VP202    BRAS  RE,MOVHEX           IS INPUT VALID HEX (A DISPLACEMENT)          
         BNE   *+14                NO                                           
         MVC   DISPLACE,FULL       SAVE DISPLACEMENT                            
         B     EXITOK                                                           
*                                                                               
         MVI   FERN,0              DON'T CARE ABOUT MOVHEX ERROR                
         XR    RF,RF               TRY TO MATCH KEYWORD                         
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         L     R3,AKEYTAB                                                       
         USING KEYTABD,R3                                                       
*                                                                               
VP206    CLI   KEYWORD,EOTB        EOT?                                         
         BE    VP212                                                            
         CLC   FHIL,KEYMINL        BELOW MINIMUM?                               
         BL    VP208                                                            
         CLC   FHIL,KEYMAXL        ABOVE MAXIMUM?                               
         BH    VP208                                                            
         EX    RF,VP2CLC1          COMPARE AGAINST KEYWORD                      
         BE    VP210                                                            
*                                                                               
VP208    AHI   R3,KEYTABL                                                       
         B     VP206                                                            
*                                                                               
VP2CLC1  CLC   FHDA(0),KEYWORD                                                  
*                                                                               
VP210    ST    R3,AKEYWORD         SET A(KEYWORD ENTRY)                         
         MVC   FHDA(L'KEYWORD),KEYWORD                                          
         MVC   FHOL,KEYMAXL                                                     
         B     EXITOK                                                           
*                                                                               
VP212    MVI   FERN,2                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE 'ALL' SCREEN FILTER FIELDS                                 *         
* INPUT IS ONE OF <FACPAK> OR <SYSTEM> OR <SYSTEM/PROGRAM>            *         
*                                                                     *         
* NTRY: R2       = A(FIELD HEADER)                                    *         
* EXIT: CC EQ    = FILTER VALIDATED AND LEVEL SET                     *         
*       CC NEQ   = INVALID FILTER                                     *         
***********************************************************************         
         USING FHD,R2                                                           
VFLTALL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ASCANNER,DMCB,FHD,(X'82',SCANBLK),C',=/='                        
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CHI   R0,3                                                             
         BL    *+12                                                             
         MVI   FERN,3              TOO MANY PARAMETERS IN FIELD                 
         B     EXITL                                                            
*                                                                               
         LA    R3,SCANBLK          R3 = A(SCANBLK)                              
         USING SCANBLKD,R3                                                      
         CHI   R0,1                SYSTEM/PROGRAM INPUT?                        
         BNE   VFA06               YES                                          
*                                                                               
         CLI   SC2NDLEN,0          DIVIDED FIELD NOT ALLOWED                    
         BE    *+12                                                             
         MVI   FERN,2              INVALID INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CLC   =C'SAVED',SC1STFLD                                               
         BNE   *+12                                                             
         MVI   ALLFLVL,ALLFLVLX                                                 
         B     EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
*                                                                               
         L     R1,AFACITAB         SEE IF IT IS A FACPAK                        
         USING FACITABD,R1                                                      
VFA02    CLI   FACITAB,255         END OF TABLE                                 
         BE    VFA06                                                            
         EX    RF,VFACPAK          MATCH FACPAK NAME?                           
         BE    VFA04               YES                                          
         AHI   R1,L'FACITAB                                                     
         B     VFA02                                                            
*                                                                               
VFACPAK  CLC   FHDA(0),FACISN4                                                  
*                                                                               
VFA04    MVI   ALLFLVL,ALLFLVLF    SET FILTER IS A FACPAK                       
         MVC   ALLFFACL,SC1STLEN                                                
         MVC   ALLFFAC,SC1STFLD                                                 
         B     EXITOK                                                           
         DROP  R1                                                               
*                                                                               
VFA06    MVC   ALLFSYSL,SC1STLEN   COPY SYSTEM FILTER                           
         MVC   ALLFSYS,SC1STFLD                                                 
         MVI   ALLFLVL,ALLFLVLS    SET FILTER IS SYSTEM ONLY                    
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
*                                                                               
         CLC   =C'SPT',SC1STFLD    CHANGE SPTX TO SPOTX                         
         BE    *+14                                                             
         CLC   =C'PRT',SC1STFLD    DITTO FOR PRT                                
         BNE   VFA08                                                            
         AHI   R1,1                INCREMENT LENGTH                             
         MVC   DUB,SC1STFLD                                                     
         MVI   SC1STFLD+2,C'O'     SP(O)TX                                      
         CLC   =C'PRT',DUB                                                      
         BNE   *+8                                                              
         MVI   SC1STFLD+2,C'N'     PR(N)TX                                      
         MVC   SC1STFLD+3(2),DUB+2                                              
*                                                                               
VFA08    L     R4,ASELIST          FILTER SYSTEM INPUT                          
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING SELISTD,R4                                                       
         EX    R1,VFACSYS          MATCH SYSTEM NAME?                           
         BE    VFA10               YES                                          
         BXLE  R4,RE,*-8                                                        
         MVI   FERN,4              INVALID SYSTEM                               
         B     EXITL                                                            
*                                                                               
VFACSYS  CLC   SENAME(0),ALLFSYS                                                
*                                                                               
VFA10    CLC   =C'SPOT',ALLFSYS    CHANGE SPOT1 TO SPT1                         
         BE    *+14                                                             
         CLC   =C'PRNT',ALLFSYS    DITTO FOR PRNT                               
         BNE   VFA12                                                            
         XR    R1,R1               REDUCE LENGTH                                
         IC    R1,ALLFSYSL                                                      
         BCTR  R1,0                                                             
         STC   R1,ALLFSYSL                                                      
         MVC   ALLFSYS+2(2),ALLFSYS+3                                           
         MVI   ALLFSYS+4,C' '                                                   
*                                                                               
VFA12    CHI   R0,1                PROGRAM TO FOLLOW SYSTEM FILTER?             
         BE    EXITOK              NO                                           
         AHI   R3,SCBLKLQ          GO ON TO PROGRAM FILTER                      
*                                                                               
         CLI   SC2NDLEN,0          SECOND VALUE IS INVALID                      
         BNE   VFA14                                                            
*                                                                               
         MVC   ALLFPRGL,SC1STLEN   COPY PROGRAM FILTER                          
         MVC   ALLFPRG,SC1STFLD                                                 
         MVI   ALLFLVL,ALLFLVLP    SET FILTER IS SYSTEM/PROGRAM                 
*                                                                               
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
*                                                                               
         L     R4,SEPGMS           TRY TO MATCH PROGRAM NAME                    
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING PGMLSTD,R4                                                       
         EX    R1,VFACPRG          TRY TO MATCH PROGRAM NAME                    
         BE    EXITOK              MATCH                                        
         BXLE  R4,RE,*-8                                                        
*                                                                               
VFA14    MVI   FERN,5              INVALID PROGRAM FILTER                       
         MVC   FERRDSP,SC1STNUM                                                 
         B     EXITL                                                            
*                                                                               
VFACPRG  CLC   PGMNAME(0),ALLFPRG                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* HELP DISPLAY ROUTINE FOR P2 FIELD                                   *         
***********************************************************************         
P2HELP   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SRVP2H                                                        
         USING FHD,R2                                                           
         LA    R5,DMPLAST          R5 = A(END OF SCREEN)                        
         BCTR  R5,0                                                             
         XR    R4,R4                                                            
*                                                                               
         MVI   BYTE,1                                                           
         CLI   FHDA+1,C'1'         ONLY SUPPORT 2 HELP SCREENS                  
         BL    P2H02                                                            
         CLI   FHDA+1,C'2'                                                      
         BH    P2H02                                                            
         MVC   BYTE,FHDA+1                                                      
         NI    BYTE,X'0F'          BYTE HOLDS HELP SCREEN NUMBER                
*                                                                               
P2H02    GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         LA    R2,DMPL1H           POINT TO FIRST DATA FIELD                    
         XR    RE,RE                                                            
         ICM   RE,1,FHLN                                                        
         AHI   RE,-(FHDAD)         LENGTH OF DATA FIELD                         
         SRDL  RE,32                                                            
         LHI   R0,L'KEYWORD+1                                                   
         STH   R0,FULL             FULL+0(2) = LENGTH TO ADD FOR NEXT           
         DR    RE,R0                                                            
         STH   RF,FULL+2           FULL+2(2) = NUMBER THAT FIT ON LINE          
*                                                                               
         L     R3,AKEYTAB          KEYWORDS FOR FACPAK ADDRESSES                
         USING KEYTABD,R3                                                       
P2H04    LH    R0,FULL+2           R0 = NUMBER OF COLUMNS                       
         XR    RF,RF               RF = DISPLACEMENT INTO LINE                  
*                                                                               
P2H06    LA    R2,DMPL1H           R2 = CURRENT LINE                            
*                                                                               
P2H08    CLI   KEYWORD,EOTB        EOT?                                         
         BE    P2H14                                                            
         LA    RE,FHDA(RF)         RE = CURRENT POSITION ON LINE                
         MVC   0(L'KEYWORD,RE),KEYWORD                                          
         AHI   R3,KEYTABL                                                       
         IC    R4,FHLN             NEXT ROW IN COLUMN                           
         BXLE  R2,R4,P2H08                                                      
*                                                                               
         AH    RF,FULL             NEXT COLUMN                                  
         BCT   R0,P2H06                                                         
*                                                                               
P2H10    XR    R0,R0               BYTE HOLDS HELP PAGE NUMBER                  
         IC    R0,BYTE                                                          
         AHI   R0,-1                                                            
         BNP   P2H14                                                            
*                                                                               
         LA    R2,DMPL1H           CLEAR OUT SCREEN                             
P2H12    NI    FHAT,255-FHATHI     TURN OFF HIGLIGHTS                           
         IC    R4,FHLN                                                          
         LHI   RF,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LHI   RF,-(FHDAD+FHDAD+1)                                              
         SR    R4,RF                                                            
         MVC   FHDA(0),SPACES      CLEAR FIELD                                  
         EX    R4,*-6                                                           
         AR    R4,RF                                                            
         BXLE  R2,R4,P2H12                                                      
         B     P2H04               KEEP DISPLAYING DATA                         
*                                                                               
P2H14    CLI   PFKEY,2             'SELECT' CURSOR PRESSED                      
         BE    P2H16               YES                                          
         LA    R2,SRVP2H                                                        
         MVC   FHDA(3),=CL3'?  '   SET ?(PAGE #)                                
         MVC   FHDA+1(1),BYTE                                                   
         OI    FHDA+1,X'F0'                                                     
         MVI   FHOL,3                                                           
*                                                                               
         LA    RF,DMPL1H           SET SELECT WITH PFK MESSAGE                  
         ST    RF,FADRH                                                         
         MVI   FERN,0                                                           
         MVI   HDRN,3                                                           
         B     EXITOK                                                           
*                                                                               
P2H16    LA    R2,DMPL1H           FIND FIELD FOR CURSOR                        
         CLC   SCURSOR,FHAD                                                     
         BL    P2H20                                                            
         ST    R2,FULL1                                                         
*                                                                               
P2H18    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    P2H22                                                            
         ST    R2,FULL1                                                         
         IC    R4,FHLN                                                          
         BXLE  R2,R4,P2H18                                                      
*                                                                               
P2H20    LA    R2,SRVP2H                                                        
         MVC   FHDA(3),=CL3'?  '   SET ?(PAGE #)                                
         MVC   FHDA+1(1),BYTE                                                   
         OI    FHDA+1,X'F0'                                                     
         MVI   FHOL,3                                                           
         B     P2H24               INVALID CURSOR POSITION                      
*                                                                               
P2H22    L     R2,FULL1            CURSOR FIELD IS IN FULL1                     
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    RE,SCURSOR                                                       
         SR    RE,RF               GET DISP INTO FIELD                          
         SRDL  RE,32                                                            
         LH    R1,FULL                                                          
         DR    RE,R1                                                            
         MH    RF,FULL             GET 1ST CHARACTER OF THIS ENTRY              
         LA    RF,FHDA(RF)                                                      
*                                                                               
         LH    R1,FULL             R1 = L'KEYWORD+1                             
         AHI   R1,-2                                                            
         EX    R1,P2HCLC1                                                       
         BNH   P2H24                                                            
         MVC   SRVP2,SPACES                                                     
         MVC   SRVP2(0),0(RF)                                                   
         EX    R1,*-6                                                           
         LA    R2,SRVP2H                                                        
         ST    R2,FADRH                                                         
*                                                                               
         LA    RF,SRVP2+L'SRVP2-1  SET LENGTH OF INPUT FIELD                    
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,1                                                             
         LA    R0,SRVP2                                                         
         SR    RF,R0                                                            
         STC   RF,FHIL                                                          
         MVI   PFKEY,0                                                          
         B     EXITH               SHOW SELECT DONE                             
*                                                                               
P2HCLC1  CLC   0(0,RF),SPACES                                                   
*                                                                               
P2H24    MVI   FERN,8              INVALID CURSOR POSITION                      
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE P3                                                         *         
***********************************************************************         
VALP3    NTR1  BASE=*,LABEL=*                                                   
         XC    DUMPREG,DUMPREG                                                  
*                                                                               
         LA    R2,SRVP3H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
*                                                                               
         CLC   DUMPNUM,=AL2(DUMPNUMA)                                           
         BE    EXITOK              'ALL' DUMPS IGNORE THIS FIELD                
*                                                                               
         ICM   RF,15,AKEYWORD      USING A KEYWORD?                             
         BZ    VP300               NO                                           
         USING KEYTABD,RF                                                       
         TM    KEYSRCE,KEYSVP3     KEYWORD VALIDATES P3 ITSELF?                 
         BO    EXITOK              YES                                          
         CLI   FHIL,0              INPUT?                                       
         BE    EXITOK              NO - OK FOR A KEYWORD                        
         TM    KEYSRCE,KEYSCMD     SPECIAL ACTION COMMAND?                      
         BZ    VP300               NO                                           
         CLI   KEYACT,KEYAIND      INDIRECT (OFF REGISTER)?                     
         BE    VP314               NO                                           
         DROP  RF                                                               
*                                                                               
VP300    OC    DISPLACE,DISPLACE   ANY DISPLACEMENT SET?                        
         BZ    VP302                                                            
         CLI   FHIL,0              YES - THEN NO INPUT IS OK                    
         BE    EXITOK                                                           
*                                                                               
VP302    XR    RF,RF               REGISTER CAN BE (R)N OR XN                   
         ICM   RF,1,FHIL           R DENOTES 24 BIT ADDRESS - OPTIONAL          
         BNZ   VP304                                                            
         CLI   SRVP2H+FHIL-FHD,0   SET DEFAULT IF NOTHING INPUT                 
         BNE   VP304                                                            
*                                                                               
         MVI   FHDA,C'B'           SET DEFAULT TO RB                            
         MVI   FHIL,1                                                           
         LHI   RF,1                                                             
*                                                                               
VP304    LA    R1,FHDA             X DENOTES 31 BIT ADDRESS - REQUIRED          
         MVI   XREGS,C'N'                                                       
         CLI   0(R1),C'R'                                                       
         BNE   VP306                                                            
         AHI   R1,1                                                             
         BCTR  RF,0                                                             
         B     VP308                                                            
*                                                                               
VP306    CLI   0(R1),C'X'                                                       
         BNE   VP308                                                            
         MVI   XREGS,C'Y'                                                       
         AHI   R1,1                                                             
         BCTR  RF,0                                                             
*                                                                               
VP308    CHI   RF,1                LENGTH MUST BE 1 FOR A REGISTER              
         BNE   VP314                                                            
*                                                                               
         LA    RE,REG2NUM          MATCH IN REGISTER LIST                       
VP310    CLI   0(RE),EOTB                                                       
         BE    VP312                                                            
         CLC   0(1,R1),0(RE)                                                    
         BE    *+12                                                             
         AHI   RE,2                                                             
         B     VP310                                                            
*                                                                               
         MVC   DUMPREG,1(RE)       SET REGISTER DISPLACEMENT                    
         B     EXITOK                                                           
*                                                                               
VP312    TM    FHII,FHIIHE         IF HEX, TRY FOR DISPLACEMENT INSTEAD         
         BO    VP314                                                            
         LA    RF,FHDA                                                          
         SR    R1,RF                                                            
         STC   R1,FERRDSP                                                       
         MVI   FERN,12             NOT A VALID REGISTER                         
         B     EXITL                                                            
*                                                                               
VP314    XC    DUMPREG,DUMPREG     NOT A REGISTER                               
         BRAS  RE,MOVHEX           GET HEX FIELD (IF INPUT)                     
*NOP     BNE   EXITL                                                            
         MVI   FERN,0              DON'T CARE ABOUT MOVHEX ERROR                
         L     RF,FULL                                                          
         A     RF,DISPLACE         INCREASE DISPLACEMENT                        
         ST    RF,DISPLACE                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE P4                                                         *         
***********************************************************************         
VALP4    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SRVP4H                                                        
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0              INPUT IS OPTIONAL IN THIS FIELD              
         BE    EXITOK                                                           
*                                                                               
         CLC   DUMPNUM,=AL2(DUMPNUMA)                                           
         BNE   VP402                                                            
         TM    FHII,FHIINU         ALL DUMPS HAVE START NUMBER                  
         BO    *+12                                                             
         MVI   FERN,6              FIELD NOT NUMERIC                            
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,VP4PACK                                                       
         CVB   RF,DUB                                                           
         STH   RF,ASNUM                                                         
         B     EXITOK                                                           
*                                                                               
VP4PACK  PACK  DUB,FHDA(0)                                                      
*                                                                               
VP402    CLC   FHDA(4),=C'DMGR'    P4 CAN BE TABS OR DMGR                       
         BE    VP402A                                                           
         CLC   FHDA(4),=C'TABS'                                                 
         BE    VP402B                                                           
         B     VP403                                                            
*                                                                               
VP402A   MVI   DUMPDSPC,C'D'       SET DSPACE                                   
         B     *+8                                                              
VP402B   MVI   DUMPDSPC,C'T'                                                    
         LA    RF,SRVL1S1H         SET CURSOR AFTER P4                          
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
*                                                                               
VP403    CLC   FHDA(2),=C'C'''   * ONLY 2 VALID TYPES C'...'                    
         BNE   VP404                                                            
         CLI   FHIL,3              THREE CHARACTERS OR LESS?                    
         BH    *+12                INVALID - NO ARGUMENT                        
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHIL             WHOLE ARGUMENT IN FIELD                      
         BCTR  R1,0                                                             
         LA    RF,FHDA(R1)         POINT TO LAST CHARACTER                      
         CLI   0(RF),C''''         IS THERE AND END QUOTE?                      
         BE    *+12                NEED AN END QUOTE                            
         MVI   FERN,25                                                          
         B     EXITL                                                            
*                                                                               
         AHI   R1,-2               MINUS LENGTH OF C''                          
         STC   R1,SRCHLEN                                                       
         BCTR  R1,0                                                             
         MVC   SRCHSTR(0),FHDA+2   SKIP THE C' AND COPY STRING                  
         EX    R1,*-6                                                           
         B     EXITOK                                                           
*                                                                               
VP404    CLC   FHDA(2),=C'X'''   * AND X'...'                                   
         BNE   VP412                                                            
         CLI   FHIL,4              ONLY FOUR CHARACTERS OR LESS?                
         BH    *+12                ERROR, HEX NEEDS EVEN NUMBER                 
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FHIL             WHOLE ARGUMENT IN FIELD                      
         BCTR  R1,0                -1                                           
         LA    RF,FHDA(R1)         POINT TO LAST CHARACTER                      
         CLI   0(RF),C''''         IS THERE AND END QUOTE?                      
         BE    *+12                NEED AN END QUOTE                            
         MVI   FERN,25                                                          
         B     EXITL                                                            
*                                                                               
         TM    FHIL,X'01'          ODD-NUMBER LENGTH, EVEN NUMBER STR           
         BNZ   *+12                ERROR, HEX NEEDS EVEN NUMBER                 
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         AHI   R1,-2               MINUS LENGTH OF C''                          
         LR    R3,R1               MAKE A COPY OF LENGTH                        
*                                                                               
         GOTO1 AHEXIN,DMCB,FHDA+2,SRCHSTR,(R3)  CONVERT TO HEX                  
         MVC   SRCHLEN,15(R1)      COPY THE LENGTH OF THE HEX INPUT             
         CLI   SRCHLEN,0                                                        
         BNE   EXITOK              HEX IS OK                                    
*                                                                               
         LA    RF,FHDA+2           GO PAST THE X'                               
VP406    CLI   0(RF),C'A'                                                       
         BL    VP410                                                            
         CLI   0(RF),C'F'                                                       
         BNH   VP408                                                            
         CLI   0(RF),C'0'                                                       
         BL    VP410                                                            
         CLI   0(RF),C'9'                                                       
         BH    VP410                                                            
VP408    AHI   RF,1                                                             
         B     VP406                                                            
*                                                                               
VP410    MVI   FERN,11                                                          
         LA    R0,FHDA                                                          
         SR    RF,R0                                                            
         STC   RF,FERRDSP                                                       
         B     EXITL                                                            
*                                                                               
VP412    ICM   R3,15,AKEYWORD      CHECK SPECIAL KEYWORDS                       
         BZ    EXITOK                                                           
         USING KEYTABD,R3                                                       
         TM    KEYSRCE,KEYSCMD     SPECIAL ACTION COMMAND?                      
         BZ    VP422               NO                                           
         CLI   KEYACT,KEYAIND      INDIRECT (OFF REGISTER)?                     
         BNE   VP422               NO                                           
*                                                                               
         XR    RF,RF               REGISTER CAN BE (R)N OR XN                   
         ICM   RF,1,FHIL           R DENOTES 24 BIT ADDRESS - OPTIONAL          
         BNZ   VP414                                                            
*                                                                               
         CLI   SRVP3H+FHIL-FHD,0   SET DEFAULT IF NOTHING INPUT                 
         BNE   VP414                                                            
         MVI   FHDA,C'1'           SET DEFAULT TO RB                            
         MVI   FHIL,1                                                           
         LHI   RF,1                                                             
*                                                                               
VP414    LA    R1,FHDA             X DENOTES 31 BIT ADDRESS - REQUIRED          
         MVI   XREGS,C'N'                                                       
         CLI   0(R1),C'R'                                                       
         BNE   VP416                                                            
         AHI   R1,1                                                             
         BCTR  RF,0                                                             
         B     VP418                                                            
*                                                                               
VP416    CLI   0(R1),C'X'                                                       
         BNE   VP418                                                            
         MVI   XREGS,C'Y'                                                       
         AHI   R1,1                                                             
         BCTR  RF,0                                                             
*                                                                               
VP418    CHI   RF,1                LENGTH MUST BE 1 FOR A REGISTER              
         BE    *+12                                                             
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         LA    RE,REG2NUM          MATCH IN REGISTER LIST                       
VP420    CLI   0(RE),EOTB                                                       
         BNE   *+12                                                             
         MVI   FERN,12             NOT A VALID REGISTER                         
         B     EXITL                                                            
*                                                                               
         CLC   0(1,R1),0(RE)                                                    
         BE    *+12                                                             
         AHI   RE,2                                                             
         B     VP420                                                            
*                                                                               
         MVC   DUMPREG,1(RE)       SET REGISTER DISPLACEMENT                    
         B     EXITOK                                                           
*                                                                               
VP422    B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIND WHERE IN THE DUMP FILE THE START OF THE DUMP IS AND THEN       *         
* EXTRACT DUMP FILE HEADER INTO W/S                                   *         
***********************************************************************         
DOPFKEY  NTR1  BASE=*,LABEL=*                                                   
         CLI   PFKEY,7                                                          
         BE    *+12                                                             
         CLI   PFKEY,8                                                          
         BNE   EXITOK                                                           
         TM    SFLAG,SFT11OK                                                    
         BZ    EXITOK                                                           
*                                                                               
         L     RE,START                                                         
         A     RE,DISPLACE                                                      
         ST    RE,FULL                                                          
         BRAS  RE,SETADDR                                                       
*                                                                               
DOP02    CLI   PFKEY,7                                                          
         BE    DOP04                                                            
         OC    START,START         PURE DISPLACEMENT WILL SCROLL ANYWAY         
         BZ    EXITOK                                                           
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVC   DISPLACE,NDNXT-NDSAVED(RF)                                       
         XC    START,START                                                      
         XC    DUMPREG,DUMPREG                                                  
         B     EXITOK                                                           
*                                                                               
DOP04    L     RF,ASVBLOCK                                                      
         L     RF,NDLSTRT-NDSAVED(RF)                                           
         AHI   RF,-256                                                          
         ST    RF,DISPLACE                                                      
         C     RF,ADDRLOW                                                       
         BNL   *+10                                                             
         MVC   DISPLACE,ADDRLOW                                                 
         XC    START,START                                                      
         XC    DUMPREG,DUMPREG                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIND WHERE IN THE DUMP FILE THE START OF THE DUMP IS AND THEN       *         
* EXTRACT DUMP FILE HEADER INTO W/S                                   *         
***********************************************************************         
GETHDR   NTR1  BASE=*,LABEL=*                                                   
         CLC   DUMPNUM,=AL2(DUMPNUMA)                                           
         BE    EXITOK                                                           
*                                                                               
         LH    R0,DUMPNUM          COPY THE DUMP NUMBER                         
         BCTR  R0,0                OFFSET OF DUMP NUMBER 1                      
         XR    R1,R1                                                            
         IC    R1,MYDMPCY          R1=NUM OF CYLS IN DUMP                       
         MR    R0,R0               FIND WHICH CYLINDER DUMP IS IN               
         M     R0,TRKCYL           FIND WHICH TRACK DUMP IS IN                  
         AHI   R1,1                      T = TRACK                              
         SLL   R1,16                     B = BLOCK                              
         ST    R1,ADDRSTR          SAVE TTTTBB00 OF START OF DUMP               
         ST    R1,ADDR             SAVE TTTTBB00 FOR READ OF DUMP               
*                                                                               
         BRAS  RE,READ             READ DUMP FILE HEADER                        
         BE    *+20                                                             
         MVI   FERN,9              DISK ERROR ON DUMP FILE                      
         BL    EXITL                                                            
         MVI   FERN,34             OUT OF SCOPE                                 
         B     EXITL                                                            
*                                                                               
         LA    RE,DHDR             COPY DUMP HEADER LOCALLY                     
         LHI   RF,DMPHDRL                                                       
         L     R0,AINA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0               COPY HEADER INTO OUR STORAGE                 
*                                                                               
         MVC   DINDIC,HDR.DMPPART  GET THE DUMP INDICATOR                       
         MVI   HDR.DMPPART,0       SET DUMP INDICATOR OFF, COPIED               
*                                                                               
         MVC   SETREGS+00(36),HDR.DMPR0                                         
         MVC   SETREGS+36(28),HDR.DMPR9                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ TWA 11                                                         *         
***********************************************************************         
RDTWAB   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(24),DMCB       READ TWA 11 FOR THIS TERMINAL                
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRMNUM                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(0,DMREAD),TEMPSTR,(R0),ATIA                          
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
         L     RE,ATIA             COPY SAVED DATA FROM PAGE 11                 
         LA    RE,SR$DUMP-SRSD(RE)                                              
         L     R0,ASVBLOCK                                                      
         LHI   R1,L'SRCOMWRK                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ASVBLOCK                                                      
         USING NDSAVED,R2                                                       
         OI    SFLAG,SFT11OK       OKAY TO READ FROM THE FILE                   
*                                                                               
         CLC   NDUMP,NDWORD        DID I PUT SOMETHING IN TWAB?                 
         BNE   RTWA02              NO, BAD TWA B                                
         CLC   NDNUM,DUMPNUM       DUMP REQUEST SAME AS SAVED?                  
         BNE   RTWA02              YES                                          
         CLC   NDTIME,HDR.DMPTIME  DUMPED ON THE SAME TIME?                     
         BNE   RTWA02              YES                                          
*                                                                               
         MVC   NDLSSCRN,NDTSSCRN                                                
         MVI   NDTSSCRN,0                                                       
         MVC   SETREGS,NDCURREG                                                 
         B     EXITOK                                                           
*                                                                               
RTWA02   NI    SFLAG,255-SFT11OK   NOT THE SAME TWA B                           
         XC    NDLSTCNT,NDLSTCNT   NOTHING IN LIST NOW                          
         MVI   NDFLAG,0                                                         
         MVI   SAVEDFLG,0          SAVED REGS AND PARAMS NOT GOOD NOW           
*                                                                               
         L     RF,ATCB                                                          
         MVC   LEN,0(RF)           LENGTH OF TCB                                
         MVC   FROM,HDR.DMPTCBE                                                 
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA             RF = A(DUMP TCB)                             
         USING TCBD,RF                                                          
         MVC   NDSYSN,TCBSWSOV     SAVE SYSTEM NUMBER FOR DICTATE CALLS         
         MVC   NDWRKA,TCBWRKA      SAVE A(TCBWRK)                               
         MVC   NDWRKX,TCBTIA       SAVE A(END OF TCBWRK)                        
         DROP  RF                                                               
*                                                                               
         LA    R0,NDSVREG          CLEAR SAVED REGISTERS                        
         LHI   R1,NDSVREGL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LHI   R0,NDSVRCNT                                                      
         LA    R3,NDSVREG                                                       
*                                                                               
         MVC   LEN,=AL2(64+12)     BUILD REGISTER CHAIN LIST                    
         MVC   FROM,NDWRKA                                                      
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         NI    NDFLAG,255-NDFSROK                                               
*                                                                               
         L     RF,ATIA             MAKE SURE W/S CHAIN LOOKS OK                 
         CLC   0(4,RF),=C'MNTR'                                                 
         BE    RTWA06                                                           
         CLC   0(4,RF),=C'SCRU'                                                 
         BE    RTWA06                                                           
         B     EXITOK                                                           
*                                                                               
RTWA04   MVC   FROM,8(RF)                                                       
         MVC   LEN,=AL2(88)                                                     
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
         MVC   56(04,R1),04(RF)    SET CORRECT RD (FROM NEXT IN CHAIN)          
*                                                                               
RTWA06   OC    8(4,RF),8(RF)       END OF CHAIN?                                
         BZ    RTWA08                                                           
         CLC   FROM,8(RF)          MUST BE INCREASING                           
         BNL   RTWA08                                                           
         CLC   NDWRKX,8(RF)        AND LESS THAN END OF WORK AREA               
         BNH   RTWA08                                                           
*                                                                               
         XC    00(60,R3),00(R3)                                                 
         MVC   00(04,R3),00(RF)    EYECATCHER - USED ONLY FOR DEBUGGING         
         MVC   04(52,R3),20(RF)    R0-RC                                        
         MVC   60(08,R3),12(RF)    RE-RF                                        
         OI    NDFLAG,NDFSROK                                                   
         LR    R1,R3               SAVE TO SET RD                               
         AHI   R3,L'NDSVREG                                                     
         BCT   R0,RTWA04                                                        
         MVI   FERN,20             TABLE OVERFLOW                               
         B     EXITL                                                            
*                                                                               
RTWA08   MVC   00(04,R3),=CL4'END*'                                             
         MVC   56(04,R3),FROM                                                   
         AHI   R3,L'NDSVREG                                                     
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE TWA 11                                                        *         
***********************************************************************         
WTTWAB   NTR1  BASE=*,LABEL=*                                                   
         L     R6,ASVBLOCK                                                      
         USING NDSAVED,R6                                                       
         MVC   NDWORD,NDUMP        INDICATE DUMP SAVED                          
         MVC   NDNUM,DUMPNUM                                                    
         MVC   NDTIME,HDR.DMPTIME                                               
*                                                                               
         XC    NDCURREG,NDCURREG   CLEAR CURRENT REGISTERS                      
         CLC   DUMPNUM,=AL2(DUMPNUMA)                                           
         BE    WTB02                                                            
         MVC   NDCURREG,SETREGS                                                 
         OI    NDFLAG,NDFOKCHK                                                  
*                                                                               
WTB02    XC    DMCB(24),DMCB       READ TWA 11 FOR UPDATE                       
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRMNUM                                                      
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R0),ATIA                      
         CLI   8(R1),0                                                          
         BE    *+6                 SHOULDN'T BE ANY ERRORS                      
         DC    H'0'                                                             
*                                                                               
         L     RE,ATIA             MOVE BACK SAVED STORAGE                      
         LA    RE,SR$DUMP-SRSD(RE)                                              
         L     R0,ASVBLOCK                                                      
         LHI   RF,L'SRCOMWRK                                                    
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,SRPAGENO         WRITE BACK TWA 11                            
         SLL   R0,32-8                                                          
         ICM   R0,3,TRMNUM                                                      
         GOTO1 ADMGR,DMCB,(0,DMWRT),TEMPSTR,(R0),ATIA                           
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TCB INTO RECORD AREA                                 *         
* EXIT: FULL   = A(TCB IN RECORD AREA)                                *         
***********************************************************************         
GETTCB   NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL,HDR.DMPTCBE    SET START ADDRESS                            
         MVC   BYTE,DUMPDSPC                                                    
         MVI   DUMPDSPC,0          MAKE SURE HOME ADDRESS SPACE HERE            
         BRAS  RE,SETADDR          RETURNS A(TCB) IN FULL                       
         MVC   DUMPDSPC,BYTE                                                    
         BNE   EXITL                                                            
         BRAS  RE,READ             READ TCB BLOCK INTO MEMORY                   
         BE    EXITOK                                                           
         MVI   FERN,9                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* SET INITIAL ADDRESS FOR READING FROM DISK                           *         
* NTRY: FULL     = START ADDRESS + DISPLACEMENT (IF SET)              *         
*       DISPLACE = DISPLACEMENT FROM START ADDRESS                    *         
*       ADDRSTR  = SET                                                *         
* EXIT: ADDR     = A(BLOCK FOR THIS ADDRESS)                          *         
*       FULL     = A(IN IO AREA FOR THIS ADDRESS)                     *         
*       NBLKS2   = NUMBER OF BLOCKS REMAINING IN THIS DUMP            *         
***********************************************************************         
SETADDR  NTR1  BASE=*,LABEL=*                                                   
         MVC   ADDRMAX,=X'FFFFFFFF'                                             
*                                                                               
         CLI   DUMPDSPC,C'D'       DMGR ADDRESS SPACE                           
         BE    STAD10                                                           
         CLI   DUMPDSPC,C'T'       TABS ADDRESS SPACE                           
         BE    STAD20                                                           
*                                                                               
STAD01   L     RE,FULL                                                          
         C     RE,HDR.DMPXALO      IS ADDRESS VALID XA ADDRESS?                 
         BL    STAD02                                                           
         C     RE,HDR.DMPXAHI                                                   
         BNH   STAD04                                                           
*                                                                               
         C     RE,HDR.DMPISGLO     IS ADDRESS VALID ISG ADDRESS?                
         BL    STAD02                                                           
         C     RE,HDR.DMPISGHI                                                  
         BNH   STAD200                                                          
*                                                                               
         C     RE,HDR.DMPXA9LO     IS ADDRESS VALID XA9 ADDRESS?                
         BL    STAD02                                                           
         C     RE,HDR.DMPXA9HI                                                  
         BNH   STAD100                                                          
*                                                                               
STAD02   N     RE,=X'7FFFFFFF'                                                  
         S     RE,HDR.DMPPART      START ADDRESS BELOW MINIMUM?                 
         BNM   *+12                NO                                           
         MVI   FERN,16                                                          
         B     EXITL                                                            
*                                                                               
         SRL   RE,RSHIFT           DIVIDE BY BLOCK SIZE                         
         ST    RE,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
         SRDL  RE,32                                                            
         D     RE,RECTRK           RE = BLOCK NUM ON TRK MINUS 1                
         STC   RE,ADDR+2           RF = TRACK NUM AFTER HEADER                  
         AH    RF,ADDRSTR          RF = TRK NUM (ABSOLUTE DISK ADDRESS)         
         AHI   RF,1                ADJUST FOR HDR REC ON FIRST TRK              
         STH   RF,ADDR                                                          
*                                                                               
         LH    RE,HDR.DMPPAGE      RF = # OF 2K DUMP PAGES IN DUMP FILE         
         SRDL  RE,32                                                            
         D     RE,=A(BLKFCTR)      RF = # OF 8K RECORDS IN DUMP FILE            
         L     RE,BLKNUM                                                        
         LA    RE,1(RE)                                                         
         SR    RF,RE                                                            
         BNM   STAD03              ADDRESS OUT OF RANGE                         
         CLI   FULL,0                                                           
         BE    *+20                                                             
         MVI   FULL,0              TRY 24 BIT                                   
         MVI   FROM,0                                                           
         MVI   START,0                                                          
         B     STAD01                                                           
         MVI   FERN,32                                                          
         B     EXITL                                                            
*                                                                               
STAD03   STH   RF,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
*                                                                               
         L     RE,FULL                                                          
         S     RE,HDR.DMPPART      START ADDRESS BELOW MINIMUM?                 
         SRDL  RE,RSHIFT           DIVIDE BY 8192                               
         SRL   RF,32-RSHIFT        REMAINDER IS # OF BYTES INTO RECORD          
         A     RF,AINA             R5 = A(START POINT)                          
         ST    RF,FULL                                                          
         B     EXITOK                                                           
*                              *** SET XA ADDRESSES                             
STAD04   S     RE,HDR.DMPXALO                                                   
         SRL   RE,RSHIFT           RE=# OF BLOCKS FROM START                    
         ST    RE,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
         SRDL  RE,32                                                            
         D     RE,RECTRK           RE = BLOCK NUM ON TRK MINUS 1                
         STC   RE,ADDR+2           RF = TRACK NUM AFTER HEADER                  
         AH    RF,ADDRSTR          RF = TRK NUM (ABSOLUTE DISK ADDRESS)         
         AH    RF,HDR.DMPXA1                                                    
         STH   RF,ADDR                                                          
*                                                                               
         LH    RE,HDR.DMPXA2K      RF = # OF 2K DUMP PAGES IN DUMP FILE         
         SRDL  RE,32                                                            
         D     RE,=A(BLKFCTR)      RF = # OF 8K RECORDS IN DUMP FILE            
         L     RE,BLKNUM                                                        
         LA    RE,1(RE)                                                         
         SR    RF,RE                                                            
         BNM   *+12                ADDRESS OUT OF RANGE                         
         MVI   FERN,32                                                          
         B     EXITL                                                            
*                                                                               
         STH   RF,NBLKS2           USED TO COUNT DOWN BLOCKS READ               
*                                                                               
         L     RE,FULL                                                          
         S     RE,HDR.DMPXALO      START ADDRESS BELOW MINIMUM?                 
         SRDL  RE,RSHIFT           DIVIDE BY 8192                               
         SRL   RF,32-RSHIFT        REMAINDER IS # OF BYTES INTO RECORD          
         A     RF,AINA                                                          
         ST    RF,FULL                                                          
         B     EXITOK                                                           
*                                                                               
STAD10   CLC   FULL,HDR.DMPDM1LO   CHECK HI LO BOUNDS                           
         BL    STADERRL                                                         
         CLC   FULL,HDR.DMPDM1HI                                                
         BH    STAD15                                                           
         MVC   ADDR,HDR.DMPDM1TK                                                
         MVC   NBLKS2,HDR.DMPDM12K+2                                            
         MVC   ADDRLOW,HDR.DMPDM1LO      SET LOW CORE ADDR                      
         B     STAD50                                                           
*                                                                               
STAD15   CLC   FULL,HDR.DMPDM2LO   CHECK HI LO BOUNDS                           
         BL    STADERRL                                                         
         CLC   FULL,HDR.DMPDM2HI                                                
         BH    STADERRH                                                         
         MVC   ADDR,HDR.DMPDM2TK                                                
         MVC   NBLKS2,HDR.DMPDM22K+2                                            
         MVC   ADDRLOW,HDR.DMPDM2LO      SET LOW CORE ADDR                      
         L     RF,FULL                                                          
         S     RF,HDR.DMPDM2LO                                                  
         ST    RF,FULL                                                          
         B     STAD50                                                           
*                                                                               
STAD20   CLC   FULL,HDR.DMPTB1LO   CHECK HI LO BOUNDS                           
         BL    STADERRL                                                         
         CLC   FULL,HDR.DMPTB1HI                                                
         BH    STADERRH                                                         
         MVC   ADDR,HDR.DMPTB1TK                                                
         MVC   NBLKS2,HDR.DMPTB12K+2                                            
         MVC   ADDRLOW,HDR.DMPTB1LO      SET LOW CORE ADDR                      
         B     STAD50                                                           
*                                                                               
STAD30   MVC   ADDR,HDR.DMPXA9TK                                                
         MVC   NBLKS2,HDR.DMPXA92K+2                                            
         MVC   ADDRLOW,HDR.DMPXA9LO      SET LOW CORE ADDR                      
         L     RF,FULL                                                          
         S     RF,HDR.DMPXA9LO                                                  
         ST    RF,FULL                                                          
         B     STAD50                                                           
*                                                                               
STAD50   LH    RE,NBLKS2           2K BLOCKS AVAILABLE                          
         SRL   RE,2                CONVERT TO 8K BLOCKS                         
         SRDL  RE,32                                                            
         D     RE,RECTRK           RE = BLOCK NUM ON TRK MINUS 1                
         AH    RF,ADDR                                                          
         STH   RF,ADDRMAX          MAX TRACK                                    
         STC   RE,ADDRMAX+2        MAX BLOCK                                    
         MVI   ADDRMAX+3,0                                                      
         L     RF,FULL                                                          
         A     RF,AINA                                                          
         ST    RF,FULL                                                          
         B     EXITOK                                                           
*                                                                               
STAD100  MVC   ADDR,HDR.DMPXA9TK         SET BASE ADDR                          
         MVC   NBLKS2,HDR.DMPXA92K+2     TOTAL BLOCKS FOR REGION                
         MVC   ADDRLOW,HDR.DMPXA9LO      SET LOW CORE ADDR FOR REGION           
         L     RF,FULL                   CURRENT ADDRESS                        
         S     RF,HDR.DMPXA9LO           CONVERT TO OFFSET                      
         ST    RF,FULL                   SAVE IN FULL                           
*                                                                               
         LH    RE,NBLKS2                 2K BLOCKS AVAILABLE                    
         SRL   RE,2                      CONVERT TO 8K BLOCKS                   
         SRDL  RE,32                                                            
         L     R1,RECTRK                                                        
         DR    RE,R1                     RF=TRACKS RE=BLOCKS                    
*                                                                               
         AH    RF,ADDR                                                          
         STH   RF,ADDRMAX          MAX TRACK                                    
         LA    RE,1(RE)                                                         
         STC   RE,ADDRMAX+2        MAX BLOCK                                    
         MVI   ADDRMAX+3,0                                                      
*                                                                               
         L     RF,FULL             RF=OFFSET FROM BASE                          
         SRL   RF,13               RF=OFFS/8K = BLOCKS FROM BASE                
         XR    RE,RE                                                            
         DR    RE,R1               RF=TRACKS RE=BLOCKS                          
         AH    RF,ADDR                                                          
         STH   RF,ADDR                                                          
         STC   RE,ADDR+2                                                        
*                                                                               
         L     R1,FULL                                                          
         N     R1,=X'00001FFF'                                                  
         A     R1,AINA                                                          
         ST    R1,FULL                                                          
         B     EXITOK                                                           
*                                                                               
STAD200  MVC   ADDR,HDR.DMPISGTK         SET BASE ADDR                          
         MVC   NBLKS2,HDR.DMPISG2K+2     TOTAL BLOCKS FOR REGION                
         MVC   ADDRLOW,HDR.DMPISGLO      SET LOW CORE ADDR FOR REGION           
         L     RF,FULL                   CURRENT ADDRESS                        
         S     RF,HDR.DMPISGLO           CONVERT TO OFFSET                      
         ST    RF,FULL                   SAVE IN FULL                           
*                                                                               
         LH    RE,NBLKS2                 2K BLOCKS AVAILABLE                    
         SRL   RE,2                      CONVERT TO 8K BLOCKS                   
         SRDL  RE,32                                                            
         L     R1,RECTRK                                                        
         DR    RE,R1                     RF=TRACKS RE=BLOCKS                    
*                                                                               
         AH    RF,ADDR                                                          
         STH   RF,ADDRMAX          MAX TRACK                                    
         LA    RE,1(RE)                                                         
         STC   RE,ADDRMAX+2        MAX BLOCK                                    
         MVI   ADDRMAX+3,0                                                      
*                                                                               
         L     RF,FULL             RF=OFFSET FROM BASE                          
         SRL   RF,13               RF=OFFS/8K = BLOCKS FROM BASE                
         XR    RE,RE                                                            
         DR    RE,R1               RF=TRACKS RE=BLOCKS                          
         AH    RF,ADDR                                                          
         STH   RF,ADDR                                                          
         STC   RE,ADDR+2                                                        
*                                                                               
         L     R1,FULL                                                          
         N     R1,=X'00001FFF'                                                  
         A     R1,AINA                                                          
         ST    R1,FULL                                                          
         B     EXITOK                                                           
*                                                                               
STADERRL MVI   FERN,16                                                          
         B     EXITL                                                            
STADERRH MVI   FERN,32                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* READS NEXT FROM THE DUMP FILE                                       *         
* NTRY: ADDR     = DISK ADDRESS TO READ                               *         
***********************************************************************         
READ     NTR1  BASE=*,LABEL=*                                                   
         L     R0,AINA             CLEAR READ BUFFER                            
         LHI   R1,INAL                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   P1,ARDID            INITIALISE DADDS PARAMS FOR READ             
         XR    RE,RE               GET BLOCK NUMBER FOR TRACK                   
         IC    RE,ADDR+2                                                        
         C     RE,RECTRK           READ BLOCK ON THIS TRACK?                    
         BE    RD1                 YES                                          
         AHI   RE,1                NO, BUMP TO NEXT BLOCK(RECORD)               
         STC   RE,ADDR+2           SAVE IT FOR NEXT READ                        
         B     RD2                 READ THE RECORD                              
*                                                                               
RD1      ICM   RE,3,ADDR           BUMP TO NEXT TRACK                           
         LA    RE,1(RE)                                                         
         STH   RE,ADDR                                                          
         MVI   ADDR+2,1            SET BLOCK NUMBER TO FIRST ON TRACK           
*                                                                               
RD2      XC    P3,P3               P3 = NOT NEEDED (SET BY DADDS)               
         CLC   ADDR,ADDRMAX                                                     
         BNL   EXITH                                                            
         GOTO1 ADADDS,P1           READ RECORD FROM ADDRESS                     
         OC    P3(2),P3            SET CC TO EQUAL IF READ IS OKAY              
         BZ    EXITOK                                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* READ FROM THE DUMP FILE INTO ATIA FOR SPECIFIED LENGTH (MAX=BLKSIZE)*         
* NTRY:  FROM   = DUMP FILE LOGICAL ADDRESS                           *         
*        LEN    = LENGTH REQUIRED                                     *         
* EXIT:  ATIA   = A(DATA)                                             *         
***********************************************************************         
RDADR    NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL,FROM           SET START ADDRESS                            
         BRAS  RE,SETADDR                                                       
         BNE   EXITL                                                            
         L     R5,FULL                                                          
         BRAS  RE,READ                                                          
         BE    *+20                                                             
         MVI   FERN,9              DISK ERROR ON DUMP FILE                      
         BL    EXITL                                                            
         MVI   FERN,34             OUT OF SCOPE                                 
         B     EXITL                                                            
*                                                                               
         L     RF,AINA             FIND RESIDUAL L'REC FROM TWA START           
         AHI   RF,INAL                                                          
         SR    RF,R5               RF = REMAING L'RECORD                        
         L     RE,ATIA             RE = A(TIA)                                  
         LR    R0,R5               R0 = A(TWA START)                            
         CH    RF,LEN              MOVE A MAXIMUM OF 1 TWA LENGTH               
         BNH   *+8                                                              
         LH    RF,LEN                                                           
         LR    R1,RF                                                            
         STH   RF,HALF             SAVE LENGTH TO BE MOVED                      
         MVCL  RE,R0                                                            
*                                                                               
         CLC   HALF,LEN                                                         
         BNL   EXITOK              WHOLE TWA IN FIRST RECORD                    
         BRAS  RE,READ             GET REST OF TWA                              
         BE    *+20                                                             
         MVI   FERN,9              DISK ERROR ON DUMP FILE                      
         BL    EXITL                                                            
         MVI   FERN,34             OUT OF SCOPE                                 
         B     EXITL                                                            
*                                                                               
         L     RE,ATIA                                                          
         AH    RE,HALF             RE = A(TIA + L'FIRST MOVE)                   
         LH    RF,LEN                                                           
         SH    RF,HALF             RF = L'REMAINDER OF TWA                      
         L     R0,AINA             R0 = A(START OF REMAINDER OF TWA)            
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE THE REMAINDER                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET DUMP SAVE FLAG IN DATASPACE                                     *         
*                                                                     *         
* NOTE: THE DUMPS ARE SPLIT INTO 2(UK) OR 3(US) FILES - TEST/ADV/REP  *         
* EACH SYSTEM GETS AN (APPROXIMATELY) EQUAL NUMBER OF DUMPS FROM THE  *         
* FILE THEY ARE IN. IN ORDER TO SET THE CORRECT FLAG, YOU HAVE TO     *         
* MATCH ON THE FILE BEFORE YOU MATCH ON THE LOW/HIGH NUMBER FOR THIS  *         
* FACPAK.                                                             *         
***********************************************************************         
SVDUMP   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         TM    SFLAG,NOHDR         NO HEADER WRITE (FROM PROCESS)               
         BO    SVD01               YES                                          
*                                                                               
         CLI   KEYACT,KEYAREL      RELEASE                                      
         BE    SVD01                                                            
         CLI   KEYACT,KEYASAV      SAVE                                         
         BNE   EXITH                                                            
*                                                                               
         BRAS  RE,GETHDR           GET DUMP HEADER                              
         LA    R2,SRVP3H           MAKE INITIALS REQUIRED                       
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,30                                                          
         B     EXITL                                                            
*                                                                               
         MVC   HDR.DMPWHO,FHDA                                                  
         MVC   HDR.DMPPART(1),DINDIC                                            
         MVC   P1,AWTID            WRITE BACK HEADER RECORD                     
         LA    RE,DHDR                                                          
         ST    RE,P2                                                            
         GOTO1 ADADDS,P1                                                        
         OC    P3(2),P3            TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         MVC   P2,AINA             RESET A(HEADER)                              
*                                                                               
SVD01    L     R4,ASSB                                                          
         USING SSBD,R4                                                          
         MVC   HALF(1),SSBSYSFL    SAVE TEST/REP SYSTEM STATUS                  
         NI    HALF,FACITST+FACIREP                                             
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,TBOFFS                                                     
         SAC   512                                                              
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
         LH    R1,DUMPNUM                                                       
         CHI   R1,1                FIRST ENTRY IS A SPECIAL (DUMP 1)            
         BNE   SVD06                                                            
         B     SVD08                                                            
*                                                                               
SVD02    L     R1,AFACITAB         FIND OUT IF ENTRY IS SAME TYPE               
         USING FACITABD,R1                                                      
         LHI   RF,L'FACITAB                                                     
*                                                                               
SVD04    CLI   FACISN4,EOTB        EOT                                          
         BE    SVD06                                                            
         CLC   TDSYSNA,FACISN4     DOES ENTRY LOOK GOOD?                        
         BE    *+8                                                              
         BXH   R1,RF,SVD04                                                      
*                                                                               
         MVC   HALF+1(1),FACIFL    TEST FOR SAME TYPE OF FACPAK                 
         NI    HALF+1,FACITST+FACIREP                                           
         CLC   HALF(1),HALF+1      SAME TYPE (ADV/REP/TEST) ?                   
         BNE   SVD06                                                            
         DROP  R1                                                               
*                                                                               
         LH    R1,DUMPNUM          MATCH TYPE, NOW SEE IF DUMP IS IN            
         CLM   R1,15,TDDUMPFS      THIS SLOT                                    
         BL    SVD06                                                            
         CLM   R1,15,TDDUMPMX                                                   
         BH    SVD06                                                            
         B     SVD08                                                            
*                                                                               
SVD06    AHI   R2,TDLENQ           NEXT ENTRY                                   
         OC    TDSYSNA,TDSYSNA     ANOTHER ENTRY IN LIST?                       
         BNZ   SVD02               YES                                          
         B     SVDDIE                                                           
*                                                                               
SVD08    CLI   KEYACT,KEYAREL      RELEASE REQUESTED?                           
         BE    SVD12                                                            
         ICM   R0,15,TDDUMPMX                                                   
         ICM   RF,15,TDDUMPFS                                                   
         SR    R0,RF               NUMBER OF DUMPS IN THIS FACPAK               
         STH   R0,HALF                                                          
         AHI   R0,1                (ONES BASED)                                 
         SLR   R1,R1                                                            
         LHI   RF,1                SET UP BIT TEST MASK                         
         SLL   RF,32-1                                                          
*                                                                               
SVD10    SRL   RF,1                COUNT DUMPS CURRENTLY SAVED                  
         ICM   RE,15,TDDUMPST                                                   
         NR    RE,RF                                                            
         BZ    *+8                                                              
         AHI   R1,1                                                             
         BCT   R0,SVD10                                                         
*                                                                               
         LH    RE,HALF                                                          
         SRL   RE,1                RE=(#DUMPS IN FACPAK)/2                      
         CR    R1,RE                                                            
         BNH   SVD12               CANT SAVE >50% OF DUMPS FOR FACPAK           
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         MVI   FERN,13                                                          
         B     EXITL                                                            
*                                                                               
SVD12    MVC   MSG,SPACES                                                       
         MVC   MSG(6),=C'Dump #'                                                
*                                                                               
         ICM   R0,15,TDDUMPFS                                                   
         LH    R1,DUMPNUM                                                       
         SR    R1,R0               R1 = INDEX INTO THIS TABLE                   
         LHI   RF,1                SET UP BIT TEST MASK                         
         SLL   RF,32-1                                                          
         SRL   RF,1(R1)                                                         
*                                                                               
         CLI   KEYACT,KEYAREL      RELEASE REQUESTED?                           
         BE    SVD16                                                            
*                                                                               
         MVC   MSG+9(5),=C'saved'                                               
         ICM   R1,15,TDDUMPST                                                   
         NR    R1,RF               TEST FLAG STATE                              
         BZ    *+14                ALREADY SAVED                                
         MVC   MSG+9(13),=C'already saved'                                      
         B     SVD20                                                            
*                                                                               
         LR    R0,RF               SAVE MASK BIT                                
SVD14    ICM   RE,15,TDDUMPST                                                   
         LR    RF,R0                                                            
         OR    RF,RE                                                            
         CS    RE,RF,TDDUMPST                                                   
         BNE   SVD14                                                            
         B     SVD20                                                            
*                                                                               
SVD16    MVC   MSG+9(8),=C'released'                                            
         ICM   R1,15,TDDUMPST                                                   
         NR    R1,RF                                                            
         BNZ   *+14                                                             
         MVC   MSG+9(16),=C'already released'                                   
         B     SVD20                                                            
*                                                                               
         LR    R0,RF                                                            
         X     R0,=XL4'FFFFFFFF'                                                
SVD18    ICM   RE,15,TDDUMPST                                                   
         LR    RF,R0                                                            
         NR    RF,RE                                                            
         CS    RE,RF,TDDUMPST                                                   
         BNE   SVD18                                                            
*                                                                               
SVD20    SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         CURED (B2,DUMPNUM),(2,MSG+6),0,ZERO=NOBLANK,ALIGN=LEFT                 
*                                                                               
         MVC   SRVP1,SPACES        RESET DISPLAY AND DISPLAY ALL                
         MVI   SRVP1,C'A'                                                       
         MVI   SRVP1H+(FHIL-FHD),1                                              
         MVC   SRVP2,SPACES                                                     
         MVI   SRVP2H+(FHIL-FHD),0                                              
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3H+(FHIL-FHD),0                                              
         MVC   SRVP4,SPACES                                                     
         MVI   SRVP4H+(FHIL-FHD),0                                              
         BRAS  RE,DISALL                                                        
*                                                                               
         MVI   FERN,255            SET OWN MESSAGE                              
         LA    RF,MSG                                                           
         STCM  RF,7,FERNA                                                       
         B     EXITL                                                            
*                                                                               
SVDDIE   DC    H'0'                DUMP TABLE IN DATASPACE IS MESSED UP         
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE DUMP STATUS FLAG IN DUMP HEADER                               *         
***********************************************************************         
WRFL     NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYAFIX      FIXED                                        
         BE    WRFL02                                                           
         CLI   KEYACT,KEYAPROC     PROCESS                                      
         BE    WRFL02                                                           
         CLI   KEYACT,KEYAKNOW     KNOWN                                        
         BE    WRFL02                                                           
         CLI   KEYACT,KEYAUNDO     UNDO                                         
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
WRFL02   BRAS  RE,GETHDR           GET DUMP HEADER                              
         OI    SFLAG,NOHDR         SET NO WRITE                                 
*                                                                               
         XC    HDR.DMPSTAT,HDR.DMPSTAT                                          
         XC    HDR.DMPWHO,HDR.DMPWHO                                            
         CLI   KEYACT,KEYAPROC     PROCESS?                                     
         BNE   *+8                                                              
         MVI   HDR.DMPSTAT,DMPSPROC                                             
         CLI   KEYACT,KEYAFIX      FIXED?                                       
         BNE   *+8                                                              
         MVI   HDR.DMPSTAT,DMPSFIX                                              
         CLI   KEYACT,KEYAKNOW     KNOWN?                                       
         BNE   *+8                                                              
         MVI   HDR.DMPSTAT,C'K'                                                 
*                                                                               
         CLI   KEYACT,KEYAPROC     PROCESS?                                     
         BE    *+8                                                              
         CLI   KEYACT,KEYAKNOW     KNOWN?                                       
         BE    *+12                                                             
         CLI   KEYACT,KEYAFIX      OR FIX?                                      
         BNE   WRFL04                                                           
*                                                                               
         LA    R2,SRVP3H           MAKE INITIALS REQUIRED                       
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,30                                                          
         B     EXITL                                                            
*                                                                               
         MVC   HDR.DMPWHO,FHDA                                                  
         MVC   HDR.DMPPART(1),DINDIC                                            
         DROP  R2                                                               
*                                                                               
WRFL04   MVC   P1,AWTID            WRITE BACK HEADER RECORD                     
         LA    RE,DHDR                                                          
         ST    RE,P2                                                            
         GOTO1 ADADDS,P1                                                        
         OC    P3(2),P3            TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P2,AINA             RESET A(HEADER)                              
*                                                                               
         CLI   KEYACT,KEYAPROC     PROCESS?                                     
         BNE   WRFL08                                                           
         MVI   KEYACT,KEYASAV      SET ACTION TO SAVE                           
         BRAS  RE,SVDUMP                                                        
         MVI   KEYACT,KEYAPROC     RESTORE REAL ACTION                          
         B     WRFL16                                                           
*                                                                               
WRFL08   CLI   KEYACT,KEYAKNOW     KNOWN?                                       
         BE    *+12                                                             
         CLI   KEYACT,KEYAFIX      FIXED?                                       
         BNE   WRFL14                                                           
         MVI   KEYACT,KEYAREL      SET ACTION TO RELEASE                        
         BRAS  RE,SVDUMP                                                        
         MVI   KEYACT,KEYAFIX      RESTORE REAL ACTION                          
*                                                                               
         LAM   AR0,ARF,ARZERO      CLEAR OUT DATASPACE - FORCE NEW DUMP         
         LH    R0,DUMPNUM                                                       
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,TBOFFS                                                     
         SAC   512                                                              
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
WRFL10   C     R0,TDDUMPFS         IS THIS MY FACPAK?                           
         BL    *+12                                                             
         C     R0,TDDUMPMX         IS THIS MY FACPAK?                           
         BNH   WRFL12              NO                                           
         AHI   R2,TDLENQ           NEXT ENTRY                                   
         OC    TDSYSNA,TDSYSNA     ANOTHER ENTRY IN LIST?                       
         BNZ   WRFL10              YES                                          
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         B     WRFL16                                                           
*                                                                               
WRFL12   XC    TDPSW,TDPSW                                                      
         XC    TDREGB,TDREGB                                                    
         XC    TDMODID,TDMODID                                                  
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         B     WRFL16                                                           
*                                                                               
WRFL14   MVI   KEYACT,KEYAREL      SET ACTION TO RELEASE                        
         BRAS  RE,SVDUMP                                                        
         MVI   KEYACT,KEYAUNDO     RESET REAL ACTION                            
         B     WRFL16                                                           
*                                                                               
WRFL16   MVC   MSG,SPACES                                                       
         MVC   MSG(6),=C'Dump #'                                                
         CURED (B2,DUMPNUM),(2,MSG+6),0,ZERO=NOBLANK,ALIGN=LEFT                 
         MVC   MSG+9(10),=CL10'status set'                                      
         MVI   FERN,255                                                         
         LA    RF,MSG                                                           
         STCM  RF,7,FERNA                                                       
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SET DUMP SUPPRESSION FLAGS IN DATASPACE                             *         
***********************************************************************         
SUPPRESS NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYASTP      SAVE                                         
         BE    *+12                                                             
         CLI   KEYACT,KEYASTRT     RELEASE                                      
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,TBOFFS                                                     
         SAC   512                                                              
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
SUP02    CLC   MYSYSN4,TDSYSNA     IS THIS MY FACPAK?                           
         BE    SUP04               NO                                           
         AHI   R2,TDLENQ           NEXT ENTRY                                   
         OC    TDSYSNA,TDSYSNA     ANOTHER ENTRY IN LIST?                       
         BNZ   SUP02               YES                                          
         DC    H'0'                                                             
*                                                                               
SUP04    CLI   KEYACT,KEYASTRT     STOP REQUESTED?                              
         BE    SUP08                                                            
         MVI   HDRN,4                                                           
*                                                                               
SUP06    TM    TDDUMPST,X'80'                                                   
         BZ    *+12                                                             
         MVI   FERN,14                                                          
         B     SUP10                                                            
*                                                                               
         ICM   RE,15,TDDUMPST                                                   
         LR    RF,RE                                                            
         O     RF,=XL4'80000000'                                                
         CS    RE,RF,TDDUMPST                                                   
         BNE   SUP06                                                            
         B     SUP10                                                            
*                                                                               
SUP08    MVI   HDRN,5                                                           
         TM    TDDUMPST,X'80'                                                   
         BO    *+12                                                             
         MVI   FERN,15                                                          
         B     SUP10                                                            
*                                                                               
         ICM   RE,15,TDDUMPST                                                   
         LR    RF,RE                                                            
         N     RF,=XL4'7FFFFFFF'                                                
         CS    RE,RF,TDDUMPST                                                   
         BNE   SUP08                                                            
*                                                                               
SUP10    SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET START ADDRESS FROM KEYWORD IF REQUIRED (OR RETURN CC=HI IF NOT) *         
* ROUTINE LOOPS THROUGH DISPLACEMENTS READING INDIRECT ADDRESSES SET  *         
* IN KEYDISPS 1-4 IN TABLE ENTRY                                      *         
*                                                                     *         
* EXIT: START  = ADDRESS REQUIRED (CC OK)                             *         
***********************************************************************         
KWSTART  NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,AKEYWORD      MAKE SURE A KEYWORD                          
         BZ    EXITH                                                            
         USING KEYTABD,R2                                                       
         TM    KEYSRCE,KEYSCMD     AND NOT A COMMAND                            
         BO    EXITH                                                            
*                                                                               
         XR    R4,R4                                                            
         TM    KEYSRCE,KEYSFAC                                                  
         BZ    *+8                                                              
         ICM   R4,15,HDR.DMPFACS                                                
         TM    KEYSRCE,KEYSTCB                                                  
         BZ    *+8                                                              
         ICM   R4,15,HDR.DMPTCBE                                                
         TM    KEYSRCE,KEYSUTL                                                  
         BZ    *+8                                                              
         ICM   R4,15,HDR.DMPUTL                                                 
*                                                                               
         LHI   R0,KEYLOOP#         R0 = MAX TIMES TO LOOP                       
         LA    R3,KEYDISP1         R3 = DISPLACEMENT LOOP                       
*                                                                               
KWS02    ST    R4,START                                                         
         S     R4,HDR.DMPPART      START ADDRESS BELOW MINIMUM?                 
         BNM   *+12                YES                                          
         MVI   FERN,16                                                          
         B     EXITL                                                            
*                                                                               
         OC    0(L'KEYDISP1,R3),0(R3)                                           
         BZ    EXITOK              NO MORE IN CHAIN TO FOLLOW                   
*                                                                               
         MVC   FROM,START                                                       
         ICM   R1,15,0(R3)         R1 = DISPLACEMENT FROM START                 
KWS04    CHI   R1,4096             WITHIN 4K OF START?                          
         BNH   KWS06                                                            
         L     RF,START                                                         
         AHI   RF,4096                                                          
         ST    RF,START                                                         
         AHI   R1,-4096                                                         
         B     KWS04                                                            
*                                                                               
KWS06    LA    RF,4(R1)            RF = DISPLACEMENT + ACTUAL ADDRESS           
         STH   RF,LEN                                                           
         BRAS  RE,RDADR            READ REQUESTED BLOCK INTO TIA                
         BNE   EXITL                                                            
*                                                                               
         A     R1,ATIA                                                          
         ICM   R4,15,0(R1)         R4 = (NEXT ADDRESS TO USE)                   
         AHI   R3,L'KEYDISP1       NEXT IN CHAIN                                
         BCT   R0,KWS02                                                         
         B     EXITOK                                                           
*                                                                               
KWS08    DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SECTION THAT SHOWS HOW A PORTION OF THE TWA LOOKS LIKE              *         
* INITIATED BY KEYWORDS TT(U) AND TT(L)                               *         
***********************************************************************         
TWASHOW  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         L     RF,AKEYWORD                                                      
         USING KEYTABD,RF                                                       
         MVI   BYTE,C'U'           DEFAULT IS TOP OF TWA                        
         CLC   =C'TTL',KEYWORD                                                  
         BNE   *+8                                                              
         MVI   BYTE,C'L'                                                        
         DROP  RF                                                               
*                                                                               
         MVC   FROM,START                                                       
         MVC   LEN,MAXTWAL         SET LENGTH TO READ                           
         BRAS  RE,RDADR            GET TWA INTO TIA                             
         BNE   EXITL                                                            
*                                                                               
TWS02    MVC   DMPL1,STARS         BORDER FROM INFORMATION AND SCREEN           
         MVC   SRVP4F(16),SPACES                                                
         MVC   SRVP4F(7),=CL7'A(TWA)='                                          
         GOTO1 AHEXOUT,DMCB,START,SRVP4F+7,4                                    
*                                                                               
         LA    R2,DMPL2H                                                        
SCR      USING FHD,R2                                                           
         XR    RF,RF                                                            
         ICM   RF,3,SCR.FHAD       SAVE A(FIRST FIELD)                          
         BCTR  RF,0                FIX COLUMNS                                  
         STH   RF,HALF                                                          
*                                                                               
         L     R3,ATIA             START OF TWA                                 
         AHI   R3,64               R6 = A(FIRST FIELD IN TWA)                   
DMP      USING FHD,R3                                                           
*                                                                               
TWS04    CLI   DMP.FHLN,0          SCREEN END?                                  
         BE    TWS16               YES                                          
         CLI   DMP.FHAT,FHATNP     NOP (INVISIBLE) FIELD                        
         BO    TWS14               YES - NEXT                                   
*                                                                               
         LHI   R0,FHDAD                                                         
         TM    DMP.FHAT,FHATXH                                                  
         BZ    *+8                                                              
         LHI   R0,FHDAD+FHDAD                                                   
         CLM   R0,1,DMP.FHLN                                                    
         BL    *+12                                                             
         MVI   FERN,28             TWA IS INVALID FOR DISPLAY                   
         B     EXITL                                                            
*                                                                               
         LHI   R0,80+FHDAD                                                      
         TM    DMP.FHAT,FHATXH                                                  
         BZ    *+8                                                              
         LHI   R0,80+FHDAD+FHDAD                                                
         CLM   R0,1,DMP.FHLN                                                    
         BNL   *+12                                                             
         MVI   FERN,29                                                          
         B     EXITL                                                            
*                                                                               
         CLC   DMP.FHAD,SCRNEND                                                 
         BNH   *+12                                                             
         MVI   FERN,17             TWA IS INVALID FOR DISPLAY                   
         B     EXITL                                                            
*                                                                               
         CLI   BYTE,C'U'           DISPLAYING TOP OF SCREEN?                    
         BNE   TWS06               NO                                           
*                                                                               
         XR    RF,RF               COPY FIELD TO SCREEN                         
         IC    RF,DMP.FHLN                                                      
         BCTR  RF,0                                                             
         MVC   SCR.FHD(0),DMP.FHD                                               
         EX    RF,*-6                                                           
         OI    SCR.FHAT,FHATPR     FIX DISPLAY BITS                             
         MVI   SCR.FHII,0                                                       
         MVI   SCR.FHIL,0                                                       
         MVI   SCR.FHOI,FHOITR                                                  
         MVI   SCR.FHOL,0                                                       
*                                                                               
         CLC   SCR.FHAD,TTULST     PAST TOP DISPLAY PORTION?                    
         BNL   TWS16               YES                                          
         ICM   RF,3,SCR.FHAD                                                    
         AH    RF,HALF             ADJUST FIELD POSITION ON SCREEN              
         STCM  RF,3,SCR.FHAD                                                    
         B     TWS08                                                            
*                                                                               
TWS06    CLC   DMP.FHAD,TTLFST     BEFORE BOTTOM DISPLAY PORTION?               
         BL    TWS14               YES                                          
*                                                                               
         XR    RF,RF               COPY FIELD TO SCREEN                         
         IC    RF,DMP.FHLN                                                      
         BCTR  RF,0                                                             
         MVC   SCR.FHD(0),DMP.FHD                                               
         EX    RF,*-6                                                           
         OI    SCR.FHAT,FHATPR     FIX DISPLAY BITS                             
         MVI   SCR.FHII,0                                                       
         MVI   SCR.FHIL,0                                                       
         MVI   SCR.FHOI,FHOITR                                                  
         MVI   SCR.FHOL,0                                                       
*                                                                               
         ICM   RF,3,SCR.FHAD                                                    
         SH    RF,TTLFST                                                        
         AH    RF,HALF                                                          
         STCM  RF,3,SCR.FHAD        ADJUST FIELD POSITION ON SCREEN             
*                                                                               
TWS08    TM    DMP.FHAT,FHATXH     EXTENDED FIELD HEADER?                       
         BZ    TWS10               NO                                           
         XR    RF,RF                                                            
         IC    RF,SCR.FHLN                                                      
         LA    RF,SCR.FHD(RF)                                                   
         AHI   RF,-(FHDAD)                                                      
         XC    0(FHDAD,RF),0(RF)   CLEAR OUT EXTENDED FIELD HEADER              
*                                                                               
TWS10    XC    DMCB(24),DMCB                                                    
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNT      TRANSLATE WHOLE FIELD                        
         MVI   DDRETN,DDCASEU                                                   
         TM    SCR.FHAT,FHATLC     LOWER CASE?                                  
         BZ    *+8                 NO                                           
         MVI   DDRETN,DDCASEL                                                   
*                                                                               
         L     RF,ASVBLOCK         GET SYSTEM NUMBER FOR DD TRANSLATE           
         MVC   DDSYS,NDSYSN-NDSAVED(RF)                                         
         MVI   DDLANG,0            ENGLISH, PLEASE                              
         LA    RF,SCR.FHDA         SET ADDRESS AND LENGTH OF DATA               
         STCM  RF,7,DDIADR                                                      
*                                                                               
         XR    RF,RF               SET LENGTH TO TRANSLATE                      
         IC    RF,SCR.FHLN                                                      
         AHI   RF,-(FHDAD)                                                      
         TM    SCR.FHAT,FHATXH                                                  
         BZ    *+8                                                              
         AHI   RF,-(FHDAD)                                                      
         STC   RF,DDILEN                                                        
         GOTO1 ADICTATE,(R1)                                                    
         DROP  R1                                                               
*                                                                               
TWS12    XR    RF,RF               NEXT FIELD IN DUMP / ON SCREEN               
         IC    RF,DMP.FHLN                                                      
         AR    R2,RF                                                            
         AR    R3,RF                                                            
         B     TWS04                                                            
*                                                                               
TWS14    XR    RF,RF               NEXT FIELD IN DUMP ONLY                      
         IC    RF,DMP.FHLN                                                      
         AR    R3,RF                                                            
         B     TWS04                                                            
*                                                                               
TWS16    MVC   SCR.FHLN(3),=XL3'000101'                                         
         MVC   SRVP2(3),=CL3'TTL'  SWAP UPPER TO LOWER                          
         XC    FERN,FERN                                                        
         MVI   HDRN,6                                                           
         CLI   BYTE,C'U'                                                        
         BE    *+14                                                             
         MVI   HDRN,7                                                           
         MVC   SRVP2(3),=CL3'TTU'                                               
         OI    SRVP2H+(FHOI-FHD),FHOITR                                         
         LA    RF,SRVP3H                                                        
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
         DROP  SCR,DMP                                                          
*                                                                               
TTLFST   DC    AL2((05-1)*80)                                                   
TTULST   DC    AL2((21-1)*80)                                                   
SCRNEND  DC    AL2(24*80)                                                       
         EJECT                                                                  
***********************************************************************         
* SECTION THAT PRINTS OUT TWA FIELD BY FIELD                          *         
***********************************************************************         
TWADISP  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         LA    RF,SRVP4H                                                        
         ST    RF,FADRH                                                         
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         MVC   FROM,START                                                       
         MVC   LEN,MAXTWAL         SET LENGTH TO READ                           
         BRAS  RE,RDADR            GET TWA INTO TIA                             
         BNE   EXITL                                                            
*                                                                               
TWD02    MVC   SRVP4F(16),SPACES                                                
         MVC   SRVP4F(7),=CL7'A(TWA)='                                          
         GOTO1 AHEXOUT,DMCB,START,SRVP4F+7,4                                    
*                                                                               
         LA    R2,DMPL1H           R2 = A(FIRST DISPLAY LINE ON SCREEN)         
         USING FHD,R2                                                           
         USING DMPLINED,FHDA                                                    
         L     R3,ATIA                                                          
         AHI   R3,64                                                            
DMP      USING FHD,R3                                                           
         ICM   RF,15,DISPLACE      DISPLACEMENT TO SCAN FOR?                    
         BZ    TWD10               NO                                           
         BP    *+12                                                             
         MVI   FERN,19             DISPLACEMENT CAN'T BE NEGATIVE               
         B     EXITL                                                            
*                                                                               
         CH    RF,MAXTWAL                                                       
         BH    TWD08                                                            
         LH    RF,MAXTWAL                                                       
         A     RF,ATIA                                                          
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
TWD04    CLI   DMP.FHLN,0          END OF SCREEN?                               
         BE    TWD08               YES                                          
*                                                                               
         LR    R1,R3                                                            
         AHI   R1,-64                                                           
         S     R1,ATIA                                                          
         C     R1,DISPLACE         CHECK DISPLACEMENT                           
         BE    TWD10               MATCH                                        
         BL    TWD06                                                            
         ST    R1,DISPLACE                                                      
         B     TWD10                                                            
*                                                                               
TWD06    IC    RE,DMP.FHLN                                                      
         BXLE  R3,RE,TWD04                                                      
*                                                                               
TWD08    MVI   FERN,18             DISPLACEMENT TOO BIG                         
         B     EXITL                                                            
*                                                                               
TWD10    CLI   DMP.FHLN,0          END OF DUMP SCREEN?                          
         BE    TWD18               YES                                          
         CLI   DMP.FHLN,FHDAD                                                   
         BH    *+12                                                             
         MVI   FERN,17             TWA INVALID FOR DISPLAY                      
         B     EXITL                                                            
*                                                                               
         CLI   DMP.FHLN,80+FHDAD+FHDAD                                          
         BL    *+12                                                             
         MVI   FERN,17             TWA INVALID FOR DISPLAY                      
         B     EXITL                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,DMP.FHLN                                                    
         AHI   RE,-(FHDAD)                                                      
         TM    DMP.FHAT,FHATXH                                                  
         BZ    *+8                                                              
         AHI   RE,-(FHDAD)                                                      
         STH   RE,HALF             HALF = L'DATA IN FIELD                       
         SRDL  RE,32                                                            
         LHI   R0,L'DMPLALF                                                     
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AHI   RF,1                RF = # LINES TO DISPLAY THIS DATA            
         AHI   RF,1                ADD HEADER INFO LINE                         
*                                                                               
         LA    R0,DMPLAST          SEE IF THIS FIELD WILL FIT ON SCREEN         
         SR    R0,R2                                                            
         SRDL  R0,32                                                            
         D     R0,=A(DMPL2H-DMPL1H)                                             
         CR    RF,R1                                                            
         BH    TWD20               NO - HAVE TO END HERE                        
*                                                                               
         BRAS  RE,HEDOUT           DISPLAY HEADER DATA                          
         XR    R0,R0                                                            
         IC    R0,FHLN                                                          
         AR    R2,R0               NEXT LINE ON SCREEN                          
*                                                                               
         IC    R0,DMP.FHLN                                                      
         AHI   R0,-(FHDAD)                                                      
         TM    DMP.FHAT,FHATNP     NOP FIELD - DISPLAY HDR AND DATA             
         BO    TWD12                                                            
         TM    DMP.FHAT,FHATXH     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         AHI   R0,-(FHDAD)                                                      
*                                                                               
TWD12    ST    R0,FULL                                                          
         XC    SCANBLK,SCANBLK     HEXOUT DATA FOR DISPLAY                      
         GOTO1 AHEXOUT,DMCB,DMP.FHDA,SCANBLK,(R0)                               
         L     RF,FULL                                                          
         BCTR  RF,0                                                             
         MVC   WRK(0),DMP.FHDA                                                  
         EX    RF,*-6                                                           
*                                                                               
         LA    RE,WRK              RE=A(UNEDITED DATA)                          
         LA    R1,SCANBLK          R1=A(HEXOUT DATA)                            
*                                                                               
TWD14    L     RF,FULL             RF = AMOUNT OF DATA REMAINING                
         CHI   RF,L'DMPLALF                                                     
         BNH   *+8                                                              
         LHI   RF,L'DMPLALF                                                     
*                                                                               
         BCTR  RF,0                DISPLAY ALPHA DATA                           
         MVC   DMPLALF(0),0(RE)                                                 
         EX    RF,*-6                                                           
                                                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    DMPLALF(0),TRTAB                                                 
         AHI   RF,1                                                             
         AR    RE,RF               NEXT ALPHA                                   
         SLL   RF,1                                                             
         BCTR  RF,0                DISPLAY HEX DATA                             
         MVC   DMPLHEX(0),0(R1)                                                 
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF               NEXT HEX                                     
         SRL   RF,1                                                             
*                                                                               
         XR    R0,R0                                                            
         IC    R0,FHLN                                                          
         AR    R2,R0               NEXT LINE                                    
*                                                                               
         L     R0,FULL             REDUCE AMOUNT TO DISPLAY                     
         SR    R0,RF                                                            
         BNP   TWD16                                                            
         ST    R0,FULL                                                          
         B     TWD14                                                            
*                                                                               
TWD16    XR    RF,RF               NEXT FIELD IN DUMP SCREEN                    
         IC    RF,DMP.FHLN                                                      
         AR    R3,RF                                                            
         A     RF,DISPLACE                                                      
         ST    RF,DISPLACE                                                      
         B     TWD10                                                            
*                                                                               
TWD18    MVC   DMPLHEX(24),=CL24'** End of TWA - Length ='                      
         L     R0,DISPLACE                                                      
         AHI   R0,3                                                             
         AHI   R0,64                                                            
         CURED (R0),(4,DMPLHEX+25),0,ALIGN=LEFT                                 
         XR    R0,R0                                                            
         IC    R0,FHLN                                                          
         AR    R2,R0                                                            
         XC    DISPLACE,DISPLACE                                                
         MVI   HDRN,9                                                           
         B     TWD22                                                            
*                                                                               
TWD20    MVI   HDRN,8                                                           
         MVC   FHLN(3),=XL3'000101'                                             
*                                                                               
TWD22    MVC   SRVP3,SPACES        SET NEXT START DISPLACEMENT                  
         OI    SRVP3H+(FHOI-FHD),FHOITR                                         
         OI    SRVP4H+(FHOI-FHD),FHOICU                                         
         XC    FERN,FERN                                                        
         OC    DISPLACE,DISPLACE                                                
         BZ    EXITOK                                                           
         GOTO1 AHEXOUT,DMCB,DISPLACE,DUB,4                                      
         MVC   SRVP3(4),DUB+4                                                   
         CLC   SRVP3(2),=CL2'00'   STRIP LEADING ZEROS (IF ANY)                 
         BNE   EXITOK                                                           
         MVC   SRVP3+0(2),SRVP3+2                                               
         MVC   SRVP3+2(2),SPACES                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADER LINE DETAILS                                         *         
* NTRY: R2     = A(SCREEN LINE)                                       *         
*       R3     = A(DUMP FIELD)                                        *         
***********************************************************************         
HEDOUT   NTR1  ,                                                                
         OI    FHAT,FHATHI         SET FIELD HIGHLIGHTED                        
*                                                                               
         L     R0,DISPLACE         SHOW A(TWA FIELD)                            
         A     R0,START                                                         
         AHI   R0,64                                                            
         ST    R0,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,DUB,4                                          
         MVC   DMPLSTRT,DUB+2                                                   
*                                                                               
         L     R0,DISPLACE                                                      
         AHI   R0,64                                                            
         ST    R0,FULL                                                          
         GOTO1 (RF),(R1),FULL,DUB,4                                             
         MVC   DMPLDSP,DUB+4       DISPLAY DISPLACEMENT AND HEADER              
*                                                                               
         MVC   DMPLHDR,=CL2'H='                                                 
         GOTO1 (RF),(R1),DMP.FHLN,DMPLHDRI,FHDAD                                
         MVC   DMPLROW,=CL2'R='                                                 
         MVC   DMPLCOL,=CL2'C='                                                 
*                                                                               
         XR    RE,RE               PULL OUT ROW/COLUMN DETAIL                   
         ICM   RE,3,DMP.FHAD                                                    
         SRDL  RE,32                                                            
         D     RE,=F'80'                                                        
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         STH   RF,FULL             FULL+0(2) = ROW                              
         STH   RE,FULL+2           FULL+2(2) = COLUMN                           
         EDIT  (B2,FULL+0),(2,DMPLROWI),ZERO=NOBLANK,FILL=0                     
         EDIT  (B2,FULL+2),(2,DMPLCOLI),ZERO=NOBLANK,FILL=0                     
*                                                                               
         TM    DMP.FHAT,FHATNP     NOP FIELD ?                                  
         BNO   *+14                                                             
         MVC   DMPLINF(10),=CL10',NOP Field'                                    
         B     EXITOK                                                           
*                                                                               
         TM    DMP.FHOI,FHOINE     NEW FIELD ?                                  
         BNO   *+14                                                             
         MVC   DMPLINF(10),=CL10',New Field'                                    
         B     EXITOK                                                           
*                                                                               
         LA    R4,DMPLINF          R4 = DISPLACEMENT INTO FIELD                 
         TM    DMP.FHAT,FHATXH                                                  
         BZ    HED02                                                            
         MVC   0(4,R4),=CL4',XH='                                               
         XR    RF,RF                                                            
         IC    RF,DMP.FHLN                                                      
         LA    RF,DMP.FHD(RF)                                                   
         AHI   RF,-(FHDAD)                                                      
         GOTO1 AHEXOUT,DMCB,(RF),4(R4),FHDAD                                    
         AHI   R4,20                                                            
*                                                                               
HED02    TM    DMP.FHAT,FHATPR                                                  
         BO    *+12                                                             
         TM    DMP.FHOI,FHOIPR                                                  
         BZ    HED04                                                            
         MVC   0(5,R4),=C',Prot'                                                
         AHI   R4,5                                                             
*                                                                               
HED04    TM    DMP.FHAT,FHATMO                                                  
         BO    *+12                                                             
         TM    DMP.FHOI,FHOIMO                                                  
         BZ    HED06                                                            
         MVC   0(4,R4),=C',Mod'                                                 
         AHI   R4,4                                                             
*                                                                               
HED06    TM    DMP.FHOI,FHOICU                                                  
         BZ    HED08                                                            
         MVC   0(4,R4),=C',Cur'                                                 
         AHI   RF,4                                                             
*                                                                               
HED08    TM    DMP.FHAT,FHATHI                                                  
         BO    *+12                                                             
         TM    DMP.FHOI,FHOIHI                                                  
         BZ    HED10                                                            
         MVC   0(3,R4),=C',Hi'                                                  
         AHI   R4,3                                                             
*                                                                               
HED10    TM    DMP.FHAT,FHATLO                                                  
         BO    *+12                                                             
         TM    DMP.FHOI,FHOILO                                                  
         BNO   HED12                                                            
         MVC   0(3,R4),=C',Lo'                                                  
         AHI   R4,3                                                             
*                                                                               
HED12    B     EXITOK                                                           
         DROP  R2,DMP                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SHOW HOLE TYPE INFORMATION                               *         
***********************************************************************         
AHOLE    NTR1  BASE=*,LABEL=*                                                   
         XC    FERN,FERN                                                        
         MVI   HDRN,25             SET DEFAULT MESSAGE                          
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'F9',SRVTAGH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     RF,ASVBLOCK         SET LAST LOADED SCREEN                       
         MVI   NDTSSCRN-NDSAVED(RF),X'F9'                                       
*                                                                               
         L     RF,HDR.DMPFACS      GET INFO FROM DUMP FILE SSB                  
         AHI   RF,VSSB-SYSFACD     EXTRACT A(SSB) in dump file                  
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(4)                                                      
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                  EXTRACT SSB                                  
         L     RF,ATIA             POINTER TO A(SSB) IN DUMP FILE               
         L     RF,0(,RF)                                                        
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(SSBLNQ)                                                 
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RE,ATIA             Save off SSB                                 
         L     R0,=A(DMPSSB)                                                    
         A     R0,RELO                                                          
         LA    RF,L'DMPSSB                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               Save of SSB before we clobber it             
*                                                                               
         USING SSBD,R6                                                          
         L     R6,=A(DMPSSB)                                                    
         A     R6,RELO                                                          
         LA    R2,SRVD1                                                         
         MVC   0(L'BFADDR,R2),BFADDR   BUFFER ADDRESS                           
         AHI   R2,L'BFADDR                                                      
         LA    R0,SSBCRADR+1                                                    
         GOTO1 AHEXOUT,DMCB,(R0),(R2),3,0                                       
         AHI   R2,6                                                             
*                                                                               
         MVC   0(L'BFLEN,R2),BFLEN     BUFFER LENGTH                            
         AHI   R2,L'BFLEN                                                       
         ICM   R0,15,SSBCRLEN                                                   
         EDIT  (R0),(9,(R2)),0,ALIGN=LEFT,ZERO=NOBLANK                          
         AR    R2,R0                                                            
*                                                                               
         MVC   0(L'BFAVAIL,R2),BFAVAIL BUFFER LENGTH                            
         AHI   R2,L'BFAVAIL                                                     
         ST    R2,AAVAIL                                                        
         L     RF,SSBCRADR                                                      
********************************************************************            
* DISPLAY LINES                                                                 
********************************************************************            
         USING FHD,R3                                                           
         LA    R3,SRVZ1H                                                        
         LHI   R0,OLINEL                                                        
         XR    RE,RE                                                            
         LLC   RF,FHLN                                                          
         DR    RE,R0                                                            
*                                                                               
         STH   RF,HALF3            HALF3 = Number OF COLUMNS                    
         LHI   RF,1                                                             
         STH   RF,HALF4            HALF4 = CURRENT COLUMN                       
*                                                                               
         L     R2,SSBCRADR                                                      
         ST    R2,FROM                                                          
DLINE02  LA    R3,SRVZ1H           BACK TO START OF DISPLAY LINES               
*                                                                               
DLINE04  MVC   LEN,=AL2(HOLELQ)                                                 
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         LH    R4,HALF4                                                         
         BCTR  R4,0                                                             
         MHI   R4,OLINEL                                                        
         LA    R4,FHDA(R4)         R4 = A(OUTPUT BLOCK)                         
*                                                                               
         USING HOLED,R2                                                         
         L     R2,ATIA                                                          
         OC    HOKEY,HOKEY                                                      
         BZ    DLINE90                                                          
*                                                                               
         BRAS  RE,FORMAT           FORMAT OUTPUT BLOCK                          
*                                                                               
         ICM   RF,15,HPSLENH                                                    
         AHI   RF,HOLELQ                                                        
         L     R2,FROM                                                          
         AR    R2,RF               NEXT HOLE ENTRY                              
         ST    R2,FROM                                                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FHLN                                                        
         BZ    *+8                                                              
         BXH   R3,RF,DLINE04                                                    
*                                                                               
         LH    RF,HALF4            NEXT COLUMN                                  
         AHI   RF,1                                                             
         CH    RF,HALF3                                                         
         BH    DLINE90                                                          
         STH   RF,HALF4                                                         
         B     DLINE02                                                          
*                                                                               
DLINE90  L     RE,SSBCRLEN                                                      
         L     RF,FROM                                                          
         S     RF,SSBCRADR                                                      
         SR    RE,RF                                                            
         L     R2,AAVAIL                                                        
         EDIT  (RE),(9,(R2)),0,ALIGN=LEFT,ZERO=NOBLANK                          
         BH    EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT A PHASE LINE - A(PHSHDR)/PHASE NAME/LENGTH                   *         
* NTRY: R2     = A(HOLED ENTRY)                                       *         
*       R4     = A(OUTPUT AREA)                                       *         
***********************************************************************         
         USING PROGSPCD,HOKEY                                                   
         USING OLINED,R4                                                        
FORMAT   NTR1  ,                                                                
         MVC   OPHASE,DELETED                                                   
         CLC   HOKEY(PSKEYL),EFFS  PHASE HAS BEEN DELETED?                      
         BE    FMT02               YES                                          
         MVC   OPHASE,SPACES                                                    
*                                                                               
         GOTO1 AHEXOUT,DMCB,PSNAME,OPHASE,L'PSNAME,0                            
         MVC   OPHASE(1),PSLANG    SET LANGUAGE                                 
*                                                                               
         CLI   PSLVL,C' '          SET TEST LEVEL (IF ANY)                      
         BNH   *+10                                                             
         MVC   OPHASE+6(1),PSLVL                                                
*                                                                               
FMT02    MVC   FULL,FROM                                                        
         GOTO1 AHEXOUT,DMCB,FULL+1,OADDR,3,0                                    
*                                                                               
         EDIT  (B4,HPSLEN),(6,OLEN),0,ZERO=NOBLANK                              
         B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO SHOW WHOAMI TYPE INFORMATION                             *         
***********************************************************************         
WHOAMI   NTR1  BASE=*,LABEL=*                                                   
         XC    FERN,FERN                                                        
         MVI   HDRN,10             SET DEFAULT MESSAGE                          
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FB',SRVTAGH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     RF,ASVBLOCK         SET LAST LOADED SCREEN                       
         MVI   NDTSSCRN-NDSAVED(RF),X'FB'                                       
*                                                                               
         L     RF,HDR.DMPFACS      GET INFO FROM DUMP FILE SSB                  
         AHI   RF,VSSB-SYSFACD     EXTRACT A(SSB) in dump file                  
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(4)                                                      
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                  EXTRACT SSB                                  
         L     RF,ATIA             POINTER TO A(SSB) IN DUMP FILE               
         L     RF,0(,RF)           A(SSB)                                       
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(SSBLNQ)                                                 
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RE,ATIA             Save off SSB                                 
         L     R0,=A(DMPSSB)                                                    
         A     R0,RELO                                                          
         LA    RF,L'DMPSSB                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE               Save of SSB before we clobber it             
*                                                                               
         MVC   FROM,START                                                       
         MVC   LEN,=AL2(TUTLLENV)  SET LENGTH TO READ                           
         BRAS  RE,RDADR            GET UTL INTO TIA                             
         BNE   EXITL                                                            
*                                                                               
WMI02    MVC   SRVP4F(16),SPACES                                                
         MVC   SRVP4F(7),=CL7'A(UTL)='                                          
         GOTO1 AHEXOUT,DMCB,START,SRVP4F+7,4                                    
*                                                                               
         USING UTLD,R2                                                          
         L     R2,ATIA             R2=A(UTL FROM DUMP)                          
         OC    TPRNT,TPRNT         PRINTER?                                     
         BNZ   WMI44               YES                                          
*                                                                               
WMI04    OC    TUSER,TUSER         CONNECTED?                                   
         BZ    WMI08               NO                                           
*                                                                               
         L     R3,AIO              READ ID RECORD FOR CONNECTED USER            
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TUSER                                                    
         GOTO1 ADMGR,DMCB,(0,DMREAD),CTFILE,AIO,AIO                             
         CLI   8(R1),0                                                          
         BNE   WMIDIE                                                           
*                                                                               
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
         XR    RF,RF                                                            
WMI06    CLI   CTDSCEL,0           FIND DESCRIPTION ELEMENT                     
         BE    WMIDIE                                                           
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R3,RF,WMI06                                                      
         MVC   SRVCID(10),CTDSC    SET USER ID                                  
*                                                                               
WMI08    OC    TSVCREQ,TSVCREQ     SERVICE REQUEST?                             
         BZ    WMI10               NO                                           
         LHI   R0,1                HARD CODE SERVICE SYSTEM                     
         XR    R1,R1                                                            
         IC    R1,TSVCREQ+1        SET PROGRAM NUMBER                           
         B     WMI12                                                            
*                                                                               
WMI10    SLR   R0,R0                                                            
         IC    R0,TSYS             R0 = SYSTEM                                  
         SLR   R1,R1                                                            
         IC    R1,TPRG             R1 = PROGRAM                                 
*                                                                               
WMI12    LTR   R0,R0               CONNECTED?                                   
         BZ    WMI20                                                            
         CHI   R0,1                SERVICE SYSTEM?                              
         BE    *+8                 YES                                          
         IC    R0,TOVSYS           SET OVERLAY SYSTEM                           
*                                                                               
         LA    R3,SYSLST           GET OVERLAY SYSTEM NAME                      
         USING SYSLSTD,R3                                                       
         LH    RE,0(R3)                                                         
         ICM   RF,15,2(R3)                                                      
         A     RF,RELO                                                          
         AHI   R3,6                                                             
         CLM   R0,1,SYSLNUM                                                     
         BE    WMI14                                                            
         BXLE  R3,RE,*-8                                                        
         STC   R0,BYTE             HEXOUT UNKNOWN SYSTEM                        
         GOTO1 AHEXOUT,DMCB,BYTE,SRVCSS,L'BYTE                                  
         B     WMI16                                                            
*                                                                               
WMI14    MVC   SRVCSS,SYSLNAME     MOVE IN OVERLAY SYSTEM NAME                  
         CHI   R0,1                                                             
         BE    *+8                                                              
         IC    R0,TSYS             CONNECTED SYSTEM                             
*                                                                               
         L     R3,ASELIST                                                       
         LH    RE,0(R3)                                                         
         ICM   RF,15,2(R3)                                                      
         AHI   R3,6                                                             
         USING SELISTD,R3                                                       
         CLM   R0,1,SESYS          MATCH CONNECTED SYSTEM                       
         BE    *+10                                                             
         BXLE  R3,RE,*-8                                                        
         DC    H'0'                                                             
*                                                                               
         ST    R3,ASYS             SAVE A(SELIST ENTRY)                         
         L     R3,SEPGMS           PROGRAM                                      
         LH    RE,0(R3)                                                         
         ICM   RF,15,2(R3)                                                      
         AHI   R3,6                                                             
         USING PGMLSTD,R3                                                       
         CLM   R1,1,PGMNUM                                                      
         BE    WMI18                                                            
         BXLE  R3,RE,*-8                                                        
*                                                                               
WMI16    MVC   SRVCPG(4),=C'Pgm='  HEXOUT UNKNOWN PROGRAM                       
         STC   R1,BYTE                                                          
         GOTO1 AHEXOUT,DMCB,BYTE,SRVCPG+4,L'BYTE                                
         B     WMI20                                                            
*                                                                               
WMI18    MVC   SRVCPG(L'PGMNAME),PGMNAME   SET CONNECTED PROGRAM                
*                                                                               
WMI20    L     R3,ACTRYTAB         GET CONNECTED COUNTRY                        
         LH    RE,0(R3)                                                         
         ICM   RF,15,2(R3)                                                      
         AHI   R3,6                                                             
         USING CTRYTABD,R3                                                      
         CLC   CTRYCODE,TCTRY                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         MVC   SRVCTY,CTRYNAMN                                                  
*                                                                               
         MVC   BYTE,TLANG          GET LANGUAGE                                 
         NI    BYTE,X'0F'                                                       
         L     R3,ALANGTAB                                                      
         LH    RE,0(R3)                                                         
         ICM   RF,15,2(R3)                                                      
         AHI   R3,6                                                             
         USING LANGTABD,R3                                                      
         CLC   LANGCODE,BYTE                                                    
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
*                                                                               
D        USING SSBD,RF                                                          
         L     RF,=A(DMPSSB)                                                    
         A     RF,RELO                                                          
         MVC   SRVLNG,LANGFULN                                                  
         MVC   SRVFAC(4),D.SSBSYSN4                                             
         TM    D.SSBSTAT4,SSBSAOR                                               
         BZ    WMI21                                                            
         LLC   R1,D.SSBSYSIX                                                    
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         LA    RE,SRVFAC+3                                                      
         CLI   D.SSBSYSN4+3,C' '                                                
         BNH   *+8                                                              
         AHI   RE,1                                                             
         STC   R1,0(RE)                                                         
         DROP  D                                                                
*                                                                               
WMI21    OC    TUSER,TUSER         CONNECTED TO A USERID?                       
         BZ    WMI22                                                            
         CURED (B2,TUSER),(4,SRVIDN),0,ALIGN=LEFT                               
         LA    R3,SRVIDN                                                        
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         MVC   1(2,R3),TAGY                                                     
         LA    R3,3(R3)                                                         
         MVI   0(R3),C'/'                                                       
         GOTO1 AHEXOUT,DMCB,TAGYB,1(R3),1,0                                     
*                                                                               
WMI22    CLI   TSYS,0              SE NUMBER                                    
         BE    WMI24                                                            
         CURED (B1,TSYS),(3,SRVSEN),0,ALIGN=LEFT                                
         LA    R1,SRVSEN           BUMP TO POSTION AFTER SE NUMBER              
         AR    R1,R0                                                            
         MVI   0(R1),C'/'                                                       
         L     RF,ASYS                                                          
         MVC   1(7,R1),SENAME-SELISTD(RF)                                       
*                                                                               
WMI24    TM    TTEST,(TTESTCIL+TTESTLVL)                                        
         BZ    WMI26                                                            
         MVC   SRVTSTL(1),TTEST                                                 
         NI    SRVTSTL,TTESTLVL                                                 
         OI    SRVTSTL,X'C0'       MAKE TEST LEVEL A-C                          
*                                                                               
         LA    R1,SRVTSTL+1                                                     
         CLI   SRVTSTL,X'C0'                                                    
         BNE   *+14                                                             
         MVC   SRVTSTL(3),=C'Yes'                                               
         LA    R1,SRVTSTL+3                                                     
*                                                                               
         TM    TTEST,TTESTCIL                                                   
         BZ    *+10                                                             
         MVC   0(8,R1),=C',cil=yes'                                             
*                                                                               
WMI26    CLI   TSYS,0              PROGRAM AUTH                                 
         BE    WMI28                                                            
         GOTO1 AHEXOUT,DMCB,TAUTH,SRVAUTH,2                                     
*                                                                               
WMI28    TM    TTEST,TTESTTAC      TEST ID                                      
         BZ    WMI30                                                            
         GOTO1 AHEXOUT,DMCB,TACCS,SRVAACC,4,=C'TOG'                             
         ICM   RF,15,TACCS                                                      
         USING TSTTABD,RF                                                       
         MVC   SRVTSTI,TSTACCS                                                  
         B     WMI32                                                            
         DROP  RF                                                               
*                                                                               
WMI30    CLI   TSYS,0              ACCESS CODE                                  
         BE    WMI32                                                            
         OC    TACCS,TACCS                                                      
         BZ    WMI32                                                            
         MVC   SRVAACT,=CL11'Access Code'                                       
         MVC   SRVTSTT,=CL11'           '                                       
         MVC   SRVAACC(4),TACCS                                                 
*&&US                                                                           
         L     R3,ASYS             A(SELIST ENTRY)                              
         USING SELISTD,R3                                                       
         CLI   SENAME,C'A'         TEST ACCOUNT SYSTEM                          
         BNE   WMI32                                                            
         CLI   TACCS,C'T'          TALENT ACCESS CODES                          
         BNE   WMI32                                                            
         GOTO1 AHEXOUT,DMCB,TACCS+1,SRVAACC+1,2  EXPAND LEDGER/CPY              
         MVC   SRVAACC+5(1),TACCS+3                                             
         DROP  R3                                                               
*&&                                                                             
WMI32    MVC   SRVUPDT(3),=C'Yes'                                               
         TM    TTEST,TTESTNOU      UPDATE                                       
         BZ    WMI34                                                            
         MVC   SRVUPDT(3),=C'No '                                               
         TM    TTEST,TTESTTAC      TEST IF TACCS IS A(TSTTAB)                   
         BZ    WMI34                                                            
         ICM   RF,15,TACCS                                                      
         USING TSTTABD,RF                                                       
         OC    TSTLOW,TSTLOW                                                    
         BZ    *+10                                                             
         MVC   SRVUPDT+2(4),=C',Log'                                            
         DROP  RF                                                               
*                                                                               
WMI34    OC    TPASSWD,TPASSWD     PASSWORD NAME                                
         BZ    WMI40                                                            
*                                                                               
         L     R3,AIO                                                           
         USING CTTREC,R3                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   23(2,R3),TPASSWD                                                 
         GOTO1 ADMGR,DMCB,(0,DMREAD),CTFILE,AIO,AIO                             
         CLI   8(R1),0                                                          
         BE    WMI36                                                            
*                                                                               
         USING CT0REC,R3                                                        
         XC    CT0KEY,CT0KEY       BUILD AUTH(NUMERIC) KEY                      
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TAGYSEC                                                  
         MVC   CT0KNUM,TPASSWD                                                  
         GOTO1 ADMGR,DMCB,(0,DMREAD),CTFILE,AIO,AIO                             
         CLI   8(R1),0                                                          
         BNE   WMI40                                                            
*                                                                               
WMI36    AHI   R3,CTTDATA-CTTREC                                                
         USING CTPASD,R3                                                        
         XR    RF,RF                                                            
WMI38    CLI   CTPASEL,0           LOOK FOR PASSIVE ELEMENT                     
         BE    WMI42                                                            
         CLI   CTPASEL,SAPALELQ                                                 
         BE    WMI39A                                                           
         CLI   CTPASEL,CTPASELQ                                                 
         BE    *+12                                                             
WMI39    IC    RF,CTPASLEN                                                      
         BXH   R3,RF,WMI38                                                      
                                                                                
         MVC   SRVPNME,CTPASDTA                                                 
         CLC   =C'IBESHOWN',SRVP4                                               
         BE    *+10                                                             
         MVC   SRVPNME,=10C'*'                                                  
         B     WMI39                                                            
*                                                                               
         USING SAPALD,R3                                                        
WMI39A   MVC   SRVCPID(L'SAPALPID),SAPALPID                                     
         B     WMI39                                                            
         DROP  R3                                                               
*                                                                               
WMI40    OC    TPERSON,TPERSON     DON'T DISPLAY SECRET CODES                   
         BZ    WMI42                                                            
         BRAS  RE,GETSCOD          GET SECRET PASSWORD                          
*                                                                               
WMI42    XR    R1,R1               SESSION INFORMATION                          
         IC    R1,TSESSION                                                      
         LA    R1,C'A'(R1)                                                      
         STC   R1,SRVSESN                                                       
*                                                                               
         LA    R3,SRVSESN+2                                                     
         L     RE,ASSB             EXTRACT MAX SESSION INFO FROM SSB            
         MVC   FULL+0(2),SSBSSMAX-SSBD(RE)                                      
         MVC   FULL+2(2),SSBSSMXP-SSBD(RE)                                      
         TM    TSTATC,TSTCXSES     TEST IF TRM SUPPORTS EXTRA SESSIONS          
         BO    WMI43                                                            
         LHI   R0,4                OLD CMV SUPPORTS 4 SESSIONS                  
         LHI   R1,4                                                             
         CLC   FULL+2(2),FULL+0                                                 
         BNH   *+8                                                              
         LHI   R1,5                PLUS EXTRA HIDDEN SESSION                    
         STH   R0,FULL                                                          
         STH   R1,FULL+2                                                        
*                                                                               
WMI43    LH    R0,FULL                                                          
         STC   R0,00(R3)                                                        
         OI    0(R3),C'0'          MAXIMUM LOGICAL SESSIONS                     
         MVI   1(R3),C' '                                                       
         CLC   FULL+2(2),FULL+0                                                 
         BNH   *+8                                                              
                                                                                
         MVI   1(R3),C'+'          EXTRA PHYSICAL SESSIONS                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,1,TSSBITS        ACTIVE SESSION BITS                          
         AHI   R3,2                                                             
         LA    R1,1                                                             
WMI43A   SRDL  RE,1                MOVE OUT NEXT SESSION ACTIVE BIT             
         MVI   0(R3),C'*'                                                       
         LTR   RF,RF                                                            
         BZ    WMI43B                                                           
         STC   R1,0(R3)                                                         
         OI    0(R3),X'C0'                                                      
WMI43B   LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         SR    RF,RF                                                            
         BCT   R0,WMI43A                                                        
*                                                                               
         MVI   SRVASWP,C'N'        AUTOSWAP INFORMATION                         
         TM    TSTAT8,TST8ASWP                                                  
         BZ    *+8                                                              
         MVI   SRVASWP,C'Y'                                                     
*                                                                               
WMI44    CURED (B2,TNUM),(4,SRVTNO),0,ALIGN=LEFT                                
         MVC   SRVLUID,TLUID       SHOW LUID                                    
*                                                                               
         OC    TPRNT,TPRNT                                                      
         BZ    WMI54                                                            
         MVCDD SRVTTY,SR#PRR                                                    
         TM    TTYPE,TTYPERMC                                                   
         BZ    WMI56                                                            
         MVCDD SRVTTY,SR#SHTL                                                   
         B     WMI56                                                            
*                                                                               
WMI54    MVC   SRVTTY,=CL10'3270'                                               
         TM    TTYPE,TTYPE327                                                   
         BO    WMI56                                                            
         MVC   SRVTTY,=CL10'ICC'                                                
         TM    TTYPE,TTYPEICC                                                   
         BO    WMI56                                                            
         MVC   SRVTTY,=CL10'TWX'                                                
         TM    TTYPE,TTYPETWX                                                   
         BO    WMI56                                                            
         MVC   SRVTTY,=CL10'RJE'                                                
         TM    TTYPE,TTYPE378                                                   
         BO    WMI56                                                            
         MVC   SRVTTY,=CL10'COURIER'                                            
*                                                                               
WMI56    OC    TNUM,TNUM           UNLESS NOT IN UTL                            
         BZ    WMI57                                                            
         MVC   SRVOFFC,TOFFCODE    OFFICE CODE                                  
         GOTO1 AHEXOUT,DMCB,HDR.DMPUTL,SRVAUTL,4,0                              
*                                                                               
         ICM   R0,15,TBUFF         A(TBUFF)                                     
         BZ    WMI57                                                            
         ST    R0,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,SRVATBU,4                                      
*                                                                               
WMI57    DS    0H                                                               
*&&UK                                                                           
         SR    R0,R0               OUTPUT PC PROGRAM INFO IF DEFINED            
         ICM   R0,3,TXPNUM                                                      
         BZ    WMI60                                                            
         TM    TXPTYPE,TXPTPC                                                   
         BZ    WMI60                                                            
         CVD   R0,DUB              OUTPUT PC APP PROGRAM NUMBER                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R2),DUB                                                      
         L     RF,=V(FAXPTPC)      POINT TO PC PROGRAM TABLE                    
         A     RF,RELO                                                          
         USING FAXPTABD,RF                                                      
         LA    RF,L'FAXPNTRY(RF)   BUMP PAST FIRST ENTRY                        
*                                                                               
WMI57A   CLI   FAXPTYPE,0          SEARCH FOR NUMBER TO GET NAME                
         BE    WMI60                                                            
         TM    FAXPTYPE,FAXPTPC                                                 
         BZ    WMI57B                                                           
         CLM   R0,3,FAXPNUM                                                     
         BE    WMI57C                                                           
WMI57B   LA    RF,L'FAXPNTRY(RF)                                                
         B     WMI57A                                                           
WMI57C   MVC   SRVPAPP(10),FAXPNEXE                                             
         DROP  RF                                                               
*&&                                                                             
WMI60    DS    0H                                                               
*&&US                                                                           
         ICM   RF,14,TXPVER        A.B.CC.DDD                                   
         LA    R1,SRVPVER                                                       
         SR    RE,RE               A                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(1,R1),DUB                                                      
         MVI   1(R1),C'.'                                                       
         LA    R1,2(R1)                                                         
*                                                                               
         SR    RE,RE               B                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(1,R1),DUB                                                      
         MVI   1(R1),C'.'                                                       
         LA    R1,2(R1)                                                         
*                                                                               
         SR    RE,RE               CC                                           
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R1),DUB                                                      
         MVI   2(R1),C'.'                                                       
         LA    R1,3(R1)                                                         
*                                                                               
         SR    RE,RE               DDD                                          
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(3,R1),DUB                                                      
*&&                                                                             
         OC    TSIN,TSIN                                                        
         BZ    EXITOK                                                           
         GOTO1 AHEXOUT,DMCB,TSIN,SRVSIN#,4                                      
*                                                                               
         B     EXITOK                                                           
*                                                                               
WMIDIE   DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GET PASSWORD                                                        *         
***********************************************************************         
GETSCOD  NTR1  ,                                                                
         L     R3,AIO                                                           
         USING CT0REC,R3                                                        
         XC    CT0KEY,CT0KEY       BUILD AUTH(NUMERIC) KEY                      
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TAGYSEC                                                  
         MVC   CT0KNUM,TPERSON                                                  
         GOTO1 ADMGR,DMCB,(0,DMREAD),CTFILE,AIO,AIO                             
         CLI   8(R1),0                                                          
         BNE   EXITOK                                                           
*                                                                               
         LA    R3,CT0DATA                                                       
         USING CTPASD,R3                                                        
         XR    RF,RF                                                            
GSC02    CLI   CTPASEL,0           FIND AUTH(ALPHA) ELEMENT                     
         BE    EXITOK                                                           
         CLC   0(2,R3),=X'030C'                                                 
         BE    *+12                                                             
         IC    RF,CTPASLEN                                                      
         BXH   R3,RF,GSC02                                                      
         MVC   SRVPNME,CTPASDTA    SET PASSWORD                                 
         CLC   =C'IBESHOWN',SRVP4                                               
         BE    *+10                                                             
         MVC   SRVPNME,=10C'*'                                                  
*                                                                               
         L     R3,AIO                                                           
         AHI   R3,SA0DATA-SA0REC   FIND PERSONAL ID ELEMENT                     
         XR    RF,RF                                                            
         USING SAPALD,R3                                                        
GSC04    CLI   SAPALEL,0                                                        
         BE    EXITOK                                                           
         CLI   SAPALEL,SAPALELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPALLN                                                       
         BXH   R3,RF,GSC04                                                      
         MVC   DUB,SAPALPID        SAVE PERSONAL ID                             
         MVC   SRVCPID(L'DUB),DUB                                               
*                                                                               
         L     R3,AIO              GET PASSWORD CODE FROM PERSON RECORD         
         USING SAPEREC,R3                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,TAGYSEC                                                  
         MVC   SAPEPID,DUB                                                      
         MVC   SAPEDEF,EFFS                                                     
         XC    SAPEDEF,TODAYB                                                   
         GOTO1 ADMGR,DMCB,(0,DMREAD),CTFILE,AIO,AIO                             
         CLI   8(R1),0                                                          
         BNE   EXITL                                                            
*                                                                               
         LA    R3,SAPEDATA         GET ELEMENT DATA                             
         USING SAPWDD,R3                                                        
         XR    RF,RF                                                            
GSC06    CLI   SAPWDEL,0                                                        
         BE    EXIT                                                             
         CLI   SAPWDEL,SAPWDELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPWDLN                                                       
         BXH   R3,RF,GSC06                                                      
         MVC   SRVPNME,SAPWDCOD-SAPWDD(R3)                                      
         CLC   =C'IBESHOWN',SRVP4                                               
         BE    *+10                                                             
         MVC   SRVPNME,=10C'*'                                                  
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SECTION THAT SHOWS THE PARAMETER LIST REQUESTED                     *         
***********************************************************************         
PARMSHOW NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
         MVI   HDRN,11                                                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,DUMPREG        GET DUMP REGISTER IF SET                     
         BZ    PSHW01                                                           
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         LA    RF,SETREGS(RF)                                                   
         L     R0,START            ADJUST START POINT FOR REGISTER              
         A     R0,0(RF)                                                         
         ST    R0,START                                                         
*                                                                               
PSHW01   L     R6,ASVBLOCK                                                      
         USING NDSAVED,R6                                                       
         CLI   SRVP2H+5,0          NOTHING IN FIELD 2?                          
         BE    PSHW02              NO JUST DISPLAY THE LIST                     
*                                                                               
         OI    SAVEDFLG,X'40'      WE HAVE A PARAMETER LIST NOW                 
*                                                                               
         L     RF,START                                                         
         A     RF,DISPLACE                                                      
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(NDPRMCQ*L'NDPRM)                                        
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA             COPY PARAMETERS                              
         MVC   NDPRM(NDPRMLQ),0(RF)                                             
         BRAS  RE,WTTWAB           AND WRITE TWA OFF                            
*                                                                               
PSHW02   LA    R2,DMPL1H                                                        
         USING FHD,R2                                                           
         USING DMPPARMD,FHDA                                                    
         MVC   DMPPPARM,=C'P#'                                                  
         MVC   DMPPCONT,=C'Contents'                                            
         MVC   DMPPHEX,COUNTH                                                   
         MVC   DMPPALPH,COUNTD                                                  
         BRAS  RE,PSNXT                                                         
         MVC   DMPPPARM,HYPHENS                                                 
         MVC   DMPPCONT,HYPHENS                                                 
         MVC   DMPPHEX,HYPHENS                                                  
         MVC   DMPPALPH,HYPHENS                                                 
         BRAS  RE,PSNXT                                                         
*                                                                               
         LHI   R4,NDPRMCQ          R4 = PARAMETER COUNTDOWN                     
         LA    R3,NDPRM            R3 = 1ST PARAMETER                           
*                                                                               
PSHW04   LHI   R1,NDPRMCQ+1        SHOW P#                                      
         SR    R1,R4                                                            
         MVI   DMPPPARM,C'P'                                                    
         STC   R1,DMPPPARM+1                                                    
         OI    DMPPPARM+1,X'F0'                                                 
*                                                                               
         MVC   FULL,0(R3)          SHOW CONTENTS OF PARAMETER                   
         GOTO1 AHEXOUT,DMCB,FULL,DMPPCONT,4                                     
*                                                                               
         XR    RF,RF               SEE IF VALID ADDRESS                         
         ICM   RF,7,FULL+1                                                      
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(32)                                                     
         BRAS  RE,RDADR            GET (FULL)                                   
         BNE   PSHW08                                                           
*                                                                               
         LHI   R0,2                PRINT 2 LINES OF DATA AT (FULL)              
         L     R5,ATIA                                                          
PSHW06   GOTO1 AHEXOUT,DMCB,(R5),DMPPHEX,16                                     
         MVC   DMPPALPH,0(R5)                                                   
         TR    DMPPALPH,TRTAB                                                   
         BRAS  RE,PSNXT            NEXT LINE                                    
         AHI   R5,16                                                            
         BCT   R0,PSHW06           PRINT THE 2ND LINE                           
         B     PSHW10                                                           
*                                                                               
PSHW08   MVI   DMPPHEX,C'('                                                     
         MVC   DMPPHEX+1(4),0(R3)    IT'S NOT                                   
         TR    DMPPHEX+1(4),TRTAB      SEE IF IT IS VALID EBCDIC                
         MVI   DMPPHEX+5,C')'                                                   
         BRAS  RE,PSNXT                                                         
         BRAS  RE,PSNXT                                                         
*                                                                               
PSHW10   AHI   R3,L'NDPRM          R3 = NEXT PARAMETER IN LIST                  
         BCT   R4,PSHW04           LOOP UNTIL WE'VE DISPLAY ALL PARMS           
         XC    FERN,FERN                                                        
         B     EXITOK                                                           
*                                                                               
PSNXT    XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         BR    RE                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE REGISTERS AND OTHER INFO ON SCREEN                      *         
* NTRY: SETREGS(64) = REGISTERS R0 - RF TO DISPLAY                    *         
***********************************************************************         
DISPREGS NTR1  BASE=*,LABEL=*                                                   
         XC    WRK,WRK             HEXOUT DATA INTO WRK                         
         GOTO1 AHEXOUT,DMCB,SETREGS,WRK,64                                      
*                                                                               
         LHI   R0,8                R0 - R7                                      
         LA    RF,SRVRGS1                                                       
         LA    RE,WRK                                                           
DRG02    MVC   0(8,RF),0(RE)                                                    
         AHI   RF,9                                                             
         AHI   RE,8                                                             
         BCT   R0,DRG02                                                         
*                                                                               
         LHI   R0,8                R8 - RF                                      
         LA    RF,SRVRGS2                                                       
DRG04    MVC   0(8,RF),0(RE)                                                    
         AHI   RF,9                                                             
         AHI   RE,8                                                             
         BCT   R0,DRG04                                                         
*                                                                               
         GOTO1 AHEXOUT,DMCB,HDR.DMPPSWD,WRK,8                                   
         MVC   SRVPSW+0(8),WRK     PSW                                          
         MVC   SRVPSW+9(8),WRK+8                                                
*NOP     MVC   PSW,WORK+8          SAVE OFF PSW                                 
*                                                                               
         L     R0,START            SAVE CURRENT DUMP START POINTER              
         LH    R1,LEN                                                           
*                                                                               
         L     RF,HDR.DMPFACS      GET INFO FROM DUMP FILE SSB                  
         AHI   RF,VSSB-SYSFACD                                                  
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(4)                                                      
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA             POINTER TO A(SSB) IN DUMP FILE               
         L     RF,0(RF)                                                         
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(1024)                                                   
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA             RETURNS A(SSB) IN CORE BUFFER                
         USING SSBD,RF                                                          
         MVC   SRVDAT,=CL8'TIME===>'                                            
         CLC   TODAY3,SSBDATEB     COMPARE SSB DUMP DATE WITH TODAY             
         BE    *+10                                                             
         MVC   SRVDAT,SSBDATE      IF DIFFERENT SHOW DUMP DATE                  
         DROP  RF                                                               
*                                                                               
         ST    R0,START            RESTORE DUMP START POINTER                   
         STH   R1,LEN              RESTORE DUMP LENGTH                          
*                                                                               
         GOTO1 ADATTIM,DMCB,(X'81',HDR.DMPTIME),WORK                            
         MVC   SRVTIM+0(2),WORK+8                                               
         MVC   SRVTIM+3(2),WORK+10                                              
         MVC   SRVTIM+6(2),WORK+12                                              
         MVI   SRVTIM+2,C'.'                                                    
         MVI   SRVTIM+5,C'.'                                                    
*                                                                               
         XR    R1,R1               PSW-RB                                       
         ICM   R1,7,HDR.DMPPSWD+5                                               
         XR    RF,RF                                                            
         ICM   RF,7,HDR.DMPRB+1                                                 
         SR    R1,RF               OFFSET TO NEXT INTRUCTION                    
         ST    R1,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,SRVDSP,4                                       
*                                                                               
         L     RF,ASVBLOCK                                                      
         TM    NDFLAG-NDSAVED(RF),NDFSRSET                                      
         BZ    DRG06                                                            
         MVC   SRVDSPD,=CL8'RE-RB==>'                                           
         XR    RE,RE                                                            
         ICM   RE,7,SETREGS+56+1                                                
         XR    RF,RF                                                            
         ICM   RF,7,SETREGS+44+1                                                
         SR    RE,RF                                                            
         ST    RE,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,SRVDSP,4                                       
*                                                                               
DRG06    CLI   DINDIC,C'I'         LOOP INDICATOR                               
         BNE   *+14                                                             
         MVC   SRVLOOP,=CL8'**LOOP**'                                           
         B     EXITOK                                                           
*                                                                               
         CLI   HDR.DMPSRSN,255     SPIE DUMP?                                   
         BNE   DRG10               NO - ESTAE                                   
*                                                                               
         LA    RF,SOCTAB                                                        
DRG08    CLI   0(RF),255                                                        
         BE    DRG10                                                            
         CLC   HDR.DMPSRSN+2(1),0(RF)                                           
         BE    *+12                                                             
         AHI   RF,L'SOCTAB                                                      
         B     DRG08                                                            
*                                                                               
         MVC   SRVLOOP,1(RF)                                                    
         B     EXITOK                                                           
*                                                                               
DRG10    MVC   SRVLOOP,SPACES                                                   
         GOTO1 AHEXOUT,DMCB,HDR.DMPSRSN,DUB,L'DMPSRSN                           
         MVI   SRVLOOP,C'S'                                                     
         MVC   SRVLOOP+1(3),DUB                                                 
         MVI   SRVLOOP+4,C'U'                                                   
         MVC   SRVLOOP+5(3),DUB+3                                               
         B     EXITOK                                                           
*                                                                               
SOCTAB   DS    0XL9                                                             
         DC    X'01',CL8'Op-Code '                                              
         DC    X'02',CL8'Priv Ins'                                              
         DC    X'03',CL8'Execute '                                              
         DC    X'04',CL8'Protectn'                                              
         DC    X'05',CL8'Address '                                              
         DC    X'06',CL8'Specn   '                                              
         DC    X'07',CL8'Data    '                                              
         DC    X'09',CL8'Int div '                                              
         DC    X'0B',CL8'Dec div '                                              
         DC    X'0C',CL8'XP oflow'                                              
         DC    X'0F',CL8'Flt div '                                              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GET THE TICTRACE INFORMATION                           *         
***********************************************************************         
TICTRACE NTR1  BASE=*,LABEL=*                                                   
         XC    DISPLACE,DISPLACE                                                
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
         CLI   FHIL,0              ZERO INPUT IS OK                             
         BE    TTR02                                                            
*                                                                               
         TM    FHII,FHIINU         OTHERWISE MUST BE A NUMBER                   
         BO    *+12                                                             
         MVI   FERN,6                                                           
         B     EXITL                                                            
*                                                                               
         XR    RF,RF               PACK NUMBER                                  
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,TTRPACK                                                       
         CVB   R0,DUB                                                           
         ST    R0,DISPLACE         GET IT AS BINARY                             
         B     TTR02                                                            
*                                                                               
TTRPACK  PACK  DUB,FHDA(0)                                                      
*                                                                               
TTR02    L     R0,DISPLACE         DISPLACE = # ENTRIES TO GO BACK              
         CHI   R0,TICNTRY#                                                      
         BL    *+12                                                             
         MVI   FERN,23             TOO MUCH INPUT                               
         B     EXITL                                                            
*                                                                               
         MVC   SRVP3,SPACES        SET NEXT GOBACK NUMBER                       
         AHI   R0,20                                                            
         CHI   R0,TICNTRY#                                                      
         BNL   TTR04                                                            
         CURED (R0),(2,SRVP3),0,ALIGN=LEFT                                      
*                                                                               
TTR04    L     RF,START            TABLE AT TICTOCT+88                          
         AHI   RF,88                                                            
         ST    RF,FROM                                                          
         MVC   LEN,=AL2((TICNTRY#*TICNTRYL)+12)                                 
         BRAS  RE,RDADR            GET TICTRACE TABLE INTO TIA                  
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA             GET A(CURRENT ENTRY)                         
         L     R0,8(RF)                                                         
         S     R0,FROM             ADJUST AND                                   
         ST    R0,8(RF)            SET DISPLACEMENT CORRECTLY                   
         BRAS  RE,TICTBLE                                                       
         BNE   EXITL                                                            
         MVI   HDRN,14                                                          
         MVI   FERN,0                                                           
         LA    RF,SRVP4H                                                        
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GET THE TICTPOPS INFORMATION                           *         
***********************************************************************         
TICPOPS  NTR1  BASE=*,LABEL=*                                                   
         XC    DISPLACE,DISPLACE                                                
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         USING FHD,R2                                                           
         ST    R3,FADRH                                                         
         CLI   FHIL,0              ZERO INPUT IS OK                             
         BE    TTP02                                                            
*                                                                               
         TM    FHII,FHIINU         OTHERWISE MUST BE A NUMBER                   
         BO    *+12                                                             
         MVI   FERN,6                                                           
         B     EXITL                                                            
*                                                                               
         XR    RF,RF               PACK NUMBER                                  
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,TTPPACK                                                       
         CVB   R0,DUB                                                           
         ST    R0,DISPLACE         GET IT AS BINARY                             
         B     TTP02                                                            
*                                                                               
TTPPACK  PACK  DUB,FHDA(0)                                                      
*                                                                               
TTP02    L     R0,DISPLACE         DISPLACE = # ENTRIES TO GO BACK              
         CHI   R0,TICNTRY#                                                      
         BL    *+12                                                             
         MVI   FERN,23             TOO MUCH INPUT                               
         B     EXITL                                                            
*                                                                               
         MVC   SRVP3,SPACES        SET NEXT GOBACK NUMBER                       
         AHI   R0,20                                                            
         CHI   R0,TICNTRY#                                                      
         BNL   TTP04                                                            
         CURED (R0),(2,SRVP3),0,ALIGN=LEFT                                      
*                                                                               
TTP04    L     RF,START            TABLE AT TICTOCT+88+L'TICTRACE TBL           
         AHI   RF,88                                                            
         AHI   RF,((TICNTRY#*TICNTRYL)+12)                                      
         ST    RF,FROM                                                          
         MVC   LEN,=AL2((TICNTRY#*TICNTRYL)+12)                                 
         BRAS  RE,RDADR            GET TICTRACE TABLE INTO TIA                  
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA             GET A(CURRENT ENTRY)                         
         L     R0,8(RF)                                                         
         S     R0,FROM             ADJUST AND                                   
         ST    R0,8(RF)            SET DISPLACEMENT CORRECTLY                   
         BRAS  RE,TICTBLE                                                       
         BNE   EXITL                                                            
         MVI   HDRN,15                                                          
         MVI   FERN,0                                                           
         LA    RF,SRVP4H                                                        
         ST    RF,FADRH                                                         
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* DISPLAY TICTRACE INFORMATION                                       *          
* NTRY: ATIA   = A(TABLE)                                            *          
**********************************************************************          
TICTBLE  NTR1  BASE=*,LABEL=*                                                   
         L     RF,ATIA                                                          
         CLC   0(8,RF),=C'TICTRACE'                                             
         BE    TTB02                                                            
         CLC   0(8,RF),=C'*TICPOP*'                                             
         BE    TTB02                                                            
         DC    H'0'                                                             
*                                                                               
TTB02    GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         L     R0,ASORTBLK         CLEAR SORT BUILD BLOCK                       
         LHI   R1,SORTBLKL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,ATIA                                                          
         L     R0,8(R1)            OFFSET INTO TABLE OF MOST RECENT             
         AR    R0,R1                                                            
         ST    R0,ABEGIN           SAVE A(MOST RECENT)                          
*                                                                               
         AHI   R1,12               A(FIRST ENTRY)                               
         ST    R1,FSTENT                                                        
         AHI   R1,((TICNTRYL)*(TICNTRY#)-TICNTRYL)                              
         ST    R1,ENDTBL           A(LAST TABLE ENTRY)                          
*                                                                               
         ICM   R0,15,DISPLACE      FIND OUR START POINT                         
         BZ    TTB06               MOST RECENT                                  
*                                                                               
         L     RF,ABEGIN                                                        
TTB04    AHI   RF,-(TICNTRYL)                                                   
         C     RF,FSTENT                                                        
         BNE   *+8                                                              
         L     RF,ENDTBL                                                        
         BCT   R0,TTB04                                                         
         ST    RF,ABEGIN           START HERE                                   
*                                                                               
TTB06    BRAS  RE,FILLBLCK         BUILD MY DISPLAY BLOCK                       
         BNE   EXITL                                                            
         BRAS  RE,FILLSCRN         DISPLAY ONTO SCREEN                          
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* FILL MY DISPLAY TABLE WITH REQUESTED INFORMATION                   *          
**********************************************************************          
FILLBLCK NTR1  ,                                                                
         L     R2,ASORTBLK                                                      
         USING TBDSECT,R2                                                       
         LHI   R0,ALLSCLNE         NUMBER OF DISPLAY LINES ON SCREEN            
         L     RF,ABEGIN           WHERE TO START IN TICTRACE TABLE             
*                                                                               
FBLK02   MVC   TBDTYPE,0(RF)       TYPE                                         
         MVC   TBDTIME,8(RF)       TIME                                         
         CLI   0(RF),C'*'          POP?                                         
         BNE   FBLK04              NO                                           
         MVC   TBDTYPE,0(RF)                                                    
         MVC   TBDTIME,4(RF)       MOVE AGAIN BECAUSE MEL SCREWED UP            
         MVC   TBDPSW,8(RF)        SAVE PSW ADDRESS (THANKS MEL)                
         MVC   TBDBREG,64(RF)      SAVE RB                                      
         B     FBLK06                                                           
*                                                                               
FBLK04   MVC   TBDATCB,16(RF)      ADDRESS TCB ENTRY                            
         MVC   TBDSTATS,20(RF)     TCB INFO                                     
         MVC   TBDUSER,32(RF)      USER ID                                      
*                                                                               
FBLK06   AHI   RF,-(TICNTRYL)      POINT TO PREVIOUS ENTRY                      
         C     RF,FSTENT           BEFORE FIRST ENTRY?                          
         BNL   *+8                 NO                                           
         L     RF,ENDTBL           YES - LOAD A(LAST ENTRY)                     
         AHI   R2,TBDLENQ                                                       
         BCT   R0,FBLK02           NEXT ENTRY IN MY TABLE                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* GET INFO AND FILL SCREEN                                           *          
**********************************************************************          
FILLSCRN NTR1  ,                                                                
         LA    R2,DMPL1H           FIRST SCREEN LINE                            
         USING FHD,R2                                                           
         USING SCDSECT,FHDA                                                     
         MVC   SCDAWRK,=C'TCBWRK'                                               
         MVC   SCDATWA,=C'TCBTWA'                                               
         MVC   SCDAPGM,=C'TCBPGM'                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF               NEXT SCREEN LINE                             
*                                                                               
         LHI   R3,ALLSCLNE         NUMBER OF DISPLAY LINES ON SCREEN            
         L     R4,ASORTBLK                                                      
         USING TBDSECT,R4                                                       
*                                                                               
FSC02    MVC   SCDTIME,DOTFILL     DISPLAY TIME                                 
*                                                                               
         L     RE,TBDTIME          TIME IN TU'S                                 
         SRDL  RE,32                                                            
         D     RE,TU1SEC           MVS TU'S TO SECS (1/38000)                   
         XR    RE,RE                                                            
         D     RE,=F'60'           REMAINDER TO GET MIN                         
         ST    RF,FULL                                                          
         STH   RE,HALF                                                          
         EDIT  HALF,(2,SCDSEC),ZERO=NOBLANK,FLOAT=0                             
*                                                                               
         L     RE,FULL                                                          
         SRDL  RE,32                                                            
         D     RE,=F'60'           REMAINDER TO GET HOURS                       
         ST    RF,FULL                                                          
         STH   RE,HALF                                                          
         EDIT  HALF,(2,SCDMIN),ZERO=NOBLANK,FLOAT=0                             
         EDIT  FULL,(2,SCDHRS),ZERO=BLANK                                       
*                                                                               
         MVC   SCDTYPE,TBDTYPE     SET TYPE                                     
         CLI   TBDTYPE,C'1'                                                     
         BE    FSC04                                                            
         CLI   TBDTYPE,C'S'                                                     
         BE    FSC04                                                            
         CLI   TBDTYPE,C'R'                                                     
         BE    FSC04                                                            
         CLI   TBDTYPE,C'W'                                                     
         BE    FSC04                                                            
         CLI   TBDTYPE,C'*'        TYPE * INFO                                  
         BE    FSC18                                                            
         B     FSC20               NO INFO                                      
*                                                                               
FSC04    OC    TBDATCB,TBDATCB       R5 = A(TCB)                                
         BZ    FSC20                                                            
*                                                                               
         L     RF,ATCB             LENGTH OF A TCB ENTRY                        
         MVC   LEN,0(RF)                                                        
         MVC   FROM,TBDATCB        A(TCB)                                       
         BRAS  RE,RDADR                                                         
         BNE   FSC20                                                            
*                                                                               
         L     R5,ATIA             MAKE SURE WE REALLY HAVE A TCB               
         USING TCBD,R5                                                          
         CLC   =C'*TASK',TCBID                                                  
         BNE   FSC20                                                            
*                                                                               
         MVI   SCDTID,C'T'         TASK ID                                      
         MVC   SCDTID+1(1),TBDTASK                                              
*                                                                               
         MVC   FULL(3),TCBWRKA+1                                                
         GOTO1 AHEXOUT,DMCB,FULL,SCDAWRK,3                                      
         MVC   FULL(3),TCBTWA+1                                                 
         GOTO1 (RF),(R1),,SCDATWA,3                                             
         MVC   FULL(3),TCBPGMA+1                                                
         GOTO1 (RF),(R1),,SCDAPGM,3                                             
         DROP  R5                                                               
*                                                                               
         MVC   SCDLINE,DOTFILL     LINE                                         
         MVC   SCDTRM,DOTFILL      TERM                                         
         CLI   TBDTSYM,X'40'       IS THERE A LINE ADDRESS?                     
         BNH   *+10                NO, SKIP                                     
         MVC   SCDLINE(4),TBDTSYM                                               
         CLI   TBDTSYM+4,X'40'     IS THERE A TERM                              
         BNH   *+10                NO, SKIP                                     
         MVC   SCDTRM,TBDTSYM+4                                                 
*                                                                               
         MVC   SCDSYSPR,DOTFILL    DISPLAY SYSTEM/PROGRAM                       
         CLI   TBDTSYS,0                                                        
         BE    FSC10                                                            
         MVC   SCDSYSPR,SPACES                                                  
*                                                                               
         L     R5,ASELIST          POINT TO START OF SELIST                     
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         AHI   R5,6                                                             
         USING SELISTD,R5                                                       
         CLC   SESYS,TBDTSYS       IS THIS THE SYSTEM ?                         
         BE    FSC06               YES - SKIP                                   
         BXLE  R5,RE,*-10          LOOP TO END OF SELIST                        
         GOTO1 AHEXOUT,DMCB,TBDTSYS,SCDSYSPR,1                                  
         MVI   SCDSYSPR+2,C'/'                                                  
         GOTO1 (RF),(R1),TBDTPRG,SCDSYSPR+3,1                                   
         B     FSC10                                                            
*                                                                               
FSC06    MVC   SCDSYSPR(L'SENAME),SENAME                                        
         LA    RF,SCDSYSPR+L'SENAME-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         AHI   RF,2                                                             
         ST    RF,FULL                                                          
*                                                                               
         L     R5,SEPGMS           PROGRAM NAME LIST FOR THIS SYSTEM            
         USING PGMLSTD,R5                                                       
         LH    RE,0(,R5)                                                        
         L     RF,2(,R5)                                                        
         AHI   R5,6                                                             
         CLC   PGMNUM,TBDTPRG                                                   
         BE    FSC08                                                            
         BXLE  R5,RE,*-10                                                       
         L     RF,FULL                                                          
         GOTO1 AHEXOUT,DMCB,TBDTPRG,(RF),1                                      
         B     FSC10                                                            
*                                                                               
FSC08    L     RF,FULL                                                          
         MVC   0(L'PGMNAME,RF),PGMNAME                                          
         DROP  R5                                                               
*                                                                               
FSC10    MVC   SCDUSER,DOTFILL     DISPLAY USERID IF SET                        
         OC    TBDUSER,TBDUSER                                                  
         BZ    FSC20                                                            
*                                                                               
         L     R5,AIO              READ USERID                                  
         USING CTIREC,R5                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TBDUSER                                                  
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                               
         TM    8(R1),X'10'         RECORD NOT FOUND?                            
         BO    FSC20                                                            
*                                                                               
         LA    R5,CTIDATA          GET NAME                                     
         USING CTDSCD,R5                                                        
         XR    RF,RF                                                            
FSC12    CLI   CTDSCEL,0                                                        
         BE    FSC14                                                            
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    FSC16                                                            
         ICM   RF,1,CTDSCLEN                                                    
         BZ    FSC14                                                            
         BXH   R5,RF,FSC12                                                      
*                                                                               
FSC14    DC    H'0'                ID RECORD IS CORRUPTED SOMEHOW               
*                                                                               
FSC16    MVC   SCDUSER,CTDSC                                                    
         B     FSC20                                                            
         DROP  R5                                                               
*                                                                               
FSC18    OC    TBDATCB,TBDATCB     BASE REGISTER SET?                           
         BZ    FSC20               NO                                           
*                                                                               
         MVC   LEN,=AL2(64)                                                     
         MVC   FROM,TBDATCB        A(BASE REGISTER)                             
         BRAS  RE,RDADR                                                         
         BNE   FSC20                                                            
*                                                                               
         L     R5,ATIA             BASE REG                                     
         MVC   SCDSYSPR(8),22(R5)  NAME OF MODULE                               
         CLC   =X'90ECD00C0DB0A7BAFFFA47FB',0(R5)                               
         BE    FSC19                                                            
         CLC   =X'90ECD00C0DB0A7BAFFFAA7F4',0(R5) WITH JUMP, NOT BRANCH         
         BE    FSC19                                                            
         MVC   SCDSYSPR(8),20(R5)  NAME OF MODULE                               
*                                                                               
FSC19    DS    0H                                                               
         MVI   SCDSYSPR+9,C'+'                                                  
         L     RF,TBDPSW           PSW ADDRESS AT POP                           
         S     RF,TBDATCB                                                       
         BNP   FSC20                                                            
         STCM  RF,7,FULL                                                        
         GOTO1 AHEXOUT,DMCB,FULL,SCDSYSPR+10,3                                  
*                                                                               
FSC20    XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF               NEXT SCREEN LINE                             
         AHI   R4,TBDLENQ          NEXT TABLE ENTRY                             
         BCT   R3,FSC02                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GOTO PARAMETER LIST (FROM R1) ADDRESS                               *         
***********************************************************************         
PGOTO    NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYAPGO      PGOTO                                        
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
         LHI   RF,2                DUMP REGISTER 2                              
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         LA    RF,SETREGS(RF)                                                   
         MVC   START,0(RF)                                                      
         ICM   RE,15,START                                                      
         A     RE,DISPLACE                                                      
         ST    RE,FROM                                                          
         MVC   LEN,=AL2(64)                                                     
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   PGO02                                                            
         MVI   FHIL,1                                                           
         MVI   FHDA,C'1'                                                        
         MVI   FHII,FHIIHE                                                      
*                                                                               
PGO02    BRAS  RE,MOVHEX           RANGE BETWEEN 1 AND 8                        
         BNE   EXITL                                                            
         ICM   RF,15,FULL                                                       
         BNZ   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
         CHI   RF,8                                                             
         BNH   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,ATIA                                                          
         MVC   START,0(RF)                                                      
         MVI   XREGS,C'N'                                                       
         CLC   =C'PXGOTO',KEYWORD                                               
         BNE   *+12                                                             
         MVI   XREGS,C'Y'                                                       
         B     *+8                                                              
         MVI   START,0                                                          
*                                                                               
         XC    DUMPREG,DUMPREG                                                  
         XC    DISPLACE,DISPLACE                                                
         B     EXITH               CONTINUE PROCESSING                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GOTO ADDRESS IN LIST                                                *         
***********************************************************************         
GOTO     NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYAGO       GOTO                                         
         BNE   EXITH               NOT PROCESSED                                
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         CLI   NDLSTCNT,0                                                       
         BNE   *+12                                                             
         MVI   FERN,26             EMPTY LIST                                   
         B     EXITL                                                            
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   GO02                                                             
         MVI   FHIL,1                                                           
         MVI   FHDA,C'1'                                                        
         MVI   FHII,FHIINU                                                      
*                                                                               
GO02     TM    FHII,FHIINU         NUMBER?                                      
         BO    *+12                                                             
         MVI   FERN,6                                                           
         B     EXITL                                                            
*                                                                               
         BRAS  RE,MOVDEC           RANGE BETWEEN 1 AND 24                       
         BNE   EXITL                                                            
         ICM   RF,15,FULL                                                       
         BNZ   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
         CHI   RF,24                                                            
         BNH   *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         XR    R0,R0                                                            
         IC    R0,NDLSTCNT                                                      
         CR    RF,R0                                                            
         BNH   *+12                                                             
         MVI   FERN,27                                                          
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         MHI   RF,NDLDLQ           INDEX INTO NUMBER                            
         LA    RF,NDLIST(RF)                                                    
*                                                                               
         MVC   START,NDLADDR-NDLD(RF)                                           
         MVI   XREGS,C'N'                                                       
         CLI   START,0                                                          
         BE    *+8                                                              
         MVI   XREGS,C'Y'                                                       
*                                                                               
         XC    DUMPREG,DUMPREG                                                  
         XC    DISPLACE,DISPLACE                                                
         MVI   FERN,0                                                           
         B     EXITH               CONTINUE PROCESSING                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET INDIRECT ADDRESS                                                *         
***********************************************************************         
INDIRECT NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYAIND      INDIRECT                                     
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
         XR    RF,RF                                                            
         IC    RF,DUMPREG          GET DUMP REGISTER IF SET                     
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         LA    RF,SETREGS(RF)                                                   
         L     R0,START            ADJUST START POINT FOR REGISTER              
         A     R0,0(RF)                                                         
         ST    R0,START                                                         
*                                                                               
         ICM   RE,15,START                                                      
         A     RE,DISPLACE                                                      
         ST    RE,FROM                                                          
         MVC   LEN,=AL2(64)                                                     
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     RF,ATIA                                                          
         MVC   START,0(RF)                                                      
         CLI   XREGS,C'Y'                                                       
         B     *+8                                                              
         MVI   START,0                                                          
*                                                                               
         XC    DUMPREG,DUMPREG                                                  
         XC    XREGS,XREGS                                                      
         XC    DISPLACE,DISPLACE                                                
         B     EXITH               CONTINUE PROCESSING                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SET REGISTERS BY FOLLOWING THE D-CHAIN                              *         
***********************************************************************         
SRXREGS  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYASRX      SET REGISTERS                                
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         TM    NDFLAG,NDFSROK                                                   
         BO    *+12                                                             
         MVI   FERN,21                                                          
         B     EXITL                                                            
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,1              MISSING INPUT FIELD                          
         B     EXITL                                                            
*                                                                               
         CLI   FHIL,1              CLEARING REGISTER SAVE?                      
         BNE   SREG02              NO                                           
         CLI   FHDA,C'C'                                                        
         BNE   SREG02                                                           
         NI    NDFLAG,255-NDFSRSET                                              
         MVC   SETREGS+00(36),HDR.DMPR0                                         
         MVC   SETREGS+36(28),HDR.DMPR9                                         
*                                                                               
         MVC   SRVP2,SPACES        FUDGE INPUT FIELDS                           
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3,C'B'                                                       
         MVI   DUMPREG,X'0C'       SET REGISTER B DISPLAY                       
         MVI   XREGS,C'N'                                                       
         XC    DISPLACE,DISPLACE                                                
         XC    AKEYWORD,AKEYWORD                                                
         LA    R2,SRVP2H           SET CURSOR ON P2                             
         ST    R2,FADRH                                                         
         B     EXITH               CONTINUE PROCESSING                          
*                                                                               
SREG02   MVI   BYTE,C'+'                                                        
         TM    FHII,FHIINU         NUMERIC?                                     
         BO    SREG04                                                           
         CLI   FHDA,C'-'                                                        
         BE    *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         MVI   BYTE,C'-'           SET NEGATIVE INPUT                           
         MVI   FHDA,C'0'           OVERLAY THE '-' FOR VALIDATION               
*                                                                               
SREG04   BRAS  RE,MOVDEC                                                        
         IPM   R0                                                               
         CLI   BYTE,C'-'           SET NEGATIVE INPUT                           
         BNE   *+8                                                              
         MVI   FHDA,C'-'                                                        
         SPM   R0                                                               
         BNE   EXITL               BAD INPUT FIELD                              
*                                                                               
         XR    R0,R0                                                            
         LA    R4,NDSVREG                                                       
SREG06   CLC   HDR.DMPRD,56(R4)                                                 
         BE    SREG08                                                           
         AHI   R4,L'NDSVREG                                                     
         AHI   R0,1                                                             
         CHI   R0,NDSVRCNT                                                      
         BNH   SREG06                                                           
         MVI   FERN,21                                                          
         B     EXITL                                                            
*                                                                               
SREG08   L     RF,FULL                                                          
         CLI   BYTE,C'-'                                                        
         BE    SREG10                                                           
*                                                                               
         AHI   RF,1                SR 0 MEANS GO BACK 1 SET                     
         SR    R0,RF                                                            
         BNM   *+12                                                             
         MVI   FERN,22             SETTING OUT OF RANGE                         
         B     EXITL                                                            
*                                                                               
         LA    R4,NDSVREG                                                       
         LTR   R0,R0                                                            
         BZ    SREG20                                                           
         AHI   R4,L'NDSVREG                                                     
         BCT   R0,*-4                                                           
         B     SREG20                                                           
*                                                                               
SREG10   LTR   RF,RF               CHECK FOR SR(X) -0 BUG                       
         BNZ   *+12                                                             
         MVI   FERN,33                                                          
         B     EXITL                                                            
*                                                                               
         AR    R0,RF                                                            
         CHI   R0,NDSVRCNT                                                      
         BL    *+12                                                             
         MVI   FERN,22             SETTING OUT OF RANGE                         
         B     EXITL                                                            
*                                                                               
         AHI   R4,L'NDSVREG                                                     
         BCT   RF,*-4                                                           
         OC    0(L'NDSVREG,R4),0(R4)                                            
         BNZ   *+12                                                             
         MVI   FERN,22             SETTING OUT OF RANGE                         
         B     EXITL                                                            
*                                                                               
SREG20   MVC   SETREGS,4(R4)                                                    
         OI    NDFLAG,NDFSRSET                                                  
         XC    AKEYWORD,AKEYWORD                                                
*                                                                               
         MVC   SRVP2,SPACES        FUDGE INPUT FIELDS                           
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3,C'B'                                                       
         MVI   DUMPREG,X'0C'       SET REGISTER B DISPLAY                       
         MVI   XREGS,C'N'                                                       
         XC    DISPLACE,DISPLACE                                                
         LA    R2,SRVP2H           SET CURSOR ON P2                             
         ST    R2,FADRH                                                         
         B     EXITH                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SHOW RD CHAIN                                                       *         
***********************************************************************         
SXREGS   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYASX       SHOW RD CHAIN                                
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         TM    NDFLAG,NDFSROK                                                   
         BO    *+12                                                             
         MVI   FERN,21                                                          
         B     EXITL                                                            
*                                                                               
         OI    DMPFLGS1,DMPSX                                                   
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   SX02                                                             
         MVI   FHDA,C'0'           DEFAULT IS X(0)                              
         MVI   FHIL,1                                                           
         MVI   FHII,FHIINU                                                      
*                                                                               
SX02     MVI   BYTE,C'+'                                                        
         TM    FHII,FHIINU         NUMERIC?                                     
         BO    SX04                                                             
         CLI   FHDA,C'-'                                                        
         BE    *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         MVI   BYTE,C'-'           SET NEGATIVE INPUT                           
         MVI   FHDA,C'0'           OVERLAY THE '-' FOR VALIDATION               
*                                                                               
SX04     BRAS  RE,MOVDEC                                                        
         IPM   R0                                                               
         CLI   BYTE,C'-'           SET NEGATIVE INPUT                           
         BNE   *+8                                                              
         MVI   FHDA,C'-'                                                        
         SPM   R0                                                               
         BNE   EXITL               BAD INPUT FIELD                              
*                                                                               
         XR    R0,R0                                                            
         LA    R4,NDSVREG                                                       
SX06     CLC   HDR.DMPRD,56(R4)                                                 
         BE    SX08                                                             
         AHI   R4,L'NDSVREG                                                     
         AHI   R0,1                                                             
         CHI   R0,NDSVRCNT                                                      
         BNH   SX06                                                             
         MVI   FERN,21                                                          
         B     EXITL               SOMETHING IS BADLY WRONG                     
*                                                                               
SX08     L     RF,FULL                                                          
         CLI   BYTE,C'-'                                                        
         BE    SX10                                                             
*                                                                               
         SR    R0,RF                                                            
         BNM   *+12                                                             
         MVI   FERN,22             SETTING OUT OF RANGE                         
         B     EXITL                                                            
*                                                                               
         LA    R4,NDSVREG                                                       
         LTR   R0,R0                                                            
         BZ    SX12                                                             
         AHI   R4,L'NDSVREG                                                     
         BCT   R0,*-4                                                           
         B     SX12                                                             
*                                                                               
SX10     AR    R0,RF                                                            
         CHI   R0,NDSVRCNT                                                      
         BL    *+12                                                             
         MVI   FERN,22             SETTING OUT OF RANGE                         
         B     EXITL                                                            
*                                                                               
         LTR   RF,RF                                                            
         BNZ   *+12                                                             
         MVI   FERN,31                                                          
         B     EXITL                                                            
*                                                                               
         AHI   R4,L'NDSVREG                                                     
         BCT   RF,*-4                                                           
         OC    0(L'NDSVREG,R4),0(R4)                                            
         BNZ   *+12                                                             
         MVI   FERN,22             SETTING OUT OF RANGE                         
         B     EXITL                                                            
*                                                                               
SX12     MVC   START,56(R4)                                                     
         MVI   START,0                                                          
         B     EXITH                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SHOW RD CHAIN                                                       *         
***********************************************************************         
RDCHAIN  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYACHN      SHOW RD CHAIN                                
         BNE   EXITH               NOT PROCESSED                                
*                                                                               
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         TM    NDFLAG,NDFSROK                                                   
         BO    *+12                                                             
         MVI   FERN,21                                                          
         B     EXITL                                                            
*                                                                               
         MVI   HDRN,23                                                          
         MVI   FERN,0                                                           
*                                                                               
         LA    R2,SRVP3H           VALIDATE P3                                  
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         CLI   FHIL,0                                                           
         BNE   RDC02                                                            
         MVI   FHDA,C'0'           DEFAULT IS X(0)                              
         MVI   FHIL,1                                                           
         MVI   FHII,FHIINU                                                      
*                                                                               
RDC02    BRAS  RE,MOVDEC                                                        
         BNE   EXITL               BAD INPUT FIELD                              
*                                                                               
         XR    R0,R0                                                            
         LA    R4,NDSVREG                                                       
RDC04    OC    0(L'NDSVREG,R4),0(R4)                                            
         BZ    RDC06                                                            
         AHI   R4,L'NDSVREG                                                     
         AHI   R0,1                                                             
         CHI   R0,NDSVRCNT                                                      
         BNH   RDC04                                                            
         MVI   FERN,21                                                          
         B     EXITL               SOMETHING IS BADLY WRONG                     
*                                                                               
RDC06    LA    R4,NDSVREG                                                       
         L     RF,FULL                                                          
         CR    R0,RF                                                            
         BH    *+12                                                             
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
         LTR   RF,RF               GO TO START OF DISPLAY                       
         BZ    *+12                                                             
         AHI   R4,L'NDSVREG                                                     
         BCT   RF,*-4                                                           
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),255           ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         LA    R2,DMPL1H           R2 = A(FIRST DISPLAY LINE ON SCREEN)         
         USING FHD,R2                                                           
         OI    FHAT,FHATHI                                                      
         USING RDLINED,FHDA                                                     
         MVC   RDLLBL(5),=C'Label'                                              
         MVC   RDLEYE(8),=C'Eyecatch'                                           
         MVC   RDLR1(2),=C'R1 '                                                 
         MVC   RDLRB(2),=C'RB '                                                 
         MVC   RDLRD(2),=C'RD '                                                 
         MVC   RDLRE(2),=C'RE '                                                 
         MVC   RDLRF(2),=C'RF'                                                  
         MVC   RDLRERB(5),=C'RE-RB'                                             
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
*                                                                               
RDC08    OC    0(L'NDSVREG,R4),0(R4)                                            
         BZ    RDC14                                                            
         MVC   RDLLBL,0(R4)                                                     
         CLC   =C'STAR',RDLLBL     STORE ACCESS REGISTER                        
         BE    RDC12                                                            
*                                                                               
         MVC   FROM,48(R4)         THESE ADDRESSES ARE ALL 24 BIT               
         MVI   FROM,0                                                           
         CLC   FROM,HDR.DMPPART    MAKE SURE IT IS VALID                        
         BNH   RDC10                                                            
         MVC   LEN,=AL2(64)                                                     
         BRAS  RE,RDADR                                                         
         BNE   RDC12                                                            
*                                                                               
         L     RF,ATIA             EYECATCHER FROM RB LOCATION                  
         MVC   RDLEYE,22(RF)                                                    
         CLC   =X'90ECD00C0DB0A7BAFFFA47FB',0(RF)                               
         BE    RDC10                                                            
         CLC   =X'90ECD00C0DB0A7BAFFFAA7F4',0(RF) WITH JUMP, NOT BRANCH         
         BE    RDC10                                                            
         MVC   RDLEYE,20(RF)                                                    
*                                                                               
RDC10    MVI   FERN,0                                                           
         MVC   FULL,08(R4)                                                      
         OC    FULL,FULL                                                        
         BZ    RDC10A                                                           
         GOTO1 AHEXOUT,DMCB,FULL,RDLR1,4                                        
RDC10A   MVC   FULL,48(R4)                                                      
         OC    FULL,FULL                                                        
         BZ    RDC10B                                                           
         GOTO1 AHEXOUT,DMCB,FULL,RDLRB,4                                        
RDC10B   MVC   FULL,60(R4)                                                      
         OC    FULL,FULL                                                        
         BZ    RDC10C                                                           
         GOTO1 AHEXOUT,DMCB,FULL,RDLRE,4                                        
RDC10C   MVC   FULL,64(R4)                                                      
         OC    FULL,FULL                                                        
         BZ    RDC10D                                                           
         GOTO1 AHEXOUT,DMCB,FULL,RDLRF,4                                        
*                                                                               
RDC10D   L     RF,60(R4)                                                        
         LA    RF,0(RF)                                                         
         L     RE,48(R4)                                                        
         LA    RE,0(RE)                                                         
         SR    RF,RE                                                            
         ST    RF,FULL                                                          
         OC    FULL,FULL                                                        
         BZ    RDC12                                                            
         GOTO1 AHEXOUT,DMCB,FULL+1,RDLRERB,3                                    
*                                                                               
RDC12    MVC   FULL,56(R4)                                                      
         GOTO1 AHEXOUT,DMCB,FULL+1,RDLRD,3                                      
*                                                                               
         CLC   HDR.DMPRD,56(R4)    HIGHLIGHT CURRENT RD                         
         BNE   *+8                                                              
         OI    FHAT,FHATHI                                                      
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FHLN                                                        
         BZ    RDC14                                                            
         AHI   R4,L'NDSVREG                                                     
         BXH   R2,RF,RDC08                                                      
*                                                                               
RDC14    CLI   PFKEY,2             SELECT PFKEY PRESSED?                        
         BNE   EXITL               NO - FORCE EXIT                              
*                                                                               
         LA    R2,DMPL2H           FIND FIELD FOR CURSOR                        
         CLC   SCURSOR,FHAD                                                     
         BL    RDC24                                                            
         LA    R2,DMPLLH                                                        
         CLC   SCURSOR,FHAD                                                     
         BNL   RDC24                                                            
*                                                                               
         LA    R2,DMPL2H           SET A(FIRST FIELD)                           
         ST    R2,FULL                                                          
         LA    RF,DMPLLH                                                        
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
*                                                                               
RDC16    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    RDC18                                                            
         ST    R2,FULL                                                          
         IC    RE,FHLN                                                          
         BXLE  R2,RE,RDC16                                                      
         DC    H'0'                                                             
*                                                                               
RDC18    L     R2,FULL             CURSOR FIELD IS IN FULL                      
         LH    R0,SCURSOR                                                       
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         SR    R0,RF               R0 = DISPLACEMENT INTO FIELD                 
*                                                                               
         XC    DUB,DUB                                                          
         CHI   R0,RDLRD-RDLINED    WANT RD?                                     
         BL    RDC24                                                            
         CHI   R0,RDLRD+L'RDLRD-RDLINED                                         
         BH    *+14                                                             
         MVC   DUB(6),RDLRD                                                     
         B     RDC20                                                            
*                                                                               
         CHI   R0,RDLRB-RDLINED    WANT RB?                                     
         BL    RDC24                                                            
         CHI   R0,RDLRB+L'RDLRB-RDLINED                                         
         BH    *+14                                                             
         MVC   DUB(6),RDLRB+2                                                   
         B     RDC20                                                            
*                                                                               
         CHI   R0,RDLRE-RDLINED    WANT RE?                                     
         BL    RDC24                                                            
         CHI   R0,RDLRE+L'RDLRE-RDLINED                                         
         BH    *+14                                                             
         MVC   DUB(6),RDLRE+2                                                   
         B     RDC20                                                            
*                                                                               
         CHI   R0,RDLRF-RDLINED    WANT RF?                                     
         BL    RDC24                                                            
         CHI   R0,RDLRF+L'RDLRF-RDLINED                                         
         BH    *+14                                                             
         MVC   DUB(6),RDLRF+2                                                   
         B     RDC20                                                            
*                                                                               
         CHI   R0,RDLR1-RDLINED    WANT R1?                                     
         BL    RDC24                                                            
         CHI   R0,RDLR1+L'RDLR1-RDLINED                                         
         BH    RDC24                                                            
         MVC   DUB(6),RDLR1+2                                                   
*                                                                               
RDC20    CLI   DUB,C'A'            ASSUME IF THIS HEX ALL ARE                   
         BL    RDC24                                                            
         CLI   DUB,C'F'                                                         
         BNH   RDC22                                                            
         CLI   DUB,C'0'                                                         
         BL    RDC24                                                            
         CLI   DUB,C'9'                                                         
         BH    RDC24                                                            
*                                                                               
RDC22    MVC   SRVP2,SPACES                                                     
         MVC   SRVP2(6),DUB                                                     
         MVI   SRVP2H+(FHIL-FHD),6                                              
         MVI   SRVP2H+(FHOL-FHD),6                                              
         MVI   SRVP2H+(FHII-FHD),FHIIHE                                         
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3H+(FHIL-FHD),0                                              
         MVI   SRVP3H+(FHOL-FHD),0                                              
         MVC   SRVP4,SPACES                                                     
         MVI   SRVP4H+(FHIL-FHD),0                                              
         MVI   SRVP4H+(FHOL-FHD),0                                              
         B     EXITOK              SHOW SELECT DONE                             
*                                                                               
RDC24    LA    R2,SRVMSGH          INVALID CURSOR POSITION                      
         USING FHD,R2                                                           
         LR    R0,R2                                                            
         XR    RF,RF                                                            
RDC26    CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    RDC28                                                            
         LR    R0,R2                                                            
         ICM   RF,1,FHLN                                                        
         BZ    *+8                                                              
         BXH   R2,RF,RDC26                                                      
         DC    H'0'                                                             
*                                                                               
RDC28    LR    R2,R0               SET FIELD CONTAINING CURSOR                  
         ST    R2,FADRH                                                         
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    R0,SCURSOR                                                       
         SR    R0,RF                                                            
         STC   R0,FERRDSP          SET DISP TO CURSOR                           
         MVI   FERN,8                                                           
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK INPUT TO SCREEN FIELDS                             *         
***********************************************************************         
CHKFLDS  NTR1  BASE=*,LABEL=*                                                   
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         TM    NDFLAG,NDFOKCHK     OK TO CHECK?                                 
         BZ    EXITOK              NO                                           
*                                                                               
         LA    R2,SRVL1H                                                        
         USING FHD,R2                                                           
         LA    R4,NDMPCOLS         LIST OF DATA IN EACH COLUMN                  
*                                                                               
CHKF02   CLI   FHLN,0              SCREEN END?                                  
         BE    EXITOK                                                           
         TM    FHAT,FHATPR         PROTECTED?                                   
         BO    CHKF10              YES                                          
         CLI   FHIL,0              INPUT?                                       
         BE    CHKF08              NO                                           
         CLI   FHDA,C' '           DOUBLE CHECK INPUT TO FIELD                  
         BNH   CHKF08                                                           
*                                                                               
         CLI   FHDA,C'H'           HELP?                                        
         BNE   *+12                                                             
         BRAS  RE,SELHLP                                                        
         B     EXITL               MAKE SURE WE EXIT OUT OF PROGRAM             
*                                                                               
         LA    R1,SELCMDS          TABLE OF SELECT COMMANDS                     
         USING SELCMDSD,R1                                                      
CHKF04   CLI   SELCMD,EOTB                                                      
         BNE   CHKF06                                                           
         ST    R2,FADRH            UNKNOWN COMMAND                              
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
CHKF06   CLC   SELCMD,FHDA         MATCH COMMAND?                               
         BE    *+12                                                             
         AHI   R1,SELCMDSL                                                      
         B     CHKF04                                                           
*                                                                               
         ICM   RF,15,SELRTN        GO TO PROCESSING ROUTINE                     
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BH    EXITH               NEED TO REPROCESS PARAMETER FIELDS           
         BL    EXITL               ERROR EXIT                                   
*                                                                               
CHKF08   AHI   R4,4                NEXT DISPLAY COLUMN                          
*                                                                               
CHKF10   XR    RF,RF               NEXT SCREEN FIELD                            
         ICM   RF,1,FHLN                                                        
         BXH   R2,RF,CHKF02                                                     
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SELECT FIELD HELP DISPLAY                                           *         
***********************************************************************         
SELHLP   NTR1  ,                                                                
         MVI   HDRN,16                                                          
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         LA    R2,DMPL1H           POINT TO FIRST DATA FIELD                    
         USING FHD,R2                                                           
         LA    R1,SELCMDS                                                       
         USING SELCMDSD,R1                                                      
         XR    RF,RF                                                            
         MVC   FHDA+00(3),=CL3'Cmd'                                             
         MVC   FHDA+04(6),=CL6'Action'                                          
         OI    FHAT,FHATHI                                                      
         B     SELH04                                                           
*                                                                               
SELH02   CLI   SELCMD,EOTB         JUST GET OUT                                 
         BE    EXITL                                                            
         MVC   FHDA+00(L'SELCMD),SELCMD                                         
         MVC   FHDA+04(L'SELHELP),SELHELP                                       
         AHI   R1,SELCMDSL                                                      
*                                                                               
SELH04   ICM   RF,1,FHLN           MIGHT NEED 2 PAGES IN FUTURE                 
         BZ    EXITL               I MAY BE RETIRED BY THEN (AATK2002)          
         BXH   R2,RF,SELH02                                                     
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ADD THE ADDRESS USER SELECTED IN THE DUMP                           *         
* NTRY   R2    = A(SELECT FIELD)                                      *         
*        R3    = A(NDSAVE BLOCK)                                      *         
*        R4    = A(DISPLAY VALUE)                                     *         
***********************************************************************         
         USING FHD,R2                                                           
         USING NDSAVED,R3                                                       
SELDOA   NTR1  ,                                                                
         XR    R0,R0                                                            
         ICM   R0,7,1(R4)          GET ADDRESS VALUE                            
         CLI   FHDA,C'C'                                                        
         BNE   *+8                                                              
         ICM   R0,15,0(R4)         GET XA ADDRESS VALUE                         
         CLI   FHDA,C'S'                                                        
         BNE   *+8                                                              
         L     R0,START            GET IMMEDIATE AND SAVE                       
*                                                                               
         XR    R1,R1                                                            
         IC    R1,NDLSTCNT         UPDATE LIST COUNT                            
         AHI   R1,1                                                             
         STC   R1,NDLSTCNT                                                      
         BCTR  R1,0                                                             
         MHI   R1,NDLDLQ                                                        
         LA    R1,NDLIST(R1)       INDEX INTO LIST                              
         STCM  R0,15,0(R1)         SAVE VALUE                                   
*                                                                               
         MVI   FHDA,C' '                                                        
         MVI   FHIL,0                                                           
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ADD THE ADDRESS USER SELECTED IN THE DUMP AND THEN GOTO IT          *         
* NTRY   R2    = A(SELECT FIELD)                                      *         
*        R3    = A(NDSAVE BLOCK)                                      *         
*        R4    = A(DISPLAY VALUE)                                     *         
***********************************************************************         
         USING FHD,R2                                                           
         USING NDSAVED,R3                                                       
SELDOB   NTR1  ,                                                                
         XR    R0,R0                                                            
         ICM   R0,7,1(R4)          GET ADDRESS VALUE                            
         CLI   FHDA,C'D'                                                        
         BNE   *+8                                                              
         ICM   R0,15,0(R4)         GET XA ADDRESS VALUE                         
*                                                                               
         XR    R1,R1                                                            
         IC    R1,NDLSTCNT         UPDATE LIST COUNT                            
         AHI   R1,1                                                             
         STC   R1,NDLSTCNT                                                      
         BCTR  R1,0                                                             
         MHI   R1,NDLDLQ                                                        
         LA    R1,NDLIST(R1)       INDEX INTO LIST                              
         ST    R0,0(R1)            SAVE VALUE                                   
*                                                                               
         MVI   FHDA,C' '                                                        
         MVI   FHIL,0                                                           
         OI    FHOI,FHOITR                                                      
         ST    R0,FULL                                                          
*                                                                               
         MVC   SRVP2,SPACES                                                     
         MVI   SRVP2H+(FHIL-FHD),8                                              
         MVI   SRVP2H+(FHII-FHD),FHIIHE                                         
         GOTO1 AHEXOUT,DMCB,FULL,SRVP2,4                                        
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3H+(FHIL-FHD),0                                              
         MVC   SRVP4,SPACES                                                     
         MVI   SRVP4H+(FHIL-FHD),0                                              
*                                                                               
         BRAS  RE,WTTWAB           WRITE TWA OFF                                
         B     EXITH                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GOTO THE ADDRESS USER SELECTED IN THE DUMP                          *         
* NTRY   R2    = A(SELECT FIELD)                                      *         
*        R3    = A(NDSAVE BLOCK)                                      *         
*        R4    = A(DISPLAY VALUE)                                     *         
***********************************************************************         
         USING FHD,R2                                                           
         USING NDSAVED,R3                                                       
SELDOG   NTR1  ,                                                                
         XR    R0,R0                                                            
         ICM   R0,7,1(R4)          GET ADDRESS VALUE                            
*NOP     CLI   FHDA,C'X'                                                        
*NOP     BNE   *+8                                                              
         ICM   R0,15,0(R4)         GET XA ADDRESS VALUE                         
         ST    R0,FULL                                                          
*                                                                               
         MVI   FHDA,C' '                                                        
         MVI   FHIL,0                                                           
         OI    FHOI,FHOITR                                                      
         MVC   SRVP2,SPACES                                                     
         MVI   SRVP2H+(FHIL-FHD),8                                              
         MVI   SRVP2H+(FHII-FHD),(FHIITH+FHIIHE)                                
         GOTO1 AHEXOUT,DMCB,FULL,SRVP2,4                                        
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3H+(FHIL-FHD),0                                              
         MVC   SRVP4,SPACES                                                     
         MVI   SRVP4H+(FHIL-FHD),0                                              
         BRAS  RE,WTTWAB           WRITE TWA OFF                                
         B     EXITH                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* USE ADDRESS SELECTED IN THE DUMP AS BEGINNING OF PARAMETER LIST     *         
* NTRY   R2    = A(SELECT FIELD)                                      *         
*        R3    = A(NDSAVE BLOCK)                                      *         
*        R4    = A(DISPLAY VALUE)                                     *         
***********************************************************************         
         USING FHD,R2                                                           
         USING NDSAVED,R3                                                       
SELDOP   NTR1  ,                                                                
         XR    R0,R0                                                            
         ICM   R0,7,1(R4)          GET ADDRESS VALUE                            
         STCM  R0,7,FULL                                                        
*                                                                               
         MVI   FHIL,0                                                           
         MVI   FHDA,C' '                                                        
*                                                                               
         MVC   SRVP2,SPACES                                                     
         MVI   SRVP2,C'P'                                                       
         MVI   SRVP2H+(FHIL-FHD),1                                              
*                                                                               
         MVC   SRVP3,SPACES                                                     
         GOTO1 AHEXOUT,DMCB,FULL,SRVP3,3                                        
         MVI   SRVP3H+(FHIL-FHD),6                                              
         MVI   SRVP3H+(FHII-FHD),(FHIITH+FHIIHE)                                
         MVC   SRVP4,SPACES                                                     
         MVI   SRVP4H+(FHIL-FHD),0                                              
         BRAS  RE,WTTWAB           WRITE TWA OFF                                
         B     EXITH                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* HANDLE LIST SCREEN (NOT THE ALL SCREEN - STUPID)                    *         
***********************************************************************         
LIST     NTR1  BASE=*,LABEL=*                                                   
         L     R3,AKEYWORD                                                      
         USING KEYTABD,R3                                                       
         CLI   KEYACT,KEYALST      BUILD AND PROCESS LIST SCREEN                
         BNE   EXITH               NOT ME                                       
         DROP  R3                                                               
*                                                                               
         MVI   HDRN,19                                                          
         GOTO1 ACALLOV,DMCB,(X'FA',SRVTAGH),0                                   
         CLI   4(R1),255           ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         MVI   NDTSSCRN,X'FA'      SET THIS SCREEN TO X'FA'                     
*                                                                               
         BRAS  RE,DISLST           DISPLAY LIST SCREEN                          
*                                                                               
         CLI   NDLSSCRN,X'FA'      WAS LAST SCREEN X'FA'                        
         BNE   LIST02              NO                                           
*                                                                               
         CLI   DOMRGE,C'Y'         MERGED DATA FROM TBUFF?                      
         BNE   LIST02                                                           
         BRAS  RE,SCRMRGE          MERGE DATA INTO SCREEN                       
         MVI   DOMRGE,C'N'         JUST ONCE THOUGH                             
*                                                                               
LIST02   BRAS  RE,RDLST            REVALIDATE MERGED LIST SCREEN                
         BL    EXITL                                                            
*                                                                               
         MVI   FERN,0                                                           
         BRAS  RE,ACTLST           DO ANY REQUESTED ACTIONS                     
         BH    EXITOK              WANT TO GO SOMEWHERE                         
         MVI   FERN,0                                                           
         BRAS  RE,DISLST           REDISPLAY LIST SCREEN                        
         BNE   EXITL                                                            
         MVI   FERN,0                                                           
         LA    RF,LSTSLCTH         SET CURSOR                                   
         ST    RF,FADRH                                                         
         L     RF,AUTL             KEEP ALL INFO ON SCREEN                      
         OI    TSVCREQ-UTLD(RF),X'02'                                           
         B     EXITL               FORCE EXIT                                   
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST SCREEN                                                 *         
***********************************************************************         
DISLST   NTR1  ,                                                                
         L     R3,ASVBLOCK                                                      
         USING NDSAVED,R3                                                       
         LA    R2,LSTNUMBH         FIRST LINE FIRST ENTRY                       
         USING FHD,R2                                                           
         ST    R2,FADRH                                                         
*                                                                               
         XR    RF,RF                                                            
DSLS02   ICM   RF,1,FHLN                                                        
         BZ    DSLS04                                                           
         TM    FHAT,FHATPR                                                      
         BZ    *+8                                                              
         BXH   R2,RF,DSLS02                                                     
         OI    FHOI,FHOITR                                                      
         LHI   RE,FHDAD+1                                                       
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RE,FHDAD                                                         
         SR    RF,RE                                                            
         XC    FHDA(0),FHDA                                                     
         EX    RF,*-6                                                           
         AR    RF,RE                                                            
         BXH   R2,RF,DSLS02                                                     
*                                                                               
DSLS04   LA    R2,LSTNUMBH         FIRST LINE FIRST ENTRY                       
         XR    R0,R0                                                            
         ICM   R0,1,NDLSTCNT       ANY ENTRIES TO DISPLAY?                      
         BZ    EXITOK              NO                                           
*                                                                               
         LA    R4,NDLIST           DISPLAY ENTRIES THAT ALREADY EXIST           
         USING NDLD,R4                                                          
*                                                                               
DSLS06   XR    R1,R1                                                            
         IC    R1,FHLN                                                          
         AR    R2,R1               SELECT FIELD                                 
         IC    R1,FHLN                                                          
         AR    R2,R1               ADDRESS FIELD                                
*                                                                               
         CLC   NDLADDR,EFFS        NOT VALIDATED VALUE IN FIELD                 
         BNE   DSLS08                                                           
         IC    R1,NDLAFLDL                                                      
         BCTR  R1,0                                                             
         MVC   FHDA(0),NDLAFLD                                                  
         EX    R1,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,FHIL                                                          
         STC   RF,FHOL                                                          
         B     DSLS10                                                           
*                                                                               
DSLS08   GOTO1 AHEXOUT,DMCB,NDLADDR,DUB,4                                       
         BRAS  RE,STRIP0                                                        
         XR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         BZ    DSLS10                                                           
         BCTR  RF,0                                                             
         MVC   FHDA(0),DUB                                                      
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,FHIL                                                          
         STC   RF,FHOL                                                          
         MVI   FHII,FHIIHE         MAKE IT VALID HEX                            
*                                                                               
DSLS10   XR    R1,R1               OUTPUT COMMENT FIELD                         
         IC    R1,FHLN                                                          
         AR    R2,R1                                                            
         MVC   FHDA(L'NDLCFLD),NDLCFLD                                          
*                                                                               
         IC    R1,FHLN                                                          
         AR    R2,R1               NEXT LINE                                    
         AHI   R4,NDLDLQ           NEXT ADDRESS                                 
         BCT   R0,DSLS06                                                        
         B     EXITOK                                                           
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS LIST SCREEN FIELDS AFTER MERGE                              *         
***********************************************************************         
RDLST    NTR1  ,                                                                
         LA    R2,LSTNUMBH         FIRST LINE                                   
         ST    R2,FADRH                                                         
         USING FHD,R2                                                           
         L     R3,ASVBLOCK         SAVED BLOCK                                  
         USING NDSAVED,R3                                                       
         LA    R4,NDLIST           LIST BLOCK                                   
         USING NDLD,R4                                                          
         MVI   LVERR,C'N'                                                       
*                                                                               
         LR    R0,R4               CLEAR BLOCK                                  
         LHI   R1,NDLISTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
RDLS02   CLI   FHLN,0              END OF TWA?                                  
         BE    RDLS12              YES                                          
         BRAS  RE,RDLSNXF      *** SELECT FIELD                                 
*                                                                               
         CLI   FHIL,0              SELECT FIELD HAS INPUT?                      
         BE    RDLS04              NO                                           
         CLI   FHDA,C'D'           DELETE THIS ENTRY?                           
         BNE   RDLS04              NO                                           
         BRAS  RE,RDLSNXF          GO PAST ADDRESS FIELD                        
         BRAS  RE,RDLSNXF          GO PAST COMMENT FIELD                        
         BRAS  RE,RDLSNXF          GO PAST NEXT # FIELD                         
         B     RDLS02                                                           
*                                                                               
RDLS04   BRAS  RE,RDLSNXF      *** ADDRESS FIELD                                
         CLI   FHIL,0              ANY ADDRESS TO ADD?                          
         BNE   RDLS06                                                           
         BRAS  RE,RDLSNXF          NO - IGNORE REST OF LINE                     
         BRAS  RE,RDLSNXF                                                       
         B     RDLS02                                                           
*                                                                               
RDLS06   CLI   LVERR,C'Y'          PREVIOUS FIELD IN ERROR?                     
         BNE   *+10                                                             
         MVC   NDLADDR,EFFS                                                     
*                                                                               
         IC    R1,FHIL                                                          
         STC   R1,NDLAFLDL         SAVE L'INPUT FIELD                           
         BCTR  R1,0                                                             
         MVC   NDLAFLD(0),FHDA     SAVE FIELD DATA                              
         EX    R1,*-6                                                           
*                                                                               
         CLI   LVERR,C'Y'          PREVIOUS FIELD IN ERROR?                     
         BE    RDLS10                                                           
         BRAS  RE,MOVHEX           VALIDATE IT AS HEX INPUT                     
         BE    RDLS08                                                           
         MVC   NDLADDR,EFFS                                                     
         MVI   LVERR,C'Y'                                                       
         B     RDLS10                                                           
*                                                                               
RDLS08   MVC   NDLADDR,FULL                                                     
*                                                                               
         GOTO1 AHEXOUT,DMCB,FULL,DUB,4                                          
         BRAS  RE,STRIP0                                                        
         XR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         BZ    RDLS10                                                           
         BCTR  RF,0                                                             
         MVC   FHDA(0),DUB         REDISPLAY ADDRESS                            
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,FHIL                                                          
         STC   RF,FHOL                                                          
         MVI   FHII,FHIIHE                                                      
         XR    R1,R1               CLEAR THIS AGAIN                             
*                                                                               
RDLS10   BRAS  RE,RDLSNXF       ** COMMENT FIELD                                
         MVC   NDLCFLD,FHDA                                                     
*                                                                               
         AHI   R0,1                NUMBER OF LIST ENTRIES                       
         AHI   R4,NDLDLQ           NEXT ADDRESS IN SAVED LIST                   
         BRAS  RE,RDLSNXF          NEXT NUMBER FIELD                            
         B     RDLS02                                                           
*                                                                               
RDLS12   STC   R0,NDLSTCNT         SET # FIELDS                                 
*                                                                               
         LA    R4,NDLIST           NOW CLOSE UP GAPS                            
         LHI   RE,NDLDLQ                                                        
         LA    RF,NDLIST+NDLISTLQ-1                                             
*                                                                               
RDLS14   LA    R1,0(RE,R4)         NEXT ADDRESS IN SAVED LIST                   
NX       USING NDLD,R1                                                          
         OC    NDLD(NDLDLQ),NDLD   SLOT EMPTY?                                  
         BNZ   RDLS20              NO                                           
*                                                                               
RDLS16   OC    NX.NDLD(NDLDLQ),NX.NDLD                                          
         BZ    RDLS18                                                           
         MVC   NDLD(NDLDLQ),NX.NDLD                                             
         XC    NX.NDLD(NDLDLQ),NX.NDLD                                          
         B     RDLS20              NEXT ADDRESS IN SAVED LIST                   
*                                                                               
RDLS18   BXLE  R1,RE,RDLS16                                                     
*                                                                               
RDLS20   BXLE  R4,RE,RDLS14                                                     
         DROP  NX                                                               
*                                                                               
RDLS22   BRAS  RE,WTTWAB           WRITE OFF TWA PAGE                           
         XR    R0,R0                                                            
         ICM   R0,1,NDLSTCNT       LIST HAS ANY ENTRIES?                        
         BZ    EXITOK              NO                                           
         CLI   LVERR,C'Y'          ANY ERRORS?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         LA    R2,LSTNUMBH         FIRST LINE                                   
         BRAS  RE,RDLSNXF          SELECT FIELD                                 
         BRAS  RE,RDLSNXF          ADDRESS FIELD                                
         LA    R4,NDLIST           SET CURSOR AT FIRST ERROR                    
*                                                                               
RDLS24   CLC   NDLADDR,EFFS        FIELD IN ERROR?                              
         BNE   *+12                YES                                          
         ST    R2,FADRH            MOVHEX SETS ERROR MESSAGE                    
         B     EXITL                                                            
*                                                                               
         BRAS  RE,RDLSNXF          GO PAST COMMENT FIELD                        
         BRAS  RE,RDLSNXF          GO PAST NEXT # FIELD                         
         AHI   R4,NDLDLQ                                                        
         BCT   R0,RDLS24                                                        
         DC    H'0'                WHERE ERROR?                                 
*                                                                               
RDLSNXF  IC    R1,FHLN             NEXT FIELD ON SCREEN                         
         AR    R2,R1                                                            
         BR    RE                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS ANY  REQUESTED ACTIONS ON LIST SCREEN                       *         
***********************************************************************         
ACTLST   NTR1  ,                                                                
         LA    R2,LSTNUMBH         FIRST LINE                                   
         USING FHD,R2                                                           
*                                                                               
         XR    R1,R1                                                            
ACLS02   CLI   FHLN,0              END OF TWA?                                  
         BE    EXITOK              YES                                          
*                                                                               
         BRAS  RE,ACLSNXF       ** SELECT FIELD                                 
         CLI   FHIL,0              INPUT?                                       
         BE    ACLS06              NO                                           
         CLI   FHDA,C'D'           IGNORE DELETES (PROCESSED BEFORE)            
         BE    ACLS06              NO                                           
*                                                                               
         CLI   FHDA,C'G'           GOTO ADDRESS?                                
         BE    ACLS04              YES                                          
         CLI   FHDA,C'X'           GOTO EXTENDED ADDRESS?                       
         BE    ACLS04              YES                                          
*                                                                               
         ST    R2,FADRH                                                         
         MVI   FERN,2                                                           
         B     EXITL                                                            
*                                                                               
ACLS04   BRAS  RE,ACLSNXF       ** ADDRESS FIELD                                
*                                                                               
         LA    R3,SRVP2H           MOVE ADDRESS INTO P2                         
         MVC   SRVP2,SPACES                                                     
X        USING FHD,R3                                                           
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
         MVC   X.FHDA(0),FHDA      COPY IN FIELD INFORMATION                    
         EX    R1,*-6                                                           
         TM    FHII,FHIIHE                                                      
         BZ    *+8                                                              
         OI    X.FHII,FHIIHE                                                    
*                                                                               
         AHI   R1,1                                                             
         STC   R1,X.FHIL                                                        
         MVI   X.FHII,0                                                         
         LA    R3,SRVP3H           CLEAR P3 AND P4                              
         MVI   X.FHIL,0                                                         
         MVC   SRVP3,SPACES                                                     
         LA    R3,SRVP4H                                                        
         MVI   X.FHIL,0                                                         
         MVC   SRVP4,SPACES                                                     
         B     EXITH               SHOW SOMETHING SELECTED                      
         DROP  X                                                                
*                                                                               
ACLS06   BRAS  RE,ACLSNXF          ADDRESS FIELD                                
         BRAS  RE,ACLSNXF          COMMENT FIELD                                
         BRAS  RE,ACLSNXF          NEXT NUMBER FIELD                            
         B     ACLS02                                                           
*                                                                               
ACLSNXF  IC    R1,FHLN             NEXT FIELD ON SCREEN                         
         AR    R2,R1                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SEARCH MODE                                                         *         
***********************************************************************         
SRCH     NTR1  BASE=*,LABEL=*                                                   
         CLI   SRCHLEN,0           ANY SEARCH TO DO?                            
         BE    EXITOK              NO                                           
*                                                                               
         LA    RF,SRVL1S1H         SET CURSOR AFTER P4                          
         ST    RF,FADRH                                                         
         OI    DMPFLGS1,DMPSRCH    SET IN SEARCH MODE                           
*                                                                               
         L     RE,START                                                         
         A     RE,DISPLACE                                                      
         ST    RE,FULL                                                          
         BRAS  RE,SETADDR                                                       
         BNE   EXITL                                                            
         L     R5,FULL             R5 = A(ADDRESS IN IO AREA)                   
*                                                                               
         BRAS  RE,READ                                                          
         BE    *+20                                                             
         MVI   FERN,9              DISK ERROR ON DUMP FILE                      
         BL    EXITL                                                            
         MVI   FERN,34             OUT OF SCOPE                                 
         B     EXITL                                                            
*                                                                               
SRCH02   XC    FULL,FULL           FULL = LENGTH OF DATA SEARCHED               
*                                                                               
         L     RF,AINA             FIND RESIDUAL L'REC FROM TWA START           
         AHI   RF,INAL                                                          
         SR    RF,R5               RF = REMAINING L'BLOCK                       
         CHI   RF,16               IF MORE THAN 16 KEEP GOING                   
         BH    SRCH05                                                           
*                                                                               
SRCH04   OC    NBLKS2,NBLKS2       NUMBER OF BLOCKS LEFT ZERO?                  
         BNZ   SRCH04A             NO                                           
         L     R3,FULL                                                          
         A     R3,START                                                         
         A     R3,DISPLACE                                                      
         ST    R3,START                                                         
         XC    DISPLACE,DISPLACE                                                
         MVI   BYTE,255                                                         
         B     SRCH12                                                           
*                                                                               
SRCH04A  L     RF,AINA                                                          
         AHI   RF,INAL-16                                                       
         L     R5,AINBACK          SET NEW R5 VALUE                             
         MVC   0(L'INBACK,R5),0(RF)                                             
*                                                                               
         LH    RF,NBLKS2           REDUCE NUMBER OF BLOCKS LEFT                 
         BCTR  RF,0                                                             
         STH   RF,NBLKS2                                                        
         BRAS  RE,READ             READ IN A BLOCK                              
         BNE   EXITL               ERROR                                        
*                                                                               
SRCH05   LR    R2,R5               R2 = START ADDRESS                           
         L     R3,AINA                                                          
         AHI   R3,INAL-16          R3 = END ADDRESS                             
         XR    R0,R0                                                            
         IC    R0,SRCHSTR          R0 = FIRST BYTE OF SEARCH STRING             
SRCH06   SRST  R3,R2                                                            
         BC    1,SRCH06            CPU INTERRUPT                                
         BC    2,SRCH08            NOT FOUND                                    
*                                                                               
         XR    RF,RF               MATCH REMAINDER OF STRING                    
         IC    RF,SRCHLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    SRCH10              MATCHED                                      
         CLC   SRCHSTR(0),0(R3)                                                 
*                                                                               
         LA    R2,1(R3)            CONTINUE SEARCHING - FIX R2 + R3             
         L     R3,AINA                                                          
         AHI   R3,INAL-16          R3 = END ADDRESS                             
         CR    R2,R3                                                            
         BNH   SRCH06                                                           
*                                                                               
SRCH08   SR    R3,R5               NOT FOUND - UPDATE LENGTH SEARCHED           
         A     R3,FULL                                                          
         ST    R3,FULL                                                          
         C     R3,=A(256*1024)     MAX SEARCH LENGTH                            
         BNH   SRCH04              KEEP GOING                                   
*                                                                               
         L     R3,=A(256*1024)                                                  
         A     R3,START                                                         
         A     R3,DISPLACE                                                      
         ST    R3,START                                                         
         XC    DISPLACE,DISPLACE                                                
         MVI   BYTE1,255           FLAG NOT FOUND                               
         B     SRCH12                                                           
*                                                                               
SRCH10   SR    R3,R5               FOUND - UPDATE LENGTH SEARCHED               
         A     R3,FULL                                                          
         A     R3,START                                                         
         A     R3,DISPLACE                                                      
         ST    R3,START                                                         
         XC    DISPLACE,DISPLACE                                                
         MVI   BYTE1,0             FLAG FOUND                                   
*                                                                               
SRCH12   XC    SRVP2,SRVP2         CLEAR START + DISPLACEMENT                   
         XC    SRVP3,SRVP3                                                      
*                                                                               
         GOTO1 AHEXOUT,DMCB,START,DUB,4  SET START ADDRESS                      
         BRAS  RE,STRIP0                                                        
         XR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         BZ    SRCH14                                                           
         BCTR  RF,0                                                             
         MVC   SRVP2(0),DUB                                                     
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,SRVP2H+(FHIL-FHD)                                             
         STC   RF,SRVP2H+(FHOL-FHD)                                             
*                                                                               
SRCH14   MVI   HDRN,21                                                          
         OC    NBLKS2,NBLKS2                                                    
         BNZ   *+8                                                              
         MVI   HDRN,22                                                          
*                                                                               
         CLI   BYTE1,255           DID WE FIND WHAT WE SEARCHED FOR?            
         BE    EXITOK              NO                                           
*                                                                               
         MVI   HDRN,20                                                          
*                                                                               
         L     R1,START            FIX UP THE START ADDRESS                     
         AHI   R1,1                                                             
         ST    R1,FULL             SET NEXT SEARCH ADDRESS                      
         BCTR  R1,0                                                             
*                                                                               
         SRL   R1,2                ALIGN ON FULLWORD                            
         SLL   R1,2                                                             
         ST    R1,START                                                         
         LR    R0,R1                                                            
         AHI   R1,-16              SEE IF WE CAN SHOW CONTEXT                   
*                                                                               
         C     R0,HDR.DMPXALO      IS ADDRESS VALID XA ADDRESS?                 
         BL    SRCH16                                                           
         C     R1,HDR.DMPXALO      CAN SHOW IN CONTEXT?                         
         BL    SRCH18              NO                                           
         B     SRCH20                                                           
*                                                                               
SRCH16   C     R0,HDR.DMPPART      IN MEMORY?                                   
         BL    EXITOK              BELOW LOWER BOUND                            
         C     R1,HDR.DMPPART      SUBTRACT IT WITH THE LOWER BOUND             
         BH    SRCH20              BELOW LOWER BOUND                            
*                                                                               
SRCH18   OI    SRVL1H+(FHAT-FHD),FHATHI HIGHLIGHT FIRST LINE                    
         OI    SRVL1H1H+(FHAT-FHD),FHATHI                                       
         OI    SRVL1H2H+(FHAT-FHD),FHATHI                                       
         OI    SRVL1H3H+(FHAT-FHD),FHATHI                                       
         OI    SRVL1H4H+(FHAT-FHD),FHATHI                                       
         B     SRCH22                                                           
*                                                                               
SRCH20   ST    R1,START                                                         
         LA    RF,SRVL2S1H         SET CURSOR + HIGHLIGHT 2ND LINE              
         ST    RF,FADRH                                                         
         OI    SRVL2H+(FHAT-FHD),FHATHI                                         
         OI    SRVL2H1H+(FHAT-FHD),FHATHI                                       
         OI    SRVL2H2H+(FHAT-FHD),FHATHI                                       
         OI    SRVL2H3H+(FHAT-FHD),FHATHI                                       
         OI    SRVL2H4H+(FHAT-FHD),FHATHI                                       
*                                                                               
SRCH22   GOTO1 AHEXOUT,DMCB,FULL,DUB,4                                          
         BRAS  RE,STRIP0                                                        
         XR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         BZ    EXITOK                                                           
         BCTR  RF,0                                                             
         MVC   SRVP2(0),DUB                                                     
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,SRVP2H+(FHIL-FHD)                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN FROM ADDRESS IN START+DISPLACE                       *         
***********************************************************************         
DISSCRN  NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,START                                                      
         A     RE,DISPLACE                                                      
         ST    RE,START                                                         
         ST    RE,FULL                                                          
         L     RF,ASVBLOCK                                                      
         ST    RE,NDLSTRT-NDSAVED(RF)                                           
         BRAS  RE,SETADDR                                                       
         BNE   EXITL                                                            
         L     R5,FULL             R5 = A(START POINT)                          
         BRAS  RE,READ                                                          
         BE    *+20                                                             
         MVI   FERN,9              DISK ERROR ON DUMP FILE                      
         BL    EXITL                                                            
         MVI   FERN,34             OUT OF SCOPE                                 
         B     EXITL                                                            
*                                                                               
         LA    R2,SRVL1H                                                        
         USING LIND,R2                                                          
         L     R6,ASVBLOCK                                                      
         AHI   R6,NDMPCOLS-NDSAVED                                              
         L     R3,ASVBLOCK                                                      
         AHI   R3,NDMPADDR-NDSAVED                                              
*                                                                               
DSS02    L     RF,AINA             FIND RESIDUAL L'REC FROM TWA START           
         AHI   RF,INAL                                                          
         SR    RF,R5               RF = REMAINING L'BLOCK                       
         CHI   RF,16               IF MORE THAN 16 KEEP GOING                   
         BH    DSS04                                                            
*                                                                               
         L     R1,AINA             COPY DATA TO INBACK                          
         AHI   R1,INAL-16                                                       
         L     RE,AINBACK                                                       
         MVC   0(L'INBACK,RE),0(R1)                                             
         AHI   R5,-(RLEN)          POINT TO WHERE WE ARE                        
*                                                                               
         L     R0,AINA             CLEAR OUT INPUT BUFFER                       
         LHI   R1,INAL                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LH    R0,NBLKS2           ANY MORE BLOCKS TO READ?                     
         AHI   R0,-1                                                            
         STH   R0,NBLKS2                                                        
         BNZ   *+12                YES                                          
         MVI   DONESW,1                                                         
         B     DSS04                                                            
*                                                                               
         BRAS  RE,READ                                                          
         BE    *+20                                                             
         MVI   FERN,9              DISK ERROR ON DUMP FILE                      
         BL    EXITL                                                            
         MVI   FERN,34             OUT OF SCOPE                                 
         B     EXITL                                                            
*                                                                               
DSS04    CLC   1(15,R5),0(R5)                                                   
         BNE   DSS08                                                            
         CLI   SAMIND,1                                                         
         MVI   SAMIND,1                                                         
         BNE   DSS06                                                            
         BCTR  R5,R0                                                            
         CLC   0(1,R5),1(R5)       BYPASS LINE IF SAME AS LAST                  
         LA    R5,1(R5)                                                         
         BE    DSS12                                                            
*                                                                               
DSS06    GOTO1 AHEXOUT,DMCB,0(R5),LINCOL1,4                                     
         MVC   LINCOL2,=C'--SAME--'                                             
         MVC   LINHEX(4),0(R5)                                                  
         TR    LINHEX(4),TRTAB                                                  
         MVC   0(16,R6),0(R5)      STORE THE IDENTICAL BYTES                    
         B     DSS10                                                            
*                                                                               
DSS08    MVI   SAMIND,0                                                         
         GOTO1 AHEXOUT,DMCB,(R5),WORK,16                                        
         MVC   LINCOL1(8),WORK+00                                               
         MVC   LINCOL2(8),WORK+08                                               
         MVC   LINCOL3(8),WORK+16                                               
         MVC   LINCOL4(8),WORK+24                                               
         MVC   LINHEX,0(R5)                                                     
         TR    LINHEX,TRTAB                                                     
         MVC   0(16,R6),0(R5)      STORE THE DATA                               
*                                                                               
DSS10    MVC   0(4,R3),START       COPY ADDRESS CONTAINING THE 16 BYTES         
         GOTO1 AHEXOUT,DMCB,START,DUB,4                                         
         MVC   LINSTRT,DUB                                                      
         AHI   R6,16               NEXT QUAD IN COLUMN DATA                     
         LA    R3,4(R3)            NEXT ADDRESS IN ADDRESS DATA                 
*                                                                               
         LA    R2,LINNXT                                                        
         CLI   0(R2),0             BEYOND TWA?                                  
         BE    DSS14               YES, DONE                                    
*                                                                               
DSS12    LHI   R4,16               BUMP POINTERS                                
         A     R4,DISPLACE                                                      
         ST    R4,DISPLACE                                                      
         LHI   R4,16                                                            
         A     R4,START                                                         
         ST    R4,START                                                         
*                                                                               
         AHI   R5,16                                                            
         CLI   DONESW,1            CHECK HAVE LAST REC                          
         BNE   DSS02               NO                                           
         C     R5,AINA             YES - CHECK START OF THIS LINE               
         BL    DSS02                                                            
         XC    DISPLACE,DISPLACE   ZERO DISP AT END OF PTTN                     
*                                                                               
DSS14    ICM   RF,15,AKEYWORD         INDIRECT MODE                             
         BZ    *+12                                                             
         CLI   KEYACT-KEYTABD(RF),KEYAIND                                       
         BE    DSS16                                                            
         CLI   KEYACT-KEYTABD(RF),KEYAPGO                                       
         BE    DSS16                                                            
         CLI   KEYACT-KEYTABD(RF),KEYAGO                                        
         BE    DSS16                                                            
         OC    DUMPREG,DUMPREG     DUMP REGISTER USED?                          
         BNZ   DSS16               YES                                          
         TM    DMPFLGS1,DMPSRCH    IN SEARCH MODE                               
         BO    DSS16                                                            
         TM    DMPFLGS1,DMPSX      IN X MODE                                    
         BO    DSS16                                                            
*                                                                               
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+(FHIL-FHD),0                                              
         MVI   SRVP3H+(FHOL-FHD),0                                              
         GOTO1 AHEXOUT,DMCB,DISPLACE,DUB,4                                      
         BRAS  RE,STRIP0                                                        
         XR    RF,RF                                                            
         ICM   RF,1,BYTE                                                        
         BZ    DSS16                                                            
         BCTR  RF,0                                                             
         MVC   SRVP3(0),DUB                                                     
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,SRVP3H+(FHIL-FHD)                                             
         STC   RF,SRVP3H+(FHOL-FHD)                                             
*                                                                               
DSS16    L     R2,ASVBLOCK                                                      
         USING NDSAVED,R2                                                       
         L     R0,START            SAVE IN CASE OF PF8                          
         ST    R0,NDNXT                                                         
*                                                                               
         TM    DMPFLGS1,DMPSRCH    IN SEARCH MODE                               
         BO    *+8                 YES                                          
         MVI   HDRN,12                                                          
         OC    NBLKS2,NBLKS2       ANY MORE BLOCKS TO READ                      
         BZ    EXITOK              YES                                          
*                                                                               
         TM    DMPFLGS1,DMPSRCH    IN SEARCH MODE                               
         BO    EXITOK              YES                                          
*                                                                               
         MVI   HDRN,12             SET DISPLAYED                                
         OC    DUMPREG,DUMPREG                                                  
         BNZ   EXITOK                                                           
*                                                                               
         LA    RF,SRVP4H                                                        
         ST    RF,FADRH                                                         
         MVI   HDRN,13             SET DISPLAYED, ENTER TO PAGE                 
         ICM   RF,15,AKEYWORD                                                   
         BZ    EXITOK                                                           
         CLI   KEYACT-KEYTABD(RF),KEYASX                                        
         BNE   *+12                                                             
         LA    RF,SRVP3H                                                        
         ST    RF,FADRH                                                         
         CLI   KEYACT-KEYTABD(RF),KEYAIND                                       
         BNE   *+12                                                             
         LA    RF,SRVL1S1H                                                      
         ST    RF,FADRH                                                         
         CLI   KEYACT-KEYTABD(RF),KEYAGO                                        
         BNE   EXITOK                                                           
         MVI   HDRN,24                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE                                               *         
***********************************************************************         
DISERR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),MYSYSN4                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'E#'        SET ERROR                                  
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET ERROR NUMBER                             
         IC    R0,FERN                                                          
         CHI   R0,255                                                           
         BNE   *+6                                                              
         XR    R0,R0                                                            
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         LTR   R0,R0               SPECIAL ERROR MESSAGE?                       
         BNE   DERR02              NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,FERNA                                                       
         MVC   0(EMSGL,R4),0(RF)                                                
         B     DERR04                                                           
*                                                                               
DERR02   AHI   R0,-1                                                            
         MHI   R0,EMSGL                                                         
         L     RF,AERRMSGS                                                      
         AR    RF,R0                                                            
         MVC   0(EMSGL,R4),0(RF)                                                
*                                                                               
DERR04   ICM   R2,15,FADRH         SET CURSOR ON BAD FIELD                      
         BZ    EXITOK                                                           
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY OK MESSAGE                                                  *         
***********************************************************************         
DISOK    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SRVMSGH                                                       
         USING FHD,R2                                                           
         MVC   SRVMSG,SPACES                                                    
         OI    FHOI,FHOITR                                                      
*                                                                               
         LA    R4,FHDA             SET UP (SYS) AT START OF FIELD               
         MVI   0(R4),C'('                                                       
         MVC   1(4,R4),MYSYSN4                                                  
         AHI   R4,4                                                             
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         AHI   R4,3                                                             
*                                                                               
         MVC   0(2,R4),=C'I#'      SET OK                                       
         AHI   R4,2                                                             
*                                                                               
         XR    R0,R0               SET MESSAGE NUMBER                           
         IC    R0,HDRN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R4),DUB                                                      
         AHI   R4,3                                                             
*                                                                               
         AHI   R0,-1                                                            
         MHI   R0,IMSGL                                                         
         L     RF,AOKMSGS                                                       
         AR    RF,R0                                                            
         MVC   0(IMSGL,R4),0(RF)                                                
*                                                                               
         L     R3,ATIOB                                                         
         USING TIOBD,R3                                                         
         OI    TIOBINDS,TIOBSETC                                                
         ICM   RF,15,FADRH                                                      
         BNZ   *+8                                                              
         LA    RF,SRVL1H                                                        
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,FERRDSP                                                 
         B     EXITOK                                                           
         DROP  R2,R3                                                            
*                                                                               
         TITLE '$DUMP - DISPLAY ALL SCREEN'                                     
***********************************************************************         
* LIST ALL DUMPS IN DUMP FILE                                         *         
***********************************************************************         
DISALL   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                PROBLEM LOADING 'ALL' SCREEN                 
*                                                                               
         BRAS  RE,VALP4            REVALIDATE ASNUM                             
         BNE   EXITL                                                            
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         MVC   SRVP1F,ALL1HMSG     SET NEW PARAMETER HEADERS                    
         MVC   SRVP2F,ALL2HMSG                                                  
         MVC   SRVP3F,ALL3HMSG                                                  
         MVC   SRVP4F,ALL4HMSG                                                  
         MVC   DMPL1,ALLL1                                                      
         OI    DMPL1H+(FHAT-FHD),FHATHI                                         
         MVC   DMPL2,ALLL2                                                      
         OI    DMPL2H+(FHAT-FHD),FHATHI                                         
*                                                                               
         MVI   HDRN,1              ALL DUMPS MESSAGE                            
         CLI   PFKEY,0             <ENTER> VALID                                
         BE    DISA02                                                           
         CLI   PFKEY,2             PFK02 VALID                                  
         BE    DISA02                                                           
         CLI   PFKEY,7             PFK07 VALID                                  
         BE    DISA02                                                           
         CLI   PFKEY,8             PFK08 VALID                                  
         BE    DISA02                                                           
         MVI   FERN,7                                                           
         B     EXITL               INVALID PFKEY VALUE SET                      
*                                                                               
DISA02   LA    R3,WRK                                                           
         XC    WRK,WRK             CLEAR OUT LIST OF RANGES                     
         MVC   HALF(1),MYSYSFL     HALF(1) = MY TYPE (ADV/REP/TEST)             
         NI    HALF,FACITST+FACIREP                                             
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,TBOFFS                                                     
         SAC   512                                                              
         ICM   R2,15,TABSADDR-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
*                                                                               
         USING FACITABD,R1                                                      
DISA04   L     R1,AFACITAB         MATCH FACPAK NAME TO LIST                    
         LHI   RF,L'FACITAB                                                     
DISA06   CLI   FACISN4,EOTB        FACPAK NAME NOT FOUND                        
         BE    DISA10                    -DON'T DISPLAY IT                      
         CLC   FACISN4,TDSYSNA                                                  
         BE    *+8                                                              
         BXH   R1,RF,DISA06                                                     
*                                                                               
         MVC   HALF+1(1),FACIFL    ADV/REP/TEST SYSTEMS POSSIBLE                
         NI    HALF+1,(FACITST+FACIREP)                                         
         CLC   HALF(1),HALF+1      ONLY DISPLAY SAME TYPE AS SELF               
         BNE   DISA10                                                           
         DROP  R1                                                               
*                                                                               
DISA08   ICM   RF,15,TDDUMPFS      0(2,R3) = START NUMBER                       
         STCM  RF,3,0(R3)                                                       
         ICM   RF,15,TDDUMPMX      2(2,R3) = END NUMBER                         
         STCM  RF,3,2(R3)                                                       
         ICM   RF,15,TDDUMPST      4(4,R3) = STATUS FLAGS                       
         STCM  RF,15,4(R3)                                                      
         AHI   R3,8                                                             
         CLC   MYSYSN4,TDSYSNA     IS THIS MY FACPAK?                           
         BNE   DISA10              NO                                           
*                                                                               
         TM    TDDUMPST,X'80'      TEST STOP FLAG                               
         BZ    *+8                                                              
         MVI   HDRN,2              ALL DUMPS - DUMPS SUPPRESSED                 
*                                                                               
         ICM   RF,15,TDDUMPFS      SAVE START NUMBER                            
         STH   RF,SYSDMIN                                                       
         ICM   RF,15,TDDUMPMX      SAVE END NUMBER                              
         STH   RF,SYSDMAX                                                       
*                                                                               
DISA10   AHI   R2,TDLENQ           NEXT DUMP IN LIST                            
         ICM   RF,15,TDSYSNA       FINISHED LIST OF FACPAKS?                    
         BNZ   DISA04                                                           
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO      GET OUT OF AR MODE                           
         DROP  R2                                                               
*                                                                               
         LA    R3,WRK              BUILD VALID DUMP LIST                        
         L     R4,ASORTBLK                                                      
         USING ALIND,R4                                                         
         L     R0,ASORTBLK         CLEAR SORT BLOCK TO SPACES                   
         LHI   R1,SORTBLKL                                                      
         LHI   RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
         XC    DUMPINDX,DUMPINDX   RESET COUNT OF MATCHING DUMPS                
*                                                                               
DISA14   OC    0(2,R3),0(R3)       FINISHED BUILDING DUMP LIST                  
         BZ    DISA26              YES                                          
         LH    R2,0(R3)            R2=CURRENT DUMP COUNTER                      
*                                                                               
DISA16   STH   R2,DUMPNUM          GET HEADER FOR THIS DUMP                     
         BRAS  RE,GETHDR                                                        
         BE    DISA20                                                           
*                                                                               
         TM    MYSYSFL,FACITST     TEST SYSTEM?                                 
         BZ    DISA18              NO                                           
*                                                                               
         MVI   FERN,0                                                           
         CH    R2,SYSDMIN          ONLY MY ERRORS IF TEST SYSTEM                
         BL    DISA24                                                           
         CH    R2,SYSDMAX                                                       
         BH    DISA24                                                           
*                                                                               
DISA18   MVI   4(R4),X'FF'        FLAG BAD                                      
         XC    ALINDAT,ALINDAT    SET SORT HIGH                                 
         CURED (R2),(3,ALINNUM),0,ALIGN=LEFT                                    
         B     DISA22                                                           
*                                                                               
DISA20   BRAS  RE,ALDIS            DISPLAY LINE INTO SORT BLOCK                 
         BRAS  RE,ALFLT            FILTER THIS DUMP                             
         BE    DISA22                                                           
         MVC   ALINLIN,SPACES                                                   
         B     DISA24                                                           
*                                                                               
DISA22   LH    RF,DUMPINDX         INCREMENT COUNT OF DUMPS TO DISPLAY          
         AHI   RF,1                                                             
         STH   RF,DUMPINDX                                                      
         AHI   R4,ALLSCLEN         NEXT LINE IN SORT BLOCK                      
*                                                                               
DISA24   AHI   R2,1                NEXT DUMP IN SLOT                            
         CH    R2,2(R3)            STILL WITHIN THIS SLOT RANGE?                
         BNH   DISA16              YES                                          
         AHI   R3,8                NEXT FACPAK SLOT                             
         B     DISA14                                                           
*                                                                               
DISA26   OC    DUMPINDX,DUMPINDX   ANY DUMPS TO DISPLAY?                        
         BZ    EXITOK              NO                                           
*                                                                               
         L     R4,ASORTBLK         SORT DISPLAY LINES                           
         USING ALIND,R4                                                         
         LHI   R0,ALLSCLEN         SET UP XSORT PARAMETERS                      
         LH    RF,DUMPINDX                                                      
         LA    R2,ALINTRM-ALINDAT                                               
         LA    R3,ALINDAT-ALINLIN                                               
         GOTO1 AXSORT,DMCB,(X'FF',(R4)),(RF),(R0),(R2),(R3)                     
         BRAS  RE,DISASCN          DISPLAY ALL SCREEN                           
*                                                                               
         XC    FERN,FERN                                                        
         CLI   PFKEY,2             PRESSED SELECT KEY?                          
         BNE   EXITOK                                                           
         BRAS  RE,ALLSLCT          DEAL WITH SELECT FROM ALL SCREEN             
         BE    EXITH               SELECTED OK                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SET DUMP DETAILS INTO SORT BLOCK FOR CURRENT LINE                   *         
* NTRY: R2 = CURRENT DUMP NUMBER                                      *         
*       R3 = A(DUMP DETAIL BLOCK)                                     *         
*       R4 = A(LINE ENTRY - COVERED BY ALIND)                         *         
***********************************************************************         
         USING ALIND,R4                                                         
ALDIS    NTR1  BASE=*,LABEL=*                                                   
         CURED HDR.DMPNUM,(3,ALINNUM),0,ALIGN=LEFT                              
         MVC   ALINADV,HDR.DMPDESC+4                                            
*                                                                               
         LR    RF,R2                                                            
         SH    RF,0(R3)            RF = (CURRENT DUMP - FIRST DUMP)             
         LHI   R1,1                                                             
         SLL   R1,32-1             SET BITMASK                                  
         SRL   R1,1(RF)                                                         
         N     R1,4(R3)                                                         
         BZ    *+8                                                              
         MVI   ALINSVE,C'*'        DUMP SAVED                                   
*                                                                               
         CURED HDR.DMPDUPS,(2,ALINDUP),0,ALIGN=LEFT                             
         CLC   HDR.DMPDUPS,=F'99'      WITHIN RANGE?                            
         BNH   *+10                                                             
         MVC   ALINDUP,=CL2'!!'    TOO MANY                                     
*                                                                               
ALD02    MVC   ALINSTA,HDR.DMPSTAT DUMP STATUS - ?/P/F                          
         CLI   ALINSTA,C' '                                                     
         BH    *+8                                                              
         MVI   ALINSTA,C'?'        NO PROGRESS YET!                             
*                                                                               
         MVC   ALINWHO,HDR.DMPWHO                                               
*                                                                               
         MVC   ALINDAT,EFFS        DATE AND TIME                                
         OC    HDR.DMPHDTE,HDR.DMPHDTE                                          
         BZ    ALD04                                                            
         XC    ALINDAT,ALINDAT                                                  
         MVC   ALINDAT(L'DMPHDTE),HDR.DMPHDTE                                   
*                                                                               
ALD04    GOTO1 ADATTIM,DMCB,(X'81',HDR.DMPTIME),WORK                            
         MVC   ALINTIM+0(2),WORK+8                                              
         MVC   ALINTIM+3(2),WORK+10                                             
         MVC   ALINTIM+6(2),WORK+12                                             
         MVI   ALINTIM+2,C'.'                                                   
         MVI   ALINTIM+5,C'.'                                                   
*                                                                               
         MVC   ALINTRM,HDR.DMPHTRM TERMINAL                                     
         TR    ALINTRM,TRTAB                                                    
         MVC   ALINSYS,HDR.DMPHSYS SYSTEM                                       
*&&US                                                                           
         CLC   =C'SPOT',ALINSYS    CHANGE SPOTX TO SPTX                         
         BNE   *+10                                                             
         MVC   ALINSYS+2(3),HDR.DMPHSYS+3                                       
         CLC   =C'PRNT',ALINSYS    SAME FOR PRNT                                
         BNE   *+10                                                             
         MVC   ALINSYS+2(3),HDR.DMPHSYS+3                                       
*&&                                                                             
*&&UK                                                                           
         CLC   =C'CONT',ALINSYS                                                 
         BNE   *+10                                                             
         MVC   ALINSYS(5),=C'CONT '                                             
         CLC   =C'SERV',ALINSYS                                                 
         BNE   *+10                                                             
         MVC   ALINSYS(5),=C'SERV '                                             
         CLI   ALINSYS+4,C' '                                                   
         BNH   *+14                                                             
         MVC   ALINSYS+2(2),ALINSYS+3                                           
         MVI   ALINSYS+4,C' '                                                   
*&&                                                                             
ALD05    TR    ALINSYS,TRTAB                                                    
*                                                                               
         MVC   ALINPRG,HDR.DMPHPRG PROGRAM                                      
         TR    ALINPRG,TRTAB                                                    
         MVC   ALINSUB,HDR.DMPHRTN FAILING ROUTINE                              
         TR    ALINSUB,TRTAB                                                    
         MVC   ALINDSP,DOTFILL     OFFSET (PSW-RB)                              
         L     R1,HDR.DMPPSWD+4                                                 
         LA    R1,0(R1)            ADDRESS OF NEXT INSTRUCTION                  
         L     RF,HDR.DMPRB                                                     
         LA    RF,0(RF)            CONTENTS OF BASE REGISTER FROM DUMP          
         SR    R1,RF               OFFSET OF NEXT INTRUCTION                    
         ST    R1,FULL                                                          
         OC    FULL(2),FULL        DISPLACEMENT UP TO 64K ONLY                  
         BNZ   ALD08                                                            
*                                                                               
         GOTO1 AHEXOUT,DMCB,FULL+2,ALINDSP,2,0                                  
                                                                                
         LA    R1,ALINDSP                                                       
         LA    RE,1                                                             
         LA    RF,ALINDSP+L'ALINDSP-2                                           
ALD06    CLI   0(R1),C'0'          BLANK LEADING ZEROES                         
         BNE   ALD08                                                            
         MVI   0(R1),C' '                                                       
         BXLE  R1,RE,ALD06                                                      
*                                                                               
ALD08    MVC   ALINABC,HDR.DMPPART ABEND CODE                                   
         CLI   HDR.DMPPART,C'I' PROGRAM CHECK                                   
         BNE   *+8                                                              
         MVI   ALINABC,C'L'        LOOP                                         
*                                                                               
         MVC   ALININS,DOTFILL     CODE AT ABEND ADDRESS-6                      
         OC    HDR.DMPHPW6,HDR.DMPHPW6                                          
         BZ    EXITOK                                                           
         GOTO1 AHEXOUT,DMCB,HDR.DMPHPW6,ALININS,6,0                             
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DO FILTERING FOR THIS LINE                                          *         
* NTRY: R4 = SCREEN LINE TO FILTER ON                                 *         
* EXIT: CC EQ  - KEEP                                                 *         
*       CC NEQ - DISCARD                                              *         
***********************************************************************         
         USING ALIND,R4                                                         
ALFLT    NTR1  BASE=*,LABEL=*                                                   
         CLI   ALLFLVL,ALLFLVLX    SAVED DUMPS ONLY                             
         BNE   ALFT01                                                           
         CLI   ALINSVE,C' '                                                     
         BNH   EXITL                                                            
         B     EXITOK                                                           
*                                                                               
ALFT01   CLI   ALLFFACL,0          SPECIFIC FACPAK REQUESTED?                   
         BNE   ALFT02              YES                                          
         TM    MYSYSFL,FACITST     TEST SYSTEM?                                 
         BZ    *+14                NO                                           
         CLC   MYSYSN4,ALINADV     FILTER FOR OUR TEST SYSTEM ONLY              
         BNE   EXITL                                                            
*                                                                               
ALFT02   CLI   ALLFLVL,0           ANY FILTER INPUT BY USER?                    
         BE    EXITOK              NO                                           
*                                                                               
         CLI   ALLFFACL,0          FACPAK FILTER SET?                           
         BE    ALFT06              NO                                           
*                                                                               
         CLI   ALLFFACL,4          SPECIFIC FACPAK FILTER?                      
         BNE   ALFT04              NO                                           
         CLC   =C'AD',ALLFFAC      ADV FACPAK?                                  
         BE    *+14                NO                                           
         CLC   =C'RE',ALLFFAC      REP FACPAK?                                  
         BNE   ALFT04              NO                                           
*                                                                               
         CLC   ALLFFAC(2),ALINADV  MATCH AD OR RE?                              
         BNE   EXITL                                                            
         CLC   ALLFFAC+3(1),ALINADV+2                                           
         B     EXIT                                                             
*                                                                               
ALFT04   XR    RF,RF               MATCH FOR INPUT LENGTH ONLY                  
         IC    RF,ALLFFACL                                                      
         BCTR  RF,0                                                             
         EX    RF,ALFFCLC                                                       
         B     EXIT                                                             
*                                                                               
ALFT06   CLI   ALLFSYSL,0          SYSTEM FILTER SET?                           
         BE    EXITOK              NO                                           
         XR    RF,RF                                                            
         IC    RF,ALLFSYSL                                                      
         BCTR  RF,0                                                             
         EX    RF,ALFSCLC          FILTER ON SYSTEM                             
         BNE   EXITL                                                            
*                                                                               
         CLI   ALLFPRGL,0          SYSTEM HAS PROGRAM FILTER SET ALSO           
         BE    EXITOK              NO                                           
         XR    RF,RF                                                            
         IC    RF,ALLFPRGL                                                      
         BCTR  RF,0                                                             
         EX    RF,ALFPCLC          FILTER ON PROGRAM                            
         B     EXIT                                                             
*                                                                               
ALFFCLC  CLC   ALINADV(0),ALLFFAC                                               
ALFSCLC  CLC   ALINSYS(0),ALLFSYS                                               
ALFPCLC  CLC   ALINPRG(0),ALLFPRG                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ALL SCREEN FROM SORTBLK                          *         
***********************************************************************         
DISASCN  NTR1  BASE=(*,DISASCNX),LABEL=*                                        
         L     R4,ASORTBLK                                                      
         MVC   HALF,DUMPINDX       HALF = COUNT OF DUMPS TO DISPLAY             
         OC    ASNUM,ASNUM         START NUMBER INPUT?                          
         BZ    DASC06              NO                                           
*                                                                               
         MVC   DUB,SPACES                                                       
         CURED ASNUM,(3,DUB),0,ALIGN=LEFT                                       
*                                                                               
         L     R4,ASORTBLK                                                      
         USING ALIND,R4                                                         
         LH    R0,DUMPINDX                                                      
DASC02   CLC   ALINNUM,DUB                                                      
         BE    DASC04                                                           
         AHI   R4,ALLSCLEN                                                      
         BCT   R0,DASC02                                                        
*                                                                               
         L     R4,ASORTBLK         START FROM BEGINNING IF NO MATCH             
         LH    R0,DUMPINDX                                                      
*                                                                               
DASC04   STH   R0,HALF             SET NUMBER OF DUMPS LEFT TO DISPLAY          
*                                                                               
DASC06   CLI   PFKEY,7             SCROLL UP?                                   
         BNE   DASC08              NO                                           
         LH    R0,HALF                                                          
         AHI   R0,ALLSCLNE         UP COUNT OF LINES LEFT                       
         CH    R0,DUMPINDX         MORE THAN MAX?                               
         BL    *+8                 NO                                           
         LH    R0,DUMPINDX         SET TO MAX                                   
         STH   R0,HALF                                                          
         B     DASC10                                                           
*                                                                               
DASC08   CLI   PFKEY,8             SCROLL DOWN?                                 
         BNE   DASC10              NO                                           
*                                                                               
         LH    R0,HALF                                                          
         AHI   R0,-(ALLSCLNE)                                                   
         BP    *+8                                                              
         LHI   R0,1                DISPLAY ONLY LAST DUMP                       
         STH   R0,HALF                                                          
*                                                                               
DASC10   L     R4,ASORTBLK                                                      
         LH    R0,DUMPINDX                                                      
         SH    R0,HALF                                                          
         BNP   *+12                                                             
         AHI   R4,ALLSCLEN                                                      
         BCT   R0,*-4                                                           
*                                                                               
         OC    HALF,HALF           ENSURE WE HAVE SOMETHING TO DISPLAY          
         BZ    EXITOK              NO                                           
*                                                                               
         XR    R0,R0               R0=COUNT OF LINES SHOWN                      
         LA    R3,DMPL3H           NOW DISPLAY ONTO SCREEN                      
         USING FHD,R3                                                           
         ST    R3,FADRH                                                         
*                                                                               
         MVC   SRVP4,SPACES        SHOW NUMBER OF FIRST DUMP                    
         MVC   SRVP4(L'ALINNUM),ALINNUM                                         
         MVI   SRVP4H+(FHOL-FHD),L'ALINNUM                                      
         OI    SRVP4H+(FHOI-FHD),FHOITR                                         
*                                                                               
DASC12   OI    FHOI,FHOITR         TRANSMIT                                     
         NI    FHAT,255-FHATHI     NORMAL                                       
         CLC   ALINDAT,EFFS        CONVERT DATE IF ONE IS SET                   
         BE    DASC16                                                           
         CLC   TODAY3,ALINDAT      SET WORD TODAY AS DATE FOR DUMP?             
         BNE   DASC14              NO                                           
         MVC   ALINDAT,LTODAY                                                   
*                                                                               
         CLI   ALINSTA,C'?'        HIGHLIGHT TODAYS UNASSIGNED DUMPS            
         BNE   DASC16                                                           
         OI    FHAT,FHATHI                                                      
         B     DASC16                                                           
*                                                                               
DASC14   GOTO1 ADATCON,DMCB,(X'03',ALINDAT),(X'0C',ALINDAT)                     
*                                                                               
DASC16   MVC   FHDA(ALLSCLEN),0(R4)                                             
         OC    ALINDAT,ALINDAT                                                  
         BNZ   *+10                                                             
         MVC   ALINDAT,DOTFILL                                                  
         CLI   FHDA+4,X'FF'                                                     
         BNE   *+10                                                             
         MVC   FHDA+4(L'DSPAERR1),DSPAERR1                                      
*                                                                               
         AHI   R3,ALLSCLEN+L'DMPL1H                                             
*                                                                               
         AHI   R0,1                INCREMENT SCREEN LINES DISPLAYED             
         CHI   R0,ALLSCLNE         SCOPE                                        
         BE    EXITOK                                                           
*                                                                               
DASC18   LH    RF,HALF             DECREMENT NUMBER OF LINES LEFT               
         AHI   RF,-1                                                            
         BZ    EXITOK              NO MORE                                      
         STH   RF,HALF                                                          
         AHI   R4,ALLSCLEN                                                      
         B     DASC12                                                           
         DROP  R3,R4                                                            
*                                                                               
DISASCNX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS SELECT KEY PRESS                                 *         
***********************************************************************         
ALLSLCT  NTR1  BASE=(*,ALLSLCTX),LABEL=*                                        
         LA    R2,DMPL3H           FIND FIELD FOR CURSOR                        
         USING FHD,R2                                                           
         CLC   SCURSOR,FHAD                                                     
         BL    ASLC06                                                           
         LA    R2,DMPLLH                                                        
         CLC   SCURSOR,FHAD                                                     
         BNL   ASLC06                                                           
*                                                                               
         LA    R2,DMPL3H           SET A(FIRST FIELD)                           
         ST    R2,FULL                                                          
         LA    RF,DMPLLH                                                        
         XR    RE,RE                                                            
*                                                                               
ASLC02   CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    ASLC04                                                           
         ST    R2,FULL                                                          
         IC    RE,FHLN                                                          
         BXLE  R2,RE,ASLC02                                                     
         DC    H'0'                                                             
*                                                                               
ASLC04   L     R2,FULL             CURSOR FIELD IS IN FULL                      
         CLI   FHDA,X'F0'                                                       
         BL    ASLC06              FIELD IS NON-NUMERIC                         
         USING ALIND,FHDA                                                       
*                                                                               
         MVC   SRVP1,SPACES                                                     
         MVC   SRVP1(L'ALINNUM),ALINNUM                                         
*                                                                               
         LHI   R0,L'ALINNUM        COUNT NUMBER OF DIGITS DISPLAYED             
         LA    R1,SRVP1                                                         
         XR    RF,RF                                                            
         CLI   0(R1),C'0'          DIGIT IS 0-9 IN THIS CASE                    
         BL    *+16                                                             
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         BCT   R0,*-16                                                          
*                                                                               
         STC   RF,SRVP1H+(FHIL-FHD)                                             
         STC   RF,SRVP1H+(FHOL-FHD)                                             
         OI    SRVP1H+(FHOI-FHD),FHOITR                                         
         OI    SRVP1H+(FHII-FHD),FHIINU+FHIIAL+FHIITH                           
*                                                                               
         MVC   SRVP1F,DIS1HMSG                                                  
         OI    SRVP1FH+(FHOI-FHD),FHOITR                                        
         MVC   SRVP2F,DIS2HMSG                                                  
         OI    SRVP2FH+(FHOI-FHD),FHOITR                                        
         MVC   SRVP2,SPACES                                                     
         MVI   SRVP2H+(FHIL-FHD),0                                              
         MVI   SRVP2H+(FHOL-FHD),0                                              
         OI    SRVP2H+(FHOI-FHD),FHOITR                                         
         MVC   SRVP3F,DIS3HMSG                                                  
         OI    SRVP3FH+(FHOI-FHD),FHOITR                                        
         MVC   SRVP3,SPACES                                                     
         MVI   SRVP3H+(FHIL-FHD),0                                              
         MVI   SRVP3H+(FHOL-FHD),0                                              
         OI    SRVP3H+(FHOI-FHD),FHOITR                                         
         MVC   SRVP4F,DIS4HMSG                                                  
         OI    SRVP4FH+(FHOI-FHD),FHOITR                                        
         MVC   SRVP4,SPACES                                                     
         MVI   SRVP4H+(FHIL-FHD),0                                              
         MVI   SRVP4H+(FHOL-FHD),0                                              
         OI    SRVP4H+(FHOI-FHD),FHOITR                                         
         B     EXITOK                                                           
*                                                                               
ASLC06   LA    R2,SRVMSGH          INVALID CURSOR POSITION                      
         USING FHD,R2                                                           
         ST    R2,FULL                                                          
         XR    RF,RF                                                            
ASLC08   CLC   FHAD,SCURSOR        LOOK FOR FIELD CONTAINING CURSOR             
         BH    ASLC10                                                           
         ST    R2,FULL                                                          
         ICM   RF,1,FHLN                                                        
         BZ    *+8                                                              
         BXH   R2,RF,ASLC08                                                     
         DC    H'0'                                                             
*                                                                               
ASLC10   L     R2,FULL             SET FIELD CONTAINING CURSOR                  
         ST    R2,FADRH                                                         
         XR    RF,RF                                                            
         ICM   RF,3,FHAD                                                        
         LH    R0,SCURSOR                                                       
         SR    R0,RF                                                            
         STC   R0,FERRDSP          SET DISP TO CURSOR                           
         MVI   FERN,8                                                           
         B     EXITL                                                            
         DROP  R2                                                               
*                                                                               
ALLSLCTX EQU   *                                                                
         TITLE '$DUMP - DSECT DISPLAYER'                                        
***********************************************************************         
* DSECT DISPLAYER                                                     *         
***********************************************************************         
DSCDISP  NTR1  BASE=(*,DSCDISPX),LABEL=*                                        
         MVI   RECOREL,0                                                        
*                                                                               
         GOTO1 ACALLOV,DMCB,(X'FD',SRVTAGH),0                                   
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
*                                                                               
         L     RF,ASVBLOCK                                                      
         MVI   NDTSSCRN-NDSAVED(RF),X'FD'                                       
*                                                                               
         BRAS  RE,DSSTART          SET STARTING POINT IN DSECT PHASE            
         BRAS  RE,DSDISP           DISPLAY THE DATA                             
         BNE   *+18                                                             
         MVC   ACURRPOS,ADSECT     FINISHED - SET TO START AT DSECT             
         BRAS  RE,DSCSRF                                                        
         L     R2,FULL                                                          
*                                                                               
         BRAS  RE,DSSNXT           SET ARGUMENTS FOR NEXT TIME IN               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE SETS STARTING POINT IN DSECT PHASE                          *         
***********************************************************************         
DSSTART  NTR1  ,                                                                
         L     RF,START            POINT TO DATA                                
         A     RF,DISPLACE                                                      
         ST    RF,FROM                                                          
         MVC   LEN,=AL2(8000)      MAX 800 BYTES IN A DISPLAY                   
         BRAS  RE,RDADR                                                         
         BNE   EXITL                                                            
*                                                                               
         L     R4,ATIA                                                          
         MVI   ELLEN,0             INITIALIZE REC/ELEMENT LENGTH                
         CLI   RECOREL,C'R'        IF POINTING TO RECORD                        
         BNE   *+10                                                             
         MVC   ELLEN,DATADISP      SET DUMMY LENGTH                             
         CLI   RECOREL,C'E'        ELSE IF POINTING TO ELEMENT                  
         BNE   *+10                                                             
         MVC   ELLEN,1(R4)         SET ELEMENT LENGTH                           
         BRAS  RE,LOOKUP           LOOK UP STARTING LABEL                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DISPLAY THE RECORD                                               
***********************************************************************         
DSDISP   NTR1  ,                                                                
         MVC   DMPL1(L'DSHEAD1),DSHEAD1                                         
         OI    DMPL1H+(FHAT-FHD),FHATHI                                         
         LA    R3,DMPL2H           R3 = A(FIRST FIELD ON SCREEN)                
         USING FHD,R3                                                           
         USING LINED,FHDA                                                       
*                                                                               
DSDS02   L     R2,ACURRPOS         R2 = A(LOCATION IN DSECT PHASE)              
         USING SRFADSEC,R2                                                      
*                                                                               
DSDS04   L     R4,ATIA             SET R4 = A(DATA)                             
         AH    R4,SRFADDSP                                                      
         LA    R1,DMPLAST                                                       
         CLI   FHD,0               ENSURE NOT PAST END OF SCREEN                
         BE    EXITL               RETURN CC NE - MORE TO COME                  
*                                                                               
         BRAS  RE,DSSVDTA          SAVE RELEVENT DATA                           
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   DSDS06                                                           
         GOTO1 AHEXOUT,DMCB,SRFADDSP,LINDISP,2,0 DISPLAY DISPLACEMENT           
*                                                                               
DSDS06   MVC   LINLBL,SRFADLBL     DISPLAY LABEL                                
*                                                                               
         LA    RF,DSDDEFT          DISPLAY DEFINITION                           
DSDS08   CLI   0(RF),EOTB                                                       
         BE    DSDS10                                                           
         CLC   SRFADDEF,0(RF)                                                   
         BE    DSDS10                                                           
         AHI   RF,L'DSDDEFT                                                     
         B     DSDS08                                                           
*                                                                               
DSDS10   MVC   LINDEF,1(RF)                                                     
*                                                                               
         XR    R1,R1               DISPLAY TYPE                                 
         ICM   R1,1,SRFADTYL                                                    
         BZ    DSDS12                                                           
         BCTR  R1,0                                                             
         CHI   R1,L'LINTYPE-1                                                   
         BNH   *+8                                                              
         LHI   R1,L'LINTYPE-1                                                   
         MVC   LINTYPE(0),SRFADTY                                               
         EX    R1,*-6                                                           
*                                                                               
DSDS12   CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   *+12                                                             
         BRAS  RE,DSDDATA          DISPLAY DATA                                 
         B     *+8                                                              
         BRAS  RE,DISCMNT          ELSE DISPLAY COMMENT                         
         XR    R0,R0                                                            
         IC    R0,FHLN                                                          
         AR    R3,R0                                                            
*                                                                               
DSDS14   BRAS  RE,DSNSRF           BUMP TO NEXT RECORD IN PHASE                 
         BNE   EXITOK                                                           
         L     R2,FULL                                                          
*                                                                               
         CLI   SRFADDEF,SRFQDSCT   IF NOT STARTING NEW DSECT                    
         BNE   DSDS04              DISPLAY NEXT FIELD IN DSECT                  
*                                                                               
         CLI   RECOREL,0           IF NOT POINTING TO REC OR EL, DONE           
         BE    EXITOK                                                           
*                                                                               
         L     R4,START            ELSE POINT TO START OF DATA                  
         XR    R1,R1                                                            
         IC    R1,ELLEN            BUMP TO FIRST/NEXT ELEMENT                   
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         ST    R4,START            SAVE A(BEGINNING OF ELEMENT)                 
*                                                                               
         MVC   ELLEN,1(R4)         SAVE ITS LENGTH                              
         MVI   RECOREL,C'E'        SET NOW POINTING TO ELEMENT                  
*                                                                               
         A     R1,START            BUMP STARTING ADDRESS FOR DISPLAY            
         ST    R1,START                                                         
         BRAS  RE,LOOKUP           GET DSECT ENTRY FOR NEW CODE                 
         B     DSDS02              LOOP                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT                                                     *         
***********************************************************************         
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
         USING LINED,R3            R3 = A(CURRENT DISPLAY LINE)                 
*                                                                               
DISCMNT  XR    R1,R1               DISPLAY COMMENT                              
         IC    R1,SRFADTYL         BUMP PAST TYPE                               
         LA    RF,SRFADTY(R1)                                                   
         ICM   R1,1,0(RF)            L'COMMENT                                  
         BZR   RE                                                               
         BCTR  R1,0                                                             
         CHI   R1,L'LINDATA-1                                                   
         BNH   *+8                                                              
         LHI   R1,L'LINDATA-1                                                   
         MVC   LINDATA(0),1(RF)    COMMENT IS 1 PAST L'COMMENT                  
         EX    R1,*-6                                                           
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATA                                                        *         
***********************************************************************         
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
         USING LINED,R3            R3 = A(CURRENT DISPLAY LINE)                 
*                                                                               
DSDDATA  NTR1  ,                                                                
         BRAS  RE,GETFLEN          GET L'FIELD                                  
         XR    RF,RF                                                            
         ICM   RF,3,FIELDLEN                                                    
         BZ    EXITOK              DON'T BOTHER IF LENGTH IS ZERO               
*                                                                               
         CLI   SRFADTY,C'C'        TEST FOR CHARACTER                           
         BE    *+14                                                             
         CLC   =C'0C',SRFADTY                                                   
         BNE   DSDD02                                                           
*                                                                               
         BCTR  RF,0                                                             
         CHI   RF,L'LINDATA-1      ENSURE LE MAX LENGTH OF FIELD                
         BNH   *+8                                                              
         LHI   RF,L'LINDATA-1                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R4),DISPCHRS    TEST IF ALL DISPLAYABLE CHARS.               
*                                                                               
         L     R2,ACURRPOS         (TRT CREAMS R2)                              
         BE    *+12                                                             
         AHI   RF,1                IT'S NOT - RESTORE CORRECT LENGTH            
         B     DSDD02              DISPLAY AS HEX DATA                          
*                                                                               
         MVC   LINDATA(0),0(R4)    DISPLAY CHARACTER DATA                       
         EX    RF,*-6                                                           
         B     EXITOK                                                           
*                                                                               
DSDD02   CHI   RF,(L'LINDATA/2)    ENSURE NOT GT MAX DISPLAYABLE                
         BNH   *+8                                                              
         LHI   RF,(L'LINDATA/2)                                                 
         GOTO1 AHEXOUT,DMCB,(R4),LINDATA,(RF),0 DISPLAY DATA IN HEX.            
*                                                                               
         CLI   SRFADTY,C'F'        TEST FOR FULLWORD                            
         BNE   DSDD06                                                           
         LA    RE,LINDATA+9                                                     
         CLI   DSSYS,0             IF SYSTEM IS FACPAK EDIT W/O DEC             
         BNE   DSDD04                                                           
         EDIT  (4,0(R4)),(12,(RE)),FLOAT=-                                      
         B     EXITOK                                                           
*                                                                               
DSDD04   EDIT  (4,0(R4)),(12,(RE)),2,FLOAT=-                                    
         B     EXITOK                                                           
*                                                                               
DSDD06   CLI   SRFADTY,C'H'        TEST FOR HALFWORD                            
         BNE   EXITOK                                                           
         LA    RE,LINDATA+9                                                     
         EDIT  (2,0(R4)),(12,(RE)),FLOAT=-                                      
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LOOK UP CODE OR LABEL IN DSECT PHASE                                *         
* NTRY: LLABEL <> 0 USE LABEL FIELD                                   *         
*              =  0 THEN R4 = A(CODE)                                 *         
***********************************************************************         
LOOKUP   NTR1  ,                                                                
         CLI   LLABEL,0            IF PROCESSING CODE                           
         BNE   DSLU02                                                           
         GOTO1 AHEXOUT,DMCB,(R4),HALF,1,0 CONVERT CODE TO CHARACTER             
*                                                                               
DSLU02   XR    R1,R1                                                            
         ICM   R1,1,DSSYS          LOAD FIRST PHASE FOR SYSTEM                  
         SLL   R1,4                                                             
         BNZ   *+8                                                              
         LHI   R1,1                                                             
         STC   R1,OVERLAY                                                       
*                                                                               
         BRAS  RE,DSLOAD                                                        
         L     R2,APHASE           R2 = A(CURRENT POSITION IN PHASE)            
         USING SRFADSEC,R2                                                      
         BRAS  RE,DSFSRF           BUMP TO FIRST RECORD                         
         L     R2,FULL                                                          
*                                                                               
DSLU04   BRAS  RE,DSSVDTA          SAVE RELEVENT DATA FOR PHASE RECORD          
         CLI   LLABEL,0            PROCESSING LABEL?                            
         BE    DSLU06              NO                                           
         XR    RF,RF                                                            
         IC    RF,LLABEL           MATCH ON L'INPUT                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   DSLU10                                                           
         CLC   SRFADLBL(0),LABEL                                                
         B     DSLU12                                                           
*                                                                               
DSLU06   TM    LBLSTAT,LBLQREC     IF THIS IS EQUATE FOR A RECORD               
         BZ    *+12                                                             
         CLI   RECOREL,C'R'        AND WE'RE POINTING TO A RECORD               
         BE    DSLU08                                                           
         TM    LBLSTAT,LBLQEL      OR IF THIS IS EQUATE FOR AN ELEMENT          
         BZ    DSLU10                                                           
         CLI   RECOREL,C'E'        AND WE'RE POINTING TO AN ELEMENT             
         BNE   DSLU10                                                           
*                                                                               
DSLU08   CLC   SRFADTY+2(2),HALF   MATCH ON CODE                                
         BNE   DSLU10                                                           
         MVC   ACURRPOS,ADSECT     SET TO START WITH DSECT                      
         B     DSLU12              FOUND IT - RETURN                            
*                                                                               
DSLU10   BRAS  RE,DSNSRF           BUMP TO NEXT RECORD IN PHASE                 
         BE    *+12                                                             
         MVI   FERN,1              ERROR - INVALID LABEL                        
         B     EXITL                                                            
*                                                                               
         L     R2,FULL                                                          
         B     DSLU04              LOOP                                         
*                                                                               
DSLU12   BRAS  RE,DSCSRF           INSURE CORRECT PHASE IS LOADED               
         MVI   LLABEL,0            CLEAR LABEL LENGTH FOR NEXT TIME IN          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE RETURNS LENGTH OF CURRENT FIELD                             *         
* NTRY: R2     = A(CURRENT FIELD ENTRY)                               *         
***********************************************************************         
         USING SRFADSEC,R2                                                      
GETFLEN  NTR1  ,                                                                
         XR    R0,R0                                                            
         CLI   SRFADTYL,1          TYPE LENGTH IS ONE?                          
         BNE   GETF02              NO                                           
*                                                                               
         LHI   R0,4                                                             
         STH   R0,FIELDLEN                                                      
         CLI   SRFADTY,C'F'        FULLWORD                                     
         BE    GETF10                                                           
         CLI   SRFADTY,C'A'        ADDRESS                                      
         BE    GETF10                                                           
         CLI   SRFADTY,C'V'        EXTERNAL ADDRESS                             
         BE    GETF10                                                           
         LHI   R0,2                                                             
         STH   R0,FIELDLEN                                                      
         CLI   SRFADTY,C'H'        HALFWORD                                     
         BE    GETF10                                                           
         LHI   R0,1                                                             
         STH   R0,FIELDLEN                                                      
         CLI   SRFADTY,C'C'        CHAR                                         
         BE    GETF10                                                           
         CLI   SRFADTY,C'X'        HEX                                          
         BE    GETF10                                                           
*                                                                               
GETF02   LH    R3,SRFADDSP         SAVE DISP. TO CURRENT FIELD                  
         BRAS  RE,DSNSRF           BUMP TO NEXT RECORD IN PHASE                 
         BNE   GETF04                                                           
         L     R2,FULL                                                          
*                                                                               
         CLI   SRFADDEF,SRFQDS     FOR DS                                       
         BE    *+12                                                             
         CLI   SRFADDEF,SRFQDC     AND DC                                       
         BNE   GETF04                                                           
         LH    R0,SRFADDSP         DISP. TO NEXT FIELD                          
         SR    R0,R3               LESS DISP. TO CURRENT FIELD                  
         STH   R0,FIELDLEN                                                      
         B     GETF10                                                           
*                                                                               
GETF04   BRAS  RE,DSCSRF           RESTORE A(CURRENT ENTRY)                     
         L     R2,FULL                                                          
         XR    R1,R1                                                            
         IC    R1,SRFADTYL         ATTEMPT TO EXTRACT LENGTH FROM TYPE          
         AHI   R1,-3                                                            
         BM    GETF06                                                           
         EX    R1,GETFMVC          ASSUME TYPE IS CL???                         
         EX    R1,GETFOC           INSURE IT'S NUMERIC                          
         EX    R1,GETFCLC                                                       
         BNE   GETF06                                                           
         EX    R1,GETFPCK                                                       
         CVB   RF,DUB                                                           
         STH   RF,FIELDLEN         SAVE IT                                      
         B     GETF10                                                           
*                                                                               
GETFMVC  MVC   WORK(0),SRFADTY+2                                                
GETFCLC  CLC   WORK(0),SRFADTY+2                                                
GETFOC   OC    WORK(0),ZEROS                                                    
GETFPCK  PACK  DUB,WORK(0)                                                      
*                                                                               
GETF06   BRAS  RE,DSNSRF           BUMP TO NEXT RECORD IN PHASE AGAIN           
         BNE   GETF10                                                           
         L     R2,FULL                                                          
         CLI   SRFADDEF,SRFQDSCT   IF IT'S A DSECT                              
         BE    GETF08                                                           
         CLI   SRFADDEF,SRFQEQU    OR IT'S AN EQUATE                            
         BNE   GETF10                                                           
         CLI   SRFADTYL,7          AND TYPE LENGTH IS 7 L'*-XX??D               
         BNE   GETF10                                                           
         MVC   WORK(2),=C'*-'      BUILD EOL TYPE FOR THIS EL.                  
         MVC   WORK+2(4),SRFADLBL                                               
         MVI   WORK+6,C'D'         NOW HAVE '*-XX??D'                           
         CLC   SRFADTY(7),WORK     IF IT MATCHES THEN REACHED END OF EL         
         BNE   GETF10                                                           
*                                                                               
GETF08   BRAS  RE,DSCSRF           RESTORE A(CURRENT ENTRY)                     
         L     R2,FULL                                                          
         ZIC   R0,ELLEN            L'ELEMENT                                    
         SH    R0,SRFADDSP         - DISP. TO THIS FIELD                        
         BM    *+8                                                              
         STH   R0,FIELDLEN         IS L'THIS FIELD                              
*                                                                               
GETF10   BRAS  RE,DSCSRF           INSURE WE'RE RESTORED CURRENT PTR.           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET FIRST DSECT PHASE RECORD                                        *         
* NTRY: R2     = A(CURRENT ENTRY)                                     *         
* EXIT: FULL   = A(NEXT ENTRY)                                        *         
***********************************************************************         
DSFSRF   NTR1    ,                                                              
         B     DSNXT02                                                          
***********************************************************************         
* GET NEXT DSECT PHASE RECORD                                         *         
* NTRY: R2     = A(CURRENT ENTRY)                                     *         
* EXIT: FULL   = A(NEXT ENTRY)                                        *         
***********************************************************************         
         USING SRFADSEC,R2                                                      
DSNSRF   NTR1  ,                                                                
         XR    R1,R1                                                            
         IC    R1,SRFADTYL                                                      
         LA    R2,SRFADTY(R1)      BUMP PAST L'TYPE, TYPE                       
         IC    R1,0(R2)                                                         
         LA    R2,1(R1,R2)         BUMP PAST L'COMMENT, COMMENT                 
*                                                                               
DSNXT02  CLC   SRFADDSP,=AL2(SRFQSPCL)                                          
         BE    DSNXT04             SPECIAL RECORD                               
         ST    R2,FULL                                                          
         B     EXITOK                                                           
*                                                                               
DSNXT04  CLI   SRFSPCL,SRFQEOF     REACHED END OF FILE?                         
         BE    EXITL                                                            
*                                                                               
         CLI   SRFSPCL,SRFQMEM     START OF MEMBER?                             
         BNE   *+12                                                             
         LA    R2,SRFMEMB+L'SRFMEMB  BUMP TO NEXT RECORD                        
         B     DSNXT02                                                          
*                                                                               
         CLI   SRFSPCL,SRFQEOP     MUST BE END OF PHASE                         
         BE    *+6                                                              
         DC    H'0'                UNRECOGNIZED SPECIAL RECORD TYPE             
*                                                                               
         XR    R0,R0                                                            
         IC    R0,OVERLAY                                                       
         AHI   R0,1                BUMP OVERLAY NUMBER                          
         STC   R0,OVERLAY                                                       
         BRAS  RE,DSLOAD           LOAD NEXT DSECT PHASE                        
         BNE   EXITL                                                            
         L     R2,APHASE                                                        
         B     DSNXT02             CONTINUE                                     
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RESTORES SAVED POSITION IN DSECT PHASE                              *         
* EXIT: FULL   = A(SAVED POSITION)                                    *         
***********************************************************************         
DSCSRF   NTR1  ,                                                                
         CLC   OVERLAY,ACURRPOS    TEST CORRECT PHASE IS LOADED                 
         BE    *+14                                                             
         MVC   OVERLAY,ACURRPOS    NO, SO LOAD IT                               
         BRAS  RE,DSLOAD                                                        
         L     R2,ACURRPOS         RESTORE A(DSECT RECORD)                      
         ST    R2,FULL                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOAD A DSECT PHASE                                                  *         
* NTRY: OVERLAY = OVERLAY NUMBER                                      *         
***********************************************************************         
DSLOAD   NTR1  ,                                                                
         L     R0,=V(DUMMY)        SET A(WHERE TO LOAD DSECT PHASES)            
         A     R0,RELO                                                          
         ST    R0,APHASE                                                        
         MVC   DMCB+4(3),=X'D9015E'  SET TO LOAD T15E PHASE                     
         MVC   DMCB+7(1),OVERLAY     SET OVERLAY                                
         GOTO1 ACALLOV,DMCB,APHASE,,0                                           
         CLI   4(R1),255                                                        
         BNE   EXITOK                                                           
         MVI   FERN,255                                                         
         LA    RF,WORK                                                          
         STCM  RF,7,FERNA                                                       
         MVC   WORK(64),SPACES                                                  
         MVC   WORK(L'DSERR4),DSERR4                                            
         GOTO1 AHEXOUT,DMCB,OVERLAY,WORK+L'DSERR4,L'OVERLAY,0                   
         B     EXITL                                                            
*                                                                               
DSERR4   DC    C'Can''t find dsect phase T15E'                                  
         EJECT                                                                  
***********************************************************************         
* SAVE DATA FOR A DSECT PHASE RECORD                                  *         
* NTRY: R2     = A(CURRENT DSECT PHASE RECORD)                        *         
***********************************************************************         
         USING SRFADSEC,R2         R2 = A(CURRENT DSECT PHASE RECORD)           
DSSVDTA  NTR1  ,                                                                
         ST    R2,ACURRPOS         SAVE A(CURRENT POSITION)                     
         MVC   ACURRPOS(1),OVERLAY SAVE CURRENT OVERLAY NUMBER IN HOB           
*                                                                               
         CLI   SRFADDEF,SRFQDSCT   IF THIS IS DSECT RECORD                      
         BNE   *+10                                                             
         MVC   ADSECT,ACURRPOS     SAVE PHASE/ADDRESS                           
*                                                                               
         NI    LBLSTAT,255-(LBLQREC+LBLQEL)                                     
         CLI   SRFADDEF,SRFQEQU    TEST THIS IS EQUATE TYPE                     
         BNE   EXITOK                                                           
*                                                                               
         CLI   DSSYSCHR,C'A'       FOR ACC                                      
         BNE   *+14                                                             
         CLC   SRFADLBL+4(4),=C'TYPQ'  RECORD EQUATES END WITH 'TYPQ'           
         BE    DSSV02                                                           
*                                                                               
         CLI   DSSYSCHR,C'T'       FOR TALENT                                   
         BNE   DSSV04                                                           
         CLC   SRFADLBL(2),=C'TL'  RECORD EQUATES START WITH 'TL'               
         BNE   DSSV04                                                           
         CLC   SRFADLBL+4(3),=C'CDQ'  AND END WITH 'CDQ'                        
         BNE   DSSV04                                                           
*                                                                               
DSSV02   OI    LBLSTAT,LBLQREC     SET THIS IS EQUATE FOR RECORD                
         B     EXITOK                                                           
*                                                                               
DSSV04   CLC   SRFADLBL+3(3),=C'ELQ'  IF LABEL ENDS WITH 'ELQ'                  
         BE    *+14                                                             
         CLC   SRFADLBL+4(3),=C'ELQ'                                            
         BNE   EXITOK                                                           
         OI    LBLSTAT,LBLQEL      SET THIS IS EQUATE FOR ELEMENT               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP ARGUMENTS FOR NEXT TIME IN                                   *         
***********************************************************************         
DSSNXT   NTR1  ,                                                                
         MVC   SRVP3,SPACES        ENSURE LAST TWO FIELDS CLEARED               
         MVC   SRVP4,SPACES                                                     
         LA    RF,SRVP3H                                                        
         ST    RF,FADRH                                                         
*                                                                               
         L     R2,ACURRPOS         R2 = A(LAST LABEL DISPLAYED)                 
         USING SRFADSEC,R2                                                      
         C     R2,ADSECT           UNLESS POINTING TO DSECT                     
         BE    *+12                                                             
DSSN02   BRAS  RE,DSNSRF           GET NEXT                                     
         BNE   DSSN04                                                           
         CLC   SRFADLBL,SPACES     IF DSECT PHASE RECORD HAS NO LABEL           
         BE    DSSN02              KEEP ON LOOKING                              
*                                                                               
DSSN04   XR    RF,RF               SET LABEL IN FIELD                           
         IC    RF,DSPTOLBL                                                      
         LA    RF,SRVP1(RF)                                                     
         MVC   0(L'SRFADLBL,RF),SRFADLBL                                        
         AHI   RF,L'SRFADLBL-1                                                  
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         CLI   RECOREL,0           RECORD/ELEMENT SWITCH SET?                   
         BE    *+14                                                             
         MVI   1(RF),C','          YES - DISPLAY IT                             
         MVC   2(1,RF),RECOREL                                                  
*                                                                               
         MVI   HDRN,17                                                          
         CLC   ACURRPOS,ADSECT     IF NOT STARTING AT DSECT                     
         BNE   *+8                                                              
         MVI   HDRN,18                                                          
         OC    AKEYWORD,AKEYWORD   KEYWORDS STAY ON SCREEN                      
         BNZ   EXITOK                                                           
*                                                                               
         MVC   SRVP2,SPACES                                                     
         L     R0,START                                                         
         A     R0,DISPLACE                                                      
         ST    R0,FULL                                                          
         GOTO1 AHEXOUT,DMCB,FULL,SRVP2,4,0  SET STARTING ADDRESS                
*NOP     CLC   SRVP2(2),ZEROS                                                   
*        BNE   *+16                                                             
*        MVC   SRVP2(6),SRVP2+2                                                 
*NOP     MVC   SRVP2+6(2),SPACES                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAYABLE CHARACTERS FOR TRT IN DSECT DISPLAY                     *         
***********************************************************************         
DISPCHRS DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'006F6F6F6F6F6F6F6F6F6F006F00006F'  40-4F                    
         DC    XL16'006F6F6F6F6F6F6F6F6F6F0000006F6F'  50-5F                    
         DC    XL16'00006F6F6F6F6F6F6F6F6F00006F6F00'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F0000000000'  70-7F                    
         DC    XL16'6F000000000000000000006F6F6F6F6F'  80-8F                    
         DC    XL16'6F0000000000000000006F6F6F6F6F6F'  90-9F                    
         DC    XL16'6F6F00000000000000006F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'6F0000000000000000006F6F6F6F6F6F'  C0-CF                    
         DC    XL16'6F0000000000000000006F6F6F6F6F6F'  D0-DF                    
         DC    XL16'6F6F00000000000000006F6F6F6F6F6F'  E0-EF                    
         DC    XL16'000000000000000000006F6F6F6F6F6F'  F0-FF                    
*                                                                               
DSCDISPX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* MERGE TBUFF INTO SCREEN JUST LOADED                                 *         
* NOTE: DESTROYS TIA AND CONTENTS OF WORK                             *         
***********************************************************************         
SCRMRGE  NTR1  BASE=(*,SCRMRGEX),LABEL=*                                        
         MVC   WORK+(L'SRVP1*0)(L'SRVP1),SRVP1                                  
         MVC   WORK+(L'SRVP1*1)(L'SRVP2),SRVP2                                  
         MVC   WORK+(L'SRVP1*2)(L'SRVP3),SRVP3                                  
         MVC   WORK+(L'SRVP1*3)(L'SRVP4),SRVP4                                  
*                                                                               
         L     R0,ATIA             CLEAR TIA                                    
         LHI   R1,4096                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,AUTL                                                          
         ICM   RE,15,TBUFF-UTLD(RE)                                             
         AHI   RE,-8                                                            
         LH    RF,TBHMSGL-TBHD(RE)                                              
         AHI   RE,8                                                             
         AHI   RE,7                FOR 3270 TERMINALS                           
         AHI   RF,-7                                                            
*                                                                               
         L     R4,ATIA                                                          
         USING TWAD,R4                                                          
         STH   RF,4(R4)                                                         
         LA    R0,64(R4)                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ALNKTAB                                                       
         ICM   RF,15,LTI3270-LNKTABD(RF)                                        
         GOTO1 (RF),DMCB,ATIA,ATWA,AUTL                                         
*                                                                               
         MVC   SRVP1,WORK+(L'SRVP1*0)                                           
         MVC   SRVP2,WORK+(L'SRVP1*1)                                           
         MVC   SRVP3,WORK+(L'SRVP1*2)                                           
         MVC   SRVP4,WORK+(L'SRVP1*3)                                           
         B     EXITOK                                                           
*                                                                               
SCRMRGEX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* STRIPS LEADING ZEROS FROM 8 BYTE VALUE IN DUB                       *         
* NTRY: DUB    = 8 BYTE FIELD RIGHT JUSTIFIED                         *         
* EXIT: DUB    = 8 BYTE FIELD LEFT JUSTIFIED                          *         
*       BYTE   = LENGTH OF DATA IN DUB                                *         
***********************************************************************         
         USING FHD,R2                                                           
STRIP0   NTR1  BASE=(*,STRIP0X),LABEL=*                                         
         LHI   R0,7                R0 = SIGNIFICANT LENGTH OF DATA              
*                                                                               
STP02    CLI   DUB,C'0'            ANOTHER ZERO TO STRIP?                       
         BNE   STP04               NO                                           
         MVC   DUB(L'DUB-1),DUB+1                                               
         MVI   DUB+L'DUB-1,C' '                                                 
         BCT   R0,STP02                                                         
*                                                                               
STP04    AHI   R0,1                                                             
         STC   R0,BYTE                                                          
         B     EXITOK                                                           
*                                                                               
STRIP0X  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* CONVERTS EBCDIC HEX INPUT FIELD INTO FULLWORD                       *         
* NTRY: R2     = A(FIELD HEADER)                                      *         
* EXIT: FULL   = FULLWORD HEX INPUT                                   *         
***********************************************************************         
         USING FHD,R2                                                           
MOVHEX   NTR1  BASE=(*,MOVHEXX),LABEL=*                                         
         XC    FULL,FULL                                                        
         CLI   FHIL,8                                                           
         BNH   *+12                                                             
         MVI   FERN,10             TOO MUCH INPUT IN FIELD                      
         B     EXITL                                                            
         CLI   FHIL,0                                                           
         BNE   *+12                                                             
         MVI   FERN,1              NO INPUT IN FIELD                            
         B     EXITL                                                            
*                                                                               
         TM    FHII,FHIIHE         FIELD IS HEX?                                
         BO    MVH06               YES                                          
*                                                                               
         XR    R0,R0               CHECK IF BIT WAS NOT SET (FIXED FLD)         
         IC    R0,FHIL                                                          
         XR    RF,RF                                                            
         LA    R1,FHDA                                                          
*                                                                               
MVH02    CLI   0(R1),C'A'          FIND FIRST INVALID CHARACTER                 
         BL    MVH08                                                            
         CLI   0(R1),C'F'                                                       
         BNH   MVH04                                                            
         CLI   0(R1),C'0'                                                       
         BL    MVH08                                                            
         CLI   0(R1),C'9'                                                       
         BH    MVH08                                                            
*                                                                               
MVH04    AHI   RF,1                NEXT IN FIELD                                
         AHI   R1,1                                                             
         BCT   R0,MVH02                                                         
*                                                                               
MVH06    MVC   DUB,ZEROS           ZERO FILL DUB                                
         XR    R1,R1                                                            
         ICM   R1,1,FHIL           LENGTH OF HEX                                
         LHI   RF,8                                                             
         SR    RF,R1                                                            
         LA    RF,DUB(RF)          RIGHT JUSTFY INTO DUB                        
         BCTR  R1,0                                                             
         MVC   0(0,RF),FHDA                                                     
         EX    R1,*-6                                                           
*                                                                               
         GOTO1 AHEXIN,DMCB,DUB,FULL,8                                           
         OC    12(4,R1),12(R1)                                                  
         BNZ   EXITOK              VALID HEX INPUT                              
         XR    RF,RF                                                            
*                                                                               
MVH08    MVI   FERN,11             FIELD IS NOT VALID HEX                       
         STC   RF,FERRDSP                                                       
         B     EXITL                                                            
*                                                                               
MOVHEXX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* CONVERTS EBCDIC DEC INPUT FIELD INTO FULLWORD                       *         
* NTRY: R2     = A(FIELD HEADER)                                      *         
* EXIT: FULL   = FULLWORD HEX INPUT                                   *         
***********************************************************************         
         USING FHD,R2                                                           
MOVDEC   NTR1  BASE=(*,MOVDECX),LABEL=*                                         
         XC    FULL,FULL                                                        
         CLI   FHIL,8                                                           
         BNH   *+12                                                             
         MVI   FERN,10             TOO MUCH INPUT IN FIELD                      
         B     EXITL                                                            
*                                                                               
         XR    R0,R0               MAKE SURE FIELD IS VALID                     
         IC    R0,FHIL                                                          
         XR    RF,RF                                                            
         LA    R1,FHDA                                                          
MVD02    CLI   0(R1),C'0'                                                       
         BL    MVD04                                                            
         CLI   0(R1),C'9'                                                       
         BH    MVD04                                                            
         AHI   RF,1                NEXT IN FIELD                                
         AHI   R1,1                                                             
         BCT   R0,MVD02                                                         
*                                                                               
         MVC   DUB1,ZEROS          ZERO FILL DUB1                               
         XR    R1,R1                                                            
         ICM   R1,1,FHIL           LENGTH OF NUMERIC DATA                       
         LHI   RF,8                                                             
         SR    RF,R1                                                            
         LA    RF,DUB1(RF)         RIGHT JUSTFY INTO DUB1                       
         BCTR  R1,0                                                             
         MVC   0(0,RF),FHDA                                                     
         EX    R1,*-6                                                           
*                                                                               
         PACK  DUB,DUB1            PACK AND CONVERT NUMBER                      
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         B     EXITOK                                                           
*                                                                               
MVD04    MVI   FERN,6              FIELD IS NOT VALID NUMERIC                   
         STC   RF,FERRDSP                                                       
         B     EXITL                                                            
*                                                                               
MOVDECX  EQU   *                                                                
         TITLE '$DUMP - COMMON STORAGE'                                         
***********************************************************************         
* START OF COMMONLY ADDRESSIBLE STORAGE                               *         
***********************************************************************         
COMMON   DS    0D                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         ORG   *-2                                                              
         BSM   0,RE                                                             
*                                                                               
XMOD     CLI   FERN,0              ANY ERROR MESSAGE SET?                       
         BNE   *+12                                                             
         BRAS  RE,DISOK                                                         
         B     *+8                                                              
         BRAS  RE,DISERR                                                        
         BRAS  RE,WTTWAB                                                        
         L     RD,SAVERD                                                        
         XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND EQUATES                                                *         
***********************************************************************         
EMSGL    EQU   45                                                               
IMSGL    EQU   45                                                               
EOTB     EQU   X'FF'                                                            
*                                                                               
RSHIFT   EQU   13                  SHIFT VALUE (8192)                           
BLKFCTR  EQU   4                   2K RECS/BLOCK                                
RLEN     EQU   BLKFCTR*2048        BLOCK FACTOR                                 
*                                                                               
TICNTRY# EQU   60                  COUNT OF ENTRIES IN TICTRACE TABLE           
TICNTRYL EQU   72                  LENGTH OF AN ENTRY IN TICTRACE TABLE         
*                                                                               
ALLSCMAX EQU   10                                                               
ALLSCLEN EQU   L'DMPL3                                                          
ALLSCLNE EQU   (DMPLLH-DMPL3H)/(ALLSCLEN+L'DMPL3H)                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
ARZERO   DC    16F'0'                                                           
TU1SEC   DC    A(38400)                                                         
*                                                                               
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
*                                                                               
ALL4U    DC    CL4'ALL '                                                        
*                                                                               
NDUMP    DC    CL8'NDUMPAJA'                                                    
SUMMDSP  DC    CL30'Dump file summary information '                             
FLTERR   DC    CL30'Invalid filter value          '                             
STOPMSG  DC    CL20'- dumps are stopped'                                        
$ND      DC    CL4'$ND'                                                         
LTODAY   DC    CL6'Today'                                                       
DSPAERR1 DC    CL40'Has a disk error - unable to display'                       
ZEROS    DC    16C'0'                                                           
EFFS     DC    16X'FF'                                                          
SPACES   DC    80C' '                                                           
STARS    DC    80C'*'                                                           
*                                                                               
DOTFILL  DC    40C'.'              FILL FOR ALL SCREENS                         
HYPHENS  DC    40C'-'                                                           
*                                                                               
COUNTH   DC    C'1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 '                              
COUNTD   DC    C'12345678901234561234567890123456'                              
DSHEAD1  DC    C'Disp Label    Def Type               Value'                    
*                                                                               
ALL1HMSG DC    CL16'Dump Number     '                                           
ALL2HMSG DC    CL16'Filters         '                                           
ALL3HMSG DC    CL16'                '                                           
ALL4HMSG DC    CL16'Start Number    '                                           
*                                                                               
DIS1HMSG DC    CL16'Dump Number     '                                           
DIS2HMSG DC    CL16'Option 1        '                                           
DIS3HMSG DC    CL16'Option 2        '                                           
DIS4HMSG DC    CL16'Option 3        '                                           
*                                                                               
ALLL1    DC    C'No Fac Dp S  Who Date  Time     Terminal Sys  Prg '            
         DC    C'Routine  Oset R PSW-6 Code              '                      
ALLL2    DC    C'-- --- -- -  --- ----- -------- -------- ---- --- '            
         DC    C'-------- ---- - ----------------        '                      
*                                                                               
REG2NUM  DC    C'0',X'01',C'1',X'02',C'2',X'03',C'3',X'04'                      
         DC    C'4',X'05',C'5',X'06',C'6',X'07',C'7',X'08'                      
         DC    C'8',X'09',C'9',X'0A',C'A',X'0B',C'B',X'0C'                      
         DC    C'C',X'0D',C'D',X'0E',C'E',X'0F',C'F',X'10'                      
         DC    AL1(EOTB)                                                        
*                                                                               
DSDDEFT  DS    0XL6                                                             
         DC    AL1(SRFQDS),CL5'DS   '                                           
         DC    AL1(SRFQDC),CL5'DC   '                                           
         DC    AL1(SRFQEQU),CL5'EQU  '                                          
         DC    AL1(SRFQORG),CL5'ORG  '                                          
         DC    AL1(SRFQDSCT),CL5'DSECT'                                         
         DC    AL1(EOTB),CL5'?????'                                             
*                                                                               
DELETED  DC    CL7'Deleted'                                                     
*                                                                               
BFADDR   DC    C'Buffer address='                                               
BFLEN    DC    C',Buffer length='                                               
BFAVAIL  DC    C',Available='                                                   
*                                                                               
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLES                                                    *         
***********************************************************************         
*        DISPLAY TRANSLATION FOR OUTPUT SCREENS                                 
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4C4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D5E5F'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D6E6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B7C7D7E7F'     70-7F                    
         DC    X'4B8182838485868788894B4B4B4B4B4B'     80-8F                    
         DC    X'4B9192939495969798994B4B4B4B4B4B'     90-9F                    
         DC    X'4B4BA2A3A4A5A6A7A8A94B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
*        DECIMAL TO EBCDIC TRANSLATION TABLE FOR STEREO DATA BUFFER             
*                                                                               
DECTRAN  DC    XL16'00000000000000000000000000000000'  00-0F                    
         DC    XL16'00000000000000000000000000000000'  10-1F                    
         DC    XL16'00000000000000000000000000000000'  20-2F                    
         DC    XL16'00000000000000000000000000000000'  30-3F                    
         DC    XL16'00000000000000000000004041424300'  40-4F                    
         DC    XL16'45000000000000000000464748494A00'  50-5F                    
         DC    XL16'4C4D0000000000000000004F50515253'  60-6F                    
         DC    XL16'000000000000000000545556573F444B'  70-7F                    
         DC    XL16'0025262728292A2B2C2D000000000000'  80-8F                    
         DC    XL16'002E2F30313233343536000000000000'  90-9F                    
         DC    XL16'00003738393A3B3C3D3E000000000000'  A0-AF                    
         DC    XL16'00000000000000000000000000000000'  B0-BF                    
         DC    XL16'4E010203040506070809000000000000'  C0-CF                    
         DC    XL16'000A0B0C0D0E0F101112000000000000'  D0-DF                    
         DC    XL16'0000131415161718191A000000000000'  E0-EF                    
         DC    XL16'1B1C1D1E1F2021222324000000000000'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* SELECT COMMANDS TABLE                                               *         
***********************************************************************         
SELCMDS  DS    0F                                                               
         DC    C'A',AL3(0),AL4(SELDOA)                                          
         DC    CL40'Add address to saved list (24 bit)      '                   
         DC    CL24'                        '                                   
*                                                                               
         DC    C'B',AL3(0),AL4(SELDOB)                                          
         DC    CL40'Add address to saved list (24 bit) then '                   
         DC    CL24'go to address           '                                   
*                                                                               
         DC    C'C',AL3(0),AL4(SELDOA)                                          
         DC    CL40'Add address to saved list (31 bit)      '                   
         DC    CL24'                        '                                   
*                                                                               
         DC    C'D',AL3(0),AL4(SELDOB)                                          
         DC    CL40'Add address to saved list (31 bit) then '                   
         DC    CL24'go to address           '                                   
*                                                                               
         DC    C'G',AL3(0),AL4(SELDOG)                                          
         DC    CL40'Go to address (24bit)                   '                   
         DC    CL24'                        '                                   
*                                                                               
         DC    C'P',AL3(0),AL4(SELDOP)                                          
         DC    CL40'Display address as parameter list (24 bi'                   
         DC    CL24't)                      '                                   
*                                                                               
         DC    C'S',AL3(0),AL4(SELDOA)                                          
         DC    CL40'Save address - same as "A" command      '                   
         DC    CL24'                        '                                   
*                                                                               
         DC    C'X',AL3(0),AL4(SELDOG)                                          
         DC    CL40'Go to address (31bit)                   '                   
         DC    CL24'                        '                                   
*                                                                               
         DC    C'?',AL3(0),AL4(SELDOG)                                          
         DC    CL40'Go to address (31bit)                   '                   
         DC    CL24'                        '                                   
         DC    AL1(EOTB)                                                        
*                                                                               
SELCMDSD DSECT                                                                  
SELCMD   DS    CL1                 INPUT TO SELECT FIELD                        
         DS    AL3                                                              
SELRTN   DS    AL4                 A(HANDLING ROUTINE)                          
SELHELP  DS    CL64                HELP TEXT                                    
SELCMDSL EQU   *-SELCMDSD                                                       
*                                                                               
DUMP     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* KEYWORD TABLE                                                       *         
***********************************************************************         
KEYTAB   DC    CL8'ACCFACS '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VSYSFAC3-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'ADRBUFF '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VADRBUFF-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'ADDAFT  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VADDAFT-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'ADRFILE '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VADRFILE-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'CALLOV  '                                                    
         DC    AL1(02,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VCALLOV-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'CCBLAST '                                                    
         DC    AL1(02,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBSVECB-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'CHAIN   '                                                    
         DC    AL1(03,05,KEYACHN),AL1(KEYSCMD+KEYSVP3),AL4(0)                   
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'CHKPT1  '                                                    
         DC    AL1(06,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VCHKPT1-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'CHKPT1X '                                                    
         DC    AL1(07,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VCHKPT1X-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'CHKPT2  '                                                    
         DC    AL1(06,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VCHKPT2-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'CHKPT2X '                                                    
         DC    AL1(07,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VCHKPT2X-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'CLSESYS '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VCLSESYS-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'COMFACS '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VSYSFAC0-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'DABACK  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDABACK-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'DACPUID '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDACPUID-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'DADDS   '                                                    
         DC    AL1(03,05,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDADDS-SYSFACD,0,0,0)                                        
*                                                                               
         DC    CL8'DADTF   '                                                    
         DC    AL1(03,05,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBDTFLK-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'DARPT   '                                                    
         DC    AL1(03,05,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDARPT-SYSFACD,0,0,0)                                        
*                                                                               
         DC    CL8'DATRNS  '                                                    
         DC    AL1(04,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDATRNS-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'DATAMGR '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    XL1'80',AL3(VDATAMGR-SYSFACD),AL4(0,0,0)                         
*                                                                               
         DC    CL8'DECBLST '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDECBLST-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'DMGR    '                                                    
         DC    AL1(04,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    XL1'80',AL3(VDATAMGR-SYSFACD),AL4(0,0,0)                         
*                                                                               
         DC    CL8'DMGRFLES'                                                    
         DC    AL1(05,08,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDMGRFLS-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'DMOD000 '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDMOD000-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'DMPFILE '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDMPFILE-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'DTFIOA  '                                                    
         DC    AL1(04,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VDTFIOA-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'DTFS    '                                                    
         DC    AL1(03,04,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBDTFS-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'EKEY    '                                                    
         DC    AL1(03,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VEKEY-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'ENQDEQ  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VENQDEQ-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'FILEBUFF'                                                    
         DC    AL1(03,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBFILES-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'FINDSYS '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VFINDSYS-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'FIXED   '                                                    
         DC    AL1(02,05,KEYAFIX),AL1(KEYSCMD+KEYSVP3),AL4(0)                   
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'FNDEOF  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VFNDEOF-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'GOTO    '                                                    
         DC    AL1(01,04,KEYAGO),AL1(KEYSCMD),AL4(0)                            
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'HOLE    '                                                    
         DC    AL1(01,04,00),AL1(KEYSTCB),AL4(AHOLE)                            
         DC    AL4(TCBUTL-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'ISCPUID '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VISCPUID-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'INDIRECT'                                                    
         DC    AL1(01,08,KEYAIND),AL1(KEYSCMD),AL4(0)                           
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'LCBUFFS '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VLCBUFFS-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'LCBUFFX '                                                    
         DC    AL1(07,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VLCBUFFX-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'LCWRITE '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VLCWRITE-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'LIST    '                                                    
         DC    AL1(01,04,KEYALST),AL1(KEYSCMD),AL4(0)                           
         DC    AL4(TCBTWA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'LOCKTAB '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VLOCKTAB-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'MAP     '                                                    
         DC    AL1(03,03,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBMAP-TCBD,0,0,0)                                           
*                                                                               
*&&UK*&& DC    CL8'MEDFACS '                                                    
*&&UK*&& DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
*&&UK*&& DC    AL4(VSYSFAC4-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'MINBUFF1'                                                    
         DC    AL1(04,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBMINIO-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'MINBUFF2'                                                    
         DC    AL1(04,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBMINI2-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'OPEN    '                                                    
         DC    AL1(04,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VOPEN-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'OPENIS  '                                                    
         DC    AL1(05,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VOPENIS-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'OPENSYS '                                                    
         DC    AL1(05,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VOPENSYS-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'OWNERTSK'                                                    
         DC    AL1(03,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBLOCK-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'PARAMS  '                                                    
         DC    AL1(01,06,00),AL1(KEYSCMD),AL4(PARMSHOW)                         
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'PGMSTRT '                                                    
         DC    AL1(05,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBPGMA-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'PGOTO   '                                                    
         DC    AL1(02,05,KEYAPGO),AL1(KEYSCMD),AL4(0)                           
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'PHASEMAP'                                                    
         DC    AL1(04,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBMAP-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'PHLIST  '                                                    
         DC    AL1(04,08,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VPHLIST-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'POWWOW  '                                                    
         DC    AL1(04,08,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VPOWWOW-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'PRGMS   '                                                    
         DC    AL1(04,05,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VPRGMS-SYSFACD,0,0,0)                                        
*                                                                               
         DC    CL8'PROCESS '                                                    
         DC    AL1(03,07,KEYAPROC),AL1(KEYSCMD+KEYSVP3),AL4(0)                  
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'PSW     '                                                    
         DC    AL1(03,03,KEYAPSW),AL1(KEYSCMD),AL4(0)                           
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'KNOWN   '                                                    
         DC    AL1(03,05,KEYAKNOW),AL1(KEYSCMD+KEYSVP3),AL4(0)                  
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'PRQ     '                                                    
         DC    AL1(03,03,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VPRQ-SYSFACD,0,0,0)                                          
*                                                                               
*&&US*&& DC    CL8'PRTFACS '                                                    
*&&US*&& DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
*&&US*&& DC    AL4(VSYSFAC2-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'PXGOTO  '                                                    
         DC    AL1(03,06,KEYAPGO),AL1(KEYSCMD),AL4(0)                           
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'RDID    '                                                    
         DC    AL1(03,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VRDID-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'READ    '                                                    
         DC    AL1(03,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VREAD-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'RELEASE '                                                    
         DC    AL1(03,07,KEYAREL),AL1(KEYSCMD+KEYSVP3),AL4(0)                   
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'RKEY    '                                                    
         DC    AL1(03,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VRKEY-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'SAVE    '                                                    
         DC    AL1(02,04,KEYASAV),AL1(KEYSCMD+KEYSVP3),AL4(0)                   
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'SELIST  '                                                    
         DC    AL1(03,06,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBSE-TCBD,0,0,0)                                            
*                                                                               
         DC    CL8'SKIPA   '                                                    
         DC    AL1(03,05,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VSKIPA-SYSFACD,0,0,0)                                        
*                                                                               
*&&US*&& DC    CL8'SPTFACS '                                                    
*&&US*&& DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
*&&US*&& DC    AL4(VSYSFAC2-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'SRX     '                                                    
         DC    AL1(02,03,KEYASRX),AL1(KEYSCMD+KEYSVP3),AL4(0)                   
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'SSB     '                                                    
         DC    AL1(03,03,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VSSB-SYSFACD,0,0,0)                                          
*                                                                               
         DC    CL8'ISGENQ  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VSSB-SYSFACD,SSBXAISG-SSBD,0,0)                              
*                                                                               
         DC    CL8'STEREO  '                                                    
         DC    AL1(03,06,00),AL1(KEYSUTL),AL4(0)                                
         DC    AL4(TBUFF-UTLD,0,0,0)                                            
*                                                                               
         DC    CL8'STOP    '                                                    
         DC    AL1(04,04,KEYASTP),AL1(KEYSCMD),AL4(0)                           
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'START   '                                                    
         DC    AL1(05,05,KEYASTRT),AL1(KEYSCMD),AL4(0)                          
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'SVRD    '                                                    
         DC    AL1(04,04,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBSVRD-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'SYSFACS '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'TBUFF   '                                                    
         DC    AL1(04,05,00),AL1(KEYSUTL),AL4(0)                                
         DC    AL4(TBUFF-UTLD,0,0,0)                                            
*                                                                               
         DC    CL8'TBUFF24 '                                                    
         DC    AL1(07,07,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBTBUFF-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'TBUFF31 '                                                    
         DC    AL1(07,07,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBRBUFF-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'TCB     '                                                    
         DC    AL1(03,03,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'TCBCOMF '                                                    
         DC    AL1(05,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBCOMF-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBAZIP '                                                    
         DC    AL1(05,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBAZIP-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBLNK  '                                                    
         DC    AL1(04,06,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBLNK-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'TCBPGMA '                                                    
         DC    AL1(04,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBPGMA-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBPROG '                                                    
         DC    AL1(05,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBPGMA-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBTIA  '                                                    
         DC    AL1(05,06,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBTIA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'TCBTSAR '                                                    
         DC    AL1(05,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBTSAR-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBTWA  '                                                    
         DC    AL1(05,06,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBTWA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'TCBWORK '                                                    
         DC    AL1(04,07,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBWRKA-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBWRK  '                                                    
         DC    AL1(04,06,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBWRKA-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TCBWSSVR'                                                    
         DC    AL1(05,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBAXAWS-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'TCBAXA9 '                                                    
         DC    AL1(05,08,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBAXA9-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TEMPSTR '                                                    
         DC    AL1(03,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VTEMPSTR-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'TIA     '                                                    
         DC    AL1(03,03,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBTIA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'TICPOPS '                                                    
         DC    AL1(05,07,00),AL1(KEYSFAC+KEYSVP3),AL4(TICPOPS)                  
         DC    AL4(VTICTOCT-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'TICTOC  '                                                    
         DC    AL1(04,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VTICTOC-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'TICTOCT '                                                    
         DC    AL1(07,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VTICTOCT-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'TICTRACE'                                                    
         DC    AL1(05,08,00),AL1(KEYSFAC+KEYSVP3),AL4(TICTRACE)                 
         DC    AL4(VTICTOCT-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'TSAR    '                                                    
         DC    AL1(04,04,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBTSAR-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'TSTRCVR '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VTSTRCVR-SYSFACD,0,0,0)                                      
*                                                                               
         DC    CL8'TTL     '                                                    
         DC    AL1(03,03,00),AL1(KEYSTCB),AL4(TWASHOW)                          
         DC    AL4(TCBTWA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'TTU     '                                                    
         DC    AL1(02,03,00),AL1(KEYSTCB),AL4(TWASHOW)                          
         DC    AL4(TCBTWA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'TWA     '                                                    
         DC    AL1(01,03,00),AL1(KEYSTCB),AL4(TWADISP)                          
         DC    AL4(TCBTWA-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'UNDO    '                                                    
         DC    AL1(02,04,KEYAUNDO),AL1(KEYSCMD+KEYSVP3),AL4(0)                  
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'UTL     '                                                    
         DC    AL1(03,03,00),AL1(KEYSTCB),AL4(0)                                
         DC    AL4(TCBUTL-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'VSELIST '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VSELIST-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'VTCB    '                                                    
         DC    AL1(04,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VTCB-SYSFACD,0,0,0)                                          
*                                                                               
         DC    CL8'VTSTTAB '                                                    
         DC    AL1(04,07,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VTSTTAB-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'VUTL    '                                                    
         DC    AL1(04,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VUTL-SYSFACD,0,0,0)                                          
*                                                                               
         DC    CL8'WHOAMI  '                                                    
         DC    AL1(03,06,00),AL1(KEYSTCB),AL4(WHOAMI)                           
         DC    AL4(TCBUTL-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'WHOIBE  '                                                    
         DC    AL1(03,06,00),AL1(KEYSTCB),AL4(WHOAMI)                           
         DC    AL4(TCBUTL-TCBD,0,0,0)                                           
*                                                                               
         DC    CL8'WKFILE  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VWKFILE-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'WKEY    '                                                    
         DC    AL1(03,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VWKEY-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'WRITE   '                                                    
         DC    AL1(03,05,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VWRITE-SYSFACD,0,0,0)                                        
*                                                                               
         DC    CL8'WRTAFT  '                                                    
         DC    AL1(03,06,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VWRTAFT-SYSFACD,0,0,0)                                       
*                                                                               
         DC    CL8'WSSVR   '                                                    
         DC    AL1(03,05,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBAXAWS-TCBD,0,0,0)                                         
*                                                                               
         DC    CL8'WTCKD   '                                                    
         DC    AL1(04,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VWTCKD-SYSFACD,0,0,0)                                        
*                                                                               
         DC    CL8'WTID    '                                                    
         DC    AL1(04,04,00),AL1(KEYSFAC),AL4(0)                                
         DC    AL4(VWTID-SYSFACD,0,0,0)                                         
*                                                                               
         DC    CL8'X       '                                                    
         DC    AL1(01,01,KEYASX),AL1(KEYSCMD+KEYSVP3),AL4(0)                    
         DC    AL4(0,0,0,0)                                                     
*                                                                               
         DC    CL8'XA9BLK  '                                                    
         DC    AL1(03,06,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBAXA9-TCBD,0,0,0)                                          
*                                                                               
         DC    CL8'ZIPBLK  '                                                    
         DC    AL1(03,06,00),AL1(KEYSTCB+KEYSINXA),AL4(0)                       
         DC    AL4(TCBAZIP-TCBD,0,0,0)                                          
*                                                                               
KEYTABX  DC    AL1(EOTB)                                                        
*                                                                               
KEYTABN  EQU   (KEYTABX-KEYTAB)/(KEYTABL)                                       
         DC    X'FF'                                                            
*------- ----- -----------------                                                
         DC    CL8'DMGR1   ',AL1(15),XL7'00'                                    
         DC    CL8'DMGR2   ',AL1(16),XL7'00'                                    
         DC    CL8'TABS    ',AL1(17),XL7'00'                                    
*                                                                               
KEYTABD  DSECT                                                                  
KEYWORD  DS    CL8           KEYWORD                                            
KEYMINL  DS    X             MINIMUM INPUT LENGTH FOR THIS KEYWORD              
KEYMAXL  DS    X             MAXIMUM INPUT LENGTH FOR THIS KEYWORD              
*                                                                               
KEYACT   DS    X             ACTION (IF KEYSCMD SET)                            
KEYASAV  EQU   1             SAVE DUMP                                          
KEYAREL  EQU   2             RELEASE DUMP                                       
KEYAFIX  EQU   3             SET FIXED FLAG                                     
KEYAPROC EQU   4             SET PROCESS FLAG                                   
KEYAUNDO EQU   5             UNDO FLAGS FLAG                                    
KEYASTP  EQU   6             SUPPRESS DUMPS                                     
KEYASTRT EQU   7             ALLOW DUMPS                                        
KEYASRX  EQU   8             SR(X)                                              
KEYALST  EQU   9             LIST                                               
KEYASX   EQU   10            X                                                  
KEYACHN  EQU   11            RD CHAIN                                           
KEYAIND  EQU   12            INDIRECT                                           
KEYAPGO  EQU   13            PGOTO                                              
KEYAGO   EQU   14            GOTO                                               
KEYAKNOW EQU   15            SET KNOWN FLAG                                     
KEYAPSW  EQU   16            GO to PSW                                          
*                                                                               
KEYSRCE  DS    X             WHERE TO GET KEYWORD                               
KEYSFAC  EQU   X'80'         USE DMPFACS AS BASE POINTER                        
KEYSTCB  EQU   X'40'         USE DMPTCB  AS BASE POINTER                        
KEYSUTL  EQU   X'20'         USE DMPUTL  AS BASE POINTER                        
KEYSCMD  EQU   X'10'         KEYWORD IS AN ACTION COMMAND                       
KEYSVP3  EQU   X'02'         KEYWORD VALIDATES P3 ITSELF                        
KEYSINXA EQU   X'01'         ADDRESS IS 31-BIT                                  
*                                                                               
KEYSRTN  DS    XL4           STAND ALONE PROCESSING ROUTINE                     
*                                                                               
KEYDISP1 DS    XL4           DISPLACEMENT FROM SOURCE                           
KEYDISP2 DS    XL4           DISPLACEMENT FROM KEYDISP1                         
KEYDISP3 DS    XL4           DISPLACEMENT FROM KEYDISP2                         
KEYDISP4 DS    XL4           DISPLACEMENT FROM KEYDISP3                         
KEYLOOP# EQU   ((*-KEYDISP1)/(L'KEYDISP1))                                      
KEYTABL  EQU   *-KEYTABD                                                        
*                                                                               
DUMP     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* OK MESSAGES                                                         *         
***********************************************************************         
OKMSGS   DS    0CL(IMSGL)                                                       
OK1      DC    CL(IMSGL)'All dumps shown, filter or select using PF02'          
OK2      DC    CL(IMSGL)'All dumps shown - dumps are suppressed'                
OK3      DC    CL(IMSGL)'Help displayed. Select with PF02 or type name'         
OK4      DC    CL(IMSGL)'Dump writing suppressed'                               
OK5      DC    CL(IMSGL)'Dump writing unsuppressed'                             
OK6      DC    CL(IMSGL)'Upper portion of TWA displayed'                        
OK7      DC    CL(IMSGL)'Lower portion of TWA displayed'                        
OK8      DC    CL(IMSGL)'TWA displayed - hit <enter> to page'                   
OK9      DC    CL(IMSGL)'End of TWA - hit <enter> to go back to top'            
OK10     DC    CL(IMSGL)'WHOAMI information displayed'                          
OK11     DC    CL(IMSGL)'Parameter List displayed'                              
OK12     DC    CL(IMSGL)'System dump displayed - enter next request'            
OK13     DC    CL(IMSGL)'System dump displayed - hit <enter> to page'           
OK14     DC    CL(IMSGL)'TICTRACE table shown - hit <enter> to page'            
OK15     DC    CL(IMSGL)'TICPOPS table shown - hit <enter> to page'             
OK16     DC    CL(IMSGL)'Select field help displayed'                           
OK17     DC    CL(IMSGL)'DSECT Data displayed'                                  
OK18     DC    CL(IMSGL)'DSECT Data displayed - hit <enter> to page'            
OK19     DC    CL(IMSGL)'List of storage addresses displayed'                   
OK20     DC    CL(IMSGL)'Argument found - hit <enter> to find next'             
OK21     DC    CL(IMSGL)'Argument not found - hit <enter> to continue'          
OK22     DC    CL(IMSGL)'End of dump - try a different search'                  
OK23     DC    CL(IMSGL)'Chain shown - Select any address using PF02'           
OK24     DC    CL(IMSGL)'List address shown as requested'                       
OK25     DC    CL(IMSGL)'HOLE information displayed'                            
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
ERRMSGS  DS    0CL(EMSGL)                                                       
ERR01    DC    CL(EMSGL)'Missing input field'                                   
ERR02    DC    CL(EMSGL)'Invalid input field'                                   
ERR03    DC    CL(EMSGL)'Too many parameters in field'                          
ERR04    DC    CL(EMSGL)'Invalid system'                                        
ERR05    DC    CL(EMSGL)'Invalid program'                                       
ERR06    DC    CL(EMSGL)'Field value must be numeric'                           
ERR07    DC    CL(EMSGL)'Invalid PFKey - try again'                             
ERR08    DC    CL(EMSGL)'Invalid cursor position for selection'                 
ERR09    DC    CL(EMSGL)'Disk error on dump file'                               
ERR10    DC    CL(EMSGL)'Too much input in field'                               
ERR11    DC    CL(EMSGL)'Input is not a valid Hex character'                    
ERR12    DC    CL(EMSGL)'This is not a valid register'                          
ERR13    DC    CL(EMSGL)'50% of dumps in this facpak already saved'             
ERR14    DC    CL(EMSGL)'Dump writing already suppressed'                       
ERR15    DC    CL(EMSGL)'Dump writing already unsuppressed'                     
ERR16    DC    CL(EMSGL)'Start address below minimum'                           
ERR17    DC    CL(EMSGL)'TWA is corrupted and cannot be displayed'              
ERR18    DC    CL(EMSGL)'Displacement value is too high'                        
ERR19    DC    CL(EMSGL)'Displacement value may not be negative'                
ERR20    DC    CL(EMSGL)'Internal REGSAVE table overflow - call AATK'           
ERR21    DC    CL(EMSGL)'RD chain is corrupt - cannot set registers'            
ERR22    DC    CL(EMSGL)'Register setting requested is out of range'            
ERR23    DC    CL(EMSGL)'Start value is too big - max is 59'                    
ERR24    DC    CL(EMSGL)'Too few parameters in field'                           
ERR25    DC    CL(EMSGL)'Search argument needs a terminating quote'             
ERR26    DC    CL(EMSGL)'You don''t have anything in your list'                 
ERR27    DC    CL(EMSGL)'Not enough entries in your list'                       
ERR28    DC    CL(EMSGL)'TWA field is too short to display'                     
ERR29    DC    CL(EMSGL)'TWA field is too long to display'                      
ERR30    DC    CL(EMSGL)'Please enter your initials'                            
ERR31    DC    CL(EMSGL)'The value you input makes no sense'                    
ERR32    DC    CL(EMSGL)'Address input is out of range'                         
ERR33    DC    CL(EMSGL)'"-0" - You are joking right?'                          
ERR34    DC    CL(EMSGL)'System dump displayed -> EOD    '                      
         EJECT                                                                  
DMPSSB   DS    XL(SSBLNQ)                                                       
         EJECT                                                                  
***********************************************************************         
* MAIN WORKING STORAGE DSECT                                          *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
*                                                                               
RELO     DS    F                                                                
SAVER1   DS    F                                                                
SAVERD   DS    F                                                                
SVADR    DS    F                                                                
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
MYSELIST DS    A                   A(THIS SELIST ENTRY)                         
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
AAVAIL   DS    A                                                                
*                                                                               
AHEXIN   DS    A               *** INFO FROM COMFACS                            
AHEXOUT  DS    A                                                                
AXSORT   DS    A                                                                
ADATCON  DS    A                                                                
AGETHELP DS    A                                                                
AGETTXT  DS    A                                                                
ADICTATE DS    A                                                                
CUREDIT  DS    A                                                                
ASCANNER DS    A                                                                
ADATTIM  DS    A                                                                
AGETFACT DS    A                                                                
AHELLO   DS    A                                                                
*                              *** INFO FROM SYSFACS                            
ADMGR    DS    A                                                                
ASSB     DS    A                                                                
ATCB     DS    A                                                                
ADADDS   DS    A                                                                
ACALLOV  DS    A                                                                
ADARPT   DS    A                                                                
ARDID    DS    A                                                                
AWTID    DS    A                                                                
ADMPFILE DS    A                                                                
ALNKTAB  DS    A                                                                
*                                                                               
AFACITAB DS    A                   A(FACIDTAB)                                  
ACTRYTAB DS    A                   A(CTRYTAB)                                   
ALANGTAB DS    A                   A(LANGTAB)                                   
AINA     DS    A                   A(INPUT BUFFER)                              
AINBACK  DS    A                   A(INPUT BUFFER BACKUP SLOT)                  
ASORTBLK DS    A                   A(SORT BLOCK)                                
ASVBLOCK DS    A                   A(SAVE BLOCK)                                
AIO      DS    A                   A(I/O AREA)                                  
*                                                                               
ASELIST  DS    A                   VSELIST                                      
DMCB     DS    6F                                                               
*                                                                               
AERRMSGS DS    A                                                                
AOKMSGS  DS    A                                                                
AKEYTAB  DS    A                   A(KEYWORD TABLE)                             
AKEYWORD DS    A                   A(ENTRY IN KEYTAB)                           
*                                                                               
FULL     DS    F                                                                
FULL1    DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
HALF3    DS    H                                                                
HALF4    DS    H                                                                
*                                                                               
TBALET   DS    F                   TABS DATASPACE TBALET                        
TBOFFS   DS    F                   TABS DATASPACE OFFSET                        
*                                                                               
DPARMS   DS    0XL32               PARAMETERS FOR DADDS                         
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
P7       DS    F                                                                
P8       DS    F                                                                
*                                                                               
SRVMFST  DS    H                   DISPLACEMENT TO FIRST BYTE OF SRVMSG         
DUMPINDX DS    H                   DUMP # SUMMARY LIST INDEX                    
*                                                                               
TYPDMIN  DS    H                   MIN DUMP # FOR TYPE (ADV/REP/TST)            
TYPDMAX  DS    H                   MAX DUMP # FOR TYPE (ADV/REP/TST)            
SYSDMIN  DS    H                   MIN DUMP # FOR SYSTEM                        
SYSDMAX  DS    H                   MAX DUMP # FOR SYSTEM                        
ASNUM    DS    H                   ALL SCREEN START DISPLAY NUMBER              
*                                                                               
SCURSOR  DS    H                                                                
PFKEY    DS    X                   PFKEY INPUT (IF ANY)                         
TODAY3   DS    XL3                 TODAYS DATE (YMD BINARY)                     
WHOINIT  DS    XL3                 INITIALS                                     
TODAYB   DS    XL2                 TODAYS DATE (BINARY)                         
*                                                                               
MYSYSN4  DS    CL4                 SSBSYSN4                                     
MYSYSFL  DS    X                   SSBSYSFL                                     
MYSYSCH  DS    X                   SSBSYSCH                                     
MYSYSN1  DS    X                   SSBSYSN1                                     
MYDMPCY  DS    X                   SSBDMPCY                                     
MYDMPSV  DS    XL4                 SSBDMPSV                                     
*                                                                               
FACPAKNM DS    XL5                                                              
*                                                                               
DUMPNUM  DS    H                   DUMP NUMBER TO DISPLAY                       
DUMPNUMA EQU   X'FFFF'             ALL DUMPS                                    
DUMPREG  DS    C                   REGISTER TO USE                              
*                                                                               
DUMPDSPC DS    C                   DATASPACE TO USE                             
*                                                                               
ALLFLVL  DS    X                   FILTER LEVEL                                 
ALLFLVLF EQU   C'F'                FACPAK                                       
ALLFLVLS EQU   C'S'                SYSTEM                                       
ALLFLVLP EQU   C'P'                SYSTEM/PROGRAM                               
ALLFLVLX EQU   C'X'                SAVED DUMPS ONLY                             
ALLFFACL DS    X                                                                
ALLFFAC  DS    CL4                                                              
ALLFSYSL DS    X                                                                
ALLFSYS  DS    CL8                                                              
ALLFPRGL DS    X                                                                
ALLFPRG  DS    CL8                                                              
ALLFACID DS    CL(L'FACITAB)                                                    
*                                                                               
DISPLACE DS    F                   INPUT DISPLACEMENT                           
FADRH    DS    F                   ADDR OF ERROR FIELD HEADER                   
FERN     DS    X                   ERROR NUMBER (FOR MESSAGE)                   
FERNA    DS    XL3                 ADDR OF ERROR MSG IF FERN IS 255             
FERRDSP  DS    X                   DISPLACEMENT TO ERROR IN FIELD               
HDRN     DS    X                   HEADER MESSAGE NUMBER (NO ERROR)             
*                                                                               
SRCHLEN  DS    X                   LENGTH OF SEARCH STRING                      
SRCHSTR  DS    XL15                SEARCH STRING                                
*                                                                               
SCANBLK  DS    8CL(SCBLKLQ)                                                     
*                                                                               
TRKCYL   DS    F                   NUMBER OF TRACKS/CYLINDER                    
RECTRK   DS    F                   NUMBER OF RECORDS/TRACK                      
BLKNUM   DS    F                                                                
ADDR     DS    F                   DISK ADDRESS TO READ DUMP FROM               
ADDRSTR  DS    F                   STARTING DISK ADDRESS OF DUMP                
ADDRMAX  DS    F                   MAX DA FOR THIS AREA IF SET                  
ADDRLOW  DS    F                   LOWEST CORE ADDR FOR THIS BLOCK              
*                                                                               
TRMNUM   DS    XL2                 TERMINAL NUMBER                              
*                                                                               
ABEGIN   DS    F                   A(MOST RECENT ENTRY IN TICTRACE)             
FSTENT   DS    F                   A(FIRST ENTRY IN TICTRACE)                   
ENDTBL   DS    F                   A(AFTER THE LAST ENTRY IN TICTRACE)          
SAVERE   DS    F                   SAVE REG. E                                  
ENTNUM   DS    H                   HOW MANY ENTRIES TO GO BACK                  
*                                                                               
DOMRGE   DS    C                                                                
LVERR    DS    X                                                                
LVERRA   DS    F                                                                
*                                                                               
SFLAG    DS    XL1                 TELL IF WE CAN USE INFO IN TWA 11            
SFT11OK  EQU   X'80'                                                            
GOTOBIT  EQU   X'40'                                                            
*SETREGS EQU   X'20'                                                            
FOUNDFLG EQU   X'10'                                                            
DSKERFLG EQU   X'08'               * DISK ERROR IN ALL DISPLAY                  
STEREO   EQU   X'04'               SOFT FIELD DISPLAY ON THIS AREA              
NOHDR    EQU   X'02'               don't write header on save                   
*                                                                               
SAVEDFLG DS    XL1                 TELL IF WE KEEP REGISTERS OR NOT             
*                                  X'80' - REGISTERS ARE TO BE SAVED            
*                                  X'40' - PARAMETERS ARE TO BE SAVED           
*                                                                               
START    DS    F                   STARTING ADDRESS                             
FROM     DS    F                                                                
LEN      DS    H                   LENGTH                                       
NXTSTART DS    F                   NEXT ADDRESS AFTER SUCCESSFUL SEARCH         
TWASTRT  DS    F                                                                
ASYS     DS    A                                                                
*                                                                               
SETREGS  DS    XL64                CURRENT SET REGISTERS                        
*                                                                               
REGHOLD  DS    16F                 HOLDS VALUES OF REGISTERS ON DUMP            
NBLKS2   DS    H                                                                
DONESW   DS    C                                                                
SAMIND   DS    X                                                                
DINDIC   DS    C                   DUMP INDICATOR                               
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
REGADD   DS    C                                                                
SRXCLEAR DS    XL1                 TELL IF USER DID SRX 0                       
SYSSAVE  DS    X                   UTL CODE SAVE                                
PRGSAVE  DS    X                   UTL CODE SAVE                                
TODAY    DS    XL3                 TODAY'S DATE FROM DATCON                     
WORK     DS    CL80                                                             
WRK      DS    CL256                                                            
MSG      DS    CL(EMSGL)                                                        
KEYSAVE  DS    CL12                                                             
XREGS    DS    C                   EXTENDED REGISTER (Y/N)                      
*                                                                               
FLAG     DS    C                                                                
DDS      DS    C                                                                
DDSTRM   EQU   X'01'               DDS TERMINAL                                 
*                                                                               
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
MAXTWAL  DS    H                   MAXIMUM SIZE OF TWA                          
*                                                                               
ADSECT   DS    A                   A(CURRENT DSECT IN PHASE)                    
ACURRPOS DS    A                   A(CURRENT POSITION IN DSECT PHASE)           
APHASE   DS    A                   A(DSECT PHASE)                               
FIELDLEN DS    H                   L'CURRENT FIELD                              
DSPTOLBL DS    XL1                 DISPLACEMENT TO LABEL IN FIELD               
DSSYSCHR DS    CL1                 CHAR SYSTEM REQUESTED FOR DSECT              
DSSYS    DS    XL1                 SYSTEM REQUESTED FOR DSECT                   
OVERLAY  DS    XL1                                                              
ELLEN    DS    XL1                 L'CURRENT ELEMENT                            
LABEL    DS    CL8                 LABEL                                        
LLABEL   DS    XL1                 L'LABEL                                      
DATADISP DS    X                   DISP. TO FIRST ELEMENT                       
ELCODE   DS    XL1                 ELEMENT CODE                                 
RECOREL  DS    CL1                 'R'ECORD OR 'E'LEMENT INDICATOR              
LBLSTAT  DS    XL1                 CURRENT DSECT RECORD STATUS                  
LBLQREC  EQU   X'80'               LABEL IS EQUATE FOR A RECORD                 
LBLQEL   EQU   X'40'               LABEL IS EQUATE FOR AN ELEMENT               
*                                                                               
DMPFLGS1 DS    XL1                 SPECIAL DUMP FLAG #1                         
DMPDSCTD EQU   X'80'               CALL HABER'S DSECT DISPLAYER                 
DMPSRCH  EQU   X'40'               IN SEARCH MODE                               
DMPSX    EQU   X'20'               IN X MODE                                    
*                                                                               
DHDR     DS    XL(DMPHDRL)         DUMP HEADER RECORD                           
*                                                                               
FACIDTNQ EQU   15*3                FACIDMAX*3 FOR (TST+MEL/FQA/CSC)             
FACIDTT  DS    (FACIDTNQ+1)CL(L'FACITAB) DYN BUILT TEST FACIDTAB                
FACIDTTX EQU   *                                                                
*                                                                               
IO       DS    2000C               I/O AREA                                     
IOL      EQU   *-IO                                                             
*                                                                               
SVBLOCK  DS    (L'SRCOMWRK)X       SAVED STORAGE COPY                           
SVBLOCKL EQU   *-SVBLOCK                                                        
*                                                                               
SORTBLK  DS    XL(ALLSCLNE*ALLSCLEN*ALLSCMAX)                                   
SORTBLKL EQU   *-SORTBLK                                                        
*                                                                               
INBACK   DS    CL16                LAST BYTES OF PREVIOUS RECORD                
*INA     DS    (RLEN)C            RECORD AREA                                   
*INAL    EQU   *-INA                                                            
INA      DS    (57000)C            RECORD AREA                                  
INAL     EQU   RLEN                                                             
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER FILE DEFINITION TABLE                                *         
***********************************************************************         
HELEND   DSECT                                                                  
HELFLEN  DS    CL1                 L'FILE NAME - 1                              
HELNAME  DS    CL8                 NAME                                         
HELMSIZE DS    CL2                 MAXIMUM RECORD SIZE                          
HELEDIS  DS    CL1                 DISPLACEMENT TO FIRST ELEMENT                
HELLDIS  DS    CL1                 DISPLACEMENT TO LENGTH                       
HELENL   EQU   *-HELEND                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN LINE FORMAT                                          *         
***********************************************************************         
LIND     DSECT                     DISPLAY LINE FORMAT                          
LINSTRTH DS    CL8                 HEADER                                       
LINSTRT  DS    CL8                 START ADDRESS                                
LINCL1SH DS    CL8                 HEADER OF SELECT                             
LINCL1S  DS    CL1                 SELECT                                       
LINCOL1H DS    CL8                 HEADER OF COLUMN                             
LINCOL1  DS    CL8                 FIRST COLUMN HEX VALUE                       
LINCL2SH DS    CL8                 HEADER OF SELECT                             
LINCL2S  DS    CL1                 SELECT                                       
LINCOL2H DS    CL8                 HEADER OF COLUMN                             
LINCOL2  DS    CL8                 SECOND COLUMN                                
LINCL3SH DS    CL8                 HEADER OF SELECT                             
LINCL3S  DS    CL1                 SELECT                                       
LINCOL3H DS    CL8                 HEADER OF COLUMN                             
LINCOL3  DS    CL8                 THIRD COLUMN                                 
LINCL4SH DS    CL8                 HEADER OF SELECT                             
LINCL4S  DS    CL1                 SELECT                                       
LINCOL4H DS    CL8                 HEADER OF COLUMN                             
LINCOL4  DS    CL12                FOURTH COLUMN                                
LINHEX   DS    CL16                EBCDIC EQUIVALENT                            
         DS    CL7                 MORE SPACES                                  
LINNXT   DS    0C                  ADDRESS OF NEXT LINE                         
         EJECT                                                                  
***********************************************************************         
* ALL SCREEN LINE FORMAT DSECT                                        *         
***********************************************************************         
ALIND    DSECT                     ALL DUMP # LIST LINE FORMAT                  
ALINLIN  DS    0CL78                                                            
ALINNUM  DS    CL3                 DUMP #                                       
ALINADV  DS    CL3                 DUMPING SYSTEM                               
         DS    CL1                                                              
ALINDUP  DS    CL2                 DUPLICATE COUNTS                             
         DS    CL1                                                              
ALINSTA  DS    CL1                 STATUS                                       
ALINSVE  DS    CL1                 SAVED (IF *)                                 
         DS    CL1                                                              
ALINWHO  DS    CL3                                                              
         DS    CL1                                                              
ALINDAT  DS    CL5                 DATE (DAY/MONTH)                             
         DS    CL1                                                              
ALINTIM  DS    CL8                 TIME                                         
         DS    CL1                                                              
ALINTRM  DS    CL8                 TERMINAL                                     
         DS    CL1                                                              
*&&US                                                                           
ALINSYS  DS    CL4                 SYSTEM                                       
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
ALINSYS  DS    CL5                 SYSTEM                                       
*&&                                                                             
ALINPRG  DS    CL3                 PROGRAM                                      
         DS    CL1                                                              
ALINSUB  DS    CL8                 SUBROUTINE                                   
         DS    CL1                                                              
ALINDSP  DS    CL4                 PSW-RB OFFSET                                
         DS    CL1                                                              
ALINABC  DS    CL1                 TYPE OF ABEND CODE                           
         DS    CL1                                                              
ALININS  DS    CL12                INSTRUCTION                                  
         EJECT                                                                  
***********************************************************************         
* TWA DISPLAY SCREEN DSECT                                            *         
***********************************************************************         
DMPLINED DSECT                     DISPLAY LINE FORMAT (TWA)                    
DMPLSTRT DS    CL06                HEADER LINE                                  
         DS    X                                                                
DMPLDSP  DS    CL04                                                             
         DS    X                                                                
DMPLROW  DS    CL02                                                             
DMPLROWI DS    CL02                                                             
         DS    X                                                                
DMPLCOL  DS    CL02                                                             
DMPLCOLI DS    XL02                                                             
         DS    X                                                                
DMPLHDR  DS    CL02                                                             
DMPLHDRI DS    CL16                                                             
DMPLINF  DS    CL39                                                             
         ORG   DMPLSTRT                                                         
         DS    XL2                 FIELD DATA LINE                              
DMPLHEX  DS    CL48                                                             
         DS    XL2                                                              
DMPLALF  DS    CL24                                                             
         EJECT                                                                  
**********************************************************************          
* CHAIN DISPLAY LINE DSECT                                           *          
**********************************************************************          
RDLINED  DSECT                                                                  
RDLLBL   DS    CL4                 RD LABEL                                     
         DS    XL2                                                              
RDLRD    DS    CL6                 RD                                           
         DS    XL2                                                              
RDLEYE   DS    CL8                 RB EYECATCHER                                
         DS    XL2                                                              
RDLRB    DS    CL8                 RB                                           
         DS    XL2                                                              
RDLRE    DS    CL8                 RE                                           
         DS    XL2                                                              
RDLRERB  DS    CL6                 RE-RB                                        
         DS    XL2                                                              
RDLRF    DS    CL8                 RF                                           
         DS    XL2                                                              
RDLR1    DS    CL8                 R1                                           
         EJECT                                                                  
**********************************************************************          
* SOFT FIELD DISPLAY LINE DSECT                                      *          
**********************************************************************          
DMPSOFTD DSECT                     DISPLAY LINE FORMAT (SOFT FIELDS)            
DMPFHDR  DS    CL8                 HEADER                                       
DMPFADDR DS    CL6                 START ADDRESS                                
         DS    X                                                                
DMPFROWH DS    CL12                'SET NEW ROW='                               
DMPFROW  DS    CL3                                                              
         ORG   DMPFROWH                                                         
DMPFHHEX DS    CL8                 HEXOUT HEADER DETAILS                        
         DS    XL2                                                              
DMPFHFLD DS    CL18                FIELD TYPE                                   
         DS    X                                                                
DMPFHCL  DS    CL54                'COL=XXX,ROW=XXX'                            
         ORG   DMPFROWH                                                         
DMPFINF  DS    XL4                                                              
         DS    XL6                                                              
DMPFDTA  DS    XL50                                                             
         ORG   DMPFROWH                                                         
DMPFCURH DS    CL30                'CURSOR EXCEPTION,DISPLACEMENT='             
DMPFCUR  DS    CL3                                                              
         ORG   DMPFROWH                                                         
DMPFSSFH DS    CL24                'SET STEREO FIELD='                          
DMPFSSF  DS    CL3                                                              
         ORG   DMPSOFTD+L'DMPFHDR+79                                            
DMPFNEXT DS    0C                  ADDRESS OF NEXT LINE                         
         EJECT                                                                  
**********************************************************************          
* PARAMETER LIST DISPLAY LINE DSECT                                  *          
**********************************************************************          
DMPPARMD DSECT                     DISPLAY LINE FORMAT (PARAMETER LIST)         
DMPPPARM DS    CL2                 PARAMETER NUMBER (P#)                        
         DS    XL2                                                              
DMPPCONT DS    CL8                 CONTENTS OF THE PARAMETER                    
         DS    XL2                                                              
DMPPHEX  DS    CL32                IF PARAMETER IS AN ADDRESS, WHAT IS          
         DS    XL2                 BEING POINTED TO BY THE ADDRESS              
DMPPALPH DS    CL16                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT LINE FORMAT DSECT                                            *         
***********************************************************************         
OLINED   DSECT                                                                  
OADDR    DS    XL6                                                              
         DS    XL1                                                              
OPHASE   DS    XL7                                                              
         DS    XL1                                                              
OLEN     DS    XL6                                                              
         DS    XL2                                                              
OLINEL   EQU   *-OLINED                                                         
         EJECT                                                                  
**********************************************************************          
* DSECT DISPLAYER SCREEN LINE                                        *          
**********************************************************************          
LINED    DSECT                                                                  
LINLHS   DS    CL37                LHS                                          
         ORG   LINLHS                                                           
LINDISP  DS    CL4                 DISPLACEMENT                                 
         DS    CL1                                                              
LINLBL   DS    CL8                 LABEL                                        
         DS    CL1                                                              
LINDEF   DS    CL5                 DEFINITION                                   
LINTYPE  DS    CL18                TYPE                                         
         DS    CL1                                                              
         ORG                                                                    
LINDATA  DS    CL(L'DMPL1-L'LINLHS)  DATA                                       
         EJECT                                                                  
**********************************************************************          
* TICTOC SCREEN LINE DISPLAY DSECT                                   *          
**********************************************************************          
SCDSECT  DSECT                     DSECT FOR SCREEN LINE                        
SCDTID   DS    CL2                 TASK ID 'TN'                                 
         DS    CL1                                                              
SCDTIME  DS    0CL8                TIME (HH.MM.SS)                              
SCDHRS   DS    CL2                                                              
         DS    CL1                                                              
SCDMIN   DS    CL2                                                              
         DS    CL1                                                              
SCDSEC   DS    CL2                                                              
         DS    CL1                                                              
SCDTYPE  DS    CL4                 TYPE OF ENTRY                                
         DS    CL1                                                              
SCDUSER  DS    CL10                USER NAME                                    
         DS    CL1                                                              
SCDSYSPR DS    CL17                SYS/PROG  OR MODULE AND DISP                 
         DS    CL1                                                              
SCDLINE  DS    CL4                 TCB LINE ADDR                                
SCDTRM   DS    CL4                 TCB TERM ADDR                                
         DS    CL1                                                              
SCDAWRK  DS    CL6                 A(WRK)                                       
         DS    CL1                                                              
SCDATWA  DS    CL6                 A(TWA)                                       
         DS    CL1                                                              
SCDAPGM  DS    CL6                 A(PGM)                                       
SCDLENQ  EQU   *-SCDSECT           RIGHT NOW  75 BYTES                          
         EJECT                                                                  
**********************************************************************          
* TICTOC SCREEN DSECT                                                *          
**********************************************************************          
TBDSECT  DSECT                     DSECT FOR TABLE ENTRY                        
TBDTYPE  DS    CL4                 TYPE OF ENTRY (1SET, SSET, ETC)              
TBDPSW   DS    CL4                 PSW ADDR WHEN POP OCCURED                    
TBDTIME  DS    CL4                 TIME IN TU'S                                 
TBDATCB  DS    CL4                 ADDRESS OF TCB ENTRY                         
         ORG   *-4                                                              
TBDBREG  DS    CL4                 BASE REG FOR POPS                            
TBDSTATS DS    0CL12                                                            
TBDTSYM  DS    CL8                 LINE,TERMINAL                                
TBDTOSYS DS    CL1                                                              
TBDTSYS  DS    CL1                 SYSTEM NUMBER                                
TBDTPRG  DS    CL1                 PROGRAM NUMBER                               
TBDTASK  DS    CL1                 TASK ID                                      
TBDUSER  DS    CL2                 USER ID IN BINARY                            
         DS    CL1                                                              
TBDLENQ  EQU   *-TBDSECT           32 BYTES                                     
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE DSECT (SAVED AT SR$DUMP IN TWA 11)                    *         
***********************************************************************         
NDSAVED  DSECT                                                                  
NDWORD   DS    CL8                 THE WORD 'NDUMP'                             
NDNUM    DS    AL2                 DUMP NUMBER                                  
NDSPR1   DS    XL2                 N/D                                          
NDLSSCRN DS    X                   LAST SCREEN                                  
NDTSSCRN DS    X                   THIS SCREEN                                  
NDTIME   DS    F                   TIME DUMPED                                  
NDLSTRT  DS    A                   LAST START                                   
*                                                                               
NDFLAG   DS    XL1                 SAVED FLAGS                                  
NDFOKCHK EQU   X'80'                                                            
*??                                X'80' - REGISTERS ARE SAVED                  
*??                                X'40' - PARAMETERS ARE SAVED                 
NDFSROK  EQU   X'02'               SR(X) IS OK                                  
NDFSRSET EQU   X'01'               SR(X) IS SET                                 
*                                                                               
NDSYSN   DS    XL1                 SYSTEM NUMBER USED FOR DICTATE               
NDSPR2   DS    XL1                 N/D                                          
*                                                                               
NDLSTCNT DS    XL1                 # ITEMS IN LIST (LIST SCREEN)                
NDLIST   DS    24XL(NDLDLQ)        LIST OF ADDRESSES AND REMARKS                
NDLISTLQ EQU   *-NDLIST                                                         
*                                                                               
NDWRKA   DS    A                   A(TCBWRKA)                                   
NDWRKX   DS    A                   A(END OF TCBWRKA)                            
NDNXT    DS    A                                                                
*                                                                               
NDPRMCQ  EQU   8                   UP TO 8 PARAMETERS                           
NDPRM    DS    (NDPRMCQ)XL4        SAVED PARAMETER LIST                         
NDPRMLQ  EQU   *-NDPRM                                                          
*                                                                               
NDCURREG DS    XL64                CURRENT REGISTERS                            
NDMPCOLS DS    72F                 THE COLUMNS OF HEX (1 COL = 4 BYTES)         
NDMPADDR DS    17F                 THE ADDRESS OF HEX COLUMNS                   
*                                                                               
NDSVRCNT EQU   72                                                               
NDSVREG  DS    (NDSVRCNT)XL68      SR(X) REGISTERS FROM TCB CHAIN               
NDSVREGL EQU   *-NDSVREG                                                        
*                                                                               
NDLD     DSECT                     COVERS LIST LINE DETAILS                     
NDLADDR  DS    F                                                                
NDLAFLDL DS    X                                                                
NDLAFLD  DS    XL8                                                              
NDLCFLD  DS    XL(L'LSTRMRK)                                                    
NDLDLQ   EQU   *-NDLD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT DISPLAYER DSECT                                               *         
***********************************************************************         
       ++INCLUDE SRFADDSECT                                                     
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECTS                                                       *         
***********************************************************************         
       ++INCLUDE SRDMPFFD                                                       
         EJECT                                                                  
         ORG   SRVTAGH                                                          
       ++INCLUDE SRDMPFED                                                       
         EJECT                                                                  
         ORG   SRVTAGH                                                          
       ++INCLUDE SRDMPFDD                                                       
         EJECT                                                                  
***      ORG   SRVTAGH                                                          
***    ++INCLUDE SRDMPFCD                                                       
***      EJECT                                                                  
         ORG   SRVTAGH                                                          
       ++INCLUDE SRDMPFBD                                                       
         EJECT                                                                  
         ORG   SRVTAGH                                                          
       ++INCLUDE SRDMPFAD                                                       
         ORG   SRVTAGH                                                          
       ++INCLUDE SRDMPF9D                                                       
         ORG   SRVTAGH                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FADSECTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
*DMDTFPH                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*FATABSD                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
*FATABSDMP                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
*FALNKTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FALNKTAB                                                       
         PRINT ON                                                               
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*DDFH                                                                           
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
*FAHOLED                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAHOLED                                                        
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*FASYSLSTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*FAPROGSPCD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAPROGSPCD                                                     
         PRINT ON                                                               
*SRDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE SRDDEQUS                                                       
         PRINT ON                                                               
*FATBHD                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATBHD                                                         
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*SEACSFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*DDDICTATED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
* FAXPTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXPTABD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SRDMP00   02/11/16'                                      
         END                                                                    
