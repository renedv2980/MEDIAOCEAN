*          DATA SET ACWFM00    AT LEVEL 020 AS OF 02/11/18                      
*PHASE T61A00A                                                                  
*INCLUDE CONVMOS                                                                
*INCLUDE BMONVAL                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE BINSRCH                                                                
WFM      TITLE '- WORKER FILE MAINTENANCE'                                      
***********************************************************************         
*JSAY  020  06FEB18  <SPEC-19997> Allow updates for AAJ worker files  *         
***********************************************************************         
* REGISTER USAGE: RC = GWS DSECT                                      *         
*                 RB = 1ST BASE                                       *         
*                 RA = 2ND BASE                                       *         
*                 R9 = 3RD BASE                                       *         
*                 R8 = 4TH BASE                                       *         
*                 R7 = SAVED WS DSECT                                 *         
*                 R6 = TWAD DSECT                                     *         
*            RE - R5 = GP REGS                                        *         
***********************************************************************         
WFM      CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL (WORKX-WORKD),**WFM***,RA,R9,R8,RR=RE,CLEAR=YES                  
         USING WORKD,RC                                                         
         ST    RE,RELO                                                          
         MVC   AINP,0(R1)          SAVE A(TIOB)                                 
         L     R6,4(R1)                                                         
         ST    R6,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
         USING TWAD,R6                                                          
         MVC   COMPANY,0(R1)       EXTRACT COMPANY FROM FAPARM                  
         LA    R7,SAVEAREA                                                      
         USING SAVED,R7                                                         
*                                                                               
         L     RE,8(R1)            RF=A(ACCFACS)                                
         USING ACCFACSD,RE                                                      
         MVC   VACCEMU,AACCEMU                                                  
         DROP  RE                                                               
*                                                                               
         L     RF,20(R1)           RF=A(EXTRA INFO BLOCK)                       
         MVC   AGYOPTS,0(R1)                                                    
         MVC   AGYCTRY,1(RF)                                                    
         MVC   AGYLANG,3(RF)                                                    
         MVC   AGYCURR,4(RF)                                                    
         CLI   AGYCTRY,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   AGYCTRY,CTRYGBR                                                  
*&&US*&& MVI   AGYCTRY,CTRYUSA                                                  
         CLI   AGYLANG,0                                                        
         BNE   *+8                                                              
*&&UK*&& MVI   AGYLANG,LANGEUK                                                  
*&&US*&& MVI   AGYLANG,LANGEUS                                                  
*                                                                               
         L     R1,16(R1)                                                        
         ST    R1,ACOM             EXTRACT A(COMFACS) FROM FAPARMS              
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VCALLOVL,CCALLOV                                                 
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VCUREDIT,CCUREDIT                                                
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETPROF,CGETPROF                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VREPORT,CREPORT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VUNSCAN,CUNSCAN                                                  
         DROP  R1                                                               
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB(4),EFFS        GET UTL ENTRY                                
         GOTO1 VSWITCH,DMCB                                                     
         MVC   AUTL,0(R1)          SAVE A(UTL ENTRY)                            
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   SENO,TSYS           EXTRACT SENO                                 
         MVC   USER,TUSER          AND CONNECTED USER-ID                        
         MVC   LUID,TSYM           AND CONNECTED USER LUID                      
         MVC   PGNO,TPRG           AND PROGRAM NO.                              
         TM    TTEST,TTESTNOU      TEST CONNECTED AS U=N                        
         BZ    *+8                                                              
         OI    TWAMODE2,TWAM2NUP   SET NO UPDATES BIT                           
         DROP  R1                                                               
*                                                                               
         MVI   DMCB,X'FE'          GET V(SELIST)                                
         MVC   DMCB+1(3),EFFS                                                   
         GOTO1 VSWITCH,DMCB                                                     
         L     R1,0(R1)            R1=A(SYSFACS)                                
         MVC   VSEXLST,VSELIST-SYSFACD(R1)                                      
*                                                                               
         LA    R1,ROUTS            SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,ROUTSN                                                        
         SR    RE,RE                                                            
         L     RF,=A(ROUT)                                                      
         A     RF,RELO             RF=A(GLOBAL ROUTINES)                        
INIT02   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT02                                                        
*                                                                               
         LA    R0,TABLESN          SET A(ROOT TABLES IN W/S)                    
         L     R1,=A(TABLES)                                                    
         A     R1,RELO                                                          
         LA    RE,ATABLES                                                       
INIT04   LH    RF,0(R1)                                                         
         LA    RF,WFM(RF)                                                       
         ST    RF,0(RE)            SET TABLE ADDRESS                            
         LA    R1,2(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,INIT04                                                        
*                                                                               
         LA    R1,WORKD                                                         
         AH    R1,=Y(OFFBLK-WORKD)                                              
         ST    R1,AOFFBLK                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(TSARBLK-WORKD)                                             
         ST    R1,ATSARBLK                                                      
         LA    R1,WORKD                                                         
         AH    R1,=Y(TRNBLOCK-WORKD)                                            
         ST    R1,ATRNBLK                                                       
         LA    R1,WORKD                                                         
         A     R1,=A(PQBUFF-WORKD)                                              
         ST    R1,APQBUFF                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(IOAS-WORKD)                                                
         ST    R1,AIOAS                                                         
         LA    R1,WORKD                                                         
         AH    R1,=Y(WKBUF1-WORKD)                                              
         ST    R1,AWKBUF1                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(WKBUF2-WORKD)                                              
         ST    R1,AWKBUF2                                                       
         LA    R1,WORKD                                                         
         AH    R1,=Y(WKIO-WORKD)                                                
         ST    R1,AWKIO                                                         
         LA    R1,TWAD                                                          
         AH    R1,=Y(UIDTAB-TWAD)                                               
         ST    R1,AUIDTAB                                                       
         L     R1,=V(CONVMOS)                                                   
         A     R1,RELO                                                          
         ST    R1,VCONVMOS                                                      
         L     R1,=V(BMONVAL)                                                   
         A     R1,RELO                                                          
         ST    R1,VBMONVAL                                                      
         L     R1,=V(CHOPPER)                                                   
         A     R1,RELO                                                          
         ST    R1,VCHOPPER                                                      
         L     R1,=V(BINSRCH)                                                   
         A     R1,RELO                                                          
         ST    R1,VBINSRCH                                                      
*                                                                               
         GOTO1 VCALLOVL,DMCB,0,X'D9000A5D'                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)         GET TSAR ADDRESS - CORERES                   
*                                                                               
         GOTO1 (RF),(R1),0,X'D9000A62'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFAL,0(R1)        GET OFFAL ADDRESS - CORERES                  
*                                                                               
         GOTO1 (RF),(R1),0,X'D9000A63'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VADDTRN,0(R1)       GET ADDTRN ADDRESS - CORERES                 
*                                                                               
         GOTO1 (RF),(R1),0,X'D9000AFA'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETIDS,0(R1)       GET GETIDS ADDRESS - CORERES                 
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   LARE,=X'41E0'                                                    
         MVC   LARF,=X'41F0'                                                    
*                                                                               
         L     R2,AIOAS            GET CONNECTED USER-ID NAME                   
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,USER                                                     
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
INIT06   CLI   CTDSCEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R2,R0                                                            
         B     INIT06                                                           
         MVC   UIDNAME,CTDSC       SAVE CONNECTED ID NAME                       
         DROP  R2                                                               
*                                                                               
         LA    RF,WKBINI           SET INITIALISE WKBUFF                        
         TM    TWAMODE,TWAMINIT    TEST FIRST TIME                              
         BZ    *+8                                                              
         LA    RF,WKBRST           SET RESTORE WKBUFF                           
         GOTO1 VDMGR,DMCB,(RF),WKFILE,WKID,SWKAREA,AWKBUF1                      
*                                                                               
         TM    TWAMODE,TWAMINIT    FIRST TIME?                                  
         BNZ   INIT08                                                           
         LA    R0,SAVEAREA         CLEAR SAVED VALUES                           
         LH    R1,=Y(SAVEDLN)                                                   
         CH    R1,=Y(SAVEAREL)                                                  
         BNH   *+6                                                              
         DC    H'0'                ENSURE WE HAVEN'T OVERSTEPPED                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LU  ',ADCLISTU,DSLISTU                           
         GOTO1 (RF),(R1),C'LL  ',ADCLISTL,DSLISTL                               
*                                                                               
         MVI   KEYFLTS,1           FORCE WORKER INDEX READ 1ST TIME             
*                                  ESTABLISH ACCOUNT FILE FORMAT                
***********************************************************************         
*        MVI   FILEFORM,VLISQ                                         *         
*        GOTO1 VDMGR,DMCB,=C'DTFAD',ACCFIL                            *         
*        L     R1,12(R1)                                              *         
*        TM    DTFTYPE-DTFPHD(R1),DTFTEMU                             *         
*        BZ    *+8                                                    *         
***********************************************************************         
*                                                                               
         MVI   FILEFORM,ISDAQ                                                   
         LA    R2,KEY              R2=A(KEY)                                    
         USING CPYRECD,R2          READ COMPANY RECORD                          
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,COMPANY                                                  
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    *+6                                                              
         DC    H'0'                NO COMPANY DIRECTORY RECORD                  
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                NO COMPANY DATA RECORD                       
*                                                                               
         L     R1,AIOBUFF                                                       
         LA    R1,CPYRFST-CPYRECD(R1)                                           
         USING CPYELD,R1                                                        
         CLI   CPYEL,CPYELQ                                                     
         BE    *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
*                                                                               
         MVC   COMPALFA,CPYALPHA   EXTRACT DATA FROM COMPANY ELEMENT            
         MVC   COMPUID,CPYUID                                                   
         MVC   COMPSTA1,CPYSTAT1                                                
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN2Q                                                    
         BL    INIT07                                                           
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q                                                    
         BL    INIT07                                                           
         MVC   COMPSTA9,CPYSTAT9                                                
         MVC   COMPSTAA,CPYSTATA                                                
         CLI   CPYLN,CPYLN4Q                                                    
         BL    INIT07                                                           
         MVC   COMPGMOA,CPYGLMOA                                                
*                                                                               
INIT07   GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         GOTO1 (RF),(R1),(0,WORK),(2,TODAYC)                                    
         GOTO1 (RF),(R1),(0,WORK),(3,TODAYB)                                    
*                                  SET USER STATUS                              
INIT08   TM    WFMPASH+(FVATRB-FVIHDR),FVAPROT  TEST PASSWD FLD UNPROT          
         BNZ   INIT12                                                           
         OI    WFMPASH+(FVATRB-FVIHDR),FVAPROT  RE-PROTECT IT                   
         SR    RF,RF                                                            
         IC    RF,TODAYB+1                                                      
         MH    RF,=Y(L'PASTAB)                                                  
         LA    RF,PASTAB-L'PASTAB(RF)                                           
         CLC   WFMPAS,0(RF)        TEST PASSWORD                                
         BNE   INIT10                                                           
         MVI   TWAPCNT,0                                                        
         MVI   TWASALFA,C'Z'       SET MAX STATUS                               
         OI    DISIND,DISINOSC     SET NO SCROLL THIS TIME                      
         B     INIT12                                                           
*                                                                               
INIT10   CLI   TWAPCNT,TWAPMAX                                                  
         BNH   *+6                                                              
         DC    H'0'                SECURITY VIOLATION                           
         SR    RF,RF                                                            
         IC    RF,TWAPCNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,TWAPCNT                                                       
*                                                                               
INIT12   MVI   USERSTS,IAMGODQ                                                  
         CLI   TWASALFA,C'Z'       TEST STS OVERRIDE (SYSTEMS ONLY)             
         BE    INIT18                                                           
         CLC   TWAAGY,=C'&&&&'     DATA CONTROL - RESERVED ALPHA(1)             
         BE    *+10                                                             
         CLC   TWAAGY,=C'++'       DATA CONTROL - RESERVED ALPHA(2)             
         BE    *+10                                                             
         CLC   TWAAGY,=C'&&+'      DATA CONTROL - RESERVED ALPHA(3)             
         BE    *+10                                                             
         CLC   TWAAGY,=C'+&&'      DATA CONTROL - RESERVED ALPHA(4)             
         BNE   INIT16                                                           
         MVC   FVMSGNO,=AL2(AE$ERRCT)  'USER-ID NOT ALLOWED'                    
         LA    RF,WFMSRVH                                                       
         ST    RF,FVADDR                                                        
         LA    RF,LUITAB                                                        
INIT14   CLI   0(RF),0                                                          
         BE    FVERR                                                            
         CLC   LUID,0(RF)                                                       
         BE    INIT18                                                           
         LA    RF,L'LUITAB(RF)                                                  
         B     INIT14                                                           
INIT16   MVI   USERSTS,DDSUSRQ                                                  
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BE    INIT18                                                           
         MVI   USERSTS,PUSRIDQ                                                  
         CLC   COMPUID,USER                                                     
         BE    *+8                                                              
         MVI   USERSTS,SUSRIDQ                                                  
         TM    TWAAUTH,X'0F'       TEST UPDATE SECURITY (NON-DDS)               
         BZ    *+8                                                              
         OI    USERSTS,UPDATEQ     SET USER CAN UPDATE                          
         TM    TWAAUTH,X'F0'       TEST DELETE SECURITY (NON-DDS)               
         BZ    *+8                                                              
         OI    USERSTS,DELETEQ     SET USER CAN DELETE                          
*                                                                               
         USING SCRTABD,RE                                                       
INIT18   L     RE,ASCRTAB          SET SCREEN ADDRESSES/ATTRIBUTES              
         CLI   TWASCROV,TWASCDET   TEST DETAIL SCREEN LOADED                    
         BNE   *+8                 NO - LIST OR 0                               
         LA    RE,SCRTABL(RE)                                                   
         LA    R0,SCRYTYPS                                                      
         LA    R1,SCRHEAD                                                       
         LA    RF,ADISHEAD                                                      
INIT20   LH    R2,0(R1)                                                         
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         AR    R2,R6                                                            
         ST    R2,0(RF)                                                         
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT20                                                        
         MVC   DISLINEL,SCRWIDT    USABLE L'DISPLAY LINE                        
         MVC   DISSCRNL,SCRPHSW    PHYSICAL LINE LENGTH                         
         MVC   DISLINES,SCRLINS    N'DISPLAY LINES                              
         MVC   DISCOLS,SCRCOLS     COL NUMBERS FOR THIS SCREEN                  
         MVC   DISDISD,SCRDISD                                                  
         DROP  RE                                                               
*                                                                               
         TM    TWAMODE,TWAMINIT                                                 
         BNZ   INIT22                                                           
         CLI   USERSTS,DDSUSRQ     SET DIFFERENT HELP PANEL NOS. IF DDS         
         BL    INIT22                                                           
         LA    RF,5                STANDARD HELP NOS. +5 FOR DDS USER           
         CLI   USERSTS,IAMGODQ                                                  
         BL    *+8                                                              
         LA    RF,10               STD HELP NOS. +10 FOR SUPER-DDS USER         
         SR    R0,R0                                                            
         IC    R0,WFMKEYX          BUMP KEY HELP                                
         AR    R0,RF                                                            
         STC   R0,WFMKEYX                                                       
         IC    R0,WFMOPTX          BUMP OPTIONS HELP                            
         AR    R0,RF                                                            
         STC   R0,WFMOPTX                                                       
*                                                                               
INIT22   GOTO1 ATSARINI            INITIALISE/RESTORE TSAR BUFFER               
*                                                                               
         L     R1,AOFFBLK          INITIALISE OFFAL FOR OFFICE ACCESS           
         USING OFFALD,R1                                                        
         MVI   OFFACTRL,OFFACCNV   NEW STYLE RECORDS                            
         MVC   OFFACOMF,ACOM                                                    
         MVC   OFFATSAR,VTSAR                                                   
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
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
         B     VALKEY                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELD INPUT                                            *         
***********************************************************************         
VALKEY   LA    R1,WFMKEYH                                                       
         ST    R1,FVADDR                                                        
         L     R2,AKEYTAB          R2=A(VALID KEY INPUT TABLE)                  
         GOTO1 ASCANFLD                                                         
         BH    FVERR               BAD INPUT, FVMSGNO SET BY SCANFLD            
         BE    *+10                GOOD INPUT                                   
         MVC   SCIESEQ,EFFS        NO INPUT - SET DEFAULT END SEQ               
         OC    SCISENO,SCISENO     TEST ACC SYSTEM SPECIFIED                    
         BNZ   VALKEY4             YES                                          
VALKEY2  OC    SCIUSER,SCIUSER     TEST ANY ID SPECIFIED                        
         BNZ   VALKEY4             YES                                          
         CLI   USERSTS,IAMGODQ     NO - SO UNLESS DATA CONTROL                  
         BE    *+10                                                             
         MVC   SCIUSER,USER        DEFAULT TO USER'S OWN ID                     
VALKEY4  CLC   SCIUSER,EFFS                                                     
         BNE   *+10                                                             
         XC    SCIUSER,SCIUSER                                                  
         OI    WFMKEYH+(FVOIND-FVIHDR),FVOXMT                                   
         CLC   KEYFLTS(KEYFLTSL),SCIFLTS                                        
         BNE   *+14                                                             
         CLC   SGUIDTAB,GUIDTAB                                                 
         BE    VALKEYX                                                          
         OI    KEYIND,KEYCHNG      SET KEY HAS CHANGED                          
         CLI   TWASCROV,TWASCLST   TEST LIST SCREEN ALREADY LOADED              
         BNE   VALKEY6                                                          
         LA    R1,WFMLACTH         YES - CLEAR IT                               
         LA    RF,WFMLDTLH+(DISLHDR2-DISLINED)                                  
         SR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         TWAXC (R1),(RF),PROT=Y                                                 
VALKEY6  MVI   DISIND,DISIRST      CLEAR DISIND & SET DISPLAY RESTART           
         MVC   KEYFLTS(KEYFLTSL),SCIFLTS                                        
         MVC   SGUIDTAB,GUIDTAB                                                 
         NI    TWAMODE,TWAMRSRV                                                 
         GOTO1 ATSARINI            RE-INITIALISE TSAR BUFFER                    
VALKEYX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT-ID FIELD INPUT                                      *         
***********************************************************************         
VALREP   XC    PRTSUB,PRTSUB       CLEAR REPORT-ID                              
         GOTO1 AFLDVAL,WFMREPH                                                  
         BH    ERRNONE                                                          
         BL    VALREPX                                                          
         MVC   PRTSUB,FVIFLD       SET REPORT-ID                                
         OI    WFMREPH+(FVOIND-FVIHDR),FVOXMT                                   
VALREPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS FIELD INPUT                                        *         
***********************************************************************         
VALOPT   LA    R1,WFMOPTH                                                       
         ST    R1,FVADDR                                                        
         L     R2,AOPTTAB          R2=A(VALID INPUT OPTIONS TABLE)              
         GOTO1 ASCANFLD                                                         
         BH    FVERR               BAD INPUT, FVMSGNO SET BY SCANFLD            
         BE    *+10                GOOD INPUT                                   
         MVC   SCIESEQ,EFFS        NO INPUT - DEFAULT END SEQUENCE NO           
         OC    SCIUSER,SCIUSER     TEST OPTION 'ID=ALL' SPECIFIED               
         BNO   *+10                                                             
         XC    SCIUSER,SCIUSER     IT'S MEANINGLESS HERE                        
         OI    WFMOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         CLC   OPTRDST(OPTSL-OPTFLTSL),SCIRDST                                  
         BE    *+12                                                             
         OI    DISIND,DISINOSC     SET NO SCROLL                                
         B     VALOPT2                                                          
         CLC   OPTFLTS(OPTFLTSL),SCIFLTS                                        
         BE    *+8                                                              
         MVI   DISIND,DISIRST      CLEAR DISIND & SET DISPLAY RESTART           
VALOPT2  MVC   OPTFLTS(OPTSL),SCIFLTS     MOVE ALL OPTIONS IN                   
         TM    KEYIND,KEYCHNG      TEST KEY CHANGED                             
         BNO   VALOPTX                                                          
         GOTO1 AREADWK             YES - RE/READ WORKER INDEX RECORDS           
         BNE   FVERR               NO INDEXES FOUND/TSAR ERROR                  
*                                  ELSE SET INITIAL SCREEN MSG                  
         MVC   FVMSGNO,=AL2(AI$FSEAP) 'N FILES SELECTED - ENTER ACT/PF'         
         MVI   FVOMTYP,GTMINF                                                   
         XC    FVSUBT,FVSUBT                                                    
         LA    RF,FVSUBT+1                                                      
         CURED (B2,STSARKNO),(5,(RF)),0,ALIGN=LEFT                              
         AH    R0,=Y(1)                                                         
         STC   R0,FVSUBT                                                        
         OI    DISIND,DISIMMSG                                                  
         B     DISCTRL             AND DISPLAY                                  
*                                                                               
VALOPTX  TM    DISIND,DISIRST      TEST DISPLAY-SIGNIFICANT OPTS CHNGED         
         BO    DISLSCR             YES- RE-FILTER DISPLAY WITH NEW OPTS         
         CLI   TWASCROV,TWASCDET   TEST DETAIL SCREEN                           
         BE    VALPFK                                                           
         TM    DISIND,DISINOSC     TEST NO SCROLL                               
         BO    DISCTRL                                                          
         B     VALPFK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PFKEY COMMAND                                              *         
***********************************************************************         
VALPFK   XC    SCRVALS(SCRVALSL),SCRVALS                                        
         L     RF,AINP                                                          
         MVC   WORK(1),TIOBAID-TIOBD(RF)                                        
         CLI   WORK,0              TEST ANY PFKEY HIT                           
         BNE   VALPFK02            YES                                          
         OC    WORK(1),PFKPEND     NO - TEST ANYTHING PENDING                   
         BZ    VALPFK12            NO - NOTHING HIT, NOTHING PENDING            
         MVI   PFKPEND,0                                                        
VALPFK02 CLI   WORK,PFK01                                                       
         BE    *+12                                                             
         CLI   WORK,PFK13                                                       
         BNE   VALPFK04                                                         
         CLI   TWASCROV,TWASCLST   NO ALTERNATE KEYS ON LIST SCREEN             
         BE    VALPFK12                                                         
         XI    TWAMODE,TWAMALTP    FLIP TO/FROM ALTERNATE PFKEYS                
         GOTO1 ABLDPFK                                                          
         LA    R1,WFMOPTH                                                       
         ST    R1,FVADDR                                                        
         B     FVERRX                                                           
*                                                                               
VALPFK04 L     R1,APFKTAB                                                       
         USING PFKTABD,R1          R1=A(SCROLL PFKEY TABLE)                     
VALPFK06 CLI   PFKTNUM,EOT         TEST E-O-T                                   
         BE    VALPFK12            NO ACTION/SCROLL                             
         CLC   PFKTNUM,WORK        MATCH TABLE PFKEY TO INPUT                   
         BNE   VALPFK08                                                         
         IC    RE,TWASCROV                                                      
         SLL   RE,30                                                            
         SRL   RE,30                                                            
         EX    RE,*+8              X'FE' -> X'02', X'FD' -> X'01'               
         BNO   VALPFK08                                                         
         TM    PFKTIND1,0          TEST PFKEY VALID FOR SCREEN                  
         CLI   PFKTIND2,0          TEST PFKEY RESTRICTED                        
         BE    *+14                                                             
         CLC   PFKTIND2,USERSTS                                                 
         BH    VALPFK08            NOT AUTHORISED FOR USER                      
         CLI   PFKTNUM,PFK06       TEST ACTION OR SCROLL                        
         BNH   VALPFK10            ACTION                                       
         MVC   SCRLINDS,PFKTIND1   SET AMOUNT/DIRECTION IND                     
         B     VALPFK12                                                         
VALPFK08 LA    R1,PFKTABL(R1)                                                   
         B     VALPFK06                                                         
*                                                                               
VALPFK10 MVC   HALF,STSARNUM       HALF=TSAR NUMBER OF WORKER INDEX             
         SR    RF,RF                                                            
         ST    RF,FVADDR           CLEAR FVADDR                                 
         ICM   RF,3,PFKTACT        RF=DISP TO ACTION PROCESSING ROUT            
         BAS   RE,WFM(RF)          BRANCH TO ROUT                               
         BNE   FVERR                                                            
         B     VALPFKX             DISPLAY IF NO ERROR                          
*                                                                               
VALPFK12 OC    SCRVALS(SCRVALSL),SCRVALS                                        
         BNZ   VALPFKX                                                          
         MVI   SCRLINDS,SCRLPAGE+SCRLDOWN   SET DEFAULT SCROLL                  
         B     VALPFKX                                                          
*                                                                               
VALPFKX  B     DISCTRL                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CONTROL                                                     *         
***********************************************************************         
DISCTRL  CLI   TWASCROV,0          TEST FIRST TIME DISPLAY                      
         BE    DISLOAD             YES                                          
         TM    TWAMODE,TWAMLISS    TEST LIST SCREEN SAVED                       
         BNO   ACTVAL              NO - VALIDATE MARK FIELD                     
         GOTO1 ADISPLAY            YES - DISPLAY-ONLY DETAIL SCREEN             
         B     FVERR                                                            
*                                                                               
DISLOAD  MVI   TWASCROV,TWASCLST   LOAD LIST SCREEN                             
         GOTO1 AOVRSCR                                                          
*                                  SET DIFFERENT HELP PANEL NOS. IF DDS         
         CLI   USERSTS,DDSUSRQ                                                  
         BL    DISLSCR                                                          
         LA    RF,5                STANDARD HELP NOS. +5 FOR DDS USER           
         CLI   USERSTS,IAMGODQ                                                  
         BL    *+8                                                              
         LA    RF,10               STD HELP NOS. +10 FOR DATA CONTROL           
         LH    R1,DISSCRNL                                                      
         LH    R2,DISLINES                                                      
         LA    RE,WFMLACTX         BUMP ACTION FIELD HELP                       
         IC    R0,0(RE)                                                         
         AR    R0,RF                                                            
         STC   R0,0(RE)                                                         
         AR    RE,R1                                                            
         BCT   R2,*-12             DO FOR EACH ACTION FIELD                     
*                                                                               
DISLSCR  OI    DISIND,DISIRST      START DISPLAY FROM BEGINNING                 
         GOTO1 ADISPLAY                                                         
         TM    DISIND,DISIOFLO     TEST TSAR BUFFER OVERFLOW                    
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$SWFNI)  'SOME WKR FILES NOT INCLUDED'            
         B     FVERR                                                            
         EJECT                                                                  
***********************************************************************         
* RESTORE LIST SCREEN ON EXIT FROM DETAIL SCREEN                      *         
***********************************************************************         
RESLSCR  DS    0H                                                               
         GOTO1 VDMGR,DMCB,DMREAD,TEMPSTR,(1,0),ATIA                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,WFMMSGH                                                       
         LA    R1,SAVLISTL         WFMMSG->TWAD+3072 SAVED                      
         L     RE,ATIA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE TIA SCREEN INTO TWA                     
         LA    R1,WFMMSGH          RE-TRANSMIT LIST SCREEN                      
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ICM   RE,1,0(R1)                                                       
         BZ    *+8                                                              
         BXLE  R1,RE,*-12                                                       
         MVI   1(R1),1             SET INDICS                                   
         MVI   2(R1),1                                                          
         MVI   TWASCROV,TWASCLST           SET LIST SCREEN DISPLAYED            
         NI    TWAMODE,255-TWAMLISS        AND NOT SAVED                        
         NI    WFMKEYH+(FVATRB-FVIHDR),255-FVAPROT                              
         L     RF,AUTL             RESTORE UTL MAP FOR HEADER SWAP              
         MVC   TSCRNE-UTLD(L'TWAUTLSV,RF),TWAUTLSV                              
         MVC   DISLIST(DISLISTL),SDISLIST  RESTORE DISPLAY VARIABLES            
         MVC   DISLCNT,SDISLCNT                                                 
         MVC   OPTFLTS(OPTSL),SLOPTS       AND LIST SCREEN OPTIONS              
         NI    TWAMODE,255-TWAMALTP                                             
         OC    WKRNO,WKRNO           TEST EXIT DIRECTLY FROM HEXDISP            
         BZ    *+8                                                              
         NI    DISIND,255-DISINOSC   SWITCH OFF 'NO SCROLL' REQUEST             
         LH    R1,SACTADR                  SET CURSOR TO ACTION FIELD           
         AR    R1,R6                                                            
         ST    R1,FVADDR                                                        
         B     FVERRX                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE/PROCESS INPUT TO ACTION FIELD                              *         
***********************************************************************         
ACTVAL   LA    R2,DISLIST          R2=A(LIST OF TSAR RECS ON DISPLAY)           
         L     R1,ADISDET1         R1=A(1ST DISPLAY LINE)                       
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT        R0=N'TSAR RECORDS DISPLAYED                  
         BNZ   ACTVAL2                                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NOWFS)                                           
         B     ACTVALX             NONE - NOTHING TO DO                         
ACTVAL2  ST    R1,FVADDR                                                        
         LA    R3,L'FVIHDR(R1)                                                  
         CLI   0(R3),C' '                                                       
         BH    ACTVAL4                                                          
         MVI   0(R3),C' '          SET FUNNY TO SPACE                           
         LA    R2,L'DISLIST(R2)                                                 
         AH    R1,DISSCRNL                                                      
         BCT   R0,ACTVAL2          LOOK AT NEXT                                 
         B     ACTVAL14                                                         
*                                                                               
ACTVAL4  MVC   HALF,0(R2)          SAVE TSARREC NUMBER                          
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FVOMTYP,0                                                        
         MVC   FVMSGNO,=AL2(AE$AIWKF)  'ACTN INVALID ON THIS WKR FILE'          
         TM    TSARDSTS,WKISDELT   TEST USER HAS DELETED FILE                   
         BNZ   ACTVALX             NO ACTIONS POSSIBLE                          
         TM    TSARIND2,TSARNOAC   TEST NO ACTIONS ON THIS FILE                 
         BNZ   ACTVALX                                                          
         USING ACTTABD,R1                                                       
         L     R1,AACTTAB                                                       
         MVC   FVMSGNO,=AL2(EGACTNOR)  'ACTION NOT RECOGNISED'                  
ACTVAL6  CLI   0(R1),EOT                                                        
         BE    ACTVALX             ACTION INVALID                               
         MVC   LAREADDR,ACTION                                                  
         EX    0,LARE                                                           
         CLC   0(1,R3),0(RE)       R3=A(INPUT FIELD)                            
         BNE   ACTVAL8                                                          
         IC    RF,ACTSLVL                                                       
         EX    RF,*+8              TEST STATUS LEVEL OF ACTION                  
         BO    ACTVAL10            USER HAS SUFFICIENT STATUS                   
         TM    USERSTS,0                                                        
ACTVAL8  LA    R1,ACTTABL(R1)                                                   
         B     ACTVAL6                                                          
*                                                                               
ACTVAL10 MVC   FVMSGNO,=AL2(AE$AIWKF)                                           
         ICM   R4,1,ACTINDS        TEST ACTION COMPATIBLE WITH FILE             
         BZ    ACTVAL12            COMPATIBLE WITH ANY FILE                     
         IC    RF,ACTBCCD          GET BRANCH CONDITION CODE                    
         EX    R4,*+8                                                           
         EX    RF,*+8                                                           
         TM    TSARIND1,0                                                       
         NOP   ACTVALX             NO                                           
ACTVAL12 LH    RF,ACTROUT                                                       
         BAS   RE,WFM(RF)          BAS TO ACTION ROUTINE                        
         BNE   ACTVALX                                                          
         OI    DISIND,DISINOSC     DON'T SCROLL                                 
ACTVAL14 GOTO1 ADISPLAY            SCROLL/RE-DISPLAY CURRENT SCREENFULL         
*                                                                               
ACTVALX  B     FVERR                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION ACTIVATE (UNKEEP)                                    *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PACTV    TM    TSARDSTS,WKISACTV+WKISKEEP                                       
         BO    *+12                STATUS MUST BE  ACTIVE,KEEP                  
         TM    TSARDSTS,WKISHOLD   OR HOLD                                      
         BZ    PXITRE                                                           
         NTR1  ,                                                                
         TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PACTV2                                                           
         GOTO1 ACONFRM,AC@ACTV     CONFIRM WILL SET MESSAGE                     
         B     PXITL                                                            
*                                                                               
PACTV2   GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE               CONFIRM SET CC                               
*                                  UPDATE WORKER FILE STATUS                    
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING UKRECD,WKID                                                      
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UKSTAT,WKISACTV+WKISKEEP                                         
         BO    PACTV3                                                           
         TM    UKSTAT,WKISHOLD                                                  
         BO    PACTV3                                                           
         DC    H'0'                FILE STATUS CHANGED BY ANOTHER USER          
*                                  SET STATUS TO ACTIVE                         
PACTV3   TM    TWAMODE2,TWAM2NUP   TEST UPDATE=NO SET                           
         BNZ   PACTV4                                                           
*                                  TURN OFF KEEP STATUS                         
         GOTO1 VDMGR,DMCB,WKUNKP,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  RESTORE FILE STATUS TO ACTIVE                
         GOTO1 VDMGR,DMCB,WKRSTR,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE STATUS IN TSAR RECORD                 
PACTV4   NI    TSARDSTS,255-WKISKEEP                                            
         NI    TSARDSTS,255-WKISHOLD                                            
         OI    TSARDSTS,WKISACTV                                                
         OI    TSARIND1,TSARMKQ                                                 
         L     RE,ATSARBLK                                                      
         USING TSARD,RE                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PXITE                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION COPY                                                 *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
OLD      USING UKRECD,WKID                                                      
NEW      USING UKRECD,WKID2                                                     
*                                                                               
PCOPY    TM    TSARDSTS,255-WKISACTV                                            
         BNZ   PXITRE              MUST BE ACTIVE ONLY FOR COPY                 
         TM    TSARIND2,TSARICOP                                                
         BZ    PXITRE              FILE CANNOT BE COPIED                        
         NTR1  ,                                                                
*                                  READ TSAR WORKER RECORD                      
PCOPY2   GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    OLD.UKINDEX,OLD.UKINDEX READ WORKER INDEX                        
         MVC   OLD.UKBKEY,TSARWKEY                                              
         MVC   OLD.UKFILNO,TSARKSEQ                                             
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    OLD.UKSTAT,255-WKISACTV                                          
         BZ    *+6                                                              
         DC    H'0'                FILE STATUS CHANGED BY ANOTHER USER          
         XC    HALF2,HALF2         INDICATE  FIRST POSTING                      
*                                  READ WORKER POSTING RECORDS                  
PCOPY4   GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BNE   PCOPY6                                                           
*                                                                               
         L     R2,AWKIO                                                         
         LA    R2,4(R2)            BUMP PASE RECORD LENGTH                      
         USING PSHEADD,R2                                                       
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER                          
         BNE   PCOPY4                                                           
         IC    R0,PSHDLEN                                                       
         AR    R2,R0                                                            
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        FOLLOWED BY TRANSACTION ELEMENT              
         BNE   PCOPY4                                                           
*                                                                               
         OC    HALF2,HALF2         IS THIS THE FIRST POSTING?                   
         BZ    *+14                YES, MOVE IN MONTH                           
         CLC   HALF2,TRNMOS        NO, HAS THE POSTING MONTH CHANGED?           
         BNE   *+14                YES , SET INDICATOR AND END SEARCH           
         MVC   HALF2,TRNMOS        NO,MOVE IN NEW MONTH                         
         B     PCOPY4              AND GET NEXT POSTING                         
         MVI   MXMNTH,1            SET MIXED MONTH INDICATOR                    
*                                                                               
PCOPY6   CLI   SCISGN,0            HAS 'NEWMOA= ' BEEN SPECIFIED?               
         BNE   PCOPY8                                                           
         SR    RE,RE               BUILD OPT KEYWORD + POSITION CURSOR          
         LA    RF,WFMOPTH          RF=A(OPTION FIELD)                           
         ICM   RE,1,FVILEN-FVIHDR(RF)  FIND LENGTH OF DATA IN OPTION            
         LA    RE,WFMOPT(RE)                                                    
         BZ    *+12                NO PREVIOUS DATA                             
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         MVC   0(L'AC8NMOA,RE),AC8NMOA                                          
         LA    RE,L'AC8NMOA(RE)                                                 
         CLI   0(RE),C' '          SEARCH FOR END OF 'NEWMOA'                   
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         MVC   0(2,RE),=C'=?'                                                   
*                                                                               
         L     R1,AINP             LOCATE CURSOR                                
         USING TIOBD,R1                                                         
         LH    R0,=Y(WFMOPTH-TWAD)                                              
         STCM  R0,3,TIOBCURD       SET DISPLACEMENT TO FIELD                    
         LA    RF,L'FVIHDR-1(RF)                                                
         SR    RE,RF               CALCULATE LENGTH OF DATA                     
         STC   RE,TIOBCURI         SET DISPLACEMENT INTO FIELD                  
         OI    TIOBINDS,TIOBSETC   POSITION CURSOR                              
         DROP  R1                                                               
*                                                                               
         MVC   FVMSGNO,=AL2(AI$SPM1F)  'SPECIFY MONTHS AS +1/-1'                
         CLI   MXMNTH,1            ARE THERE MIXED MONTHS?                      
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$SPMYF)  NO,'SPECIFY NEW MONTH'                   
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     PXITL                                                            
*                                                                               
PCOPY8   CLI   SCISGN,C'-'         TEST USER SPECIFIED '+1/-1'                  
         BNH   PCOPY10             YES, MOVE IN CONFIRM MESSAGE                 
         CLI   MXMNTH,1            ARE THERE MIXED MONTHS?                      
         BNE   PCOPY10             NO, MOVE IN CONFIRM MESSAGE                  
*                                                                               
         SR    RE,RE                                                            
         LA    RF,WFMOPTH          RF=A(OPTION FIELD)                           
         ICM   RE,1,FVILEN-FVIHDR(RF)  LENGTH OF DATA IN OPTION FIELD           
         LA    RE,WFMOPT(RE)                                                    
         CLI   0(RE),C'='          SEARCH FOR '='                               
         BE    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,L'FVIHDR-1(RF)                                                
         SR    RE,RF                                                            
         L     R1,AINP             LOCATE CURSOR                                
         USING TIOBD,R1                                                         
         LH    R0,=Y(WFMOPTH-TWAD)                                              
         STCM  R0,3,TIOBCURD       SET DISPLACEMENT TO FIELD                    
         STC   RE,TIOBCURI         SET DISPLACEMENT INTO FIELD                  
         OI    TIOBINDS,TIOBSETC   POSITION CURSOR                              
         DROP  R1                                                               
*                                                                               
         MVC   FVMSGNO,=AL2(AI$SPM1F)  'SPECIFY MONTHS AS +1/-1'                
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     PXITL                                                            
*                                                                               
PCOPY10  TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PCOPY12                                                          
         GOTO1 ACONFRM,AC@COPY     CONFIRM WILL SET MESSAGE                     
         B     PXITL                                                            
*                                                                               
PCOPY12  GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE                                                            
*                                  OPEN NEW WORKER FILE                         
         MVC   WKID2,WKID                                                       
         XC    NEW.UKFILNO,NEW.UKFILNO  CLEAR SEQUENCE NUMBER                   
         GOTO1 VDATCON,DMCB,(5,0),(1,WORK)                                      
         MVC   NEW.UKDAY,WORK+2                                                 
         OI    NEW.UKFLAG,UKFSDUPS      ALLOW DUPLICATE KEYS                    
         GOTO1 VDMGR,DMCB,WKOPEN,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    OLD.UKINDEX,OLD.UKINDEX  RE-READ ORIGINAL WORKER INDEX           
         MVC   OLD.UKBKEY,TSARWKEY                                              
         MVC   OLD.UKFILNO,TSARKSEQ                                             
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    OLD.UKSTAT,255-WKISACTV                                          
         BZ    *+6                                                              
         DC    H'0'                FILE STATUS CHANGED BY ANOTHER USER          
*                                  READ RECORDS ON OLD FILE                     
PCOPY14  GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BNE   PCOPY26                                                          
*                                                                               
         L     R2,AWKIO                                                         
         LA    R2,4(R2)            BUMP PAST RECORD LENGTH                      
         USING PSHEADD,R2                                                       
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER                          
         BNE   PCOPY14                                                          
         IC    R0,PSHDLEN                                                       
         AR    R2,R0                                                            
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        FOLLOWED BY TRANSACTION ELEMENT              
         BNE   PCOPY14                                                          
*                                                                               
         CLI   SCISGN,C'+'         DOES NEWMOA=+1?                              
         BE    PCOPY16                                                          
         CLI   SCISGN,C'-'         DOES NEWMOA=-1?                              
         BE    PCOPY20                                                          
*                                  NO, NEWMOA=MMMYY                             
         MVC   TRNMOS(1),SCIMTH    MOVE IN YEAR                                 
         OI    TRNMOS,X'F0'                                                     
         SR    RE,RE                                                            
         IC    RE,SCIMTH+1                                                      
         LA    RE,MOSTAB-1(RE)                                                  
         MVC   TRNMOS+1(1),0(RE)   MOVE IN NEW MONTH                            
         B     PCOPY24                                                          
*                                                                               
PCOPY16  SR    RE,RE               BUMP POSTING MONTH                           
         IC    RE,TRNMOS+1                                                      
         LA    RE,1(RE)                                                         
         STC   RE,TRNMOS+1                                                      
         CLI   TRNMOS+1,X'C4'      DO WE NEED TO BUMP THE YEAR?                 
         BL    PCOPY24                                                          
         BE    PCOPY18                                                          
         CLI   TRNMOS+1,X'FA'                                                   
         BL    PCOPY24                                                          
         MVI   TRNMOS+1,X'C1'      SET MONTH TO OCTOBER                         
         B     PCOPY24             GO AND ADD RECORD                            
*                                                                               
PCOPY18  MVI   TRNMOS+1,X'F1'      SET MONTH TO JANUARY                         
         IC    RE,TRNMOS           ADD POSTING YEAR                             
         LA    RE,1(RE)                                                         
         STC   RE,TRNMOS                                                        
         CLI   TRNMOS,X'FA'        DO WE NEED TO ADD A DECADE?                  
         BL    PCOPY24                                                          
         MVI   TRNMOS,X'F0'        YES, MOVE IN YEAR 0                          
         B     PCOPY24             GO AND ADD RECORD                            
*                                                                               
PCOPY20  SR    RE,RE               DROP POSTING MONTH                           
         IC    RE,TRNMOS+1                                                      
         BCTR  RE,0                                                             
         STC   RE,TRNMOS+1                                                      
         CLI   TRNMOS+1,X'F0'      DO WE NEED TO DROP THE YEAR?                 
         BH    PCOPY24             NO, GO AND ADD RECORD                        
         BE    PCOPY22                                                          
         CLI   TRNMOS+1,X'C0'                                                   
         BH    PCOPY24                                                          
         MVI   TRNMOS+1,X'F9'      SET MONTH TO SEPTEMBER                       
         B     PCOPY24             GO AND ADD RECORD                            
*                                                                               
PCOPY22  MVI   TRNMOS+1,X'C3'      SET MONTH TO DECEMBER                        
         IC    RE,TRNMOS           DROP POSTING YEAR                            
         BCTR  RE,0                                                             
         STC   RE,TRNMOS                                                        
         CLI   TRNMOS,X'F0'        DO WE NEED TO DROP A DECADE?                 
         BH    PCOPY24                                                          
         MVI   TRNMOS,X'F9'        YES, MOVE IN YEAR 9                          
*                                                                               
PCOPY24  TM    TWAMODE2,TWAM2NUP   TEST UPDATE=NO SET                           
         BNZ   PXITE                                                            
*                                  ADD RECORD TO NEW FILE                       
         GOTO1 VDMGR,DMCB,WKADDR,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    PCOPY14                                                          
         DC    H'0'                                                             
*                                                                               
PCOPY26  XC    OLD.UKINDEX,OLD.UKINDEX  RE-READ ORIGINAL WORKER INDEX           
         MVC   OLD.UKBKEY,TSARWKEY                                              
         MVC   OLD.UKFILNO,TSARKSEQ                                             
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  TEST STATUS AGAIN                            
         TM    OLD.UKSTAT,255-WKISACTV                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  CHANGE FILE STATUS TO HOLD                   
         GOTO1 VDMGR,DMCB,WKHOLD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE STATUS IN TSAR                        
         NI    TSARDSTS,255-WKISACTV                                            
         OI    TSARDSTS,WKISHOLD   SET HOLD STATUS                              
         OI    TSARIND1,TSARMKQ                                                 
         NI    TSARIND1,255-TSWKREVQ  TURN OFF REVERSE STATUS                   
         L     R3,ATSARBLK                                                      
         USING TSARD,R3                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         USING UKSOFD,R2           CHANGE COMMENT                               
         L     R2,AWKIO            READ WORKER RECORD 0                         
         XC    0(4,R2),0(R2)                                                    
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING WFMCMTD,R3          BUILD COMMENT                                
         LA    R3,UKSOFCOM                                                      
         MVC   WFMCAREA,SPACES                                                  
         MVI   WFMCACT,C'T'        FILE COPIED TO                               
         LA    RF,WFMCCID          ID OF FILE COPIED TO                         
         UNPK  0(3,RF),NEW.UKDAY(2)   Trick to print PL1(DAY)                   
         AHI   RF,2                                                             
         MVC   0(L'UKCLASS,RF),NEW.UKCLASS                                      
         AHI   RF,1                                                             
         CLI   WKIEXTR2,C' '                                                    
         BNH   PCOPY27                                                          
         MVC   0(L'WKIEXTR2,RF),WKIEXTR2                                        
         AHI   RF,1                                                             
PCOPY27  MVI   0(RF),C','                                                       
         LA    R5,1(RF)                                                         
         CURED (B2,WKISEQN2),(4,(R5)),0,ALIGN=LEFT                              
         GOTO1 VDATCON,DMCB,(2,TODAYC),(16,WFMCDAT)                             
*                                  WRITE BACK COMMENT TO FILE                   
         GOTO1 VDMGR,DMCB,WKCOMT,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AWKIO            READ TRAILER RECORD                          
*        MVC   0(4,R2),(WKRECS-WKRECD)+28(R2)                                   
         MVC   0(4,R2),UKSOFRCT                                                 
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         USING PSSUBFD,R2                                                       
         LA    R2,4(R2)                                                         
         CLI   0(R2),PSSBELQ       ENSURE TRAILER REC FOUND                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PFSTATD,R3          RE-DEFINE TRAILER RECORD                     
         LA    R3,PSSBDESC                                                      
         MVC   PFSTAREA,SPACES                                                  
         MVI   PFSTAREA,X'FF'                                                   
         MVC   PFSTCID(L'PFSTCID),WKISYS2  ID OF FILE COPIED TO                 
         MVC   PFSTCSQ,WKISEQN2    SEQUENCE #                                   
         MVC   PFSTDAT,TODAYC      TODAY'S DATE                                 
         THMS                                                                   
         SRL   R1,12                                                            
         STCM  R1,3,PFSTTIM        TIME                                         
         OI    PFSTIND,PFSTICID    FILE COPIED TO INDICATOR                     
*                                  WRITE BACK TRAILER REC                       
         GOTO1 VDMGR,DMCB,WKWRIT,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  BUILD TRAILER REC FOR NEW FILE               
         MVC   PFSTCID(L'PFSTCID),WKISYS  ID OF FILE COPIED TO                  
         MVC   PFSTCSQ,WKISEQN     SEQUENCE #                                   
         XC    PFSTIND,PFSTIND                                                  
         OI    PFSTIND,PFSTICOF    FILE COPIED FROM INDICATOR                   
         DROP  R3                                                               
*                                  ADD TRAILER TO NEW FILE                      
         GOTO1 VDMGR,DMCB,WKADDR,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  CLOSE NEW FILE                               
         GOTO1 VDMGR,DMCB,WKCLOS,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  READ INDEX ON NEW FILE                       
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AWKIO                                                         
         XC    0(4,R2),0(R2)       CLEAR START OF WKIO                          
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    *+6                                                              
         DC    H'0'                BAD WORKER FILE                              
*                                                                               
         USING UKSOFD,R2                                                        
         USING WFMCMTD,R3          BUILD COMMENT FOR NEW FILE                   
         LA    R3,UKSOFCOM                                                      
         MVC   UKSOFCOM,SPACES                                                  
         MVI   WFMCACT,C'F'        FILE COPIED FROM                             
         LA    RF,WFMCCID          ID OF FILE COPIED FROM                       
         UNPK  0(3,RF),NEW.UKDAY(2)                                             
         AHI   RF,2                                                             
         MVC   0(L'UKCLASS,RF),NEW.UKCLASS                                      
         AHI   RF,1                                                             
         CLI   NEW.UKEXTRA,C' '                                                 
         BNH   PCOPY27B                                                         
         MVC   0(L'UKEXTRA,RF),NEW.UKEXTRA                                      
         AHI   RF,1                                                             
*                                                                               
PCOPY27B MVI   0(RF),C','                                                       
         LA    R5,1(RF)                                                         
         CURED (B2,WKISEQN),(4,(R5)),0,ALIGN=LEFT                               
         GOTO1 VDATCON,DMCB,(2,TODAYC),(16,WFMCDAT)                             
         DROP  R3                                                               
*                                  WRITE BACK COMMENT TO NEW FILE               
         GOTO1 VDMGR,DMCB,WKCOMT,WKFILE,WKID2,AWKIO,AWKBUF2                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  ADD TSAR RECORD FOR NEW FILE                 
         LH    R1,STSARKNO         BUMP AND SAVE KEY SEQUENCE NO.               
         LA    R1,1(R1)                                                         
         STH   R1,STSARKNO                                                      
*        STCM  R1,3,TSARKNO                                                     
         MVC   TSARWKEY,NEW.UKBKEY                                              
         MVC   TSARKSEQ,NEW.UKFILNO                                             
*                                                                               
         MVC   TEMP(TSARRECL),TSARREC    SAVE TSARREC                           
         L     RE,ATSARBLK                                                      
         USING TSARD,RE                                                         
         MVI   TSACTN,TSARDH       CHECK TSAR RECORD ALREADY EXISTS             
         MVI   TSERRS,0                                                         
         GOTO1 VTSAR,TSARD                                                      
         BNE   PCOPY28             END OF FILE                                  
         CLC   TEMP(TSARKEYL),TSARKEY                                           
         BNE   PCOPY28                                                          
         TM    TSARDSTS,WKISDELT   MUST HAVE DELETE STATUS                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         NI    TSARDSTS,255-WKISDELT  NO, TURN OFF DELETE                       
         NI    TSARIND1,255-TSWKPRGQ  AND PURGE STATUS                          
         OI    TSARIND2,TSWKCOPQ   SET FILE IS A COPY                           
         OI    TSARDSTS,WKISACTV   ACTIVE FILE                                  
         MVI   TSACTN,TSAWRT       WRITE BACK TSAR RECORD                       
         GOTO1 VTSAR,TSARD                                                      
         BE    PCOPY30                                                          
         DC    H'0'                                                             
*AH3                                                                            
PCOPY28  MVC   TSARREC(TSARRECL),TEMP   MOVE BACK TSARREC                       
         MVC   TSARDSTS,NEW.UKSTAT                                              
         MVC   TSARDDAT,UKSOFAGD                                                
         MVC   TSARDTIM,UKSOFAGT                                                
         ICM   R1,15,UKSOFRCT      USE REC0 RECORD COUNT INITIALLY              
         BCTR  R1,0                                                             
         CVD   R1,DUB                                                           
         ZAP   TSARDCNT,DUB                                                     
*                                                                               
         OI    TSARIND2,TSWKCOPQ   SET FILE IS A COPY                           
         GOTO1 ATSARADD            ADD RECORD                                   
         BH    ROUTX               TSAR ERROR                                   
*                                                                               
PCOPY30  MVC   FVMSGNO,=AL2(AI$FCENA)  FILE XXXXXXX CREATED                     
         MVI   FVOMTYP,GTMINF                                                   
         XC    FVSUBT,FVSUBT       BUILD SUBSTITUTION TEXT                      
         LA    RF,FVSUBT+1                                                      
         MVC   0(L'TSARKSPG,RF),TSARKSPG                                        
         LA    RF,L'TSARKSPG(RF)                                                
         OC    0(L'TSARKSUB,RF),TSARKSUB                                        
         BNZ   *+8                                                              
         MVI   0(RF),C'*'          DEFAULT                                      
         LA    RF,L'TSARKSUB(RF)                                                
         UNPK  0(3,RF),TSARKDAY(2)                                              
         LA    RF,2(RF)                                                         
         MVC   0(L'TSARKTYP,RF),TSARKTYP                                        
         CLI   TSARKXTR,C' '                                                    
         BNH   PCOPY31                                                          
         LA    RF,1(RF)                                                         
         MVC   0(L'TSARKXTR,RF),TSARKXTR                                        
*                                                                               
PCOPY31  LA    R0,FVSUBT-1                                                      
         SR    RF,R0                                                            
         STC   RF,FVSUBT           PASS FILE ID TO GETTXT                       
         OI    DISIND,DISIMMSG     TELL DISPLAY TO USE MY MESSAGE               
         OI    DISIND,DISIRST      RESTART DISPLAY FROM BEGINING                
         B     PXITE                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION DELETE                                               *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PDELT    TM    TSARDSTS,WKISACTV                                                
         BZ    PXITRE              MUST BE ACTIVE FOR DELETE                    
         NTR1  ,                                                                
         TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PDELT2                                                           
         GOTO1 ACONFRM,AC@DELT     CONFIRM WILL SET MESSAGE                     
         B     PXITL                                                            
*                                                                               
PDELT2   GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE                                                            
*                                  UPDATE WORKER FILE STATUS                    
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UKSTAT,WKISACTV                                                  
         BO    *+6                                                              
         DC    H'0'                FILE STATUS CHANGED BY ANOTHER USER          
*                                  COMMAND IS PURGE                             
         TM    TWAMODE2,TWAM2NUP   TEST UPDATE=NO SET                           
         BNZ   PDELT4                                                           
*                                                                               
         GOTO1 VDMGR,DMCB,WKPRGE,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE STATUS IN TSAR RECORD                 
PDELT4   NI    TSARDSTS,255-WKISACTV                                            
         OI    TSARDSTS,WKISDELT   SET DELETE STATUS                            
         OI    TSARIND1,TSARMKQ+TSWKPRGQ                                        
         L     RE,ATSARBLK                                                      
         USING TSARD,RE                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PXITE                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION KEEP                                                 *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PKEEP    TM    TSARDSTS,255-WKISACTV                                            
         BNZ   PXITRE              MUST BE ACTIVE ONLY FOR KEEP                 
         NTR1  ,                                                                
         TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PKEEP2                                                           
         GOTO1 ACONFRM,AC@KEEP     CONFIRM WILL SET MESSAGE                     
         B     PXITL                                                            
*                                                                               
PKEEP2   GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE                                                            
*                                  UPDATE WORKER FILE STATUS                    
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UKSTAT,255-WKISACTV                                              
         BZ    *+6                                                              
         DC    H'0'                FILE STATUS CHANGED BY ANOTHER USER          
*                                  SET STATUS TO KEEP                           
         TM    TWAMODE2,TWAM2NUP   TEST UPDATE=NO SET                           
         BNZ   PKEEP4                                                           
*                                                                               
         GOTO1 VDMGR,DMCB,WKKEEP,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  UPDATE STATUS IN TSAR RECORD                 
PKEEP4   OI    TSARDSTS,WKISKEEP                                                
         OI    TSARIND1,TSARMKQ                                                 
         L     RE,ATSARBLK                                                      
         USING TSARD,RE                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PXITE                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION PRINT                                                *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PPRNT    STM   RE,RC,12(RD)        NTR1 BY HAND                                 
         LR    R1,RD                                                            
         AH    R1,=Y(REPL)         TO GRAB TEMP W/S THIS LONG                   
         ST    RD,76(R1)                                                        
         LA    R3,72(RD)           R3=A(TEMP W/S)                               
         MVC   0(4,RD),=C'+PPR'                                                 
         LA    RE,72(R1)                                                        
         ST    RE,8(RD)                                                         
         LR    RD,RE                                                            
*                                                                               
         OC    PRTSUB,PRTSUB       TEST REPORT ID PROVIDED                      
         BNZ   PPRNT2                                                           
         MVI   PFKPEND,0                                                        
         CLI   TWASCROV,TWASCDET   TEST DETAIL SCREEN                           
         BNE   *+8                                                              
         MVI   PFKPEND,PFK05       SET PRINT PFKEY PENDING                      
         LA    RF,WFMREPH                                                       
         ST    RF,FVADDR                                                        
         B     PLNTRID             ENTER REPORT ID                              
*                                                                               
PPRNT2   ICM   RF,15,FVADDR        CLEAR ACTION INPUT                           
         BZ    *+8                                                              
         MVI   L'FVIHDR(RF),C' '                                                
         GOTO1 ATSARGET,HALF       GET THE TSAR RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING REPD,R3                                                          
         ICM   R3,8,=C'L'          SET REPORT TYPE 'LISTING'                    
         GOTO1 APRTINI,(R3)        INITIALISE REPORT, FORMAT HEADS              
*                                                                               
         MVI   REPSUBPG,1          SUBPROG FOR WKFILE LISTING                   
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         ZAP   WKFILDR,PZERO                                                    
         ZAP   WKFILCR,PZERO                                                    
*                                  READ WORKER POSTING RECORDS                  
PPRNT4   GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BNE   PPRNT12                                                          
         L     R2,AWKIO                                                         
         LA    R2,4(R2)            BUMP PAST RECORD LENGTH                      
         USING PSHEADD,R2                                                       
         CLI   PSHDEL,PSSBELQ      TEST TRAILER RECORD                          
         BE    PPRNT12             END OF WORKER FILE                           
         CLI   PSHDEL,PSTKELQ                                                   
         BE    PPRNT09                                                          
         CLI   PSHDEL,PSHDELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AFILTER             CHECK AGAINST OPTION FILTERS                 
         BNE   PPRNT4                                                           
         MVC   PACT,PSHDACC+(ACTKUNT-ACTKEY)                                    
         MVC   PCAC(L'PCAC-1),PSHDSBAC+1                                        
         CLI   PSHDSBAC,C'*'                                                    
         BE    *+12                                                             
         CLI   PSHDSBAC,C' '                                                    
         BNE   *+10                                                             
         MVC   PCAC,PSHDSBAC                                                    
         MVC   PWRK1,PSHDANAL                                                   
         SR    R0,R0                                                            
         IC    R0,PSHDLEN                                                       
         AR    R2,R0                                                            
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   PPRNT10                                                          
         MVC   PWRK2,TRNOFFC                                                    
         CLC   PWRK1,SPACES                                                     
         BE    PPRNT6                                                           
         CLC   PWRK2,SPACES                                                     
         BE    PPRNT6                                                           
         MVI   PDLM,C'/'                                                        
PPRNT6   GOTO1 VDATCON,DMCB,(1,TRNDATE),(17,PDAT)                               
         MVC   PREF,TRNREF                                                      
         MVC   PBAT,TRNBTCH                                                     
         IC    R0,TRNLN            GET L'NARRATIVE                              
         SH    R0,=Y(TRNNARR+1-TRNELD)                                          
         BM    PPRNT8              NONE                                         
         LR    RF,R0                                                            
         CLC   TRNOFFC,=C'99'                                                   
         BNE   *+8                                                              
         LA    RF,14               FIXED FOR BILLS (CRAP BEYOND)                
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TRNNARR                                                  
         GOTO1 VCHOPPER,DMCB,(L'WORK,WORK),(L'PNAR,PNAR),1                      
PPRNT8   LA    RE,WKFILDR          ADD TRNAMNT INTO DR/CR TOTALS                
         LA    RF,PDEB             AND EDIT OUT TO PRINT LINE                   
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+12                                                             
         LA    RE,WKFILCR                                                       
         LA    RF,PCRD                                                          
         AP    0(L'WKFILDR,RE),TRNAMNT                                          
         CURED (P6,TRNAMNT),(L'PDEB,(RF)),2,MINUS=YES                           
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,=P'-1'                                                       
         B     PPRNT10                                                          
*                                                                               
         USING PSTKEYD,R2                                                       
PPRNT09  LA    R2,PSTKEY                                                        
         USING TRNRECD,R2                                                       
         MVC   PACT,TRNKULA                                                     
         MVC   PCAC(L'PCAC-1),TRNKULC                                           
         MVC   PWRK1,TRNKOFF                                                    
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(17,PDAT)                              
         MVC   PREF,TRNKREF                                                     
PPRNT10  GOTO1 VREPORT,REPD        PRINT DETAIL LINE                            
         B     PPRNT4                                                           
*                                  BUILD TOTALS LINE                            
PPRNT12  GOTO1 VREPORT,REPD        PRINT BLANK LINE                             
         MVC   PNAR(L'LC@TRX),LC@TRX                                            
         LA    RF,PNAR+L'LC@TRX+1                                               
         CURED TSARDCNT,(L'LC@RCS,(RF)),0,ALIGN=LEFT                            
         MVC   PNAR+26(L'LC@CTO),LC@CTO                                         
         CURED WKFILDR,(13,PDEB),2,MINUS=YES                                    
         CURED WKFILCR,(13,PCRD),2,MINUS=YES                                    
         GOTO1 VREPORT,REPD        PRINT TOTAL LINE                             
*                                                                               
         GOTO1 APRTCLO,(R3)        CLOSE REPORT, BUILD SUBTEXT FOR MSG          
*                                                                               
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(AI$RPSPL)  REPORT XXX,999 HAS BEEN SPOOLED          
         OC    OPTRDST,OPTRDST                                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$RPSPU)  REPORT XXX,999 SPOOLED TO USERID         
         LA    R1,WFMOPTH                                                       
         OI    DISIND,DISIMMSG     TELL DISPLAY TO USE MY MSG                   
         B     PXITE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION REVERSE                                              *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PREVS    TM    TSARDSTS,255-WKISACTV                                            
         BNZ   PXITRE              MUST BE ACTIVE ONLY                          
         TM    TSARIND2,TSARNORV                                                
         BNZ   PXITRE              NON-REVERSABLE FILE                          
         NTR1  ,                                                                
         TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PREVS2                                                           
         GOTO1 ACONFRM,AC@REVS     CONFIRM WILL SET MESSAGE                     
         B     PXITL                                                            
*                                                                               
PREVS2   GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE                                                            
*                                  READ TSAR WORKER RECORD                      
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UKSTAT,255-WKISACTV                                              
         BZ    *+6                                                              
         DC    H'0'                FILE STATUS CHANGED BY ANOTHER USER          
         MVI   BYTE,0                                                           
         SR    R0,R0                                                            
*                                                                               
         TM    TWAMODE2,TWAM2NUP   TEST UPDATE=NO SET                           
         BNZ   PREVS16                                                          
*                                  READ WORKER POSTING RECORDS                  
PREVS4   GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BNE   PREVS14                                                          
*                                                                               
         L     R3,AWKIO                                                         
         LA    R3,4(R3)            BUMP PAST RECORD LENGTH                      
         USING PSHEADD,R3                                                       
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER                          
         BNE   PREVS4                                                           
         IC    R0,PSHDLEN                                                       
         AR    R3,R0                                                            
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ        FOLLOWED BY TRANSACTION ELEMENT              
         BNE   PREVS4                                                           
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,=P'-1'          REVERSE AMOUNT                               
         ZAP   TRNAMNT,DUB                                                      
*                                  LOOK FOR SUB CASH/MEDIA INTERFACE EL         
PREVS6   IC    R0,TRNLN                                                         
         AR    R3,R0                                                            
         CLI   TRNEL,0                                                          
         BE    PREVS12                                                          
         CLI   TRNEL,AFCELQ                                                     
         BE    PREVS7                                                           
         CLI   TRNEL,MDTELQ                                                     
         BE    PREVS8                                                           
         CLI   TRNEL,SCIELQ                                                     
         BNE   PREVS6                                                           
         USING SCIELD,R3                                                        
         ZAP   DUB,SCIAMNT                                                      
         MP    DUB,=P'-1'          REVERSE THAT TOO                             
         ZAP   SCIAMNT,DUB                                                      
         CLI   SCILN,SCILN2Q       TEST LONG EL                                 
         BL    PREVS6                                                           
         ZAP   DUB,SCINET                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   SCINET,DUB                                                       
         B     PREVS6                                                           
*                                  FOREIGN CURRENCY ELEMENT                     
         USING AFCELD,R3                                                        
PREVS7   ZAP   DUB,AFCAMNT                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   AFCAMNT,DUB                                                      
         B     PREVS6                                                           
*                                                                               
         USING MDTELD,R3                                                        
PREVS8   LA    R0,6                                                             
         LA    RE,MDTGRS           REVERSE ALL TOTALS                           
PREVS10  ICM   RF,15,0(RE)                                                      
         BZ    *+10                IF NOT ZERO                                  
         LCR   RF,RF               = *-1                                        
         STCM  RF,15,0(RE)                                                      
         LA    RE,L'MDTGRS(RE)                                                  
         BCT   R0,PREVS10                                                       
         ICM   RF,15,MDTVAT                                                     
         LCR   RF,RF                                                            
         STCM  RF,15,MDTVAT                                                     
         B     PREVS6                                                           
*                                  WRITE BACK WORKER POSTING RECORD             
PREVS12  GOTO1 VDMGR,DMCB,WKWRIT,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE,1              SET POSTING REVERSED FLAG                    
         B     PREVS4                                                           
*                                  UPDATE WORKER TRAILER RECORD                 
PREVS14  CLI   BYTE,1              TEST ANY POSTINGS REVERSED                   
         BNE   PHERRNP                                                          
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AWKIO            READ WORKER RECORD 0                         
         XC    0(4,R3),0(R3)                                                    
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING UKSOFD,R3                                                        
*                                                                               
         USING WFMCMTD,R2          BUILD COMMENT FOR NEW FILE                   
         LA    R2,UKSOFCOM                                                      
         MVC   WFMCAREA,SPACES                                                  
         TM    TSARIND1,TSWKREVQ   TEST FILE WAS ALREADY REVERSED               
         BO    PREVS15                                                          
         MVI   WFMCACT,C'R'        FILE IS A REVERSAL                           
         GOTO1 VDATCON,DMCB,(2,TODAYC),(16,WFMCDAT)                             
         DROP  R2                                                               
*                                  WRITE BACK COMMENT TO NEW FILE               
PREVS15  GOTO1 VDMGR,DMCB,WKCOMT,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*AH3                               READ WORKER TRAILER REC                      
         L     R3,AWKIO                                                         
*        MVC   0(4,R2),(WKRECS-WKRECD)+28(R2)                                   
         MVC   0(4,R3),UKSOFRCT                                                 
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         USING PSSUBFD,R3                                                       
         LA    R3,4(R3)            BUMP PAST RECORD LENGTH                      
         CLI   0(R3),PSSBELQ       ENSURE TRAILER REC FOUND                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PFSTATD,RE          RE-DEFINE TRAILER RECORD                     
         LA    RE,PSSBDESC                                                      
         MVC   PFSTAREA,SPACES                                                  
         MVI   PFSTAREA,X'FF'                                                   
         MVC   PFSTDAT,TODAYC      TODAY'S DATE                                 
         THMS                                                                   
         SRL   R1,12                                                            
         STCM  R1,3,PFSTTIM        TIME                                         
         OI    PFSTIND,PFSTIREV    FILE IS A REVERSAL                           
*                                  WRITE BACK TRAILER REC                       
         ZAP   DUB,PSSBCASH        REVERSE CONTROL TOTAL                        
         MP    DUB,=P'-1'                                                       
         ZAP   PSSBCASH,DUB                                                     
         ZAP   TSARDCSH,DUB        ON TSAR RECORD TOO                           
         GOTO1 VDMGR,DMCB,WKWRIT,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PREVS16  XI    TSARIND1,TSWKREVQ   FLIP REVERSE STATUS                          
         OI    TSARIND1,TSARMKQ                                                 
         NI    TSARIND2,255-TSARNCTQ   SET TRAILER RECORD WAS READ              
         L     RE,ATSARBLK                                                      
         USING TSARD,RE                                                         
         MVI   TSACTN,TSAPUT       WRITE BACK TSAR RECORD                       
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PXITE                                                            
         DROP  R3,RE                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION SELECT                                               *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PSELC    NTR1                                                                   
         L     R1,FVADDR           CLEAR ACTION FIELD                           
         MVC   L'FVIHDR(L'WFMLACT,R1),SPACES                                    
         SR    R1,R6               CALC DISPLACEMENT TO ACTION FIELD            
         STH   R1,SACTADR                                                       
         GOTO1 VDMGR,DMCB,DMWRT,TEMPSTR,(1,0),WFMMSGH     SAVE LIST             
         BE    *+6                                        SCREEN                
         DC    H'0'                                                             
*                                                                               
         OI    TWAMODE,TWAMLISS    SET HEADER SCREEN SAVED                      
         OI    TWAMODE,TWAMALTP    SET ACTION PFKEYS TO BE DISPLAYED            
         MVC   SDISLIST,DISLIST    SAVE DISLIST                                 
         MVC   SDISLCNT,DISLCNT    AND DISPLAY LINE COUNT                       
         MVC   SLOPTS,OPTFLTS      AND LIST SCREEN OPTIONS                      
         MVC   WFMOPT,SPACES       CLEAR OPTIONS FIELD                          
         XC    OPTFLTS(OPTSL),OPTFLTS                                           
         MVC   OPTESEQ,EFFS        RE-INITIALISE OPTION FILTERS                 
         MVI   TWASCROV,TWASCDET                                                
         GOTO1 AOVRSCR             OVERLAY DETAIL SCREEN                        
         OI    WFMKEYH+(FVATRB-FVIHDR),FVAPROT                                  
         GOTO1 ATSARGET,HALF       GET THE TSAR RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   STSARNUM,HALF       SAVE TSAR INDEX NUMBER                       
         ZAP   DUB,TSARDCNT        SAVE REC COUNT FOR DISPLAY CONTROL           
         CVB   R0,DUB                                                           
         STH   R0,DISWMAX                                                       
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   WKIPRG,=C'DJ'       TEST NEW DAILY JOURNAL                       
         BNE   PSELC2                                                           
         ZAP   TSARDCNT,PZERO      NO TRAILER - SO CLEAR TOTALS                 
         ZAP   TSARDCSH,PZERO                                                   
         NI    TSARIND2,255-TSARNCTQ  AND STATUS                                
PSELC2   TM    TSARIND2,TSARNCTQ   TEST WORKER TRAILER REC ALREADY READ         
         BZ    PSELC6                                                           
         OI    TSARIND1,TSWKBADQ   NO - PRESET BAD STATUS                       
         NI    TSARIND2,255-TSARNCTQ                                            
         L     RE,AWKIO            SET TO READ SUB-FILE (TRAILER) REC           
         ZAP   DUB,TSARDCNT                                                     
         CVB   R1,DUB                                                           
         LA    R1,1(R1)                                                         
         STCM  R1,15,0(RE)                                                      
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                TRAILER NOT FOUND                            
*                                                                               
         L     RE,AWKIO            LOCATE SUB-FILE ELEMENT                      
         LA    RE,4(RE)            BUMP PAST RECORD LENGTH                      
         USING PSSUBFD,RE                                                       
         CLI   0(RE),PSSBELQ       TEST SUB-FILE                                
         BE    *+6                                                              
         DC    H'0'                INVALID WORKER FILE RECORD                   
*                                                                               
         CP    TSARDCNT,PSSBRECS                                                
         BNE   *+8                 WKRECS NEQ PSSBRECS - STUFFED                
         NI    TSARIND1,255-TSWKBADQ                                            
         ZAP   TSARDCSH,PSSBCASH   GET WORKER FILE CONTROL TOTAL                
*                                                                               
         L     RE,ATSARBLK         RE-WRITE TSAR RECORD                         
         USING TSARD,RE                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
         GOTO1 ATSARGET,HALF       THEN GET IT AGAIN                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSELC6   XC    TEMP(L'WFMDWID),TEMP   BUILD WORKER-ID LINE                      
         MVC   TEMP(L'LC@WKY),LC@WKY                                            
         LA    RF,TEMP+L'LC@WKY-1                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
*                                  ASSEMBLE KEY                                 
         CLC   TSARKUID,TWAUSRID   TEST FILE UID=CONNECTED UID                  
         BE    PSELC8              YES                                          
         MVC   0(L'TSARDUNM,RF),TSARDUNM  NO - PREFIX KEY WITH FILE UID         
         LA    RF,L'TSARDUNM-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         MVI   0(RF),0                                                          
         BCT   RF,*-12                                                          
         MVI   1(RF),C','                                                       
         LA    RF,2(RF)                                                         
PSELC8   MVC   0(L'TSARKSPG,RF),TSARKSPG                                        
         LA    RF,L'TSARKSPG(RF)                                                
         OC    0(L'TSARKSUB,RF),TSARKSUB                                        
         BNZ   *+8                                                              
         MVI   0(RF),C'*'          DEFAULT                                      
         LA    RF,L'TSARKSUB(RF)                                                
         UNPK  0(3,RF),TSARKDAY(2)   CHEAT A LITTLE TO EXTRACT DAY              
         AHI   RF,2                                                             
         MVC   0(L'TSARKTYP,RF),TSARKTYP                                        
         CLI   TSARKXTR,C' '       WAS THERE AN EXTRA LETTER?                   
         BNH   PSELC8A             NO                                           
         AHI   RF,1                                                             
         MVC   0(L'TSARKXTR,RF),TSARKXTR                                        
PSELC8A  AHI   RF,1                                                             
         MVI   0(RF),C','                                                       
         LA    R2,1(RF)                                                         
         CURED (B2,TSARKSEQ),(4,(R2)),0,ALIGN=LEFT                              
         GOTO1 AGETWKN,TSARWKEY    GET FILE NAME                                
         LA    R1,1(R2)                                                         
         AR    R1,R0               R1=A(FILE NAME POSITION IN P)                
         ST    R1,FULL2                                                         
         L     RE,FULL             RE=A(FILE NAME)                              
         MVC   2(L'LC@PGA08,R1),0(RE)                                           
*                                                                               
         L     R2,AWKIO            READ WORKER RECORD 0                         
         XC    0(4,R2),0(R2)                                                    
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING UKSOFD,R2                                                        
         USING WFMCMTD,R3                                                       
         LA    R3,UKSOFCOM         COMMENT FILED                                
         CLC   UKSOFCOM,SPACES     IS THERE A COMMMENT?                         
         BNH   PSELC16             No                                           
         L     R1,FULL2            YES,BUILD COMMENT TO PRINT                   
         LA    R1,PRGTNAML+2(R1)   R1=A(POSITION OF COMMENT ON WFMDWID)         
         CLI   WFMCACT,C'T'        HAS THE FILE BEEN COPIED TO?                 
         BE    PSELC10                                                          
         CLI   WFMCACT,C'F'        HAS THE FILE BEEN COPIED FROM?               
         BE    PSELC12                                                          
         MVC   0(L'LC@RVRON,R1),LC@RVRON  FILE IS A REVERSAL                    
         LA    R1,L'LC@RVRON+2(R1)                                              
         B     PSELC14                                                          
PSELC10  MVC   0(L'LC@COPTO,R1),LC@COPTO  'COPIED TO'                           
         LA    R1,L'LC@COPTO+1(R1)                                              
         B     *+14                                                             
PSELC12  MVC   0(L'LC@COPFR,R1),LC@COPFR  'COPIED FROM'                         
         LA    R1,L'LC@COPFR+1(R1)                                              
         MVC   0(L'WFMCCID,R1),WFMCCID  ID OF FILE COPIED TO/FROM               
         LA    R1,L'WFMCCID+1(R1)                                               
         MVC   0(L'LC@ON,R1),LC@ON                                              
         LA    R1,L'LC@ON+1(R1)                                                 
PSELC14  MVC   0(L'WFMCDAT,R1),WFMCDAT  DATE OF ACTION                          
*                                                                               
PSELC16  MVC   WFMDWID,TEMP                                                     
         OI    WFMDWIDH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    WFMDWIDH+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
         XC    TEMP(L'WFMDTOT),TEMP   BUILD TOTALS LINE                         
         MVC   TEMP(L'LC@TRX),LC@TRX                                            
         LA    RF,TEMP+L'LC@TRX-1                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='          FLOAT IN '=' AFTER LAST CHAR                 
         LA    RF,2(RF)                                                         
         CURED TSARDCNT,(L'LC@RCS,(RF)),0,ZERO=BLANK,ALIGN=LEFT                 
*                                                                               
         LA    RF,TEMP+L'WFMDTOT-(L'LC@AMT+L'LC@CTO+4)                          
         MVC   0(L'LC@CTO,RF),LC@CTO  RIGHT ALIGN CONTROL TOTAL                 
         LA    RE,L'LC@CTO-1(RF)                                                
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'='                                                       
         LA    RF,L'LC@CTO+1(RF)                                                
         CURED TSARDCSH,(L'LC@AMT,(RF)),2,MINUS=YES                             
*                                                                               
         MVC   WFMDTOT,TEMP                                                     
         OI    WFMDTOTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    WFMDTOTH+(FVATRB-FVIHDR),FVAPROT+FVAHIGH                         
         OI    DISIND,DISIRST      SET START DISPLAY AT BEGINNING               
         B     PXITE                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION TRACE - ADDTRN HOOK FOR DUMMY UPDATES                *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PTRCE    TM    TSARDSTS,255-WKISACTV    DUMMY ROUTINE                           
         BNZ   PXITRE              MUST BE ACTIVE/REVERSED ONLY                 
         NTR1  ,                                                                
         TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PTRCE2                                                           
         GOTO1 ACONFRM,AC@TRCE     CONFIRM WILL SET MESSAGE                     
         B     PXITL                                                            
*                                                                               
PTRCE2   GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE                                                            
*                                  TRACE CODE TO BE ADDED HERE                  
         B     PXITE                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTION UPDATE                                               *         
* NTRY HALF=TSAR RECORD NUMBER OF SELECTED INDEX                      *         
***********************************************************************         
PUPDT    TM    TSARDSTS,255-WKISACTV                                            
         BNZ   PXITRE              MUST BE ACTIVE/REVERSED ONLY                 
*                                                                               
         STM   RE,RC,12(RD)        NTR1 BY HAND                                 
         LR    R1,RD                                                            
         AH    R1,=Y(REPL)         TO GRAB TEMP W/S THIS LONG                   
         ST    RD,76(R1)                                                        
         LA    R3,72(RD)           R3=A(TEMP W/S)                               
         MVC   0(4,RD),=C'+PUP'                                                 
         LA    RE,72(R1)                                                        
         ST    RE,8(RD)                                                         
         LR    RD,RE                                                            
*                                                                               
         TM    TWAMODE,TWAMCFRM                                                 
         BNZ   PUPDT6                                                           
         CLI   USERSTS,IAMGODQ     TEST MAXIMUM STATUS                          
         BNL   PUPDT2              YES - CAN UPDATE (ALMOST) ANYTHING           
         TM    TSARIND1,TSARUPDQ   NO - 'UPDATEABLE' BIT MUST BE SET            
         BNO   PXITH                                                            
         CLC   TSARSENO,SENO       AND BE FOR THE CONNECTED ACC SYSTEM          
         BNE   PXITH                                                            
*                                  TEMPORARILY, UNTIL GRP-IDS INSTALLED         
         CLC   TSARKUID,USER       AND FOR THE CONNECTED USER                   
         BNE   PXITH                                                            
*                                                                               
PUPDT2   MVI   EXITFLAG,0          INITIALISE INDICATOR                         
         CP    TSARDCNT,UPDMAX     TEST RECORD COUNT                            
         BH    PHERRBP             TOO MANY POSTINGS FOR ONLINE UPDATE          
*                                                                               
         OC    PRTSUB,PRTSUB       TEST REPORT ID PROVIDED                      
         BNZ   PUPDT4                                                           
         LA    RF,WFMREPH                                                       
         ST    RF,FVADDR                                                        
         B     PLNTRID             ENTER REPORT ID                              
*                                                                               
PUPDT4   GOTO1 ACONFRM,AC@UPDT     ELSE SET CONFIRMATION MESSAGE                
         B     PXITL                                                            
*                                                                               
PUPDT6   GOTO1 ACONFRM             GET USER RESPONSE                            
         L     R1,FVADDR                                                        
         MVI   L'FVIHDR(R1),C' '   CLEAR ACTION INPUT                           
         BNE   PXITE               CONFIRM SET CC                               
*                                  READ TSAR RECORD                             
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDMGR,DMCB,WKBINI,WKFILE,WKID,AWKIO,AWKBUF1                      
         L     RF,AWKBUF1                                                       
         MVC   WKBDSP,8(RF)        GET DISP TO WORKER SAVE AREA                 
*                                                                               
         MVC   STSARUID,TSARKUID   SAVE USER-ID FROM WORKER FILE                
         XC    WKID,WKID           READ WORKER INDEX                            
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    WKISTAT,255-WKISACTV     VERIFY FILE STATUS                      
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$FCAAT)   UPDATE CANCELLED -                      
         B     PHERRUP             FILE STATUS CHANGED AT ANOTHER TERM          
*                                                                               
         USING REPD,R3                                                          
         ICM   R3,8,=C'A'          SET REPORT TYPE 'ANALYSIS'                   
         GOTO1 APRTINI,(R3)        INITIALISE REPORT, FORMAT HEADS              
*                                                                               
         MVI   REPSUBPG,2          SUBPROG FOR UPDATE LISTING                   
         GOTO1 VREPORT,REPD                                                     
*                                                                               
         MVC   STSARNUM,DISTMAX    SAVE CURRENT TSAR RECORD COUNT               
         LH    RF,DISTMAX                                                       
         LA    RF,1(RF)                                                         
         STH   RF,TSTRNLO                 AND COUNT+1                           
         MVC   WORK(L'TSARWKEY),TSARWKEY  AND WKID                              
         MVC   WORK+L'TSARWKEY(L'TSARKSEQ),TSARKSEQ                             
         MVC   CHAR,TSARSENO              AND PHYSICAL SYSTEM NO.               
         MVI   ANYADD,0                                                         
         SR    R2,R2                                                            
         L     R4,AWKIO                                                         
         LA    R4,4(R4)                                                         
PUPDT8   GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BNE   PUPDT10                                                          
*                                                                               
         USING PSHEADD,R4                                                       
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER                          
         BNE   PUPDT8                                                           
         SR    RF,RF                                                            
         IC    RF,PSHDLEN                                                       
         LA    RF,0(RF,R4)                                                      
         CLI   0(RF),TRNELQ        FOLLOWED BY TRANSACTION ELEMENT              
         BNE   PUPDT8                                                           
*                                  BUILD TSAR TRANSACTION RECORD                
         XC    TSARREC(TSARRECL),TSARREC                                        
         MVC   TSARKNO,EFFS                                                     
         MVC   TSARTACC,PSHDACC                                                 
         MVC   TSARTANL,PSHDANAL                                                
         MVC   TSARTSBA,PSHDSBAC                                                
         LR    RF,R2                                                            
         LA    R2,1(R2)            BUMP WORKER RECORD NUMBER                    
         STCM  R2,15,TSARTRNO                                                   
         AH    RF,TSTRNLO                                                       
         STH   RF,TSTRNHI          BUMP HIGHEST TSAR TRANS REC NUMBER           
         L     RF,AWKBUF1                                                       
         A     RF,WKBDSP           RF=A(SAVE AREA IN WORKER BUFFER)             
         USING SKBUFFD,RF                                                       
         MVC   TSARTDAB,SKADDR     SAVE DISK ADDRESS OF FILE BLOCK              
         LH    RE,SKDISP                                                        
         SH    RE,SKLEN                                                         
         STH   RE,TSARTDSP         SAVE DISP INTO BLOCK OF CURRENT REC          
         DROP  RF                                                               
         GOTO1 ATSARADD                                                         
         BE    PUPDT8                                                           
         MVC   DISTMAX,STSARNUM    RESTORE RECORD COUNT                         
         MVC   FVMSGNO,=AL2(AE$BFCBP)                                           
         MVI   EXITFLAG,1          SET NON-ZERO FOR ERROR EXIT                  
         B     PUPDT22             CLEAR TSAR TRANSACTION RECS                  
*                                                                               
PUPDT10  OC    TSTRNHI,TSTRNHI     TEST ANYTHING ADDED                          
         BZ    PHERRNP             NO POSTINGS TO ADD                           
         USING TRNBLK,R2                                                        
         L     R2,ATRNBLK          INIT BLOCK FOR ADDTRN                        
         MVC   TRNCTRY,AGYCTRY                                                  
         MVC   TRNCOMF,ACOM                                                     
         MVC   TRNPUSER,STSARUID   WORKER FILE'S USER-ID                        
         MVC   TRNUDATE,TODAYC                                                  
         MVC   TRNCPYS1,COMPSTA1                                                
         MVC   TRNCPYS2,COMPSTA2                                                
         MVC   TRNCPYS3,COMPSTA3                                                
         MVC   TRNCPYS4,COMPSTA4                                                
         MVC   TRNCPYS5,COMPSTA5                                                
         MVC   TRNCPYS6,COMPSTA6                                                
         MVC   TRNCPYS7,COMPSTA7                                                
         MVC   TRNCPYS8,COMPSTA8                                                
         MVC   TRNCPYS9,COMPSTA9                                                
         MVC   TRNCPYSA,COMPSTAA                                                
         MVC   TRNGLMOA,COMPGMOA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                                                               
         L     RF,AIOAS            SUPPLY BUFFER ADDRESSES                      
         LA    RF,L'IOSAVE(RF)                                                  
         STCM  RF,15,TRNREC        TRANSACTION BUILT IN IOA1                    
         LA    RF,IOALQ(RF)                                                     
         STCM  RF,15,TRNACC        ACCOUNT BUFFER = IOA2                        
         LA    RF,IOALQ(RF)                                                     
         STCM  RF,15,TRNCAC        CONTRA BUFFER  = IOA3                        
         XC    SAVEACC,SAVEACC                                                  
         XC    WKID,WKID           RE-READ WORKER INDEX                         
         MVC   UKBKEY,WORK                                                      
         MVC   UKFILNO,WORK+L'TSARWKEY                                          
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,WKIKEY                                                        
         ICM   R1,8,EFFS                                                        
         GOTO1 AGETWKN             GET DD EQUATE FOR PROGRAM NAME               
         MVC   SAVEDDQ,FULL        SAVE EQUATE FOR WFMEL                        
*                                                                               
         GOTO1 VGETFACT,DMCB,0     SAVE SYSTEM INPUT NUMBER                     
         L     R1,0(R1)                                                         
         MVC   SAVESIN,FASIN+1-FACTSD(R1)                                       
******                             *TEMPORARY TEST CODE*                        
         XC    TEMP,TEMP                                                        
         LA    RF,TEMP+4                                                        
         STCM  RF,15,TEMP                                                       
******                                                                          
         MVC   HALF2,TSTRNLO       SET HALF2 FOR TSAR READ                      
         ZAP   WKLDGDR,PZERO       INITIALISE REPORT ACCUMULATORS               
         ZAP   WKLDGCR,PZERO                                                    
         ZAP   WKUNTDR,PZERO                                                    
         ZAP   WKUNTCR,PZERO                                                    
         ZAP   WKFILDR,PZERO                                                    
         ZAP   WKFILCR,PZERO                                                    
         XC    SAVEACC,SAVEACC                                                  
         XC    FVMSGNO,FVMSGNO                                                  
*                                                                               
         GOTO1 VGETFACT,DMCB,0     SAVE SYSTEM INPUT NUMBER                     
         L     R1,0(R1)                                                         
         MVC   SAVESIN,FASIN+1-FACTSD(R1)                                       
*                                                                               
         XC    FULL,FULL                                                        
         CLC   CHAR,SENO           TEST UPDATE TO CONNECTED ACC SYSTEM          
         BE    PUPDT12             YES                                          
         XC    DMCB(8),DMCB        NO - SWITCH TO IT                            
         MVC   DMCB(1),CHAR                                                     
         MVC   DMCB+1(3),EFFS                                                   
         GOTO1 VSWITCH,DMCB                                                     
         CLI   DMCB+4,1                                                         
         BL    PUPDT12             SWITCH SUCCESSFUL                            
         BH    PHERRNS             SYSTEM NOT STARTED                           
         DC    H'0'                INVALID SYSTEM                               
*                                                                               
*                                  READ BACK TSAR TRANSACTION RECS AND          
*                                  UPDATE ACC FILE - ABEND ON ANY ERROR         
PUPDT12  MVI   EXITFLAG,UPDERROR   PRESET UPDATE ERROR                          
         GOTO1 ATSARGET,HALF2                                                   
         BNE   PUPDT22             ERROR                                        
         CLC   TSARKNO,EFFS        TEST TRANSACTION TYPE RECORD                 
         BNE   PUPDT22             ERROR                                        
*                                  READ WKR POSTING RECS IN ACCFILE SEQ         
         CLC   FULL,TSARTDAB       TEST BLOCK SAME AS PREVIOUS                  
         BE    PUPDT14                                                          
         MVC   FULL,TSARTDAB       NO - READ IT                                 
*                                  READ FILE BLOCK TO IMPROVE HIT-RATE          
         GOTO1 VDMGR,DMCB,DMREAD,WKFILE,FULL,AWKBUF1                            
         BE    *+6                                                              
         DC    H'0'                                                             
PUPDT14  L     R4,AWKBUF1                                                       
         AH    R4,TSARTDSP         DISPLACE INTO BUFFER FOR RECORD              
         LA    R4,4(R4)                                                         
         USING PSHEADD,R4                                                       
*                                                                               
         MVC   TRNCACNM,PSHDSBNM   SUPPLY CONTRA ACCOUNT NAME                   
         OC    SAVEACC,SAVEACC     TEST FIRST TIME                              
         BZ    *+10                YES                                          
         CLC   PSHDACC(3),SAVEACC  TEST UNIT/LEDGER HAS CHANGED                 
         BE    *+8                 NO                                           
         BAS   RE,PRTTOT           FORMAT AND PRINT UNIT/LEDGER TOTALS          
         MVC   SAVEACC,PSHDACC                                                  
*                                                                               
PUPDT16  LA    R0,IOBUFF                                                        
         LH    R1,=Y(L'IOBUFF)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR IOA1                                   
         USING TRNRECD,R2                                                       
         LA    R2,IOBUFF           BUILD TRANSACTION REC IN IOA1                
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,PSHDACC                                                 
         MVC   TRNKWORK,PSHDANAL                                                
         MVC   TRNKCULC,PSHDSBAC                                                
         IC    RE,PSHDLEN                                                       
         LA    RE,0(RE,R4)         RE=A(TRNEL) IN WORKER REC                    
         USING TRNELD,RE                                                        
         MVC   TRNKDATE,TRNDATE                                                 
         MVC   TRNKREF,TRNREF                                                   
         MVC   TRNKSBR,TRNSUB                                                   
         LA    R1,WKLDGDR          ADD TRNAMNT TO REPORT ACCUMS                 
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,WKLDGCR                                                       
         AP    0(8,R1),TRNAMNT                                                  
         SR    R0,R0                                                            
         IC    RF,TRNLN                                                         
PUPDT18  LA    R1,0(RF,RE)         BUMP ALONG WORKER REC                        
         CLI   0(R1),0             TEST EOR                                     
         BE    PUPDT20                                                          
         IC    R0,1(R1)                                                         
         AR    RF,R0               RF=ACCUMULATED L'ELEMENTS                    
         B     PUPDT18                                                          
*                                                                               
PUPDT20  XC    KEY,KEY             PROVIDE ISKEY FOR EMU                        
         MVC   KEY(L'TRNKEY),TRNKEY                                             
         MVC   KEY+L'TRNKEY(L'TRNKSTA),TRNRSTA                                  
         LA    R0,TRNRFST                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE ELEMENTS INTO REC AFTER KEY             
         LR    R1,R0               SET R1=A(EOR)                                
         USING WFMELD,R1                                                        
         MVI   WFMEL,WFMELQ        ADD WFMEL AT EOR FOR JOURNAL POSTING         
         MVI   WFMLN,WFMLNQ                                                     
         MVC   WFMSIN,SAVESIN      SAVED SYS INPUT NUMBER FROM GETFACT          
         MVC   WFMPROG,SAVEDDQ     SAVED DD EQUATE FROM GETWKN                  
         DROP  R1                                                               
         AH    R1,=Y(WFMLNQ+1)     SET R1=A(EOR)+1                              
         SR    R1,R2               R2=A(SOR)                                    
         STCM  R1,3,TRNRLEN        L'TRANSACTION REC                            
         GOTO1 VACCEMU,DMCB,=C'NEWO',,KEY,(R2)                                  
         ORG   *-2                                                              
         LR    R2,R1                                                            
         BASR  RE,RF                                                            
         USING TRNBLK,R2                                                        
         L     R2,ATRNBLK                                                       
         ICM   R1,3,TRNBSEQN       BUMP ADDTRN'S REC COUNT                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,TRNBSEQN                                                    
         OI    TRNINDS2,TRNIADDG                                                
         GOTO1 VADDTRN,TRNBLK      ADD THE TRANSACTION                          
         BE    PUPDT21                                                          
         LA    RF,IOBUFF           NOT ADDED - SET BAD A/C IN ERR MSG           
         MVC   FVXTRA(L'TRNKCULA-1),TRNKUNT-TRNRECD(RF)                         
         MVC   FVMSGNO,=AL2(AE$CPAUC)                                           
         B     PUPDT22                                                          
*                                                                               
PUPDT21  MVI   EXITFLAG,0                                                       
         GOTO1 VGETFACT,DMCB,0     TEST IO COUNT                                
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,FATIOCNT-FACTSD(R1)                                         
         C     R0,MAXIOS           COMPARE WITH OUR MAXIMUM                     
         BNH   *+12                                                             
         MVI   EXITFLAG,GTMAXIOS                                                
         B     PUPDT22                                                          
******                             *TEMPORARY TEST CODE*                        
         ICM   RF,15,TEMP                                                       
         LA    R0,TEMP+L'TEMP                                                   
         CR    RF,R0               TEST TEMP FULL                               
         BL    *+8                                                              
         LA    RF,TEMP+4           RESET                                        
******   STCM  R0,3,0(RF)          SAVE IOCOUNT EACH TIME                       
         MVC   0(2,RF),TSARTRNO+2                                               
         LA    RF,2(RF)                                                         
         STCM  RF,15,TEMP                                                       
******                                                                          
         CLC   HALF2,TSTRNHI       TEST LAST TRANS REC READ                     
         BE    PUPDT22             YES                                          
         LH    RF,HALF2                                                         
         LA    RF,1(RF)                                                         
         STH   RF,HALF2            INCREMENT TSAR RECORD NUMBER                 
         B     PUPDT12                                                          
*                                                                               
         USING TSARD,R2                                                         
PUPDT22  L     R2,ATSARBLK         DELETE ALL TSAR TRANS RECORDS                
         MVI   TSACTN,TSADEL       IN REVERSE ORDER                             
         MVC   TSRNUM,TSTRNHI                                                   
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BNE   PUPDT24             CHECK TSAR ERROR                             
         CLC   TSTRNHI,TSTRNLO     TEST LOWEST TRANS RECORD NO. DELETED         
         BE    PUPDT24             YES - FINISHED                               
         LH    RF,TSTRNHI                                                       
         BCTR  RF,0                                                             
         STH   RF,TSTRNHI          DECREMENT TSAR RECORD NO.                    
         B     PUPDT22                                                          
*                                  CHECK FOR ERRORS                             
PUPDT24  MVC   DISTMAX,STSARNUM    RESTORE 'REAL' TSAR RECORD COUNT             
         CLI   TSERRS,0            TEST TSAR ERROR                              
         BNE   PUPDT26             YES                                          
         CLI   EXITFLAG,GTMAXIOS                                                
         BE    PHERRBP             EXIT MSG 'TOO MANY POSTINGS'                 
         CLI   EXITFLAG,0          TEST NO ERRORS                               
         BE    PUPDT28             YES                                          
PUPDT26  MVI   EXITFLAG,UPDERROR   EXIT MSG 'ERROR IN UPDATE'                   
         B     PHERRUP                                                          
         DROP  R2                                                               
*                                                                               
PUPDT28  XC    PSHDACC,PSHDACC     PRINT FILE TOTALS                            
         BAS   RE,PRTTOT                                                        
*                                                                               
         GOTO1 APRTCLO,(R3)        CLOSE REPORT                                 
*                                  UPDATE TSAR AND WORKER FILE STATUS           
         USING TRNBLK,R2                                                        
         L     R2,ATRNBLK                                                       
         OI    TRNINDS,TRNILAST                                                 
         OI    TRNINDS2,TRNIUPDG                                                
         GOTO1 VADDTRN,TRNBLK      FINISHED WITH ADDTRN                         
*                                                                               
         CLC   CHAR,SENO           TEST UPDATE TO CONNECTED ACC SYSTEM          
         BE    PUPDT30             YES                                          
         XC    DMCB(8),DMCB        NO - SWITCH BACK TO CONNECTED SYSTEM         
         MVC   DMCB(1),SENO                                                     
         MVC   DMCB+1(3),EFFS                                                   
         GOTO1 VSWITCH,DMCB                                                     
         BE    *+6                 SWITCH SUCCESSFUL                            
         DC    H'0'                                                             
*                                                                               
PUPDT30  GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WKID,WKID           RE-READ WORKER INDEX                         
         MVC   UKBKEY,TSARWKEY                                                  
         MVC   UKFILNO,TSARKSEQ                                                 
         GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    UKSTAT,255-WKISACTV   TEST STATUS AGAIN BEFORE UPDATE            
         BZ    PUPDT32               OK                                         
*                                                                               
         MVI   EXITFLAG,UPDERROR     FILE HAS BEEN CHANGED AT ANOTHER           
         MVC   FVMSGNO,=AL2(AE$FCAAT)     TERMINAL - UPDATE CANCELLED           
         B     PHERRUP                                                          
*                                                                               
PUPDT32  TM    TWAMODE2,TWAM2NUP   TEST UPDATE=NO SET                           
         BNZ   PUPDT34                                                          
*                                                                               
         GOTO1 VDMGR,DMCB,WKKEEP,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUPDT34  OI    TSARDSTS,WKISKEEP                                                
         OI    TSARIND1,TSARMKQ                                                 
         L     RE,ATSARBLK                                                      
         USING TSARD,RE                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    PUPDTX                                                           
         DC    H'0'                                                             
*                                                                               
PUPDTX   MVC   DISTMAX,STSARNUM    RESTORE 'REAL' TSAR RECORD COUNT             
*                                  SELECT MSG (PRTCLO BUILT SUBTEXT)            
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(AI$FURSP)  FILE UPDATED,REPT XXX,999 SPOOLD         
         OC    OPTRDST,OPTRDST                                                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$FURSU)  FILE UPDATED, XXX,999 TO USERID          
         LA    R1,WFMOPTH                                                       
         OI    DISIND,DISIMMSG     TELL DISPLAY TO USE MY MSG                   
         B     PXITE                                                            
         DROP  R2,RE                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS FOR LEDGER/UNIT                                        *         
* NTRY - R3=REPD                                                      *         
*        R4=WORKER POSTING RECORD                                     *         
***********************************************************************         
PRTTOT   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(LDGKEND),SAVEACC                                             
         MVC   PLGC,SAVEACC+2                                                   
         MVI   BYTE,1              DO LEDGER TOTALS                             
         LA    R5,WKLDGDR                                                       
         B     PRTTOT4                                                          
*                                                                               
PRTTOT2  CLC   PSHDACC(2),SAVEACC  TEST UNIT HAS CHANGED                        
         BE    PRTTOTX                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(UNTKEND),SAVEACC                                             
         MVC   PUNC,SAVEACC+1                                                   
         MVI   BYTE,2              DO UNIT TOTALS                               
         LA    R5,WKUNTDR                                                       
         B     PRTTOT4                                                          
*                                  READ UNIT/LEDGER RECORD                      
PRTTOT4  LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   PRTTOTX                                                          
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   PRTTOTX                                                          
         L     R1,AIOBUFF                                                       
         LA    R1,ACCRFST-ACCRECD(R1)                                           
         USING NAMELD,R1                                                        
         SR    RF,RF                                                            
PRTTOT6  CLI   0(R1),NAMELQ        GET NAME ELEMENT                             
         BE    PRTTOT8                                                          
         CLI   0(R1),0                                                          
         BE    PRTTOTX                                                          
         IC    RF,NAMLN                                                         
         AR    R1,RF                                                            
         B     PRTTOT6                                                          
*                                                                               
PRTTOT8  LA    R2,PLGN                                                          
         CLI   BYTE,1                                                           
         BE    *+8                                                              
         LA    R2,PUNN                                                          
         LA    RF,L'PLGN-1                                                      
         CLI   NAMLN,L'PLGN+2                                                   
         BH    *+12                                                             
         IC    RF,NAMLN                                                         
         SHI   RF,3                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NAMEREC     R2=A(UNIT/LEDGER NAME PRINT POSN)            
         DROP  R1                                                               
*                                  DO TOTALS                                    
         AP    16(8,R5),0(8,R5)    R5=A(TOTALS FOR THIS LEVEL)                  
         AP    24(8,R5),8(8,R5)                                                 
PRTTOT10 ZAP   EDIT1,0(8,R5)                                                    
         ZAP   EDIT2,8(8,R5)                                                    
         ZAP   EDIT3,0(8,R5)                                                    
         SP    EDIT3,8(8,R5)                                                    
         ZAP   0(8,R5),PZERO                                                    
         ZAP   8(8,R5),PZERO                                                    
         LA    R0,3                                                             
         LA    R2,EDIT1                                                         
         LA    R5,PDRT                                                          
PRTTOT12 CURED (P8,0(R2)),(15,0(R5)),2,COMMAS=YES,MINUS=YES                     
         LA    R2,8(R2)                                                         
         LA    R5,15(R5)                                                        
         BCT   R0,PRTTOT12                                                      
*                                                                               
         GOTO1 VREPORT,REPD                                                     
         CLI   BYTE,1              TEST LEDGER TOTALS PRINTED                   
         BE    PRTTOT2             YES - TEST CHANGE OF UNIT                    
         OC    PSHDACC,PSHDACC     TEST TIME TO DO FILE TOTALS                  
         BNZ   PRTTOTX             NO                                           
         MVI   PSHDACC,1                                                        
         GOTO1 VREPORT,REPD        YES - SPACE A LINE                           
         MVC   PUNN,LC@TFWF        TOTALS FOR WORKER FILE                       
         LA    R5,WKFILDR                                                       
         B     PRTTOT10                                                         
*                                                                               
PRTTOTX  XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ACTION PROCESSING ROUTINES - COMMON EXIT                            *         
***********************************************************************         
PHERRNP  MVC   FVMSGNO,=AL2(AE$NPORV)  NOTHING TO REVERSE                       
         B     PXITH                                                            
*                                                                               
PHERRBP  MVC   FVMSGNO,=AL2(AE$PFBIG)  FILE TOO BIG FOR ONLINE UPDATE           
         B     PXITH                                                            
*                                                                               
PHERRUP  OC    FVMSGNO,FVMSGNO     TEST MESSAGE ALREADY SET                     
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$FPEUA)  FILE/PROC ERROR - CAN'T UPDATE           
         B     PXITH                                                            
*                                                                               
PHERRNS  MVC   FVMSGNO,=AL2(AE$ANSCU)  CAN'T UPDATE - ACCFILE NO-OP'D           
         L     R1,VSEXLST          R1=V(SELIST)                                 
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
*                                  BUMP DOWN SELIST ENTRIES                     
         USING SELISTD,R1                                                       
PHERRNS2 CLI   SEOVSYS,6           TEST ACCOUNT SYSTEM ENTRY                    
         BNE   *+14                                                             
         CLC   SESYS,CHAR          FIND ENTRY FOR NO-OP'D ACCFILE SENO          
         BE    *+10                                                             
         BXLE  R1,RE,PHERRNS2                                                   
         DC    H'0'                NO MATCH - INVALID                           
*                                                                               
         MVC   FVSUBT+1(L'SENAME),SENAME  PASS ACCFILE NAME TO GETTXT           
         LA    RF,FVSUBT+L'SENAME                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,FVSUBT-1                                                      
         SR    RF,R0                                                            
         STC   RF,FVSUBT                                                        
         B     PXITH                                                            
         DROP  R1                                                               
*                                                                               
PLNTRID  MVC   FVMSGNO,=AL2(AI$EINIT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     PXITL                                                            
*                                                                               
PXITL    MVI   DUB,0               SET CC LOW                                   
         B     PXITCC                                                           
PXITH    MVI   DUB,2               SET CC HIGH                                  
         B     PXITCC                                                           
PXITE    MVI   DUB,1               SET CC EQUAL                                 
PXITCC   CLI   DUB,1                                                            
*                                                                               
PXITX    XIT1  ,                                                                
*                                                                               
PXITRE   LTR   RE,RE               ERROR EXIT WITH CC NEQ BEFORE NTR1           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY SELECTED WORKER POSTING RECORD IN HEX                       *         
***********************************************************************         
HEXDISP  LA    R1,WFMOPTH                                                       
         ST    R1,FVADDR                                                        
         OC    WKRNO,WKRNO         TEST HEX DISPLAY ALREADY IN USE              
         BZ    HEXD4               NO                                           
         LH    RF,WKRNO            YES                                          
         OC    HEXDREGS,HEXDREGS   TEST PREV RECORD DISPLAY UNFINISHED          
         BNZ   HEXD2               YES - CONTINUE IT                            
         LA    RF,1(RF)            NO - INCREMENT WORKER RECORD NUMBER          
         STH   RF,WKRNO                                                         
HEXD2    LR    R0,RF                                                            
         B     HEXD8               BRANCH TO READ                               
*                                                                               
HEXD4    L     RF,AINP             LOCATE CURSOR                                
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURD-TIOBD(RF)                                          
         SH    R1,=Y(WFMDDETH-TWAD)                                             
         BZ    HEXD6               1ST DISPLAYED POSTING RECORD                 
         BM    FVERRX              CURSOR NOT ON A POSTING RECORD               
         LA    RF,DISLDLNL         CALCULATE DISP INTO DISLIST                  
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         SLL   R1,1                *2                                           
HEXD6    LA    R1,DISLIST(R1)                                                   
         ICM   R0,3,0(R1)                                                       
         BZ    FVERRX              CURSOR NOT ON A POSTING RECORD               
         STCM  R0,3,WKRNO                                                       
         XC    HEXDREGS,HEXDREGS   INIT WORK REGS SAVE AREA                     
*                                                                               
HEXD8    L     R3,AWKIO                                                         
         XC    0(4,R3),0(R3)                                                    
         STCM  R0,3,2(R3)          WORKER POSTING RECORD NUMBER                 
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         B     HEXD12                                                           
*                                                                               
HEXD10   GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
HEXD12   BNE   HEXDX4              EOF                                          
         CLI   4(R3),PSHDELQ       TEST POSTING RECORD                          
         BE    *+8                                                              
         CLI   4(R3),PSTKELQ       OR TRANSACTION RECORD                        
         BNE   HEXDX4                                                           
         GOTO1 AFILTER             FILTER ON OPTIONS                            
         BE    HEXD13              PASSES                                       
         LH    RF,WKRNO            BUMP RECORD NUMBER                           
         LA    RF,1(RF)                                                         
         STH   RF,WKRNO                                                         
         B     HEXD10              READ NEXT                                    
*                                                                               
HEXD13   ICM   R0,3,0(R3)          L'WORKER RECORD                              
         GOTO1 VHEXOUT,DMCB,AWKIO,AWKBUF2,(R0)                                  
*                                  CLEAR SCREEN                                 
         L     R1,ADISDET1         A(FIRST DETAIL LINE)                         
         L     RF,ADISDETL         A(LAST DETAIL LINE)                          
         SR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         TWAXC (R1),(RF),PROT=Y                                                 
*                                                                               
         L     R1,ADISDET1         RE-DISPLAY POSTING RECORD                    
         GOTO1 ABLDLIN                                                          
*                                                                               
         LA    R2,DISLDLNL(R1)     FORMAT DISPLAY HEADINGS                      
         USING DISLINED,R2                                                      
         L     R3,AWKIO                                                         
         OC    HEXDREGS,HEXDREGS   TEST CONTINUATION OF PREV DISPLAY            
         BZ    *+10                                                             
         MVC   DISLDBYT,=C'(Continued)'                                         
         MVC   DISLDCHR(4),=C'R/L='                                             
         ICM   R0,3,0(R3)          OVERALL RECORD LENGTH                        
         SH    R0,=Y(6)            LESS L'HEADER BYTES                          
         CVD   R0,DUB                                                           
         UNPK  DISLDCHR+4(5),DUB                                                
         OI    DISLDCHR+8,X'F0'                                                 
         OI    DISLHDR1+(FVATRB-FVIHDR),FVAHIGH                                 
         LA    R2,DISLDLNL(R2)                                                  
         MVC   DISLDBYT,=C'---Bytes---'                                         
         MVI   DISLDHEX,C'.'                                                    
         MVC   DISLDHEX+1(L'DISLDHEX-1),DISLDHEX                                
         LA    RF,DISLDHEX                                                      
         LA    RE,C'0'                                                          
         LA    R0,10                                                            
         STC   RE,0(RF)                                                         
         STC   RE,20(RF)                                                        
         LA    RF,2(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         MVC   DISLDCHR,=C'*****Characters*****'                                
         OI    DISLHDR1+(FVATRB-FVIHDR),FVAHIGH                                 
         LA    R2,DISLDLNL(R2)                                                  
*                                                                               
         LH    R0,DISLINES                                                      
         SH    R0,=Y(3)            SET BCT REG AS LIMIT FOR DISPLAY             
         SR    R1,R1                                                            
         LM    R3,R4,HEXDARE       LOAD ADDRESS REGS DISPS TEMPORARILY          
         L     RE,AWKIO                                                         
         LA    RE,4(RE)            RE=A(FIRST REAL ELEMENT)                     
         ST    RE,HEXDARE          SAVE ADDRESS TILL END OF DISPLAY             
         AR    RE,R3               ADD DISPLACEMENT (OR 0)                      
         L     RF,AWKBUF2                                                       
         LA    RF,8(RF)            RF=A(FIRST EBCDIC HEX ELEMENT)               
         ST    RF,HEXDARF          SAVE ADDRESS TILL END OF DISPLAY             
         AR    RF,R4               ADD DISPLACEMENT (OR 0)                      
         LM    R3,R4,HEXDWR3       LOAD WORK REGS                               
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         BCTR  R4,0                INIT R4 -VE                                  
*                                                                               
HEXD16   LTR   R3,R3               TEST ANYTHING LEFT OF ELEMENT                
         BZ    HEXD18              NO                                           
         SH    R3,=Y(L'DISLDCHR)   YES, SUBTRACT L'MOVED OUT PREVIOUSLY         
         LR    R1,R3                                                            
         B     HEXD20                                                           
HEXD18   CLI   0(RE),0             TEST EOR                                     
         BE    HEXDX                                                            
         IC    R1,1(RE)            GET L'REAL ELEMENT                           
HEXD20   CH    R1,=Y(L'DISLDCHR)   TEST L'TO BE MOVED > L'SCREEN FIELD          
         BH    *+10                                                             
         SR    R3,R3               NO - CLEAR SAVED LENGTH                      
         B     *+10                                                             
         LR    R3,R1               YES - SAVE LENGTH                            
         LH    R1,=Y(L'DISLDCHR)   AND SUBSTITUTE L'SCREEN FIELD                
         BCTR  R1,0                -1 FOR EXECUTE                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DISLDCHR(0),0(RE)   MOVE OUT CHARACTERS                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         TR    DISLDCHR(0),TRNTAB  SET NON-ALPHANUMERIC CHARS TO '.'            
         LA    R4,1(R4)            BUILD BYTE COUNT                             
         CVD   R4,DUB                                                           
         UNPK  DISLDBYT+0(5),DUB   FROM                                         
         OI    DISLDLIN+4,X'F0'                                                 
         MVI   DISLDLIN+5,C'-'     -                                            
         AR    R4,R1                                                            
         CVD   R4,DUB                                                           
         UNPK  DISLDLIN+6(5),DUB   TO                                           
         OI    DISLDLIN+10,X'F0'                                                
         LA    R1,1(R1)            RESTORE TRUE OUTPUT LENGTH                   
         AR    RE,R1               BUMP CHAR ELEMENT REGISTER                   
         SLL   R1,1                *2 FOR EBCDIC HEX                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DISLDHEX(0),0(RF)   MOVE OUT HEX                                 
         LA    RF,1(R1,RF)         BUMP HEX ELEMENT REGISTER                    
         LA    R2,DISLDLNL(R2)     BUMP TO NEXT SCREEN LINE                     
         BCT   R0,HEXD16                                                        
         S     RE,HEXDARE          SAVE DISPS INTO RECORD                       
         S     RF,HEXDARF                                                       
         STM   R3,R4,HEXDWR3       RUN OUT OF ROOM, SO SAVE                     
         STM   RE,RF,HEXDARE       WORK/ADDR REGS FOR NEXT TIME                 
         B     HEXDX2                                                           
*                                                                               
HEXDX    XC    HEXDREGS,HEXDREGS   COMPLETE RECORD DISPLAYED                    
HEXDX2   MVC   FVMSGNO,=AL2(AI$RDPFN)  'REC DISPLAYED, PF4 FOR NEXT'            
         B     *+10                                                             
HEXDX4   MVC   FVMSGNO,=AL2(AI$EPFNM)  'ENTER PFK - NO MORE POSTINGS'           
         MVI   FVOMTYP,GTMINF                                                   
         OI    DISIND,DISINOSC     SET NO SCROLL FOR DETAIL SCREEN              
         B     FVERR                                                            
         DROP  R2                                                               
         SPACE 2                                                                
TRNTAB   DC    (08*16)C'.'             00-7F    TRANSLATE TABLE FOR             
         ORG   TRNTAB+X'40'                     CHARACTER DISPLAY               
         DC    C' '                                                             
         ORG   TRNTAB+X'80'                                                     
         DC    CL16'.abcdefghi......'  80-8F                                    
         DC    CL16'.jklmnopqr......'  90-9F                                    
         DC    CL16'..stuvwxyz......'  A0-AF                                    
         ORG   TRNTAB+X'C0'                                                     
         DC    CL16'.ABCDEFGHI......'  C0-CF                                    
         DC    CL16'.JKLMNOPQR......'  D0-DF                                    
         DC    CL16'..STUVWXYZ......'  EO-EF                                    
         DC    CL16'0123456789......'  F0-FF                                    
         EJECT                                                                  
***********************************************************************         
* ALLOW ENTRY TO PASSWORD FIELD (SYSTEMS ONLY)                        *         
***********************************************************************         
PASSCHK  NI    WFMPASH+(FVATRB-FVIHDR),255-FVAPROT  UNPROTECT FIELD             
         OI    WFMPASH+(FVOIND-FVIHDR),FVOCUR+FVOXMT AND SET CURSOR             
         B     FVERRX                                                           
         EJECT                                                                  
***********************************************************************         
* COMMON ERROR EXITS                                                  *         
***********************************************************************         
ERRNONE  MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     FVERR                                                            
ERRNOUID MVC   FVMSGNO,=AL2(AE$USRNS)                                           
         B     FVERR                                                            
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
         BNZ   FVERR2                                                           
         LA    R1,DMCB                                                          
         STCM  R1,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
FVERR2   CLI   FVSUBT,0            LOOK FOR SUBSTITUTION TEXT                   
         BNH   FVERR4                                                           
         LA    R1,FVSUBT                                                        
         STCM  R1,7,GTASUBST                                                    
FVERR4   CLI   FVXTRA,C' '         LOOK FOR ADDITIONAL TEXT                     
         BNH   FVERR6                                                           
         LA    R1,FVXTRA                                                        
         STCM  R1,7,GTATXT                                                      
         LA    RF,L'FVXTRA-1(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1                                                            
         LA    RF,1(RF)                                                         
         STC   RF,GTLTXT                                                        
*                                                                               
FVERR6   LA    R2,PARM             PARM DEFINED INTERNALLY                      
         CLI   GTMSGNO,X'FF'       CHECK FOR GENERAL MESSAGES                   
         BNE   *+12                                                             
         MVI   GTMSYS,X'FF'        FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
         XC    WFMMSG,WFMMSG                                                    
         GOTO1 VGETTXT,GETTXTD                                                  
         DROP  R2                                                               
*                                                                               
FVERRX   OI    WFMMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         ICM   R1,15,FVADDR        TEST IF FIELD ADDRESS SET                    
         BZ    *+8                                                              
         OI    FVOIND-FVIHDR(R1),FVOCUR                                         
         SPACE 1                                                                
         L     R1,ATSARBLK         SAVE TSAR BUFFER ON DISK                     
         USING TSARD,R1                                                         
         OC    TSABUF,TSABUF       TEST WE HAVE A TSAR BUFFER                   
         BZ    EXIT                                                             
         MVI   TSACTN,TSASAV       YES - SAVE IT                                
         GOTO1 VTSAR                                                            
         DROP  R1                                                               
*                                  SAVE WORKER BITS                             
         GOTO1 VDMGR,DMCB,WKBSAV,WKFILE,WKID,SWKAREA,AWKBUF1                    
*                                                                               
         CLI   EXITFLAG,0          TEST ERROR IN PUPDT ROUTINE                  
         BE    EXIT                NO                                           
         SPACE                                                                  
         DC    H'0',C'$ABEND'      KILL UPDATE, UNWIND RECOVERY                 
         SPACE 2                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROOT ROUTINES - CONTROL                                             *         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         DS    0F                                                               
ROUT     NMOD1 0,**ROUT**,RA,R9                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     DISPLAY             BUILD & DISPLAY DETAIL SCREEN                
         B     DISREC              DISPLAY SUBSIDIARY ROUTINE                   
         B     BLDDIS              DISPLAY SUBSIDIARY ROUTINE                   
         B     BLDLIN              DISPLAY SUBSIDIARY ROUTINE                   
         B     BLDPFK              BUILD PFKEY DISPLAY LINE                     
         B     CONFRM              ACTION CONFIRMATION DISPLAY/VALIDN.          
         B     FLDVAL              GENERAL FIELD VALIDATION                     
         B     FILTER              VALIDATE RECORD AGAINST FILTERS              
         B     GETUID              TEST USERID/BUILD UID TABLE                  
         B     GETWKN              EXTRACT WRKR FILE NAME FROM PRGTAB           
         B     IOEXEC              IO EXECUTIVE                                 
         B     OVRSCR              OVERLAY SCREEN                               
         B     PRTCLO              CLOSE PRINT FILE                             
         B     PRTINI              INITIALISE PRINT FILE                        
         B     READWK              READ/FILTER WORKER INDEX RECS                
         B     SCANFLD             SCAN KEY OR OPTIONS INPUT                    
         B     TSARADD             ADD A TSAR RECORD                            
         B     TSARGET             GET A TSAR RECORD                            
         B     TSARINI             INITIALISE/RESTORE TSAR BUFFER               
         B     VALACC              VALIDATE ACCOUNT                             
         B     VALAMT              VALIDATE AMOUNT                              
         B     VALCHQ              VALIDATE INCLUDE CHEQUES                     
         B     VALDST              VALIDATE REPORT DESTINATION                  
         B     VALPER              VALIDATE DATE PERIOD                         
         B     VALSTS              VALIDATE WORKER FILE STATUS                  
         B     VALSYS              VALIDATE ACCOUNT SYSTEM NAME                 
         B     VALID               VALIDATE 'ID='                               
         B     VALAID              VALIDATE 'ALLIDS='                           
         B     VALGID              VALIDATE 'GROUPID='                          
         B     VALUID              VALIDATE 'USERID='                           
         B     VALWKY              VALID WORKER KEY                             
         B     VALMTH              VALIDATE NEW POSTING MONTH                   
*                                                                               
RHERRNV  MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUTH                                                            
RHERRID  MVC   FVMSGNO,=AL2(EGIFDUPL)                                           
         B     ROUTH                                                            
RHERRRI  MVC   FVMSGNO,=AL2(AE$INVRG)                                           
         B     ROUTH                                                            
RHERRLG  MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUTH                                                            
RHERRUI  MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUTH                                                            
*                                                                               
RECFACT  MVC   FVMSGNO,=AL2(AI$CNFRM)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     ROUTE                                                            
*                                                                               
ROUTL    MVI   DUB,0               SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   DUB,2               SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   DUB,1               SET CC EQUAL                                 
ROUTCC   CLI   DUB,1                                                            
*                                                                               
ROUTX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*  CONSTANTS ADDRESSABLE FROM BOTH WFM AND ROUT CSECTS                *         
***********************************************************************         
********************                                                            
* DATAMGR LITERALS *                                                            
********************                                                            
ACCFIL   DC    C'ACCFIL  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
WKFILE   DC    C'WKFILE  '                                                      
*********************************                                               
* I/O COMMANDS FOR WORKER FILES *                                               
*********************************                                               
WKADDR   DC    CL8'ADD'                                                         
WKBINI   DC    CL8'BUFFER'                                                      
WKBRST   DC    CL8'BURSTR'                                                      
WKBSAV   DC    CL8'BUSAVE'                                                      
WKCLOS   DC    CL8'CLOSE'                                                       
WKCOMT   DC    CL8'COMMENT'                                                     
                                                                                
WKDELT   DC    CL8'DELETE'                                                      
WKHOLD   DC    CL8'HOLD'                                                        
WKINDX   DC    CL8'INDEX'                                                       
WKKEEP   DC    CL8'KEEP'                                                        
WKOPEN   DC    CL8'OPEN'                                                        
WKPRGE   DC    CL8'PURGE'                                                       
WKRAND   DC    CL8'RANDOM'                                                      
WKREAD   DC    CL8'READ'                                                        
WKRSTR   DC    CL8'RESTORE'                                                     
WKSEQU   DC    CL8'SEQ'                                                         
WKUNKP   DC    CL8'UNKEEP'                                                      
WKWRIT   DC    CL8'WRITE'                                                       
                                                                                
**********************************************************************          
* I/O COMMANDS FOR ACCDIR/ACCMST/ACCARC FILES WITH ACCFIL EQUIVALENTS           
**********************************************************************          
*                ACCDIR     ACCFIL      INDS                                    
*                                                                               
ISCMNDS  DS    0CL7                                                             
DMRDHI   DC    C'DMRDHI ',C'DMRDHI ',AL1(0,0)                                   
DMREAD   DC    C'DMREAD ',C'DMREAD ',AL1(0,0)                                   
DMRSEQ   DC    C'DMRSEQ ',C'DMRSEQ ',AL1(0,0)                                   
DMADD    DC    C'DMADD  ',XL7'00',AL1(0,0)                                      
DMWRT    DC    C'DMWRT  ',XL7'00',AL1(0,0)                                      
*                                                                               
*                ACCMST     ACCFIL      INDS                                    
*                                                                               
DACMNDS  DS    0CL7                                                             
GETREC   DC    C'GETREC ',C'DMREAD ',AL1(IOCTIGET,0)                            
PUTREC   DC    C'PUTREC ',C'DMWRT  ',AL1(0,0)                                   
ADDREC   DC    C'ADDREC ',C'DMADD  ',AL1(0,0)                                   
ADFREC   DC    C'ADFREC ',XL7'00',AL1(0,0)                                      
         SPACE 2                                                                
PZERO    DC    P'0'                                                             
EFFS     DC    X'FFFFFFFF'                                                      
*                                                                               
PASTAB   DS    0CL3                                                             
         DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
LUITAB   DS    0CL8                                                             
         DC    C'D1LM229T'                                                      
         DC    C'D1LM248T'                                                      
         DC    C'D1LM252T'                                                      
         DC    C'D1LM257T'                                                      
         DC    C'D1LM259T'                                                      
         DC    C'D1LM320T'                                                      
         DC    C'D1LV251T'                                                      
         DC    C'D1LV255T'                                                      
         DC    C'D1LV257T'                                                      
         DC    X'00'                                                            
*                                                                               
UPDMAX   DC    P'1000'             MAX WKR PSTG COUNT FOR ONLINE UPDATE         
MAXIOS   DC    A(28000)            IO MAX FOR ONLINE UPDATE                     
*                                                                               
MOSTAB   DC    C'123456789......ABC'                                            
         EJECT                                                                  
***********************************************************************         
* MAIN SCREEN DISPLAY ROUTINE -                                       *         
* BUILDS LIST OF NUMBERS OF TSAR RECORDS TO BE DISPLAYED ACCORDING TO *         
* SCROLL AND FILTER PARAMETERS                                        *         
***********************************************************************         
DISPLAY  L     RE,ASCRTAB          GET ATTRIBUTES FOR CURRENT SCREEN            
         CLI   TWASCROV,TWASCLST                                                
         BE    *+8                                                              
         LA    RE,SCRTABL(RE)                                                   
         USING SCRTABD,RE                                                       
         LA    R0,SCRYTYPS         LOAD DISPLAY A-TYPES                         
         LA    R1,SCRHEAD                                                       
         LA    RF,ADISHEAD                                                      
DISPLY2  LH    R2,0(R1)                                                         
         LTR   R2,R2                                                            
         BZ    *+6                                                              
         AR    R2,R6                                                            
         ST    R2,0(RF)                                                         
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,DISPLY2                                                       
*                                                                               
         MVC   DISLINEL,SCRWIDT    USABLE L'DISPLAY LINE                        
         MVC   DISSCRNL,SCRPHSW    PHYSICAL LINE LENGTH                         
         MVC   DISLINES,SCRLINS    N'DISPLAY LINES                              
         MVC   DISCOLS,SCRCOLS     COL NUMBERS FOR THIS SCREEN                  
         CLC   WKIPRG,=C'DJ'       TEST NEW JOURNAL                             
         BNE   *+10                                                             
         MVC   DISCOLS,SCRCOLS2    SET ALTERNATIVE COLUMNS                      
         MVC   DISDISD,SCRDISD                                                  
         MVC   DISRMAX,DISTMAX     SET DISPLAY LIMIT FROM TSAR RECD MAX         
         CLI   TWASCROV,TWASCLST                                                
         BE    *+10                                                             
         MVC   DISRMAX,DISWMAX     OR WKFILE RECORD MAX                         
         XC    WKRNO,WKRNO         CLEAR SAVED WKR POSTING NUMBER               
         XC    TEMP,TEMP           CLEAR TEMP (FOR DISLCNT & DISLIST)           
         TM    DISIND,DISIRST      TEST RESTART FROM BEGINNING                  
         BNO   DISPLY4                                                          
         MVI   SCRLINDS,SCRLMAXI+SCRLUP                                         
         B     DISPLY10            ALWAYS SCROLL IN THIS CASE                   
*                                                                               
DISPLY4  TM    DISIND,DISINOSC     TEST NO SCROLL REQUESTED                     
         BNZ   DISOUT              DON'T SCROLL, RE-BUILD DISPLAY               
         TM    DISIND,DISIFFLT     TEST FORCE RE-FILTERING OF DISLIST           
         BNO   DISPLY10                                                         
         LH    R0,DISLCNT          R0=COUNT OF RECORDS IN DISLIST               
         LTR   R0,R0                                                            
         BZ    DISPLY10                                                         
         XC    TEMP(DISLISTL),TEMP CLEAR TEMP FOR RE-FILTERED DISLIST           
         LA    R2,DISLIST          R2=A(DISLIST)                                
         SR    R3,R3               R3 COUNTS RE-FILTERED TEMP RECORDS           
DISPLY6  MVC   DISNUM,0(R2)        SET RECORD NUMBER                            
         GOTO1 ADISREC,(R2)        GET TSAR OR WORKER POSTING RECORD            
         BNH   *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER             FILTER THE RECORD                            
         BNE   DISPLY8             FAILS - GET NEXT DISLIST RECORD              
         LA    RF,TEMP(R3)         PASSES - INDEX INTO TEMP LIST                
         MVC   0(2,RF),DISNUM      AND SET THE NUMBER                           
         LA    R3,2(R3)            ADVANCE INDEX INTO TEMP LIST                 
DISPLY8  LA    R2,2(R2)            R2=A(NEXT DISLIST RECORD NO.)                
         BCT   R0,DISPLY6          DO FOR NUMBER OF RECORDS IN DISLIST          
         SRL   R3,1                R3=NEW RECORD COUNT*2, SO HALVE IT           
         STH   R3,DISLCNT          AND SET NEW RECORD COUNT                     
         MVC   DISLIST(DISLISTL),TEMP  REFRESH DISLIST                          
*                                                                               
DISPLY10 NI    DISIND,255-DISIBOF-DISIEOF  RESET BOF/EOF INDICATORS             
         TM    SCRLINDS,SCRLMAXI     TEST MAXIMUM SCROLL                        
         BNO   DISPLY14                                                         
         XC    DISLIST(DISLISTL),DISLIST   CLEAR LIST OF RECORD NUMBERS         
         XC    DISLCNT,DISLCNT     CLEAR COUNT OF RECORDS DISPLAYED             
         LH    R0,DISLINES         SET SCROLL TO A PAGE                         
         LH    R2,DISRMAX          HIGHEST RECORD NUMBER                        
         AH    R2,=H'1'                                                         
         TM    SCRLINDS,SCRLDOWN   TEST MAXIMUM SCROLL DOWN                     
         BNO   DISPLY12                                                         
         NI    SCRLINDS,X'FF'-SCRLDOWN                                          
         OI    SCRLINDS,SCRLUP     YES - SET SCROLL UP                          
         B     DISPLY16                                                         
*                                                                               
DISPLY12 SR    R2,R2               SET MINIMUM RECORD NUMBER-1                  
         NI    SCRLINDS,X'FF'-SCRLUP  SET SCROLL DOWN                           
         OI    SCRLINDS,SCRLDOWN      YES - SET SCROLL UP                       
         OI    DISIND,DISIRST      INDICATE FIRST PAGE                          
         B     DISPLY16                                                         
*                                  CLEAR DISLIST IF NO PREVIOUS RECORD          
DISPLY14 OC    DISLCNT,DISLCNT                                                  
         BNZ   *+10                                                             
         XC    DISLIST(DISLISTL),DISLIST                                        
         MVC   TEMP(DISLISTL),DISLIST                                           
         MVC   TEMP+DISLISTL(L'DISLCNT),DISLCNT                                 
         LH    R0,DISLINES         R0=SCROLL AMOUNT (NN LINES)                  
         TM    SCRLINDS,SCRLHALF                                                
         BNO   *+8                                                              
         SRL   R0,1                                                             
         LH    R2,DISLIST          R2=FIRST DISPLAYED RECORD                    
         TM    SCRLINDS,SCRLUP                                                  
         BO    DISPLY16                                                         
         LH    R2,DISLCNT          SET R2 TO TSAR NUMBER OF LAST                
         LTR   R2,R2               DISPLAYED RECORD                             
         BZ    *+12                                                             
         SLL   R2,1                                                             
         LH    R2,DISLIST-L'DISLIST(R2)                                         
*                                                                               
DISPLY16 TM    SCRLINDS,SCRLUP     TEST SCROLL UP (BACKWARDS)                   
         BNO   DISPLY18                                                         
         SHI   R2,1                DECREMENT RECORD COUNT                       
         BP    DISPLY20            GET RECORD IF NOT AT BOF                     
         OI    DISIND,DISIBOF      SET BOF REACHED                              
         B     DISPLY26                                                         
*                                                                               
DISPLY18 AHI   R2,1                INCREMENT RECORD COUNTER                     
         CH    R2,DISRMAX          GET RECORD IF NOT AT EOF                     
         BNH   DISPLY20                                                         
         OI    DISIND,DISIEOF      SET EOF REACHED                              
         B     DISPLY26                                                         
*                                                                               
DISPLY20 STH   R2,DISNUM           SET RECORD NUMBER                            
         GOTO1 ADISREC,DISNUM      GET TSAR OR WORKER POSTING RECORD            
         BNH   *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER             FILTER THE RECORD                            
         BNE   DISPLY16            GET NEXT IF FILTERED OUT                     
*                                                                               
         LTR   R0,R0               TEST HERE AFTER ACTIVE LOOP                  
         BZ    DISPLY28            YES - ONE RECORD FOUND IS ENOUGH             
*                                                                               
         LH    RE,DISLCNT          RE=NUMBER OF ENTRIES IN DISLIST              
         TM    SCRLINDS,SCRLDOWN   TEST SCROLL UP OR DOWN                       
         BNO   DISPLY22                                                         
*                                  ADD NUMBER TO END OF LIST                    
         LR    RF,RE                                                            
         CH    RF,DISLINES         SHIFT OFF FIRST ENTRY IF LIST FULL           
         BL    *+12                                                             
         MVC   DISLIST(DISLISTL-L'DISLIST),DISLIST+L'DISLIST                    
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         LA    RF,DISLIST(RF)      RF=A(NEW LAST ENTRY IN TABLE)                
         B     DISPLY24                                                         
*                                  ADD NUMBER TO BEGINNING OF LIST              
*                                  SHIFT OFF LAST ENTRY                         
DISPLY22 MVC   WORK(DISLISTL-L'DISLIST),DISLIST                                 
         MVC   DISLIST+L'DISLIST(DISLISTL-L'DISLIST),WORK                       
         LA    RF,DISLIST          RF=A(NEW FIRST ENTRY IN TABLE)               
*                                                                               
DISPLY24 MVC   0(L'DISNUM,RF),DISNUM                                            
         AHI   RE,1                                                             
         CH    RE,DISLINES         TEST DISLIST FULL                            
         BH    *+8                                                              
         STH   RE,DISLCNT          NO - SET NEW NUMBER OF ENTRIES               
         LTR   R0,R0               TEST HERE AFTER ACTIVE LOOP                  
         BZ    DISPLY16            YES - DON'T LET R0 GO NEGATIVE               
         BCT   R0,DISPLY16                                                      
         B     DISPLY16            GO BACK TO LOOK FOR ONE MORE                 
*                                                                               
DISPLY26 LTR   RE,R0               TEST ANY SCROLLING LEFT TO DO                
         BZ    DISPLY28                                                         
         TM    SCRLINDS,SCRLDOWN   TEST SCROLLING DOWN (NOT FIRST)              
         BNO   DISPLY28                                                         
         TM    DISIND,DISIRST      TEST SCROLL FROM BEGINNING                   
         BNZ   DISPLY28                                                         
         LH    RF,DISLCNT                                                       
         SR    RF,RE               RF=NUMBER OF ENTRIES REMAINING               
         BNP   DISPLY28                                                         
         STH   RF,DISLCNT                                                       
         SLL   RE,1                                                             
         LA    RE,DISLIST(RE)      RE=A(SHIFT FROM POSITION)                    
         SLL   RF,1                                                             
         BCTR  RF,0                                                             
         XC    WORK(DISLISTL),WORK                                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)       SAVE REMAINDER & SET AS FIRST                
         XC    DISLIST(DISLISTL),DISLIST                                        
         MVC   DISLIST(DISLISTL),WORK                                           
*                                                                               
DISPLY28 NI    DISIND,255-DISIRST-DISIFFLT                                      
         OC    DISLIST(DISLISTL),DISLIST                                        
         BNZ   DISPLYX                                                          
         MVC   DISLIST(DISLISTL),TEMP                                           
         MVC   DISLCNT,TEMP+DISLISTL                                            
DISPLYX  B     DISOUT                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD DISPLAY SCREEN FROM DISTAB                                    *         
***********************************************************************         
DISOUT   GOTO1 ABLDDIS             BUILD HEADS & DISPLAY DISPLACEMENTS          
         L     R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    RF,DISLINEL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,R1),DISHEAD     MOVE HEADING TO SCREEN                
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         TM    DISIND,DISINOSC     TEST 'NO SCROLL' SPECIFIED                   
         BNO   *+12                NO                                           
         CLI   TWASCROV,TWASCLST   YES - TEST LIST SCREEN                       
         BE    DISOUT2             YES - DON'T CLEAR IT                         
*                                                                               
         L     R1,ADISDET1         A(FIRST DETAIL LINE)                         
         L     RF,ADISDETL         A(LAST DETAIL LINE)                          
         SR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         TWAXC (R1),(RF),PROT=Y    CLEAR SCREEN DETAIL                          
*                                                                               
DISOUT2  L     R3,ADISDET1                                                      
         USING DISLINED,R3         R3=A(FIRST TWA LINE)                         
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT        R0=NUMBER OF LINES IN TWA                    
         BZ    DISOUTX                                                          
         XC    FVADDR,FVADDR                                                    
         LA    R2,DISLIST          R2=A(RECORD NUMBERS)                         
DISOUT4  GOTO1 ADISREC,(R2)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABLDLIN,DISLINED    DISPLAY THIS RECORD                          
         TM    DISLHDR1+(FVATRB-FVIHDR),FVAPROT                                 
         BO    DISOUT6                                                          
         OC    FVADDR,FVADDR                                                    
         BNZ   DISOUT6                                                          
         LA    RF,DISLHDR1                                                      
         ST    RF,FVADDR           SET CURSOR TO 1ST UNPROT MARK FIELD          
DISOUT6  AH    R3,DISSCRNL         BUMP TO NEXT LINE AND RECORD                 
         LA    R2,2(R2)                                                         
         BCT   R0,DISOUT4          DO FOR N'ITEMS IN TABLE                      
         OC    FVADDR,FVADDR       IF NO UNPROT MARK FIELDS                     
         BNZ   DISOUTX                                                          
         LA    R3,WFMOPTH                                                       
         ST    R3,FVADDR                                                        
*                                                                               
DISOUTX  GOTO1 ABLDPFK                                                          
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         NI    DISIND,255-DISINOSC ENSURE DISIND IS RESET                       
         TM    DISIND,DISIMMSG     TEST MESSAGE ALREADY SET                     
         BNO   DISOUTX1            NO                                           
         NI    DISIND,255-DISIMMSG YES                                          
         OC    DISLIST,DISLIST     BUT OVERRIDE IF DISPLAY SUPPRESSED           
         BNZ   DISOUTX2            BY USER INPUT OPTIONS                        
         CLC   FVMSGNO,=AL2(AI$FSEAP)  AND IT'S THE INITIAL MESSAGE             
         BNE   DISOUTX1                                                         
         MVC   FVMSGNO,=AL2(AI$FSAOD)  N FILS SEL'D, AMEND OPTS TO DISP         
         B     DISOUTX2                                                         
*                                                                               
DISOUTX1 MVC   FVMSGNO,=AL2(AI$EAPFS)                                           
         CLI   TWASCROV,TWASCLST                                                
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$PRDSA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    ROUTE                                                            
         MVC   FVMSGNO,=AL2(AI$EANMW)                                           
         CLI   TWASCROV,TWASCLST                                                
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$EPFNM)                                           
         OC    DISLCNT,DISLCNT     TEST ANYTHING DISPLAYED                      
         BNZ   ROUTE                                                            
         MVC   FVMSGNO,=AL2(AI$NOWFS)                                           
         CLI   TWASCROV,TWASCLST                                                
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$NOREC)                                           
DISOUTX2 LA    R3,WFMOPTH          SET CURSOR TO OPTION FIELD                   
         ST    R3,FVADDR                                                        
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY SUBROUTINE TO GET TSAR OR WORKER POSTING RECORD FOR DISPLAY *         
* NTRY - R1=REQUIRED RECORD NUMBER                                    *         
* EXIT - CC EQU IF RECORD FOUND, OR LOW IF NEXT HIGHER WORKER POSTING *         
*        RECORD FOUND, WHEN DISNUM IS SET TO NUMBER OF RECORD FOUND   *         
***********************************************************************         
DISREC   MVC   HALF,0(R1)          SAVE PASSED RECORD NUMBER                    
         CLI   TWASCROV,TWASCDET   IF DETAIL SCREEN DISPLAYED                   
         BE    DISREC2             MUST WANT WORKER RECORD                      
         GOTO1 ATSARGET            ELSE GET TSAR RECORD                         
         B     DISRECX                                                          
*                                                                               
DISREC2  SR    R0,R0                                                            
         MVC   DISNUM,0(R1)                                                     
         L     R3,AWKIO            RANDOM READ FOR REQUIRED RECORD              
         XC    0(4,R3),0(R3)                                                    
         MVC   2(2,R3),HALF                                                     
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    DISREC6                                                          
         DC    H'0'                                                             
*                                                                               
DISREC4  GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF1                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DISREC6  LA    RE,4(R3)                                                         
         USING PSHEADD,RE                                                       
         CLI   PSHDEL,PSTKELQ      TEST TRANSACTION KEY POSTING                 
         BE    DISREC8                                                          
         CLI   PSHDEL,PSHDELQ      TEST POSTING HEADER                          
         BNE   DISREC4                                                          
         IC    R0,PSHDLEN                                                       
         AR    RE,R0                                                            
         USING TRNELD,RE                                                        
         CLI   TRNEL,TRNELQ        FOLLOWED BY TRANSACTION ELEMENT              
         BE    DISREC8                                                          
         LH    R1,HALF             BUMP RECORD NUMBER                           
         LA    R1,1(R1)                                                         
         STH   R1,HALF                                                          
         B     DISREC4                                                          
*                                                                               
DISREC8  CLC   DISNUM,HALF         SET CC EQU/LOW                               
         MVC   DISNUM,HALF         SET DISNUM TO RECORD FOUND                   
         B     DISRECX                                                          
         DROP  RE                                                               
*                                                                               
DISRECX  B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD HEADINGS AND DISPLAY DISPLACEMENTS                            *         
***********************************************************************         
BLDDIS   MVC   DISHEAD,SPACES                                                   
         XC    DISDISP(DISDISPL),DISDISP                                        
         MVI   FLAG,0              SET FIRST TIME THROUGH                       
*                                                                               
BLDDIS2  LA    R1,DISCOLS          R1=A(COL NUMBERS LIST)                       
         LH    R0,DISSCRNL         L'SCREEN DISPLAY LINE                        
         MVI   CHAR,0                                                           
         LH    R4,DISDISD          DISP TO START OF DATA FROM DISLINED          
         SR    RE,RE               RE=DISPLACEMENT TO DISPLAY VALUE             
BLDDIS4  SR    R2,R2                                                            
         ICM   R2,1,0(R1)          R2=DISPLAY LINE ELEMENT NUMBER               
         BZ    BLDDIS12            END OF LIST                                  
         CH    R2,=Y(DISTABN)      TEST WITHIN DISPLAY RANGE                    
         BH    BLDDIS10                                                         
         CLI   FLAG,0              TEST FIRST TIME THROUGH                      
         BE    BLDDIS6             YES - NOT SETTING DISPLACEMENTS              
         LR    RF,R2               TAKE ELEMENT NO. FOR DISPLAY LINE            
*                                                                               
BLDDIS6  BCTR  R2,0                ESTABLISH DISTAB ENTRY                       
         MH    R2,=Y(DISTABL)      DISNUM-1*4                                   
         A     R2,ADISTAB                                                       
         USING DISTABD,R2          R2=A(DISPLAY ELEMENT TABLE ENTRY)            
*                                                                               
         CLC   DISSLVL,USERSTS     TEST ELEMENT IS RESTRICTED                   
         BH    BLDDIS10            YES - USER HAS INSUFFICIENT STATUS           
         SR    R3,R3               TEST POTENTIAL OVERFLOW                      
         IC    R3,DISLEN           TAKE LENGTH OF ELEMENT                       
         AR    R3,RE               ADD DISPLACEMENT SO FAR                      
         CR    R3,R0                                                            
         BH    BLDDIS10            OVERFLOW - DROP THIS COLUMN                  
         CLI   FLAG,0              TEST FIRST TIME THROUGH                      
         BE    BLDDIS8             YES - DON'T SET DISPLACEMENTS                
         LA    R3,0(R4,RE)         ADD START POINT TO DISPLACEMENT              
         STC   R3,DISDISP-1(RF)    SCREEN LINE DISPLACEMENT                     
         IC    R3,DISLEN           RESET LENGTH OF ELEMENT                      
         BCTR  R3,0                                                             
         MVC   LARFADDR,DISADDR                                                 
         LA    R2,DISHEAD(RE)                                                   
         EX    0,LARF                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         LA    RE,2(RE,R3)         RE=DISPLACEMENT OF NEXT ELEMENT              
         A     RE,FULL2            ADD CONSTANT FACTOR FOR SPARE                
         B     BLDDIS10                                                         
*                                                                               
BLDDIS8  LA    RE,1(R3)            RE=DISPLACEMENT OF NEXT ELEMENT              
         IC    RF,CHAR             TAKE NO. OF COLUMNS TO BE DISPLAYED          
         LA    RF,1(RF)                                                         
         STC   RF,CHAR             BUMP NO. OF COLUMNS TO BE DISPLAYED          
         B     BLDDIS10                                                         
*                                                                               
BLDDIS10 LA    R1,1(R1)            BUMP TO NEXT FIELD                           
         B     BLDDIS4             PERFORM FOR NUMBER OF PROFILES               
*                                                                               
BLDDIS12 CLI   FLAG,0              TEST FIRST TIME THROUGH                      
         BNE   BLDDISX             NO - WE HAVE FINISHED                        
         MVI   FLAG,X'FF'          SET NOT FIRST TIME                           
         XC    FULL2,FULL2         CLEAR CONSTANT FACTOR FOR SPARE              
         LH    R1,DISLINEL                                                      
         SH    RE,=H'1'            SUBTRACT ONE ADDED FOR 'NEXT' DISP           
         SR    R1,RE               GET REMAINING SPACE INTO R1                  
         BNP   BLDDIS2             NOTHING LEFT TO ADD                          
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         CLI   CHAR,2              TEST LESS THAN TWO COLUMNS                   
         BL    BLDDIS2             DON'T DIVIDE OR SET FACTOR                   
         IC    RF,CHAR             NO. OF COLS (THOUGH ONE IS FIXED)            
         DR    R0,RF                                                            
         ST    R1,FULL2            SET CONSTANT FACTOR FOR SPARE                
         B     BLDDIS2             GO BACK AND SET DISPLACEMENTS                
*                                                                               
BLDDISX  B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD DETAIL DISPLAY LINE                                           *         
***********************************************************************         
         USING DISLINED,R2                                                      
BLDLIN   LR    R2,R1                                                            
*                                                                               
BLDLIN2  OI    DISLHDR1+(FVOIND-FVIHDR),FVOXMT                                  
         NI    DISLHDR1+(FVATRB-FVIHDR),255-FVAHIGH                             
         CLI   TWASCROV,TWASCDET   TEST DETAIL SCREEN LOADED                    
         BNE   BLDLIN4                                                          
         OI    DISLHDR1+(FVATRB-FVIHDR),FVAPROT                                 
         XC    DISLDLIN,DISLDLIN                                                
         L     R3,AWKIO            ADDRESS RELEVANT BITS OF WKR RECORD          
         LA    R3,4(R3)                                                         
         USING PSHEADD,R3                                                       
         SR    R4,R4                                                            
         IC    R4,PSHDLEN                                                       
         LA    R4,0(R4,R3)                                                      
         USING TRNELD,R4                                                        
         B     BLDLIN6                                                          
*                                                                               
BLDLIN4  XC    DISLLLIN,DISLLLIN                                                
*        XC    DISLACTN,DISLACTN                                                
         OI    DISLHDR2+(FVOIND-FVIHDR),FVOXMT                                  
         NI    DISLHDR2+(FVATRB-FVIHDR),255-FVAHIGH                             
         TM    TSARIND1,TSARMKQ                                                 
         BNO   BLDLIN6                                                          
         OI    DISLHDR1+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAHIGH                                 
*                                                                               
BLDLIN6  TM    TSARIND2,TSARDISQ   TEST THIS RECORD IS DISPLAY ONLY             
         BNO   *+8                                                              
         OI    DISLHDR2+(FVATRB-FVIHDR),FVAPROT                                 
*                                                                               
BLDLIN8  CLI   PSHDEL,PSTKELQ      TEST TRANSACTION KEY RECORD                  
         BE    BLDLIN40                                                         
         SR    RE,RE               DISPLAY ACCOUNT                              
         ICM   RE,1,DISACCD                                                     
         BZ    BLDLIN10                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'PSHDACC-1,RF),PSHDACC+1                                      
*                                                                               
BLDLIN10 SR    RE,RE               DISPLAY AMOUNT                               
         ICM   RE,1,DISAMTD                                                     
         BZ    BLDLIN12                                                         
         LA    RF,DISLINED(RE)                                                  
         CP    TRNAMNT,PZERO                                                    
         BNL   *+8                                                              
         MVI   L'LC@AMT-3(RF),C'-'                                              
         MVC   L'LC@AMT-2(L'AC2DR,RF),AC2DR                                     
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                                                             
         MVC   L'LC@AMT-2(L'AC2CR,RF),AC2CR                                     
         CURED TRNAMNT,(L'LC@AMT-3,(RF)),2                                      
*                                                                               
BLDLIN12 SR    RE,RE               DISPLAY BATCH REFERENCE                      
         ICM   RE,1,DISBRFD                                                     
         BZ    BLDLIN14                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TRNBREF,RF),TRNBREF                                          
*                                                                               
BLDLIN14 SR    RE,RE               DISPLAY CONTRA A/C                           
         ICM   RE,1,DISCACD                                                     
         BZ    BLDLIN16                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'PSHDSBAC-1,RF),PSHDSBAC+1                                    
*                                                                               
BLDLIN16 SR    RE,RE               DISPLAY CODES                                
         ICM   RE,1,DISCODD                                                     
         BZ    BLDLIN18                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'PSHDANAL,RF),PSHDANAL                                        
         MVC   L'PSHDANAL+1(L'TRNOFFC,RF),TRNOFFC                               
*                                                                               
BLDLIN18 SR    RE,RE               DISPLAY DATE                                 
         ICM   RE,1,DISDATD                                                     
         BZ    BLDLIN20                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   WORK(L'TSARDDAT),TSARDDAT                                        
         CLI   TWASCROV,TWASCLST                                                
         BE    *+10                                                             
         MVC   WORK(L'TRNDATE),TRNDATE                                          
         GOTO1 VDATCON,DMCB,(1,WORK),(17,(RF))                                  
*                                                                               
BLDLIN20 SR    RE,RE               DISPLAY MONTH                                
         ICM   RE,1,DISMTHD                                                     
         BZ    BLDLIN22                                                         
         LA    RF,DISLINED(RE)                                                  
         LR    R0,RF                                                            
         CLI   TRNMOS,C' '         TEST TRNMOS SET                              
         BNH   *+12                                                             
         CLI   TRNMOS+1,C' '                                                    
         BH    *+12                                                             
         MVI   2(RF),C'?'          NO BATCH MONTH                               
         B     BLDLIN22                                                         
         GOTO1 VCONVMOS,DMCB,(R4),WORK                                          
         MVI   WORK+2,X'01'                                                     
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(R0))                                   
*                                                                               
BLDLIN22 SR    RE,RE               DISPLAY FILE NAME                            
         ICM   RE,1,DISNAMD                                                     
         BZ    BLDLIN24                                                         
         LA    RF,DISLINED(RE)                                                  
         TM    OPTIND1,SCIITOT     TEST SHOW FILE TOTALS INSTEAD                
         BZ    BLDLIN23                                                         
         TM    TSARIND2,TSARNCTQ   IF AVAILABLE                                 
         BNZ   BLDLIN23                                                         
         TM    TSARIND2,TSARNOAC   AND FILE CAN'T BE LISTED                     
         BZ    BLDLIN23                                                         
         MVC   0(L'LC@TOTL,RF),LC@TOTL                                          
         MVI   L'LC@TOTL(RF),C'='                                               
         LA    RF,L'LC@TOTL+1(RF)                                               
         CURED TSARDCSH,(L'TSARDFNM-(L'LC@TOTL+2),(RF)),2,ALIGN=LEFT            
         B     BLDLIN24                                                         
BLDLIN23 MVC   0(L'TSARDFNM,RF),TSARDFNM                                        
         TM    OPTIND1,SCIISNO     APPEND SENO TO NAME (DDS OPTION)             
         BZ    BLDLIN24                                                         
         LA    RF,L'TSARDFNM-3(RF)                                              
         MVI   0(RF),C'/'                                                       
         XOUT  TSARSENO,1(RF),1                                                 
*                                                                               
BLDLIN24 SR    RE,RE               DISPLAY RECORD COUNT                         
         ICM   RE,1,DISRCSD                                                     
         BZ    BLDLIN26                                                         
         LA    RF,DISLINED(RE)                                                  
         CURED TSARDCNT,(L'LC@RCS,(RF)),0,ZERO=BLANK                            
*                                                                               
BLDLIN26 SR    RE,RE               DISPLAY REFERENCE                            
         ICM   RE,1,DISREFD                                                     
         BZ    BLDLIN28                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TRNREF,RF),TRNREF                                            
*                                                                               
BLDLIN28 SR    RE,RE               DISPLAY STATUS                               
         ICM   RE,1,DISSTSD                                                     
         BZ    BLDLIN34                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'LC@STS,RF),SPACES                                            
         TM    TSARIND1,TSWKBADQ   TEST WKR FILE IS CORRUPT/NO TRAILER          
         BNO   *+14                                                             
         MVC   0(L'AC8CRPT,RF),AC8CRPT                                          
         B     BLDLIN34                                                         
         MVC   0(L'LC4ACTV,RF),LC4ACTV                                          
         TM    TSARDSTS,WKISACTV                                                
         BO    BLDLIN30                                                         
         MVC   0(L'LC4HOLD,RF),LC4HOLD                                          
         TM    TSARDSTS,WKISHOLD                                                
         BO    BLDLIN30                                                         
         MVC   0(L'LC4DELT,RF),LC4DELT                                          
         TM    TSARDSTS,WKISDELT                                                
         BO    BLDLIN30                                                         
         MVC   0(L'LC4ACTV,RF),=4C'?'    UNKNOWN STATUS                         
BLDLIN30 TM    TSARDSTS,WKISKEEP   TEST ANY STATUS QUALIFIER                    
         BO    BLDLIN32                                                         
         TM    TSARIND2,TSWKCOPQ                                                
         BO    BLDLIN32                                                         
         TM    TSARIND1,TSWKREVQ+TSWKPRGQ                                       
         BZ    BLDLIN34                                                         
BLDLIN32 CLI   L'LC4ACTV(RF),C' '                                               
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   L'LC4ACTV+1(RF),C','                                             
*                                  APPEND QUALIFIER TO STATUS                   
         TM    TSARDSTS,WKISKEEP                                                
         BNO   *+14                                                             
         MVC   L'LC4ACTV+2(L'LC4KEEP,RF),LC4KEEP                                
         B     BLDLIN34                                                         
*                                  PSEUDO-STATUS VALUES                         
         TM    TSARIND1,TSWKPRGQ                                                
         BNO   *+14                                                             
         MVC   L'LC4ACTV+2(L'LC4PRGE,RF),LC4PRGE                                
         B     BLDLIN34                                                         
*                                                                               
         TM    TSARIND1,TSWKREVQ                                                
         BNO   *+14                                                             
         MVC   L'LC4ACTV+2(L'LC4REVS,RF),LC4REVS                                
         B     BLDLIN34                                                         
*                                                                               
         TM    TSARIND2,TSWKCOPQ                                                
         BNO   *+14                                                             
         MVC   L'LC4COPY+2(L'LC4COPY,RF),LC4COPY                                
         B     BLDLIN34                                                         
*                                                                               
BLDLIN34 SR    RE,RE               DISPLAY TIME                                 
         ICM   RE,1,DISTIMD                                                     
         BZ    BLDLIN36                                                         
         LA    RF,DISLINED(RE)                                                  
         UNPK  DUB+1(5),TSARDTIM(3)  LET UNPK FLIP DUMMY LOW BYTE               
         MVC   DUB(2),DUB+1                                                     
         MVI   DUB+2,C':'          FORMAT HH:MM                                 
         MVC   0(5,RF),DUB                                                      
*                                                                               
BLDLIN36 SR    RE,RE               DISPLAY USER-ID                              
         ICM   RE,1,DISUIDD                                                     
         BZ    BLDLIN38                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TSARDUNM,RF),TSARDUNM                                        
*                                                                               
BLDLIN38 SR    RE,RE               DISPLAY WORKER KEY                           
         ICM   RE,1,DISWKYD                                                     
         BZ    BLDLINX                                                          
         LA    RF,DISLINED(RE)     BUILD KEY FOR DISPLAY                        
         MVC   0(L'TSARKSPG,RF),TSARKSPG                                        
         LA    RF,L'TSARKSPG(RF)                                                
         OC    0(L'TSARKSUB,RF),TSARKSUB                                        
         BNZ   *+8                                                              
         MVI   0(RF),C'*'          DEFAULT                                      
         LA    RF,L'TSARKSUB(RF)                                                
         UNPK  0(3,RF),TSARKDAY(2)                                              
         AHI   RF,2                                                             
         MVC   0(L'TSARKTYP,RF),TSARKTYP                                        
         AHI   RF,1                                                             
         CLI   TSARKXTR,C' '                                                    
         BNH   BLDLIN39                                                         
         MVC   0(L'TSARKXTR,RF),TSARKXTR                                        
         AHI   RF,1                                                             
BLDLIN39 MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         CURED (B2,TSARKSEQ),(4,(RF)),0,ALIGN=LEFT                              
         B     BLDLINX                                                          
*                                  FORMAT FIELDS FROM ADJ WORKER FILE           
         USING PSTKEYD,R3                                                       
BLDLIN40 LA    R3,PSTKEY                                                        
         USING TRNRECD,R3                                                       
         SR    RE,RE               DISPLAY ACCOUNT                              
         ICM   RE,1,DISACCD                                                     
         BZ    BLDLIN42                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TRNKULA,RF),TRNKULA                                          
*                                                                               
BLDLIN42 SR    RE,RE               DISPLAY CONTRA ACCOUNT                       
         ICM   RE,1,DISCACD                                                     
         BZ    BLDLIN44                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TRNKULC,RF),TRNKULC                                          
*                                                                               
BLDLIN44 SR    RE,RE               DISPLAY CODES                                
         ICM   RE,1,DISCODD                                                     
         BZ    BLDLIN46                                                         
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TRNKOFF,RF),TRNKOFF                                          
*                                                                               
BLDLIN46 SR    RE,RE               DISPLAY DATE                                 
         ICM   RE,1,DISDATD                                                     
         BZ    BLDLIN48                                                         
         LA    RF,DISLINED(RE)                                                  
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(17,(RF))                              
*                                                                               
BLDLIN48 SR    RE,RE               DISPLAY REFERENCE                            
         ICM   RE,1,DISREFD                                                     
         BZ    BLDLINX                                                          
         LA    RF,DISLINED(RE)                                                  
         MVC   0(L'TRNKREF,RF),TRNKREF                                          
*                                                                               
BLDLINX  B     ROUTE                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD PFKEY DISPLAY LINE                                            *         
***********************************************************************         
BLDPFK   MVC   TEMP,SPACES                                                      
         LA    RF,TEMP             RF=A(PFKEY BUILD AREA)                       
         LR    R3,RF               SAVE A(START OF STRING)                      
         L     R1,APFKTAB                                                       
         LA    R4,X'D0'            BNH                                          
         TM    TWAMODE,TWAMALTP    TEST ACTION/SCROLL KEYS                      
         BZ    *+8                                                              
         LA    R4,X'20'            BH                                           
*                                                                               
         USING PFKTABD,R1                                                       
BLDPFK2  SR    R0,R0                                                            
         ICM   R0,1,PFKTNUM        R0=PFKEY NUMBER                              
         BZ    BLDPFKX             EOT                                          
         OC    PFKTLCAD,PFKTLCAD                                                
         BZ    BLDPFK4             NON-DISPLAY PFKEY                            
         CLI   PFKTNUM,PFK06       TEST ACTION/SCROLL PFKEY                     
         EX    R4,*+8                                                           
         B     *+8                                                              
         NOP   BLDPFK4                                                          
         TM    PFKTIND1,PFKTILST+PFKTIDET  TEST PFKEY VALID EITHER SCRN         
         BZ    BLDPFK3             YES                                          
         IC    RE,TWASCROV         TEST SCREEN/PFKEY COMPATIBILITY              
         SLL   RE,30                                                            
         SRL   RE,30               RE=PFKTILST OR PFKTIDET                      
         EX    RE,*+8                                                           
         BNO   BLDPFK4                                                          
         TM    PFKTIND1,0                                                       
BLDPFK3  CLI   PFKTIND2,0          TEST PFKEY RESTRICTED                        
         BE    BLDPFK6             NO                                           
         CLC   PFKTIND2,USERSTS                                                 
         BNH   BLDPFK6                                                          
BLDPFK4  LA    R1,PFKTABL(R1)      SET A(NEXT TABLE ENTRY)                      
         B     BLDPFK2                                                          
*                                                                               
BLDPFK6  BAS   RE,SETPFK           SET PFNN=                                    
         MVC   LAREADDR,PFKTLCAD   SET SCON ADDRESS                             
         CLI   PFKTNUM,PFK06                                                    
         BNH   BLDPFK10                                                         
         TM    PFKTIND1,SCRLMAXI                                                
         BO    BLDPFK10            FIRST/LAST DOESN'T NEED DIRECTION            
         TM    PFKTIND1,SCRLDOWN   SET DIRECTION IF DEFINED IN TABLE            
         BNO   BLDPFK8                                                          
         MVI   0(RF),C'+'                                                       
         LA    RF,1(RF)                                                         
         B     BLDPFK10                                                         
BLDPFK8  TM    PFKTIND1,SCRLUP                                                  
         BNO   BLDPFK10                                                         
         MVI   0(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
BLDPFK10 EX    0,LARE              GET A(ACTION/SCROLL WORD)                    
         MVC   0(L'LC@PAGE,RF),0(RE)                                            
         LA    RF,L'LC@PAGE-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)            POINT ONE PAST END OF WORD                   
         LA    R1,PFKTABL(R1)      SET A(NEXT TABLE ENTRY)                      
         B     BLDPFK2                                                          
*                                                                               
BLDPFKX  BAS   RE,SETPFK           SET ALTPF KEY IF ROOM                        
         B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* BLDPFK SUB-ROUTINE TO BUILD PFKEY KEYWORD                           *         
***********************************************************************         
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
*                                                                               
SETPFK2  CLI   TWASCROV,TWASCLST   NO ALTPFS FOR LIST SCREEN                    
         BE    SETPFK4                                                          
         SR    RF,R3               SET PF1=ALTPFS IF ENOUGH ROOM                
         BZ    *+12                                                             
         CH    RF,=Y(L'WFMLPFK-L'LC@ALTPF-5)                                    
         BH    SETPFK4                                                          
         AR    RF,R3               POINT BACK TO OUTPUT AREA                    
         CR    RF,R3                                                            
         BNE   *+14                                                             
         MVC   0(2,RF),AC@PFK                                                   
         LA    RF,2(RF)                                                         
         MVI   0(RF),C'1'          SET ALTPF KEY VALUE                          
         MVI   1(RF),C'='                                                       
         MVC   2(L'LC@ALTPF,RF),LC@ALTPF                                        
SETPFK4  L     R2,ADISPFKS         R2=A(HEADER OF PF KEY TWA FIELD)             
         OI    FVOIND-FVIHDR(R2),FVOXMT                                         
         LA    RF,TEMP+L'WFMLPFK                                                
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-12                                                          
         MVC   L'FVIHDR(L'WFMLPFK,R2),TEMP                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ACTION CONFIRMATION ROUTINE                                         *         
* NTRY R1=A(ACTION NAME)                                              *         
***********************************************************************         
CONFRM   L     RE,FVADDR                  RE=A(ACTION FIELD HDR)                
         LA    RF,DISLCFMH-DISLHDR1(RE)   RF=A(CONFIRMATION FIELD HDR)          
         TM    TWAMODE,TWAMCFRM                                                 
         BO    CONFRM2             CHECK USER RESPONSE                          
         OI    TWAMODE,TWAMCFRM                                                 
         OI    FVATRB-FVIHDR(RE),FVAPROT  PROTECT ACTION FIELD                  
         OI    FVOIND-FVIHDR(RE),FVOXMT                                         
         NI    FVATRB-FVIHDR(RF),255-FVAPROT  UNPROTECT CONFIRM FIELD           
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         ST    RF,FVADDR                                                        
         XC    FVSUBT,FVSUBT                                                    
         MVI   FVSUBT+0,2          &1=CONFIRMATION CHARACTER                    
         MVC   FVSUBT+1(1),AC@YES                                               
         MVI   FVSUBT+2,8+1        &2=ACTION NAME                               
         MVC   FVSUBT+3(L'AC@KEEP),0(R1)                                        
         B     RECFACT             ASK FOR USER CONFIRMATION                    
*                                                                               
CONFRM2  NI    TWAMODE,255-TWAMCFRM                                             
         NI    FVATRB-FVIHDR(RE),255-FVAPROT  UNPROTECT ACTION FIELD            
         OI    FVOIND-FVIHDR(RE),FVOXMT                                         
         OI    FVATRB-FVIHDR(RF),FVAPROT  RE-PROTECT CONFIRM FIELD              
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         CLC   L'FVIHDR(1,RF),AC@YES      TEST ACTION CONFIRMED                 
         MVI   L'FVIHDR(RF),C' '          CLEAR IT                              
         B     ROUTX                      EXIT WITH CC SET                      
         EJECT                                                                  
***********************************************************************         
* STANDARD FIELD VALIDATION ROUTINE                                   *         
* NTRY - R1=A(FIELD HEADER)                                           *         
*        FVMINL=MINIMUM FIELD LENGTH OR 0 IF NOT REQUIRED             *         
*        FVMAXL=MAXIMUM FIELD LENGTH OR 0                             *         
***********************************************************************         
FLDVAL   ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVC   FVIFLD,SPACES                                                    
         MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         SR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         LA    R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    *+8                                                              
         LA    R0,L'FVIHDR+L'FVIHDR+1                                           
         SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         EX    RF,*+8              EXTRACT FIELD DATA                           
         B     *+10                                                             
         MVC   FVIFLD(0),L'FVIHDR(R1)                                           
*                                                                               
         LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         LA    RF,1(RF)            RF=LOOP COUNT                                
FLDVAL2  CLI   0(R1),C' '          LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FLDVAL4                                                          
         MVI   0(R1),C' '          SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCT   RF,FLDVAL2                                                       
FLDVAL4  STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(EGIFSHRT) ENSURE NOT TOO SHORT OR LONG              
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         CLM   RF,1,FVMINL                                                      
         BL    FLDVALX                                                          
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLM   RF,1,FVMAXL                                                      
         BH    FLDVALX                                                          
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(IGOK)                                               
         B     FLDVAL14                                                         
*                                  SET FIELD VALIDITY BITS                      
         MVC   FVMSGNO,=AL2(IGOK) INDICATE FIELD IS OK                          
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
FLDVAL6  TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FLDVAL10                                                         
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         B     FLDVAL8                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,255-FVIALF                                                
         B     FLDVAL8                                                          
         NI    FVIIND,255-FVINUM                                                
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,255-FVIHEX                                                
FLDVAL8  BCTR  R1,0                                                             
         BCT   RF,FLDVAL6                                                       
FLDVAL10 IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         CLI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         BNE   FLDVAL12                                                         
         TM    FVIIND,FVINUM                                                    
         BZ    FLDVAL12                                                         
         EX    RF,*+8              SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  DUB,FVIFLD(0)                                                    
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
FLDVAL12 MVC   FVMSGNO,=AL2(IGOK)                                               
*                                                                               
FLDVAL14 MVI   FVHELP,0            RESET THIS TIME VALUES                       
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVNUMER,0                                                        
         MVC   FVXTRA,SPACES                                                    
         B     FLDVALX                                                          
*                                  HANDLE ERRORS HERE                           
FLDVALX  CLC   FVMSGNO,=AL2(EGIFMISS)                                           
         BE    FLDVALX2                                                         
         MVI   FVFLAG,0                                                         
         CLI   FVILEN,0                                                         
         BE    FLDVALXX                                                         
         MVI   FVFLAG,1                                                         
         CLC   FVMSGNO,=AL2(IGOK)                                               
         BE    FLDVALXX                                                         
FLDVALX2 MVI   FVFLAG,2                                                         
FLDVALXX CLI   FVFLAG,1            SET CONDITION CODE FOR CALLER                
         MVI   FVFLAG,0                                                         
         B     ROUTX               RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* FILTER TSAR OR WORKER RECORDS ON OPTIONS ENTERED                    *         
***********************************************************************         
FILTER   CLI   TWASCROV,TWASCDET   TEST DETAIL SCREEN                           
         BNE   FILT2                                                            
         L     R3,AWKIO                                                         
         LA    R3,4(R3)            SET R3=A(WORKER REC)                         
         USING PSHEADD,R3                                                       
         SR    R4,R4                                                            
         IC    R4,PSHDLEN                                                       
         LA    R4,0(R4,R3)         SET R4=A(TRNEL)                              
         USING TRNELD,R4                                                        
*                                                                               
FILT2    OC    OPTUSER,OPTUSER     USER-ID                                      
         BZ    FILT4                                                            
         CLC   TSARKUID,OPTUSER                                                 
         BNE   FILTNEQ                                                          
*                                                                               
FILT4    CLI   OPTSYS,0            SYSTEM                                       
         BE    FILT6                                                            
         CLC   TSARKSYS,OPTSYS                                                  
         BNE   FILTNEQ                                                          
*                                                                               
FILT6    OC    OPTPRG,OPTPRG       PROGRAM                                      
         BZ    FILT10                                                           
         LA    R0,L'TSARKPRG                                                    
         LA    RE,TSARKPRG                                                      
         LA    RF,OPTPRG                                                        
FILT8    CLC   0(1,RE),0(RF)                                                    
         BE    *+12                                                             
         CLI   0(RF),0                                                          
         BNE   FILTNEQ                                                          
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT8                                                         
         BNE   FILTNEQ                                                          
*                                                                               
FILT10   CLI   OPTSUB,0            SUB-PROGRAM                                  
         BE    FILT12                                                           
         CLC   TSARKSUB,OPTSUB                                                  
         BNE   FILTNEQ                                                          
*                                                                               
FILT12   CLI   OPTDAY,0            DAY ADDED                                    
         BE    FILT14                                                           
         CLC   TSARKDAY,OPTDAY                                                  
         BNE   FILTNEQ                                                          
*                                                                               
FILT14   CLI   OPTTYPE,0           FILE TYPE                                    
         BE    FILT14B                                                          
         CLC   TSARKTYP,OPTTYPE                                                 
         BNE   FILTNEQ                                                          
*                                                                               
FILT14B  CLI   OPTEXTRA,0          EXTRA SYSTEM ID                              
         BE    FILT15                                                           
         CLC   TSARKXTR,OPTEXTRA                                                
         BNE   FILTNEQ                                                          
*                                                                               
FILT15   CLI   OPTCHQS,0           CHEQUE FILES                                 
         BE    FILT16                                                           
         CLI   OPTCHQS,C'Y'        TEST INCLUDING CHEQUES                       
         BE    FILT16                                                           
         CLI   TSARKTYP,C'P'       TEST POSTING FILE                            
         BNE   FILT15A             NO                                           
         CLI   OPTCHQS,C'O'        TEST EXCLUDING POSTING FILES                 
         BE    FILTNEQ             YES                                          
         B     FILT16                                                           
FILT15A  CLI   OPTCHQS,C'N'        ELSE TEST EXCLUDING CHEQUES FILES            
         BE    FILTNEQ             YES                                          
*                                                                               
FILT16   OC    OPTSSEQ(L'OPTSSEQ+L'OPTESEQ),OPTSSEQ  SEQUENCE NO. RANGE         
         BZ    FILT18                                                           
         CLC   TSARKSEQ,OPTSSEQ                                                 
         BL    FILTNEQ                                                          
         CLC   TSARKSEQ,OPTESEQ                                                 
         BH    FILTNEQ                                                          
*                                                                               
FILT18   OC    OPTSDAT(KEYDATSL),OPTSDAT  DATE RANGE FILTER                     
         BZ    FILT22                                                           
         MVC   WORK(L'TSARDDAT),TSARDDAT                                        
         CLI   TWASCROV,TWASCLST                                                
         BE    FILT20                                                           
         MVC   WORK(L'TRNDATE),TRNDATE                                          
FILT20   CLC   WORK(L'TRNDATE),OPTSDAT                                          
         BL    FILTNEQ                                                          
         CLC   WORK(L'TRNDATE),OPTEDAT                                          
         BH    FILTNEQ                                                          
*                                                                               
FILT22   SR    R1,R1               FILE STATUS                                  
         ICM   R1,1,OPTFSTS                                                     
         BZ    FILT23                                                           
         LA    RF,X'E0'            CC=BNO                                       
         CLI   OPTFSBC,0           TEST INCLUDE/EXCLUDE                         
         BE    *+8                                                              
         LA    RF,X'10'            CC=BO                                        
         EX    R1,*+8                                                           
         EX    RF,*+8                                                           
         TM    TSARDSTS,0                                                       
         NOP   FILTNEQ                                                          
*                                                                               
FILT23   CLI   OPTSENO,0           SYSTEM NUMBER                                
         BE    FILT24                                                           
         CLC   TSARSENO,OPTSENO                                                 
         BNE   FILTNEQ                                                          
*                                                                               
FILT24   ICM   R1,1,OPTACCL        ACCOUNT                                      
         BZ    FILT26                                                           
         BCTR  R1,0                                                             
         LA    RF,PSHDACC          RF=A(ACC CODE)                               
         CLI   PSHDEL,PSHDELQ                                                   
         BE    *+8                                                              
         LA    RF,PSTKEY-PSTKEYD(R3)                                            
         EX    R1,*+8                                                           
         BE    FILT26                                                           
         CLC   1(0,RF),OPTACC                                                   
         B     FILTNEQ                                                          
*                                                                               
FILT26   ICM   R1,1,OPTCACL        CONTRA                                       
         BZ    FILT28                                                           
         BCTR  R1,0                                                             
         LA    RF,PSHDSBAC+1        RF=A(C/A CODE)                              
         CLI   PSHDEL,PSHDELQ                                                   
         BE    *+8                                                              
         LA    RF,(PSTKEY+(TRNKCULC-TRNRECD))-PSTKEYD(R3)                       
         EX    R1,*+8                                                           
         BE    FILT28                                                           
         CLC   0(0,RF),OPTCAC                                                   
         B     FILTNEQ                                                          
*                                                                               
FILT28   CLI   PSHDEL,PSTKELQ                                                   
         BE    FILT32                                                           
         OC    OPTAMT,OPTAMT       AMOUNT - VALUE OR DR/DR                      
         BZ    FILT32                                                           
         TM    OPTAMT,X'C0'                                                     
         BNO   FILT30                                                           
         LA    RF,X'10'            CC=BO                                        
         CLI   OPTSIGN,C'D'                                                     
         BE    *+8                                                              
         LA    RF,X'E0'            CC=BNO                                       
         TM    TRNSTAT,TRNSDR                                                   
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   FILT32                                                           
         B     FILTNEQ                                                          
FILT30   CP    TRNAMNT,OPTAMT                                                   
         BNE   FILTNEQ                                                          
*                                                                               
FILT32   ICM   R1,1,OPTREFL                                                     
         BZ    FILT34                                                           
         BCTR  R1,0                                                             
         LA    RF,TRNREF                                                        
         CLI   PSHDEL,PSTKELQ                                                   
         BNE   *+8                                                              
         LA    RF,(PSTKEY+(TRNKREF-TRNRECD))-PSTKEYD(R3)                        
         EX    R1,*+8                                                           
         BE    FILT34                                                           
         CLC   0(0,RF),OPTREF                                                   
         B     FILTNEQ                                                          
*                                                                               
FILT34   DS    0H                                                               
*                                                                               
FILTEQU  B     ROUTE                                                            
*                                                                               
FILTNEQ  B     ROUTL                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH AND FILTER USER-ID VALUES                      *         
***********************************************************************         
         USING UIDTABD,R2                                                       
GETUID   L     R2,AUIDNTRY                                                      
         CLC   UIDTUSER,WKIUSER    TEST SAME AS LAST                            
         BE    GETUIDX             YES - DON'T SEARCH                           
         GOTO1 VBINSRCH,DMCB,(2,WKIUSER),AUIDTAB,UIDTCNT,UIDTABL,      >        
               (0,L'WKIUSER),UIDTMAX                                            
         MVC   AUIDNTRY,DMCB                                                    
         CLI   DMCB,1              TEST ENTRY FOUND                             
         BNE   GETUIDX             YES                                          
*                                  NO - ADD A NEW ONE                           
         XC    WORK(UIDTABL),WORK                                               
         LA    R2,WORK             BUILD NEW ENTRY IN WORK                      
         L     R3,AIOAS            =A(IOAREA1)                                  
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY       READ CONTROL FILE                            
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,WKIUSER                                                  
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                           
         BNE   GETUID14                                                         
*                                                                               
         LA    R1,CTIDATA          SEARCH USER-ID RECORD FOR VALUES             
         SR    R0,R0                                                            
GETUID4  CLI   0(R1),0             TEST E-O-R                                   
         BE    GETUID14                                                         
         CLI   0(R1),CTDSCELQ      TEST DESCRIPTION ELEMENT                     
         BE    GETUID8                                                          
         CLI   0(R1),CTSYSELQ      TEST SYSTEM ELEMENT                          
         BE    GETUID10                                                         
*                                                                               
GETUID6  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETUID4                                                          
*                                                                               
         USING CTDSCEL,R1                                                       
GETUID8  MVC   UIDTCODE,CTDSC      EXTRACT USER-ID CODE                         
         B     GETUID6                                                          
*                                                                               
         USING CTSYSEL,R1                                                       
GETUID10 CLI   CTSYSNUM,6          TEST ACCOUNTING SYTEM                        
         BNE   GETUID6                                                          
         MVC   UIDTSENO,CTSYSSE    EXTRACT ACCOUNT SE NUMBER                    
         B     GETUID6                                                          
*                                                                               
GETUID14 OC    KEYSENO,KEYSENO     TEST USER SPECIFIED SENO                     
         BZ    *+14                                                             
         CLC   UIDTSENO,KEYSENO                                                 
         BNE   ROUTH               DROP THIS USER-ID                            
         OC    UIDTCODE,UIDTCODE   TEST USER-ID CODE SET                        
         BNZ   GETUID16                                                         
         SR    R0,R0               EDIT USER-ID NUMBER IF NO CODE               
         ICM   R0,3,UIDTUSER                                                    
         BNZ   *+8                                                              
         ICM   R0,3,WKIUSER                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   UIDTCODE,SPACES                                                  
         UNPK  UIDTCODE(5),DUB                                                  
*                                  ADD THE ENTRY                                
GETUID16 GOTO1 VBINSRCH,DMCB,(1,WORK),AUIDTAB,UIDTCNT,UIDTABL,         >        
               (0,L'WKIUSER),UIDTMAX                                            
         MVC   AUIDNTRY,DMCB                                                    
         OC    AUIDNTRY,AUIDNTRY                                                
         BNZ   GETUIDX                                                          
         DC    H'0'                TABLE IS FULL                                
         MVC   UIDTCNT,DMCB+8      UPDATE COUNT                                 
*                                                                               
GETUIDX  B     ROUTE                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ PRGTAB AND RETURN A(FILE NAME) FOR WORKER INDEX     *         
* NTRY   R1:HOB = X'FF' TO RETURN DD EQUATE OF FILE NAME ONLY         *         
*           AL3(WORKER KEY)                                           *         
* EXIT FULL:HOB = PRGTICHQ IF FILE IS CHEQUE TYPE                     *         
*               = PRGTUPDT IF UPDATEABLE BY NON-DDS                   *         
*           AL3(FILE NAME)                                            *         
***********************************************************************         
         USING PRGTABD,RE                                                       
GETWKN   L     RE,APRGTAB                                                       
         USING TSARWKEY,R1                                                      
GETWK2   CLI   0(RE),PRGTEOTQ      END OF TABLE                                 
         BE    GETWKX              UNKNOWN TYPE                                 
         CLC   PRGTSPR,TSARKSPG    MATCH ON SYS/PRG                             
         BE    GETWK4                                                           
         LA    RE,PRGTABL(RE)                                                   
         B     GETWK2                                                           
*                                                                               
GETWK4   TM    PRGTIND2,PRGTXWFM   TEST FILE NOT FOR WFM                        
         BZ    *+12                                                             
         CLI   USERSTS,IAMGODQ     CONTINUE ONLY IF STATUS PERMITS              
         BNE   GETWKX                                                           
         ST    RE,APRGTENT                                                      
         XC    FULL,FULL                                                        
         CLM   R1,8,EFFS           TEST EQUATE ONLY REQUIRED                    
         BNE   GETWK5                                                           
         MVC   FULL,PRGTDDEQ       RETURN DD EQUATE                             
         B     ROUTE                                                            
*                                                                               
GETWK5   XC    WORK,WORK           BUILD FILE NAME IN WORK                      
         SR    RF,RF                                                            
         ICM   RF,3,PRGTDISP       RE=DISP INTO DSLISTL OF DD EXPRESSN          
         LA    RF,DSLISTL(RF)                                                   
         MVC   WORK(L'LC@PGA08),0(RF)                                           
         TM    PRGTIND1,PRGTICHQ   TEST CHEQUE FILE                             
         BNO   GETWK8                                                           
         LA    R2,WORK+L'LC@PGA08                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)            APPEND CHQ TYPE TO FILE NAME                 
         USING CHQTABD,RE                                                       
         L     RE,ACHQTAB                                                       
GETWK6   CLI   CHQTLEDG,CHQTEOTQ                                                
         BE    GETWK8              NOT FOUND                                    
         CLC   CHQTLEDG,TSARKSUB   MATCH LEDGER CODE FROM TSARWKEY              
         BE    *+12                                                             
         LA    RE,CHQTABL(RE)                                                   
         B     GETWK6                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CHQTDISP                                                    
         LA    RF,DSLISTL(RF)                                                   
         MVC   0(L'LC@PGA08,R2),0(RF)                                           
GETWK8   LA    RF,WORK                                                          
         STCM  RF,7,FULL+1         SET A(FILE NAME)                             
         B     ROUTE                                                            
*                                                                               
GETWKX   B     ROUTH                                                            
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO READ ACCOUNT FILES                                      *         
***********************************************************************         
IOEXEC   ST    R1,IOCTRL                                                        
         MVI   IOERROR,0           CLEAR ERROR BYTE                             
         MVI   IOQUAL,0            ESTABLISH IO QUALIFIERS                      
         TM    IOCTCOMM,IOLOCK     TEST READ FOR UPDATE                         
         BZ    *+8                                                              
         OI    IOQUAL,IOQLOCK                                                   
         TM    IOCTCOMM,IORDEL     TEST READ DELETES                            
         BZ    *+8                                                              
         OI    IOQUAL,IOQDELT                                                   
*                                  CLEAR READ FOR UPDATE/DELETE BITS            
         NI    IOCTCOMM,255-IOLOCK-IORDEL                                       
*                                                                               
         LA    R1,IO1Q+IO2Q        TEST IOAREA NUMBER PASSED                    
         N     R1,IOCTRL           SET IOAREA IN R1                             
         BNZ   *+6                                                              
         DC    H'0'                IOAREA NUMBER MISSING                        
         SRL   R1,6                ESTABLISH IO AREA ADDRESSES                  
         CLM   R1,1,=AL1(IOAMAX)                                                
         BNH   *+6                                                              
         DC    H'0'                INVALID IO AREA                              
         BCTR  R1,0                                                             
         MH    R1,=Y(IOALQ)                                                     
         L     R0,AIOAS            R0=A(IOAS)                                   
         AR    R1,R0                                                            
         ST    R1,AIOSAVE          SAVE A(SAVED D/A IODA/IOWORK)                
         LA    R1,L'IOSAVE(R1)                                                  
         ST    R1,AIOBUFF          SAVE A(DATA RECORD AREA)                     
*                                                                               
         LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   *+6                                                              
         DC    H'0'                FILE NUMBER MISSING                          
         SRL   R1,8                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(L'ACCDIR)                                                  
         LA    R1,ACCDIR(R1)                                                    
         MVC   IOFILE,0(R1)        SET FILE NAME                                
*                                                                               
         LA    R1,IOCMNDS          ESTABLISH DATAMGR COMMAND                    
         N     R1,IOCTRL                                                        
         BNZ   *+6                                                              
         DC    H'0'                COMMAND NUMBER MISSING                       
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
         TM    IOCTFILE,IOACCDA    TEST D/A IO (TO ACCMST/ACCARC)               
         BO    *+12                                                             
         LA    R1,ISCMNDS(R1)      INDEX INTO I/S COMMANDS                      
         B     *+8                                                              
         LA    R1,DACMNDS(R1)      INDEX INTO D/A COMMANDS                      
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BNE   *+12                                                             
         LA    R1,L'ISCMNDS(R1)    OFFSET INTO COMMANDS FOR OLD FILE            
         B     IOEX4                                                            
*                                                                               
         MVC   IOCOMM,0(R1)        SET COMMAND NAME                             
         TM    IOCTFILE,IOACCDA    TEST D/A IO (TO ACCMST/ACCARC)               
         BNO   IOEX2                                                            
*                                                                               
         ICM   R0,15,AIOBUFF       D/A DATAMGR IO                               
         BNZ   *+6                                                              
         DC    H'0'                A(IO BUFFER) MISSING                         
*                                                                               
         GOTO1 VDMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,IODA,(R0),IOWORK               
*                                                                               
         MVC   IOERROR,8(R1)                                                    
         TM    IOERROR,IOEALL-IOEDEL                                            
         BNZ   IOEXECX                                                          
         L     R1,AIOSAVE                                                       
         MVC   0(L'IODA,R1),IODA                                                
         MVC   L'IODA(L'IOWORK,R1),IOWORK                                       
         B     IOEXECX                                                          
*                                                                               
IOEX2    CLI   IOCTCOMM,IOHIGH     TEST READ HIGH                               
         BE    *+12                                                             
         CLI   IOCTCOMM,IOSEQ      TEST READ SEQUENTIAL                         
         BNE   *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 VDMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,KEY,KEY                        
*                                                                               
         MVC   IOERROR,8(R1)                                                    
         TM    IOERROR,IOEALL-IOEDEL                                            
         BNZ   IOEXECX                                                          
         MVC   IODA,KEY+(ACCKDA-ACCRECD)                                        
         B     IOEXECX                                                          
         SPACE 2                                                                
*                                  I/O CALLS - TRANSLATE TO OLD FILE            
IOEX4    MVC   IOFILE,ACCFIL       SET FILE NAME                                
         MVC   IOCOMM,0(R1)        SET COMMAND NAME                             
         MVC   IOCTINDS,L'IOCOMM(R1)                                            
         OC    IOCOMM,IOCOMM       TEST COMMAND VALID FOR FILE                  
         BZ    IOEXECX             NO COMMAND                                   
         TM    IOCTINDS,IOCTIGET   TEST GETREC                                  
         BZ    IOEX6                                                            
         TM    IOQUAL,IOQLOCK      TEST GETREC FOR UPDATE                       
         BZ    IOEXECX                                                          
         LA    R1,KEY              RE-BUILD KEY FOR READ WITH LOCK              
         USING TRNRECD,R1                                                       
         DROP  R1                                                               
*                                                                               
IOEX6    CLI   IOCTCOMM,IOHIGH                                                  
         BE    *+12                                                             
         CLI   IOCTCOMM,IOSEQ                                                   
         BNE   *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY ON HIGH OR SEQ                      
         TM    IOCTINDS,IOCTIGET   TEST GETREC                                  
         BNZ   IOEX8                                                            
         TM    IOCTFILE,IOACCDA    IF A WRITE COMMAND, TRANSLATE TO OLD         
         BZ    IOEX8               FORMAT RECORD BEFORE DATAMGR CALL            
         GOTO1 VACCEMU,DMCB,=C'NEWO',,,AIOBUFF                                  
         ORG   *-2                                                              
         LR    R2,R1               EMU REQUIRES R2=A(DMCB)                      
         BASR  RE,RF                                                            
*                                                                               
IOEX8    GOTO1 VDMGR,DMCB,(IOQUAL,IOCOMM),IOFILE,KEY,AIOBUFF                    
         MVC   IOERROR,8(R1)                                                    
         TM    IOCTFILE,IOACCDA    IF A READ COMMAND, TRANSLATE TO NEW          
         BZ    *+12                FORMAT RECORD AFTER DATAMGR CALL             
         TM    IOCTINDS,IOCTIGET                                                
         BZ    IOEXECX                                                          
         GOTO1 VACCEMU,DMCB,=C'OLDN',,,AIOBUFF                                  
         ORG   *-2                                                              
         LR    R2,R1                                                            
         BASR  RE,RF                                                            
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF          SET MOS IN KEY AREA FROM TRANS               
         LA    RF,TRNRFST                                                       
         CLI   0(RF),TRNELQ                                                     
         BNE   IOEX10                                                           
         GOTO1 VCONVMOS,DMCB,(X'FE',(RF)),TRNRSMOS                              
*                                                                               
IOEX10   TM    IOCTFILE,IOACCDA                                                 
         BNZ   IOEXECX                                                          
         LA    R1,KEY              BUILD NEW DIR RECORD KEY IN KEY AREA         
         USING ACCRECD,R1                                                       
         MVC   ACCKEY,TRNKEY                                                    
         MVC   ACCKSTA,TRNRSTA                                                  
         XC    ACCKDA,ACCKDA                                                    
         DROP  R1,R2                                                            
*                                                                               
IOEXECX  TM    IOERROR,IOEALL                                                   
         BZ    ROUTE               NO ERROR - CC EQUAL                          
         TM    IOERROR,IOEEOF+IOERNF+IOEDEL                                     
         BNZ   ROUTH               LOGICAL ERROR - CC HIGH                      
         B     ROUTL               HARD ERROR - CC LOW                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OVERLAY LIST OR DETAIL SCREEN                            *         
* ENTRY TWASCROV=SCREEN PHASE NUMBER                                  *         
***********************************************************************         
OVRSCR   MVC   DMCB+4(2),=X'D906'                                               
         MVC   DMCB+6(1),PGNO                                                   
         MVC   DMCB+7(1),TWASCROV                                               
         GOTO1 VCALLOVL,DMCB,(0,WFMOLAYH)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KILL IF CAN'T LOAD SCREEN                    
         LA    R1,WFMMSGH          RE-TRANSMIT ALL HEADER FIELDS                
         SR    RE,RE                                                            
         LA    RF,WFMOLAYH-1                                                    
         ICM   RE,1,0(R1)                                                       
         BZ    *+12                                                             
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BXLE  R1,RE,*-12                                                       
         CLI   TWASCROV,TWASCLST   TEST LIST SCREEN JUST LOADED                 
         BNE   OVRSCRX                                                          
         L     RF,AUTL                                                          
         MVC   TWAUTLSV,TSCRNE-UTLD(RF)  SAVE UTL MAP                           
OVRSCRX  B     ROUTX                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINT FILE                                                    *         
***********************************************************************         
         USING REPD,R3                                                          
PRTCLO   LR    R3,R1                                                            
         MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTO1 VREPORT,REPD                                                     
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         LA    RF,FVSUBT           SET UP XXX,999                               
         MVC   1(3,RF),REPSUBID                                                 
         MVI   4(RF),C','                                                       
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
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   5(0,RF),0(RE)                                                    
         LA    R1,6(R1)                                                         
         STC   R1,0(RF)                                                         
         OC    OPTRDST,OPTRDST     TEST USER SPECIFIED REPT DESTINATION         
         BZ    ROUTE                                                            
         AR    RF,R1                                                            
         MVC   1(7,RF),OPTRUSR     INCLUDE USER-ID IN SUBTEXT                   
         MVI   0(RF),8                                                          
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN/INITIALISE PRINT FILE                                          *         
***********************************************************************         
PRTINI   LR    R3,R1                                                            
         LR    R0,R1               INIT REPORT W/S                              
         LH    R1,=Y(REPL)                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         USING REPD,R3             R3=A(REPORT W/S)                             
         MVC   REPACOM,ACOM                                                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVC   REPABOX,ABOX                                                     
         MVI   REPACTN,REPAINI                                                  
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         MVI   REPWIDTH,REPWREGQ                                                
         MVC   REPDATE,TODAYB      SET ONLINE VALUES                            
         MVC   REPAPQB,APQBUFF                                                  
         MVC   REPSYSID,=C'AC'                                                  
         MVC   REPPRGID,=C'WF'                                                  
         MVC   REPDESC,AC@WFL                                                   
         CLM   R3,8,=C'L'          TEST REPORT NAME IS 'WKFILE LIST'            
         BE    *+10                                                             
         MVC   REPDESC,AC@AWP      ELSE 'WKPOST ANLS'                           
         OC    OPTRDST,OPTRDST     TEST USER SPECIFIED REPORT DEST.             
         BZ    *+10                                                             
         MVC   REPUSRID,OPTRDST                                                 
         MVC   REPSUBID,PRTSUB                                                  
         MVC   REPRLH,=AL2(48)     SET LIVE RETAIN=48 HOURS                     
         MVC   REPRDH,=AL2(12)     SET DEAD RETAIN=12 HOURS                     
         OI    REPIND2,REPILOW     SET LOWER CASE OUTPUT                        
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD        CALL REPORT TO INITIALISE REPD               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SET UP HEADINGS                              
         L     R1,AREPSPEC                                                      
         STCM  R1,15,REPAPHS       SET A(SPEC POOL)                             
         OI    REPHEADI,REPHFRCE   FORCE HEADLINE PRINTING                      
         MVC   REPPAGE,=H'1'                                                    
         MVI   REPACTN,REPAPUT                                                  
*                                  BUILD WORKER FILE ID KEY                     
         MVC   REPH3+9(L'TSARDUNM),TSARDUNM                                     
         LA    RF,REPH3+9+L'TSARDUNM-1                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   2(L'TSARKSPG,RF),TSARKSPG                                        
         LA    RF,L'TSARKSPG+2(RF)                                              
         MVC   0(L'TSARKSUB,RF),TSARKSUB                                        
         CLI   0(RF),0                                                          
         BNE   *+8                                                              
         MVI   0(RF),C'*'          DEFAULT                                      
         LA    RF,L'TSARKSUB(RF)                                                
         UNPK  0(3,RF),TSARKDAY(2)                                              
         AHI   RF,2                                                             
         MVC   0(L'TSARKTYP,RF),TSARKTYP                                        
         AHI   RF,1                                                             
         CLI   TSARKXTR,C' '                                                    
         BNH   PINI10                                                           
         MVC   0(L'TSARKXTR,RF),TSARKXTR                                        
         AHI   RF,1                                                             
PINI10   MVI   0(RF),C','                                                       
         LA    R2,1(RF)                                                         
         CURED (B2,TSARKSEQ),(4,(R2)),0,ALIGN=LEFT                              
         GOTO1 AGETWKN,TSARWKEY    GET FILE NAME                                
         LA    R1,1(R2)                                                         
         AR    R1,R0               R1=A(FILE NAME POSITION IN P)                
         L     RE,FULL             RE=A(FILE NAME)                              
         MVC   0(L'LC@PGA08,R1),0(RE)  MOVE IT IN                               
*                                                                               
         B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ WORKER FILES, BUILD TSAR RECORDS                               *         
***********************************************************************         
READWK   MVI   ANYADD,0                                                         
*                                                                               
         L     R0,AUIDTAB          CLEAR USER-ID TABLE                          
         LH    R1,=Y(UIDTMAX*UIDTABL)                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AWKBUF1          CLEAR WORKER I/O BUFFER1                     
         LH    R1,=Y(WKBUFL)                                                    
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    WKID,WKID           CLEAR WKFILE ID                              
RDWK2    GOTO1 VDMGR,DMCB,WKINDX,WKFILE,WKID,AWKIO,AWKBUF1                      
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BNZ   RDWK46                                                           
         OC    UKBKEY,UKBKEY       TRAP BAD INDEX                               
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ERRWF) 'ERROR ON WORKER FILE - CALL DDS'         
         B     ROUTH                                                            
         CLI   UKCLASS,WKITPOST    TEST POSTING FILE                            
         BNE   RDWK4                                                            
         CLI   KEYCHQS,C'O'        TEST INCLUDING POSTING FILES                 
         BE    RDWK2               NO                                           
         B     RDWK6                                                            
*                                                                               
RDWK4    CLI   UKCLASS,WKITCHQS    TEST CHEQUES FILE                            
         BNE   RDWK2                                                            
         CLI   KEYCHQS,C'N'        TEST EXCLUDING CHEQUES                       
         BE    RDWK2               YES                                          
         CLI   KEYCHQS,0           EXCLUDE BY DEFAULT                           
         BE    RDWK2                                                            
RDWK6    XC    TSARREC(TSARRECL),TSARREC                                        
         GOTO1 AGETWKN,UKBKEY      IDENTIFY FILE/VALIDATE FOR WFM               
         BNE   RDWK2               UNKNOWN/NOT WANTED                           
         L     RE,FULL             PICK UP A(FILE NAME)                         
         MVC   TSARDFNM,0(RE)      AND MOVE IT IN                               
         L     RE,APRGTENT                                                      
         USING PRGTABD,RE                                                       
         TM    PRGTIND2,PRGTUPDT   TEST FILE UPDATEABLE BY NON-DDS              
         BNO   *+8                                                              
         OI    TSARIND1,TSARUPDQ   YES - SET INDICATOR                          
         TM    PRGTIND2,PRGTDELT   TEST FILE DELETEABLE BY NON-DDS              
         BNO   *+8                                                              
         OI    TSARIND1,TSARDELQ   YES - SET INDICATOR                          
         TM    PRGTIND2,PRGTNORV   TEST FILE NON-REVERSABLE                     
         BNO   *+8                                                              
         OI    TSARIND2,TSARNORV   YES - SET INDICATOR                          
         TM    PRGTIND2,PRGTICOP   TEST FILE CAN BE COPIED                      
         BNO   *+8                                                              
         OI    TSARIND2,TSARICOP   YES - SET INDICATOR                          
         TM    PRGTIND2,PRGTXWFM   TEST FILE NOT HANDLED BY WFM                 
         BZ    *+8                 (DATA CONTROL ONLY)                          
         OI    TSARIND2,TSARNOAC   SET 'LOOK, BUT DON'T TOUCH'                  
         TM    PRGTIND1,PRGTICHQ   TEST FILE TYPE IS CHEQUES                    
         BNO   RDWK8                                                            
         CLI   UKCLASS,WKITCHQS    TEST CHEQUES FILE                            
         BNE   *+12                                                             
         OI    TSARIND1,TSARCHXQ   YES - SET CHEQUES INDICATOR                  
         B     *+8                                                              
         OI    TSARIND1,TSARCHPQ   ELSE SET CHEQUE POSTINGS                     
*                                  APPLY KEY FILTERS                            
RDWK8    TM    WKISTAT,WKISHOLD+WKISDELT+WKISKEEP                               
         BZ    *+12                                                             
         CLI   USERSTS,DDSUSRQ     STATUS IS RESTRICTED                         
         BL    RDWK2                                                            
         SR    R1,R1                                                            
         ICM   R1,1,KEYFSTS                                                     
         BZ    RDWK10                                                           
         LA    RF,X'E0'            CC=BNO                                       
         CLI   KEYFSBC,0           TEST INCLUDE/EXCLUDE                         
         BE    *+8                                                              
         LA    RF,X'10'            CC=BO                                        
         EX    R1,*+8                                                           
         EX    RF,*+8                                                           
         TM    WKISTAT,0                                                        
         NOP   RDWK2                                                            
*                                                                               
RDWK10   OC    KEYUSER,KEYUSER     USER-ID                                      
         BZ    RDWK12                                                           
         CLC   WKIUSER,KEYUSER                                                  
         BNE   RDWK2                                                            
         B     RDWK16                                                           
*                                                                               
RDWK12   CLI   USERSTS,DDSUSRQ     IF NOT DDS                                   
         BNL   RDWK16                                                           
         LA    RE,GUIDTAB          CHECK GROUP-ID LIST                          
RDWK14   OC    0(L'GUID,RE),0(RE)                                               
         BZ    RDWK2               NO LIST/END OF LIST                          
         CLC   0(L'GUID,RE),WKIUSER  TEST THIS ID IS IN LIST                    
         BE    RDWK16                                                           
         BH    RDWK2               NO                                           
         LA    RE,L'GUID(RE)                                                    
         B     RDWK14                                                           
*                                                                               
RDWK16   GOTO1 AGETUID             TEST UID/SENO                                
         BNE   RDWK2               NO GOOD                                      
*                                                                               
RDWK18   OC    KEYSYS,KEYSYS       SYSTEM                                       
         BZ    RDWK20                                                           
         CLC   WKISYS,KEYSYS                                                    
         BNE   RDWK2                                                            
*                                                                               
RDWK20   CLI   KEYPRG,0            PROGRAM                                      
         BE    *+14                                                             
         CLC   WKIPRG(1),KEYPRG                                                 
         BNE   RDWK2                                                            
         CLI   KEYPRG+1,0                                                       
         BE    *+14                                                             
         CLC   WKIPRG+1(1),KEYPRG+1                                             
         BNE   RDWK2                                                            
*                                                                               
RDWK22   OC    KEYSUB,KEYSUB       SUB-PROGRAM                                  
         BZ    RDWK24                                                           
         CLC   WKISUB,KEYSUB                                                    
         BNE   RDWK2                                                            
*                                                                               
RDWK24   OC    KEYDAY,KEYDAY       DAY ADDED                                    
         BZ    RDWK26                                                           
         CLC   WKIDAY,KEYDAY                                                    
         BNE   RDWK2                                                            
*                                                                               
RDWK26   OC    KEYTYPE,KEYTYPE     TYPE                                         
         BZ    RDWK27                                                           
         CLC   UKCLASS,KEYTYPE                                                  
         BNE   RDWK2                                                            
*                                                                               
RDWK27   OC    KEYEXTRA,KEYEXTRA   EXTRA                                        
         BZ    RDWK28                                                           
         CLC   UKEXTRA,KEYEXTRA                                                 
         BNE   RDWK2                                                            
*                                                                               
RDWK28   OC    KEYSSEQ(L'KEYSSEQ+L'KEYESEQ),KEYSSEQ  SEQUENCE NO. RANGE         
         BZ    RDWK30                                                           
         CLC   UKFILNO,KEYSSEQ                                                  
         BL    RDWK2                                                            
         CLC   UKFILNO,KEYESEQ                                                  
         BH    RDWK2                                                            
*                                                                               
*                                  READ DATA REC FOLLOWING INDEX                
RDWK30   L     R0,AWKBUF2          COPY WKBUF1 TO WKBUF2                        
         LH    R1,=Y(WKBUFL)                                                    
         L     RE,AWKBUF1                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R3,AWKIO                                                         
         XC    0(4,R3),0(R3)       CLEAR START OF WKIO                          
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF2                      
         BE    *+6                                                              
         DC    H'0'                BAD WORKER FILE                              
         USING UKSOFD,R3                                                        
         OC    KEYSDAT(KEYDATSL),KEYSDAT  DATE RANGE FILTER                     
         BZ    RDWK32                                                           
         CLC   UKSOFAGD,KEYSDAT                                                 
         BL    RDWK2                                                            
         CLC   UKSOFAGD,KEYEDAT                                                 
         BH    RDWK2                                                            
*                                  BUILD TSAR KEY AND PART DATA                 
RDWK32   LH    R1,STSARKNO         BUMP AND SAVE KEY SEQUENCE NO.               
         LA    R1,1(R1)                                                         
         STH   R1,STSARKNO                                                      
*        STCM  R1,3,TSARKNO                                                     
         MVC   TSARWKEY,UKBKEY                                                  
         MVC   TSARKSEQ,UKFILNO                                                 
         MVC   TSARDSTS,UKSTAT                                                  
         MVC   TSARDDAT,UKSOFAGD                                                
         MVC   TSARDTIM,UKSOFAGT                                                
         ICM   R1,15,UKSOFRCT      USE REC0 RECORD COUNT INITIALLY              
         BCTR  R1,0                                                             
         CVD   R1,DUB                                                           
         ZAP   TSARDCNT,DUB        IN CASE TRAILER RECORD NOT FOUND             
         CLC   =C'R ',UKSOFCOM     TEST REVERSED IN WFM                         
         BNE   *+8                                                              
         OI    TSARIND1,TSWKREVQ   SET INDICATOR                                
         DROP  R3                                                               
*                                                                               
         USING UIDTABD,RE                                                       
RDWK34   L     RE,AUIDNTRY                                                      
         MVC   TSARDUNM,UIDTCODE   FIRST 8 CHARS OF UID HELD                    
         MVC   TSARSENO,UIDTSENO                                                
         DROP  RE                                                               
         ZAP   TSARDCSH,PZERO                                                   
         OI    TSARIND2,TSARNCTQ   SET NO CONTROL TOTAL YET                     
         TM    TSARIND2,TSARNOAC   TEST SPECIAL 'NO ACTIONS' FILE               
         BZ    RDWK44                                                           
         LA    R2,SPWKFT           YES - TRY TO GET TOTAL FOR FILE              
RDWK36   CLI   0(R2),0             EOT                                          
         BE    RDWK44                                                           
         CLC   WKIPRG,0(R2)        MATCH ON TABLE ENTRY                         
         BE    *+12                                                             
         LA    R2,SPWKFTL(R2)                                                   
         B     RDWK36                                                           
         CLI   2(R2),PSSBELQ       TEST WANT SUBFILE TRAILER ELEMENT            
         BE    RDWK42                                                           
         CLI   2(R2),PSLTELQ       TEST WANT LEDGER TOTAL ELEMENT               
         BNE   RDWK44                                                           
         ZAP   WKFILDR,PZERO                                                    
         ZAP   WKFILCR,PZERO                                                    
*                                  READ ALL WORKER RECORDS                      
RDWK38   GOTO1 VDMGR,DMCB,WKREAD,WKFILE,WKID,AWKIO,AWKBUF2                      
         BNE   RDWK40                                                           
         L     RF,AWKIO                                                         
         LA    RF,4(RF)                                                         
         USING PSLTOTD,RF                                                       
         CLC   PSLTEL,2(R2)        ENSURE CORRECT ELEMENT                       
         BNE   RDWK38                                                           
         AP    WKFILDR,PSLTDR      ACCUMULATE TOTALS FOR FILE                   
         AP    WKFILCR,PSLTCR                                                   
         B     RDWK38                                                           
RDWK40   ZAP   TSARDCSH,WKFILDR                                                 
         BNZ   *+10                                                             
         ZAP   TSARDCSH,WKFILCR                                                 
         NI    TSARIND2,255-TSARNCTQ                                            
         B     RDWK44                                                           
*                                                                               
                                                                                
         USING UKSOFD,RE                                                        
RDWK42   L     RF,AWKIO                                                         
*        MVC   0(L'WKRECS,RF),WKRECS  READ FOR TRAILER RECORD                   
         MVC   0(L'UKSOFRCT,RF),UKSOFRCT  READ FOR TRAILER RECORD               
         GOTO1 VDMGR,DMCB,WKRAND,WKFILE,WKID,AWKIO,AWKBUF2                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
         L     RF,AWKIO                                                         
         LA    RF,4(RF)                                                         
         USING PSSUBFD,RF                                                       
         CLC   PSSBEL,2(R2)        TEST CORRECT ELEMENT                         
         BNE   RDWK44                                                           
         ZAP   TSARDCSH,PSSBCASH                                                
         NI    TSARIND2,255-TSARNCTQ                                            
         DROP  RF                                                               
RDWK44   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BH    ROUTX               TSAR ERROR                                   
         BL    RDWK46              TSAR OVERFLOW                                
         MVI   ANYADD,1            SET TSAR RECORD ADDED                        
         B     RDWK2                                                            
*                                                                               
RDWK46   CLI   ANYADD,1            TEST ANYTHING ADDED                          
         BNE   *+12                                                             
         NI    KEYIND,255-KEYCHNG  RESET ONLY IF READ WAS SUCCESSFUL            
         B     ROUTE                                                            
         MVC   FVMSGNO,=AL2(AI$NOWFS)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     ROUTH                                                            
                                                                                
**********************************************************************          
*        TABLE OF WORKER FILES WITH NON-STANDARD ELEMENT TYPES                  
**********************************************************************          
SPWKFT   DC    C'09',AL1(PSSBELQ)                                               
SPWKFTL  EQU   *-SPWKFT                                                         
         DC    C'98',AL1(PSSBELQ)                                               
         DC    C'DK',AL1(PSLTELQ)                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SCAN EITHER KEY OR OPTIONS FIELD                         *         
* ON ENTRY FVADDR=A(FIELD)                                            *         
*              R2=A(KEY/OPT INPUT TAB)                                *         
***********************************************************************         
SCANFLD  XC    SCIFLTS(SCIFLTSL),SCIFLTS                                        
         L     RF,FVADDR                                                        
         CLI   FVILEN-FVIHDR(RF),0 TEST ANY INPUT                               
         BE    ROUTL               EXIT                                         
         ST    R2,ASCANTAB         SAVE A(VALIDATION TABLE)                     
         LA    R0,SCANOUT          CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,FVADDR),('SCANMAXN',SCANOUT), >        
               C',=  '                                                          
         MVC   BYTE,4(R1)                                                       
         CLI   BYTE,0                                                           
         BE    RHERRNV             INVALID INPUT                                
         MVI   FVINDX,1                                                         
         LA    R1,SCANOUT                                                       
         USING SCANOUTD,R1                                                      
SCANFL2  MVI   FLAG,0                                                           
         CLI   SCANTXT1,C'*'       TEST 'NOT' PREFIX ON KEYWORD                 
         BNE   SCANFL4                                                          
         CLI   SCANLEN1,2                                                       
         BL    SCANFL4                                                          
         MVC   SCANTXT1(L'SCANTXT1-1),SCANTXT1+1                                
         IC    RF,SCANLEN1                                                      
         BCTR  RF,0                                                             
         STC   RF,SCANLEN1                                                      
         MVI   FLAG,1              SET 'NOT' INPUT                              
         USING INPTABD,R2                                                       
SCANFL4  L     R2,ASCANTAB                                                      
         SR    RF,RF                                                            
         ICM   RF,1,SCANLEN1                                                    
         BZ    RHERRNV             INVALID INPUT - NO LHS?                      
         CLM   RF,1,=AL1(INPKLNG)                                               
         BH    RHERRLG             OPTION KEYWORD TOO LONG                      
SCANFL6  CLM   RF,1,=AL1(INPKSHT)                                               
         BH    SCANFL8                                                          
         MVC   LAREADDR,INPKWDS    TRY SHORT KEYWORD                            
         B     *+10                                                             
SCANFL8  MVC   LAREADDR,INPKWDL    FULL KEYWORD                                 
         EX    0,LARE                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCANTXT1(0),0(RE)                                                
         BE    SCANFL12                                                         
         CLC   LAREADDR,INPKWDL    HAVE WE TRIED FULL KEYWORD YET               
         BE    SCANFL10                                                         
         IC    RF,SCANLEN1                                                      
         B     SCANFL8             NO - TRY IT                                  
*                                                                               
SCANFL10 LA    R2,INPTABL(R2)                                                   
         IC    RF,SCANLEN1         REFRESH LENGTH                               
         CLI   0(R2),EOT           AND IF THERE'S ANOTHER ENTRY                 
         BNE   SCANFL6             GO BACK TO TRY IT                            
         B     RHERRNV             NO MATCH - ERROR                             
*                                                                               
SCANFL12 SR    RF,RF               TEST RESTRICTED KEYWORD                      
         ICM   RF,1,INPSLVL                                                     
         BZ    SCANFL13            UNRESTRICTED                                 
         EX    RF,*+8                                                           
         BNO   SCANFL10            INSUFFICIENT STATUS                          
         TM    USERSTS,0                                                        
SCANFL13 CLI   INPSCR,0            TEST KEYWORD OK FOR SCREEN                   
         BE    SCANFL14            ZERO=EITHER                                  
         CLC   INPSCR,TWASCROV                                                  
         BE    SCANFL14                                                         
         BL    SCANFL10                                                         
         CLI   TWASCROV,0          IF INITIAL SCREEN                            
         BNE   SCANFL10                                                         
         CLI   INPSCR,TWASCLST     ALLOW LIST SCREEN KEYWORDS                   
         BNE   SCANFL10            BEFORE LIST SCREEN IS LOADED                 
SCANFL14 TM    INPIND,INP2PTQ                                                   
         BO    SCANFL16                                                         
         CLI   SCANLEN2,0          ONE-PART MUST NOT HAVE 2ND PART              
         BNE   SCANFL10                                                         
         CLI   FLAG,1              TEST 'NOT' INPUT                             
         BNE   *+12                                                             
         TM    INPIND,INPNOTQ      TEST 'NOT' IS ALLOWED                        
         BZ    SCANFL10                                                         
         MVC   LARFADDR,INPADDR                                                 
         EX    0,LARF                                                           
         SR    RE,RE                                                            
         IC    RE,INPSCIV                                                       
         EX    RE,*+8                                                           
         BNZ   RHERRID             DUPLICATE KEYWORD                            
         TM    0(RF),0             TEST KEYWORD ALREADY INPUT                   
         EX    RE,*+8                                                           
         B     SCANFL20                                                         
         OI    0(RF),0             SET INDICATOR BIT                            
*                                                                               
SCANFL16 CLI   SCANLEN2,0          TWO-PART REQUIRE 2ND PART                    
         BE    SCANFL10                                                         
         MVI   FLAG,0                                                           
         CLI   SCANTXT2,C'*'       TEST 'NOT' PREFIX ON 2ND PART                
         BNE   SCANFL18                                                         
         TM    INPIND,INPNOTQ      TEST 'NOT' ALLOWED                           
         BNO   SCANFL18            NO - TREAT AS A WILD CARD                    
         CLI   SCANLEN2,2                                                       
         BL    RHERRNV             'NOT' ALONE IS INVALID                       
         MVC   SCANTXT2(L'SCANTXT2-1),SCANTXT2+1                                
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         STC   RF,SCANLEN2         DECREMENT APPARENT LENGTH                    
         MVI   FLAG,1              SET 'NOT' FLAG                               
SCANFL18 CLC   SCANLEN2,INPRHMX    CHECK MAXIMUM LENGTH ALLOWED                 
         BH    SCANFL10            TOO LONG, TRY AGAIN                          
         ICM   R1,8,FLAG           SET INCLUDE/EXCLUDE                          
         MVC   LARFADDR,INPADDR                                                 
         EX    0,LARF                                                           
         TM    INPIND,INPNVLQ      TEST FREE-FORM RHS                           
         BZ    SCANFL19                                                         
         SR    RE,RE                                                            
         IC    RE,SCANLEN2                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SCANTXT2                                                 
         SR    R0,R0                                                            
         IC    R0,INPRHMX                                                       
         AR    RF,R0                                                            
         MVC   0(1,RF),SCANLEN2    SAVE L'INPUT FOR COMPARE                     
         B     SCANFL20                                                         
*                                                                               
SCANFL19 L     RF,0(RF)                                                         
         BASR  RE,RF               BRANCH TO RHS VALIDATION SUBROUTINE          
         BNE   ROUTX               EXIT WITH ERROR MSG SET BY SUBR              
*                                                                               
SCANFL20 IC    RF,FVINDX           BUMP TO NEXT SCANNER ENTRY                   
         LA    RF,1(RF)                                                         
         CLM   RF,1,BYTE                                                        
         BNH   *+12                                                             
         MVI   FVINDX,0                                                         
         B     ROUTE                                                            
         STC   RF,FVINDX                                                        
         LA    R1,SCANOUTL(R1)     TAKE NEXT KEYWORD                            
         B     SCANFL2                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT A RECORD TO TSAR                                     *         
***********************************************************************         
TSARADD  L     R2,ATSARBLK                                                      
         USING TSARD,R2                                                         
         MVI   TSACTN,TSAADD                                                    
         MVI   TSERRS,0                                                         
*                                                                               
         GOTO1 VTSAR,TSARD         ADD RECORD TO TSAR                           
         BNE   TSARAD2                                                          
         LA    R0,TSPEXPN          SET HIGHEST RECORD NUMBER                    
         SR    RE,RE                                                            
         ICM   RE,3,TSPLTAB                                                     
         BNZ   *+8                                                              
         LA    R0,1                SET NUMBER OF ENTRIES TO ONE                 
         L     R1,ATSARBLK                                                      
         LA    R1,TSPTAB-TSARD(R1)                                              
         USING TSPTAB,R1                                                        
         SR    RF,RF                                                            
         AH    RF,TSPRECN          TAKE NUMBER OF RECORDS                       
         AR    R1,RE                                                            
         BCT   R0,*-6              FOR EACH TEMPSTR PAGE                        
         STH   RF,DISTMAX                                                       
         B     TSARADDX                                                         
         DROP  R1                                                               
*                                                                               
TSARAD2  TM    TSERRS,TSEDUP       ERROR PROCEDURE                              
         BNO   TSARAD4                                                          
         MVC   FVMSGNO,=AL2(EGRECAOF)                                           
         NI    TWAMODE,TWAMRSRV                                                 
         B     TSARADDX                                                         
*                                                                               
TSARAD4  TM    TSERRS,TSEEOF       TEST END OF FILE                             
         BO    *+6                                                              
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         MVC   FVMSGNO,=AL2(AE$TMTRN)                                           
         OI    DISIND,DISIOFLO     SET OVERFLOW OCCURRED                        
         B     ROUTL               SET CC LOW AND EXIT                          
*                                                                               
TSARADDX CLI   TSERRS,0            SET CONDITION CODE NEQ ON ERROR              
         B     ROUTX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A RECORD FROM TSAR                                   *         
* NTRY - R1=A(RECORD NUMBER)                                          *         
***********************************************************************         
TSARGET  L     R2,ATSARBLK                                                      
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
         MVI   TSACTN,TSAGET                                                    
         MVC   TSRNUM,0(R1)                                                     
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            SET CC FOR CALLER                            
         B     ROUTX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALISE/RESTORE TSAR                                  *         
***********************************************************************         
TSARINI  L     R1,ATSARBLK                                                      
         USING TSARD,R1            R1=A(TSAR BLOCK)                             
         MVC   TSABUF,ATIA         SET A(BUFFER)                                
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC           SET A(RECORD)                                
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,TSARKEYL     SET KEY LENGTH                               
         MVC   TSRECL,=Y(TSARRECL) SET REC LENGTH                               
         MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         MVI   TSNBUF,2            SET NUMBER OF CORE BUFFERS                   
         MVI   TSPAGN,TSPAGNCI+4   SET NUMBER OF TEMPSTR C/I'S                  
         TM    TWAMODE,TWAMRSRV    TEST TEMPEST PREVIOUSLY RESERVED             
         BZ    TSARIN2                                                          
*                                                                               
         MVC   TSINDS,TWATSARI     SET INDICATORS                               
         OI    TSINDS,TSIREUSE     SET TO RE-USE PREVIOUS ALLOCATION            
         MVC   TSPAGL,TWALOWPG     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TWANUMPG     SET NUMBER OF PAGES ALLOCATED                
*                                                                               
TSARIN2  MVI   TSACTN,TSAINI       SET INITIALISE                               
         TM    TWAMODE,TWAMINIT                                                 
         BZ    *+8                                                              
         MVI   TSACTN,TSARES       SET RESTORE                                  
         GOTO1 VTSAR               CALL TO INITIALISE/RESTORE                   
         BE    TSARIN4             SUCCESSFUL                                   
         TM    TWAMODE,TWAMRSRV    ELSE TEST FIRST TIME ALLOCATION              
         BZ    *+6                 YES                                          
         DC    H'0'                NO - CAN'T RESTORE INIT'D TEMPEST            
         NI    TSINDS,255-TSIALLOC RESET TEMPEST ALLOCN, TRY TEMPSTR            
         MVI   TSPAGL,2            SET TO USE TEMPSTR PAGES 2-4                 
         MVI   TSPAGN,3                                                         
         BASR  RE,RF               INITIALISE TEMPSTR PAGES                     
         BE    TSARIN4                                                          
         DC    H'0'                KILL IF CAN'T INITIALISE TEMPSTR             
*                                                                               
TSARIN4  MVC   TWALOWPG,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   TWANUMPG,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         MVC   TWATSARI,TSINDS     SAVE TEMPSTR/TEMPEST INDICATOR               
         NI    TWATSARI,TSIALLOC                                                
         OI    TWAMODE,TWAMINIT+TWAMRSRV                                        
         XC    STSARKNO,STSARKNO   RE/SET TSAR KEY COUNT                        
         B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT/CONTRA ACCOUNT                                     *         
* ENTRY R2=A(OPTAB ENTRY)                                             *         
*       R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
VALACC   LA    RE,SCIACC           TEST ACCOUNT OR CONTRA                       
         CLC   INPKWDL-INPTABD(L'INPKWDL,R2),=S(AC8ACC)                         
         BE    *+8                                                              
         LA    RE,SCICAC                                                        
         USING SCANOUTD,R2                                                      
         LR    R2,R1                                                            
         OC    0(L'SCIACC,RE),0(RE)                                             
         BNZ   RHERRID             DUPLICATED                                   
         CLI   SCANLEN2,L'TRNKCULA-1                                            
         BH    RHERRNV             INVALID LENGTH                               
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         STC   RF,L'SCIACC(RE)     SAVE REAL LENGTH                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SCANTXT2                                                 
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE 'AMOUNT=' (NUMERIC) OR 'DR'/CR'                            *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALAMT   LR    R2,R1                                                            
         OC    SCIAMT,SCIAMT                                                    
         BNZ   RHERRID             DUPLICATED                                   
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         MVI   SCISIGN,C'D'                                                     
         EX    RF,*+8                                                           
         BE    ROUTE                                                            
         CLC   SCANTXT2(0),AC2DR                                                
         MVI   SCISIGN,C'C'                                                     
         EX    RF,*+8                                                           
         BE    ROUTE                                                            
         CLC   SCANTXT2(0),AC2CR                                                
         LA    RF,1(RF)                                                         
         GOTO1 VCASHVAL,DMCB,SCANTXT2,(RF)                                      
         CLI   0(R1),0                                                          
         BNE   ROUTH                                                            
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         OC    DUB(2),DUB          ENSURE WILL PACK INTO 6 BYTES                
         BNZ   RHERRNV                                                          
         ZAP   SCIAMT,DUB                                                       
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE 'CHEQUES=Y'                                                *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALCHQ   LR    R2,R1                                                            
         CLI   SCICHQS,0                                                        
         BNE   RHERRID             DUPLICATED                                   
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         MVI   SCICHQS,C'Y'                                                     
         EX    RF,*+8                                                           
         BE    ROUTE                                                            
         CLC   SCANTXT2(0),AC@YES                                               
         MVI   SCICHQS,C'N'                                                     
         EX    RF,*+8                                                           
         BE    ROUTE                                                            
         CLC   SCANTXT2(0),AC@NO                                                
         MVI   SCICHQS,C'O'                                                     
         EX    RF,*+8                                                           
         BNE   RHERRNV                                                          
         CLC   SCANTXT2(0),AC@ONLY                                              
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT DESTINATION ID                                      *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALDST   LR    R2,R1                                                            
         OC    SCIRDST,SCIRDST                                                  
         BNZ   RHERRID             DUPLICATED                                   
         L     R3,AIOAS                                                         
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,USER        READ USER'S ID RECORD                        
         CLI   USERSTS,DDSUSRQ     UNLESS USER IS DDS                           
         BL    *+10                                                             
         MVC   CTIKID,SCANTXT2     READ SPECIFIED ID RECORD                     
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                           
         BNE   RHERRUI             ID RECORD NOT FOUND                          
         CLI   USERSTS,DDSUSRQ                                                  
         BL    VALDS6                                                           
*                                                                               
         LA    R1,CTIDATA          LOCATE DESCRIPTION ELEMENT                   
         SR    R0,R0                                                            
         USING CTDSCD,R1                                                        
VALDS2   CLI   CTDSCEL,0                                                        
         BE    RHERRUI             USER-ID INVALID (NOT FOUND)                  
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    VALDS4                                                           
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         B     VALDS2                                                           
VALDS4   MVC   SCIRDST,CTDSC       EXTRACT USER-ID NUMBER                       
         MVC   OPTRUSR,SCANTXT2    AND SAVE NAME                                
         B     ROUTE                                                            
         DROP  R3                                                               
*                                  GET VALID ID LIST FOR NON-DDS USER           
VALDS6   GOTO1 VGETIDS,DMCB,(C'D',(R3)),0,VDMGR                                 
         SR    R0,R0                                                            
         ICM   R0,1,DMCB           R0=N'COMPATIBLE IDS                          
         BZ    RHERRNV             NONE FOUND                                   
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                 DISK ERROR                                   
         DC    H'0'                                                             
         L     RF,DMCB+4           RF=A(COMPATIBLE IDS)                         
*                                  SEARCH TABLE OF VALID IDS                    
VALDS8   CLI   0(RF),X'FF'         TEST EOT                                     
         BE    RHERRNV                                                          
         CLC   0(L'UIDTCODE,RF),SCANTXT2                                        
         BE    VALDS10                                                          
         LA    RF,12(RF)                                                        
         BCT   R0,VALDS8                                                        
         B     RHERRNV                                                          
*                                                                               
VALDS10  MVC   SCIRDST,10(RF)      SET DESTINATION ID NUM                       
         MVC   OPTRUSR,SCANTXT2    AND SAVE NAME                                
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD FILTERS                                             *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALPER   LR    R2,R1                                                            
         OC    SCISDAT,SCIEDAT     DATE RANGE                                   
         BNZ   RHERRID             DUPLICATED                                   
         MVC   SCIEDAT,EFFS        PRESET END DATE                              
         GOTO1 VPERVAL,DMCB,(SCANLEN2,SCANTXT2),(AGYLANG,WORK)                  
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   RHERRNV             INPUT INVALID                                
         LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
         TM    PVALASSM,STARTASS   TEST START ENTIRELY ASSUMED                  
         BO    VALPER2             YES - INPUT WAS -DDMMYY                      
         MVC   SCISDAT,PVALPSTA    TAKE START DATE                              
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         LA    RF,SCANTXT2-1(RF)                                                
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT                         
         BE    ROUTE               INPUT WAS DDMMYY-                            
VALPER2  MVC   SCIEDAT,PVALPEND    TAKE END DATE                                
         B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKER FILE STATUS                                         *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALSTS   LR    R2,R1                                                            
         CLI   SCIFSTS,0                                                        
         BNE   RHERRID             DUPLICATED                                   
         STCM  R1,8,SCIFSBC        SET +/- TEST                                 
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   VALS2                                                            
         CLC   SCANTXT2(0),AC@ACTV                                              
         MVI   SCIFSTS,WKISACTV                                                 
         B     ROUTE                                                            
*                                                                               
VALS2    EX    RF,*+8                                                           
         BNE   VALS4                                                            
         CLC   SCANTXT2(0),AC@KEEP                                              
         MVI   SCIFSTS,WKISKEEP                                                 
         B     ROUTE                                                            
*                                                                               
VALS4    EX    RF,*+8                                                           
         BNE   VALS6                                                            
         CLC   SCANTXT2(0),AC@DELT                                              
         MVI   SCIFSTS,WKISDELT                                                 
         B     ROUTE                                                            
*                                                                               
VALS6    EX    RF,*+8                                                           
         BNE   RHERRNV             INVALID INPUT                                
         CLC   SCANTXT2(0),AC@HOLD                                              
         MVI   SCIFSTS,WKISHOLD                                                 
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT ACCOUNT SYSTEM NAME AND FIND SENO                    *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALSYS   LR    R2,R1                                                            
         L     R1,VSEXLST          R1=V(SELIST)                                 
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         SR    R3,R3                                                            
         IC    R3,SCANLEN2                                                      
         BCTR  R3,0                                                             
*                                  BUMP DOWN SELIST ENTRIES                     
         USING SELISTD,R1                                                       
VALSYS2  CLI   SEOVSYS,6           TEST ACCOUNT SYSTEM ENTRY                    
         BNE   VALSYS4                                                          
         EX    R3,*+8                                                           
         BE    VALSYS6                                                          
         CLC   SENAME(0),SCANTXT2  TEST SENAME MATCHES INPUT NAME               
VALSYS4  BXLE  R1,RE,VALSYS2                                                    
         B     RHERRNV             NO MATCH - INVALID                           
VALSYS6  MVC   SCISENO,SESYS       SAVE SENO                                    
         B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO VALIDATE INPUT USER-ID FOR WORKER KEY FILTERING, AND    *         
* BUILD AN ID-LIST IF A GROUP ID.                                     *         
* GROUP-IDS ARE NOT APPLICABLE (IN WFM) TO DDSUSRQ/IAMGODQ USERS      *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
***********************************************************************         
* VALIDATE 'ID=' EXPRESSION (DDSUSRQ/IAMGODQ ONLY)                    *         
***********************************************************************         
VALID    LR    R2,R1                                                            
         MVC   SCIUSER,EFFS        PRESET 'ALL'                                 
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    VALIDX                                                           
         CLC   SCANTXT2(0),AC@ALL  TEST 'ALL' SPECIFIED                         
         XC    SCIUSER,SCIUSER                                                  
         BAS   RE,READID           READ ID REC FOR SPECIFIED ID                 
         BNE   RHERRUI                                                          
         MVC   SCIUSER,SPUID       SET SPECIFIED UID                            
         B     VALIDX                                                           
*                                                                               
VALIDX   B     ROUTE                                                            
***********************************************************************         
* VALIDATE 'USERID=' EXPRESSION                                       *         
***********************************************************************         
VALUID   LR    R2,R1                                                            
         BAS   RE,READID           READ ID REC FOR SPECIFIED ID                 
         BNE   RHERRUI                                                          
         CLC   TWAAGY,SPALF        TEST CORRECT AGY ALPHA                       
         BNE   RHERRUI                                                          
         CLI   USERSTS,PUSRIDQ     TEST PRINCIPAL USER                          
         BNE   VALUID2             NO - ID MUST BE IN CONNECTED GROUP           
         B     VALUID6                                                          
*                                                                               
VALUID2  MVC   WORK(L'CTWKID),UIDNAME                                           
         BAS   RE,GETGID           GET ID LIST FOR CONNECTED GROUP              
         BNE   RHERRUI                                                          
         LA    RE,GUIDTAB          CHECK SPECIFIED ID IS IN LIST                
VALUID4  OC    0(L'GUID,RE),0(RE)                                               
         BZ    RDWK2               NO LIST/END OF LIST                          
         CLC   0(L'GUID,RE),WKIUSER                                             
         BE    VALUID6                                                          
         BH    RHERRUI             NO                                           
         LA    RE,L'GUID(RE)                                                    
         B     VALUID4                                                          
*                                                                               
VALUID6  MVC   SCIUSER,SPUID       SET SPECIFIED ID                             
         B     VALUIDX                                                          
*                                                                               
VALUIDX  B     ROUTE                                                            
***********************************************************************         
* VALIDATE 'GROUPID=' EXPRESSION                                      *         
***********************************************************************         
VALGID   LR    R2,R1                                                            
         MVC   SCIUSER,EFFS                                                     
         MVC   WORK,SCANTXT2                                                    
         BAS   RE,GETGID           BUILD ID LIST FOR SPECIFIED GROUP            
         BNE   RHERRUI             NOT A GROUP                                  
         B     VALGIDX                                                          
*                                                                               
VALGIDX  B     ROUTE                                                            
***********************************************************************         
* VALIDATE 'ALLIDS=' EXPRESSION                                       *         
***********************************************************************         
VALAID   LR    R2,R1                                                            
         MVC   SCIUSER,EFFS                                                     
         SR    RF,RF                                                            
         IC    RF,SCANLEN2                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   RHERRUI             ONLY 'YES' IS VALID                          
         CLC   SCANTXT2(0),AC@YES                                               
         MVC   WORK,UIDNAME        'ALLIDS' = ALL IN CONNECTED GROUP            
         BAS   RE,GETGID                                                        
         BNE   RHERRUI             CONNECTED ID IS NOT A GROUP                  
         B     VALAIDX                                                          
*                                                                               
VALAIDX  B     ROUTE                                                            
***********************************************************************         
* SUBSIDIARY USER-ID VALIDATION ROUTINES                              *         
***********************************************************************         
*                                                                               
***********************************************************************         
* READ ID RECORD FOR INPUT UID VALIDATION                             *         
***********************************************************************         
READID   ST    RE,FULL             SAVE A(RETURN)                               
         L     R3,AIOAS            READ CONTROL RECORD FOR REQUESTED ID         
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SCANTXT2                                                  
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY                           
         BNE   READIDX             ID RECORD NOT FOUND                          
*                                                                               
         SR    R0,R0                                                            
         LA    R1,CTIDATA                                                       
         USING CTAGYD,R1                                                        
READID2  CLI   CTAGYEL,0                                                        
         BE    READIDEQ                                                         
         CLI   USERSTS,DDSUSRQ                                                  
         BE    READID4                                                          
         CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   READID4                                                          
         MVC   SPALF,CTAGYID                                                    
READID4  OC    SCIUSER,SCIUSER     TEST USER-ID FOUND                           
         BNZ   READIDEQ            YES - DONE                                   
         CLI   CTAGYEL,CTDSCELQ    TEST DESCRIPTION EL                          
         BNE   *+10                                                             
         USING CTDSCD,R1                                                        
         MVC   SPUID,CTDSC         EXTRACT NUMBER OF SPECIFIED ID               
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         B     READID2                                                          
         DROP  R1                                                               
*                                                                               
READIDEQ CR    RB,RB                                                            
*                                                                               
READIDX  L     RE,FULL                                                          
         BR    RE                                                               
***********************************************************************         
* READ GROUP-ID REC/BUILD ID LIST FOR INPUT UID VALIDATION            *         
***********************************************************************         
GETGID   ST    RE,FULL             SAVE A(RETURN)                               
         L     R3,AIOAS            R3=A(IOA1)                                   
         USING CTWREC,R3                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVC   CTWKAGY,TWAAGY                                                   
         MVI   CTWKREC,CTWKRIDG                                                 
         MVC   CTWKID,WORK         USER-ID SET BY CALLER                        
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,CTWKEY,CTWKEY                           
         BNE   GETGIDX             NOT FOUND                                    
*                                                                               
         LA    R3,CTWDATA                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    RE,GUIDTAB          BUILD TABLE OF IDS IN GROUP                  
         USING CTLSTD,R3                                                        
GETGID1  CLI   CTLSTEL,0                                                        
         BE    GETGID4                                                          
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   GETGID2                                                          
         MVC   0(L'GUID,RE),CTLSTDTA+L'CTIKID   END OF LIST ELEMENT             
         LA    RE,L'GUID(RE)                                                    
         LA    R1,1(R1)                                                         
GETGID2  IC    R0,CTLSTLEN                                                      
         AR    R3,R0                                                            
         B     GETGID1                                                          
         DROP  R3                                                               
*                                                                               
GETGID4  BCTR  R1,0                SORT TABLE INTO ASCENDING SEQUENCE           
         LTR   R1,R1                                                            
         BM    RHERRUI             NO IDS IN LIST                               
         BZ    GETGIDX             1 ID - NO SORT                               
         STH   R1,HALF             SAVE COUNT                                   
GETGID6  LA    RE,GUIDTAB                                                       
         SR    RF,RF                                                            
GETGID8  CLC   0(L'GUID,RE),2(RE)                                               
         BNH   *+24                                                             
         XC    0(L'GUID,RE),2(RE)                                               
         XC    2(L'GUID,RE),0(RE)                                               
         XC    0(L'GUID,RE),2(RE)                                               
         LR    RF,R1               SET SWAPPED                                  
         LA    RE,2(RE)                                                         
         BCT   R1,GETGID8                                                       
         LTR   RF,RF               TEST ANYTHING SWAPPED                        
         BZ    GETGIDX             FINISHED                                     
         LH    R1,HALF             RESTORE COUNT                                
         B     GETGID6                                                          
*                                                                               
GETGIDX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKER FILE KEY                                            *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALWKY   LR    R2,R1                                                            
         OC    SCIKEY(SCIKEYL),SCIKEY                                           
         BNZ   RHERRID             DUPLICATED                                   
         MVC   SCIESEQ,EFFS        PRESET END SEQUENCE NO                       
         LA    R1,SCANTXT2                                                      
         LR    RF,R1                                                            
         SR    RE,RE                                                            
         IC    RE,SCANLEN2                                                      
         AR    RF,RE               RF=A(LAST INPUT BYTE+1)                      
         LA    RE,1                                                             
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),0             SET '*' TO NULL                              
         BXLE  R1,RE,*-12                                                       
*                                  SYSTEM                                       
         BCTR  RF,0                                                             
         LR    R0,RF               R0=A(LAST INPUT BYTE)                        
         LA    RF,SCANTXT2                                                      
         CLI   0(RF),0             =ANY SYSTEM                                  
         BE    VALWK4                                                           
         LA    R1,WKSYVALS                                                      
VALWK2   CLI   0(R1),EOT                                                        
         BE    RHERRNV             NO MATCH - INVALID SYSTEM                    
         CLC   0(1,RF),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     VALWK2                                                           
         MVC   SCISYS,0(RF)                                                     
*                                  PROGRAM                                      
VALWK4   LA    RF,L'SCISYS(RF)                                                  
         CR    RF,R0                                                            
         BE    RHERRNV             HALF PROGRAM NO. INVALID                     
         BH    VALWKX                                                           
         OC    0(2,RF),0(RF)       TEST PROGRAM NO. IS WILD                     
         BZ    VALWK6                                                           
         MVC   SCIPRG,0(RF)        ACCEPT ANY PROGRAM                           
*                                                                               
VALWK6   LA    RF,L'SCIPRG(RF)                                                  
         CR    RF,R0                                                            
         BH    VALWKX                                                           
         MVC   SCISUB,0(RF)        AND SUB-PROGRAM                              
*                                                                               
         LA    RF,L'SCISUB(RF)     LOOK FOR DAY                                 
         CR    RF,R0                                                            
         BE    RHERRNV             HALF DAY INVALID                             
         BH    ROUTE                                                            
         OC    0(2,RF),0(RF)       TEST DAY IS WILD                             
         BZ    VALWK8              YES                                          
         TM    0(RF),X'F0'         TEST CHAR NUMERIC                            
         BNO   RHERRNV                                                          
         TM    1(RF),X'F0'                                                      
         BNO   RHERRNV                                                          
         PACK  DUB,0(2,RF)                                                      
         ICM   R1,3,DUB+6                                                       
         SRL   R1,4                                                             
         STC   R1,SCIDAY                                                        
*                                  TYPE                                         
VALWK8   LA    RF,L'SCIDAY*2(RF)                                                
         CR    RF,R0                                                            
         BH    VALWKX                                                           
         OC    SCIWTYP,0(RF)                                                    
         BZ    VALWK10             TYPE IS WILD                                 
         CLI   SCIWTYP,WKITPOST                                                 
         BE    VALWK10                                                          
         CLI   SCIWTYP,WKITCHQS                                                 
         BNE   RHERRNV             INVALID TYPE                                 
*&&DO                              NOW LOOK FOR SEQUENCE NUMBER(S)              
VALWK9   LA    RF,L'SCIWTYP(RF)                                                 
         CR    RF,R0                                                            
         BH    VALWKX                                                           
         CLI   SCIEXTRA,C' '       WILD CARD OR NO INPUT                        
         BNH   VALWK10                                                          
         CLI   SCIWTYP,WKITCHQS                                                 
         BNE   RHERRNV             INVALID TYPE                                 
*&&                                NOW LOOK FOR SEQUENCE NUMBER(S)              
VALWK10  LA    R2,SCANOUTL(R2)     NEXT SCANBLK ENTRY                           
         SR    RE,RE                                                            
         ICM   RE,1,SCANLEN1                                                    
         BZ    VALWKX              EOT                                          
         CLI   SCANLEN2,0                                                       
         BNE   VALWKX              2-PART ENTRY CAN'T BE SEQUENCE NO            
         LA    R1,SCANTXT1                                                      
         CLI   0(R1),C'-'          SEARCH INPUT FOR RANGE SEPARATOR             
         BE    VALWK12                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         TM    SCANIND1,X'80'      TEST INPUT IS NUMERIC                        
         BNO   VALWKX              NO - NOT A SEQUENCE NUMBER                   
         CLI   SCANLEN1,L'WKISEQN*2                                             
         BH    RHERRNV             SINGLE NUMBER IS TOO LONG                    
         MVC   SCISSEQ,SCANBIN1+2  TAKE LOW TWO BYTES                           
         MVC   SCIESEQ,SCANBIN1+2                                               
         B     VALWKX2                                                          
*                                  DEAL WITH RANGE                              
VALWK12  LA    RF,SCANTXT1                                                      
         CLI   SCANLEN1,1                                                       
         BE    RHERRNV             '-' ALONE IS INVALID                         
         IC    RE,SCANLEN1                                                      
         BCTR  RE,0                                                             
         LR    R0,R1               SAVE R1                                      
         SR    R1,RF               R1=L'LH HALF                                 
         BZ    VALWK14             EXPRESSION IS '-NNNN'                        
         CH    R1,=Y(4)                                                         
         BH    RHERRNV             SEQ NO. TOO LONG                             
         SR    RE,R1               RE=L'RH HALF                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCANTXT1(0)                                                  
         CVB   RF,DUB                                                           
         STCM  RF,3,SCISSEQ                                                     
VALWK14  LTR   RE,RE               L'RH HALF                                    
         BZ    VALWKX2             EXPRESSION IS 'NNNN-'                        
         CHI   RE,4                                                             
         BH    RHERRNV             SEQ NO. TOO LONG                             
         LR    R1,R0               R1=A('-')                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   RF,DUB                                                           
         CLM   RF,3,SCISSEQ                                                     
         BL    RHERRRI             START > END                                  
         STCM  RF,3,SCIESEQ                                                     
         B     VALWKX2                                                          
*                                                                               
VALWKX   B     ROUTE                                                            
*                                  NON-STANDARD EXIT FROM ROUT                  
VALWKX2  IC    RE,FVINDX           IF USER SPECIFIED WKEY AND SEQ NOS.          
         LA    RE,1(RE)            TWO SCANBLK ENTRIES HAVE BEEN USED           
         STC   RE,FVINDX           SO BUMP FVINDX                               
         LR    R1,R2               SET R1 TO 2ND ENTRY                          
         CR    R1,R1                                                            
         XIT1  REGS=(R1)           AND PASS IT BACK TO SCANFLD                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE 'NEWMOA= (MMMYY) OR '+/-1'                                 *         
* ENTRY R1=A(SCANBLOCK ENTRY)                                         *         
***********************************************************************         
         USING SCANOUTD,R2                                                      
VALMTH   LR    R2,R1                                                            
         OC    SCIMTH,SCIMTH                                                    
         BNZ   RHERRID             DUPLICATED                                   
         CLI   SCANTXT2,C'-'                                                    
         BE    *+12                                                             
         CLI   SCANTXT2,C'+'                                                    
         BNE   VALMT2                                                           
         MVC   SCISGN(1),SCANTXT2  MOVE IN SIGN                                 
         CLI   SCANTXT2+1,C'1'     CHECK INCREMENT = 1                          
         BNE   RHERRNV             NO, INVALID INPUT                            
         MVC   SCIINC(1),SCANTXT2+1                                             
         B     ROUTE                                                            
*                                                                               
VALMT2   GOTO1 VDATVAL,DMCB,(2,SCANTXT2),DUB                                    
         OC    DMCB(4),DMCB                                                     
         BZ    RHERRNV             INVALID INPUT                                
         MVC   DUB+4(2),=C'01'     MOVE IN DAYS                                 
         GOTO1 VDATCON,DMCB,(0,DUB),(1,FULL)  CONVERT TO PACKED                 
         MVC   SCIMTH,FULL                                                      
         B     ROUTE                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLE DISPLACEMENTS                                                           
         SPACE                                                                  
TABLES   DS    0H                                                               
         DC    Y(KEYTAB-WFM)                                                    
         DC    Y(OPTTAB-WFM)                                                    
         DC    Y(DCLISTU-WFM)                                                   
         DC    Y(DCLISTL-WFM)                                                   
         DC    Y(PFKTAB-WFM)                                                    
         DC    Y(CHQTAB-WFM)                                                    
         DC    Y(PRGTAB-WFM)                                                    
         DC    Y(SCRTAB-WFM)                                                    
         DC    Y(DISTAB-WFM)                                                    
         DC    Y(ACTTAB-WFM)                                                    
         DC    Y(REPSPEC-WFM)                                                   
TABLESN  EQU   (*-TABLES)/L'TABLES                                              
*                                                                               
         SPACE 2                                                                
KEYTAB   DS    0H                  ** KEY KEYWORD TABLE (SEE INPTABD)**         
         DC    S(AC8CHQS,AC3CHQS,AVALCHQ)                                       
         DC    AL1(INP2PTQ,1,DDSUSRQ,0)                                         
*                                                                               
         DC    S(AC8DATE,AC3DATE,AVALPER)                                       
         DC    AL1(INP2PTQ,L'PVALCPER,0,0)                                      
*                                                                               
         DC    S(AC8IDALL,AC3IDALL,AVALAID)                                     
         DC    AL1(INP2PTQ,L'AC@YES,0,0)                                        
*                                                                               
         DC    S(AC8IDGRP,AC3IDGRP,AVALGID)                                     
         DC    AL1(INP2PTQ,L'UIDTCODE,PUSRIDQ,0)                                
*                                                                               
         DC    S(AC8IDUSR,AC3IDUSR,AVALUID)                                     
         DC    AL1(INP2PTQ,L'UIDTCODE,0,0)                                      
*                                                                               
         DC    S(AC8ID,AC3ID,AVALID)                                            
         DC    AL1(INP2PTQ,L'UIDTCODE,DDSUSRQ,0)                                
*                                                                               
         DC    S(AC8KEY,AC3KEY,AVALWKY)                                         
         DC    AL1(INP2PTQ,7,0,0)                                               
*                                                                               
         DC    S(AC8STS,AC3STS,AVALSTS)                                         
         DC    AL1(INP2PTQ+INPNOTQ,L'AC@ACTV,0,0)                               
*                                                                               
         DC    S(AC8SYS,AC3SYS,AVALSYS)                                         
         DC    AL1(INP2PTQ,L'SENAME,DDSUSRQ,0)                                  
*                                                                               
KEYTABX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
OPTTAB   DS    0H                  ** OPTS KEYWORD TABLE (SEE INPTABD)*         
         DC    S(AC8CHQS,AC3CHQS,AVALCHQ)                                       
         DC    AL1(INP2PTQ,1,DDSUSRQ,TWASCLST)                                  
*                                                                               
         DC    S(AC8DATE,AC3DATE,AVALPER)                                       
         DC    AL1(INP2PTQ,L'PVALCPER,0,0)                                      
*                                                                               
         DC    S(AC8KEY,AC3KEY,AVALWKY)                                         
         DC    AL1(INP2PTQ,7,0,TWASCLST)                                        
*                                                                               
         DC    S(AC8ID,AC3ID,AVALID)                                            
         DC    AL1(INP2PTQ,L'UIDTCODE,DDSUSRQ,TWASCLST)                         
*                                                                               
         DC    S(AC8IDUSR,AC3IDUSR,AVALUID)                                     
         DC    AL1(INP2PTQ,L'UIDTCODE,0,TWASCLST)                               
*                                                                               
         DC    S(AC8STS,AC3STS,AVALSTS)                                         
         DC    AL1(INP2PTQ+INPNOTQ,L'AC@ACTV,0,TWASCLST)                        
*                                                                               
         DC    S(AC8ACC,AC3ACC,AVALACC)                                         
         DC    AL1(INP2PTQ,L'LC@ACC,0,TWASCDET)                                 
*                                                                               
         DC    S(AC8CAC,AC3CAC,AVALACC)                                         
         DC    AL1(INP2PTQ,L'LC@ACC,0,TWASCDET)                                 
*                                                                               
         DC    S(AC8DEST,AC3DEST,AVALDST)                                       
         DC    AL1(INP2PTQ,L'LC@UID,0,0)                                        
*                                                                               
         DC    S(AC8SYS,AC3SYS,AVALSYS)                                         
         DC    AL1(INP2PTQ,L'SENAME,DDSUSRQ,TWASCLST)                           
*                                                                               
         DC    S(AC8SYS,AC3SYS,SCIIND1)                                         
         DC    AL1(INP1PTQ,SCIISNO,DDSUSRQ,TWASCLST)                            
*                                                                               
         DC    S(AC8TOT,AC3TOT,SCIIND1)                                         
         DC    AL1(INP1PTQ,SCIITOT,DDSUSRQ,TWASCLST)                            
*                                                                               
         DC    S(AC8AMT,AC3AMT,AVALAMT)                                         
         DC    AL1(INP2PTQ,11,0,TWASCDET)                                       
*                                                                               
         DC    S(AC8REF,AC3REF,SCIREF)                                          
         DC    AL1(INP2PTQ+INPNVLQ,L'SCIREF,0,TWASCDET)                         
*                                                                               
         DC    S(AC8NMOA,AC3NMOA,AVALMTH)                                       
         DC    AL1(INP2PTQ,5,IAMGODQ,TWASCLST)                                  
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
WKSYVALS DS    0X                  ** WKR KEY VALIDATION TABLES **              
         DC    AL1(WKISACC,WKISFEE,WKISMED,WKISPRT)                             
         DC    AL1(WKISSPT,WKISTLT)                                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
PFKTAB   DS    0H                  ** ACTION/SCROLL PFKEY TABLE **              
*                                  PFK2-6=ACTIONS, PFK7-12=SCROLL               
         DC    AL1(PFK02),AL1(PFKTILST,DDSUSRQ,0)                               
         DC    S(0),AL2(PASSCHK-WFM)                                            
*                                                                               
*        DC    AL1(PFK03),AL1(0,0,0)                                            
*        DC    S(0),AL2(0)                                                      
*                                                                               
         DC    AL1(PFK04),AL1(PFKTIDET,DDSUSRQ,0)                               
         DC    S(LC@SPCL),AL2(HEXDISP-WFM)                                      
*                                                                               
         DC    AL1(PFK05),AL1(PFKTIDET,0,0)                                     
         DC    S(LC@PRNT),AL2(PPRNT-WFM)                                        
*                                                                               
         DC    AL1(PFK06),AL1(PFKTIDET,0,0)                                     
         DC    S(LC@END),AL2(RESLSCR-WFM)                                       
*                                                                               
         DC    AL1(PFK07),AL1(SCRLUP+SCRLHALF+PFKTILST+PFKTIDET,0,0)            
         DC    S(LC@HALF),AL2(0)                                                
*                                                                               
         DC    AL1(PFK08),AL1(SCRLUP+SCRLPAGE+PFKTILST+PFKTIDET,0,0)            
         DC    S(LC@PAGE),AL2(0)                                                
*                                                                               
         DC    AL1(PFK09),AL1(SCRLUP+SCRLMAXI+PFKTILST+PFKTIDET,0,0)            
         DC    S(LC@FRST),AL2(0)                                                
*                                                                               
         DC    AL1(PFK10),AL1(SCRLDOWN+SCRLHALF+PFKTILST+PFKTIDET,0,0)          
         DC    S(LC@HALF),AL2(0)                                                
*                                                                               
         DC    AL1(PFK11),AL1(SCRLDOWN+SCRLPAGE+PFKTILST+PFKTIDET,0,0)          
         DC    S(LC@PAGE),AL2(0)                                                
*                                                                               
         DC    AL1(PFK12),AL1(SCRLDOWN+SCRLMAXI+PFKTILST+PFKTIDET,0,0)          
         DC    S(LC@LAST),AL2(0)                                                
*                                                                               
PFKTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
SCRTAB   DS    0H                  ** SCREEN ATTRIBUTES (SEE SCRTABD)**         
         DC    Y(WFMLHEDH-TWAD,WFMLACTH-TWAD)                                   
         DC    Y((WFMLDTLH-TWAD)+(DISLHDR2-DISLINED))                           
         DC    Y(0,WFMLPFKH-TWAD)                                               
         DC    Y(DISLLLIN-DISLINED)                                             
         DC    Y((((WFMLDTLH-TWAD)-(WFMLACTH-TWAD))/DISLLINL)+1)                
         DC    Y(L'DISLLLIN)                                                    
         DC    Y(DISLLINL)                                                      
         DC    AL1(14,08,06,12,09,11,13,00,00,00)    LIST SCREEN                
         DC    AL1(14,08,06,12,09,11,13,00,00,00)    DUMMY                      
*                                                                               
         DC    Y(WFMDHEDH-TWAD,WFMDDETH-TWAD,WFMDDTLH-TWAD)                     
         DC    Y(WFMDTOTH-TWAD,WFMDPFKH-TWAD)                                   
         DC    Y(DISLDLIN-DISLINED)                                             
         DC    Y((((WFMDDTLH-TWAD)-(WFMDDETH-TWAD))/DISLDLNL)+1)                
         DC    Y(L'DISLDLIN)                                                    
         DC    Y(DISLDLNL)                                                      
         DC    AL1(01,04,05,06,10,07,03,02,00,00)    DETAIL SCREEN              
         DC    AL1(01,04,05,06,10,00,00,00,00,00)    DITTO (NEW DJ)             
         SPACE 2                                                                
DISTAB   DS    0H                  ** DISPLAY COLUMNS (SEE DISTABD) **          
         DC    S(LC@ACC),AL1(L'LC@ACC,DETLSCRQ,0,0)          1                  
         DC    S(LC@AMT),AL1(L'LC@AMT,DETLSCRQ,0,0)          2                  
         DC    S(LC@BRF),AL1(L'LC@BRF,DETLSCRQ,0,0)          3                  
         DC    S(LC@CAC),AL1(L'LC@CAC,DETLSCRQ,0,0)          4                  
         DC    S(LC@CDS),AL1(L'LC@CDS,DETLSCRQ,0,0)          5                  
         DC    S(LC@DAT),AL1(L'LC@DAT,LISTSCRQ+DETLSCRQ,0,0) 6                  
         DC    S(LC@MOS),AL1(L'LC@MOS,DETLSCRQ,0,0)          7                  
         DC    S(LC@NAM),AL1(L'LC@NAM,LISTSCRQ,0,0)          8                  
         DC    S(LC@RCS),AL1(L'LC@RCS,LISTSCRQ,0,0)          9                  
         DC    S(LC@REF),AL1(L'LC@REF,DETLSCRQ,0,0)         10                  
         DC    S(LC@STS),AL1(L'LC@STS,LISTSCRQ,0,0)         11                  
         DC    S(LC@TIM),AL1(L'LC@TIM,LISTSCRQ,0,0)         12                  
         DC    S(LC@UID),AL1(L'LC@UID,LISTSCRQ,PUSRIDQ,0)   13                  
         DC    S(LC@WKY),AL1(L'LC@WKY,LISTSCRQ,0,0)         14                  
DISTABN  EQU   (*-DISTAB)/DISTABL                                               
DISTABX  DC    AL1(EOT)                                                         
         SPACE 2                                                                
ACTTAB   DS    0H                  ** ACTION VALIDATION/PROCESSING **           
         DC    S(AC3ACTV),AL2(PACTV-WFM)                                        
         DC    AL1(IAMGODQ,BNZQ,TSWKBADQ,0)                                     
         DC    S(AC3COPY),AL2(PCOPY-WFM)                                        
         DC    AL1(IAMGODQ,0,0,0)                                               
         DC    S(AC3DELT),AL2(PDELT-WFM)                                        
         DC    AL1(IAMGODQ,0,0,0)                                               
         DC    S(AC3DELT),AL2(PDELT-WFM)                                        
         DC    AL1(SUSRIDQ+DELETEQ,BZQ,TSARDELQ,0)                              
         DC    S(AC3KEEP),AL2(PKEEP-WFM)                                        
         DC    AL1(IAMGODQ,0,0,0)                                               
         DC    S(AC3PRNT),AL2(PPRNT-WFM)                                        
         DC    AL1(SUSRIDQ,0,0,0)                                               
         DC    S(AC3REVS),AL2(PREVS-WFM)                                        
         DC    AL1(IAMGODQ,BNZQ,TSWKBADQ+TSARCHXQ,0)                            
         DC    S(AC3SELC),AL2(PSELC-WFM)                                        
         DC    AL1(SUSRIDQ,0,0,0)                                               
         DC    S(AC3TRCE),AL2(PTRCE-WFM)                                        
         DC    AL1(IAMGODQ,0,0,0)                                               
         DC    S(AC3UPDT),AL2(PUPDT-WFM)                                        
         DC    AL1(SUSRIDQ+UPDATEQ,BNZQ,TSWKBADQ+TSARCHXQ,0)                    
ACTTABN  EQU   (*-ACTTAB)/ACTTABL                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
REPSPEC  DS    0H                  ** REPORT SPECS **                           
         SPROG 0,1,2                                                            
         SPEC  H1,1,RUN                                                         
         SPEC  H1,120,PAGE                                                      
         SPEC  H3,1,AC#KEY,5,L                                                  
*                                  WORKER FILE LIST                             
         SPROG 1                                                                
         SPEC  H1,40,AC#WFLST,52,C                                              
         SPEC  H2,40,AC#WFLST,52,CU                                             
         SPEC  H5,01,AC#ACC,15                                                  
         SPEC  H6,01,AC#ACC,15,LU                                               
         SPEC  H5,16,AC#CTRA,15                                                 
         SPEC  H6,16,AC#CTRA,15,LU                                              
         SPEC  H5,32,AC#CODES,6                                                 
         SPEC  H6,32,AC#CODES,6,LU                                              
         SPEC  H5,38,AC#DATE,8                                                  
         SPEC  H6,38,AC#DATE,8,LU                                               
         SPEC  H5,47,AC#REF,6                                                   
         SPEC  H6,47,AC#REF,6,LU                                                
         SPEC  H5,54,AC#BAT,6                                                   
         SPEC  H6,54,AC#BAT,6,LU                                                
         SPEC  H5,61,AC#NRTV,42                                                 
         SPEC  H6,61,AC#NRTV,42,LU                                              
         SPEC  H5,103,AC#DRS,13,R                                               
         SPEC  H6,103,AC#DRS,13,RU                                              
         SPEC  H5,117,AC#CRS,13,R                                               
         SPEC  H6,117,AC#CRS,13,RU                                              
*                                  ANALYSIS OF WORKER POSTINGS                  
         SPROG 2                                                                
         SPEC  H1,40,AC#ANWKP,30,C                                              
         SPEC  H2,40,AC#ANWKP,30,CU                                             
         SPEC  H5,1,AC#UNTCN,30,L                                               
         SPEC  H6,1,AC#UNTCN,30,LU                                              
         SPEC  H5,34,AC#LGRCN,30,L                                              
         SPEC  H6,34,AC#LGRCN,30,LU                                             
         SPEC  H5,71,AC#DRS,13,R                                                
         SPEC  H6,71,AC#DRS,13,RU                                               
         SPEC  H5,86,AC#CRS,13,R                                                
         SPEC  H6,86,AC#CRS,13,RU                                               
         SPEC  H5,101,AC#BAL,13,R                                               
         SPEC  H6,101,AC#BAL,13,RU                                              
*                                                                               
         DC    AL1(EOT)            ** SPEC END MARKER **                        
         EJECT                                                                  
DCLISTU  DS    0X                  ** UPPER CASE DICTIONARY **                  
         DCDDL AC#ACC,8                                                         
         DCDDL AC#ACC,3                                                         
         DCDDL AC#ACTV,8                                                        
         DCDDL AC#ACTV,3                                                        
         DCDDL AC#ALL,8                                                         
         DCDDL AC#AMT,8                                                         
         DCDDL AC#AMT,3                                                         
         DCDDL AC#ANWKP,11                                                      
         DCDDL AC#CHKS,8                                                        
         DCDDL AC#CHKS,3                                                        
         DCDDL AC#CTR,8                                                         
         DCDDL AC#CTR,3                                                         
         DCDDL AC#COPY,8                                                        
         DCDDL AC#COPY,3                                                        
         DCDDL AC#CRPT,8                                                        
         DCDDL AC#CR,2                                                          
         DCDDL AC#DATE,8                                                        
         DCDDL AC#DATE,3                                                        
         DCDDL AC#DR,2                                                          
         DCDDL AC#DEL,8                                                         
         DCDDL AC#DEL,3                                                         
         DCDDL AC#DEST,8                                                        
         DCDDL AC#DEST,3                                                        
         DCDDL AC#HOLD,8                                                        
         DCDDL AC#ID,8                                                          
         DCDDL AC#ID,3                                                          
         DCDDL AC#IDALL,8                                                       
         DCDDL AC#IDALL,3                                                       
         DCDDL AC#IDGRP,8                                                       
         DCDDL AC#IDGRP,3                                                       
         DCDDL AC#IDUSR,8                                                       
         DCDDL AC#IDUSR,3                                                       
         DCDDL AC#KEEP,8                                                        
         DCDDL AC#KEEP,3                                                        
         DCDDL AC#KEY,8                                                         
         DCDDL AC#KEY,3                                                         
         DCDDL AC#NO,4                                                          
         DCDDL AC#NMOA,8                                                        
         DCDDL AC#NMOA,3                                                        
         DCDDL AC#ONLY,2                                                        
         DCDDL AC#PFK,3                                                         
         DCDDL AC#PRINT,3                                                       
         DCDDL AC#REF,8                                                         
         DCDDL AC#REF,3                                                         
         DCDDL AC#RVRS2,8                                                       
         DCDDL AC#RVRS2,3                                                       
         DCDDL AC#SEL,3                                                         
         DCDDL AC#STT,8                                                         
         DCDDL AC#STT,3                                                         
         DCDDL AC#SYS,8                                                         
         DCDDL AC#SYS,3                                                         
         DCDDL AC#TOTLS,8                                                       
         DCDDL AC#TOTLS,3                                                       
         DCDDL AC#TRC,8                                                         
         DCDDL AC#TRC,3                                                         
         DCDDL AC#UPD,8                                                         
         DCDDL AC#UPD,3                                                         
         DCDDL AC#WFLST,11                                                      
         DCDDL AC#YES,4                                                         
DCLISTUX DC    AL1(EOT)                                                         
         SPACE 1                                                                
DCLISTL  DS    0X                  ** LOWER CASE DICTIONARY **                  
         DCDDL AC#ACC,14                                                        
         DCDDL AC#ACTV,4                                                        
         DCDDL AC#ALTPF,6                                                       
         DCDDL AC#AMT,14,R                                                      
         DCDDL AC#ANWKP,11                                                      
         DCDDL AC#BATRF,4                                                       
         DCDDL AC#CNTOT,16                                                      
         DCDDL AC#CODES,5                                                       
         DCDDL AC#COPFR,11                                                      
         DCDDL AC#COPTO,9                                                       
         DCDDL AC#CTRA,14                                                       
         DCDDL AC#DATE,8                                                        
         DCDDL AC#DEL,4                                                         
         DCDDL AC#END,8                                                         
         DCDDL AC#FIRST,8                                                       
         DCDDL AC#HALF,8                                                        
         DCDDL AC#HOLD,4                                                        
         DCDDL AC#KEEP,4                                                        
         DCDDL AC#LAST,8                                                        
         DCDDL AC#MOS,5                                                         
         DCDDL AC#NAME,21                                                       
         DCDDL AC#PAGE,8                                                        
         DCDDL AC#PRINT,8                                                       
         DCDDL AC#PGA08,30                                                      
         DCDDL AC#PGA21,30                                                      
         DCDDL AC#PGA25,30                                                      
         DCDDL AC#PGA90,30                                                      
         DCDDL AC#PGA91,30                                                      
         DCDDL AC#PGA98,30                                                      
         DCDDL AC#PGA55,30                                                      
         DCDDL AC#PGA5P,30                                                      
         DCDDL AC#PGA23,30                                                      
         DCDDL AC#PGA05,30                                                      
         DCDDL AC#PGA07,30                                                      
         DCDDL AC#PGA09,30                                                      
         DCDDL AC#PGA47,30                                                      
         DCDDL AC#PGA8A,30                                                      
         DCDDL AC#PGAA1,30                                                      
         DCDDL AC#PGAA2,30                                                      
         DCDDL AC#PGAA8,30                                                      
         DCDDL AC#PGAAJ,30                                                      
         DCDDL AC#PGAAT,30                                                      
         DCDDL AC#PGACE,30                                                      
         DCDDL AC#PGADB,30                                                      
         DCDDL AC#PGADC,30                                                      
         DCDDL AC#PGADD,30                                                      
*&&UK                                                                           
         DCDDL AC#PGADL,30                                                      
         DCDDL AC#PGADM,30                                                      
         DCDDL AC#PGATU,30                                                      
*&&                                                                             
         DCDDL AC#PGADS,30                                                      
         DCDDL AC#PGADT,30                                                      
         DCDDL AC#PGADU,30                                                      
         DCDDL AC#PGADV,30                                                      
         DCDDL AC#PGADW,30                                                      
         DCDDL AC#PGAD0,30                                                      
         DCDDL AC#PGAD1,30                                                      
         DCDDL AC#PGAD2,30                                                      
         DCDDL AC#PGAD4,30                                                      
         DCDDL AC#PGAD5,30                                                      
         DCDDL AC#PGAFA,30                                                      
         DCDDL AC#PGAF2,30                                                      
         DCDDL AC#PGAHP,30                                                      
         DCDDL AC#PGAPA,30                                                      
         DCDDL AC#PGAPE,30                                                      
         DCDDL AC#PGAT1,30                                                      
         DCDDL AC#PGAT4,30                                                      
         DCDDL AC#PGAT5,30                                                      
         DCDDL AC#PGF0C,30                                                      
         DCDDL AC#PGM09,30                                                      
         DCDDL AC#PGM10,30                                                      
         DCDDL AC#PGM14,30                                                      
         DCDDL AC#PGM15,30                                                      
         DCDDL AC#PGM50,30                                                      
         DCDDL AC#PGMAP,30                                                      
         DCDDL AC#PGMD4,30                                                      
         DCDDL AC#PGMD7,30                                                      
         DCDDL AC#PGMDA,30                                                      
         DCDDL AC#PGMH4,30                                                      
         DCDDL AC#PGS54,30                                                      
         DCDDL AC#PGP54,30                                                      
         DCDDL AC#PGSBA,30                                                      
         DCDDL AC#PGPBA,30                                                      
         DCDDL AC#PGNBA,30                                                      
         DCDDL AC#PGS03,30                                                      
         DCDDL AC#PGP05,30                                                      
         DCDDL AC#PRGE,4                                                        
         DCDDL AC#RECS,5,R                                                      
         DCDDL AC#REF,6                                                         
         DCDDL AC#RVRS,4                                                        
         DCDDL AC#RVRON,11                                                      
         DCDDL AC#SPCL,8                                                        
         DCDDL AC#STT,9,C                                                       
         DCDDL AC#TIME,5                                                        
         DCDDL AC#TITEM,16                                                      
         DCDDL AC#TFWF,30                                                       
         DCDDL AC#TOTAL,6                                                       
         DCDDL AC#USRID,8                                                       
         DCDDL AC#VENSF,30                                                      
         DCDDL AC#VENSP,30                                                      
         DCDDL AC#VENSQ,30                                                      
         DCDDL AC#VENSS,30                                                      
         DCDDL AC#VENST,30                                                      
         DCDDL AC#VENSU,30                                                      
         DCDDL AC#VENSV,30                                                      
         DCDDL AC#VENSW,30                                                      
         DCDDL AC#VENSX,30                                                      
         DCDDL AC#VENSY,30                                                      
         DCDDL AC#WFLST,11                                                      
         DCDDL AC#WKKEY,12                                                      
         DCDDL AC#YES,4                                                         
         DCDDL AC#COPY,4                                                        
         DCDDL AC#ON,2                                                          
         DCDDL AC#PGA27,30                                                      
         DCDDL AC#PGA29,30                                                      
         DCDDL AC#PGAIO,30                                                      
DCLISTLX DC    AL1(EOT)                                                         
         EJECT                                                                  
PRGTAB   DS    0X                  ** PROGRAM TYPE TABLE **                     
*                                                                               
         DC    AL1(WKISACC),C'08',AL2(LC@PGA08-DSLISTL)                         
         DC    AL2(AC#PGA08),AL1(PRGTINOC+PRGTIJRN,PRGTNORV)                    
*                                                                               
         DC    AL1(WKISACC),C'DJ',AL2(LC@PGA08-DSLISTL)                         
         DC    AL2(AC#PGA08),AL1(PRGTINOC+PRGTIJRN,PRGTNORV)                    
*                                                                               
         DC    AL1(WKISACC),C'PE',AL2(LC@PGAPE-DSLISTL)                         
         DC    AL2(AC#PGAPE),AL1(0,PRGTDELT+PRGTUPDT)                           
*                                                                               
         DC    AL1(WKISACC),C'21',AL2(LC@PGA21-DSLISTL)                         
         DC    AL2(AC#PGA21),AL1(0,PRGTUPDT+PRGTICOP)                           
*                                                                               
         DC    AL1(WKISACC),C'23',AL2(LC@PGA23-DSLISTL)                         
         DC    AL2(AC#PGA23),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'25',AL2(LC@PGA25-DSLISTL)                         
         DC    AL2(AC#PGA25),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'90',AL2(LC@PGA90-DSLISTL)                         
         DC    AL2(AC#PGA90),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'91',AL2(LC@PGA91-DSLISTL)                         
         DC    AL2(AC#PGA91),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'98',AL2(LC@PGA98-DSLISTL)                         
         DC    AL2(AC#PGA98),AL1(PRGTITRL+PRGTIPOF,PRGTICPY+PRGTXWFM)           
*                                                                               
         DC    AL1(WKISACC),C'55',AL2(LC@PGA55-DSLISTL)                         
         DC    AL2(AC#PGA55),AL1(PRGTICHQ,PRGTUPDT)                             
*                                                                               
         DC    AL1(WKISACC),C'5P',AL2(LC@PGA5P-DSLISTL)                         
         DC    AL2(AC#PGA5P),AL1(PRGTICHQ,PRGTUPDT)                             
*                                                                               
         DC    AL1(WKISACC),C'HP',AL2(LC@PGAHP-DSLISTL)                         
         DC    AL2(AC#PGAHP),AL1(PRGTICHQ,PRGTUPDT)                             
*&&US                                                                           
         DC    AL1(WKISACC),C'23',AL2(LC@PGA23-DSLISTL)                         
         DC    AL2(AC#PGA23),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'05',AL2(LC@PGA05-DSLISTL)                         
         DC    AL2(AC#PGA05),AL1(PRGTITRL,PRGTICPY+PRGTXWFM)                    
*                                                                               
         DC    AL1(WKISACC),C'07',AL2(LC@PGA07-DSLISTL)                         
         DC    AL2(AC#PGA07),AL1(PRGTITRL+PRGTITPY,PRGTICPY)                    
*                                                                               
         DC    AL1(WKISACC),C'47',AL2(LC@PGA47-DSLISTL)                         
         DC    AL2(AC#PGA47),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'A1',AL2(LC@PGAA1-DSLISTL)                         
         DC    AL2(AC#PGAA1),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'A8',AL2(LC@PGAA8-DSLISTL)                         
         DC    AL2(AC#PGAA8),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'CE',AL2(LC@PGACE-DSLISTL)                         
         DC    AL2(AC#PGACE),AL1(PRGTITRL,PRGTICPY)                             
*                                                                               
         DC    AL1(WKISACC),C'FA',AL2(LC@PGAFA-DSLISTL)                         
         DC    AL2(AC#PGAFA),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'PA',AL2(LC@PGAPA-DSLISTL)                         
         DC    AL2(AC#PGAPA),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'T4',AL2(LC@PGAT4-DSLISTL)                         
         DC    AL2(AC#PGAT4),AL1(0,PRGTINOT)                                    
*                                                                               
         DC    AL1(WKISACC),C'T5',AL2(LC@PGAT5-DSLISTL)                         
         DC    AL2(AC#PGAT5),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISSPT),C'54',AL2(LC@PGS54-DSLISTL)                         
         DC    AL2(AC#PGS54),AL1(PRGTICLR,PRGTUPDT)                             
*                                                                               
         DC    AL1(WKISPRT),C'54',AL2(LC@PGP54-DSLISTL)                         
         DC    AL2(AC#PGP54),AL1(PRGTICLR,PRGTUPDT)                             
*                                                                               
         DC    AL1(WKISPRT),C'BA',AL2(LC@PGPBA-DSLISTL)                         
         DC    AL2(AC#PGPBA),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISSPT),C'BA',AL2(LC@PGSBA-DSLISTL)                         
         DC    AL2(AC#PGSBA),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISNET),C'BA',AL2(LC@PGNBA-DSLISTL)                         
         DC    AL2(AC#PGNBA),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISSPT),C'03',AL2(LC@PGS03-DSLISTL)                         
         DC    AL2(AC#PGS03),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISPRT),C'05',AL2(LC@PGP05-DSLISTL)                         
         DC    AL2(AC#PGP05),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'8A',AL2(LC@PGA8A-DSLISTL)                         
         DC    AL2(AC#PGA8A),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'A2',AL2(LC@PGAA2-DSLISTL)                         
         DC    AL2(AC#PGAA2),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'AJ',AL2(LC@PGAAJ-DSLISTL)                         
         DC    AL2(AC#PGAAJ),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'AT',AL2(LC@PGAAT-DSLISTL)                         
         DC    AL2(AC#PGAAT),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'27',AL2(LC@PGA27-DSLISTL)                         
         DC    AL2(AC#PGA27),AL1(0,PRGTUPDT+PRGTICOP)                           
*                                                                               
         DC    AL1(WKISACC),C'29',AL2(LC@PGA29-DSLISTL)                         
         DC    AL2(AC#PGA29),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'IO',AL2(LC@PGAIO-DSLISTL)                         
         DC    AL2(AC#PGAIO),AL1(0,PRGTUPDT+PRGTICOP)                           
*                                                                               
*&&                                                                             
*&&UK                                                                           
         DC    AL1(WKISACC),C'09',AL2(LC@PGA09-DSLISTL)                         
         DC    AL2(AC#PGA09),AL1(PRGTITRL,PRGTICPY+PRGTXWFM)                    
*                                                                               
         DC    AL1(WKISACC),C'DK',AL2(LC@PGA09-DSLISTL)                         
         DC    AL2(AC#PGA09),AL1(PRGTINOC+PRGTIJRN,PRGTNORV+PRGTXWFM)           
*                                                                               
         DC    AL1(WKISACC),C'F2',AL2(LC@PGAF2-DSLISTL)                         
         DC    AL2(AC#PGAF2),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'T1',AL2(LC@PGAT1-DSLISTL)                         
         DC    AL2(AC#PGAT1),AL1(0,PRGTNORV)                                    
*                                                                               
         DC    AL1(WKISFEE),C'0C',AL2(LC@PGF0C-DSLISTL)                         
         DC    AL2(AC#PGF0C),AL1(0,PRGTUPDT+PRGTNORV)                           
*                                                                               
         DC    AL1(WKISMED),C'09',AL2(LC@PGM09-DSLISTL)                         
         DC    AL2(AC#PGM09),AL1(0,PRGTICOP)                                    
*                                                                               
         DC    AL1(WKISMED),C'10',AL2(LC@PGM10-DSLISTL)                         
         DC    AL2(AC#PGM10),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISMED),C'14',AL2(LC@PGM14-DSLISTL)                         
         DC    AL2(AC#PGM14),AL1(0,PRGTUPDT+PRGTICOP)                           
*                                                                               
         DC    AL1(WKISMED),C'15',AL2(LC@PGM15-DSLISTL)                         
         DC    AL2(AC#PGM15),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISMED),C'50',AL2(LC@PGM50-DSLISTL)                         
         DC    AL2(AC#PGM50),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'DS',AL2(LC@PGADS-DSLISTL)                         
         DC    AL2(AC#PGADS),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'DT',AL2(LC@PGADT-DSLISTL)                         
         DC    AL2(AC#PGADT),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'D2',AL2(LC@PGAD2-DSLISTL)                         
         DC    AL2(AC#PGAD2),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'D4',AL2(LC@PGAD4-DSLISTL)                         
         DC    AL2(AC#PGAD4),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'D5',AL2(LC@PGAD5-DSLISTL)                         
         DC    AL2(AC#PGAD5),AL1(0,PRGTUPDT+PRGTNORV)                           
*                                                                               
         DC    AL1(WKISACC),C'DU',AL2(LC@PGADU-DSLISTL)                         
         DC    AL2(AC#PGADU),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'DV',AL2(LC@PGADV-DSLISTL)                         
         DC    AL2(AC#PGADV),AL1(0,PRGTUPDT+PRGTNORV)                           
*                                                                               
         DC    AL1(WKISMED),C'D4',AL2(LC@PGAD4-DSLISTL)                         
         DC    AL2(AC#PGAD4),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISMED),C'H4',AL2(LC@PGMH4-DSLISTL)                         
         DC    AL2(AC#PGMH4),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISMED),C'DA',AL2(LC@PGMDA-DSLISTL)                         
         DC    AL2(AC#PGMDA),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISMED),C'AP',AL2(LC@PGMAP-DSLISTL)                         
         DC    AL2(AC#PGMAP),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISMED),C'D7',AL2(LC@PGMD7-DSLISTL)                         
         DC    AL2(AC#PGMD7),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'D1',AL2(LC@PGAD1-DSLISTL)                         
         DC    AL2(AC#PGAD1),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'D!',AL2(LC@PGADW-DSLISTL)                         
         DC    AL2(AC#PGADW),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'DB',AL2(LC@PGADB-DSLISTL)                         
         DC    AL2(AC#PGADB),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'DC',AL2(LC@PGADC-DSLISTL)                         
         DC    AL2(AC#PGADC),AL1(PRGTICHQ,PRGTUPDT)                             
*                                                                               
         DC    AL1(WKISACC),C'DD',AL2(LC@PGADD-DSLISTL)                         
         DC    AL2(AC#PGADD),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'D0',AL2(LC@PGAD0-DSLISTL)                         
         DC    AL2(AC#PGAD0),AL1(0,PRGTUPDT)                                    
*                                                                               
         DC    AL1(WKISACC),C'DL',AL2(LC@PGADL-DSLISTL)                         
         DC    AL2(AC#PGADL),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'DM',AL2(LC@PGADM-DSLISTL)                         
         DC    AL2(AC#PGADM),AL1(0,0)                                           
*                                                                               
         DC    AL1(WKISACC),C'CQ',AL2(LC@PGATU-DSLISTL)                         
         DC    AL2(AC#PGATU),AL1(0,0)                                           
*                                                                               
*&&                                                                             
PRGTABX  DC    AL1(PRGTEOTQ)                                                    
         EJECT                                                                  
CHQTAB   DS    0XL24               ** CHEQUE LEDGERS TABLE **                   
*&&UK                                                                           
         DC    C'F',AL2(LC@VENSF-DSLISTL)                                       
         DC    C'T',AL2(LC@VENST-DSLISTL)                                       
         DC    C'V',AL2(LC@VENSV-DSLISTL)                                       
         DC    C'X',AL2(LC@VENSX-DSLISTL)                                       
*&&                                                                             
*&&US                                                                           
         DC    C'P',AL2(LC@VENSP-DSLISTL)                                       
         DC    C'Q',AL2(LC@VENSQ-DSLISTL)                                       
         DC    C'S',AL2(LC@VENSS-DSLISTL)                                       
         DC    C'T',AL2(LC@VENST-DSLISTL)                                       
         DC    C'U',AL2(LC@VENSU-DSLISTL)                                       
         DC    C'V',AL2(LC@VENSV-DSLISTL)                                       
         DC    C'W',AL2(LC@VENSW-DSLISTL)                                       
         DC    C'X',AL2(LC@VENSX-DSLISTL)                                       
         DC    C'Y',AL2(LC@VENSY-DSLISTL)                                       
*&&                                                                             
CHQTABX  DC    AL1(CHQTEOTQ)                                                    
         EJECT                                                                  
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
DUB      DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
DMCB     DS    6F                                                               
HALF     DS    H                                                                
HALF2    DS    H                                                                
FLAG     DS    XL1                                                              
FLAGABLQ EQU   X'80'               ACCOUNT BALANCE ELEMENT REQUIRED             
BYTE     DS    XL1                                                              
CHAR     DS    XL1                                                              
PARM     DS    6F                                                               
RELO     DS    F                                                                
*                                                                               
AINP     DS    A                                                                
ATWA     DS    A                                                                
ATIA     DS    A                                                                
ACOM     DS    A                                                                
AUTL     DS    A                                                                
ABOX     DS    A                                                                
*                                                                               
ATABLES  DS    0A                  ** ROOT TABLE ADDRESSES **                   
AKEYTAB  DS    A                   A(KEY BUILD TABLE)                           
AOPTTAB  DS    A                   A(DISPLAY BUILD TABLE)                       
ADCLISTU DS    A                   A(UPPER CASE DICTIONARY)                     
ADCLISTL DS    A                   A(LOWER CASE DICTIONARY)                     
APFKTAB  DS    A                   A(PFKEY TABLE)                               
ACHQTAB  DS    A                   A(WORKER CHEQUE FILE TYPES)                  
APRGTAB  DS    A                   A(WORKER FILE TYPES)                         
ASCRTAB  DS    A                   A(SCROLL TABLE)                              
ADISTAB  DS    A                   A(DISPLAY COLS TABLE)                        
AACTTAB  DS    A                   A(ACTION TABLE)                              
AREPSPEC DS    A                   A(REPORT SPEC POOL)                          
         DS    3A                  N/D                                          
*                                                                               
AOFFBLK  DS    A                   A(OFFAL BLOCK)                               
APQBUFF  DS    A                   A(PRINT QUEUE BUFFER)                        
ATSARBLK DS    A                   A(TSAR BLOCK)                                
ATRNBLK  DS    A                   A(ADDTRN BLOCK)                              
AIOAS    DS    A                   A(I/O AREAS)                                 
AWKBUF1  DS    A                   A(WORKER WORK BUFFER 1)                      
AWKBUF2  DS    A                   A(WORKER WORK BUFFER 2)                      
AWKIO    DS    A                   A(WORKER REC AREA)                           
AUIDTAB  DS    A                   A(USER-ID TABLE)                             
*                                                                               
ROUTS    DS    0A                  ** ROOT ROUTINE ADDRESSES **                 
ADISPLAY DS    A                   BUILD & DISPLAY DETAIL LINES                 
ADISREC  DS    A                   BUILD & DISPLAY SUBSIDIARY ROUTINE           
ABLDDIS  DS    A                   BUILD & DISPLAY SUBSIDIARY ROUTINE           
ABLDLIN  DS    A                   BUILD & DISPLAY SUBSIDIARY ROUTINE           
ABLDPFK  DS    A                   BUILD PFKEY DISPLAY LINE                     
ACONFRM  DS    A                   ACTION CONFIRMATION DISPLAY/VALIDN           
AFLDVAL  DS    A                   GENERAL FIELD VALIDATION                     
AFILTER  DS    A                   FILTER TSAR RECORDS                          
AGETUID  DS    A                   BUILD USERID TABLE/VALIDATE USERID           
AGETWKN  DS    A                   GET WORKER FILE NAME FROM PRGTAB             
AIOEXEC  DS    A                   IO READ                                      
AOVRSCR  DS    A                   LOAD A SCREEN                                
APRTCLO  DS    A                   INIT PRINT FILE                              
APRTINI  DS    A                   CLOSE PRINT FILE                             
AREADWK  DS    A                   READ/FILTER WORKER INDEX RECORDS             
ASCANFLD DS    A                   SCAN KEY OR OPTIONS INPUT                    
ATSARADD DS    A                   ADD A TSAR RECORD                            
ATSARGET DS    A                   GET A TSAR RECORD                            
ATSARINI DS    A                   INITIALISE/RESTORE TSAR BUFFER               
AVALACC  DS    A                   VALIDATE ACCOUNT                             
AVALAMT  DS    A                   VALIDATE AMOUNT                              
AVALCHQ  DS    A                   VALIDATE INCLUDE CHEQUES                     
AVALDST  DS    A                   VALIDATE REPORT DESTINATION                  
AVALPER  DS    A                   VALIDATE TRANSACTION DATE PERIOD             
AVALSTS  DS    A                   VALIDATE WORKER FILE STATUS FILTER           
AVALSYS  DS    A                   VALIDATE ACCOUNT SYSTEM NAME FILTER          
AVALID   DS    A                   VALIDATE 'ID=' FILTER                        
AVALAID  DS    A                   VALIDATE 'ALLIDS='                           
AVALGID  DS    A                   VALIDATE 'GROUPID='                          
AVALUID  DS    A                   VALIDATE 'USERID='                           
AVALWKY  DS    A                   VALIDATE WORKER FILE KEY FILTER              
AVALMTH  DS    A                   VALIDATE 'NEW MONTH='                        
         DS    3A                  N/D                                          
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
*                                                                               
VADDAY   DS    V                                                                
VCALLOVL DS    V                   NAMED NOT TO CLASH WITH SYSFACD              
VCASHVAL DS    V                                                                
VCUREDIT DS    V                                                                
CUREDIT  EQU   VCUREDIT            FOR USE IN CURED MACRO                       
VDMGR    DS    V                   NAMED NOT TO CLASH WITH SYSFACD              
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
VDICTATE DS    V                                                                
VGETFACT DS    V                                                                
VGETPROF DS    V                                                                
VGETTXT  DS    V                                                                
VHELLO   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VPERVAL  DS    V                                                                
VREPORT  DS    V                                                                
VSCANNER DS    V                                                                
VSWITCH  DS    V                                                                
VUNSCAN  DS    V                                                                
VACCEMU  DS    V                   FROM ACCFACS                                 
VSEXLST  DS    V                   SYSTEM EXECUTIVE LIST                        
         DS    A                   N/D                                          
VTSAR    DS    V                   TSAR IS CORERES                              
VOFFAL   DS    V                   OFFAL IS CORERES                             
VADDTRN  DS    V                   ADDTRN IS CORERES                            
         DS    A                   N/D                                          
*                                                                               
VCONVMOS DS    V                   RELOCATED V(CONVMOS)                         
VBMONVAL DS    V                   RELOCATED V(BMONVAL)                         
VCHOPPER DS    V                   RELOCATED V(CHOPPER)                         
VBINSRCH DS    V                   RELOCATED V(BINSRCH)                         
VGETIDS  DS    V                   RELOCATED V(GETIDS)                          
         DS    A                   N/D                                          
*                                                                               
ADISHEAD DS    A                   A(DISPLAY HEADS LINE)                        
ADISDET1 DS    A                   A(FIRST DETAIL LINE)                         
ADISDETL DS    A                   A(LAST DETAIL LINE) FOR TWAXC                
ADISTOTS DS    A                   A(TOTALS LINE)                               
ADISPFKS DS    A                   A(PFKEY LINE)                                
*                                                                               
AIOSAVE  DS    A                   A(SAVED D/A IODA/IOWORK)                     
AIOBUFF  DS    A                   A(DATA RECORD AREA)                          
*                                                                               
RECALDGT DS    A                   A(LEDGER TABLE ENTRY)                        
RECVALS  DS    0X                  RECORD VALUES                                
RECCPYEL DS    A                   A(COMPANY ELEMENT)                           
RECRSTEL DS    A                   A(STATUS ELEMENT)                            
RECABLEL DS    A                   A(ACCOUNT BALANCE ELEMENT)                   
RECPPREL DS    A                   A(PRODUCTION PROFILE ELEMENT)                
RECNAME  DS    CL(L'NAMEREC)       RECORD NAME                                  
RECOFFC  DS    CL(L'TRNOFFC)       OFFICE CODE                                  
RECBAL   DS    PL8                 BALANCE                                      
RECVALSL EQU   *-RECVALS                                                        
*                                                                               
AUIDNTRY DS    A                   A(CURRENT UIDTAB ENTRY)                      
ASCANTAB DS    A                   A(VALIDATION TABLE USED BY SCANFLD)          
*                                                                               
GUID     DS    0XL2                                                             
GUIDTAB  DS    32XL(L'GUID)        TEMP STORAGE FOR GROUP-ID TABLE              
GUIDTABL EQU   *-GUIDTAB                                                        
ELEMENT  DS    XL64                                                             
WORK     DS    XL64                                                             
TEMP     DS    XL256                                                            
SPACES   DS    CL256                                                            
APRGTENT DS    A                   A(PROGRAM ENTRY TYPE)                        
         DS    XL28                N/D                                          
*                                                                               
LARE     DS    H                   FOR EXECUTED LOAD ADDRESS                    
LAREADDR DS    S                                                                
LARF     DS    H                   FOR EXECUTED LOAD ADDRESS                    
LARFADDR DS    S                                                                
*                                                                               
FVADDR   DS    A                   A(INPUT FIELD HEADER) SET BY FLDVAL          
FVFLAG   DS    XL1                 FLAG BYTE USED BY FLDVAL ROUTINE             
FVHELP   DS    XL1                 HELP INDEX (ZERO=NO HELP AVAILABLE)          
FVMINL   DS    XL1                 MINIMUM INPUT LENGTH (ZERO=OPTIONAL)         
FVMAXL   DS    XL1                 MAXIMUM INPUT LENGTH (ZERO=MAXIMUM)          
FVNUMER  DS    XL1                 TREAT AS NUMERIC IF FOUND AS NUMERIC         
FVOSYS   DS    XL1                 OVERRIDE SYSTEM FOR GETTXT CALLS             
FVOMTYP  DS    XL1                 OVERRIDE MESSAGE TYPE FOR GETTXT             
FVMSGNO  DS    XL2                 MESSAGE NUMBER                               
         ORG   *-1                                                              
FVFERN   DS    XL1                                                              
FVINDX   DS    XL1                 MULTIPLE FIELD INDEX NUMBER                  
FVSUBX   DS    XL1                 MULTIPLE FIELD SUB-INDEX NUMBER              
FVCURS   DS    XL1                 DISPLACEMENT TO CURSOR WITHIN FIELD          
FVXTRA   DS    CL40                TACKED ON TO ERROR MESSAGE IF ROOM           
FVSUBT   DS    CL20                STRING REPLACES &1-&N IN MESSAGE             
FVIHDR   DS    0XL8                ** EXTRACTED INPUT FIELD HEADER **           
FVTLEN   DS    XL1                 L'FIELD HEADER+L'FIELD                       
FVATRB   DS    XL1                 FIELD ATTRIBUTE                              
FVAPROT  EQU   X'20'               PROTECTED FIELD                              
FVAZERO  EQU   X'0C'               ZERO INTENSITY                               
FVAHIGH  EQU   X'08'               HIGH INTENSITY                               
FVAXTND  EQU   X'02'               EXTENDED HEADER                              
FVAMODF  EQU   X'01'               MODIFIED INPUT FIELD                         
FVABSA   DS    XL2                 SCREEN ABSOLUTE ADDRESS                      
FVIIND   DS    XL1                 INPUT VALIDATION INDICATORS                  
FVITHIS  EQU   X'80'               FIELD INPUT THIS TIME                        
FVILAST  EQU   X'40'               FIELD HAS BEEN INPUT PREVIOUSLY              
FVIVAL   EQU   X'20'               FIELD HAS BEEN VALIDATED PREVIOUSLY          
FVIINV   EQU   X'10'               FIELD IS INVALID                             
FVINUM   EQU   X'08'               FIELD IS VALID NUMERIC                       
FVIALF   EQU   X'04'               FIELD IS VALID ALPHA                         
FVIHEX   EQU   X'02'               FIELD IS VALID HEXADECIMAL                   
FVILEN   DS    XL1                 ACTUAL INPUT DATA LENGTH                     
FVOIND   DS    XL1                 OUTPUT INDICATORS                            
FVOXMT   EQU   X'80'               TRANSMIT FIELD                               
FVOCUR   EQU   X'40'               INSERT CURSOR TO FIELD                       
FVXLEN   DS    XL1                 ACTUAL INPUT DATA LENGTH-1                   
FVIFLD   DS    CL80                EXTRACTED & SPACE FILLED INPUT FIELD         
FVOMSG   DS    CL60                OUTPUT MESSAGE MAY BE BUILT HERE             
*                                                                               
AGYOPTS  DS    XL1                 AGENCY OPTIONS                               
AGYCTRY  DS    XL1                 AGENCY COUNTRY                               
AGYLANG  DS    XL1                 AGENCY LANGUAGE                              
AGYCURR  DS    CL3                 AGENCY CURRENCY                              
*                                                                               
COMPANY  DS    XL1                 COMPANY FROM FACPAK PARAMETER LIST           
SENO     DS    XL1                 PHYSICAL SYSTEM NUMBER                       
PGNO     DS    XL1                 CONNECTED PROGRAM NUMBER                     
USER     DS    H                   CONNECTED USER ID NUMBER                     
UIDNAME  DS    CL6                 CONNECTED USER ID NAME                       
LUID     DS    CL8                 CONNECTED USER LUID                          
*                                                                               
USERSTS  DS    XL1                 CONNECTED USER STATUS                        
UPDATEQ  EQU   X'01'               USER CAN UPDATE                              
DELETEQ  EQU   X'02'               USER CAN DELETE APE FILES                    
SUSRIDQ  EQU   X'04'               SUBSIDIARY AGENCY USER                       
PUSRIDQ  EQU   X'08'+SUSRIDQ       PRINCIPAL AGENCY USER                        
DDSUSRQ  EQU   X'10'+PUSRIDQ+DELETEQ+UPDATEQ  DDS USER                          
IAMGODQ  EQU   X'20'+DDSUSRQ       DATA CONTROL/PRIVILEGED USER                 
*                                                                               
ANYADD   DS    XL1                 0/1 = NO/TSAR RECORD(S) ADDED                
*                                                                               
MXMNTH   DS    XL1                 MIXED POSTING MONTHS INDICATOR               
SPALF    DS    CL2                 ALPHAID OF INPUT UID                         
SPUID    DS    XL2                 UID NUMBER OF INPUT UID                      
*                                                                               
EXITFLAG DS    XL1                                                              
UPDERROR EQU   1                   ERROR IN UPDATE PROCESS                      
GTMAXIOS EQU   2                   IO COUNT BLOWN                               
*                                                                               
DISRMAX  DS    H                   HIGHEST TSAR/WORKER REC FOR DISPLAY          
TSTRNLO  DS    H                   LOWEST TSAR TRANS REC NUMBER                 
TSTRNHI  DS    H                   HIGHEST TSAR TRANS REC NUMBER                
WKBDSP   DS    F                   DISP TO SAVE AREA IN WORKER BUFFER           
*                                                                               
UIDTCNT  DS    A                   BINSRCH COUNT (YES, IT'S AN A-TYPE)          
*                                                                               
SAVEACC  DS    CL(L'ACTKCULA)      SAVED ACC FOR ONLINE UPDATE                  
SAVESIN  DS    XL3                 SAVED SYSTEM INPUT NO. FOR UPDATES           
SAVEDDQ  DS    XL2                 SAVED PROGRAM NAME DD EQUATE NO.             
*                                                                               
WKLDGDR  DS    PL8                 LEDGER, UNIT, FILE TOTALS                    
WKLDGCR  DS    PL8                                                              
WKUNTDR  DS    PL8                                                              
WKUNTCR  DS    PL8                                                              
WKFILDR  DS    PL8                                                              
WKFILCR  DS    PL8                                                              
*                                                                               
EDIT1    DS    PL8                 WORK ACCUMULATORS                            
EDIT2    DS    PL8                                                              
EDIT3    DS    PL8                                                              
*                                                                               
WKID     DS    0XL16               ** WORKER FILE INDEX **                      
WKIKEY   DS    0XL9                WORKER KEY                                   
WKIUSER  DS    XL2                 USER-ID NUMBER                               
WKISPRG  DS    0CL3                                                             
WKISYS   DS    CL1                 SYSTEM CODE                                  
WKISACC  EQU   C'A'                ACCOUNTING                                   
WKISFEE  EQU   C'F'                FEE                                          
WKISMED  EQU   C'M'                MEDIA                                        
WKISNET  EQU   C'N'                NETPAK                                       
WKISPRT  EQU   C'P'                SPOT                                         
WKISSPT  EQU   C'S'                PRINT                                        
WKISTLT  EQU   C'T'                TALENT                                       
WKIPRG   DS    CL2                 PROGRAM ID                                   
WKISUB   DS    XL1                 SUB-PROGRAM/LEDGER CODE                      
WKIDAY   DS    PL1                 DAY ADDED (PWOS DD)                          
WKICLASS DS    CL1                 FILE TYPE                                    
WKITPOST EQU   C'P'                POSTING FILE                                 
WKITODDS EQU   C'O'                ACODDS FILE                                  
WKITCHQS EQU   C'C'                CHEQUE FILE                                  
SKIEXTRA DS    XL1                 EXTRA                                        
         DS    XL1                 N/D                                          
WKISEQN  DS    XL2                 FILE SEQUENCE NUMBER                         
WKISTAT  DS    XL1                 WORKER STATUS                                
WKISACTV EQU   X'80'               STATUS ACTIVE                                
WKISHOLD EQU   X'40'               STATUS HOLD                                  
WKISDELT EQU   X'20'               STATUS DELETE                                
WKISKEEP EQU   X'08'               STATUS KEEP                                  
WKIFLAG  DS    XL1                 WORKER FLAG VALUES                           
WKIDUPS  EQU   X'01'               ALLOW DUPLICATE FILES                        
         DS    XL15                N/D                                          
*                                                                               
WKID2    DS    0XL16               ** WORKER FILE INDEX 2 **                    
WKIKEY2  DS    0XL9                WORKER KEY 2                                 
WKIUSER2 DS    XL2                 USER-ID NUMBER 2                             
WKISPRG2 DS    0CL3                                                             
WKISYS2  DS    CL1                 SYSTEM CODE                                  
WKIPRG2  DS    CL2                 PROGRAM ID                                   
WKISUB2  DS    XL1                 SUB-PROGRAM/LEDGER CODE                      
WKIDAY2  DS    PL1                 DAY ADDED (PWOS DD)                          
WKICLAS2 DS    CL1                 FILE TYPE                                    
WKIEXTR2 DS    XL1                 EXTRA                                        
         DS    XL1                 N/D                                          
WKISEQN2 DS    XL2                 FILE SEQUENCE NUMBER                         
WKISTAT2 DS    XL1                 WORKER STATUS                                
WKIFLAG2 DS    XL1                 WORKER FLAG VALUES                           
         DS    XL15                N/D                                          
*                                                                               
         EJECT                                                                  
TSARREC  DS    0H                  ** TSAR RECORD **                            
*                                  ** KEY **                                    
TSARKEY  DS    0X                                                               
TSARKNO  DS    XL2                 KEY SEQUENCE NO                              
TSARWKEY DS    0XL(L'UKBKEY)       WORKER KEY                                   
TSARKUID DS    XL(L'UKUSRID)                                                    
TSARKSPG DS    0XL(L'UKSYSPRG)                                                  
TSARKSYS DS    XL(L'WKISYS)                                                     
TSARKPRG DS    XL(L'WKIPRG)                                                     
TSARKSUB DS    XL(L'UKSUBPRG)                                                   
TSARKDAY DS    XL(L'UKDAY)                                                      
TSARKTYP DS    XL(L'UKCLASS)                                                    
TSARKXTR DS    XL(L'UKEXTRA)                                                    
TSARKSEQ DS    XL(L'UKFILNO)       SEQUENCE NO                                  
         ORG   TSARWKEY                                                         
TSARTKEY DS    0X                  TRANSACTION KEY                              
TSARTACC DS    XL(L'PSHDACC)                                                    
TSARTANL DS    XL(L'PSHDANAL)                                                   
TSARTSBA DS    XL(L'PSHDSBAC)                                                   
TSARTRNO DS    XL4                 TRANS RECORD NUMBER IN WORKER FILE           
TSARKEYL EQU   *-TSARKEY                                                        
*                                  ** DATA **                                   
TSARDATA DS    0X                                                               
TSARDSTS DS    XL(L'WKISTAT)       STATUS                                       
TSARDDAT DS    XL(L'UKSOFAGD)      AGED DATE                                    
TSARDTIM DS    XL(L'UKSOFAGT)      AGED TIME                                    
TSARDUNM DS    XL(L'LC@UID)        USER NAME                                    
TSARDFNM DS    XL(PRGTNAML)        FILE NAME                                    
TSARDCNT DS    PL6                 RECORD COUNT (PSSBRECS)                      
TSARDCSH DS    PL6                 TOTAL CASH (PSSBCASH)                        
TSARSENO DS    XL(L'UIDTSENO)      PHYSICAL SYSTEM NUMBER                       
TSARIND1 DS    XL1                                                              
TSARMKQ  EQU   X'80'               USER HAS MARKED RECORD                       
TSARDELQ EQU   X'40'               FILE MAY BE DELETED BY NON-DDS USER          
TSARUPDQ EQU   X'20'               FILE MAY BE UPDATED BY NON-DDS USER          
TSARCHXQ EQU   X'10'               CHEQUE CHEQUES FILE                          
TSARCHPQ EQU   X'08'               CHEQUE POSTING FILE                          
TSWKBADQ EQU   X'04'               FILE IS CORRUPT/INCOMPLETE                   
TSWKPRGQ EQU   X'02'               FILE HAS BEEN PURGED                         
TSWKREVQ EQU   X'01'               POSTINGS HAVE BEEN REVERSED                  
TSARIND2 DS    XL1                                                              
TSARDISQ EQU   X'80'               RECORD IS FOR DISPLAY ONLY                   
TSARNCTQ EQU   X'40'               WKR TRAILER REC NOT YET READ                 
TSARNORV EQU   X'20'               FILE CANNOT BE REVERSED                      
TSARNOAC EQU   X'10'               NO ACTIONS ON THIS FILE (LOOK ONLY)          
TSARICOP EQU   X'08'               FILE MAY BE COPIED                           
TSWKCOPQ EQU   X'04'               FILE IS A COPY                               
         ORG   TSARDATA                                                         
TSARTDAB DS    XL4                 D/A OF BLOCK CONTAINING WORKER REC           
TSARTDSP DS    H                   DISPLACEMNT INTO BLOCK OF WORKER REC         
         ORG                                                                    
TSARRECL EQU   *-TSARREC           RECORD LENGTH                                
*                                                                               
SCRVALS  DS    0X                  ** SCROLL CONTROL **                         
SCRLINDS DS    CL1                 SCROLL INDICATORS                            
SCRLPAGE EQU   X'80'               SCROLL A PAGE                                
SCRLHALF EQU   X'40'               SCROLL HALF A PAGE                           
SCRLMAXI EQU   X'20'               SCROLL MAXIMUM                               
SCRLDOWN EQU   X'08'               SCROLL DOWN                                  
SCRLUP   EQU   X'04'               SCROLL UP                                    
SCRVALSL EQU   *-SCRVALS                                                        
*                                                                               
KEY      DS    XL(ACCKLEN)                                                      
KEYSAVE  DS    XL(ACCKLEN)                                                      
*                                                                               
SRCWORK  DS    CL(L'ACTKCULA-1)    SOURCE ULACC WORK AREA                       
*                                                                               
SCANOUT  DS    XL(SCANLTAB)                                                     
*                                                                               
SCIFLTS  DS    0X                  SCANNED INPUT FILTERS (KEY OR OPT)           
SCIUSER  DS    XL(L'UKUSRID)       USER-ID                                      
SCIKEY   DS    0X                  WORKER FILE FILTER 'KEY'                     
SCISYS   DS    XL(L'WKISYS)                                                     
SCIPRG   DS    XL(L'WKIPRG)                                                     
SCISUB   DS    XL(L'UKSUBPRG)                                                   
SCIDAY   DS    XL(L'UKDAY)                                                      
SCIWTYP  DS    XL(L'UKCLASS)                                                    
SCIEXTRA DS    XL(L'UKEXTRA)                                                    
SCISSEQ  DS    XL(L'UKFILNO)       START SEQUENCE NUMBER                        
SCIESEQ  DS    XL(L'UKFILNO)       END SEQUENCE NUMBER                          
SCIKEYL  EQU   *-SCIKEY                                                         
SCISDAT  DS    XL(L'UKSOFAGD)      START DATE ADDED                             
SCIEDAT  DS    XL(L'UKSOFAGD)      END DATE ADDED                               
SCIDATSL EQU   *-SCISDAT                                                        
SCIFSTS  DS    XL1                 FILE STATUS                                  
SCIFSBC  DS    XL1                 FILE STATUS 0=INCLUDE,1=EXCLUDE              
SCISENO  DS    XL1                 ACCOUNT SYSTEM NUMBER                        
SCICHQS  DS    XL1                 'Y'=INCLUDE CHEQUE FILES                     
*                                  END OF KEY FILTERS                           
SCIACC   DS    XL(L'TRNKCULA-1)    ACCOUNT                                      
SCIACCL  DS    XL1                 L'ACCOUNT                                    
SCICAC   DS    XL(L'TRNKCULA-1)    CONTRA A/C                                   
SCICACL  DS    XL1                 L'CONTRA A/C                                 
SCISIGN  DS    0CL1                D(EBIT)/C(REDIT)                             
SCIAMT   DS    PL(L'TRNAMNT)       AMOUNT                                       
SCIREF   DS    CL(L'TRNREF)                                                     
SCIREFL  DS    XL1                 L'REFERENCE                                  
SCIRDST  DS    XL(L'SCIUSER)       DESTINATION USER-ID FOR REPORT (OPT)         
SCIIND1  DS    XL1                                                              
SCIISNO  EQU   X'80'               INCLUDE SENO IN DISPLAY                      
SCIITOT  EQU   X'40'               DISPLAY FILE TOTALS (REPLACES NAME)          
SCIMTH   DS    0PL(L'TRNMOS)       NEW POSTING MONTH                            
SCISGN   DS    CL1                 SIGN OF INCREMENT                            
SCIINC   DS    XL1                 VALUE OF INCREMENT                           
SCIFLTSL EQU   *-SCIFLTS                                                        
*                                                                               
         DS    0D                                                               
IOCTRL   DS    0F                  CONTROL BYTE FOR IO CALLS                    
         DS    H                   N/D                                          
IOCTFILE DS    XL1                 FILE NUMBER                                  
IOCTCOMM DS    XL1                 COMMAND NUMBER                               
IOCTINDS DS    XL2                 I/O INDICATORS                               
IOCTIGET EQU   X'80'               GETREC                                       
IODA     DS    XL4                 DISK ADDRESS                                 
IOWORK   DS    XL96                DATAMGR D/A WORK AREA                        
IOFILE   DS    CL7                 IO FILE NAME                                 
IOCOMM   DS    CL7                 IO COMMAND                                   
IOQUAL   DS    XL1                 QUALIFIER FOR DATAMGR IO                     
IOQLOCK  EQU   X'80'               READ FOR UPDATE                              
IOQDELT  EQU   X'08'               READ DELETES                                 
IOERROR  DS    XL1                 ERROR RETURN BYTE                            
IOEEOF   EQU   X'80'               END OF FILE                                  
IOEDSK   EQU   X'40'               NON-RECOVERABLE DISK ERROR                   
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEDEL   EQU   X'02'               RECORD DELETED                               
IOEALL   EQU   X'FF'               ALL ERROR BITS (CONTROLLER USE ONLY)         
*                                                                               
IOAS     DS    0X                  IO AREAS 1/2/3                               
IOSAVE   DS    CL(L'IODA+L'IOWORK)                                              
IOBUFF   DS    XL2048              IO BUFFER 1                                  
IOALQ    EQU   *-IOAS                                                           
         DS    (IOALQ*2)X          IO AREAS 2/3                                 
IOAMAX   EQU   (*-IOAS)/IOALQ      MAX IO AREAS                                 
*                                                                               
OFFBLK   DS    XL(OFFALLEN)        OFFAL BLOCK                                  
TSARBLK  DS    XL(TSARDL2+(28*6))  TSAR BLOCK (LONGER FOR TSARKEY>32)           
TRNBLOCK DS    XL(TRNBLKL)         ADDTRN BLOCK                                 
         DS    0D                                                               
WKBUF1   DS    4096X               WORKER WORK BUFFER 1                         
WKBUF2   DS    4096X               WORKER WORK BUFFER 2                         
WKBUFL   EQU   *-WKBUF2                                                         
WKIO     DS    0X                  WORKER RECORD AREA                           
WKIOLN   DS    H                                                                
         DS    H                                                                
         DS    1000X                                                            
*                                                                               
PQBUFF   DS    16384X              PRINT QUEUE BUFFER AREA                      
WORKX    EQU   *                                                                
         EJECT                                                                  
* MISCELLANEOUS EQUATES                                                         
*                                                                               
INCLUDE  EQU   X'01'               OPTION IS INCLUDE                            
EXCLUDE  EQU   X'80'               OPTION IS EXCLUDE                            
EOT      EQU   0                   END OF TABLE                                 
ALL      EQU   X'FF'                                                            
*                                                                               
* KEY BUILD ROUTINE EQUATES                                                     
*                                                                               
SETACC   EQU   X'80'                                                            
SETOFF   EQU   X'40'                                                            
SETWRK   EQU   SETOFF                                                           
SETCON   EQU   X'20'                                                            
SETSDT   EQU   X'10'                                                            
SETREF   EQU   X'08'                                                            
SETALL   EQU   SETACC+SETOFF+SETCON+SETSDT+SETREF                               
NXTOFF   EQU   X'04'                                                            
NXTWRK   EQU   NXTOFF                                                           
NXTCON   EQU   X'02'                                                            
NXTSDT   EQU   X'01'                                                            
*                                                                               
* COMBINED DATE ASSUMPTION EQUATES                                              
*                                                                               
STARTASS EQU   PVALASD+PVALASM+PVALASY                                          
ENDASS   EQU   PVALAED+PVALAEM+PVALAEY                                          
*                                                                               
* PFKEY EQUATES                                                                 
*                                                                               
PFK01    EQU   1                                                                
PFK02    EQU   2                                                                
PFK03    EQU   3                                                                
PFK04    EQU   4                                                                
PFK05    EQU   5                                                                
PFK06    EQU   6                                                                
PFK07    EQU   7                                                                
PFK08    EQU   8                                                                
PFK09    EQU   9                                                                
PFK10    EQU   10                                                               
PFK11    EQU   11                                                               
PFK12    EQU   12                                                               
PFK13    EQU   13                                                               
         EJECT                                                                  
* IO ROUTINE EQUATES                                                            
*                                                                               
IOFILES  EQU   X'0F00'             RESERVED FOR CONTROLLER USE                  
IOACCDIR EQU   X'0100'             I/O TO ACCOUNT DIRECTORY                     
IOACCMST EQU   X'0200'             I/O TO ACCOUNT MASTER FILE                   
IOACCARC EQU   X'0300'             I/O TO ACCOUNT ARCHIVE FILE                  
IOACCIS  EQU   X'01'               INDEX SEQUENTIAL IO FILE                     
IOACCDA  EQU   X'02'               DIRECT ACCESS IO FILE                        
         SPACE 1                                                                
*                                  ** I/O AREA EQUATES **                       
IO1Q     EQU   X'0040'             I/O TO AREA 1                                
IO2Q     EQU   X'0080'             I/O TO AREA 2                                
IO3Q     EQU   X'00C0'             I/O TO AREA 3                                
         SPACE 1                                                                
*                                  ** I/O COMMAND EQUATES **                    
IOCMNDS  EQU   X'000F'             RESERVED FOR CONTROLLER USE                  
*                                  COMMAND QUALIFIERS                           
IOLOCK   EQU   X'0010'             READ FOR UPDATE                              
IORDEL   EQU   X'0020'             READ DELETED RECORDS                         
*                                  I/S COMMANDS                                 
IOHIGH   EQU   X'0001'             DMRDHI                                       
IOHI     EQU   IOHIGH              DMRDHI                                       
IOHID    EQU   IOHI+IORDEL         DMRDHI (FOR DELETES)                         
IOHIUP   EQU   IOHI+IOLOCK         DMRDHI (FOR UPDATE)                          
IOHIUPD  EQU   IOHI+IOLOCK+IORDEL  DMRDHI (FOR UPDATE & DELETES)                
IOREAD   EQU   X'0002'             DMREAD                                       
IORD     EQU   IOREAD              DMREAD                                       
IORDD    EQU   IORD+IORDEL         DMREAD (FOR DELETES)                         
IORDUP   EQU   IORD+IOLOCK         DMREAD (FOR UPDATE)                          
IORDUPD  EQU   IORD+IOLOCK+IORDEL  DMREAD (FOR UPDATE & DELETES)                
IOSEQ    EQU   X'0003'             DMRSEQ                                       
IOSEQD   EQU   IOSEQ+IORDEL        DMRSEQ (FOR DELETES)                         
IOSEQUP  EQU   IOSEQ+IOLOCK        DMRSEQ (FOR UPDATE)                          
IOSEQUPD EQU   IOSEQ+IOLOCK+IORDEL DMRSEQ (FOR UPDATE & DELETES)                
IOADD    EQU   X'0004'             DMADD                                        
IOWRITE  EQU   X'0005'             DMWRT                                        
*                                  D/A COMMANDS                                 
IOGET    EQU   X'0001'             GETREC                                       
IOGETRUP EQU   IOGET+IOLOCK        GETREC FOR UPDATE                            
IOPUTREC EQU   X'0002'             PUTREC                                       
IOPUT    EQU   IOPUTREC            PUTREC                                       
IOADDREC EQU   X'0003'             ADDREC                                       
IOADFR   EQU   X'0004'             ADFREC (MOVE FROM ACCARC TO ACCMST)          
         EJECT                                                                  
* ERROR, INFORMATION, REPORT AND SCREEN MESSAGE EQUATES                         
*                                                                               
EGACTNOR EQU   011+X'FF00'         ACTION NOT RECOGNISED                        
EGIFMISS EQU   200+X'FF00'         NO DATA INPUT                                
EGIFSHRT EQU   201+X'FF00'         NOT ENOUGH DATA INPUT                        
EGIFLONG EQU   202+X'FF00'         TOO MUCH DATA INPUT                          
EGIFNOTV EQU   203+X'FF00'         INVALID INPUT FIELD                          
EGOPTDUP EQU   208+X'FF00'         DUPLICATED OPTION KEYWORD                    
EGINVASQ EQU   212+X'FF00'         INVALID ACTION SEQUENCE                      
EGRECNOF EQU   213+X'FF00'         RECORD NOT ON FILE                           
EGRECAOF EQU   214+X'FF00'         RECORD ALREADY ON FILE                       
EGDATINV EQU   233+X'FF00'         INVALID DATE                                 
EGIFDUPL EQU   237+X'FF00'         DUPLICATED INPUT FIELD                       
*                                                                               
IGOK     EQU   255+X'FF00'         FIELD IS VALID                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
SAVED    DSECT                     ** SAVED STORAGE **                          
DISVALS  DS    0X                  ** INPUT DISPLAY VALUES **                   
DISTMAX  DS    H                   HIGHEST TSAR RECORD NUMBER                   
DISWMAX  DS    H                   HIGHEST WORKER RECORD NUMBER                 
DISNUM   DS    H                   CURRENT RECORD NUMBER                        
DISLCNT  DS    H                   COUNT OF LINES DISPLAYED ON SCREEN           
DISLIST  DS    20H                 RECS DISPLAYED/TO BE DISPLAYED               
DISLISTL EQU   *-DISLIST                                                        
DISLINES DS    H                   N'DISPLAY LINES ON SCREEN                    
DISLINEL DS    H                   L'SCREEN DISPLAY LINE (VARIABLE)             
DISSCRNL DS    H                   L'PHYSICAL SCREEN LINE                       
DISDISD  DS    H                   DISP INTO DISLINE FOR OUTPUT                 
DISHEAD  DS    CL(L'DISLDLIN)      LIST/DETAIL DISPLAY HEADING                  
DISCOLS  DS    XL(L'SCRCOLS)       COLUMN NUMBERS LIST                          
DISIND   DS    XL1                 DISPLAY INDICATORS                           
DISIRST  EQU   X'80'               RESTART FROM BEGINNING                       
DISIBOF  EQU   X'40'               INDICATES BOF                                
DISIEOF  EQU   X'20'               INDICATES EOF                                
DISIOFLO EQU   X'10'               ALLOW/INDICATE OVERFLOW                      
DISINOSC EQU   X'08'               NO SCROLL, RE-DISPLAY SAME PAGE              
DISIFFLT EQU   X'04'               FORCE RE-FILTERING OF DISLIST                
DISIMMSG EQU   X'02'               DISPLAY MESSAGE IN FVMSGNO                   
*                                                                               
KEYFLTS  DS    0X                  ** KEY INPUT FILTER VALUES **                
KEYUSER  DS    XL(L'UKUSRID)       USER-ID                                      
KEYKEY   DS    0X                  FILTER 'KEY'                                 
KEYSYS   DS    XL(L'WKISYS)                                                     
KEYPRG   DS    XL(L'WKIPRG)                                                     
KEYSUB   DS    XL(L'UKSUBPRG)                                                   
KEYDAY   DS    XL(L'UKDAY)                                                      
KEYTYPE  DS    XL(L'UKCLASS)                                                    
KEYEXTRA DS    XL(L'UKEXTRA)                                                    
KEYSSEQ  DS    XL(L'UKFILNO)       START SEQUENCE NUMBER                        
KEYESEQ  DS    XL(L'UKFILNO)       END SEQUENCE NUMBER                          
KEYSEQSL EQU   *-KEYSSEQ                                                        
KEYKEYL  EQU   *-KEYKEY                                                         
KEYSDAT  DS    XL(L'UKSOFAGD)      START DAY ADDED                              
KEYEDAT  DS    XL(L'UKSOFAGD)      END DAY ADDED                                
KEYDATSL EQU   *-KEYSDAT                                                        
KEYFSTS  DS    XL1                 FILE STATUS                                  
KEYFSBC  DS    XL1                 FILE STATUS                                  
KEYSENO  DS    XL1                 ACCOUNT SYSTEM NUMBER                        
KEYCHQS  DS    XL1                 'Y'=INCLUDE CHEQUE FILES                     
KEYFLTSL EQU   *-KEYFLTS                                                        
*                                                                               
SGUIDTAB DS    XL(GUIDTABL)        SAVED GROUPID ID LIST                        
*                                                                               
OPTFLTS  DS    0X                  ** USER OPTIONS **                           
OPTUSER  DS    XL(L'UKUSRID)       USER-ID                                      
OPTKEY   DS    0X                  DISPLAY FILTER 'KEY'                         
OPTSYS   DS    XL(L'WKISYS)                                                     
OPTPRG   DS    XL(L'WKIPRG)                                                     
OPTSUB   DS    XL(L'UKSUBPRG)                                                   
OPTDAY   DS    XL(L'UKDAY)                                                      
OPTTYPE  DS    XL(L'UKCLASS)                                                    
OPTEXTRA DS    XL(L'UKEXTRA)                                                    
OPTSSEQ  DS    XL(L'UKFILNO)       START SEQUENCE NUMBER                        
OPTESEQ  DS    XL(L'UKFILNO)       END SEQUENCE NUMBER                          
OPTKEYL  EQU   *-OPTKEY                                                         
OPTSDAT  DS    XL(L'UKSOFAGD)      START DAY ADDED                              
OPTEDAT  DS    XL(L'UKSOFAGD)      END DAY ADDED                                
OPTFSTS  DS    XL1                 FILE STATUS                                  
OPTFSBC  DS    XL1                 FILE STATUS                                  
OPTSENO  DS    XL1                 ACCOUNT SYSTEM NUMBER                        
OPTCHQS  DS    XL1                 'Y'=INCLUDE CHEQUE FILES                     
OPTACC   DS    XL(L'TRNKCULA-1)    ACCOUNT                                      
OPTACCL  DS    XL1                 L'INPUT ACCOUNT                              
OPTCAC   DS    XL(L'TRNKCULA-1)    CONTRA A/C                                   
OPTCACL  DS    XL1                 L'INPUT CONTRA A/C                           
OPTSIGN  DS    0CL1                D(EBIT)/C(REDIT)                             
OPTAMT   DS    PL(L'TRNAMNT)                                                    
OPTREF   DS    CL(L'TRNREF)                                                     
OPTREFL  DS    XL1                                                              
OPTFLTSL EQU   *-OPTFLTS           LENGTH FOR COMPARE                           
OPTRDST  DS    XL(L'OPTUSER)       DESTINATION USER-ID FOR REPORT               
OPTIND1  DS    XL1                 DISPLAY OPTIONS INDICATOR                    
OPTMTH   DS    0PL(L'TRNMOS)       NEW POSTING MONTH                            
OPTSGN   DS    CL1                 SIGN OF INCREMENT                            
OPTINC   DS    XL1                 VALUE OF INCREMENT                           
OPTSL    EQU   *-OPTFLTS                                                        
OPTRUSR  DS    XL7                 DESTINATION USER-ID NAME                     
*                                                                               
PRTSUB   DS    CL3                 REPORT-ID FOR ONLINE JOURNAL                 
*                                                                               
*                                                                               
DISDISP  DS    0X                  ** DISPLAY ELEMENT DISPLACEMENTS **          
DISACCD  DS    XL1                 ACCOUNT (DETAIL)                             
DISAMTD  DS    XL1                 AMOUNT (DETAIL)                              
DISBRFD  DS    XL1                 BATCH REF (DETAIL)                           
DISCACD  DS    XL1                 CONTRA AC (DETAIL)                           
DISCODD  DS    XL1                 CODES (DETAIL)                               
DISDATD  DS    XL1                 DATE (LIST & DETAIL)                         
DISMTHD  DS    XL1                 MONTH (DETAIL)                               
DISNAMD  DS    XL1                 FILE NAME (LIST)                             
DISRCSD  DS    XL1                 RECORDS (LIST)                               
DISREFD  DS    XL1                 REFERENCE (DETAIL)                           
DISSTSD  DS    XL1                 STATUS (LIST)                                
DISTIMD  DS    XL1                 TIME (LIST)                                  
DISUIDD  DS    XL1                 USER-ID (LIST)                               
DISWKYD  DS    XL1                 WORKER KEY (LIST)                            
DISDISPL EQU   *-DISDISP                                                        
*                                                                               
STSARNUM DS    H                   SAVED TSAR RECORD NUMBER                     
STSARKNO DS    H                   SAVED TSAR KEY SEQUENCE NUMBER               
STSARUID DS    XL2                 SAVED TSAR WORKER REC USER-ID                
SACTADR  DS    H                   SAVED DISP TO ACTION FIELD                   
STSARSTT DS    XL1                 SAVED TSAR STAT (VARIOUS)                    
SDISLIST DS    XL(DISLISTL)        SAVED DISLIST FOR SCREEN SWAP                
SDISLCNT DS    XL(L'DISLCNT)       SAVED COUNT OF LINES DISPLAYED               
SLOPTS   DS    XL(OPTSL)           SAVED LIST SCREEN OPTIONS                    
*                                                                               
TODAYC   DS    XL2                 TODAY'S DATE COMPRESSED 'YYMMDD'             
TODAYB   DS    XL3                 TODAY'S DATE B'YYMMDD'                       
PERSTA   DS    CL(L'TRNKDATE)      PWOS START DATE                              
PEREND   DS    CL(L'TRNKDATE)      PWOS END DATE                                
ADASTA   DS    XL(L'TRSDATE)       COMPRESSED START DATE                        
ADAEND   DS    XL(L'TRSDATE)       COMPRESSED END DATE                          
*                                                                               
COMPVALS DS    0X                  ** EXTRACTED COMPANY VALUES **               
COMPUID  DS    XL(L'CPYUID)        PRINCIPAL USER-ID                            
COMPALFA DS    CL(L'CPYALPHA)      ALPHA CODE                                   
COMPSTA1 DS    CL(L'CPYSTAT1)      1ST STATUS BYTE                              
COMPSTA2 DS    CL(L'CPYSTAT2)      2ND STATUS BYTE                              
COMPSTA3 DS    CL(L'CPYSTAT3)      3RD STATUS BYTE                              
COMPSTA4 DS    CL(L'CPYSTAT4)      4TH STATUS BYTE                              
COMPSTA5 DS    CL(L'CPYSTAT5)      5TH STATUS BYTE                              
COMPSTA6 DS    CL(L'CPYSTAT6)      6TH STATUS BYTE                              
COMPSTA7 DS    CL(L'CPYSTAT7)      7TH STATUS BYTE                              
COMPSTA8 DS    CL(L'CPYSTAT8)      8TH STATUS BYTE                              
COMPSTA9 DS    CL(L'CPYSTAT9)      9TH STATUS BYTE                              
COMPSTAA DS    CL(L'CPYSTATA)      10TH STATUS BYTE                             
COMPGMOA DS    CL(L'CPYGLMOA)      GL MOA                                       
*                                                                               
FILEFORM DS    XL1                 FILE FORMAT                                  
VLISQ    EQU   1                   VARIABLE LENGTH INDEXED SEQUENTIAL           
ISDAQ    EQU   2                   INDEXED SEQUENTIAL/DIRECT ACCESS             
*                                                                               
PFKPEND  DS    XL1                 SAVED PFKEY VALUE                            
KEYIND   DS    XL1                                                              
KEYCHNG  EQU   1                   INPUT KEY HAS CHANGED                        
WKRNO    DS    H                   SAVED WKR POSTING NUMBER FOR HEXDISP         
HEXDREGS DS    0XL16                                                            
HEXDWR3  DS    F                   SAVE HEXDISP WORK R3                         
HEXDWR4  DS    F                   SAVE HEXDISP WORK R4                         
HEXDARE  DS    F                   SAVE HEXDISP ADDR RE                         
HEXDARF  DS    F                   SAVE HEXDISP ADDR RF                         
*                                                                               
SWKAREA  DS    XL20                SAVE AREA FOR WORKER'S VITAL BITS            
*                                                                               
DSLISTU  DS    0C                                                               
AC8ACC   DS    CL8                                                              
AC3ACC   DS    CL3                                                              
AC@ACTV  DS    CL8                                                              
AC3ACTV  DS    CL3                                                              
AC@ALL   DS    CL8                                                              
AC8AMT   DS    CL8                                                              
AC3AMT   DS    CL3                                                              
AC@AWP   DS    CL11                                                             
AC8CHQS  DS    CL8                                                              
AC3CHQS  DS    CL3                                                              
AC8CAC   DS    CL8                                                              
AC3CAC   DS    CL3                                                              
AC@COPY  DS    CL8                                                              
AC3COPY  DS    CL3                                                              
AC8CRPT  DS    CL8                                                              
AC2CR    DS    CL2,CL1                                                          
AC8DATE  DS    CL8                                                              
AC3DATE  DS    CL3                                                              
AC2DR    DS    CL2,CL1                                                          
AC@DELT  DS    CL8                                                              
AC3DELT  DS    CL3                                                              
AC8DEST  DS    CL8                                                              
AC3DEST  DS    CL3                                                              
AC@HOLD  DS    CL8                                                              
AC8ID    DS    CL8                                                              
AC3ID    DS    CL3                                                              
AC8IDALL DS    CL8                                                              
AC3IDALL DS    CL3                                                              
AC8IDGRP DS    CL8                                                              
AC3IDGRP DS    CL3                                                              
AC8IDUSR DS    CL8                                                              
AC3IDUSR DS    CL3                                                              
AC@KEEP  DS    CL8                                                              
AC3KEEP  DS    CL3                                                              
AC8KEY   DS    CL8                                                              
AC3KEY   DS    CL3                                                              
AC@NO    DS    CL4                                                              
AC8NMOA  DS    CL8                                                              
AC3NMOA  DS    CL3                                                              
AC@ONLY  DS    CL2,CL1                                                          
AC@PFK   DS    CL3                                                              
AC3PRNT  DS    CL3                                                              
AC8REF   DS    CL8                                                              
AC3REF   DS    CL3                                                              
AC@REVS  DS    CL8                                                              
AC3REVS  DS    CL3                                                              
AC3SELC  DS    CL3                                                              
AC8STS   DS    CL8                                                              
AC3STS   DS    CL3                                                              
AC8SYS   DS    CL8                                                              
AC3SYS   DS    CL3                                                              
AC8TOT   DS    CL8                                                              
AC3TOT   DS    CL3                                                              
AC@TRCE  DS    CL8                                                              
AC3TRCE  DS    CL3                                                              
AC@UPDT  DS    CL8                                                              
AC3UPDT  DS    CL3                                                              
AC@WFL   DS    CL11                                                             
AC@YES   DS    CL4                                                              
*                                                                               
DSLISTL  DS    0C                                                               
LC@ACC   DS    CL14                                                             
LC4ACTV  DS    CL4                                                              
LC@ALTPF DS    CL6                                                              
LC@AMT   DS    CL14                                                             
LC@AWP   DS    CL11                                                             
LC@BRF   DS    CL4                                                              
LC@CTO   DS    CL16                                                             
LC@CDS   DS    CL5                                                              
LC@COPFR DS    CL11                                                             
LC@COPTO DS    CL9                                                              
LC@CAC   DS    CL14                                                             
LC@DAT   DS    CL8                                                              
LC4DELT  DS    CL4                                                              
LC@END   DS    CL8                                                              
LC@FRST  DS    CL8                                                              
LC@HALF  DS    CL8                                                              
LC4HOLD  DS    CL4                                                              
LC4KEEP  DS    CL4                                                              
LC@LAST  DS    CL8                                                              
LC@MOS   DS    CL5                                                              
LC@NAM   DS    CL21                                                             
LC@PAGE  DS    CL8                                                              
LC@PRNT  DS    CL8                                                              
LC@PGA08 DS    CL30                                                             
LC@PGA21 DS    CL30                                                             
LC@PGA25 DS    CL30                                                             
LC@PGA90 DS    CL30                                                             
LC@PGA91 DS    CL30                                                             
LC@PGA98 DS    CL30                                                             
LC@PGA55 DS    CL30                                                             
LC@PGA5P DS    CL30                                                             
LC@PGA23 DS    CL30                                                             
LC@PGA05 DS    CL30                                                             
LC@PGA07 DS    CL30                                                             
LC@PGA09 DS    CL30                                                             
LC@PGA47 DS    CL30                                                             
LC@PGA8A DS    CL30                                                             
LC@PGAA1 DS    CL30                                                             
LC@PGAA2 DS    CL30                                                             
LC@PGAA8 DS    CL30                                                             
LC@PGAAJ DS    CL30                                                             
LC@PGAAT DS    CL30                                                             
LC@PGACE DS    CL30                                                             
LC@PGADB DS    CL30                                                             
LC@PGADC DS    CL30                                                             
LC@PGADD DS    CL30                                                             
*&&UK                                                                           
LC@PGADL DS    CL30                                                             
LC@PGADM DS    CL30                                                             
LC@PGATU DS    CL30                                                             
*&&                                                                             
LC@PGADS DS    CL30                                                             
LC@PGADT DS    CL30                                                             
LC@PGADU DS    CL30                                                             
LC@PGADV DS    CL30                                                             
LC@PGADW DS    CL30                                                             
LC@PGAD0 DS    CL30                                                             
LC@PGAD1 DS    CL30                                                             
LC@PGAD2 DS    CL30                                                             
LC@PGAD4 DS    CL30                                                             
LC@PGAD5 DS    CL30                                                             
LC@PGAFA DS    CL30                                                             
LC@PGAF2 DS    CL30                                                             
LC@PGAHP DS    CL30                                                             
LC@PGAPA DS    CL30                                                             
LC@PGAPE DS    CL30                                                             
LC@PGAT1 DS    CL30                                                             
LC@PGAT4 DS    CL30                                                             
LC@PGAT5 DS    CL30                                                             
LC@PGF0C DS    CL30                                                             
LC@PGM09 DS    CL30                                                             
LC@PGM10 DS    CL30                                                             
LC@PGM14 DS    CL30                                                             
LC@PGM15 DS    CL30                                                             
LC@PGM50 DS    CL30                                                             
LC@PGMAP DS    CL30                                                             
LC@PGMD4 DS    CL30                                                             
LC@PGMD7 DS    CL30                                                             
LC@PGMDA DS    CL30                                                             
LC@PGMH4 DS    CL30                                                             
LC@PGS54 DS    CL30                                                             
LC@PGP54 DS    CL30                                                             
LC@PGSBA DS    CL30                                                             
LC@PGPBA DS    CL30                                                             
LC@PGNBA DS    CL30                                                             
LC@PGS03 DS    CL30                                                             
LC@PGP05 DS    CL30                                                             
LC4PRGE  DS    CL4                                                              
LC@RCS   DS    CL5                                                              
LC@REF   DS    CL6                                                              
LC4REVS  DS    CL4                                                              
LC@RVRON DS    CL11                                                             
LC@SPCL  DS    CL8                                                              
LC@STS   DS    CL9                                                              
LC@TIM   DS    CL5                                                              
LC@TRX   DS    CL16                                                             
LC@TFWF  DS    CL30                                                             
LC@TOTL  DS    CL6                                                              
LC@UID   DS    CL8                                                              
LC@VENSF DS    CL30                                                             
LC@VENSP DS    CL30                                                             
LC@VENSQ DS    CL30                                                             
LC@VENSS DS    CL30                                                             
LC@VENST DS    CL30                                                             
LC@VENSU DS    CL30                                                             
LC@VENSV DS    CL30                                                             
LC@VENSW DS    CL30                                                             
LC@VENSX DS    CL30                                                             
LC@VENSY DS    CL30                                                             
LC@WFL   DS    CL11                                                             
LC@WKY   DS    CL12                                                             
LC@YES   DS    CL4                                                              
LC4COPY  DS    CL4                                                              
LC@ON    DS    CL3                 NEED TO ALLOW FOR 3 BYTES HERE               
LC@PGA27 DS    CL30                                                             
LC@PGA29 DS    CL30                                                             
LC@PGAIO DS    CL30                                                             
*                                                                               
PALAREA  DS    XL20                                                             
*                                                                               
SAVEDLN  EQU   *-SAVED                                                          
         EJECT                                                                  
APFTABD  DSECT                     ** ACTION PFKEY TABLE **                     
APFTPFK  DS    XL1                 PFKEY NUMBER                                 
APFTINDS DS    XL1                 ACTION INDICATORS                            
APFTLCAD DS    S                   ACTION SCON (LOWER CASE)                     
APFTABL  EQU   *-APFTABD                                                        
         SPACE 1                                                                
PFKTABD  DSECT                     ** PFKEY TABLE **                            
PFKTNUM  DS    XL1                 PFKEY NUMBER                                 
PFKTIND1 DS    XL1                 PFKEY INDS - SCROLL+SCREEN VALIDITY          
PFKTILST EQU   TWASCLST-X'FC'      PFKEY VALID ON LIST SCREEN ONLY              
PFKTIDET EQU   TWASCDET-X'FC'      PFKEY VALID ON DETAIL SCREEN ONLY            
PFKTIND2 DS    XL1                 SET TO USERSTS FOR RESTRICTED PFKS           
         DS    XL1                 SPARE                                        
PFKTLCAD DS    S                   SCROLL SCON FOR PFKEY LINE                   
PFKTACT  DS    AL2                 DISP TO ACTION PROCESS ROUTINE               
PFKTABL  EQU   *-PFKTABD                                                        
         SPACE 1                                                                
SCRTABD  DSECT                     ** LIST/DETAIL SCREEN ATTRIBUTES **          
SCRHEAD  DS    Y                   DISPLACEMENT TO DISPLAY HEADINGS             
SCRDISF  DS    Y                                   FIRST DISPLAY LINE           
SCRDISL  DS    Y                                   LAST DISPLAY LINE            
SCRTOTS  DS    Y                                   TOTALS LINE                  
SCRPFKS  DS    Y                                   PFKEY LINE                   
SCRDISD  DS    Y                                 INTO DISLINE OF DETLS          
SCRYTYPS EQU   (*-SCRHEAD)/L'SCRHEAD                                            
SCRLINS  DS    H                   N'DISPLAY LINES                              
SCRWIDT  DS    H                   WIDTH OF A DISPLAY LINE                      
SCRPHSW  DS    H                   WIDTH INCLUDING HEADERS                      
SCRCOLS  DS    XL10                DISPLAY COLUMN NUMBERS (SEE DISTAB)          
SCRCOLS2 DS    XL10                ALTERNATIVE COLUMN NUMBERS                   
SCRTABL  EQU   *-SCRTABD                                                        
         SPACE 1                                                                
DISTABD  DSECT                     ** DISPLAY COLUMN TABLE ENTRY **             
DISADDR  DS    S                   COLUMN HEADING SCON                          
DISLEN   DS    XL1                 LENGTH (WIDTH) OF COLUMN                     
DISVAL   DS    XL1                 VALIDITY BITS                                
LISTSCRQ EQU   X'80'               COLUMN IS VALID FOR LIST SCREEN              
DETLSCRQ EQU   X'40'               COLUMN IS VALID FOR DETAIL SCREEN            
DISSLVL  DS    XL1                 STATUS LEVEL - SEE USERSTS                   
         DS    XL1                 N/D                                          
DISTABL  EQU   *-DISTABD                                                        
         SPACE 1                                                                
DISLINED DSECT                     ** DISPLAY LINE **                           
DISLHDR1 DS    XL(L'FVIHDR)        ACTION HEADER                                
DISLACTN DS    CL1                 ACTION FIELD                                 
         DS    XL(L'FVIHDR)        ACTION EXTENDED HEADER                       
DISLCFMH DS    XL(L'FVIHDR)        CONFIRMATION HEADER                          
         DS    CL1                 CONFIRMATION FIELD                           
DISLHDR2 DS    XL(L'FVIHDR)        DESCRIPTION HEADER                           
DISLLLIN DS    CL74                LIST LINE DESCRIPTION (DYNAMIC)              
DISLLINL EQU   *-DISLINED                                                       
         ORG   DISLACTN                                                         
DISLDLIN DS    0CL78               DETAIL LINE                                  
DISLDBYT DS    CL11                'PFM'-TYPE DISPLAY LINE                      
         DS    CL3                                                              
DISLDHEX DS    CL40                                                             
         DS    CL4                                                              
DISLDCHR DS    CL20                                                             
DISLDLNL EQU   *-DISLINED                                                       
         SPACE 1                                                                
ACTTABD  DSECT                                                                  
ACTION   DS    S                   SHORT ACTION NAME SCON                       
ACTROUT  DS    AL2                 DISP INTO WFM CSECT OF ACTION ROUT           
ACTSLVL  DS    XL1                 ACTION STATUS LEVEL - SEE USERSTS            
ACTBCCD  DS    XL1                 BRANCH CC FOR 'TM TSARIND,ACTINDS'           
BZQ      EQU   X'80'                                                            
BNZQ     EQU   X'70'                                                            
ACTINDS  DS    XL1                 ACTVAL INDICATORS: TSARIND1/2 VALUES         
         DS    XL1                 SPARE                                        
ACTTABL  EQU   *-ACTTABD                                                        
         SPACE 1                                                                
INPTABD  DSECT                     ** INPUT VALIDATION KEYWORD TABLE **         
INPKWDL  DS    S                   LONG KEYWORD SCON                            
INPKLNG  EQU   8                   LONG KEYWORD MAX                             
INPKWDS  DS    S                   SHORT KEYWORD SCON                           
INPKSHT  EQU   3                   SHORT KEYWORD MAX                            
INPADDR  DS    S                   AROUT SCON OR FIELD DISP                     
INPIND   DS    XL1                                                              
INP1PTQ  EQU   X'01'               KEYWORD VALUE FIXED                          
INP2PTQ  EQU   X'02'               KEYWORD IS 'XXX=YYY' FORMAT                  
INPNOTQ  EQU   X'04'               KEYWORD CAN BE PRECEDED WITH NOT             
INPNVLQ  EQU   X'08'               NO VALIDATION ON RHS                         
INPRHMX  DS    0XL1                MAX LENGTH OF RHS, IF TWO-PART               
INPSCIV  DS    XL1                 BIT SETTING FOR INPADDR, IF ONE PART         
INPSLVL  DS    XL1                 KEYWORD STATUS LEVEL - SEE USERSTS           
INPSCR   DS    XL1                 VALID SCRN: =TWASCROV, OR 0=ANY              
INPTABL  EQU   *-INPTABD                                                        
         SPACE 1                                                                
SCANOUTD DSECT                     ** SCANNER BLOCK ENTRY **                    
SCANLEN1 DS    XL1                 L'FIELD (1ST PART IF 2-PART)                 
SCANLEN2 DS    XL1                 L'2ND PART OF 2-PART OPTION                  
SCANIND1 DS    XL1                 VALIDITY BITS 1ST PART                       
SCANIND2 DS    XL1                 VALIDITY BITS 2ND PART                       
SCANBIN1 DS    CL4                 BINARY VALUE OF NUMERIC 1ST PART             
SCANBIN2 DS    CL4                 BINARY VALUE OF NUMERIC 2ND PART             
SCANTXT1 DS    CL10                1ST PART TEXT/DATA                           
SCANTXT2 DS    CL20                2ND PART TEXT/DATA                           
SCANOUTL EQU   *-SCANOUTD          SCANNER ENTRY LENGTH                         
SCANMAXN EQU   12                  MAXIMUM N'SCANNER LINES                      
SCANLTAB EQU   (SCANOUTL*SCANMAXN)                                              
         SPACE 1                                                                
LEDGTABD DSECT                     ** LEDGER TABLE **                           
LEDGTUL  DS    CL2                 UNIT/LEDGER CODE                             
LEDGTTYP DS    CL(L'LDGTYPE)       LEDGER TYPE                                  
LEDGTLIK DS    CL(L'LDGLIKE)       LEDGER IS LIKE                               
LEDGTOFF DS    XL(L'LDGOPOS)       OFFICE POSITION                              
LEDGTCLO DS    XL(L'LDGCLOS)       LEDGER CLOSE-OUT TYPE                        
LEDGTLVA DS    XL(L'ACLVLEN)       LEVEL A LENGTH                               
LEDGTLVB DS    XL(L'ACLVLEN)       LEVEL B LENGTH                               
LEDGTLVC DS    XL(L'ACLVLEN)       LEVEL C LENGTH                               
LEDGTLVD DS    XL(L'ACLVLEN)       LEVEL D LENGTH                               
LEDGTSEC DS    XL1                 SECURITY NUMBER                              
         DS    XL1                 N/D                                          
LEDGTABL EQU   *-LEDGTABD                                                       
LEDGMAXN EQU   16                  MAXIMUM NUMER OF TABLE ENTRIES               
LEDGLTAB EQU   (LEDGTABL*LEDGMAXN)                                              
         SPACE 1                                                                
PRGTABD  DSECT                     ** PROGRAM TABLE DSECT **                    
PRGTSPR  DS    0CL3                                                             
PRGTSYS  DS    CL1                 SYSTEM CODE                                  
PRGTEOTQ EQU   255                 END OF TABLE INDICATOR                       
PRGTPRG  DS    CL2                 PROGRAM ID                                   
PRGTNAML EQU   21                  PROGRAM NAME LENGTH                          
PRGTDISP DS    AL2                 DISPLACEMENT TO NAME FROM DSLISTL            
PRGTDDEQ DS    AL2                 DD EQUATE FOR PROGRAM NAME                   
PRGTIND1 DS    XL1                 PROGRAM INDICATORS BYTE 1                    
PRGTITRL EQU   X'80'               PROCESS TRAILER FOR TOTALS                   
PRGTINOC EQU   X'40'               DON'T CHECK TRAILER TOTALS                   
PRGTIJRN EQU   X'20'               DAILY JOURNAL PROGRAM                        
PRGTICLR EQU   X'10'               SPOT/PRINT DAILY CLEARANCE                   
PRGTICHQ EQU   X'08'               CHEQUE PROGRAM                               
PRGTIPOF EQU   X'04'               PEEL-OFF PROGRAM                             
PRGTITPY EQU   X'02'               TALENT PAYMENTS                              
PRGTICEX EQU   X'01'               COKE EXPENDITURE                             
PRGTIND2 DS    XL1                 PROGRAM INDICATORS BYTE 2                    
PRGTINOT EQU   X'80'               DON'T PROCESS FILES FOR TODAY                
PRGTICPY EQU   X'40'               COMPANY CODE FROM WKCPY                      
PRGTUPDT EQU   X'20'               NON-DDS USER CAN UPDATE FILE                 
PRGTDELT EQU   X'10'               NON-DDS USER CAN DELETE FILE                 
PRGTICOP EQU   X'08'               FILE MAY BE COPIED                           
PRGTNORV EQU   X'02'               FILE MUST NOT BE REVERSED                    
PRGTXWFM EQU   X'01'               DON'T PROCESS FILE IN WFM                    
PRGTABL  EQU   *-PRGTABD                                                        
         SPACE 1                                                                
CHQTABD  DSECT                     ** CHEQUE LEDGER TABLE **                    
CHQTEOTQ EQU   255                 END OF TABLE INDICATOR                       
CHQTLEDG DS    CL1                 LEDGER CODE                                  
CHQTNAML EQU   18                  LEDGER NAME LENGTH                           
CHQTDISP DS    AL2                 DISPLACMENT TO NAME FROM DSLISTL             
CHQTABL  EQU   *-CHQTABD                                                        
         SPACE 1                                                                
UIDTABD  DSECT                     ** USER-ID TABLE **                          
UIDTUSER DS    XL2                 USER-ID NUMBER                               
UIDTCODE DS    XL10                USER-ID CODE                                 
UIDTSENO DS    XL1                 ACC SYSTEM SENO                              
UIDTABL  EQU   *-UIDTABD                                                        
UIDTMAX  EQU   ((SAVEAREL-SAVEDLN)/UIDTABL)                                     
         SPACE 1                                                                
TOTTABD  DSECT                     ** TOTALS TABLE **                           
TOTTDR   DS    PL8                 DEBIT AMOUNT                                 
TOTTEOTQ EQU   255                 END OF TABLE INDICATOR                       
TOTTCR   DS    PL8                 CREDIT AMOUNT                                
TOTTINDS DS    XL1                 INDICATORS                                   
TOTTITST EQU   X'80'               DON'T PRINT ZERO AMOUNTS                     
TOTTDESC DS    CL20                TOTALS DESCRIPTION                           
TOTTABL  EQU   *-TOTTABD                                                        
         SPACE 1                                                                
PFSTATD  DSECT                     ** PSSBDESC COVER **                         
PFSTAREA DS    0XL(L'PSSBDESC)                                                  
         DS    XL1                 X'FF' =DENOTES STATUS AREA/FILE NAME         
PFSTCID  DS    CL6                 ID OF WORKER FILE COPIED TO /FROM            
PFSTCSQ  DS    CL2                 SEQUENCE # OF FILE COPIED TO/FROM            
PFSTDAT  DS    XL2                 DATE OF WFM ACTION                           
PFSTTIM  DS    XL2                 TIME OF WFM ACTION                           
PFSTIND  DS    XL1                 INDICATOR                                    
PFSTICOF EQU   X'80'               FILE COPIED FROM PFSTCID                     
PFSTICID EQU   X'40'               FILE COPIED TO PFSTCOF                       
PFSTIREV EQU   X'20'               FILE IS A REVERSAL                           
         DS    XL1                 SPARE                                        
***********************************************************************         
* Formated commnet field                                                        
***********************************************************************         
WFMCMTD  DSECT                     ** WKCOMNT COVER **                          
WFMCAREA DS    0CL(L'UKSOFCOM)                                                  
WFMCACT  DS    CL1                 R=REVERSED,T=COPIED TO,F=COPIED FROM         
         DS    CL1                 SPACE                                        
WFMCCID  DS    CL8                 ID IF FILE COPIED TO/FROM                    
         DS    CL1                 SPACE                                        
WFMCDAT  DS    CL5                 DATE, FORMAT DDMMM                           
***********************************************************************         
* WKFILE DSECTS                                                                 
***********************************************************************         
*      ++INCLUDE DMWRKRD                                                        
*                                                                               
       ++INCLUDE DMWRKRK                                                        
UKSOFD   DSECT                                                                  
         ORG  UKSOFAGT+L'UKSOFAGT+1                                             
UKSOFRCT DS   XL4                                                               
         ORG                                                                    
*                                                                               
       ++INCLUDE DMWRKRS                                                        
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
         ORG   TWAUSER                                                          
TWAMODE  DS    XL1                 CONTROL MODE INDICATORS                      
TWAMFRST EQU   X'00'               FIRST TIME                                   
TWAMINIT EQU   X'01'               INITIALISED                                  
TWAMALTP EQU   X'04'               DISPLAY ALTERNATE PF KEYS                    
TWAMLISS EQU   X'10'               LIST SCREEN SAVED                            
TWAMCFRM EQU   X'20'               CONFIRMATION MESSAGE DISPLAYED               
TWAMDOIT EQU   X'40'               ACTION CONFIRMED BY USER                     
TWAMRSRV EQU   X'80'               TEMPEST RESERVE ISSUED                       
TWAMODE2 DS    XL1                 CONTROL MODE INDICATORS                      
TWAM2NUP EQU   X'01'               USER CONNECTED WITH U=N                      
TWASALFA DS    CL2                 SAVED 'REAL' ACTION                          
TWASCROV DS    XL1                 SCREEN NUMBER                                
TWASCLST EQU   X'FE'               LIST OVERLAY SCREEN                          
TWASCDET EQU   X'FD'               DETAIL OVERLAY SCREEN                        
TWASCRTP EQU   X'FC'               TOTALS/PFK SCREEN                            
TWASCRCA EQU   X'FB'               CONFIRMATION SCREEN                          
TWAUTLSV DS    XL10                UTL SCREEN MAP (FOR HEADER SWAP)             
TWATSARI DS    XL1                 SAVED TSAR INDICATORS                        
TWALOWPG DS    XL1                 SAVED TSPAGL VALUE                           
TWANUMPG DS    XL1                 SAVED TSPAGN VALUE                           
TWAOFFSV DS    XL(OFFASAVL)        SAVED OFFAL VALUES                           
TWACOLS  EQU   80                  TWA COLUMNS                                  
TWAROWS  EQU   24                  TWA ROWS                                     
TWAHALF  DS    H                   HALF WORD                                    
TWAFULL  DS    F                   FULL WORD                                    
TWAPCNT  DS    XL1                 PASSWORD ATTEMPTS                            
TWAPMAX  EQU   3                                                                
         ORG   TWAD+64                                                          
       ++INCLUDE ACWFMFFD                                                       
         ORG   WFMOLAYH                                                         
       ++INCLUDE ACWFMFED                                                       
         ORG   WFMOLAYH                                                         
       ++INCLUDE ACWFMFDD                                                       
TWAMAXL  EQU   1024*13             13K OF TWA0 AVAILABLE                        
SAVEAREL EQU   1024*10             10K SAVED STORAGE AREA                       
         ORG   TWAD+(TWAMAXL-SAVEAREL)                                          
SAVLISTL EQU   *-WFMMSGH           SAVE LIST SCREEN FROM WFMMSGH->*             
SAVEAREA DS    XL(SAVEDLN)         SAVE AREA COVERED BY SAVED DSECT             
UIDTAB   DS    (UIDTMAX)XL(UIDTABL)  USER-ID TABLE                              
         DS    0X                                                               
         SPACE 1                                                                
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDACCFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
*********INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDCTRYEQUS/DDLANGEQUS                                                         
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
CTRYNOT  EQU   X'80'                                                            
CTRYALL  EQU   X'00'                                                            
         SPACE 1                                                                
* ACOFFALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACBMONVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACADDTRND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACADDTRND                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FAREPBLK                                                                      
         PRINT OFF                                                              
REPD     DSECT                                                                  
       ++INCLUDE FAREPBLK                                                       
         EJECT                                                                  
REPHS    DS    0CL132              ** REPORT HEAD LINES **                      
REPH1    DS    CL132                                                            
REPH2    DS    CL132                                                            
REPH3    DS    CL132                                                            
REPH4    DS    CL132                                                            
REPH5    DS    CL132                                                            
REPH6    DS    CL132                                                            
REPH7    DS    CL132                                                            
REPH8    DS    CL132                                                            
REPH9    DS    CL132                                                            
REPHA    DS    CL132                                                            
REPHB    DS    CL132                                                            
REPHC    DS    CL132                                                            
REPHN    EQU   (*-REPHS)/L'REPHS                                                
*                                                                               
REPMS    DS    0CL132              ** REPORT MID LINES **                       
REPMN    EQU   (*-REPMS)/L'REPMS                                                
*                                                                               
REPPS    DS    0CL132              ** REPORT PRINT LINES **                     
REPP1    DS    CL132                                                            
REPP2    DS    CL132                                                            
REPP3    DS    CL132                                                            
REPP4    DS    CL132                                                            
REPP5    DS    CL132                                                            
REPP6    DS    CL132                                                            
REPP7    DS    CL132                                                            
REPP8    DS    CL132                                                            
REPP9    DS    CL132                                                            
REPPA    DS    CL132                                                            
REPPB    DS    CL132                                                            
REPPC    DS    CL132                                                            
REPPD    DS    CL132                                                            
REPPE    DS    CL132                                                            
REPPF    DS    CL132                                                            
REPPG    DS    CL132                                                            
REPPH    DS    CL132                                                            
REPPI    DS    CL132                                                            
REPPJ    DS    CL132                                                            
REPPK    DS    CL132                                                            
REPPN    EQU   (*-REPPS)/L'REPPS                                                
*                                                                               
REPFS    DS    0XL132              ** REPORT FOOT LINES **                      
REPF1    DS    CL132                                                            
REPF2    DS    CL132                                                            
REPFN    EQU   (*-REPFS)/L'REPFS                                                
*                                                                               
REPX     EQU   *                                                                
         PRINT ON                                                               
REPL     EQU   *-REPD                                                           
*                                  WKFILE LISTING                               
         ORG   REPP1                                                            
PACT     DS    CL14                ACCOUNT CODE (U/L/ACCOUNT)                   
PB1      DS    CL1                                                              
PCAC     DS    CL15                CONTRA ACCOUNT                               
PB2      DS    CL1                                                              
PWRK1    DS    CL2                 WORK CODE1                                   
PDLM     DS    CL1                                                              
PWRK2    DS    CL2                 WORK CODE 2                                  
PB3      DS    CL1                                                              
PDAT     DS    CL8                 TRANSACTION DATE                             
PB4      DS    CL1                                                              
PREF     DS    CL6                 REFERENCE NUMBER                             
PB5      DS    CL1                                                              
PBAT     DS    CL6                 BATCH                                        
PB6      DS    CL1                                                              
PNAR     DS    CL42                TRANSACTION NARRATIVE                        
PB7      DS    CL1                                                              
PDEB     DS    CL13                DEBIT AMOUNT                                 
PB8      DS    CL1                                                              
PCRD     DS    CL13                CREDIT AMOUNT                                
PBR      DS    CL1                                                              
*                                  UPDATE LISTING                               
         ORG   REPP1                                                            
PUNC     DS    CL1                 UNIT CODE                                    
         DS    CL1                                                              
PUNN     DS    CL30                UNIT NAME                                    
         DS    CL1                                                              
PLGC     DS    CL1                 LEDGER CODE                                  
         DS    CL1                                                              
PLGN     DS    CL30                LEDGER NAME                                  
         DS    CL4                                                              
PDRT     DS    CL13                DEBITS TOTAL                                 
         DS    CL2                                                              
PCRT     DS    CL13                CREDITS TOTAL                                
         DS    CL2                                                              
PBAL     DS    CL13                BALANCE                                      
         ORG                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACWFM00   02/11/18'                                      
         END                                                                    
