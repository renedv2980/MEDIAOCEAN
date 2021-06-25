*          DATA SET ACGENBAT   AT LEVEL 016 AS OF 09/09/09                      
*PHASE T00ACBA                                                                  
*INCLUDE BMONVAL                                                                
*&&      SET   NOP=N                                                            
ACGENBAT TITLE 'GENERAL CREATE BATCH MODULE'                                    
*                                                                               
* TKLU 002 17JUL07 <BR12918L> INCREASE SIZE OF GBAACBUF                         
* TKLU 003 27OCT07 <UKCR00011824> BATCH MOA CALCULATION BUG FIX                 
* JFOS 004 08AUG08 MAKE IOSIZEQ = ADDTRN IOL FOR SAFETY                         
* YNGX 005 05JUN08 <LO01-7720> UNIT G IMMEDIATE UPDATE (DISABLED)               
* TKLU 006 20AUG08 <LO01-8062>    ESTIMATE NUMBER ON CLAIM ITEM                 
* YNGX 007 21FEB08 <DU01-4986> FLEXIBILL 1.12.0 - ACGENBAT VERSION              
* YNGX 008 24OCT08 <BR13219D> BUG FIX - ALLOW 35 POSTINGS PER ITEM              
* JFOS 009 11NOV08 <BR13307D> BUG FIX - SET CONTRA NAME CORRECTLY               
* MPEN 010 12DEC08 <BR21976L> INCREASE SIZE OF GBAACBUF                         
* TKLU 011 22DEC08 <BR22106L> BUG FIX - KEEP XDFELD ON ADD IN SEQUENCE          
* YNGX 012 23DEC08 <BR22049L> BUG FIX - UNLOCK LOCKED ACCOUNT                   
* JFOS 013 26JAN08 <LO01-7636> ENABLE OFFLINE USE                               
* YNGX 013 09JAN09 <LO01-7720> UNIT G IMMEDIATE UPDATE (ENABLED)                
* NSHE 014 12MAY09 MERGE US CODE                                                
*                                                                               
         PRINT NOGEN                                                            
***********************************************************************         
* NTRY: P1 = A(CALL BACK ROUTINE)                                     *         
***********************************************************************         
         SPACE 1                                                                
ACGENBAT CSECT                                                                  
         NMOD1 GBWORKL,**ACGB**,CLEAR=YES,RR=R4                                 
         LR    R9,RC                                                            
         USING GBWORKD,R9                                                       
*                                                                               
         GOTO1 MAININI             INITIALISE ADDRESSES                         
*                                                                               
         GOTO1 AOVRCALL,GB#INIT                                                 
         BNE   EXITERR                                                          
         ICM   RF,15,GBASECBK      EXTRACT PERSONAL ID FROM SEC. BLOCK          
         BZ    *+10                NOT SET IF OFFLINE                           
         MVC   BCPID,SECPID-SECD(RF)                                            
*                                                                               
         GOTO1 MAINWS,ABLKTAB      INITIALISE W/S BLOCKS                        
         MVC   AIOGEN,GBAIOGEN     SET ADDRESSES FOR IOEX                       
         MVC   AIOBAT,GBAIOBAT                                                  
         MVC   AIOITE,GBAIOITE                                                  
*                                                                               
         GOTO1 ACPYINI             INITIALISE COMPANY VALUES                    
         BNE   EXIT                                                             
*                                                                               
         GOTO1 MAINWS,ABLKCPY      INITIALISE MORE W/S BLOCKS                   
*                                                                               
         CLI   GBACT,GBACUBQ       CREATE AND UPDATE BATCH                      
         BE    MAIN02                                                           
         CLI   GBACT,GBACUTQ       CREATE AND UPDATE TIME                       
         BE    MAIN10                                                           
         DC    H'0'                UNRECOGNISED ACTION                          
*                                                                               
MAIN02   DS    0H                  * CREATE AND UPDATE BATCH *                  
         TM    GBACTIND,GBACTIDR   TEST DRAFT ONLY                              
         BZ    MAIN04                                                           
         OI    GBINDS1,GBIDRFT+GBIFINAL                                         
         GOTO1 ABATCRT                                                          
         BNE   MAINN                                                            
         B     MAINY                                                            
*                                                                               
MAIN04   DS    0H                  LIVE - BUT DO DRAFT FIRST                    
         OI    GBINDS1,GBIDRFT+GBIDRFTB                                         
         GOTO1 ABATCRT                                                          
         BNE   MAINN                                                            
         NI    GBINDS1,FF-(GBIDRFT+GBIDRFTB)                                    
         OI    GBINDS1,GBILIVE+GBIFINAL                                         
         BASR  RE,RF               RE-RUN LIVE                                  
         BE    MAINY                                                            
         B     MAINN                                                            
*                                                                               
MAIN10   DS    0H                  * CREATE AND UPDATE TIME *                   
         GOTO1 ATIMCRT                                                          
         BE    MAINY                                                            
         B     MAINN                                                            
*                                                                               
MAINY    DS    0H                                                               
         B     EXITY                                                            
*                                                                               
MAINN    DS    0H                                                               
         TM    GBINDS1,GBIUPD                                                   
         BZ    EXITN                                                            
         OI    GBINDS1,GBIUNWND    MAKE SURE UPDATES ARE UNWOUND                
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE GENERAL ADDRESSES                                        *         
***********************************************************************         
         SPACE 1                                                                
MAININI  NTR1  ,                                                                
         ST    R4,BCRELO           SAVE RELOCATION FACTOR                       
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         ST    RF,BCAOVER          SAVE A(OVERLAY CALLBACK ROUTINE)             
***      MVC   BCAOVER,0(R1)       SAVE A(OVERLAY CALLBACK ROUTINE)             
         XC    BCAXDTA,BCAXDTA                                                  
         MVI   ASONOFF,ASON        INIT ONLINE/OFFLINE SWITCH                   
         TM    0(R1),X'80'         TEST OFFLINE 'EXTRA DATA' IND. SET           
         BZ    MINI00                                                           
         OC    BCAXDTA,4(R1)       SAVE A(EXTRA DATA)                           
         BNZ   *+6                                                              
         DC    H'0'                NOT SET - ERROR                              
         MVI   ASONOFF,ASOFF       SET OFFLINE SWITCH                           
MINI00   L     RF,4(RD)                                                         
         MVC   BCSAVRD,4(RF)       SAVE OVERLAYS RD VALUE                       
*                                                                               
         XR    RE,RE               SET A(ROUTINES CALLED BY OVERLAY)            
         LA    RF,GBROUTO                                                       
         LA    R0,GBROUTON                                                      
         LA    R1,ROUTO                                                         
MINI02   ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         AHI   RE,1                                                             
         AHI   RF,L'GBROUTO                                                     
         BCT   R0,MINI02                                                        
*                                                                               
         XR    RE,RE               SET A(INTERNAL ROUTINES)                     
         LA    RF,GBROUTI                                                       
         LA    R0,GBROUTIN                                                      
         LA    R1,ROUTI                                                         
MINI04   ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         AHI   RE,1                                                             
         AHI   RF,L'GBROUTI                                                     
         BCT   R0,MINI04                                                        
*                                                                               
         LA    RF,GBTABLE          SET A(INTERNAL TABLES)                       
         LA    R1,DTABLE                                                        
         LA    R0,GBTABLEN                                                      
MINI06   LH    RE,0(R1)                                                         
         AR    RE,RB                                                            
         ST    RE,0(RF)                                                         
         AHI   R1,L'DTABLE                                                      
         AHI   RF,L'GBTABLE                                                     
         BCT   R0,MINI06                                                        
*                                                                               
         LA    RF,VTABLE           SET A(EXTERNAL ROUTINES)                     
         LA    R1,GBVTAB                                                        
         LA    R0,GBVTABN                                                       
MINI08   L     RE,0(RF)                                                         
         A     RE,BCRELO                                                        
         ST    RE,0(R1)                                                         
         AHI   RF,L'VTABLE                                                      
         AHI   R1,L'GBVTAB                                                      
         BCT   R0,MINI08                                                        
*                                                                               
         LA    RF,ANAWS            SET A(NON-ADDRESSABLE W/S AREAS)             
         LA    R0,ANAWSN                                                        
MINI10   LH    RE,0(RF)                                                         
         LA    RE,GBWORKD(RE)                                                   
         LH    R1,2(RF)                                                         
         LA    R1,GBWORKD(R1)                                                   
         ST    R1,0(RE)                                                         
         AHI   RF,L'ANAWS                                                       
         BCT   R0,MINI10                                                        
*                                                                               
         ICM   R1,15,BCAXDTA       TEST OFFLINE APP PASSED PARAMETERS           
         BNZ   MINI16                                                           
         L     RF,4(RD)            FIND SYSTEM PARMS PASSED BY MONITER          
         LA    R0,100                                                           
MINI12   DS    0H                                                               
         CLC   0(4,RF),=C'MNTR'                                                 
         BE    MINI14                                                           
         LR    RE,RF                                                            
         L     RF,4(RF)                                                         
         BCT   R0,MINI12                                                        
         DC    H'0'                                                             
MINI14   DS    0H                                                               
         L     R1,24(RE)           R1 = A(SYSTEM PARAMETERS)                    
         MVC   CUABIN,0(R1)        COMPANY CODE                                 
         MVC   AINP,00(R1)         A(TIOB)                                      
         MVC   ATWA,04(R1)         A(TWA)                                       
         MVC   ASYS,08(R1)         A(SYSTEM FACILITY LIST)                      
         MVC   ATIA,12(R1)         A(TIA)                                       
         MVC   ACOM,16(R1)         A(COMMON FACILITY LIST)                      
         L     RF,20(R1)           RF=A(FACPAK EXTRA INFORMATION)               
         MVC   CUCTRY,1(RF)        SET AGENCY COUNTRY                           
         MVC   CULANG,3(RF)                                                     
*                                                                               
         L     RF,ATWA                                                          
         USING TWAD,RF             RA=A(TWA)                                    
         MVC   CUACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   CUUSER,TWAUSRID     USER-ID NUMBER                               
         MVC   CUAUTH,TWAAUTH      AUTHORIZATION CODE                           
         MVC   CUAALF,TWAAGY       AGENCY ALPHA-ID                              
         CLI   TWAOFFC,C'*'        TEST DDS OFFICE CODE                         
         BNE   *+8                                                              
         OI    CUSTAT,CUSDDS       SET DDS STATUS INDICATOR                     
         B     MINI18                                                           
         DROP  RF                                                               
*                                                                               
         USING GBXD,R1             EXTRA DATA PASSED BY OFFLINE CALLER          
MINI16   MVC   CUACCS,GBXACCS      LIMIT ACCESS                                 
         MVC   CUUSER,GBXUSER      USER-ID NUMBER                               
         MVC   CUAUTH,GBXAUTH      AUTHORIZATION CODE                           
         MVC   CUAALF,GBXAALF      AGENCY ALPHA-ID                              
         MVC   CUCTRY,GBXCTRY      COUNTRY                                      
         MVC   CULANG,GBXLANG      LANGUAGE                                     
         MVC   CUABIN,GBXXCPY      CPY HEXCOMP                                  
         MVC   CUPRGNO,GBXPRGN     PROGRAM NUMBER                               
         MVC   ACOM,GBXACOM        A(COMFACS)                                   
         DROP  R1                                                               
MINI18   ICM   RF,15,ACOM          SET A(COMFACS ROUTINES)                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING COMFACSD,RF                                                      
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VSECRET,CSECRET                                                  
         MVC   VSWITCH,CSWITCH                                                  
*&&UK*&& MVC   VTOBACCO,CTOBACCO                                                
         DROP  RF                                                               
*                                                                               
         LA    R2,CLIST            SET A(CORE-RES PHASES)                       
         LA    R3,GBCTAB                                                        
         LA    R4,GBCTABN                                                       
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,BCPARM                                                        
         L     RF,VCALLOV                                                       
MINI20   ICM   R0,1,0(R2)                                                       
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(L'GBCTAB,R3),0(R1)                                             
         AHI   R2,L'CLIST                                                       
         AHI   R3,L'GBCTAB                                                      
         BCT   R4,MINI20                                                        
*                                                                               
         GOTO1 VGETFACT,BCPARM,0                                                
         L     R2,0(R1)                                                         
         USING FACTSD,R2           R2=A(SYSTEM DEFINITION BLOCK)                
         L     RF,FASYSLST                                                      
         ST    RF,ASYSLST                                                       
         MVC   ACTRY,FAACTRY       SET A(COUNTRY & LANGUAGE TABLES)             
         MVC   ALANG,FAALANG                                                    
*                                                                               
         MVC   CUTSYM,FASYM                                                     
         MVC   ASEDAT,FADATE       SYSTEM DATES (VARIOUS FORMATS)               
         MVC   ASBDAT,FADATEB                                                   
         OC    CUPASS,FAPASSWD     TEST CONNECTED WITH PASSWORD                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ASTIME,FATIME       SYSTEM TIME (STANDARD FORMAT)                
         MVC   ASSYSN,FASYS        SYSTEM NUMBER                                
         MVC   ASSYSO,FAOVSYS      SYSTEM NUMBER (FOR CALLOV)                   
         MVC   ASSIN,FASIN         SYSTEM INPUT NUMBER                          
         MVC   ASIOASTR,FAIOASTR   SYSTEM EXTRA AREA ADDRESS                    
         MVC   ASIOALEN,FAIOALEN   SYSTEM EXTRA AREA LENGTH                     
         DROP  R2                                                               
*                                                                               
         GOTO1 VDATCON,BCPARM,(5,0),(1,BCTODAYP)                                
         GOTO1 (RF),(R1),(5,0),(2,BCTODAYC)                                     
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    MINI22                                                           
                                                                                
         MVC   BCPARM(4),BCEFFS    SET A(UTL ENTRY)                             
         GOTO1 VSWITCH,BCPARM                                                   
         L     RF,0(R1)                                                         
         ST    RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   CUASEC,TAGYSEC      AGENCY ALPHA VALUE FOR SECURITY              
         MVC   CUPHSYS,TSYS                                                     
         MVC   CUOVSYS,TOVSYS                                                   
         MVC   CUPRGNO,TPRG                                                     
         TM    TTEST,TTESTNOU      TEST UPDATE=N ENTERED ON CONNECT             
         BZ    *+8                                                              
         OI    ASINDS,ASIRONLY                                                  
         XR    RE,RE                                                            
         ICM   RE,7,TASYS                                                       
         USING SELISTD,RE          TEST SYSTEM IN READ-ONLY MODE                
         TM    SEIND,SEISETRO+SEIRONLY                                          
         BZ    *+8                                                              
         OI    ASINDS,ASIRONLY                                                  
         DROP  RE,RF                                                            
         GOTO1 VDMGR,BCPARM,=C'DTFADD ',=C'ACCDIR '                             
         L     RE,12(R1)                                                        
         TM    ISFOPEN-ISDTF(RE),ISFORO+ISFOQUIE                                
         BZ    *+8                 TEST FOR UPDATES INHIBITED                   
         OI    ASINDS,ASIRONLY                                                  
*                                                                               
         MVC   BCPARM(4),BCEFFS    SET A(SYSFACS)                               
         MVI   BCPARM,X'FE'                                                     
         GOTO1 VSWITCH,BCPARM                                                   
         MVC   ASYSFAC,0(R1)                                                    
*                                                                               
MINI22   MVC   IOSWSYSN,ASSYSO                                                  
         MVC   IOSWSYSC,ASSYSO                                                  
         MVC   IOSWSYSP,ASSYSO                                                  
*                                                                               
         L     R1,AFILTAB          FIND FILE TABLE ENTRY (FOR IOEX)             
         XR    RE,RE                                                            
MINI26   CLI   0(R1),EOT           TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,R1),IOSWSYSN    MATCH ON OVSYS NUMBER                        
         BE    *+16                                                             
         ICM   RE,3,4(R1)                                                       
         LA    R1,5(RE,R1)                                                      
         B     MINI26                                                           
         LA    R1,6(R1)                                                         
         ST    R1,AFILNTRY         SAVE A(FILE TABLE ENTRY)                     
*                                                                               
         GOTO1 VGETFACT,BCPARM,(X'80',0),F#TCBD                                 
         L     R1,0(R1)                                                         
         USING F@TCBD,R1                                                        
         SR    R0,R0                                                            
         ICM   R0,1,F@BSWNUM       R0=N'ENTRIES IN TCBSWTAB                     
         BZ    MINI30                                                           
         CLM   R0,1,=AL1(SYSSWMAX)                                              
         BNH   *+8                                                              
         LA    R0,SYSSWMAX                                                      
         LA    R1,F@BSWTAB                                                      
         USING F@BSWTAB,R1         R1=A(TCB SWITCH TABLE)                       
         L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(LOCAL SWITCH TABLE)                     
MINI28   MVC   SYSSWSYS,F@BSWSYS                                                
         MVC   SYSSWSOV,F@BSWSOV                                                
         MVC   SYSSWAGB,F@BSWAGB                                                
         MVC   SYSSWACS,F@BSWACS                                                
         MVC   SYSSWAC2,F@BSWAC2                                                
         LA    R1,F@BSWLEN(R1)     BUMP TO NEXT                                 
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   R0,MINI28           DO FOR NUMBER OF ENTRIES                     
         DROP  R1,RE                                                            
*                                                                               
MINI30   DS    0H                                                               
*                                                                               
MAININIX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXTEND W/S FOR BLOCKS NOT PASSED BY OVERLAY                         *         
*                                                                     *         
* NTRY: R1 = A(BLKTAB)                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MAINWS   STM   RE,RC,BCWORK                                                     
         L     R7,0(R1)                                                         
         USING BLKTABD,R7                                                       
MWS02    XR    R2,R2                                                            
         ICM   R2,3,BLKDISPA                                                    
         BZ    MAINWSX                                                          
         LA    R2,GBWORKD(R2)      R2 = A(A(BLOCK))                             
*                                                                               
         XR    RF,RF               TEST INDICATOR TO TEST                       
         ICM   RF,3,BLKDISPI                                                    
         BZ    MWS04                                                            
         LA    RF,GBWORKD(RF)      R4 = A(INDICATOR)                            
         MVC   BCBYTE1,0(RF)                                                    
         NC    BCBYTE1,BLKVALI                                                  
         BZ    MWS18               DON'T USE IF INDICATOR IS NOT ON             
*                                                                               
MWS04    DS    0H                  FILTER ON ACTION (GBACT)                     
         LA    RF,BLKACTS                                                       
         LA    R0,L'BLKACTS                                                     
MWS06    CLC   GBACT,0(RF)         ACTION FOUND IN LIST                         
         BE    MWS10                                                            
         CLI   0(RF),0             END OF VALID ACTIONS                         
         BE    MWS18                                                            
         AHI   RF,1                                                             
         BCT   R0,MWS06                                                         
         B     MWS18                                                            
*                                                                               
MWS10    DS    0H                                                               
         LH    R5,BLKDEFL          R5 = L(EXTENSTION)                           
         XR    RE,RE                                                            
         ICM   RE,3,BLKDISPL       TEST FOR LENGTH IN W/S                       
         BZ    MWS12                                                            
         LA    RE,GBWORKD(RE)                                                   
         OC    0(2,RE),0(RE)       TEST LENGTH IS SET IN W/S                    
         BNZ   *+12                                                             
         STH   R5,0(RE)            NO - SET TO DEFAULT                          
         B     MWS12                                                            
         LH    R5,0(RE)            YES - USE IT                                 
*                                                                               
MWS12    DS    0H                                                               
         ICM   R4,15,0(R2)         R4 = A(W/S)                                  
         BZ    MWS14                                                            
         XR    R3,R3               ALREADY SET - STILL CLEAR                    
         MVCL  R4,R2                                                            
         B     MWS18                                                            
*                                                                               
MWS14    DS    0H                                                               
         LR    R4,RD               R4 = A(EXTENSION)                            
         ST    R4,0(R2)                                                         
         L     R6,4(RD)                                                         
         AHI   R5,-1                                                            
         SRL   R5,3                ENSURE DOUBLE WORD LENGTH                    
         AHI   R5,1                                                             
         SLL   R5,3                                                             
         AR    RD,R5                                                            
         XR    R3,R3               CLEAR EXTENSION                              
         MVCL  R4,R2                                                            
         ST    RD,8(R6)                                                         
         ST    R6,4(RD)                                                         
*                                                                               
MWS18    DS    0H                  BUMP TO NEXT ENTRY                           
         LA    R7,BLKTABL(R7)                                                   
         B     MWS02                                                            
*                                                                               
MAINWSX  DS    0H                                                               
         LM    RE,RC,BCWORK                                                     
         BR    RE                                                               
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITERR  DS    0H                  RETURN ERROR INFO TO CALLER                  
         OC    GBEMSGNO,GBEMSGNO                                                
         BZ    *+8                                                              
         OI    GBEINDS,GBEISET                                                  
         TM    GBINDS1,GBIUPD      TEST FILE UPDATED?                           
         BZ    *+8                                                              
         OI    GBINDS1,GBIUNWND    YES - TELL OVERLAY TO UNWIND                 
         GOTO1 AOVRCALL,GB#ERROR                                                
         B     EXITN                                                            
*                                                                               
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR COMMON BLOCK (DEFINED AT END AS INCLUDES LTORG)           *         
***********************************************************************         
         SPACE 1                                                                
COMBLK   DS    0H                                                               
         ORG   COMBLKX                                                          
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* BRANCH TO PUBLIC ROUTINE (IE CALLABLE FROM OVERLAY)                 *         
***********************************************************************         
         SPACE 1                                                                
ROUTO    NTR1  ,                                                                
         LA    RB,0(RF)            RESTORE CORRECT RB VALUE                     
         AHI   RB,-(ROUTO-ACGENBAT)                                             
         L     R2,4(RD)            FIND R9 PASSED FROM MAIN CALL                
         LA    R0,100                                                           
ROUTO02  DS    0H                                                               
         CLC   0(4,R2),=C'ACGB'                                                 
         BE    ROUTO04                                                          
         LR    RE,R2                                                            
         L     R2,4(R2)                                                         
         BCT   R0,ROUTO02                                                       
         DC    H'0'                                                             
ROUTO04  DS    0H                                                               
         L     R9,56(RE)           RESTORE R9 = A(GBWORKD)                      
         C     RB,64(RE)           ENSURE HAVE CORRECT VALUE                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SRL   RF,24               RF = ROUTINE NUMBER                          
         CHI   RF,AROUTON                                                       
         BL    *+6                                                              
         DC    H'0'                                                             
         SLL   RF,3                                                             
         LA    RF,AROUTO(RF)       RF = A(ROUTINE, SIZE OF LOCAL W/S)           
         L     R8,0(RF)                                                         
         A     R8,BCRELO           R8 = A(ROUTINE)                              
         ICM   R3,15,4(RF)         TEST FOR W/S REQUIRED                        
         BZR   R8                  NONE - SIMPLY BRANCH TO ROUTINE              
*                                                                               
         AHI   R3,-1                                                            
         SRL   R3,3                ENSURE DOUBLE WORD LENGTH                    
         AHI   R3,1                                                             
         SLL   R3,3                                                             
         L     RF,4(RD)            EXTEND W/S                                   
         LR    RC,RD                                                            
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
         BR    R8                                                               
*                                                                               
         SPACE 1                                                                
         DS    0A                                                               
AROUTO   DS    0XL8                ** OVERLAY ROUTINES **                       
         DC    A(PSTADD,PAWORKL)                                                
         DC    A(ACTVAL,AVWORKL)                                                
         DC    A(TIMADD,0)                                                      
AROUTON  EQU   (*-AROUTO)/L'AROUTO                                              
         DS    (AROUTON-GBROUTON)X ENSURE AROUTON=GBROUTON                      
         DS    (GBROUTON-AROUTON)X                                              
         EJECT                                                                  
***********************************************************************         
* BRANCH TO PRIVATE ROUTINE (IE INTERNAL TO ACGENBAT)                 *         
***********************************************************************         
         SPACE 1                                                                
ROUTI    NTR1  ,                                                                
*                                                                               
         SRL   RF,24               RF = ROUTINE NUMBER                          
         CHI   RF,AROUTIN                                                       
         BL    *+6                                                              
         DC    H'0'                                                             
         SLL   RF,3                                                             
         LA    RF,AROUTI(RF)       RF = A(ROUTINE, SIZE OF LOCAL W/S)           
         L     R8,0(RF)                                                         
         A     R8,BCRELO                                                        
         ICM   R3,15,4(RF)         TEST FOR W/S REQUIRED                        
         BZR   R8                  NONE - SIMPLY BRANCH TO ROUTINE              
*                                                                               
         AHI   R3,-1                                                            
         SRL   R3,3                ENSURE DOUBLE WORD LENGTH                    
         AHI   R3,1                                                             
         SLL   R3,3                                                             
         L     RF,4(RD)            EXTEND W/S                                   
         LR    RC,RD                                                            
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
         BR    R8                                                               
*                                                                               
         DS    0A                                                               
AROUTI   DS    0XL8                ** INTERNAL ROUTINES **                      
         DC    A(OVRCALL,0)                                                     
*                                                                               
         DC    A(CPYINI,0)                                                      
*                                                                               
         DC    A(BATCRT,0)                                                      
         DC    A(BATOPN,0)                                                      
         DC    A(BATCLO,0)                                                      
*                                                                               
         DC    A(ITECRT,0)                                                      
         DC    A(ITEOPN,0)                                                      
         DC    A(ITEUPD,0)                                                      
         DC    A(ITECLO,0)                                                      
*                                                                               
         DC    A(PSTSADD,0)                                                     
         DC    A(PSTTRN,0)                                                      
         DC    A(PSTBLD,0)                                                      
*                                                                               
         DC    A(ADTOPN,0)                                                      
         DC    A(ADTUPD,0)                                                      
         DC    A(ADTCLO,0)                                                      
*                                                                               
         DC    A(TIMCRT,0)                                                      
*                                                                               
         DC    A(TRSSET,0)                                                      
         DC    A(ATRADD,0)                                                      
         DC    A(ATRBLD,ABWORKL)                                                
*                                                                               
         DC    A(ACBGET,AGWORKL)                                                
         DC    A(ACBPUT,APWORKL)                                                
         DC    A(ACBFND,0)                                                      
*                                                                               
         DC    A(IOEX,IEWORKL)                                                  
         DC    A(ELADD,EAWORKL)                                                 
         DC    A(VALMOA,VMWORKL)                                                
         DC    A(INIIREC,0)                                                     
*                                                                               
AROUTIN  EQU   (*-AROUTI)/L'AROUTI                                              
         DS    (AROUTIN-GBROUTIN)X ENSURE AROUTIN=GBROUTIN                      
         DS    (GBROUTIN-AROUTIN)X                                              
         EJECT                                                                  
***********************************************************************         
* CALL OVERLAY                                                        *         
*                                                                     *         
* NTRY: R1 = MODE                                                     *         
***********************************************************************         
         SPACE 1                                                                
OVRCALL  DS    0H                                                               
         USING *,R8                                                             
         STC   R1,GBMODE                                                        
         L     RF,BCAOVER                                                       
         LA    R1,GBWORKD                                                       
         L     RE,BCSAVRD          RESTORE OVERLAYS REGISTERS                   
         LM    R2,RC,28(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                   NEED OWN XIT HERE                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE VALUES FOR COMPANY                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CPYINI   DS    0H                                                               
         USING *,R8                                                             
         ICM   R3,15,GBACPYEL      TEST PASSED COMPANY ELEMENT                  
         BZ    CINI02                                                           
         USING CPYELD,R3                                                        
         CLI   CPYEL,CPYELQ                                                     
         BNE   CINI02                                                           
         IC    RF,CPYLN            YES - COPY INTO BCCPYEL                      
         CHI   RF,L'BCCPYEL                                                     
         BNH   *+8                                                              
         LHI   RF,L'BCCPYEL                                                     
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BCCPYEL(0),CPYELD                                                
         DROP  R3                                                               
*                                                                               
         ICM   RF,15,GBADUM1R      TEST PASSED DUMMY 1R PERSON                  
         BZ    CINI02                                                           
         MVC   BCDUM1RK,0(RF)                                                   
         B     CPYINIX             NO NEED TO READ COMPANY RECORD               
*                                                                               
K        USING CPYRECD,IOKEY                                                    
CINI02   DS    0H                                                               
         MVC   BCDUM1RK,BCSPACES                                                
*                                                                               
         MVC   K.CPYKEY,BCSPACES                                                
         MVC   K.CPYKCPY,CUABIN                                                 
         GOTO1 AIO,IOREAD+IOACCDIR+IOGENQ                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IOGENQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOGEN                                                        
         USING CPYRECD,R2                                                       
         LA    R3,CPYRFST                                                       
         XR    RF,RF                                                            
CINI04   CLI   0(R3),0                                                          
         BE    CPYINIX                                                          
         IC    RF,1(R3)                                                         
*                                                                               
         USING CPYELD,R3                                                        
         CLI   CPYEL,CPYELQ                                                     
         BNE   CINI06                                                           
         LR    RE,RF                                                            
         CHI   RE,L'BCCPYEL                                                     
         BNH   *+8                                                              
         LHI   RE,L'BCCPYEL                                                     
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCCPYEL(0),CPYELD                                                
         B     CINI08                                                           
         DROP  R3                                                               
*                                                                               
         USING FFTELD,R3                                                        
CINI06   CLI   FFTEL,FFTELQ                                                     
         BNE   CINI08                                                           
         CLI   FFTTYPE,FFTTPERS    DUMMY 1R PERSON A/C                          
         BNE   CINI08                                                           
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         CHI   RE,L'BCDUM1RK                                                    
         BNH   *+8                                                              
         LHI   RE,L'BCDUM1RK                                                    
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCDUM1RK(0),FFTDATA                                              
         DROP  R3                                                               
*                                                                               
CINI08   DS    0H                                                               
         BXH   R3,RF,CINI04                                                     
*                                                                               
CPYINIX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE BATCH                                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BATCRT   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         GOTO1 AADTOPN                                                          
*                                                                               
         GOTO1 ABATOPN                                                          
         BNE   EXIT                                                             
*                                                                               
         L     RF,AITECRT                                                       
         BASR  RE,RF                                                            
         BE    *-2                 CC = EQUAL, GET NEXT ITEM                    
         BL    EXIT                CC = LOW, ERROR                              
*                                  CC = HIGH, FINISHED                          
BCRT10   DS    0H                                                               
         OC    GBBITEMS,GBBITEMS                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ABATCLO                                                          
         BNE   EXIT                                                             
*                                                                               
         GOTO1 AADTCLO                                                          
*                                                                               
BATCRTX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN BATCH                                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BATOPN   DS    0H                                                               
         USING *,R8                                                             
         XC    GBBAT,GBBAT         CLEAR BATCH BLOCK                            
         ZAP   GBBAMT,BCPZERO                                                   
         MVC   BCAATLST,GBAATBUF                                                
*                                                                               
         GOTO1 AOVRCALL,GB#BATCH_OPEN                                           
         BNE   EXITERR                                                          
*                                                                               
BOPN02   DS    0H                                                               
         BAS   RE,VALBGRP          VALIDATE BATCH GROUP                         
         BNE   EXIT                                                             
         BAS   RE,VALBTYP          VALIDATE BATCH TYPE                          
         BNE   EXIT                                                             
         BAS   RE,VALBREF          VALIDATE BATCH REF                           
         BNE   EXIT                                                             
         BAS   RE,VALBNAM          VALIDATE BATCH NAME                          
         BNE   EXIT                                                             
         GOTO1 AVALMOA,BCPARM,(C'B',GBBMOAP),GBBMOAC VALIDATE MOA               
         BNE   EXIT                                                             
         BAS   RE,VALBEDT          VALIDATE BATCH EFFECTIVE DATE                
         BNE   EXIT                                                             
         BAS   RE,VALLUID          VALIDATE TERMINAL LUID                       
         BNE   EXIT                                                             
         BAS   RE,VALPID           VALIDATE PERSONAL ID                         
         BNE   EXIT                                                             
         BAS   RE,VALBAPRV         VALIDATE BATCH APPROVER                      
         BNE   EXIT                                                             
         BAS   RE,VALUID           VALIDATE USER-ID                             
         BNE   EXIT                                                             
         BAS   RE,VALLAOFF         VALIDATE LIMIT ACCESS OFFICE                 
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,INIBREC          INITIALISE BATCH RECORD                      
         BNE   EXIT                                                             
*                                                                               
BATOPNX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH GROUP                                                *         
***********************************************************************         
         SPACE 1                                                                
VALBGRP  NTR1  ,                                                                
         CLI   GBBGRUP,TBAGGENQ    TEST IS GENERAL BATCH                        
         BE    EXIT                                                             
         CLI   GBBGRUP,TBAGPRDQ    TEST IS PRODUCTION BATCH                     
         BE    EXIT                                                             
         CLI   GBBGRUP,0                                                        
         BE    *+6                                                              
         DC    H'0'                NO POINT IN RETURNING AN ERROR               
         MVI   GBBGRUP,TBAGGENQ    DEFAULT TO GENERAL BATCH                     
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH TYPE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALBTYP  NTR1  ,                                                                
         CLI   GBBTYP,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH REF                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALBREF  NTR1  ,                                                                
*                                                                               
         LHI   R0,L'GBBREF         TEST INPUT IS LONG ENOUGH                    
         LA    R1,GBBREF+L'GBBREF-1                                             
VBREF02  CLI   0(R1),C' '                                                       
         BH    VBREF04                                                          
         AHI   R1,-1                                                            
         BCT   R0,VBREF02                                                       
VBREF04  CHI   R0,2                                                             
         BL    VALBREFN                                                         
*                                                                               
VBREF06  CLI   0(R1),C'A'          TEST FOR VALID CHARACTER                     
         BL    VALBREFN                                                         
         AHI   R1,-1                                                            
         BCT   R0,VBREF06                                                       
         B     EXITY                                                            
*                                                                               
VALBREFN MVC   GBEMSGNO,=AL2(AE$INVIF)                                          
         MVC   GBEXTRA(L'GBBREF),GBBREF                                         
         B     EXITERR                                                          
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALBNAM  NTR1  ,                                                                
         CLC   GBBNAME,BCSPACES    TEST IS NOT SET                              
         BH    EXITY                                                            
         MVC   GBBNAME,BCSPACES    DEFAULT IS TO USE PERSON CODE                
         MVC   GBBNAME(L'BCPID),BCPID                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH EFFECTIVE DATE                                       *         
***********************************************************************         
         SPACE 1                                                                
VALBEDT  NTR1  ,                                                                
         OC    GBBEDT,GBBEDT       DEFAULT IS TODAY                             
         BNZ   *+10                                                             
         MVC   GBBEDT,BCTODAYC                                                  
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE TERMINAL-ID                                                *         
***********************************************************************         
         SPACE 1                                                                
VALLUID  NTR1  ,                                                                
         CLI   GBBLUID,C' '                                                     
         BH    EXITY                                                            
         MVC   GBBLUID,CUTSYM      DEFAULT IS CONNECTED                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH PERSON ID                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPID   NTR1  ,                                                                
         OC    GBBPID,GBBPID                                                    
         BNZ   EXITY                                                            
         MVC   GBBPID,CUPASS       DEFAULT IS CONNECTED                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BATCH APPROVER                                             *         
***********************************************************************         
         SPACE 1                                                                
VALBAPRV NTR1  ,                                                                
         OC    GBBAPRVR,GBBAPRVR                                                
         BNZ   EXITY                                                            
         MVC   GBBAPRVR,GBBPID     DEFAULT IS AS PER INPUTER                    
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OWNER USER-ID NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
VALUID   NTR1  ,                                                                
         OC    GBBUID,GBBUID                                                    
         BNZ   EXITY                                                            
         MVC   GBBUID,CUUSER       DEFAULT IS CONNECTED                         
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OFFICE FROM LIMIT ACCESS                                   *         
***********************************************************************         
         SPACE 1                                                                
VALLAOFF NTR1  ,                                                                
         OC    GBBLAOFF,GBBLAOFF                                                
         BNZ   EXITY                                                            
*                                  DEFAULT IS AS CONNECTED                      
         CLI   CUACCS,C'*'         SET ONE/TWO CHARACTER OFFICE                 
         BNE   *+14                                                             
         MVC   GBBLAOFF(1),CUACCS+1                                             
         B     EXITY                                                            
         MVC   GBBLAOFF,CUACCS                                                  
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* INITIALISE BATCH RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
INIBREC  NTR1  ,                                                                
         L     R2,GBAIOBAT         INITIALISE RECORD                            
         USING TBARECD,R2                                                       
         XC    TBARECD(TBARFST+1-TBARECD),TBARECD                               
         LHI   RF,TBARFST+1-TBARECD                                             
         STH   RF,TBARLEN                                                       
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,CUABIN                                                   
         MVC   TBAKUSER,GBBUID                                                  
         MVC   TBAKADDT,BCTODAYC                                                
         XC    TBAKADDT,BCEFFS                                                  
         MVC   TBAKGRUP,GBBGRUP                                                 
         MVC   TBAKBTYP,GBBTYP                                                  
         MVC   TBAKBMOS,GBBMOAP                                                 
         MVC   TBAKBREF,GBBREF                                                  
         MVC   TBAKBCHR,GBBPID                                                  
         MVC   TBAKOFFC,GBBLAOFF                                                
*        MVI   TBARHSTA,TBAHSEND+TBAHSUPD+TBAHSIAD+TBAHSAPR                     
         MVI   TBARHSTA,TBAHSUPD                                                
         MVC   TBAHRADT,BCTODAYC   DATE BATCH ADDED                             
         MVC   TBAHREDT,GBBEDT     EFFECTIVE DATE                               
         MVC   TBAHRUDT,BCTODAYC   DATE BATCH UPDATED                           
*&&DO*&& OI    TBAHRIND,TBAHIGDJ   TURNING THIS BIT ON MEANS DO NOT             
*                                  DISPLAY ITEM/DISPLAY SCREEN SFSELD           
*                                                                               
K        USING TBARECD,IOKEY                                                    
         MVC   K.TBAKEY,TBAKEY                                                  
         TM    GBBINDSI,GBBIDUP    TEST DUPLICATES ALLOWED                      
         BNZ   IBREC02                                                          
         XC    K.TBAKBCHR(TBAKSTA-TBAKEY),K.TBAKBCHR                            
         GOTO1 AIO,IOHID+IOACCDIR+IOLOCK                                        
         CLC   K.TBAKEY(TBAKBCHR-TBAKEY),TBAKEY                                 
         BE    INIBRECN                                                         
         B     IBREC10                                                          
*                                                                               
IBREC02  DS    0H                  BUILD REST OF KEY                            
         MVC   K.TBAKEY,TBAKEY                                                  
         XC    K.TBAKOFFC(TBAKSTA-TBAKOFFC),K.TBAKSEQN                          
         GOTO1 AIO,IOHID+IOACCDIR+IOLOCK                                        
         CLC   K.TBAKEY(TBAKOFFC-TBAKEY),TBAKEY                                 
         BNE   IBREC10                                                          
         CLI   TBAKSEQN,FF         ERROR IF ALL SEQUENCE#S USED                 
         BE    INIBRECN                                                         
         IC    RE,TBAKSEQN         INCREMENT SEQUENCE NUMBER                    
         AHI   RE,1                                                             
         STC   RE,TBAKSEQN                                                      
         B     IBREC02                                                          
*                                                                               
IBREC10  DS    0H                                                               
         USING BHDELD,BCELEM                                                    
         XC    BHDELD(BHDLNQ),BHDELD                                            
         MVI   BHDEL,BHDELQ                                                     
         MVI   BHDLN,BHDLNQ                                                     
         MVC   BHDNAME,GBBNAME                                                  
         ZAP   BHDCASHC,BCPZERO                                                 
         ZAP   BHDCASHA,BCPZERO                                                 
         MVC   BHDLUID,GBBLUID                                                  
         MVC   BHDIBNO,GBBPID                                                   
         MVC   BHDAPRVR,GBBAPRVR                                                
         TM    GBBINDSI,GBBINREV   TEST REVERSAL ALLOWED                        
         BZ    *+8                                                              
         OI    BHDSTAT1,BHDSNREV                                                
         MVC   BHDPRGNO,CUPRGNO                                                 
         GOTO1 AELADD,BCPARM,TBARECD,BHDELD                                     
*                                                                               
         USING FFTELD,BCELEM                                                    
         MVI   FFTEL,FFTELQ        ADD ANY COMMENTS                             
         MVI   FFTTYPE,FFTTFREE                                                 
         MVI   FFTSEQ,1                                                         
         MVI   FFTLN,FFTLN1Q                                                    
         GOTO1 BLDCOM,GBBLCOM1                                                  
         GOTO1 (RF),GBBLCOM2                                                    
         CLI   FFTLN,FFTLN1Q                                                    
         BNH   IBREC12                                                          
         GOTO1 AELADD,BCPARM,(C'E',TBARECD),FFTELD                              
*                                                                               
IBREC12  DS    0H                                                               
         B     EXITY                                                            
*                                                                               
INIBRECN DS    0H                                                               
         MVC   GBEMSGNO,=AL2(AE$BATAE)                                          
         B     EXITERR                                                          
         SPACE 1                                                                
         USING FFTELD,BCELEM                                                    
BLDCOM   NTR1  ,                                                                
         XR    RE,RE                                                            
         ICM   RE,1,0(R1)          RE = L(COMMENT)                              
         BZ    EXIT                                                             
         ICM   RF,7,1(R1)          RF = A(COMMENT)                              
         BZ    EXIT                                                             
         LA    R3,0(RE,RF)                                                      
         BCTR  R3,0                R3 = A(LAST CHARACTER)                       
BCOM02   CLI   0(R3),C' '          REMOVE TRAILING BLANKS                       
         BH    BCOM04                                                           
         BCTR  R3,0                                                             
         BCT   RE,BCOM02                                                        
         B     EXIT                EFFECTIVELY ZERO LENGTH                      
*                                                                               
BCOM04   DS    0H                                                               
         XR    R2,R2                                                            
         IC    R2,FFTLN                                                         
         LA    R3,FFTELD(R2)                                                    
         USING FFTDLEN,R3                                                       
         STC   RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),0(RF)                                                 
         LA    R2,2(RE,R2)                                                      
         CHI   R2,X'FF'                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R2,FFTLN                                                         
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE BATCH                                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BATCLO   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIOBAT                                                        
         USING TBARECD,R2                                                       
         USING BHDELD,TBARFST                                                   
         TM    GBINDS1,GBIDRFT                                                  
         BNZ   EXITY                                                            
*                                                                               
BCLO04   OI    TBARHSTA,TBAHSUPD                                                
         MVC   BHDITEMC,GBBITEMS   ITEMS CONTROL TOTAL                          
         MVC   BHDITEMA,GBBITEMS   ITEMS INPUT                                  
         ZAP   BHDCASHC,GBBAMT     CASH CONTROL TOTAL                           
         ZAP   BHDCASHA,GBBAMT     CASH TOTAL                                   
*                                                                               
         GOTO1 AOVRCALL,GB#BATCH_CLOSE_BEFORE                                   
         BNE   EXITERR                                                          
*                                                                               
         GOTO1 AIO,IOADD+IOACCMST+IOBATQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GBBDA,IODA                                                       
*                                                                               
         L     R2,AIOBAT           ADD PASSIVE                                  
         USING TBARECD,R2                                                       
P        USING TBAPAS,IOKEY                                                     
         MVC   P.TBAPAS,TBAKEY                                                  
         MVI   P.TBAPTYP,TBAPTYPQ                                               
         MVC   P.TBAPEFDT,TBAHREDT                                              
         MVC   P.TBAPTSEQ,TBAHRADT  SET ADDED DATE HERE                         
         MVC   P.TBAKSTA,TBARSTA                                                
         MVC   P.TBAKDA,GBBDA                                                   
         GOTO1 AIO,IOADD+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AOVRCALL,GB#BATCH_CLOSE_AFTER                                    
         BNE   EXITERR                                                          
*                                                                               
BATCLOX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE ITEM                                                         *         
*                                                                     *         
* EXIT: CC = HIGH IF NO MORE ITEMS TO ADD                             *         
*       CC = HIGH IF ERROR                                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ITECRT   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         GOTO1 AITEOPN                                                          
         BNE   EXIT                                                             
         MVI   BCATINDS,BCATIITE+BCATIBLK                                       
*                                                                               
         GOTO1 APSTSADD                                                         
         BNE   EXITL                                                            
*                                                                               
*        OC    GBIPSTS,GBIPSTS     FLEXIBILL MAY NOT HAVE POSTINGS IN           
*        BNZ   *+6                 THE FIRST RUN                                
*        DC    H'0'                                                             
         GOTO1 AITECLO                                                          
         BNE   EXITL                                                            
         AP    GBBAMT,GBIAMT       UPDATE BATCH TOTAL AMOUNT                    
*                                                                               
ITECRTX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN ITEM                                                           *         
*                                                                     *         
* EXIT: CC = HIGH IF NO MORE ITEMS TO ADD                             *         
*       CC = LOW IF ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ITEOPN   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         XC    GBITE,GBITE         CLEAR ITEM BLOCK                             
         ZAP   GBIAMT,BCPZERO                                                   
         ZAP   GBITOTDR,BCPZERO                                                 
         ZAP   GBITOTCR,BCPZERO                                                 
*                                                                               
         GOTO1 AOVRCALL,GB#ITEM_OPEN                                            
         BNE   EXITERR                                                          
         TM    GBIINDSI,GBIILAST                                                
         BNZ   EXITH                                                            
*                                                                               
         LH    RE,GBBITEMS         INCREMENT ITEM COUNT                         
         AHI   RE,1                                                             
         STH   RE,GBBITEMS                                                      
*                                                                               
         BAS   RE,VALIREF          VALIDATE ITEM REF                            
         BNE   EXITL                                                            
*                                                                               
         GOTO1 AINIIREC            INITIALISE ITEM RECORD                       
         BNE   EXITL                                                            
*                                                                               
ITEOPNX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE ITEM REF                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALIREF  NTR1  ,                                                                
*                                                                               
         CLC   GBIREF,BCSPACES     TEST USER DEFINED                            
         BH    VIREF02                                                          
         MVC   GBIREF,BCSPACES     NO - SET REF#=SEQUENCE NUMBER                
         LH    RE,GBBITEMS                                                      
         CVD   RE,BCDUB                                                         
         OI    BCDUB+L'BCDUB,X'0F'                                              
         UNPK  GBIREF(4),BCDUB                                                  
         B     EXITY                                                            
*                                                                               
VIREF02  DS    0H                                                               
         LHI   R0,L'GBIREF         TEST INPUT IS LONG ENOUGH                    
         LA    R1,GBIREF+L'GBIREF-1                                             
VIREF04  CLI   0(R1),C' '                                                       
         BH    VIREF06                                                          
         AHI   R1,-1                                                            
         BCT   R0,VIREF04                                                       
VIREF06  CHI   R0,1                                                             
         BL    VALIREFN                                                         
*                                                                               
VIREF08  CLI   0(R1),C'A'          TEST FOR VALID CHARACTER                     
         BL    VALIREFN                                                         
         AHI   R1,-1                                                            
         BCT   R0,VIREF08                                                       
         B     EXITY                                                            
*                                                                               
VALIREFN MVC   GBEMSGNO,=AL2(AE$INVIF)                                          
         MVC   GBEXTRA(L'GBIREF),GBIREF                                         
         B     EXITERR                                                          
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE ITEM AFTER POSTING HAS BEEN ADDED                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ITEUPD   DS    0H                                                               
         USING *,R8                                                             
         L     R2,AIOITE                                                        
         USING TBARECD,R2                                                       
         L     R4,GBAIOTRN                                                      
         USING TRNRECD,R4                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         TM    GBPINDS,GBPIUBKO    UPDATE BUCKET ONLY                           
         BO    ITEUPDX             YES - DON'T UPDATE THE ITEM RECORD           
*                                                                               
         TM    TRNSTAT,TRNSDR      UPDATE DEBIT/CREDIT ITEM TOTAL               
         BZ    *+14                                                             
         AP    GBITOTDR,TRNAMNT                                                 
         B     *+10                                                             
         AP    GBITOTCR,TRNAMNT                                                 
*                                                                               
*        IF GINELD IS ON TRANSACTION                                            
*        THERE IS ONLY NEED TO PUT ONE GINELD ON THE                            
*        RECORD ??                                                              
*                                                                               
*                                                                               
*                                                                               
         PUSH  USING                                                            
         USING ASKELD,BCELEM                                                    
IUPD08   MVI   ASKEL,ASKELQ                                                     
         MVI   ASKLN,ASKLNQ                                                     
         MVI   ASKSEQN,0                                                        
         MVC   ASKKEY,TRNKEY                                                    
*                                                                               
         GOTO1 AELADD,BCPARM,(C'E',TBARECD),ASKELD                              
         POP   USING                                                            
*                                                                               
ITEUPDX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE ITEM                                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ITECLO   DS    0H                                                               
         USING *,R8                                                             
         TM    GBINDS1,GBIDRFT                                                  
         BNZ   EXITY                                                            
         CP    GBITOTDR,GBITOTCR   ITEM MUST BALANCE                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOITE                                                        
         USING TBARECD,R2                                                       
         USING BIAELD,TBARFST                                                   
         ZAP   BIAAMT,GBIAMT                                                    
*                                                                               
         GOTO1 AOVRCALL,GB#ITEM_CLOSE_BEFORE                                    
         BNE   EXITERR                                                          
*                                                                               
         GOTO1 AIO,IOADD+IOACCMST+IOITEQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GBIDA,IODA                                                       
*                                                                               
         GOTO1 AOVRCALL,GB#ITEM_CLOSE_AFTER                                     
         BNE   EXITERR                                                          
*                                                                               
ITECLOX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD POSTINGS                                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PSTSADD  DS    0H                                                               
         USING *,R8                                                             
         OI    BCINDS1,BCIPADD     SET WITHIN PSTSADD                           
*                                  OVERLAY SHOULD CALL APSTADD/APSTATR          
         GOTO1 AOVRCALL,GB#POSTINGS_ADD                                         
         IPM   RE                  SAVE CONDITION CODE                          
         NI    BCINDS1,FF-BCIPADD                                               
         SPM   RE                  RESTORE CONDITION CODE                       
         BNE   EXITERR                                                          
*                                                                               
PSTSADDX DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD POSTING (CALLED BY OVERLAY)                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PAWORKD,RC                                                       
PSTADD   DS    0H                                                               
         USING *,R8                                                             
         TM    BCINDS1,BCIPADD     ENSURE CALLED VIA GB#POSTINGS_ADD            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    GBBINDSI,GBBISITM   SPLIT ITEM ?                                 
         BZ    PADD02                                                           
         TM    GBPINDS,GBPIUBKO    UPDATE BUCKET ONLY                           
         BO    PADD02              YES - OK                                     
         CLC   GBIPSTS,=AL2(MAXPSTS)                                            
         BNH   PADD02              SPLIT ITEM IF MORE THAN 35 POSTINGS          
         BAS   RE,SPLTITM                                                       
                                                                                
PADD02   CLI   GBPCCPY,0           CONTRA-COMPANY DEFAULTS TO CONNECTED         
         BNE   *+10                                                             
         MVC   GBPCCPY,CUABIN                                                   
*                                                                               
         GOTO1 AVALMOA,BCPARM,GBPMOAP,GBPMOAC  VALIDATE POSTING MOA             
         BNE   EXIT                                                             
*                                                                               
         LA    R4,PATAB            FIND PATAB ENTRY                             
         LA    R0,PATABN                                                        
         USING PATABD,R4                                                        
PADD04   CLC   PATPTYPE,GBPTYPE                                                 
         BE    PADD10                                                           
         AHI   R4,PATABL                                                        
         BCT   R0,PADD04                                                        
         DC    H'0'                                                             
*                                                                               
PADD10   DS    0H                                                               
         MVC   GBPTYPE,PAT1TYPE                                                 
         GOTO1 APSTTRN                                                          
         BNE   EXIT                                                             
*                                                                               
         CLI   PAT2TYPE,0          TEST FOR 2ND POSTING                         
         BE    PADD20                                                           
         LA    RE,PAPST                                                         
         LHI   RF,L'GBPST                                                       
         LA    R0,GBPST                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
O        USING GBPST,PAPST         PAPST = ORGINAL GBPST VALUES                 
*                                                                               
         TM    PAT2INDS,PAT2INEG   TEST NEGATE AMOUNT                           
         BZ    *+16                                                             
         ZAP   GBPAMNT,BCPZERO                                                  
         SP    GBPAMNT,O.GBPAMNT                                                
*                                                                               
         TM    PAT2INDS,PAT2ICTR   TEST SWAP ACCOUNTS                           
         BZ    PADD12                                                           
         CLC   GBPCCPY,CUABIN      NOT VALID IF CONTRA ON ANOTHER COMP          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    GBPINDS,GBPICDUM    NOT VALID IF CONTRA IS DUMMY A/C             
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   GBPULA,O.GBPULC                                                  
         MVC   GBPULC,O.GBPULA                                                  
*                                                                               
PADD12   DS    0H                                                               
         MVC   GBPTYPE,PAT2TYPE                                                 
         GOTO1 APSTTRN                                                          
         BNE   EXIT                                                             
*                                                                               
         LA    RE,GBPST            RESTORE TO ORGINAL VALUES                    
         LHI   RF,L'GBPST                                                       
         LA    R0,O.GBPST                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
PADD20   DS    0H                                                               
         NI    GBPINDS,FF-GBPICDUM ALWAYS RESET CONTRA IS DUMMY A/C             
         TM    GBPINDS,GBPIKXEL    TEST KEEP EXTRA ELS                          
         BNZ   PSTADDX                                                          
         ICM   RF,15,GBPAXELS      NO - RESET FIRST ELEMENT CODE                
         BZ    PSTADDX                                                          
         MVI   0(RF),0                                                          
*                                                                               
PSTADDX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* INITIALISE ITEM RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
SPLTITM  NTR1  ,                                                                
         TM    GBINDS1,GBIDRFT                                                  
         BNZ   SITM04                                                           
         L     R2,AIOITE                                                        
         USING TBARECD,R2                                                       
         USING BIAELD,TBARFST                                                   
         ZAP   BIAAMT,GBIAMT                                                    
*                                                                               
         GOTO1 AIO,IOADD+IOACCMST+IOITEQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   GBIDA,IODA                                                       
         AP    GBBAMT,GBIAMT       UPDATE BATCH TOTAL AMOUNT                    
*                                                                               
SITM04   XC    GBIPSTS,GBIPSTS                                                  
         ZAP   GBIAMT,BCPZERO                                                   
*                                                                               
         LH    RE,GBBITEMS         INCREMENT ITEM COUNT                         
         AHI   RE,1                                                             
         STH   RE,GBBITEMS                                                      
*                                                                               
         GOTO1 AINIIREC            INITIALISE ITEM RECORD                       
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
PAWORKD  DSECT                     ** PSTADD LOCAL W/S **                       
PAPST    DS    XL(L'GBPST)         ORIGINAL GBPST VALUES                        
PAWORKL  EQU   *-PAWORKD                                                        
         SPACE 1                                                                
PATABD   DSECT                     ** PSTADD TYPE TABLE **                      
PATPTYPE DS    XL1                 ORIGINAL GBPTYPE VALUE                       
PAT1TYPE DS    XL1                 GBPTYPE FOR 1ST POSTING                      
PAT2TYPE DS    XL1                 GBTYPE FOR 2ND POSTING OR 0                  
PAT2INDS DS    XL1                 INDICATORS FOR 2ND POSTING                   
PAT2INEG EQU   X'80'               NEGATE ORIGINAL GBPAMNT POSTING              
PAT2ICTR EQU   X'40'               CONTRA POSTING (SWAP A/C AND C/A)            
PATABL   EQU   *-PATABD                                                         
*                                                                               
ACGENBAT CSECT                                                                  
PATAB    DS    0XL(PATABL)                                                      
         DC    AL1(GBPTCR),AL1(GBPTCR),AL1(0,0)                                 
         DC    AL1(GBPTDR),AL1(GBPTDR),AL1(0,0)                                 
         DC    AL1(GBPTCRDR),AL1(GBPTCR),AL1(GBPTDR,PAT2ICTR)                   
         DC    AL1(GBPTDRCR),AL1(GBPTDR),AL1(GBPTCR,PAT2ICTR)                   
         DC    AL1(GBPTCRCR),AL1(GBPTCR),AL1(GBPTCR,PAT2INEG)                   
         DC    AL1(GBPTDRDR),AL1(GBPTDR),AL1(GBPTDR,PAT2INEG)                   
PATABN   EQU   (*-PATAB)/PATABL                                                 
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ADD POSTING TRANSACTION                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PSTTRN   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         GOTO1 APSTBLD             BUILD THE TRANSACTION POSTING                
         BNE   EXIT                                                             
         GOTO1 AADTUPD                                                          
         BNE   EXIT                                                             
         GOTO1 AITEUPD                                                          
         BNE   EXIT                                                             
*                                                                               
PSTTRNX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TRANSACTION POSTING                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PSTBLD   DS    0H                                                               
         USING *,R8                                                             
         L     R2,GBAIOTRN                                                      
         USING TRNRECD,R2                                                       
*                                                                               
         BAS   RE,BTRNKEY          BUILD RECORD KEY                             
         BAS   RE,BTRNEL           BUILD TRNELD                                 
         BAS   RE,BTIDEL           BUILD TERMINAL ID                            
         BAS   RE,BPIDEL           BUILD PERSON ID                              
         BAS   RE,BFFTET           BUILD ETYPE FFT                              
         BAS   RE,BFFTEN                                                        
*                                                                               
         BAS   RE,BXELS            BUILD EXTRA ELEMENTS                         
*                                                                               
         TM    GBINDS1,GBILIVE     TEST LIVE RUN                                
         BZ    PBLD02              YES - ADD ATTRIBUTE ELEMENT(S)               
         GOTO1 AATRBLD,BCPARM,TRNRECD                                           
*                                                                               
PBLD02   DS    0H                                                               
         GOTO1 BFFTCLPR            BUILD MEMO CLIENT/PRODUCT                    
*                                                                               
PSTBLDX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* BUILD KEY / INITIALISE RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
BTRNKEY  NTR1  ,                                                                
         XC    TRNRECD(TRNRFST+1-TRNRECD),TRNRECD                               
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKULA,GBPULA                                                   
         CLI   TRNKACT,C' '                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNKWORK,BCSPACES                                                
         CLC   BCCPYPRD,TRNKULA                                                 
         BNE   *+10                                                             
         MVC   TRNKWORK,GBPWORK                                                 
         MVC   TRNKCULC,GBPCULC                                                 
         CLI   TRNKCCPY,0                                                       
         BNE   *+10                                                             
         MVC   TRNKCCPY,CUABIN                                                  
         MVC   TRNKDATE,GBPDATE                                                 
         MVC   TRNKREF,GBPREF                                                   
         MVC   TRNKSBR,GBPSBR                                                   
*                                                                               
         LHI   RF,TRNRFST+1-TRNRECD                                             
         STH   RF,TRNRLEN                                                       
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD AND ADD TRNEL                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,BCELEM                                                    
BTRNEL   NTR1  ,                                                                
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVC   TRNSUB,TRNKSBR                                                   
         MVC   TRNTYPE,GBBTYP                                                   
         CLI   GBPOBTYP,0                                                       
         BE    *+10                                                             
         MVC   TRNTYPE,GBPOBTYP                                                 
         MVC   TRNSTAT,GBPSTAT                                                  
         NI    TRNSTAT,FF-TRNSDR                                                
         CLI   GBPTYPE,GBPTDR                                                   
         BNE   *+8                                                              
         OI    TRNSTAT,TRNSDR                                                   
         ZAP   TRNAMNT,GBPAMNT                                                  
         MVC   TRNOFFC,TRNKOFF                                                  
         CLC   BCCPYPRD,TRNKULA                                                 
         BE    *+10                                                             
         MVC   TRNOFFC,GBPOFFC                                                  
*                                                                               
         MVC   TRNMOS,GBPMOAC                                                   
         MVC   TRNBREF,GBBREF                                                   
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,GBPNARRL       TEST LENGTH OF NARRATIVE SET                 
         BNZ   BTRNEL04                                                         
         LHI   RE,L'GBPNARR        CALCULATE LENGTH OF NARRATIVE                
         LA    RF,GBPNARR+L'GBPNARR-1                                           
BTRNEL02 CLI   0(RF),C' '                                                       
         BH    BTRNEL04                                                         
         AHI   RF,-1                                                            
         BCT   RE,BTRNEL02                                                      
BTRNEL04 MVC   TRNNARR,GBPNARR                                                  
         LR    RF,RE                                                            
         AHI   RE,TRNLN1Q                                                       
         STC   RE,TRNLN                                                         
         LA    R1,TRNNARR                                                       
         CHI   RF,3                                                             
         BL    BTRNEL12                                                         
         SHI   RF,2                                                             
BTRNEL06 CLC   NLSYMB,0(R1)                                                     
         BE    BTRNEL08                                                         
         CLC   TBSYMB,0(R1)                                                     
         BNE   BTRNEL10                                                         
         MVC   0(L'TBSYMB,R1),BCSPACES                                          
         B     BTRNEL10                                                         
BTRNEL08 MVC   0(L'NLSYMB,R1),BCSPACES                                          
BTRNEL10 AHI   R1,1                                                             
         BCT   RF,BTRNEL06                                                      
BTRNEL12 GOTO1 AELADD,BCPARM,TRNRECD,TRNELD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD AND ADD TIDEL                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TIDELD,BCELEM                                                    
BTIDEL   NTR1  ,                                                                
         MVI   TIDEL,TIDELQ                                                     
         MVI   TIDLN,TIDLNQ                                                     
         MVC   TID,CUTSYM                                                       
         GOTO1 AELADD,BCPARM,TRNRECD,TIDELD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD AND ADD PIDEL                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING PIDELD,BCELEM                                                    
BPIDEL   NTR1  ,                                                                
         XC    PIDELD(PIDLNQ),PIDELD                                            
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,CUPASS                                                     
         GOTO1 AELADD,BCPARM,TRNRECD,PIDELD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD AND ADD FFTEL FOR EXP. TYPE                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,BCELEM                                                    
BFFTET   NTR1  ,                                                                
         CLC   GBPETYP,BCSPACES                                                 
         BNH   EXIT                                                             
         XC    FFTELD(FFTLN1Q+L'GBPETYP+L'FFTDLEN+1),FFTELD                     
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'GBPETYP+L'FFTDLEN                                
         MVI   FFTTYPE,FFTTEXTY                                                 
         MVI   FFTDLEN,L'GBPETYP                                                
         MVC   FFTDATA(L'GBPETYP),GBPETYP                                       
         GOTO1 AELADD,BCPARM,TRNRECD,FFTELD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD AND ADD FFTEL FOR ESTIMATE NUMBER                             *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,BCELEM                                                    
BFFTEN   NTR1  ,                                                                
         CLC   GBPEGNO,BCSPACES                                                 
         BNH   EXIT                                                             
         XC    FFTELD(FFTLN1Q+L'FFTESTN+L'FFTOESTN+L'FFTDLEN+1),FFTELD          
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTESTN+L'FFTOESTN+L'FFTDLEN                     
         MVI   FFTTYPE,FFTTESTN                                                 
         MVI   FFTDLEN,L'FFTESTN+L'FFTOESTN                                     
         MVC   FFTESTN,GBPEGNO                                                  
         MVC   FFTOESTN,BCSPACES                                                
         GOTO1 AELADD,BCPARM,TRNRECD,FFTELD                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ADD EXTRA ELEMENTS                                                  *         
***********************************************************************         
         SPACE 1                                                                
BXELS    NTR1  ,                                                                
         ICM   R3,15,GBPAXELS                                                   
         BZ    EXIT                                                             
         XR    R5,R5                                                            
BXELS02  CLI   0(R3),0                                                          
         BE    BXELS10                                                          
*&&UK                                                                           
         CLI   0(R3),XDFELQ                                                     
         BE    BXELS04                                                          
         GOTO1 AELADD,BCPARM,TRNRECD,(R3)                                       
         B     BXELS06                                                          
*&&                                                                             
BXELS04  GOTO1 AELADD,BCPARM,(C'E',TRNRECD),(R3)                                
BXELS06  IC    R5,1(R3)                                                         
         BXH   R3,R5,BXELS02                                                    
*                                                                               
BXELS10  DS    0H                                                               
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD AND ADD MEMO CLIENT/PRODUCT ELEMENT                           *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,BCELEM                                                    
BFFTCLPR NTR1  ,                                                                
         CLI   GBPMEMCL,C' '       TEST HAVE MEMO CLIENT                        
         BNH   EXIT                                                             
         CLC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         BE    EXIT                NO NEED IF IS SJ POSTING                     
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,GBPMEMCL                                                 
         MVC   FFTPRAC,BCSPACES                                                 
         CLI   GBPMEMPR,C' '       TEST HAVE MEMO PRODUCT                       
         BNH   *+10                                                             
         MVC   FFTPRAC,GBPMEMPR                                                 
         GOTO1 AELADD,BCPARM,TRNRECD,FFTELD                                     
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE ADDTRN CONTROL BLOCK                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ADTOPN   DS    0H                                                               
         USING *,R8                                                             
         L     R3,GBAADTBK         INITIALISE ADDTRN CONTROL BLOCK              
         USING ADDTRND,R3          R3=A(ADDTRN BLOCK)                           
         LR    R0,R3                                                            
         LA    R1,TRNBLKL                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI     TRNINDS,TRNICONV                                                
         TM     GBINDS1,GBIDRFT                                                 
         BZ     *+8                                                             
         OI     TRNINDS,TRNIWRNO                                                
*                                                                               
         MVI   TRNMODE,TRNMONLN                                                 
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   *+8                                                              
         MVI   TRNMODE,TRNMOFLN                                                 
         MVC   TRNCTRY,CUCTRY      COUNTRY                                      
         MVC   TRNCOMF,ACOM        A(COMMON FACILITIES)                         
         MVC   TRNCPYS1,BCCPYST1                                                
         MVC   TRNCPYS2,BCCPYST2                                                
         MVC   TRNCPYS3,BCCPYST3                                                
         MVC   TRNCPYS4,BCCPYST4                                                
         MVC   TRNCPYS5,BCCPYST5                                                
         MVC   TRNCPYS6,BCCPYST6                                                
         MVC   TRNCPYS7,BCCPYST7                                                
         MVC   TRNCPYS8,BCCPYST8                                                
         MVC   TRNCPYS9,BCCPYST9                                                
         MVC   TRNCPYSA,BCCPYSTA                                                
*&&UK*&& MVC   TRNCPYSB,BCCPYSTB                                                
*&&UK*&& MVC   TRNCPYSC,BCCPYSTC                                                
         MVC   TRNGLMOA,BCCPYGLM   GLMOA DATE                                   
         MVC   TRNREC,GBAIOTRN                                                  
         MVC   TRNACC,GBAIOACC                                                  
         MVC   TRNBUK,GBAIOBUK                                                  
         MVC   TRNCAC,GBAIOCAC                                                  
*&&US*&& MVC   TRNOFA,GBAIOOFA                                                  
         XC    TRNOFA,TRNOFA                                                    
         MVC   TRNCCCUR,BCCPYCUR                                                
         MVC   TRNCCURS,BCCPYSEC                                                
*&&UK*&& MVC   TRNTOBA,VTOBACCO                                                 
*                                                                               
         MVC   TRNLDG,GBALDGT      A(LEDGER TABLE)                              
         MVI   TRNLDG,0                                                         
         MVC   TRN#LDGS,GBLDGTN    NUMBER LEDGER TABLE ENTRIES                  
         LA    RE,BCPALBLK                                                      
         STCM  RE,15,TRNPAL        A(P&L BUCKET AREA)                           
         MVC   TRNUDATE,BCTODAYC   SET TODAY'S DATE AS FOUND                    
         MVC   TRNPUSER,CUUSER     SET USER ID NUMBER                           
*                                                                               
*&&UK                                                                           
         TM    BCCPYSTB,CPYS1CAM   TEST UPDATE METHD BKTS WITH 1C ADJS.         
         BZ    ADTOPNX             NO                                           
         LA    RF,BCDUM1RK                                                      
         STCM  RF,15,TRNAD1R       SET A(DUMMY 1R KEY)                          
         L     RF,GBABFMBK         ENSURE BUFFER IS EMPTY                       
         XC    0(4,RF),0(RF)                                                    
         STCM  RF,15,TRNMBK                                                     
*&&                                                                             
*                                                                               
ADTOPNX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD POSTING VIA ADDTRN                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ADTUPD   DS    0H                                                               
         USING *,R8                                                             
         L     R3,GBAADTBK                                                      
         USING ADDTRND,R3                                                       
         L     R2,GBAIOTRN                                                      
         USING TRNRECD,R2                                                       
         TM    GBINDS1,GBIDRFT                                                  
         BZ    AUPD02                                                           
         TM    TRNINDS,TRNIWRNO                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
AUPD02   DS    0H                                                               
         LH    RE,GBBPSTS          UPDATE TOTAL NUMBER OF POSTINGS              
         AHI   RE,1                                                             
         STH   RE,GBBPSTS                                                       
         LH    RE,GBIPSTS          UPDATE ITEM'S NUMBER OF POSINGS              
         AHI   RE,1                                                             
         STH   RE,GBIPSTS                                                       
         STCM  RE,3,TRNBSEQN       SET SEQUENCE NUMBER WITHIN ITEM              
*                                                                               
         TM    GBPINDS,GBPICDUM    TEST DUMMY C/A                               
         BZ    AUPD04                                                           
         MVC   TRNCACNM,GBPDCNAM   YES - USE GIVEN NAME                         
         CLI   TRNCACNM,C' '       TEST NEED TO TRANSLATE                       
         BNL   AUPD06                                                           
         GOTO1 VDICTATE,BCPARM,C'T   ',(L'TRNCACNM,TRNCACNM)                    
         B     AUPD06                                                           
*                                                                               
AUPD04   DS    0H                  LOOK UP C/A NAME                             
         GOTO1 GB_AACTVAL,BCPARM,(X'01',TRNKCULC)                               
         BNE   EXIT                                                             
         MVC   TRNCACNM,GBANAME                                                 
*                                                                               
AUPD06   DS    0H                  SET DEFAULT TRNINDS1/2                       
         MVI   TRNINDS1,TRNIVDAT   DON'T TEST TRANSACTION DATE                  
*&&UK                                                                           
         TM    BCCPYSTB,CPYS1CAM   TEST UPDATE METHD BKTS WITH 1C ADJS.         
         BZ    *+8                                                              
         OI    TRNINDS1,TRNI1CAM                                                
*&&                                                                             
         MVI   TRNINDS2,0                                                       
         TM    GBPINDS,GBPIUBKO    UPDATE BUCKET ONLY                           
         BZ    *+8                                                              
         OI    TRNINDS2,TRNIUBKO                                                
*                                                                               
         NI    TRNINDS,FF-TRNIDUCL                                              
         TM    GBPINDS,GBPICLLK    TEST CLOSED/LOCKED A/C S ALLOWED             
         BNZ   *+8                                                              
         OI    TRNINDS,TRNIDUCL                                                 
*                                                                               
         GOTO1 AOVRCALL,GB#POSTING_ADD_BEFORE                                   
         BNE   EXITERR                                                          
*                                                                               
         GOTO1 ATRSSET             ENSURE TRSELD/TRSSPROG ON RECORD             
*                                                                               
         OI    TRNINDS2,TRNIADDG                                                
         GOTO1 VADDTRN,ADDTRND                                                  
         BE    AUPD08                                                           
         CLI   TRNERRS,TRNEACCI    INVALID ACCOUNT                              
         BNE   *+14                                                             
         MVC   GBEMSGNO,=AL2(AE$INACC)                                          
         B     ADTUPDN                                                          
         CLI   TRNERRS,TRNEOFAI    INVALID OFFICE ACCOUNT                       
         BNE   *+14                                                             
         MVC   GBEMSGNO,=AL2(AE$IVOFF)                                          
         B     ADTUPDN                                                          
         CLI   TRNERRS,TRNEACOL    ACCOUNT IS CLOSED OR LOCKED                  
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY OTHER ERRORS                      
         L     RF,TRNACC                                                        
         USING ACTRECD,RF                                                       
         MVC   GBEMSGNO,=AL2(AE$ACTLK)                                          
         TM    ACTRSTAT,ACTSLOCK                                                
         BNZ   ADTUPDN                                                          
         MVC   GBEMSGNO,=AL2(AE$ACTCL)                                          
         TM    ACTRSTAT,ACTSCLOS                                                
         BNZ   ADTUPDN                                                          
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
AUPD08   DS    0H                                                               
         GOTO1 AACBGET,BCPARM,TRNKCULA  TEST A/C ALREADY IN BUFFER              
         BNE   *+12                                                             
         L     R4,4(R1)                                                         
         B     AUPD10                                                           
         L     RF,TRNACC                                                        
         GOTO1 AACBPUT,BCPARM,(RF)      NO - ADD IT                             
         BNE   EXIT                                                             
         LH    R4,BCACBCNT                                                      
*                                                                               
AUPD10   DS    0H                                                               
         GOTO1 AATRADD,BCPARM,(R4) ADD TO ATTRIBUTE LIST                        
         CLC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         BNE   AUPD12              TEST POSTING TO PRODUCTION                   
         XR    R4,R4               YES - ADD W/C TO LIST                        
         ICM   R4,3,TRNKWORK                                                    
         GOTO1 (RF),(R1),(C'W',(R4))                                            
*                                                                               
AUPD12   DS    0H                                                               
         GOTO1 AOVRCALL,GB#POSTING_ADD_AFTER                                    
         BNE   EXITERR                                                          
*                                                                               
ADTUPDX  DS    0H                                                               
         B     EXITY                                                            
*                                                                               
ADTUPDN  DS    0H                                                               
         MVC   GBEXTRA(L'TRNKULA),TRNKULA                                       
         B     EXITERR                                                          
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE ADDTRN                                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ADTCLO   DS    0H                                                               
         USING *,R8                                                             
         L     R1,GBAADTBK                                                      
         USING ADDTRND,R1                                                       
         OI    TRNINDS,TRNILAST                                                 
         OI    TRNINDS2,TRNIUPDG   UPDATE UNIT G                                
         GOTO1 VADDTRN                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE TIME POSTINGS                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TIMCRT   DS    0H                                                               
         USING *,R8                                                             
         TM    GBACTIND,GBACTIDR                                                
         BZ    *+6                                                              
         DC    H'0'                NO DRAFT MODE FOR TIME                       
         OI    GBINDS1,GBILIVE                                                  
*                                                                               
         GOTO1 ABATOPN                                                          
         BNE   EXIT                                                             
*                                                                               
         OI    BCINDS1,BCITCRT                                                  
         GOTO1 AOVRCALL,GB#TIME_ADD                                             
         IPM   RE                                                               
         NI    BCINDS1,FF-BCITCRT                                               
         SPM   RE                                                               
         BNE   EXITERR                                                          
*                                                                               
         OC    GBBITEMS,GBBITEMS   ENSURE POSTINGS/ITEMS ADDED                  
         BNZ   *+14                                                             
         MVC   GBEMSGNO,=AL2(AE$NTGUP) NOTHING TO UPDATE                        
         B     EXITERR                                                          
*                                                                               
         GOTO1 AITECLO             CLOSE LAST ITEM                              
         BNE   EXIT                                                             
*                                                                               
         GOTO1 ABATCLO                                                          
         BNE   EXIT                                                             
*                                                                               
TIMCRTX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD POSTINGS VIA TIMETRN (CALLED BY OVERLAY)                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TIMADD   DS    0H                                                               
         USING *,R8                                                             
         TM    BCINDS1,BCITCRT     ENSURE CALLED VIA GB#TIME_ADD                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    BCINDS1,BCITADD     TEST ALREADY IN TIMADD                       
         BNZ   TADD10              YES - THIS IS 'ADDTRN' CALL                  
*                                                                               
         L     R7,GBATIMED                                                      
         USING TTRND,R7            R7 = A(ACTIMETRND)                           
         MVC   BCFULL,TTADDTRN     SAVE A(ADDTRN) (IN CASE NOT VADDTRN)         
         L     RF,GB_ATIMADD                                                    
         ST    RF,TTADDTRN                                                      
         OI    BCINDS1,BCITADD                                                  
         GOTO1 GBATIME,BCPARM,TTRND                                             
         NI    BCINDS1,FF-BCITADD                                               
         MVC   TTADDTRN,BCFULL                                                  
         CLI   TTRETURN,0          TEST FOR ANY ERRORS                          
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
TADD10   DS    0H                  'ADDTRN' CALL VIA ACTIMETRN                  
         LR    R3,R1               R3 = A(ADDTRN BLOCK)                         
         USING ADDTRND,R3                                                       
         ST    R3,GBAADTBK         SAVE A(ADDTRN BLOCK)                         
         MVC   GBAIOTRN,TRNREC     SAVE A(TRANSACTION RECORD)                   
         L     R7,GBATIMED                                                      
         USING TTRND,R7                                                         
*                                                                               
         TM    TRNINDS2,TRNIUBKO   TEST UPDATING BUCKETS ONLY                   
         BZ    TADD12                                                           
         L     RF,BCFULL           YES - JUST DO ADDTRN CALL                    
         GOTO1 (RF),ADDTRND                                                     
         BE    EXIT                                                             
         DC    H'0'                ACTIMETRN WOULD DUMP ANYWAY                  
*                                                                               
TADD12   DS    0H                                                               
         MVC   GBAIOTRN,TRNREC     SAVE A(TRANSACTION RECORD)                   
         GOTO1 ATRSSET             ENSURE TRSELD/TRSSPROG ON RECORD             
         TM    TTSTAT1,TTSTFITE    TEST FIRST FOR ITEM                          
         BZ    TADD16                                                           
         NI    TTSTAT1,FF-TTSTFITE                                              
         OC    GBBITEMS,GBBITEMS   TEST PREVIOUS ITEM NEEDS CLOSING             
         BZ    TADD14                                                           
         GOTO1 AITECLO                                                          
         BE    *+6                                                              
         DC    H'0'                ACTIMETRN WOULD DUMP ANYWAY                  
TADD14   DS    0H                                                               
         GOTO1 AITEOPN             OPEN NEW ITEM                                
         BE    *+6                                                              
         DC    H'0'                ACTIMETRN WOULD DUMP ANYWAY                  
         ZAP   GBIAMT,TTITEAMT                                                  
*                                                                               
TADD16   DS    0H                  ADDTRN CALL                                  
         L     RF,BCFULL                                                        
         GOTO1 (RF),ADDTRND                                                     
         BE    *+6                                                              
         DC    H'0'                ACTIMETRN WOULD DUMP ANYWAY                  
*                                                                               
         GOTO1 AITEUPD                                                          
         BE    *+6                                                              
         DC    H'0'                ACTIMETRN WOULD DUMP ANYWAY                  
*                                                                               
TIMADDX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ENSURE TRSELD IS ON TRANSACTION & SET TRSPROG VALUE      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TRSSET   DS    0H                                                               
         USING *,R8                                                             
         L     R2,GBAIOTRN                                                      
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING TRSELD,R3           LOCATE EXISTING TRSELD                       
         XR    RF,RF                                                            
TSET02   CLI   TRSEL,TRSELQ                                                     
         BE    TSET10                                                           
         CLI   TRSEL,0                                                          
         BE    *+12                                                             
         IC    RF,TRSLN                                                         
         BXH   R3,RF,TSET02                                                     
*                                                                               
         LA    R3,BCELEM           NOT FOUND - ADD NEW ONE                      
         XC    TRSELD(TRSLNQ),TRSELD                                            
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         GOTO1 AELADD,BCPARM,TRNRECD,TRSELD                                     
         L     R3,8(R1)                                                         
*                                                                               
TSET10   DS    0H                                                               
         CLI   TRSLN,TRSLNQ        DIE ON BAD TRSELD                            
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&UK*&& OI    TRSSTAT5,TRSSGBAT   UPDATED BY ACGENBAT, ON=SKIP BY ACBG         
*                                                                               
*                                  CONVERT SYSTEM/PROGRAM TO TRSSPROG           
         LA    R4,PRGTAB                                                        
TSET12   CLC   CUOVSPG,0(R4)                                                    
         BE    TSET14                                                           
         LA    R4,L'PRGTAB(R4)                                                  
         CLI   0(R4),EOT                                                        
         BNE   TSET12                                                           
         B     EXITY                                                            
*                                                                               
TSET14   DS    0H                                                               
         MVC   BCBYTE1,TRSSTAT3                                                 
         NI    TRSSTAT3,FF-TRSSPROG                                             
         OC    TRSSTAT3,2(R4)                                                   
         B     EXITY                                                            
*                                                                               
PRGTAB   DS    0XL3                                                             
         DC    X'0624',AL1(TRSSMCS) ACC/MCS                                     
         DC    AL1(EOT)                                                         
         DS    0H                                                               
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ATTRIBUTE VALUES TO LIST                                        *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'W' IF PASSING WORKCODE                          *         
*             2-3 = A/C SEQ# IF BYTE 0 = 0                            *         
*             2-3 = WORKCODE IF BYTE 0 = C'W'                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ATRADD   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         USING ATLSTD,BCWORK                                                    
         MVC   ATSEQ,2(R1)                                                      
         MVI   ATINDS,0                                                         
         CLI   0(R1),C'W'          TEST PASSED WORKCODE                         
         BNE   *+8                                                              
         OI    ATINDS,ATIWC                                                     
*                                                                               
         TM    BCATINDS,BCATIITE   TEST START OF ITEM                           
         BZ    *+12                                                             
         OI    ATINDS,ATIITE+ATIBLK                                             
         NI    BCATINDS,FF-BCATIITE                                             
*                                                                               
         TM    BCATINDS,BCATIBLK   TEST START OF BLOCK                          
         BZ    *+12                                                             
         OI    ATINDS,ATIBLK                                                    
         NI    BCATINDS,FF-BCATIBLK                                             
*                                                                               
         L     RF,GBAIOTRN                                                      
         USING TRNRECD,RF                                                       
         USING TRNELD,TRNRFST                                                   
         TM    TRNSTAT,TRNSDR      TEST DEBIT/CREDIT                            
         BZ    *+8                                                              
         OI    ATINDS,ATIDR                                                     
         DROP  RF                                                               
*                                                                               
         L     R3,BCAATLST                                                      
         TM    GBINDS1,GBILIVE     TEST LIVE RUN                                
         BZ    ATRADD02                                                         
         CLC   ATLSTD(ATLSTL),0(R3) YES - SHOULD BE AS PER DRAFT RUN            
         BE    ADRADDX                                                          
         DC    H'0'                                                             
*                                                                               
ATRADD02 DS    0H                                                               
         MVC   0(ATLSTL,R3),ATLSTD NO - ADD NEW ENTRY                           
         MVI   ATLSTL(R3),ATIEOL                                                
*                                                                               
ADRADDX  DS    0H                  SET A(NEXT BUFFER ENTRY)                     
         AHI   R3,ATLSTL                                                        
         ST    R3,BCAATLST                                                      
         S     R3,GBAATBUF                                                      
         CH    R3,GBLATBUF                                                      
         BL    EXITY                                                            
         DC    H'0'                NEED BIGGER BUFFER                           
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD ATTRIBUTE ELEMENT(S) AND ADD TO RECORD                        *         
*                                                                     *         
* NTRY: P1 = A(TRANSACTION RECORD)                                    *         
* EXIT: APEELD ADDED TO RECORD                                        *         
*       FFTELD (TYPE FFTTWRKJ) ADDED TO RECORD IF APPLICABLE          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ABWORKD,RC                                                       
ATRBLD   DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
*                                                                               
         XC    BCELEM,BCELEM                                                    
         LA    R4,TRNRFST                                                       
         USING APEELD,R4                                                        
         XR    RF,RF                                                            
ABLD04   CLI   APEEL,0                                                          
         BE    ABLD08                                                           
         IC    RF,APELN            ELEMENT LENGTH                               
         CLI   APEEL,APEELQ        ANALYSIS ACCOUNT ELEMENT                     
         BE    *+8                                                              
         BXH   R4,RF,ABLD04                                                     
                                                                                
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BCELEM(0),APEELD     COPY THE APEEL                              
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('APEELQ',TRNRECD),0                 
         DROP  R4                                                               
*                                                                               
ABLD08   L     R3,BCAATLST                                                      
         USING ATLSTD,R3                                                        
         LR    R0,R3               SAVE A(CURRENT POSTING A/C)                  
         TM    ATINDS,ATIBLK       FIND START OF ITEM BLOCK                     
         BNZ   *+12                                                             
         SHI   R3,ATLSTL                                                        
         B     *-12                                                             
*                                                                               
         USING APEELD,BCELEM                                                    
         OC    BCELEM,BCELEM       ANY APEEL FROM ORIGINAL TX?                  
         BZ    ABLD12              NO - BUILD A NEW ONE                         
         XR    R4,R4                                                            
         IC    R4,APELN            ELEMENT LENGTH                               
         LA    R4,APEELD(R4)       POINTS TO THE END OF ELEMENT                 
         B     ABLD16                                                           
                                                                                
ABLD12   MVI   APEEL,APEELQ                                                     
         MVI   APENUM,0                                                         
         LA    R4,APENTRY                                                       
         USING APENTRY,R4                                                       
         USING FFTELD,ABFFTEL                                                   
*                                                                               
ABLD16   DS    0H                                                               
         TM    ATINDS,ATIWC        TEST W/C ENTRY                               
         BZ    ABLD20                                                           
         XR    RE,RE               YES - ADD W/C TO FFTELD                      
         IC    RE,FFTDLEN                                                       
         LA    RF,FFTDATA(RE)                                                   
         MVC   0(L'ATSEQ,RF),ATSEQ                                              
         AHI   RE,L'ATSEQ                                                       
         STC   RE,FFTDLEN                                                       
         B     ABLD24                                                           
*                                                                               
ABLD20   DS    0H                                                               
         CR    R3,R0               DON'T ADD A/C IF CURRENT POSTING             
         BE    ABLD24                                                           
         MVI   APENSTAT,0                                                       
         TM    ATINDS,ATIDR                                                     
         BZ    *+8                                                              
         OI    APENSTAT,APENSDR                                                 
         XR    RF,RF                                                            
         ICM   RF,3,ATSEQ                                                       
         GOTO1 AACBFND,BCPARM,(RF)                                              
         LM    R5,R6,4(R1)         R5 = L(A/CODE), R6 = A(A/CODE)               
         SHI   R5,1                R5 = L(A/CODE) - 1                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   APENACT(0),0(R6)                                                 
*&&US                                                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   TRNKULC(0),APENACT                                               
         BE    ABLD24              DON'T ADD A/C IF CURRENT CONTRA ACC          
*&&                                                                             
         AHI   R5,APENACT-APENTRY+1                                             
         STC   R5,APENLEN                                                       
*                                                                               
         SHI   R5,1                R5 = L(NEW ENTRY) - 1                        
         XR    RE,RE                                                            
         XR    R1,R1                                                            
         ICM   R1,1,APENUM         NUMBER OF ENTRIES                            
         BZ    ABLD23                                                           
         LA    RF,BCELEM+APELN1Q   RF POINTS TO THE FIRST ENTRY                 
ABLD22   EX    R5,*+8                                                           
         BE    ABLD24              REMOVE DUPLICATE ENTRY                       
         CLC   APENTRY(0),0(RF)                                                 
         IC    RE,0(,RF)           L(CURRENT ENTRY)                             
         AR    RF,RE               BUMP TO NEXT ENTRY                           
         BCT   R1,ABLD22                                                        
*                                                                               
ABLD23   LA    R4,1(R5,R4)         BUMP R4 TO NEXT APENTRY                      
         IC    RE,APENUM           INCREMENT NUMBER OF SUB ELEMENTS             
         AHI   RE,1                                                             
         STC   RE,APENUM                                                        
*                                                                               
ABLD24   DS    0H                                                               
         AHI   R3,ATLSTL           BUMP TO NEXT ATLSTD ENTRY                    
         TM    ATINDS,ATIBLK+ATIEOL TEST WITHIN SAME BLOCK                      
         BZ    ABLD16                                                           
*                                                                               
         LA    RE,APEELD                                                        
         SR    R4,RE                                                            
         STC   R4,APELN            SET ELEMENT LENGTH                           
*                                                                               
         GOTO1 AELADD,BCPARM,TRNRECD,APEELD                                     
         XR    RE,RE                                                            
         ICM   RE,1,FFTDLEN        TEST ANY WORKCODES                           
         BZ    ATRBLDX             YES - TEST PRODUCTION POSTING                
         CLC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         BE    ATRBLDX                                                          
         MVI   FFTEL,FFTELQ        NO - ADD WORKCODES TO POSTING                
         AHI   RE,FFTLN1Q+L'FFTDLEN                                             
         STC   RE,FFTLN                                                         
         MVI   FFTTYPE,FFTTWRKC                                                 
         GOTO1 (RF),(R1),,FFTELD                                                
*                                                                               
ATRBLDX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         SPACE 1                                                                
ABWORKD  DSECT                     ** ATRBLD LOCAL W/S **                       
ABFFTEL  DS    XL255               WORKCODE FFTELD                              
ABWORKL  EQU   *-ABWORKD                                                        
ACGENBAT CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT DETAILS FROM BUFFER                                     *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' ON IF PASSED A(BUFFER ENTRY)                *         
*             1-3 = A(XL15 COMPANY/U/L/ACC CODE)      IF X'80' IS OFF *         
*             1-3 = A(BUFFER ENTRY)                   IF X'80' IS ON  *         
*                                                                     *         
* EXIT:        CC = EQUAL IF FOUND                                    *         
*           GBACC = DETAILS FOR ACCOUNT                               *         
*              P2 = SEQUENCE NUMBER OF ACCOUNT                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING AGWORKD,RC                                                       
ACBGET   DS    0H                                                               
         USING  *,R8                                                            
         ST    R1,AGAR1                                                         
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)                                                       
         TM    0(R1),X'80'         TEST PASSED BUFFER ENTRY                     
         BNZ   AGET10                                                           
         MVC   AGPCULA,0(R4)                                                    
*                                                                               
         XR    R5,R5               R5 = SEQUENCE NUMBER                         
         L     R4,GBAACBUF                                                      
         USING ACBELD,R4                                                        
         XR    RF,RF                                                            
AGET02   CLI   ACBEL,ACBEOLQ                                                    
         BE    EXITN               CC = NOT EQUAL                               
         IC    RF,ACBLN                                                         
         CLI   ACBEL,ACBULAQ                                                    
         BNE   AGET08                                                           
         AHI   R5,1                                                             
         MVC   AGBULA,BCSPACES                                                  
         LR    RE,RF                                                            
         AHI   RE,-(ACBLNQ+1)                                                   
         BM    AGET04                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   AGBULA(0),ACBDATA                                                
AGET04   MVC   AGBCPY,CUABIN                                                    
         LA    R1,ACBELD(RF)       TEST FOR OVERRIDE COMPANY                    
         CLI   0(R1),ACBCPYQ                                                    
         BNE   *+10                                                             
         MVC   AGBCPY,ACBDATA-ACBELD(R1)                                        
*                                                                               
         CLC   AGBCULA,AGPCULA                                                  
         BE    AGET09                                                           
*                                                                               
AGET08   DS    0H                                                               
         BXH   R4,RF,AGET02                                                     
*                                                                               
AGET09   DS    0H                                                               
         L     R1,AGAR1                                                         
         ST    R5,4(R1)            RETURN SEQUENCE NUMBER TO CALLER             
*                                                                               
AGET10   DS    0H                  ACCOUNT FOUND IN BUFFER                      
         XC    GBACC,GBACC                                                      
         MVC   GBANAME,BCSPACES                                                 
         MVC   GBACULA,AGBCULA                                                  
         XR    RF,RF                                                            
         IC    RF,ACBLN                                                         
         AR    R4,RF               BUMP R4 TO NEXT ELEMENT                      
*                                                                               
AGET12   DS    0H                  EXTRACT REST OF DATA                         
         CLI   ACBEL,ACBEOLQ                                                    
         BE    ACBGETX                                                          
         CLI   ACBEL,ACBULAQ                                                    
         BE    ACBGETX                                                          
         IC    RF,ACBLN                                                         
         LR    RE,RF                                                            
         AHI   RE,-(ACBLNQ+1)      RE = EX. LENGTH OF DATA                      
         BM    AGET28                                                           
*                                                                               
         CLI   ACBEL,ACBNAMQ       TEST NAME                                    
         BNE   AGET14                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GBANAME(0),ACBDATA                                               
         B     AGET28                                                           
*                                                                               
AGET14   CLI   ACBEL,ACBCSTQ       TEST ANALYSIS COSTING                        
         BNE   AGET16                                                           
         MVC   GBACOST,ACBDATA                                                  
         B     AGET28                                                           
*                                                                               
AGET16   CLI   ACBEL,ACBINDQ       TEST INDICATOR BYTE                          
         BNE   AGET18                                                           
         MVC   GBAINDS1,ACBDATA                                                 
         B     AGET28                                                           
*                                                                               
AGET18   CLI   ACBEL,ACBXTVQ       TEST EXTRA VALUES                            
         BNE   AGET20                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GBAXVALS(0),ACBDATA                                              
         B     AGET28                                                           
*                                                                               
AGET20   DS    0H                  TEST EXTRA DATA                              
         XR    R1,R1                                                            
         IC    R1,ACBEL                                                         
         SHI   R1,ACBXTRQ+1                                                     
         BM    AGET28                                                           
         SLL   R1,2                * 4                                          
         LA    R1,GBAXL1(R1)                                                    
         AHI   RE,1                                                             
         STC   RE,0(R1)            SET L(EXTRA DATA)                            
         LA    R0,ACBLNQ(R4)                                                    
         STCM  R0,7,1(R1)          SET A(EXTRA DATA)                            
*                                                                               
AGET28   DS    0H                                                               
         BXH   R4,RF,AGET12                                                     
*                                                                               
ACBGETX  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
AGWORKD  DSECT                                                                  
AGAR1    DS    A                   A(CALLERS R1)                                
AGPCULA  DS    0XL15               ** PASSED ACCOUNT DETAILS **                 
AGPCPY   DS    XL1                 COMPANY CODE                                 
AGPULA   DS    0CL14               UNIT/LEDGER/ACCOUNT CODE                     
AGPUL    DS    0CL2                UNIT/LEDGER                                  
AGPUNT   DS    CL1                 UNIT CODE                                    
AGPLDG   DS    CL1                 LEDGER CODE                                  
AGPACT   DS    CL12                ACCOUNT CODE                                 
*                                                                               
AGBCULA  DS    0XL15               ** BUFFER ACCOUNT DETAILS **                 
AGBCPY   DS    XL1                 COMPANY CODE                                 
AGBUL    DS    0CL2                UNIT/LEDGER                                  
AGBULA   DS    0CL14               UNIT/LEDGER/ACCOUNT CODE                     
AGBUNT   DS    CL1                 UNIT CODE                                    
AGBLDG   DS    CL1                 LEDGER CODE                                  
AGBACT   DS    CL12                ACCOUNT CODE                                 
*                                                                               
AGWORKL  EQU   *-AGWORKD                                                        
ACGENBAT CSECT                                                                  
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PUT ACCOUNT DETAILS INTO BUFFER                                     *         
*                                                                     *         
* NTRY: P1 = A(ACCOUNT RECORD)                                        *         
* EXIT: ACCOUNT DETAILS ADDED TO BUFFER IN GBAACBUF                   *         
* BCACBCNT = SEQUENCE NUMBER OF ACCOUNT IN BUFFER                     *         
*       CC = NOT EQUAL IF BUFFER NOT BIG ENOUGH                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING APWORKD,RC                                                       
S        USING GBAREC,APAREC                                                    
         USING ACBELD,APELEM                                                    
ACBPUT   DS    0H                                                               
         USING  *,R8                                                            
         L     R2,0(R1)                                                         
         USING ACTRECD,R2          R2 = A(ACCOUNT RECORD)                       
*                                                                               
         XR    RF,RF                                                            
         L     R4,GBAACBUF                                                      
         CLI   0(R4),ACBEOLQ                                                    
         BE    *+12                                                             
         IC    RF,1(R4)                                                         
         BXH   R4,RF,*-12                                                       
*                                                                               
         ST    R4,APAFST           SET A(FIRST ELEMENT)                         
         ST    R4,APANXT           SET A(NEXT ELEMENT)                          
         L     R4,GBAACBUF                                                      
         AH    R4,GBLACBUF                                                      
         ST    R4,APAEOB           SET A(END OF BUFFER)                         
*                                                                               
         MVI   ACBEL,ACBULAQ       ADD U/L/ACCOUNT CODE ELEMENT                 
         MVC   ACBDATA(L'GBAULA),ACTKULA                                        
         LA    R0,L'ACTKULA                                                     
         LA    RF,ACTKULA+L'ACTKULA-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         AHI   R0,ACBLNQ                                                        
         STC   R0,ACBLN                                                         
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
*                                                                               
         CLC   ACTKCPY,CUABIN      TEST COMPANY IS AS CONNECTED                 
         BE    APUT00                                                           
         MVI   ACBEL,ACBCPYQ       NO = ADD COMPANY CODE ELEMENT                
         MVI   ACBLN,ACBLNQ+L'ACTKCPY                                           
         MVC   ACBDATA(L'GBACPY),ACTKCPY                                        
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
*                                                                               
APUT00   DS    0H                  PROCESS ELEMENTS                             
         LA    R3,ACTRFST                                                       
         XR    RF,RF                                                            
APUT02   CLI   0(R3),0                                                          
         BE    APUT20                                                           
         IC    RF,1(R3)                                                         
         USING NAMELD,R3                                                        
         CLI   NAMEL,NAMELQ        ADD ELEMENT FOR NAME                         
         BNE   APUT04                                                           
         LR    RE,RF                                                            
         SHI   RE,NAMLN1Q+1                                                     
         BM    APUT18                                                           
         MVI   ACBEL,ACBNAMQ                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACBDATA(0),NAMEREC                                               
         AHI   RE,ACBLNQ+1                                                      
         STC   RE,ACBLN                                                         
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
         DROP  R3                                                               
*                                                                               
APUT04   DS    0H                                                               
         USING RSTELD,R3           TEST FOR STATUS ELEMENT                      
         CLI   RSTEL,RSTELQ                                                     
         BNE   APUT10                                                           
         ST    R3,S.GBAARST                                                     
         TM    RSTSTAT1,RSTSACIC   TEST CLOSED                                  
         BZ    *+8                                                              
         OI    APAINDS1,GBAICLOS                                                
         TM    RSTSTAT1,RSTSACIL   TEST LOCKED                                  
         BZ    *+8                                                              
         OI    APAINDS1,GBAILOCK                                                
         TM    RSTSTAT1,RSTSEADD   TEST FLAGGED FOR DEPERTMENT                  
         BZ    *+8                                                              
         OI    APAINDS1,GBAIDEPT                                                
         TM    RSTSTAT1,RSTSGPEI   TEST FLAGGED FOR STAFF                       
         BZ    *+8                                                              
         OI    APAINDS1,GBAISTAF                                                
         MVC   S.GBASTAT1,RSTSTAT1                                              
         MVC   S.GBASTAT3,RSTSTAT3                                              
         CLI   RSTLN,RSTLN2Q                                                    
         BL    APUT06                                                           
         MVC   S.GBASTAT2,RSTSTAT2                                              
         MVC   S.GBASTAT4,RSTSTAT4                                              
         CLI   RSTLN,RSTLN3Q                                                    
         BL    APUT06                                                           
         MVC   S.GBASTAT5,RSTSTAT5                                              
         MVC   S.GBASTAT6,RSTSTAT6                                              
APUT06   CLI   RSTCOSTG,C' '       SAVE ANALYSIS COSTING CODE                   
         BNH   APUT18                                                           
         MVI   ACBEL,ACBCSTQ                                                    
         MVI   ACBLN,ACBLNQ+L'RSTCOSTG                                          
         MVC   ACBDATA(L'RSTCOSTG),RSTCOSTG                                     
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
         B     APUT18                                                           
         DROP  R3                                                               
*                                                                               
APUT10   CLI   0(R3),PPRELQ        TEST FOR PRODUCTION PROFILE ELEMENT          
         BNE   APUT18                                                           
         ST    R3,S.GBAAPPR                                                     
*                                                                               
APUT18   IC    RF,1(R3)                                                         
         BXH   R3,RF,APUT02                                                     
*                                                                               
APUT20   DS    0H                                                               
         TM    ACTRSTAT,ACTSABLP   TEST IS LOW-LEVEL ACCOUNT                    
         BZ    *+8                                                              
         OI    APAINDS1,GBAILOW                                                 
         CLI   APAINDS1,0                                                       
         BE    APUT21                                                           
         MVI   ACBEL,ACBINDQ                                                    
         MVI   ACBLN,ACBLNQ+L'APAINDS1                                          
         MVC   ACBDATA(L'APAINDS1),APAINDS1                                     
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
*                                                                               
APUT21   DS    0H                  FILL GBACC                                   
         GOTO1 AACBGET,APPARM,(X'80',APAFST)                                    
*                                                                               
         ST    R2,S.GBAAREC        ADD USER-DEFINED EXTRA DATA                  
         MVC   GBAREC,S.GBAREC                                                  
         GOTO1 AOVRCALL,GB#ACCOUNT_GETDATA                                      
         BNE   EXITERR                                                          
*                                                                               
         LA    R0,L'GBAXVALS       ADD MISCELLANEOUS VALUES                     
         LA    RF,GBAXVALS+L'GBAXVALS-1                                         
APUT22   CLI   0(RF),0                                                          
         BNE   APUT24                                                           
         BCTR  RF,0                                                             
         BCT   R0,APUT22                                                        
         B     APUT30                                                           
APUT24   MVI   ACBEL,ACBXTVQ                                                    
         AHI   R0,ACBLNQ                                                        
         STC   R0,ACBLN                                                         
         MVC   ACBDATA(L'GBAXVALS),GBAXVALS                                     
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
*                                                                               
APUT30   DS    0H                                                               
         LA    R0,GBAXN            ADD USER DEFINED DATA                        
         LA    R3,GBAXL1                                                        
APUT32   ICM   RF,1,0(R3)          RF = L(DATA)                                 
         BZ    APUT38                                                           
         ICM   R1,7,1(R3)          R1 = A(DATA)                                 
         BZ    APUT38                                                           
         LA    RE,GBAXN+1+ACBXTRQ                                               
         SR    RE,R0                                                            
         STC   RE,ACBEL                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACBDATA(0),0(R1)                                                 
         AHI   RF,ACBLNQ+1                                                      
         STC   RF,ACBLN                                                         
         BAS   RE,ACTADD                                                        
         BNE   EXIT                                                             
*                                                                               
APUT38   DS    0H                  BUMP TO NEXT DATA                            
         AHI   R3,GBAXL2-GBAXL1                                                 
         BCT   R0,APUT32                                                        
*                                                                               
ACBPUTX  DS    0H                                                               
         LH    RE,BCACBCNT         UPDATE A/C COUNT                             
         AHI   RE,1                                                             
         STH   RE,BCACBCNT                                                      
         B     EXITY                                                            
         SPACE 1                                                                
ACTADD   NTR1  ,                   ** ADD ACBELD TO BUFFER **                   
         L     R4,APANXT           R4 = (AREA FOR ELEMENT)                      
         XR    RF,RF                                                            
         IC    RF,ACBLN                                                         
         CHI   RF,ACBLNQ                                                        
         BNL   *+6                                                              
         DC    H'0'                                                             
         LA    R5,0(R4,RF)         R5 = A(NEXT ELEMEMENT)                       
         C     R5,APAEOB           ENSURE IS INSIDE BUFFER                      
         BL    AADD02                                                           
         MVC   GBEMSGNO,=AL2(AE$TMACT) TOO MANY ACCOUNTS                        
         B     EXITERR                                                          
*                                                                               
AADD02   DS    0H                  COPY ELEMENT TO BUFFER                       
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),ACBELD                                                   
         LA    R4,1(R4,RF)                                                      
         MVI   0(R4),ACBEOLQ                                                    
         ST    R4,APANXT                                                        
         B     EXITY                                                            
         SPACE 1                                                                
APWORKD  DSECT                     ** ACBPUT LOCAL W/S **                       
APAFST   DS    A                   A(FIRST ACBELD ADDED)                        
APANXT   DS    A                   A(AREA FOR NEXT ELEMENT)                     
APAEOB   DS    A                   A(END OF BUFFER)                             
APELEM   DS    XL255                                                            
APPARM   DS    6A                                                               
APAINDS1 DS    XL1                 GBAINDS1 VALUES                              
APAREC   DS    XL(L'GBAREC)        GPAREC VALUES                                
APWORKL  EQU   *-APWORKD                                                        
ACGENBAT CSECT                                                                  
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* FIND ACCOUNT DETAILS IN BUFFER FROM SEQUENCE NUMBER                 *         
*                                                                     *         
* NTRY: P1 = ACCOUNT SEQUENCE NUMBER                                  *         
* EXIT: P2 = L'ACCOUNT CODE                                           *         
*       P3 = A(ACCOUNT CODE)                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ACBFND   DS    0H                                                               
         USING *,R8                                                             
         L     R3,0(R1)            R3 = REQUIRED ACCOUNT SEQUENCE               
         L     R4,GBAACBUF                                                      
         USING ACBELD,R4                                                        
         XR    R2,R2               R2 = ACCOUNT SEQUENCE NUMBER                 
         XR    RF,RF                                                            
AFND02   DS    0H                                                               
         CLI   ACBEL,ACBEOLQ       SHOULDN'T GET TO END OF LIST                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,ACBLN                                                         
         CLI   ACBEL,ACBULAQ                                                    
         BNE   AFND08                                                           
         AHI   R2,1                                                             
         CR    R2,R3               TEST GOT TO CORRECT SEQUENCE NUMBER          
         BE    AFND10                                                           
AFND08   DS    0H                                                               
         BXH   R4,RF,AFND02                                                     
*                                                                               
AFND10   DS    0H                                                               
         SHI   RF,ACBLNQ           RETURN L(ACCOUNT)                            
         LA    R2,ACBDATA-1(RF)    R2 POINTS TO THE LAST CHAR                   
         CLI   0(R2),C' '          TRANCATE ALL SPACES                          
         BH    AFND12                                                           
         SHI   R2,1                                                             
         BCT   RF,*-12                                                          
         DC    H'0'                NO ACCOUNT CODE!!!!                          
*                                                                               
AFND12   LA    R0,ACBDATA          RETURN A(ACCOUNT)                            
         STM   RF,R0,4(R1)                                                      
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT CODE                                               *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'00' IF PASSED CL14 UNIT/LEDGER/ACCOUNT          *         
*                   X'01' IF PASSED XL15 COMPANY/UNIT/LEDGER/ACCOUNT  *         
*                   X'02' IF PASSED ACCOUNT RECORD                    *         
*                                                                     *         
*                   X'80' ON NOT TO SET ERROR MESSAGE                 *         
*                   X'40' ON TO TEST IF ACCOUNT IS VALID FOR POSTING  *         
*                                                                     *         
*             1-3 = A(CL14 U/L/ACCOUNT CODE)                 IF X'00' *         
*                   A(XL15 COMPANY/U/L/ACC CODE)             IF X'01' *         
*                   A(ACCOUNT RECORD)                        IF X'02' *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING AVWORKD,RC                                                       
ACTVAL   DS    0H                                                               
         USING *,R8                                                             
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         MVC   AVTYPE,0(R1)                                                     
         NI    AVTYPE,X'03'                                                     
         MVC   AVINDS,0(R1)                                                     
         NI    AVINDS,X'FC'                                                     
*                                                                               
         CLI   AVTYPE,AVTULA       TEST PASSED COMPANY/U/L/ACCOUNT              
         BE    *+14                                                             
         MVC   AVCULA,0(R2)                                                     
         B     AVAL02                                                           
         MVC   AVCPY,CUABIN                                                     
         MVC   AVULA,0(R2)                                                      
*                                                                               
AVAL02   DS    0H                                                               
         CLC   GBACULA,AVCULA      TEST ALREADY HAVE DETAILS OF ACCOUNT         
         BE    AVAL10                                                           
*                                  TRY TO EXTRACT DETAILS FROM BUFFER           
         GOTO1 AACBGET,AVPARM,AVCULA                                            
         BE    AVAL10                                                           
         CLI   AVTYPE,AVTREC       TEST PASSED RECORD                           
         BE    AVAL04                                                           
*                                  READ ACCOUNT RECORD                          
D        USING ACTRECD,IOKEY                                                    
         MVC   D.ACTKEY,BCSPACES                                                
         MVC   D.ACTKCULA,AVCULA                                                
         GOTO1 AIO,IOREAD+IOACCDIR+IOGENQ                                       
         BE    *+14                                                             
         MVC   GBEMSGNO,=AL2(AE$INACC)                                          
         B     ACTVALN                                                          
         GOTO1 AIO,IOGET+IOACCMST+IOGENQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOGEN                                                        
*                                                                               
         USING ACTRECD,R2                                                       
AVAL04   DS    0H                  PUT DETAILS TO BUFFER & CALL OVERLAY         
         GOTO1 AACBPUT,AVPARM,ACTRECD                                           
         BNE   EXIT                                                             
*                                                                               
AVAL10   DS    0H                                                               
         TM    AVINDS,AVIPOST      TEST VALIDATE ACCOUNT FOR POSTING            
         BZ    ACTVALX                                                          
         TM    GBAINDS1,GBAILOW    TEST IS LOW LEVEL A/C                        
         BNZ   *+14                                                             
         MVC   GBEMSGNO,=AL2(AE$NLOWA)                                          
         B     ACTVALN                                                          
         TM    GBAINDS1,GBAICLOS   TEST IS CLOSED                               
         BZ    AVAL12                                                           
         TM    GBPINDS,GBPICLLK    TEST IF IS ALLOWED                           
         BNZ   AVAL12                                                           
         MVC   GBEMSGNO,=AL2(AE$ACCLO)                                          
         B     ACTVALN                                                          
AVAL12   TM    GBAINDS1,GBAILOCK   TEST IS LOCKED                               
         BZ    ACTVALX                                                          
         TM    GBPINDS,GBPICLLK    TEST IF IS ALLOWED                           
         BNZ   ACTVALX                                                          
         MVC   GBEMSGNO,=AL2(AE$ACLOK)                                          
         B     ACTVALN                                                          
*                                                                               
ACTVALX  DS    0H                                                               
         B     EXITY                                                            
*                                                                               
ACTVALN  DS    0H                                                               
         TM    AVINDS,AVINOERR     TEST DON'T WANT ERROR                        
         BZ    *+14                                                             
         XC    GBEMSGNO,GBEMSGNO                                                
         B     EXITN                                                            
         MVC   GBEXTRA(L'AVULA),AVULA                                           
         B     EXITERR                                                          
         SPACE 1                                                                
AVWORKD  DSECT                     ** ACTVAL LOCAL W/S **                       
AVTYPE   DS    XL1                 INPUT TYPE                                   
AVTULA   EQU   X'00'               UNIT/LEDGER/ACCOUNT                          
AVTCULA  EQU   X'01'               COMPANY/UNIT/LEDGER/ACCOUNT                  
AVTREC   EQU   X'02'               RECORD                                       
AVINDS   DS    XL1                 INPUT INDICATORS                             
AVINOERR EQU   X'80'               DON'T SET ERROR MESSAGE                      
AVIPOST  EQU   X'40'               VALIDATE A/C FOR POSTING                     
AVCULA   DS    0XL15               * ACCOUNT DETAILS *                          
AVCPY    DS    XL1                 COMPANY CODE                                 
AVULA    DS    0CL14               UNIT/LEDGER/ACCOUNT CODE                     
AVUL     DS    0CL2                UNIT/LEDGER                                  
AVUNT    DS    CL1                 UNIT CODE                                    
AVLDG    DS    CL1                 LEDGER CODE                                  
AVACT    DS    CL12                ACCOUNT CODE                                 
AVPARM   DS    6A                                                               
AVWORKL  EQU   *-AVWORKD                                                        
ACGENBAT CSECT                                                                  
         SPACE 1                                                                
         POP   USING                                                            
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
         SPACE 1                                                                
         PUSH  USING                                                            
IOEX     DS    0H                                                               
         USING *,R8                                                             
         USING IEWORKD,RC                                                       
         ST    R1,IECTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOFLAG,0            RESET I/O FLAG BYTE                          
         MVI   IEQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IECTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IEQ,X'80'                                                        
         TM    IECTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IEQ,X'08'                                                        
         TM    IOINDS1,IOIFLUSH    TEST READ FLUSH REQUESTED                    
         BZ    *+12                                                             
         OI    IEQ,X'24'                                                        
         NI    IOINDS1,FF-IOIFLUSH BUT DON'T MAKE A HABIT OF IT                 
         TM    IOINDS1,IOIMNTUP    TEST MAINTENANCE UPDATE                      
         BZ    *+12                (IE NOT CLIENT DATA)                         
         OI    IEQ,X'24'                                                        
         NI    IOINDS1,FF-IOIMNTUP                                              
*                                                                               
         LA    R3,IOITEQ           ESTABLISH I/O AREA ADDRESS (IO1-IO3)         
         N     R3,IECTRL                                                        
         BZ    IOEX02                                                           
         SRL   R3,6                R3=I/O AREA NUMBER                           
         B     IOEX04                                                           
*                                                                               
IOEX02   TM    IECTRL+2,X'F0'      ESTABLISH I/O AREA ADDRESS (IO4-IOA)         
         BZ    IOEX06                                                           
         IC    R3,IECTRL+2                                                      
         SRL   R3,4                                                             
*                                                                               
IOEX04   CHI   R3,IOMAXQ           ONLY IOMAXQ IO AREAS SUPPORTED HERE          
         BNH   *+6                                                              
         DC    H'0'                                                             
         MHI   R3,IATABL                                                        
         LA    R3,IOIATAB-IATABL(R3)                                            
         USING IATABD,R3                                                        
         ST    R3,IEATAB                                                        
         MVC   IOADDR,IAAIO        SET REAL I/O ADDRESS                         
         DROP  R3                                                               
*                                                                               
IOEX06   LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IECTRL                                                        
         BNZ   IOEX10                                                           
         OC    IOFILE,IOFILE       CALLER MUST SUPPLY FILE NAME                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IEFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IECMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX50                                                           
*                                                                               
IOEX10   SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILNTRY         POINT TO LOCAL SYSTEM FILES                  
         LA    R0,10                                                            
         CR    R1,R0                                                            
         BNH   *+8                                                              
         L     RE,ASYSTAB          POINT TO GLOBAL SYSTEM FILES                 
*                                                                               
         USING FILTABD,RE                                                       
IOEX12   CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX12                                                           
         MVC   IEFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IEFILNM,IEFILNM     TEST NATIVE SYSTEM FILE                      
         BZ    IOEX14                                                           
         GOTO1 IOSWITCH,IOSWSYSN   SWITCH TO NATIVE SYSTEM IF REQUIRED          
         BNE   IOEXX                                                            
         B     IOEX20                                                           
*                                                                               
IOEX14   MVC   IESWSYS(L'IESWSYS+L'IESWFIL),FILSYSN                             
         L     RE,AFILTAB                                                       
         SR    R1,R1                                                            
IOEX16   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THIS SYSTEM NOT SUPPORTED                    
         CLC   0(1,RE),IESWSYS     MATCH ON SYSTEM SWITCH NUMBER                
         BE    *+16                                                             
         ICM   R1,3,4(RE)                                                       
         LA    RE,5(R1,RE)                                                      
         B     IOEX16                                                           
         MVC   IESYSYSN,1(RE)      SAVE SWITCH-TO SYSTEM NAME                   
         LA    RE,6(RE)            POINT TO FIRST FILE ENTRY                    
IOEX18   CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLC   FILNUM,IESWFIL      MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX18                                                           
         MVC   IEFILV,FILNUM       EXTRACT FILE VALUES                          
         OC    IEFILNM,IEFILNM     TEST NATIVE FILE TO THIS SYSTEM              
         BNZ   *+6                                                              
         DC    H'0'                NO - KILL THE APPLICATION                    
         GOTO1 IOSWITCH,IESWSYS    SWITCH TO CORRECT SYSTEM                     
         BE    IOEX20                                                           
         GOTO1 IOSW,IOSWSYSN       CAN'T SWITCH - SWITCH BACK TO NATIVE         
         MVI   IOERR,FF            SET SWITCH FAILURE ERROR BITS                
         B     IOEXX2                                                           
*                                                                               
IOEX20   L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IECTRL                                                        
         BNZ   IOEX22                                                           
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IOEX50                                                           
*                                                                               
IOEX22   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IEDUB(1),0(RE)                                                   
         NC    IEDUB(1),IEFILI                                                  
         CLC   IEDUB(1),IEFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     IOEX24                                                           
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOEX22                                                           
*                                                                               
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
IOEX24   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOEX24                                                           
         MVC   IECMDV,CMDNAME      EXTRACT COMMAND VALUES                       
         TM    IECMDI,CMDIUPD                                                   
         BZ    IOEX26                                                           
         TM    GBINDS1,GBIDRFT     TEST DRAFT MODE                              
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    GBINDS1,GBIUPD      SET FILE HAS BEEN UPDATED                    
*                                                                               
IOEX26   TM    IECMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IECMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOEX32                                                           
         TM    IECMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX40                                                           
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IOEX28                                                           
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         XC    IODAOVER,IODAOVER                                                
         B     IOEX32                                                           
*                                                                               
IOEX28   ICM   R3,15,IEATAB                                                     
         BZ    IOEX30                                                           
         USING IATABD,R3                                                        
         TM    IECMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IADA,IODA                                                        
         OC    IADA,IADA                                                        
         BZ    *+10                                                             
         MVC   IODA,IADA           YES - SET D/A                                
*                                                                               
         OC    IAWORK,IAWORK                                                    
         BZ    *+10                                                             
         MVC   IAWORK,IOWORK                                                    
         DROP  R3                                                               
*                                                                               
IOEX30   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX32                                                           
*                                                                               
         TM    IEFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IEFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IEDUB(4),IECTRL                                                  
         NI    IEDUB+2,X'F0'       TURN OFF FILE INDICATORS                     
         L     R0,IEDUB                                                         
         XR    R1,R1                                                            
         IC    R1,IEFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTO1 AIO                 RECURSE FOR DIRECTORY I/O                    
         BE    IOEX32              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
*                                                                               
IOEX32   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO I/O AREA ADDRESS                          
         MVC   IOFILE,IEFILNM      SET FILE NAME IN WORK AREA                   
         GOTO1 VDMGR,IEPARM,(IEQ,IECMDNM),IEFILNM,IODA,(R0),IOWORK              
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
         ICM   R3,15,IEATAB                                                     
         BZ    IOEXX                                                            
         USING IATABD,R3                                                        
         MVC   IADA,IODA                                                        
         MVC   IAWORK,IOWORK                                                    
         B     IOEXX               EXIT TO CALLER                               
         DROP  R3                                                               
*                                                                               
IOEX40   TM    IEFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX50                                                           
         MVC   IOFILE,IEFILNM      SET FILE NAME IN WORK AREA                   
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IEFILI2,FILIID      TEST I/S FILE HAS D/A ATTACHED               
         BZ    *+12                YES - MUST READ INTO IOAREA                  
         TM    IEFILI,FILIVL                                                    
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,IEPARM,(IEQ,IECMDNM),IEFILNM,IOKEY,(R0)                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
         TM    IEFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEX42                                                           
         SR    R1,R1                                                            
         IC    R1,IEFILKL          YES - EXTRACT DISK ADDRESS                   
         SR    R0,R0                                                            
         IC    R0,IEFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)        POINT TO DISK ADDRESS                        
         MVC   IODA,0(R1)                                                       
         ICM   R3,15,IEATAB                                                     
         BZ    IOEXX                                                            
         USING IATABD,R3                                                        
         MVC   IADA,IODA           POINT TO IO D/A                              
         B     IOEXX                                                            
         DROP  R3                                                               
*                                                                               
IOEX42   ICM   R3,15,IEATAB        POINT TO I/O WORK AREA                       
         BZ    IOEXX                                                            
         USING IATABD,R3                                                        
         MVC   IAWORK,IOWORK       POINT TO I/O WORK AREA                       
         B     IOEXX                                                            
         DROP  R3                                                               
*                                                                               
IOEX50   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDMGR,IEPARM,(IEQ,IECMDNM),IEFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEXX    TM    IOINDS1,IOISWAUT    TEST AUTO SWITCH BACK AFTER I/O              
         BZ    IOEXX2                                                           
         TM    IOFLAG,IOFSWTCH     TEST SYSTEM SWITCH OCCURRED                  
         BZ    IOEXX2                                                           
         GOTO1 IOSWITCH,IOSWSYSP   SWITCH TO PREVIOUS SYSTEM                    
*                                                                               
IOEXX2   MVI   IEQ,1               SET I/O COMPLETED OK                         
         TM    IOERR,IOERRS                                                     
         BZ    IOEXXX                                                           
         MVI   IEQ,2               SET LOGICAL I/O ERROR                        
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   IOEXXX                                                           
         MVI   IEQ,0               SET IRRECOVERABLE ERROR                      
*                                                                               
IOEXXX   CLI   IEQ,1               SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
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
         SPACE 1                                                                
IOSWITCH CLC   IOSWSYSC,0(R1)      TEST SWITCHED TO CORRECT SYSTEM              
         BER   RE                                                               
IOSW     LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   IEBYTE,0(R1)        SAVE SYSTEM NUMBER                           
         CLI   0(R1),10            TEST SWITCH TO CONTROL SYSTEM                
         BE    IOSW4                                                            
         L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LA    RF,SYSSWMAX                                                      
IOSW2    CLC   SYSSWSOV,IEBYTE     MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   *+12                                                             
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         B     IOSW4                                                            
         LA    RE,SYSSWLEN(RE)     BUMP TO NEXT SWITCH TABLE ENTRY              
         BCT   RF,IOSW2                                                         
         MVI   IEBYTE,0            SET CC=LOW FOR INVALID SYSTEM                
         B     IOSWX                                                            
*                                                                               
IOSW4    CLI   ASONOFF,ASON        TEST OFFLINE                                 
         BE    IOSW6                                                            
         ICM   RF,15,AUTL          YES - MOVE SE NUMBER TO UTL                  
         MVC   TSYS-UTLD(,RF),0(R1)                                             
         B     IOSW8                                                            
*                                                                               
IOSW6    MVC   IEPARM(1),0(R1)     SWITCH TO A SYSTEM                           
         MVC   IEPARM+1(3),BCEFFS                                               
         XC    IEPARM+4(4),IEPARM+4                                             
         GOTO1 VSWITCH,IEPARM                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    IOSW8                                                            
         MVI   IEBYTE,2            SET CC=HIGH FOR CAN'T SWITCH                 
         B     IOSWX                                                            
*                                                                               
IOSW8    MVC   IOSWSYSP,IOSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   IOSWSYSC,IEBYTE     SAVE CURRENT SYSTEM NUMBER                   
         OI    IOFLAG,IOFSWTCH     SET SYSTEM SWITCH OCCURRED                   
*                                                                               
IOSW10   MVI   IEBYTE,1            SET CC=EQUAL FOR OK                          
*                                                                               
IOSWX    CLI   IEBYTE,1            SET CC FOR CALLER                            
         BE    *+8                                                              
         MVI   IOERR,FF            SET ALL ERROR BITS ON                        
         LR    RE,R0                                                            
         BR    RE                  RETURN TO CALLER                             
         POP   USING                                                            
         SPACE 1                                                                
IEWORKD  DSECT                     ** IOEX S/R LOCAL W/S **                     
IEDUB    DS    D                   GENERAL WORK AREA                            
IECTRL   DS    F                   I/O COMMAND WORD                             
IEPARM   DS    6A                                                               
IEATAB   DS    A                   A(IATABD ENTRY)                              
IEBYTE   DS    X                   I/O BYTE                                     
IEQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IEFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IEFILNO  DS    X                   FILE NUMBER                                  
IEFILNM  DS    CL7                 FILE NAME                                    
IEFILI   DS    X                   FILE INDICATORS - 1                          
IEFILI2  DS    X                   FILE INDICATORS - 2                          
IEFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IEFILKL  DS    X                   KEY LENGTH                                   
IEFILCL  DS    X                   CONTROL LENGTH                               
IEFILDE  EQU   IEFILCL             DISPLACEMENT TO FIRST ELEMENT                
IEFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IECMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IECMDNM  DS    CL7                 COMMAND NAME                                 
IECMDNO  DS    X                   COMMAND NUMBER                               
IECMDI   DS    X                   COMMAND INDICATORS - 1                       
IECMDI2  DS    X                   COMMAND INDICATORS - 2                       
IESWSYS  DS    XL1                 SWITCH SYSTEM NUMBER                         
IESWFIL  DS    XL1                 SWITCH FILE NUMBER                           
IESYSYSN DS    CL3                 SWITCH SYSTEM NAME                           
IEWORKL  EQU   *-IEWORKD                                                        
ACGENBAT CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT                                                         *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'E' TO ADD ELEMENT AT END OF RECORD              *         
*             1-3 = A(RECORD)                                         *         
*       P2        = A(ELEMENT)                                        *         
* EXIT: P3        = A(ELEMENT ON RECORD)                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING EAWORKD,RC                                                       
ELADD    DS    0H                                                               
         USING *,R8                                                             
         LR    R4,R1                                                            
         LM    R2,R3,0(R4)                                                      
         LA    R2,0(R2)                                                         
         XR    RF,RF                                                            
         CLI   0(R4),C'E'                                                       
         BNE   *+8                                                              
         LA    RF,ADDEND                                                        
         GOTO1 VHELLO,EAPARM,(C'P',ACCMST),(R2),(R3),(RF)                       
         MVC   8(4,R4),16(R1)                                                   
         CLI   12(R1),0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         POP   USING                                                            
         SPACE 1                                                                
EAWORKD  DSECT                     ** LOCAL W/S **                              
EAPARM   DS    6A                                                               
EAWORKL  EQU   *-EAWORKD                                                        
ACGENBAT CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH/POSTING MONTH OF ACTIVITY                            *         
* NTRY: P1 BYTE 0 = C'B' TO VALIDATE BATCH MOA                        *         
*             1-3 = A(PACKED MOA)                                     *         
*       P2        = A(CHARACTER MOA)                                  *         
* EXIT:           = SET PACKED/CHARACTER MOA IN P1 AND P2             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
VALMOA   DS    0H                                                               
         USING *,R8                                                             
         LR    R4,R1                                                            
         LM    R2,R3,0(R4)           R2=A(PACKED MOA), R3=A(CHAR MOA)           
*                                                                               
         OC    0(L'GBBMOAP,R2),0(R2) TEST PACKED MOA IS PASSED                  
         BNZ   VMOA10                                                           
         CLI   0(R3),C'0'            TEST HAVE CHARACTER MOA                    
         BNL   VMOA04                                                           
         LA    RF,BCTODAYP           DEFAULT TO TODAY FOR BATCH MOA             
         CLI   0(R4),C'B'                                                       
         BE    *+8                                                              
         LA    RF,GBBMOAP            DEFAULT TO BATCH MOA FOR POSTING           
         MVC   0(L'GBBMOAP,R2),0(RF)                                            
         B     VMOA10                                                           
*                                  CONVERT CHARACTER MOA TO PACKED MOA          
VMOA04   XR    RE,RE                                                            
         IC    RE,ASEDAT+L'ASEDAT-1 RE = CURRENT YEAR (CHARACTER)               
         XR    RF,RF                                                            
         IC    RF,0(R3)            RF = MOA YEAR (CHARACTER)                    
*                                                                               
         SR    RE,RF               RE = CURRENT YEAR - MOA                      
         CHI   RE,-5               ENSURE -9 GETS CONVERTED TO +1 ETC.          
         BNL   *+8                                                              
         AHI   RE,10                                                            
         CHI   RE,6                ENSURE +9 GETS CONVERTED TO -1 ETC.          
         BL    *+8                                                              
         SHI   RE,10                                                            
         XR    R0,R0                                                            
         IC    R0,BCTODAYP         R0 = CURRENT YEAR (PACKED)                   
         SR    R0,RE                                                            
         CHI   R0,X'AA'            TEST > A9 (=PWOS 2009)                       
         BL    *+8                                                              
         AHI   R0,-X'9A'           X'AA' -> 10 (=PWOS 2010), ETC                
         STC   R0,0(R2)            SET MOA YEAR (PACKED)                        
*                                                                               
         XR    RE,RE               CONVERT MONTH                                
         IC    RE,1(R3)                                                         
         SHI   RE,C'0'             C'0' TO C'9' CONVERT TO 0 TO 9               
         BNM   *+8                                                              
         AHI   RE,C'0'-C'A'+X'10'  C'A' TO C'C' CONVERT TO 10 TO 12             
         STC   RE,1(R2)                                                         
         CLI   1(R2),X'12'                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VMOA10   DS    0H                  PASS GBBMOAP TO ACBMONVAL                    
         MVC   BCFULL(L'GBBMOAP),0(R2)                                          
         MVI   BCFULL+L'GBBMOAP,X'01'                                           
         MVC   BCDUB,BCSPACES                                                   
         GOTO1 VDATCON,BCPARM,(1,BCFULL),(18,BCDUB)                             
*                                                                               
VMOA20   DS    0H                                                               
         USING BMONVALD,BCWORK                                                  
         XC    BMONVALD(BMONVALQ),BMONVALD                                      
         GOTO1 VBMONVAL,(R1),(L'BCDUB,BCDUB),(GBBTYP,ACOM),            *        
               (CULANG,BMONVALD),(CUABIN,CUACCS)                                
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    VMOA02                                                           
         CLI   BMOERR,BMOERNGQ     INVALID DATE RANGE?                          
         BNE   *+12                NO - SHOW ERROR                              
         TM    GBBINDSI,GBBIDVMR   CBILL WILL VALIDATE THE MOA RANGE            
         BO    VMOA02                                                           
         MVC   GBEMSGNO,BMOMSG     SAVE ERROR MESSAGE NUMBER                    
         B     EXITERR                                                          
*                                                                               
VMOA02   DS    0H                                                               
         MVC   GBBSECL,0(R1)           BATCH SECURITY LEVEL                     
         MVC   0(L'GBBMOAP,R2),BMOMOSP SET PACKED MOA                           
         MVC   0(L'GBBMOAC,R3),BMOMOSC SET CHARACTER MOA                        
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
VMWORKD  DSECT                     ** LOCAL W/S **                              
VMPARM   DS    6A                                                               
VMWORKL  EQU   *-VMWORKD                                                        
ACGENBAT CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* INITIALISE ITEM RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
INIIREC  DS    0H                                                               
         USING *,R8                                                             
         L     R2,GBAIOITE         INITIALISE RECORD                            
         USING TBARECD,R2                                                       
         XC    TBARECD(TBARFST+1-TBARECD),TBARECD                               
         L     RF,GBAIOBAT                                                      
         MVC   TBAKEY,0(RF)                                                     
         MVC   TBAKTSEQ,GBBITEMS                                                
         LHI   RF,TBARFST+1-TBARECD                                             
         STH   RF,TBARLEN                                                       
*                                                                               
         USING BIAELD,BCELEM                                                    
         XC    BIAELD(BIALNQ),BIAELD                                            
         MVI   BIAEL,BIAELQ                                                     
         MVI   BIALN,BIALNQ                                                     
         ZAP   BIAAMT,BCPZERO                                                   
         MVC   BIAREF,GBIREF                                                    
         GOTO1 AELADD,BCPARM,TBARECD,BIAELD                                     
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON BLOCK (ADDRESSABLE BY ALL ROUTINES)                          *         
***********************************************************************         
         SPACE 1                                                                
         ORG   COMBLK                                                           
         SPACE 1                                                                
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
MAXPSTS  EQU   35                  MAXIMUM NO. OF POSTINGS ON BATCH ITM         
IOSIZEQ  EQU   2048                                                             
SECBLKLQ EQU   2048                                                             
EOT      EQU   0                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
BCEFFS   DC    XL8'FFFFFFFFFFFFFFFF'                                            
BCSPACES DC    CL132' '                                                         
BCPZERO  DC    PL1'0'                                                           
ACCMST   DC    C'ACCMST '                                                       
ADDEND   DC    C'ADD=END'                                                       
NLSYMB   DC    XL4'4C95936E'       <NL>                                         
TBSYMB   DC    XL3'4CA36E'         <T>                                          
*                                                                               
DTABLE   DS    0H                  ** DISP. TO INTERNAL TABLES **               
         DC    AL2(BLKTAB-ACGENBAT)                                             
         DC    AL2(BLKCPY-ACGENBAT)                                             
         DC    AL2(FILTAB-ACGENBAT)                                             
         DC    AL2(SYSTAB-ACGENBAT)                                             
         DC    AL2(CMDTAB-ACGENBAT)                                             
DTABLEN  EQU   (*-DTABLE)/L'DTABLE                                              
         DS    (DTABLEN-GBTABLEN)X ENSURE DTABLEN=GBTABLEN                      
         DS    (GBTABLEN-DTABLEN)X                                              
*                                                                               
VTABLE   DS    0A                                                               
         DC    V(BMONVAL)                                                       
VTABLEN  EQU   (*-VTABLE)/L'VTABLE                                              
         DS    (VTABLEN-GBVTABN)X ENSURE VTABLEN=GBVTABN                        
         DS    (GBVTABN-VTABLEN)X                                               
*                                                                               
CLIST    DS    0XL1                ** CORE. RESIDENT PHASES **                  
         DC    AL1(QADDTRN)                                                     
CLISTN   EQU   (*-CLIST)/L'CLIST                                                
         DS    (CLISTN-GBCTABN)X ENSURE CLISTN=GBCTABN                          
         DS    (GBCTABN-CLISTN)X                                                
*                                                                               
ANAWS    DS    0XL4                ** A(NON-ADDRESSABLE W/S AREAS) **           
         DC    AL2(ASWSTAB-GBWORKD,SWSTAB-GBWORKD)                              
ANAWSN   EQU   (*-ANAWS)/L'ANAWS                                                
*                                                                               
COMBLKX  EQU   ACGENBAT+X'0800'      END OF COMMON BLOCK                        
         DS    (COMBLKX-*)X        IF ERROR ADJUST COMBLKX                      
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* W/S BLOCK TABLE (EITHER PASSED BY OVERLAY OR SET BY ACGENBAT        *         
***********************************************************************         
         SPACE 1                                                                
BLKTABD  DSECT                                                                  
BLKDISPA DS    AL2                 DISPLACEMENT TO A(BLOCK) IN W/S              
BLKDISPL DS    AL2                 DISPLACEMENT TO L(BLOCK) IN W/S OR 0         
BLKDEFL  DS    AL2                 DEFAULT L(BLOCK)                             
BLKDISPI DS    AL2                 DISPLACEMT TO INDICATOR TO TEST              
BLKVALI  DS    XL1                 VALUE OF INDICATOR TO TEST                   
BLKACTS  DS    XL5                 ACTIONS (GBACT) THAT NEED W/S BLOCK          
BLKTABL  EQU   *-BLKTABD                                                        
ACGENBAT CSECT                                                                  
         SPACE 1                                                                
BLKTAB   DS    0X                  ** BLOCKS REQUIRED BEFORE CPYINI **          
*                                                                               
         DC    AL2(GBAADTBK-GBWORKD,0,TRNBLKL)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
         DC    AL2(GBAIOTRN-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
         DC    AL2(GBAIOACC-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
         DC    AL2(GBAIOBUK-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
         DC    AL2(GBAIOCAC-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
         DC    AL2(GBAIOGEN-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,GBACUTQ,0,0,0)                                       
*                                                                               
         DC    AL2(GBAIOBAT-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,GBACUTQ,0,0,0)                                       
*                                                                               
         DC    AL2(GBAIOITE-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,GBACUTQ,0,0,0)                                       
*                                                                               
         DC    AL2(GBAATBUF-GBWORKD,GBLATBUF-GBWORKD,1024)                      
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
         DC    AL2(GBAACBUF-GBWORKD,GBLACBUF-GBWORKD,3072)                      
         DC    AL2(0),AL1(0)                                                    
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
BLKTABX  DC    AL2(EOT)                                                         
         SPACE 1                                                                
BLKCPY   DS    0X                  ** BLOCKS REQUIRED AFTER CPYINI **           
*                                                                               
         DC    AL2(GBAIOOFA-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(BCCPYST4-GBWORKD),AL1(CPYSOFF2)                              
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*                                                                               
*&&UK                                                                           
         DC    AL2(GBABFMBK-GBWORKD,0,IOSIZEQ)                                  
         DC    AL2(BCCPYSTB-GBWORKD),AL1(CPYS1CAM)                              
         DC    AL1(GBACUBQ,0,0,0,0)                                             
*&&                                                                             
BLKCPYX  DC    AL2(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SYSTEM FILE NAMES TABLE (FILE NUMBERS 1 THRU 9)                     *         
***********************************************************************         
         SPACE 1                                                                
FILTABD  DSECT                                                                  
FILNUM   DS    XL1                 FILE NUMBER (IOFILES EQUATE)                 
FILNAME  DS    CL7                 FILE NAME                                    
FILINDS  DS    XL1                 FILE INDICATORS - 1                          
FILIVL   EQU   X'80'               FILE RECORDS ARE VARIABLE LENGTH             
FILIDA   EQU   X'40'               FILE IS A DIRECT ACCESS FILE                 
FILIIS   EQU   X'20'               FILE IS AN INDEX SEQUENTIAL FILE             
FILINDS2 DS    XL1                 FILE INDICATORS - 2                          
FILIDI   EQU   X'80'               FILE IS D/A WITH I/S INDEX                   
FILIID   EQU   FILIDI              FILE IS I/S INDEX TO A D/A FILE              
FILNUM2  DS    XL1                 D/A OR I/S FILE NUMBER                       
FILKEYL  DS    XL1                 D/A OR I/S KEY LENGTH                        
FILCTLL  DS    XL1                 FIXED LENGTH I/S CONTROL LENGTH              
FILDISP  EQU   FILCTLL             DISPLACEMENT TO FIRST RECORD ELEMENT         
FILMAXL  DS    XL2                 MAXIMUM RECORD LENGTH                        
*                                  FOLLOWING FIELDS SET IF FILNAME=NULL         
FILSYSN  EQU   FILINDS             NATIVE SYSTEM NUMBER OF FILE                 
FILFILN  EQU   FILINDS2            NATIVE FILE NUMBER (WITHIN SYSTEM)           
         DS    XL5                 N/D                                          
FILTABL  EQU   *-FILTABD                                                        
ACGENBAT CSECT                                                                  
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
         SPACE 1                                                                
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
         SPACE 1                                                                
***********************************************************************         
* SYSTEM FILE COMMANDS TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
CMDTABD  DSECT                                                                  
CMDNAME  DS    CL7                 COMMAND NAME                                 
CMDNUMB  DS    XL1                 COMMAND NUMBER (SEE IOCMNDS)                 
CMDINDS1 DS    XL1                 COMMAND INDICATORS - 1                       
CMDIDARQ EQU   X'80'               DISK ADDRESS REQUIRED FOR I/O                
CMDIDAXC EQU   X'40'               CLEAR DISK ADDRESS BEFORE I/O                
CMDIDADD EQU   X'20'               DISK ADDRESS RETURNED FROM I/O               
CMDIUPD  EQU   X'10'               COMMAND UPDATES THE FILE                     
CMDINDS2 DS    XL1                 COMMAND INDICATORS - 2                       
CMDTABL  EQU   *-CMDTABD                                                        
ACGENBAT CSECT                                                                  
         SPACE 1                                                                
CMDTAB   DS    0X                                                               
*                                  ** INDEX SEQUENTIAL COMMANDS **              
CMDIS    DC    AL1(FILIVL+FILIIS,0),AL2(CMDISX-*)                               
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,CMDIUPD,0)                                  
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
CMDISX   DC    AL1(EOT)                                                         
*                                  ** DIRECT ACCESS COMMANDS **                 
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX-*)                                      
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD+CMDIUPD,0)                      
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ+CMDIUPD,0)                      
CMDDAX   DC    AL1(EOT)                                                         
*                                                                               
CMDTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACGENBATD                                                      
         SPACE 1                                                                
***********************************************************************         
* BATCH CONTROLLER VALUES                                             *         
***********************************************************************         
         SPACE 1                                                                
BCAOVER  DS    A                   A(OVERLAY CALLBACK ROUTINE)                  
BCSAVRD  DS    A                   SAVED RD VALUE                               
BCRELO   DS    A                   RELOCATION FACTOR                            
BCAXDTA  DS    A                   A(EXTRA DATA FOR OFFLINE USE)                
*                                                                               
BCPARM   DS    8A                                                               
*                                                                               
BCDUB    DS    D                                                                
BCFULL   DS    F                                                                
BCHALF   DS    H                                                                
BCBYTE1  DS    XL1                                                              
BCBYTE2  DS    XL1                                                              
BCWORK   DS    XL64                                                             
BCELEM   DS    XL255                                                            
*                                                                               
BCPID    DS    CL8                 PERSON-ID                                    
BCINDS1  DS    XL1                                                              
BCIPADD  EQU   X'80'               WITHIN PSTSADD CALL                          
BCITCRT  EQU   X'40'               WITHIN TIMCRT CALL                           
BCITADD  EQU   X'20'               WITHIN TIMADD CALL                           
*                                                                               
BCTODAYC DS    XL2                 TODAYS DATE (COMPRESSED)                     
BCTODAYP DS    PL3                 TODAYS DATE (PACKED)                         
*                                                                               
BCATINDS DS    XL1                 ATTRIBUTE INDICATORS                         
BCATIITE EQU   X'80'               START OF ITEM                                
BCATIBLK EQU   X'40'               START OF ATRIBUTE BLOCK                      
BCACBCNT DS    H                   ACCOUNT BUFFER COUNT                         
BCAATLST DS    A                   A(CURRENT ATLSTD ENTRY)                      
*                                                                               
BCCPYEL  DS    XL(CPYLN4Q)         COMPANY ELEMENT                              
         ORG   BCCPYEL+(CPYSTAT1-CPYELD)                                        
BCCPYST1 DS    XL1                 COMPANY STATUS BYTE 1                        
         ORG   BCCPYEL+(CPYSTAT2-CPYELD)                                        
BCCPYST2 DS    XL1                 COMPANY STATUS BYTE 2                        
         ORG   BCCPYEL+(CPYSTAT3-CPYELD)                                        
BCCPYST3 DS    XL1                 COMPANY STATUS BYTE 3                        
         ORG   BCCPYEL+(CPYSTAT4-CPYELD)                                        
BCCPYST4 DS    XL1                 COMPANY STATUS BYTE 4                        
         ORG   BCCPYEL+(CPYSTAT5-CPYELD)                                        
BCCPYST5 DS    XL1                 COMPANY STATUS BYTE 5                        
         ORG   BCCPYEL+(CPYSTAT6-CPYELD)                                        
BCCPYST6 DS    XL1                 COMPANY STATUS BYTE 6                        
         ORG   BCCPYEL+(CPYSTAT7-CPYELD)                                        
BCCPYST7 DS    XL1                 COMPANY STATUS BYTE 7                        
         ORG   BCCPYEL+(CPYSTAT8-CPYELD)                                        
BCCPYST8 DS    XL1                 COMPANY STATUS BYTE 8                        
         ORG   BCCPYEL+(CPYSTAT9-CPYELD)                                        
BCCPYST9 DS    XL1                 COMPANY STATUS BYTE 9                        
         ORG   BCCPYEL+(CPYSTATA-CPYELD)                                        
BCCPYSTA DS    XL1                 COMPANY STATUS BYTE A                        
         ORG   BCCPYEL+(CPYSTATB-CPYELD)                                        
BCCPYSTB DS    XL1                 COMPANY STATUS BYTE B                        
         ORG   BCCPYEL+(CPYSTATC-CPYELD)                                        
BCCPYSTC DS    XL1                 COMPANY STATUS BYTE C                        
         ORG   BCCPYEL+(CPYSFST-CPYELD)                                         
BCCPYFST DS    XL(L'CPYSFST)       COMPANY FININCIAL YEAR START                 
         ORG   BCCPYEL+(CPYCURR-CPYELD)                                         
BCCPYCUR DS    XL3                 COMPANY CURRENCY CODE                        
         ORG   BCCPYEL+(CPYPROD-CPYELD)                                         
BCCPYPRD DS    CL(L'CPYPROD)       PRODUCTION UNIT/LEDGER                       
         ORG   BCCPYEL+(CPYRECV-CPYELD)                                         
BCCPYREC DS    CL(L'CPYRECV)       RECEIVABLE UNIT/LEDGER                       
         ORG   BCCPYEL+(CPYBANK-CPYELD)                                         
BCCPYBNK DS    CL(L'CPYBANK)       BANK UNIT/LEDGER                             
         ORG   BCCPYEL+(CPYSUPP-CPYELD)                                         
BCCPYSUP DS    CL(L'CPYSUPP)       SUPPLIER UNIT/LEDGER                         
         ORG   BCCPYEL+(CPYVATR-CPYELD)                                         
BCCPYVAT DS    CL(L'CPYVATR)       COMPANY VAT RATES (OLD VAT)                  
         ORG   BCCPYEL+(CPYCURRS-CPYELD)                                        
BCCPYSEC DS    XL3                 COMPANY SECONDARY CURRENCY CODE              
         ORG   BCCPYEL+(CPYSCMOA-CPYELD)                                        
BCCPYSCM DS    PL2                 COMPANY SECONDARY START MONTH                
         ORG   BCCPYEL+(CPYCDC-CPYELD)                                          
BCCPYCDC DS    CL1                 CASH DISCOUNT TO CLIENTS                     
         ORG   BCCPYEL+(CPYGLMOA-CPYELD)                                        
BCCPYGLM DS    CL1                 GL MOA                                       
         ORG                                                                    
BCDUM1RK DS    CL(L'ACTKACT)       DUMMY 1R PERSON ACCOUNT FROM CPYREC          
BCPALBLK DS    XL32                P&L BUCKET BLOCK USED BY ADDTRN              
         SPACE 1                                                                
***********************************************************************         
* ADDRESS DIRECTORY                                                   *         
***********************************************************************         
         SPACE 1                                                                
AFAC     DS    0A                  ** ADDRESSES PASSED FROM FACPAK **           
AINP     DS    A                   A(TRANSLATOR I/O BLOCK)                      
ATWA     DS    A                   A(TERMINAL WORK AREA)                        
ATIA     DS    A                   A(TERMINAL INPUT AREA)                       
AUTL     DS    A                   A(USER TERMINAL LIST ENTRY)                  
ASYS     DS    A                   A(SYSTEM FACILITY LIST)                      
ACOM     DS    A                   A(COMMON FACILITY LIST)                      
ASYSLST  DS    A                   A(GETFACT SYSTEM LIST)                       
ASYSFAC  DS    A                   A(SYSFACS)                                   
         SPACE 1                                                                
***********************************************************************         
* EXTERNAL MODULES                                                    *         
***********************************************************************         
         SPACE 1                                                                
GBVTAB   DS    0V                                                               
VBMONVAL DS    V                                                                
GBVTABN  EQU   (*-GBVTAB)/L'GBVTAB                                              
         SPACE 1                                                                
***********************************************************************         
* COMFACS ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
VCALLOV  DS    V                                                                
VDATCON  DS    V                                                                
VDICTATE DS    V                                                                
VDMGR    DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VSECRET  DS    V                                                                
VSWITCH  DS    V                                                                
*&&UK                                                                           
VTOBACCO DS    V                                                                
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
* CORE RESIDENT PHASES                                                *         
***********************************************************************         
         SPACE 1                                                                
GBCTAB   DS    0V                                                               
VADDTRN  DS    V                                                                
GBCTABN  EQU   (*-GBCTAB)/L'GBCTAB                                              
         SPACE 1                                                                
***********************************************************************         
* MISCELLANEOUS ADDRESSES                                             *         
***********************************************************************         
         SPACE 1                                                                
ACTRY    DS    A                   A(COUNTRY TABLE)                             
ALANG    DS    A                   A(LANGUAGE TABLE)                            
AFILNTRY DS    A                   A(FILTAB ENTRY)                              
         SPACE 1                                                                
***********************************************************************         
* INTERNAL ROUTINES                                                   *         
***********************************************************************         
         SPACE 1                                                                
GBROUTI  DS    0A                                                               
AOVRCALL DS    A                   CALL OVERLAY                                 
*                                                                               
ACPYINI  DS    A                                                                
*                                                                               
ABATCRT  DS    A                   CREATE BATCH                                 
ABATOPN  DS    A                   OPEN BATCH                                   
ABATCLO  DS    A                   CLOSE BATCH                                  
*                                                                               
AITECRT  DS    A                   CREATE ITEM                                  
AITEOPN  DS    A                   OPEN ITEM                                    
AITEUPD  DS    A                   UPDATE ITEM                                  
AITECLO  DS    A                   CLOSE ITEM                                   
*                                                                               
APSTSADD DS    A                   ADD POSTINGS                                 
APSTTRN  DS    A                                                                
APSTBLD  DS    A                                                                
*                                                                               
AADTOPN  DS    A                                                                
AADTUPD  DS    A                                                                
AADTCLO  DS    A                                                                
*                                                                               
ATIMCRT  DS    A                                                                
*                                                                               
ATRSSET  DS    A                                                                
AATRADD  DS    A                                                                
AATRBLD  DS    A                                                                
*                                                                               
AACBGET  DS    A                                                                
AACBPUT  DS    A                                                                
AACBFND  DS    A                                                                
*                                                                               
AIO      DS    A                                                                
AELADD   DS    A                                                                
AVALMOA  DS    A                                                                
AINIIREC DS    A                                                                
*                                                                               
GBROUTIN EQU   (*-GBROUTI)/L'GBROUTI                                            
         SPACE 1                                                                
***********************************************************************         
* INTERNAL TABLES                                                     *         
***********************************************************************         
         SPACE 1                                                                
GBTABLE  DS    0A                                                               
ABLKTAB  DS    A                                                                
ABLKCPY  DS    A                                                                
AFILTAB  DS    A                                                                
ASYSTAB  DS    A                                                                
ACMDTAB  DS    A                                                                
GBTABLEN EQU   (*-GBTABLE)/L'GBTABLE                                            
         SPACE 1                                                                
***********************************************************************         
* NON-ADDRESSABLE W/S AREAS                                           *         
***********************************************************************         
         SPACE 1                                                                
ASWSTAB  DS    A                   A(SYSTEM SWITCH TABLE)                       
         EJECT                                                                  
***********************************************************************         
* APPLICATION SYSTEM VARIABLES                                        *         
***********************************************************************         
         SPACE 1                                                                
ASWS     DS    0F                                                               
ASONOFF  DS    XL1                 SYSTEM MODE                                  
ASON     EQU   0                   APPLICATION RUNNING ONLINE                   
ASOFF    EQU   1                   APPLICATION RUNNING OFFLINE                  
ASSYSN   DS    XL1                 DATAMGR SYSTEM NUMBER                        
ASSYSO   DS    XL1                 NATIVE SYSTEM NUMBER FOR CALLOV              
ASEDAT   DS    CL8                 SYSTEM DATE (MM/DD/YY OR DD/MM/YY)           
ASBDAT   DS    XL3                 SYSTEM DATE (BINARY YMD)                     
ASTIME   DS    XL4                 SYSTEM TIME (STANDARD FORMAT)                
ASSIN    DS    XL4                 SYSTEM INPUT NUMBER                          
ASIOASTR DS    AL4                 A(START OF SYSTEM EXTRA AREA)                
ASIOALEN DS    XL4                 LENGTH OF SYSTEM EXTRA AREA                  
ASINDS   DS    XL1                 INIDICATOR BYTE                              
ASIRONLY EQU   X'80'               ACCOUNT SYSTEM IS IN READ-ONLY MODE          
ASWSX    DS    0X                                                               
         SPACE 2                                                                
***********************************************************************         
* CONNECTED USER VARIABLES                                            *         
***********************************************************************         
         SPACE 1                                                                
CUWS     DS    0X                                                               
CUABIN   DS    XL1                 AGENCY BINARY VALUE                          
CUAALF   DS    CL2                 AGENCY ALPHA VALUE                           
CUTSYM   DS    CL8                 TERMINAL SYMBOLIC ID                         
CUUSER   DS    XL2                 USER-ID NUMBER                               
CUAUTH   DS    XL2                 AUTHORISATION VALUE                          
CUACCS   DS    CL4                 LIMIT ACCESS VALUE                           
CUSTAT   DS    XL1                 TERMINAL STATUS INDICATORS                   
CUSDDS   EQU   X'80'               USER CONNECTED WITH A DDS TERMINAL           
CUPASS   DS    XL2                 PERSONAL PASSWORD NUMBER                     
CUCTRY   DS    XL1                 COUNTRY CODE                                 
CULANG   DS    XL1                 LANGUAGE CODE                                
CUASEC   DS    CL2                 AGENCY ALPHA VALUE FOR SECURITY              
CUPHSYS  DS    XL1                 PHYSICAL SYSTEM NUMBER                       
CUOVSPG  DS    0XL2                OVERLAY SYSTEM/PROGRAM NUMBER                
CUOVSYS  DS    XL1                 OVERLAY SYSTEM NUMBER                        
CUPRGNO  DS    XL1                 PROGRAM NUMBER                               
CUWSX    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* I/O CONTROLLER WORK AREA                                            *         
***********************************************************************         
         SPACE 1                                                                
IOWS     DS    0X                                                               
IOADDR   DS    A                   I/O AREA ADDRESS                             
IOFILE   DS    CL7                 FILE NAME                                    
IOCMND   DS    CL7                 DATAMGR COMMAND                              
IOFLAG   DS    XL1                 FLAG BYTE USED BY IO ROUTINE                 
IOFSWTCH EQU   X'80'               SYSTEM SWITCH OCCURRED FOR I/O               
IOERR    DS    XL1                 I/O ERROR RETURN BYTE                        
IOEEOF   EQU   X'80'               END-OF-FILE                                  
IOEDSK   EQU   X'40'               NON-RECOVERABLE DISK ERROR                   
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEDEL   EQU   X'02'               RECORD IS DELETED                            
IOMAX    EQU   X'01'               MAX IOS COMPLETED                            
IOESWS   EQU   X'FF'               SYSTEM SWITCH ERROR OCCURRED                 
IOERRS   EQU   X'FF'               RESERVED FOR CONTROLLER USE                  
IOKEY    DS    XL64                ACTUAL KEY VALUE                             
IOKEYSAV DS    XL64                SAVED ACTUAL KEY VALUE (BEFORE I/O)          
IODA     DS    XL4                 DISK ADDRESS OF D/A RECORD                   
IOWORK   DS    XL44                D/A LINKED FILES WORK AREA                   
IOINDS1  DS    XL1                 INDICATOR BYTE 1                             
IOISWAUT EQU   X'80'               AUTO SWITCH BACK AFTER I/O                   
IOIFLUSH EQU   X'40'               DO READ FLUSH (ACGEN6D)                      
IOIMNTUP EQU   X'20'               MAINTENANCE UPDATE (NOT CLIENT DATA)         
IOINDS2  DS    XL1                 INDICATOR BYTE 2                             
IODAOVER DS    XL4                 OVERRIDE D/A (ALWAYS USED AND XC'D)          
*                                                                               
IOSWSYSN DS    XL1                 NATIVE SYSTEM                                
IOSWSYSC DS    XL1                 CURRENT SWITCHED SYSTEM                      
IOSWSYSP DS    XL1                 PREVIOUS SWITCHED SYSTEM                     
*                                                                               
         DS    0A                                                               
IOIATAB  DS    0XL(IATABL)         IO AREAS (SEE IATABD)                        
AIOGEN   DS    A                   GENERAL I/O AREA                             
         ORG   AIOGEN+IATABL                                                    
AIOBAT   DS    A                   BATCH HEADER                                 
         ORG   AIOBAT+IATABL                                                    
AIOITE   DS    A                   BATCH ITEM RECORD                            
         ORG   AIOITE+IATABL                                                    
IOMAXQ   EQU   (*-IOIATAB)/IATABL                                               
IOWSX    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* I/O CONTROLLER EQUATES                                              *         
***********************************************************************         
         SPACE 1                                                                
*                                  ** I/O FILE EQUATES **                       
IOFILES  EQU   X'0F00'             RESERVED FOR CONTROLLER USE                  
IOGENDIR EQU   X'0100'             I/O TO GENERAL DIRECTORY                     
IOGENFIL EQU   X'0200'             I/O TO GENERAL FILE                          
IOCONFIL EQU   X'0300'             I/O TO CONTROL FILE (CONTROL SYSTEM)         
IOACCFIL EQU   X'0100'             I/O TO ACCOUNT FILE                          
IOACCDIR EQU   X'0200'             I/O TO ACCDIR                                
IOACCMST EQU   X'0300'             I/O TO ACCMST                                
IOACCARC EQU   X'0400'             I/O TO ACCARC                                
IOACCGND EQU   X'0500'             I/O TO GENDIR FROM ACC SYSTEM                
IOACCGNF EQU   X'0600'             I/O TO GENFILE FROM ACC SYSTEM               
IOCTFILE EQU   X'0F00'             I/O TO CONTROL FILE (OTHER SYSTEMS)          
*                                                                               
*              *** EQUATES FOR NEW FILE CONTROLLER ***                          
*                                                                               
IOMEDDIR EQU   X'0100'             I/O TO MEDIA DIRECTORY                       
IOMEDFIL EQU   X'0200'             I/O TO MEDIA FILE                            
IOMEDZDR EQU   X'0300'             I/O TO MEDDIRZ FROM MEDIA                    
IOMEDZFL EQU   X'0400'             I/O TO MEDFILZ FROM MEDIA                    
IOMPLDIR EQU   X'0100'             I/O TO MPL USER UPDATED DIRECTORY            
IOMPLFIL EQU   X'0200'             I/O TO MPL USER UPDATED FILE                 
IOMPRDRA EQU   X'0300'             I/O TO MPL STATIC DIRECTORY                  
IOMPRFLA EQU   X'0400'             I/O TO MPL STATIC FILE                       
IOMBADIR EQU   X'0100'             I/O TO MBASE DIRECTORY                       
IOMBANDX EQU   X'0200'             I/O TO MBASE INDEX                           
IOMBAFIL EQU   X'0300'             I/O TO MBASE FILE                            
IOMBUDIR EQU   X'0400'             I/O TO MBASE USER DIRECTORY                  
IOMBUFIL EQU   X'0500'             I/O TO MBASE USER FILE                       
IOMBAGDR EQU   X'0600'             I/O TO GENDIR FROM MBASE SYSTEM              
IOMBAGFL EQU   X'0700'             I/O TO GENFIL FROM MBASE SYSTEM              
         SPACE 1                                                                
*                                  ** I/O AREA EQUATES **                       
IOGENQ   EQU   X'0040'             I/O AREA TO BE USED FOR GENERAL I/O          
IOBATQ   EQU   X'0080'             I/O AREA TO BE USED FOR BATCH HEADER         
IOITEQ   EQU   X'00C0'             I/O AREA TO BE USED FOR BATCH ITEM           
*O4      EQU   X'4000'             I/O AREA 4 TO BE USED FOR I/O                
*O5      EQU   X'5000'             I/O AREA 5 TO BE USED FOR I/O                
*O6      EQU   X'6000'             I/O AREA 6 TO BE USED FOR I/O                
*O7      EQU   X'7000'             I/O AREA 7 TO BE USED FOR I/O                
*O8      EQU   X'8000'             I/O AREA 8 TO BE USED FOR I/O                
*O9      EQU   X'9000'             I/O AREA 9 TO BE USED FOR I/O                
*OA      EQU   X'A000'             I/O AREA A TO BE USED FOR I/O                
         SPACE 1                                                                
*                                  ** I/O COMMAND EQUATES **                    
IOCMNDS  EQU   X'000F'             RESERVED FOR CONTROLLER USE                  
IOLOCK   EQU   X'0010'             READ FOR UPDATE                              
IORDEL   EQU   X'0020'             READ DELETED RECORDS                         
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
IOSQ     EQU   IOSEQ               DMRSEQ                                       
IOSQD    EQU   IOSQ+IORDEL         DMRSEQ (FOR DELETES)                         
IOSQUP   EQU   IOSQ+IOLOCK         DMRSEQ (FOR UPDATE)                          
IOSQUPD  EQU   IOSQ+IOLOCK+IORDEL  DMRSEQ (FOR UPDATE & DELETES)                
IOGET    EQU   X'0004'             GETREC                                       
IOGETRUP EQU   IOGET+IOLOCK        GETREC (FOR UPDATE)                          
IOADD    EQU   X'0005'             DMADD                                        
IOADDREC EQU   X'0005'             ADDREC                                       
IOWRITE  EQU   X'0006'             DMWRT                                        
IOPUT    EQU   X'0006'             PUTREC                                       
IOPUTREC EQU   IOPUT               PUTREC                                       
IOUNLOCK EQU   X'0007'             DMUNLK                                       
IOADF    EQU   X'0008'             ADFREC                                       
IOADFREC EQU   IOADF                                                            
*                                                                               
SWSTAB   DS    (SYSSWMAX)XL(SYSSWLEN)                                           
         SPACE 1                                                                
GBWORKL  EQU   *-GBWORKD                                                        
         EJECT                                                                  
***********************************************************************         
* I/O CONTROLLER IO AREA DSECT                                        *         
***********************************************************************         
         SPACE 1                                                                
IATABD   DSECT                                                                  
IAAIO    DS    AL4                 A(IO AREA)                                   
IADA     DS    XL4                 DISK ADDRESS                                 
IAWORK   DS    XL44                WORK AREA                                    
IATABL   EQU   *-IATABD                                                         
         SPACE 1                                                                
***********************************************************************         
* SYSTEM SWITCH TABLE (LOCATED AT ASSWTAB)                            *         
***********************************************************************         
         SPACE 1                                                                
SYSSWTAB DSECT                     ** SYSTEM SWITCH TABLE **                    
SYSSWSYS DS    XL1                 SE NUMBER                                    
SYSSWSOV DS    XL1                 SYSTEM CALLOV NUMBER                         
SYSSWAGB DS    XL1                 AGENCY BINARY VALUE                          
SYSSWACS DS    XL4                 LIMIT ACCESS VALUE 1                         
SYSSWAC2 DS    XL4                 LIMIT ACCESS VALUE 2                         
         DS    XL1                 N/D                                          
SYSSWLEN EQU   *-SYSSWTAB                                                       
SYSSWMAX EQU   8                   MAXIMUM NUMBER OF ENTRIES IN TABLE           
         SPACE 1                                                                
***********************************************************************         
* ATTRIBUTES LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ATLSTD   DSECT                                                                  
ATINDS   DS    XL1                 INDICATOR BYTE                               
ATIITE   EQU   X'80'               START OF ITEM                                
ATIBLK   EQU   X'40'               START OF ATTRIBUTE BLOCK                     
ATIDR    EQU   X'20'               POSTING IS DR/CR                             
ATIWC    EQU   X'10'               ATSEQ = WORKCODE                             
ATIEOL   EQU   X'01'               END-OF-LIST                                  
ATSEQ    DS    XL2                 ACCOUNT SEQUENCE # IN ACLSTD                 
ATLSTL   EQU   *-ATLSTD                                                         
         SPACE 1                                                                
***********************************************************************         
* ACCOUNT LIST                                                        *         
***********************************************************************         
         SPACE 1                                                                
ACBELD   DSECT                                                                  
ACBEL    DS    XL1                 ELEMENT CODE                                 
ACBEOLQ  EQU   X'00'               END-OF-LIST                                  
ACBULAQ  EQU   X'01'               UNIT/LEDGER/ACCOUNT CODE                     
ACBCPYQ  EQU   X'02'               COMPANY CODE                                 
ACBNAMQ  EQU   X'03'               NAME                                         
ACBCSTQ  EQU   X'04'               ANALYSIS COSTING                             
ACBINDQ  EQU   X'05'               INDICATORS                                   
ACBXTVQ  EQU   X'EF'               USER-DEFINED EXTRA VALUES                    
ACBXTRQ  EQU   X'F0'+1 TO GBAXTRN  USER-DEFINED EXTRA DATA                      
ACBLN    DS    XL1                 ELEMENT LENGTH                               
ACBLNQ   EQU   *-ACBELD                                                         
ACBDATA  DS    0X                  DATA                                         
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACADDTRND                                                                     
         PRINT OFF                                                              
ADDTRND  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACBMONVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACTIMETRND                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACTIMETRND                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACTOBACCOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACTOBACCOD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
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
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACGENBAT  09/09/09'                                      
         END                                                                    
