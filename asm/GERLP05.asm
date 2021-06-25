*          DATA SET GERLP05    AT LEVEL 009 AS OF 11/04/11                      
*PHASE TF2D05A                                                                  
                                                                                
***********************************************************************         
* MODULE HANDLES THE FOLLOWING FUNCTIONS:-                            *         
*                                                                     *         
* GROUP/REPORT    USES X'F7' SCREEN - REP PREFIXES - CODE=GRPREP      *         
*                                                                     *         
* XFILE/REPORT    USES X'F7' SCREEN - REP PREFIXES - CODE=GRPREP      *         
*                                                                     *         
***********************************************************************         
                                                                                
RLP05    TITLE '- GROUP REPORTING'                                              
RLP05    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 REPL,**RLP5**,R8,R7,RR=RE,CLEAR=YES                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         ST    RB,OVNTRYA                                                       
         ST    RC,AREP             SAVE A(REPORT W/S)                           
         USING TWAD,RA                                                          
         L     R4,AREP                                                          
         USING REPD,R4             R4=A(REPORT INTERFACE BLOCK)                 
         L     R5,ARFPIOB                                                       
         USING RFPD,R5             R5=A(RFP INTERFACE BLOCK)                    
         USING RQHD,RFPVREQH                                                    
         OI    RFPFLAGS,RFPXRDUP+RFPXSYMS                                       
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         B     GRPREP                                                           
                                                                                
EXITY    LHI   RE,0                CC=EQUAL EXIT                                
         J     EXITCC                                                           
EXITN    LHI   RE,1                CC=NOT EQUAL EXIT                            
EXITCC   CHI   RE,0                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* GROUP/REPORT - LOAD AND VALIDATE REQUEST SCREEN                     *         
***********************************************************************         
                                                                                
         USING GRWORKD,RC          RC=A(LOCAL W/S)                              
         USING LSTTABD,CSLSTCUR                                                 
GRPREP   LHI   RE,KEYTGRP-RLP05    SET A(KEY DRIVER TABLE)                      
         CLI   CSREC,RECGRP                                                     
         BE    *+8                                                              
         LHI   RE,KEYTXFG-RLP05                                                 
         A     RE,OVNTRYA                                                       
         ST    RE,AKEYTAB                                                       
                                                                                
         LA    R0,FILTERS          CLEAR FILTER VALUES                          
         LHI   R1,FILTERSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    GRPFSTR,GRPFSTR     SET START GROUP TO ZEROES                    
         MVC   GRPFEND,PCEFFS      SET END GROUP TO X'FF'S                      
         XC    NXTRSTDT,NXTRSTDT   SET START NEXT RUN DATE TO ZEROES            
         MVC   NXTRENDT,PCEFFS     SET END NEXT RUN DATE TO X'FF'S              
         XC    LSTRSTDT,LSTRSTDT   SET START LAST RUN DATE TO ZEROES            
         MVC   LSTRENDT,PCEFFS     SET END LAST RUN DATE TO X'FF'S              
         XC    ENDSTDT,ENDSTDT     SET START END RUN DATE TO ZEROES             
         MVC   ENDENDT,PCEFFS      SET END END RUN DATE TO X'FF'S               
         XC    EFFSTDT,EFFSTDT     SET EFFECTIVE START DATE TO ZEROES           
         MVC   EFFENDT,PCEFFS      SET EFFECTIVE END DATE TO X'FF'S             
                                                                                
         MVC   CUAALF,TWAAGY       SET DEFAULT USER VALUES                      
         MVC   CUUSER,TWAUSRID                                                  
         MVC   CUSYSL,ASSYSL                                                    
         TM    CSOIND1,CSIUSELC                                                 
         BZ    GRKEYVAL                                                         
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUSYSL,GRPLSYST                                                  
                                                                                
GRKEYVAL LHI   RE,GRPREPSQ                                                      
         CLI   CSREC,RECGRP                                                     
         BE    *+8                                                              
         LHI   RE,XFGREPSQ                                                      
         CLM   RE,1,TWASCRN        TEST CORRECT SCREEN LOADED                   
         BE    GRKEYV10                                                         
         GOTOR AOVRSCR,PCPARM,('GRPREPSQ',RLPOLY1H)                             
         JNE   EXIT                                                             
                                                                                
         TM    CUSTAT,CUSDDS       ONLY DDS MAY SPECIFY AGENCY                  
         BNZ   *+8                                                              
         OI    REPAGYH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
         MVC   REPAGY(L'CUAALF),CUAALF                                          
         GOTOR AGETAGY,CUAALF                                                   
         MVC   REPAGYN,PCWORK                                                   
                                                                                
         TM    CUSTAT,CUSDDS                                                    
         BNE   *+16                                                             
         TM    TWAINDS1,TWAIUPID                                                
         BNZ   *+8                                                              
         OI    REPUIDH+(FVATRB-FVIHDR),FVAPROT                                  
         GOTOR AGETUID,CUUSER                                                   
         MVC   REPUID,PCWORK+(GIDCODE-GIDTABD)                                  
         MVC   REPUIDN,PCWORK+(GIDNAME-GIDTABD)                                 
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BE    GRKEYV02                                                         
         XC    REPGRPL,REPXFGL     SWAP GROUP/XFILE LABELS                      
         XC    REPXFGL,REPGRPL                                                  
         XC    REPGRPL,REPXFGL                                                  
         XC    REPSYSL,REPSYSL     XFILE HAS NO SYSTEM FIELD                    
         XC    REPRQSL,REPRQSL     AND NO REQUEST                               
         OI    REPSYSH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    REPXFGFH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    REPSYMFH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    REPRQSCH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    REPRQSFH+(FVATRB-FVIHDR),FVAPROT                                 
         MVI   TWASCRN,XFGREPSQ    SET 'DUMMY' SCREEN PHASE                     
         B     GRKEYV06                                                         
                                                                                
GRKEYV02 CLI   ASSYSL,RFPFSCTL     TEST CONNECTED TO CONTROL SYSTEM             
         BE    GRKEYV04                                                         
         OI    REPSYSH+(FVATRB-FVIHDR),FVAPROT                                  
         GOTOR AGETSYS,CUSYSL                                                   
         MVC   REPSYS,PCWORK                                                    
         B     GRKEYV06                                                         
                                                                                
GRKEYV04 TM    CSOIND1,CSIUSELC    TEST NESTED LIST CALL                        
         BZ    GRKEYV06                                                         
         GOTOR AGETSYS,CUSYSL      DISPLAY SYSTEM                               
         MVC   REPSYS,PCWORK                                                    
                                                                                
GRKEYV06 TM    CSOIND1,CSIUSELC    TEST NESTED LIST CALL                        
         BZ    GRKEYV08                                                         
         MVC   REPGRPF(L'GRPLGRP),GRPLGRP                                       
         MVC   REPGRPF+L'GRPLGRP(L'PCWILD),PCWILD                               
                                                                                
GRKEYV08 LA    R1,REPAGYH                                                       
         SR    RE,RE                                                            
         LA    RF,REPPFKH                                                       
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    *+14                                                             
         IC    RE,FVTLEN-FVIHDR(R1)                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EKEYD)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
                                                                                
GRKEYV10 MVC   AGYFSTR,CUAALF      SET DEFAULT AGENCY FILTERS                   
         MVC   AGYFEND,CUAALF                                                   
         TM    REPAGYH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   GRKEYV14                                                         
                                                                                
         MVI   FVMINL,1                                                         
         GOTOR AFVAL,REPAGYH       VALIDATE AGENCY FIELD                        
         JNE   EXIT                                                             
         CLC   PCUALL,FVIFLD       TEST FOR 'ALL'                               
         BNE   GRKEYV12                                                         
         XC    AGYFSTR,AGYFSTR                                                  
         MVC   AGYFEND,PCEFFS                                                   
         XC    REPAGYN,REPAGYN                                                  
         OI    REPAGYNH+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   UIDFLTY,KEYTRNGE                                                 
         XC    UIDFSTR,UIDFSTR                                                  
         MVC   UIDFEND,PCEFFS                                                   
         XC    REPUID,REPUID                                                    
         OI    REPUIDH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    REPUIDN,REPUIDN                                                  
         OI    REPUIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         B     GRKEYV16                                                         
                                                                                
GRKEYV12 GOTOR AVALAGY,FVIFLD      VALIDATE AGENCY ALPHA                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IAGY)                                            
         J     EXIT                                                             
         MVC   AGYFSTR,FVIFLD                                                   
         MVC   AGYFEND,FVIFLD                                                   
         GOTOR AGETAGY,FVIFLD                                                   
         MVC   REPAGYN,PCWORK                                                   
         OI    REPAGYNH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
GRKEYV14 MVC   UIDFSTR,CUUSER      SET DEFAULT USER-ID FILTERS                  
         MVC   UIDFEND,CUUSER                                                   
         MVI   UIDFLTY,KEYTRNGE                                                 
         TM    REPUIDH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   GRKEYV16                                                         
         MVI   UIDFLTY,KEYTRNGE                                                 
         XC    UIDFSTR,UIDFSTR                                                  
         MVC   UIDFEND,PCEFFS                                                   
         GOTOR AFVAL,REPUIDH       VALIDATE USER-ID FIELD                       
         BNE   GRKEYV16                                                         
         GOTOR AVALUID,FVIFLD      VALIDATE USER-ID                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IID)                                             
         J     EXIT                                                             
         CLC   AGYFSTR,PCWORK+(GIDALPH-GIDTABD)                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IID)                                             
         J     EXIT                                                             
         MVI   UIDFLTY,KEYTRNGE                                                 
         MVC   UIDFSTR,PCWORK+(GIDNUM-GIDTABD)                                  
         MVC   UIDFEND,PCWORK+(GIDNUM-GIDTABD)                                  
                                                                                
GRKEYV16 CLI   CSREC,RECGRP        TEST FOR GROUP RECORD                        
         BNE   GRSCRVAL                                                         
         MVI   SYSFLTY,KEYTLIST    SET DEFAULT SYSTEM                           
         MVC   SYSFSYST(L'ASSYSL),ASSYSL                                        
         CLI   CUSYSL,RFPFSCTL     TEST CONTROL SYSTEM                          
         BNE   *+16                                                             
         MVI   SYSFLTY,KEYTRNGE    YES - ALLOW ALL SYSTEMS                      
         MVI   SYSFSYST+0,00                                                    
         MVI   SYSFSYST+1,FF                                                    
         TM    REPSYSH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   GRSCRVAL                                                         
         MVI   FVMINL,1                                                         
         GOTOR AFVAL,REPSYSH       VALIDATE AGENCY FIELD                        
         CLC   PCUALL,FVIFLD       TEST FOR 'ALL'                               
         BE    GRSCRVAL                                                         
         GOTOR AVALSYS,REPSYSH                                                  
         JNE   EXIT                                                             
         MVC   SYSFSYST(L'CUSYSL),PCWORK                                        
         MVC   SYSFSYST+1(L'CUSYSL),PCWORK                                      
         B     GRSCRVAL                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMNS AND FILTERS                                        *         
***********************************************************************         
                                                                                
GRSCRVAL LHI   R2,COLTAB-RLP05                                                  
         A     R2,OVNTRYA                                                       
         USING COLTABD,R2                                                       
                                                                                
GRSCRV02 CLI   COLTREC,0           TEST RECORD TYPE FILTER PRESENT              
         BE    *+14                                                             
         CLC   CSREC,COLTREC       YES - MATCH RECORD TYPES                     
         BNE   GRSCRV06                                                         
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,COLTTDSP                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,TWAD(R3)         R3=A(SCREEN LINE)                            
         USING LSTD,R3                                                          
                                                                                
         GOTOR AFVAL,LSTINPH       VALIDATE COLUMN FIELD                        
         BNE   GRSCRV04                                                         
         GOTOR VALCOL                                                           
         JNE   EXIT                                                             
                                                                                
GRSCRV04 TM    LSTFLTH+(FVATRB-FVIHDR),FVAPROT                                  
         BNZ   GRSCRV06                                                         
         GOTOR AFVAL,LSTFLTH       VALIDATE COLUMN FILTER FIELD                 
         BNE   GRSCRV06                                                         
         SR    RF,RF                                                            
         ICM   RF,3,COLTVDSP                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         A     RF,OVNTRYA                                                       
         GOTOR (RF)                                                             
         JNE   EXIT                                                             
                                                                                
GRSCRV06 AHI   R2,COLTABL          BUMP TO NEXT TABLE ENTRY                     
         CLI   COLTABD,COLTEOTQ    TEST END OF TABLE                            
         BNE   GRSCRV02                                                         
         GOTOR BLDCOL              BUILD COLUMNS                                
         JNE   EXIT                                                             
                                                                                
         XC    IOKEY,IOKEY         CLEAR KEY FOR FIRST TIME THROUGH             
         B     GRPRTREP                                                         
         EJECT                                                                  
***********************************************************************         
* READ RECORDS AND PRINT REPORT                                       *         
***********************************************************************         
                                                                                
         USING LSTTABD,GRLSTCUR    USE WORKING STORAGE AREA FOR LIST            
GRPRTREP GOTOR ASETKEY             SET NEXT KEY                                 
         BH    GRPRTR02                                                         
         GOTOR AIO,IOHIGH+IOGENDIS+IO1                                          
         GOTOR AFLTKEY             FILTER KEY                                   
         BNE   GRPRTREP                                                         
         TM    CUSTAT,CUSDDS       DROP DESKTOP FOLDERS UNLESS DDS              
         BNZ   *+12                                                             
         CLI   IOKEY+(GRPKDTF-GRPKEYD),GRPKDTFQ                                 
         BE    GRPRTREP                                                         
         GOTOR AIO,IOGET+IOGENFIS+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    GRFLAG,FF-(GRFVALID)                                             
                                                                                
         GOTOR LSTBLD              BUILD LIST/EXTRACT VALUES                    
         BNE   GRPRTREP                                                         
                                                                                
         GOTOR FLTREC              APPLY RECORD FILTERS                         
         BNE   GRPRTREP                                                         
                                                                                
         GOTOR PRTREP              PRINT REPORT                                 
         B     GRPRTREP                                                         
                                                                                
GRPRTR02 TM    REPIND1,REPIPUT     TEST ANY RECORDS PUT                         
         BZ    GRPRTR04                                                         
         MVI   REPACTN,REPACLO     YES - CLOSE THE REPORT                       
         GOTOR VREPORT,REPD                                                     
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,REPGRPCH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$REPS2)                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVXTRA,PCSPACES                                                  
         MVC   FVXTRA(L'REPSUBID),REPSUBID                                      
         MVC   FVXTRA+L'REPSUBID(L'PCCOMMA),PCCOMMA                             
         SR    R0,R0                                                            
         ICM   R0,3,REPREPNO                                                    
         CVD   R0,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  PCWORK(5),PCDUB                                                  
         LA    RE,PCWORK                                                        
         LHI   RF,4                                                             
         CLI   0(RE),NUMERIC                                                    
         BNE   *+12                                                             
         AHI   RE,1                                                             
         BCT   RF,*-12                                                          
         EX    RF,*+8                                                           
         J     EXIT                                                             
         MVC   FVXTRA+L'REPSUBID+L'PCCOMMA(0),0(RE)                             
                                                                                
GRPRTR04 LA    R0,REPGRPCH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$NOREP)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN NUMBER                                              *         
***********************************************************************         
                                                                                
VALCOL   NTR1  LABEL=NO                                                         
         CLI   FVILEN,1                                                         
         BNE   VALCOL02                                                         
         CLI   FVIFLD,NEXTCOLQ                                                  
         BNE   VALCOL02                                                         
         SR    RE,RE                                                            
         IC    RE,GRNXTN                                                        
         AHI   RE,1                                                             
         STC   RE,GRNXTN                                                        
         LA    RE,GRNXTC-1(RE)                                                  
         MVC   0(L'COLTNUM,RE),COLTNUM                                          
         B     VALCOL04                                                         
                                                                                
VALCOL02 TM    FVIIND,FVINUM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INVIF)                                           
         J     EXITN                                                            
         OC    PCFULL,PCFULL                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INVIF)                                           
         J     EXITN                                                            
         CLC   PCFULL,=AL4(L'GRUSEC)                                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INVIF)                                           
         J     EXITN                                                            
         ICM   RE,15,PCFULL                                                     
         LA    RE,GRUSEC-1(RE)                                                  
         CLI   0(RE),0             TEST UNUSED COLUMN NUMBER                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$DUPIF)                                           
         J     EXITN                                                            
         MVC   0(L'COLTNUM,RE),COLTNUM                                          
                                                                                
VALCOL04 SR    RE,RE                                                            
         ICM   RE,1,COLTHWID                                                    
         CLM   RE,1,COLTDWID                                                    
         BNL   *+8                                                              
         ICM   RE,1,COLTDWID                                                    
         SR    RF,RF                                                            
         IC    RF,GRTOTWID                                                      
         LA    RF,1(RE,RF)                                                      
         CHI   RF,L'REPP1-1                                                     
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$RTW)                                             
         J     EXITN                                                            
         STC   RF,GRTOTWID                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD COLUMN LIST                                                   *         
***********************************************************************         
                                                                                
BLDCOL   NTR1  LABEL=NO                                                         
         CLI   GRTOTWID,0          TEST ANY COLUMNS SELECTED                    
         BNE   BLDCOL02                                                         
         LA    R0,REPGRPCH                                                      
         ST    R0,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GI$EDATA)                                           
         MVI   FVOMTYP,GTMINF                                                   
         J     EXITN                                                            
                                                                                
BLDCOL02 SR    R1,R1               TACK-ON NON-SPECIFIED COLUMNS                
         ICM   R1,1,GRNXTN                                                      
         BZ    BLDCOL10                                                         
         LA    RE,GRNXTC           POINT TO TACK-ON LIST                        
                                                                                
BLDCOL04 LA    RF,GRUSEC                                                        
         LHI   R0,L'GRUSEC                                                      
BLDCOL06 CLI   0(RF),0             TEST FREE COLUMN IN USED LIST                
         BE    BLDCOL08                                                         
         AHI   RF,1                                                             
         BCT   R0,BLDCOL06                                                      
         DC    H'0'                                                             
                                                                                
BLDCOL08 MVC   0(1,RF),0(RE)       USE THE FREE SLOT                            
         AHI   RE,1                BUMP TO NEXT TACK-ON COLUMN                  
         BCT   R1,BLDCOL04         DO FOR NUMBER OF ENTRIES                     
                                                                                
BLDCOL10 LA    RF,GRUSEC+L'GRUSEC-1                                             
         CLI   0(RF),0             LOCATE LAST USED COLUMN IN LIST              
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
                                                                                
         LA    RE,GRUSEC           LOCATE FIRST UNUSED COLUMN IN LIST           
         LHI   R1,1                                                             
BLDCOL12 CLI   0(RE),0                                                          
         BE    BLDCOL14                                                         
         AHI   RE,1                                                             
         AHI   R1,1                                                             
         B     BLDCOL12                                                         
                                                                                
BLDCOL14 CR    RE,RF               TEST UNUSED AFTER LAST USED                  
         JH    EXITY               YES - ALL OKAY                               
         MVC   FVMSGNO,=AL2(GE$COLTU)                                           
         CVD   R1,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  FVXTRA(2),PCDUB                                                  
         CLI   FVXTRA,NUMERIC                                                   
         BNE   *+14                                                             
         MVC   FVXTRA(1),FVXTRA+1                                               
         MVI   FVXTRA+1,SPACE                                                   
         LA    R0,REPGRPCH                                                      
         ST    R0,FVADDR                                                        
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUP FILTER                                               *         
***********************************************************************         
                                                                                
VALGRP   NTR1  LABEL=NO                                                         
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         ICM   RE,1,FVILEN                                                      
         LA    RF,FVIFLD-1(RE)     POINT TO LAST INPUT CHARACTER                
                                                                                
VALGRP04 CLC   PCWILD,0(RF)        TEST GROUPS STARTING WITH FEATURE            
         BNE   VALGRP06                                                         
         LHI   R0,1                                                             
         BCTR  RF,0                                                             
         BCT   RE,VALGRP04                                                      
         MVC   FVMSGNO,=AL2(GE$INV)                                             
         J     EXITN                                                            
                                                                                
VALGRP06 BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   GRPFSTR(0),FVIFLD                                                
         LTR   R0,R0                                                            
         JZ    EXITY                                                            
         EX    RE,*+8                                                           
         J     EXITY                                                            
         MVC   GRPFEND(0),FVIFLD                                                
                                                                                
***********************************************************************         
* VALIDATE DESCRIPTION FILTER                                         *         
***********************************************************************         
                                                                                
VALDSC   NTR1  LABEL=NO                                                         
         MVC   DESCFLEN,FVILEN                                                  
         MVC   DESCFVAL,FVIFLD                                                  
         J     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE YES/NO INPUT FILTERS                                       *         
***********************************************************************         
                                                                                
VALREQ   NTR1  LABEL=NO                                                         
         GOTOR VALYNO,REQFLT                                                    
         JE    EXITY                                                            
         CLI   FVILEN,2                                                         
         JE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INVIF)                                           
         J     EXITN                                                            
         MVI   REQFLT,FLTYES                                                    
         MVC   REQPROG,FVIFLD                                                   
         J     EXITY                                                            
                                                                                
VALRUN   LA    R1,RUNFLT                                                        
         B     VALYNO                                                           
VALSYM   LA    R1,SYMFLT                                                        
         B     VALYNO                                                           
VALCAL   LA    R1,CALFLT                                                        
                                                                                
VALYNO   NTR1  LABEL=NO                                                         
         SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   0(R1),FLTYES                                                     
         LA    RF,PCUYES                                                        
         EX    RE,*+8                                                           
         BE    VALYNO02                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         MVI   0(R1),FLTNO                                                      
         LA    RF,PCUNO                                                         
         EX    RE,*+8                                                           
         BE    VALYNO02                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         MVC   FVMSGNO,=AL2(GE$INVIF)                                           
         J     EXITN                                                            
                                                                                
VALYNO02 CLC   LSTINP(L'PCUYES),0(RF)                                           
         JE    EXITY                                                            
         MVC   LSTFLT(L'PCUYES),0(RF)                                           
         OI    LSTFLTH+(FVOIND-FVIHDR),FVOXMT                                   
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE FREQUENCY FILTER                                           *         
***********************************************************************         
                                                                                
VALFRQ   NTR1  LABEL=NO                                                         
         LA    R1,FVIHDR                                                        
         ICM   R1,8,=X'80'                                                      
         GOTOR AVALFRQ,(R1)                                                     
         JNE   EXITN                                                            
         MVC   FREQFVAL,PCWORK     SET FREQUENCY FILTER                         
         MVI   FREQFCC,CCNE        SET NOT EQUAL                                
         TM    FREQFVAL,SPACE                                                   
         JNZ   EXITY                                                            
         MVI   FREQFCC,CCEQ        SET EQUAL                                    
         OI    FREQFVAL,SPACE                                                   
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE LAST RUN DATE FILTER                                       *         
***********************************************************************         
                                                                                
VALLRU   NTR1  LABEL=NO                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITYMD',LSTFLTH),                    *        
               ('SOFOTSD8+SOFOTMIX',LSTRFLT),('FF-SOFIIONE',0)                  
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE END RUN DATE FILTER                                        *         
***********************************************************************         
                                                                                
VALERU   NTR1  LABEL=NO                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITYMD',LSTFLTH),                    *        
               ('SOFOTSD8+SOFOTMIX',ENDFLT),('FF-SOFIIONE',0)                   
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE NEXT RUN DATE FILTER                                       *         
***********************************************************************         
                                                                                
VALNRU   NTR1  LABEL=NO                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITYMD',LSTFLTH),                    *        
               ('SOFOTSD8+SOFOTMIX',NXTRFLT),('FF-SOFIIONE',0)                  
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE EFFECTIVE DATES FILTER                                     *         
***********************************************************************         
                                                                                
VALEFF   NTR1  LABEL=NO                                                         
         GOTOR AGOSOFT,PCPARM,('SOFITYMD',LSTFLTH),                    *        
               ('SOFOTSD8+SOFOTMIX',EFFSTDT),('FF-SOFIIONE',0)                  
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE FILTER NAME FILTER                                         *         
***********************************************************************         
                                                                                
VALFLT   NTR1  LABEL=NO                                                         
         CLI   FVILEN,L'NAMEFVAL                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$ILTL)                                            
         J     EXITN                                                            
         MVC   NAMEFLEN,FVILEN                                                  
         MVC   NAMEFVAL,FVIFLD                                                  
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE DESTINATION ID FILTER                                      *         
***********************************************************************         
                                                                                
VALDST   NTR1  LABEL=NO                                                         
         CLI   FVILEN,L'DSTCFVAL                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IDEST)                                           
         J     EXITN                                                            
         MVC   DSTCFLEN,FVILEN                                                  
         MVC   DSTCFVAL,FVIFLD                                                  
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE OUTPUT TYPE FILTER                                         *         
***********************************************************************         
                                                                                
VALOTY   NTR1  LABEL=NO                                                         
         CLI   FVILEN,L'OTYPFVAL                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IOUT)                                            
         J     EXITN                                                            
         MVC   OTYPFLEN,FVILEN                                                  
         MVC   OTYPFVAL,FVIFLD                                                  
         J     EXITY                                                            
                                                                                
                                                                                
***********************************************************************         
* VALIDATE XFILE FILTER                                               *         
***********************************************************************         
                                                                                
VALXFG   NTR1  LABEL=NO                                                         
         SR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         MVI   XFGFTYP,FLTYES                                                   
         LA    RF,PCUYES                                                        
         EX    RE,*+8                                                           
         BE    VALXFG02                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         MVI   XFGFTYP,FLTNO                                                    
         LA    RF,PCUNO                                                         
         EX    RE,*+8                                                           
         BE    VALXFG02                                                         
         CLC   FVIFLD(0),0(RF)                                                  
         MVI   XFGFTYP,FLTVAL                                                   
         MVC   XFGFTGRP,FVIFLD                                                  
         J     EXITY                                                            
                                                                                
VALXFG02 CLC   LSTFLT(L'PCUYES),0(RF)                                           
         JE    EXITY                                                            
         MVC   LSTFLT(L'PCUYES),0(RF)                                           
         OI    LSTFLTH+(FVOIND-FVIHDR),FVOXMT                                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST TABLE ENTRY                                   *         
***********************************************************************         
                                                                                
LSTBLD   NTR1  LABEL=NO                                                         
         XC    LSTTKEY(LSTTDATL),LSTTKEY                                        
         LA    R2,IOKEY                                                         
         USING GRPKEYD,R2          R2=A(GROUP/XFILE RECORD)                     
         MVC   LSTTDA,GRPKDA       SET DISK ADDRESS                             
                                                                                
         CLI   CSREC,RECGRP        TEST XFILE GROUP RECORD                      
         BNE   LSTBLD04                                                         
         MVI   LSTTRTYP,RECGRP     SET GROUP LIST ENTRY                         
         MVC   GRPLSYST,GRPKSYST   SET SYSTEM                                   
         MVC   GRPLAGY,GRPKAGY     SET AGENCY                                   
         MVC   GRPLUSER,GRPKUSER   SET USER                                     
         MVC   GRPLGRP,GRPKGRP     SET GROUP CODE                               
         B     LSTBLD06                                                         
                                                                                
LSTBLD04 CLI   CSREC,RECXFG        SET DISK ADDRESS                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING XFILED,R2                                                        
         MVI   LSTTRTYP,RECXFG     SET XFILE LIST ENTRY                         
         MVC   GRPLAGY,XFAGY       SET AGENCY                                   
         MVC   GRPLUSER,XFUSER     SET USER                                     
         MVC   GRPLGRP,XFGRP       SET GROUP CODE                               
                                                                                
LSTBLD06 L     R2,AIO1                                                          
         CLI   XFFRSTEL,GRPHCDQ    FIRST ELEMENT MUST BE HEADER                 
         JNE   EXITN                                                            
                                                                                
         GOTOR AXFGRFP             EXTRACT VALUES IN RFPD                       
                                                                                
         MVC   GRPLFREQ,RFPVFREQ   SET FREQUENCY                                
         MVC   GRPLOTYP,RFPVOTYP   SET OUTPUT TYPE                              
         MVC   GRPLDEST,RFPVDEST   SET DESTINATION ID                           
         MVC   GRPLNAME,RFPVNAME   SET GROUP NAME                               
         MVC   GRPLNXTR,RFPVNXTR   SET NEXT RUN DATE                            
         MVC   GRPLLSTR,RFPVLSTR   SET LAST RUN DATE                            
         MVC   GRPLEND,RFPVENDR    SET END RUN DATE                             
         MVC   GRPLDESC,RFPVDESC   SET GROUP DESCRIPTION                        
         MVC   GRPLXFIL,RFPXFILE   SET XFILE GROUP CODE                         
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER A GROUP RECORD                                    *         
*                                                                     *         
* NTRY - AIO1 CONTAINS GROUP RECORD                                   *         
* EXIT - CC=NOT EQUAL IF GROUP FILTERED OUT                           *         
***********************************************************************         
                                                                                
FLTREC   NTR1  LABEL=NO                                                         
         SR    RE,RE                                                            
                                                                                
         ICM   RE,1,FREQFCC        FREQUENCY FILTER                             
         BZ    FLTREC04                                                         
         CLC   GRPLFREQ,FREQFVAL                                                
         EX    RE,*+8                                                           
         J     FLTREC04                                                         
         NOP   FLTRECN                                                          
                                                                                
FLTREC04 ICM   RE,1,OTYPFLEN       TEST OUTPUT TYPE FILTER SET                  
         BZ    FLTREC06                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTRECN                                                          
         CLC   GRPLOTYP(0),OTYPFVAL                                             
                                                                                
FLTREC06 CLI   DSTCFLEN,0          DESTINATION ID CODE FILTER                   
         BE    FLTREC08                                                         
         OC    GRPLDEST,GRPLDEST                                                
         BZ    FLTRECN                                                          
         GOTOR AGETUID,GRPLDEST    SET DESTINATION ID CODE                      
         ICM   RE,1,DSTCFLEN       DESTINATION ID CODE FILTER                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTRECN                                                          
         CLC   PCWORK+(GIDCODE-GIDTABD)(0),DSTCFVAL                             
                                                                                
FLTREC08 ICM   RE,1,NAMEFLEN       FILTER NAME FILTER                           
         BZ    FLTREC10                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BNE   FLTRECN                                                          
         CLC   GRPLNAME(0),NAMEFVAL                                             
                                                                                
FLTREC10 GOTOR AGETNXT             GET NEXT RUN DATE                            
         CLC   PCWORK(L'NXTRSTDT),NXTRSTDT                                      
         BL    FLTRECN                                                          
         CLC   PCWORK(L'NXTRENDT),NXTRENDT                                      
         BH    FLTRECN                                                          
                                                                                
         GOTOR AGETLST             GET LAST RUN DATE                            
         CLC   PCWORK(L'LSTRSTDT),LSTRSTDT                                      
         BL    FLTRECN                                                          
         CLC   PCWORK(L'LSTRENDT),LSTRENDT                                      
         BH    FLTRECN                                                          
                                                                                
         GOTOR AGETEND             GET END RUN DATE                             
         CLC   PCWORK(L'ENDSTDT),ENDSTDT                                        
         BL    FLTRECN                                                          
         CLC   PCWORK(L'ENDENDT),ENDENDT                                        
         BH    FLTRECN                                                          
                                                                                
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNE   FLTREC16                                                         
         CLI   XFGFTYP,FLTNOT      APPLY XFILE FILTER                           
         BE    FLTREC16                                                         
         CLI   XFGFTYP,FLTYES                                                   
         BNE   FLTREC12                                                         
         OC    GRPLXFIL,GRPLXFIL                                                
         BZ    FLTRECN                                                          
         B     FLTREC16                                                         
FLTREC12 CLI   XFGFTYP,FLTNO                                                    
         BNE   FLTREC14                                                         
         OC    GRPLXFIL,GRPLXFIL                                                
         BNZ   FLTRECN                                                          
         B     FLTREC16                                                         
FLTREC14 CLI   XFGFTYP,FLTVAL                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GRPLXFIL,XFGFTGRP                                                
         BNE   FLTRECN                                                          
                                                                                
FLTREC16 SR    RE,RE               DESCRIPTION (SCAN)                           
         ICM   RE,1,DESCFLEN                                                    
         BZ    FLTREC20                                                         
         BCTR  RE,0                                                             
         LHI   RF,L'GRPLDESC                                                    
         SR    RF,RE                                                            
         LA    R1,GRPLDESC                                                      
FLTREC18 EX    RE,*+8                                                           
         BE    FLTREC20                                                         
         CLC   DESCFVAL(0),0(R1)                                                
         AHI   R1,1                                                             
         BCT   RF,FLTREC18                                                      
         B     FLTRECN                                                          
                                                                                
FLTREC20 CLI   RUNFLT,FLTNOT       RUN SCHEDULE                                 
         BE    FLTREC24                                                         
         CLI   RUNFLT,FLTYES                                                    
         BNE   FLTREC22                                                         
         OC    RFPVDAYS,RFPVDAYS                                                
         BZ    FLTRECN                                                          
         B     FLTREC24                                                         
FLTREC22 OC    RFPVDAYS,RFPVDAYS                                                
         BNZ   FLTRECN                                                          
                                                                                
FLTREC24 CLI   CALFLT,FLTNOT       CALENDAR                                     
         BE    FLTREC28                                                         
         CLI   CALFLT,FLTYES                                                    
         BNE   FLTREC26                                                         
         OC    RFPVNXTR,RFPVNXTR                                                
         BZ    FLTRECN                                                          
         OC    RFPVENDR,RFPVENDR                                                
         BZ    FLTRECN                                                          
         OC    RFPVRNGE,RFPVRNGE                                                
         BZ    FLTRECN                                                          
         B     FLTREC28                                                         
FLTREC26 OC    RFPVRNGE,RFPVRNGE                                                
         BNZ   FLTRECN                                                          
                                                                                
FLTREC28 CLI   SYMFLT,FLTNOT       RFP SYMBOLS                                  
         BE    FLTREC32                                                         
         GOTOR SETRFP              SET RFP VALUES FOR GROUP                     
         CLI   SYMFLT,FLTYES                                                    
         BNE   FLTREC30                                                         
         OC    RFPVNUMS,RFPVNUMS                                                
         BZ    FLTRECN                                                          
         B     FLTREC32                                                         
FLTREC30 OC    RFPVNUMS,RFPVNUMS                                                
         BNZ   FLTRECN                                                          
                                                                                
FLTREC32 CLI   REQFLT,FLTNOT       REQUESTS                                     
         BE    FLTREC38                                                         
         GOTOR SETRFP              SET RFP VALUES FOR GROUP                     
         CLI   REQFLT,FLTYES                                                    
         BNE   FLTREC36                                                         
         OC    RFPVSEQN,RFPVSEQN                                                
         BZ    FLTRECN                                                          
         OC    REQPROG,REQPROG     TEST PROGRAM FILTER                          
         BZ    FLTREC38                                                         
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPMODE,RFPRETRQ                                                 
                                                                                
FLTREC34 GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RFPRET,RFPEOF       TEST END OF FILE                             
         BE    FLTRECN                                                          
         GOTOR FLTPRG              APPLY PROGRAM FILTER                         
         BE    FLTREC38                                                         
         B     FLTREC34                                                         
                                                                                
FLTREC36 OC    RFPVSEQN,RFPVSEQN                                                
         BNZ   FLTRECN                                                          
                                                                                
FLTREC38 CLC   GRPLNXTR,EFFSTDT    EFFECTIVE DATES                              
         BL    FLTRECN                                                          
         CLC   GRPLEND,EFFENDT                                                  
         BH    FLTRECN                                                          
                                                                                
FLTRECY  J     EXITY                                                            
                                                                                
FLTRECN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECORD                                                    *         
***********************************************************************         
                                                                                
PRTREP   NTR1  LABEL=NO                                                         
                                                                                
         NI    GRFLAG,FF-(GRFFIRST+GRFMULTI)                                    
                                                                                
         TM    REPIND1,REPIOPN     TEST REPORT IS OPEN                          
         BNZ   PRTREP20                                                         
         LA    R0,REPD                                                          
         AHI   R0,BOXAREA-REPD                                                  
         ST    R0,REPABOX                                                       
         LA    R0,REPD                                                          
         AHI   R0,PQBUFF-REPD                                                   
         ST    R0,REPAPQB                                                       
         MVC   REPACOM,ACOM        INITIALISE REPBLK VALUES                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVI   REPHEADN,REPHNUM                                                 
         MVI   REPMIDSN,REPMNUM                                                 
         MVI   REPPRNTN,REPPNUM                                                 
         MVI   REPIND2,REPILOW+REPIBOX+REPIBOXJ                                 
         MVI   REPHEADI,REPHCLRA                                                
         MVI   REPMIDSI,REPMCLRA                                                
*&&UK*&& MVI   REPMAXL,60                                                       
*&&US*&& MVI   REPMAXL,58                                                       
         OI    REPPRNTI,REPPSPAC+REPPCLRA                                       
         MVC   REPDATE,ASBDAT                                                   
         MVC   REPSYSID,=C'CT'                                                  
         MVC   REPPRGID,=C'RF'                                                  
         MVC   REPSUBID,=C'RLP'                                                 
         LHI   R0,48                                                            
         STCM  R0,3,REPRLH                                                      
         LHI   R0,12                                                            
         STCM  R0,3,REPRDH                                                      
         MVI   REPHEADH,1                                                       
         LHI   R0,PRTHKS-RLP05                                                  
         A     R0,OVNTRYA                                                       
         ST    R0,REPAUSR                                                       
         LHI   R0,PRTSPC-RLP05                                                  
         A     R0,OVNTRYA                                                       
         ST    R0,REPAPHS                                                       
                                                                                
         MVI   REPACTN,REPAINI     INITIALISE REPD                              
         GOTOR VREPORT,REPD                                                     
                                                                                
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTOR (RF)                                                             
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
                                                                                
         L     R0,AIO3             CLEAR SPACE FOR HEADER SCREEN FORMAT         
         LHI   R1,TWACOLS*TWAROWS                                               
         SR    RE,RE                                                            
         LHI   RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,RLPSRVH          FORMAT HEADER SCREEN INTO IO/IO2             
         SR    RE,RE                                                            
         LA    RF,4095(R1)                                                      
PRTREP02 ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    PRTREP04                                                         
         SR    R2,R2                                                            
         ICM   R2,3,FVABSA-FVIHDR(R1)                                           
         A     R2,AIO3                                                          
         SHI   RE,L'FVIHDR                                                      
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SHI   RE,L'FVIHDR                                                      
         BCTR  RE,0                                                             
         TM    FVATRB-FVIHDR(R1),FVAZERO                                        
         BO    *+18                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),L'FVIHDR(R1)                                             
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BXLE  R1,RE,PRTREP02                                                   
                                                                                
PRTREP04 L     RF,VREPORT          INITIALISE HEADER PRINT LOOP                 
         LA    R1,REPD                                                          
         L     R2,AIO3                                                          
         LHI   R0,TWAROWS                                                       
                                                                                
         L     RE,REPABOX                                                       
         USING BOXD,RE                                                          
         MVI   BOXCOLS,SPACE                                                    
         MVC   BOXCOLS+1(L'REPP1-1),BOXCOLS                                     
         MVI   BOXCOLS+00,BOXCLFTQ                                              
         MVI   BOXCOLS+82,BOXCRGTQ                                              
         MVI   BOXROWS,SPACE                                                    
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXROWS+02,BOXRTOPQ                                              
         MVI   BOXROWS+27,BOXRBOTQ                                              
         DROP  RE                                                               
                                                                                
         GOTOR (RF)                                                             
PRTREP06 MVC   REPP1+1(TWACOLS),0(R2)                                           
         GOTOR (RF)                                                             
         AHI   R2,TWACOLS                                                       
         BCT   R0,PRTREP06                                                      
         GOTOR (RF)                                                             
         GOTOR (RF)                                                             
                                                                                
         OI    REPHEADI,REPHSPAC   SET SPACE AFTER HEADLINES                    
         OI    REPMIDSI,REPMSPAC   AND MIDLINES                                 
         MVI   REPSUBPG,1                                                       
         CLI   CSREC,RECXFG                                                     
         BNE   *+8                                                              
         MVI   REPSUBPG,2                                                       
         OI    REPHEADI,REPHFRCE                                                
         LHI   R0,1                                                             
         STCM  R0,3,REPPAGE                                                     
         OI    GRFLAG,GRFDOHED     DO HEADLINE HOOKS FROM NOW ON                
         B     PRTREP22                                                         
                                                                                
PRTREP20 CLC   GRPLAGY,GRLAGY      TEST CHANGE OF HEADER INFO                   
         BNE   *+10                                                             
         CLC   GRPLUSER,GRLUSER                                                 
         BNE   *+10                                                             
         CLC   GRPLSYST,GRLSYST                                                 
         BE    PRTREP22                                                         
         OI    REPHEADI,REPHFRCE   YES - FORCE NEW PAGE                         
                                                                                
PRTREP22 XC    GRSAVAL(GRSAVALL),GRSAVAL                                        
         LA    R2,GRUSEC           R2=A(COLUMN LIST)                            
         LA    R3,REPP1+1          R3=A(NEXT PRINT COLUMN)                      
                                                                                
PRTREP24 SR    R6,R6                                                            
         ICM   R6,1,0(R2)          R6=NEXT COLUMN NUMBER                        
         BZ    PRTREP28            ZERO=END OF LIST                             
         BCTR  R6,0                                                             
         MHI   R6,COLTABL                                                       
         AHI   R6,COLTAB-RLP05                                                  
         A     R6,OVNTRYA                                                       
         USING COLTABD,R6                                                       
                                                                                
         TM    COLTINDS,COLTIMLT   TEST THIS IS A MULTI-LINE COLUMN             
         BZ    *+8                                                              
         OI    GRFLAG,GRFMULTI     SET MULTI-LINE SELECTED                      
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,COLTFDSP                                                    
         A     RF,OVNTRYA                                                       
         GOTOR (RF)                CALL COLUMN DISPLAY ROUTINE                  
         BE    PRTREP26                                                         
                                                                                
         OC    GRSAVPTR,GRSAVPTR   OVERFLOW - TEST FIRST                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R3,GRSAVPTR         SAVE CURRENT COLUMN ADDRESS                  
         ST    RF,GRSAVDIS         SAVE DISPLAY ROUTINE ADDRESS                 
                                                                                
PRTREP26 SR    R1,R1               GET COLUMN WIDTH                             
         ICM   R1,1,COLTHWID                                                    
         CLM   R1,1,COLTDWID                                                    
         BH    *+8                                                              
         ICM   R1,1,COLTDWID                                                    
         LA    R3,1(R3,R1)         BUMP TO NEXT OUTPUT COLUMN                   
         AHI   R2,1                BUMP TO NEXT COLUMN NUMBER                   
         B     PRTREP24                                                         
                                                                                
PRTREP28 TM    GRFLAG,GRFMULTI     TEST MIDLINE BOX PENDING                     
         BZ    PRTREP32                                                         
         TM    GRFLAG,GRFBOXLP                                                  
         BZ    PRTREP34                                                         
         NI    GRFLAG,FF-(GRFBOXLP)                                             
         LHI   RE,REPPNUM          CALCULATE N'LINES USED                       
         MHI   RE,L'REPP1                                                       
         LA    RE,REPP1-L'REPP1(RE)                                             
         LHI   RF,REPPNUM                                                       
         CLC   0(L'REPP1,RE),PCSPACES                                           
         BNE   PRTREP30                                                         
         SHI   RE,L'REPP1                                                       
         BCT   RF,*-14                                                          
         B     PRTREP36                                                         
                                                                                
PRTREP30 SR    RE,RE               TEST BLOCK+BOX LINE WILL FIT ON PAGE         
         IC    RE,REPLINE                                                       
         LA    RE,1(RF,RE)                                                      
         CLM   RE,1,REPMAXL                                                     
         BNH   *+12                                                             
         OI    REPHEADI,REPHFRCE   NO - FORCE NEW PAGE                          
         B     PRTREP34                                                         
         OI    REPIND2,REPIBOXF    YES - SET FORCE BOX LINE                     
         B     PRTREP34                                                         
                                                                                
PRTREP32 MVI   REPPRNSA,2          DOUBLE SPACE REPORT                          
PRTREP34 GOTOR VREPORT,REPD                                                     
         NI    REPIND2,FF-(REPIBOXF)                                            
                                                                                
         ICM   R3,15,GRSAVPTR      TEST MORE FORMATTING TO BE DONE              
         BZ    PRTREP36                                                         
         GOTOR GRSAVDIS            CALL DISPLAY ROUTINE                         
         BNE   *+10                                                             
         XC    GRSAVPTR,GRSAVPTR                                                
         B     PRTREP28                                                         
                                                                                
PRTREP36 MVC   GRLAGY,GRPLAGY                                                   
         MVC   GRLUSER,GRPLUSER                                                 
         MVC   GRLSYST,GRPLSYST                                                 
                                                                                
         TM    GRFLAG,GRFMULTI     TEST MULTI-LINE COLUMN FORMATTED             
         BZ    *+8                                                              
         OI    GRFLAG,GRFBOXLP     YES - SET BOX LINE PENDING                   
                                                                                
PRTREPX  J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS PRINT HOOKS                                                 *         
***********************************************************************         
                                                                                
PRTHKS   NI    GRFLAG,FF-(GRFBOXLP)                                             
         NI    REPIND2,FF-(REPIBOXF)                                            
         TM    GRFLAG,GRFDOHED     TEST WANT HEADLINES FORMATTED YET            
         BZR   RE                                                               
                                                                                
PRTHKSN  NTR1  LABEL=NO                                                         
                                                                                
PRTHED   MVC   REPH4+1(L'PCMCPY),PCMCPY                                         
         MVC   REPH4+13(L'GRPLAGY),GRPLAGY                                      
         GOTOR AGETAGY,GRPLAGY                                                  
         MVC   REPH4+24(L'CTDSTNAM),PCWORK                                      
                                                                                
         MVC   REPH5+1(L'PCMUSRID),PCMUSRID                                     
         GOTOR AGETUID,GRPLUSER                                                 
         MVC   REPH5+13(L'GIDCODE),PCWORK+(GIDCODE-GIDTABD)                     
         MVC   REPH5+24(L'GIDNAME),PCWORK+(GIDNAME-GIDTABD)                     
                                                                                
         MVI   REPH6+1,0           FORCE HEADLINE 6 TO PRINT                    
         CLI   CSREC,RECGRP        (OTHERWISE BOXES DON'T WORK)                 
         BNE   PRTMID                                                           
         MVC   REPH6+1(L'PCMSYSTM),PCMSYSTM                                     
         GOTOR AGETSYS,GRPLSYST                                                 
         MVC   REPH6+13(L'SYSLNAME),PCWORK                                      
                                                                                
PRTMID   L     RE,REPABOX                                                       
         USING BOXD,RE                                                          
         MVI   BOXCOLS,SPACE                                                    
         MVC   BOXCOLS+1(L'REPP1-1),BOXCOLS                                     
         MVI   BOXROWS,SPACE                                                    
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXROWS+6,BOXRTOPQ                                               
         MVI   BOXROWS+9,BOXRMIDQ                                               
         SR    RF,RF                                                            
         IC    RF,REPMAXL                                                       
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),BOXRBOTQ                                                   
         MVI   BOXCOLS,BOXCLFTQ                                                 
         DROP  RE                                                               
                                                                                
         LA    R2,GRUSEC                                                        
         LA    R3,REPM1+1                                                       
PRTMID02 SR    R6,R6                                                            
         ICM   R6,1,0(R2)          R6=NEXT COLUMN NUMBER                        
         BNZ   *+6                 ZERO=END OF LIST                             
         DC    H'0'                                                             
         BCTR  R6,0                                                             
         MHI   R6,COLTABL                                                       
         AHI   R6,COLTAB-RLP05                                                  
         A     R6,OVNTRYA                                                       
         USING COLTABD,R6                                                       
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,COLTHED1                                                    
         BZ    *+8                                                              
         LA    RE,WORKD(RE)                                                     
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,COLTHED2                                                    
         BZ    *+12                                                             
         LA    RF,WORKD(RF)                                                     
         B     *+8                                                              
         LA    RF,PCSPACES                                                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,COLTHWID                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    R5,R1                                                            
         SHI   R5,1                                                             
         BZ    PRTMID04                                                         
                                                                                
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         LA    R5,0(RE,R5)                                                      
         B     *+8                                                              
         LA    R5,0(RF,R5)                                                      
                                                                                
         CLI   0(R5),SPACE         DO AUTO CENTERING                            
         BNE   *+12                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         DC    H'0'                                                             
                                                                                
         CLM   R1,1,COLTDWID       TEST HEADING WIDER THAN PRINT                
         BNL   PRTMID04                                                         
         SR    R5,R5                                                            
         IC    R5,COLTDWID                                                      
         SR    R5,R1                                                            
         SRL   R5,1                                                             
         CHI   R5,2                TEST CENTERING MAKES SENSE                   
         BNH   PRTMID04                                                         
                                                                                
         LA    R5,0(R3,R5)         R5=A(CENTRED HEADING POSITION)               
         LR    R0,RF                                                            
         SR    RF,RF                                                            
         IC    RF,COLTDWID                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),USCORE                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   L'REPM1(0,R3),USCORE                                             
         LR    RF,R0                                                            
         BCTR  R1,0                                                             
                                                                                
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LA    RE,PCSPACES                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RE)       MOVE CENTRED HEADING TEXT                    
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MVI   0(RE),TOPR                                                       
         LA    RE,2(RE,R1)                                                      
         MVI   0(RE),TOPL                                                       
                                                                                
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,PCSPACES                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'REPM1(0,R5),0(RF)                                              
         LA    RF,L'REPM1-1(R5)                                                 
         BZ    PRTMID08                                                         
         MVI   0(RF),BOTR                                                       
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),BOTL                                                       
         B     PRTMID08                                                         
                                                                                
PRTMID04 BCTR  R1,0                                                             
         LTR   RE,RE                                                            
         BZ    *+18                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'REPM1(0,R3),0(RF)                                              
         OC    COLTHED2,COLTHED2   TEST JUST UNDERSCORED                        
         BNZ   PRTMID08                                                         
         LA    RE,0(R1,R3)         POINT TO END OF MIDLINE WORD                 
         LA    RF,L'REPM1(RE)                                                   
                                                                                
PRTMID06 CLI   0(RE),SPACE         TEST END OF MIDLINE WORD                     
         BH    PRTMID08                                                         
         MVI   0(RF),SPACE         NO - MOVE SPACE TO MIDLINE2                  
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R1,PRTMID06                                                      
                                                                                
PRTMID08 SR    R1,R1                                                            
         ICM   R1,1,COLTHWID                                                    
         CLM   R1,1,COLTDWID                                                    
         BH    *+8                                                              
         ICM   R1,1,COLTDWID                                                    
         LA    R3,1(R3,R1)                                                      
                                                                                
         LR    RE,R3               SET BOX COLUMN                               
         LA    RF,REPM1+1                                                       
         SR    RE,RF               RE=DISPLACEMENT INTO BOXCOLS                 
         L     RF,REPABOX                                                       
         LA    RE,BOXCOLS-BOXD(RE,RF)                                           
         MVI   0(RE),BOXCMIDQ      SET BOX COLUMN CHARACTER                     
                                                                                
         AHI   R2,1                BUMP TO NEXT COLUMN                          
         CLI   0(R2),0             TEST ALL COLUMNS DONE                        
         BNE   PRTMID02                                                         
                                                                                
         MVI   0(RE),BOXCRGTQ      SET BOX RIGHT CHARACTER                      
         MVI   REPIND2,REPILOW+REPIBOX+REPIBOXJ                                 
                                                                                
PRTHKSX  J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA FORMAT ROUTINES                                                *         
*                                                                     *         
* NTRY - R3=A(OUTPUT AREA)                                            *         
***********************************************************************         
                                                                                
         USING RFPD,R5             R5=A(RFP INTERFACE BLOCK)                    
         USING RQHD,RFPVREQH                                                    
DISGRP   DS    0H                  ** DISPLAY GROUP CODE **                     
         MVC   0(L'GRPLGRP,R3),GRPLGRP                                          
DISGRPX  CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
DISDSC   DS    0H                  ** DISPLAY GROUP DESCRIPTION **              
         MVC   0(L'GRPLDESC,R3),GRPLDESC                                        
DISDSCX  CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
DISFRQ   DS    0H                  ** DISPLAY FREQUENCY CODE **                 
         MVC   0(L'GRPLFREQ,R3),GRPLFREQ                                        
DISFRQX  CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
DISFRN   DS    0H                  ** DISPLAY FREQUENCY NAME **                 
         LR    R0,RE                                                            
         GOTOR AGETFRQ,GRPLFREQ                                                 
         MVC   0(FRQNAMLQ,R3),PCWORK                                            
DISFRNX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISLRU   DS    0H                  ** DISPLAY LAST RUN DATE **                  
         LR    R0,RE                                                            
         GOTOR AGETLST                                                          
         JNE   DISLRUX                                                          
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),('SOFIIONE',0)                      
         MVC   0(8,R3),FVIFLD                                                   
DISLRUX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISNRU   DS    0H                  ** DISPLAY NEXT RUN DATE **                  
         LR    R0,RE                                                            
         GOTOR AGETNXT                                                          
         BNE   DISNRUX                                                          
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),('SOFIIONE',0)                      
         MVC   0(8,R3),FVIFLD                                                   
DISNRUX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISERU   DS    0H                  ** DISPLAY END RUN DATE **                   
         LR    R0,RE                                                            
         GOTOR AGETEND                                                          
         BNE   DISERUX                                                          
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),('SOFIIONE',0)                      
         MVC   0(8,R3),FVIFLD                                                   
DISERUX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISEFF   DS    0H                  ** DISPLAY EFFECTIVE DATES **                
         LR    R0,RE                                                            
         OC    GRPLNXTR,GRPLNXTR   TEST NEXT RUN DATE SET                       
         BZ    DISEFFX                                                          
         MVC   PCWORK(L'GRPLNXTR),GRPLNXTR                                      
         MVC   PCWORK+L'GRPLNXTR(L'GRPLEND),GRPLEND                             
         SR    RF,RF                                                            
         OC    GRPLEND,GRPLEND                                                  
         BNZ   *+8                                                              
         LHI   RF,SOFIIONE                                                      
         GOTOR AGOSOFT,PCPARM,('SOFITSD8',PCWORK),                     *        
               ('SOFOTPRT+SOFOTMIX',FVIHDR),((RF),0)                            
         MVC   0(17,R3),FVIFLD                                                  
DISEFFX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISFLT   DS    0H                  ** DISPLAY FILTER NAME **                    
         MVC   0(L'GRPLNAME,R3),GRPLNAME                                        
DISFLTX  CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
DISRUN   DS    0H                  ** DISPLAY RUN SCHEDULE **                   
         LR    R0,RE                                                            
         LA    R1,RFPVDAYS                                                      
         ICM   R1,8,=X'80'                                                      
         GOTOR AGETRSH,(R1)                                                     
         MVC   0(27,R3),PCWORK                                                  
DISRUNX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISCAL   NTR1  LABEL=NO            ** DISPLAY CALENDAR **                       
         LA    R3,0(R3)                                                         
         ST    R3,GRSAVREG                                                      
                                                                                
         MVI   GRLASTM,0                                                        
         OC    RFPVNXTR,RFPVNXTR   TEST DATES ARE SET                           
         BZ    DISCALX                                                          
         OC    RFPVENDR,RFPVENDR                                                
         BZ    DISCALX                                                          
                                                                                
         GOTOR SETRFP              SET GROUP VALUES                             
                                                                                
         MVC   GRLDAY+DAYMONQ-1(1),PCUMON                                       
         MVC   GRLDAY+DAYTUEQ-1(1),PCUTUE                                       
         MVC   GRLDAY+DAYWEDQ-1(1),PCUWED                                       
         MVC   GRLDAY+DAYTHUQ-1(1),PCUTHU                                       
         MVC   GRLDAY+DAYFRIQ-1(1),PCUFRI                                       
         MVC   GRLDAY+DAYSATQ-1(1),PCUSAT                                       
         MVC   GRLDAY+DAYSUNQ-1(1),PCUSUN                                       
                                                                                
         MVC   GRCALY,PCUYES       SET YES AND NO CHARACTERS                    
         MVC   GRCALN,PCUNO                                                     
         OC    RFPBSYMB,RFPBSYMB                                                
         BZ    *+12                                                             
         MVI   GRCALY,C'&&'                                                     
         MVI   GRCALN,C'|'                                                      
                                                                                
         GOTOR VDATCON,PCPARM,(6,RFPVNXTR),(3,GRPSDB)                           
         MVC   GRWSDB,GRPSDB                                                    
         GOTOR VDATCON,PCPARM,(6,RFPVENDR),(3,GRPEDB)                           
                                                                                
         GOTOR VDATCON,PCPARM,(3,GRPSDB),(15,GRPSDJ)                            
         GOTOR VDATCON,PCPARM,(3,GRPEDB),(15,GRPEDJ)                            
                                                                                
         GOTOR VDATCON,PCPARM,(3,GRWSDB),(0,GRWORK)                             
         GOTOR VGETDAY,PCPARM,GRWORK,GRWORK+6                                   
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   GRWORK+6(6),GRWORK                                               
         AHI   R0,-1               TEST START DAY IS A MONDAY                   
         BZ    DISCAL04                                                         
         LCR   R0,R0                                                            
                                                                                
DISCAL02 GOTOR VADDAY,PCPARM,('ADDDAYQ',GRWORK),GRWORK+6,(R0)                   
         GOTOR VDATCON,PCPARM,(0,GRWORK+6),(3,GRWSDB)                           
                                                                                
DISCAL04 GOTOR VADDAY,PCPARM,('ADDDAYQ',GRWORK+6),GRWORK,6                      
         GOTOR VDATCON,PCPARM,(0,GRWORK),(3,GRWEDB)                             
                                                                                
         GOTOR VDATCON,PCPARM,(3,GRWSDB),(15,GRWSDJ)                            
         MVC   GRCDDJ,GRWSDJ       SET AS TODAY TOO                             
         GOTOR VDATCON,PCPARM,(3,GRWEDB),(15,GRWEDJ)                            
                                                                                
         MVC   GRCURRM,GRWSDBM                                                  
         MVC   GRCURRY,GRWSDJY     SET CURRENT YEAR                             
         CLC   GRWSDBM,GRWEDBM     TEST WEEK SPANS A MONTH                      
         BE    DISCAL06                                                         
         CLI   GRLASTM,0           TEST FIRST TIME                              
         BNE   DISCAL06                                                         
         MVC   GRCURRM,GRWEDBM                                                  
         MVC   GRCURRY,GRWEDJY     SET CURRENT YEAR                             
                                                                                
DISCAL06 CLC   GRLASTM,GRCURRM     TEST CHANGE OF MONTH                         
         BE    DISCAL08                                                         
                                                                                
         CLI   GRLASTM,0           TEST FIRST TIME                              
         BE    *+12                                                             
         AHI   R2,CALTABL          NO - BUMP TO NEXT ENTRY IN CALTAB            
         B     *+12                                                             
         LHI   R2,CALTAB-RLP05     YES - POINT TO FIRST ENTRY IN CALTAB         
         A     R2,OVNTRYA                                                       
         USING CALTABD,R2                                                       
         MVC   GRLASTM,GRCURRM                                                  
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CALTHEDL                                                    
         A     RE,GRSAVREG                                                      
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,GRCURRM                                                     
         MHI   RF,L'PCMJAN                                                      
         LA    RF,PCMJAN-L'PCMJAN(RF)                                           
         MVC   0(L'PCMJAN,RE),0(RF)                                             
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CALTSUBL                                                    
         A     RE,GRSAVREG                                                      
         UNPK  PCDUB(6),GRCURRY(3)                                              
         MVI   0(RE),DINK                                                       
         MVC   1(2,RE),PCDUB+3                                                  
         MVC   4(L'GRLDAY,RE),GRLDAY                                            
         LA    R3,CALTWK1H         POINT TO FIRST WEEK IN MONTH                 
         B     DISCAL10                                                         
                                                                                
DISCAL08 AHI   R3,L'CALTWK1H       BUMP TO NEXT WEEK IN MONTH                   
                                                                                
DISCAL10 SR    R4,R4                                                            
         ICM   R4,3,0(R3)                                                       
         A     R4,GRSAVREG                                                      
         USING WKDD,R4                                                          
         SR    RF,RF               MOVE W/S DAY TO SCREEN                       
         IC    RF,GRWSDBD                                                       
         CVD   RF,PCDUB                                                         
         OI    PCDUB+L'PCDUB-1,DIGIT                                            
         UNPK  WKDSDAY(2),PCDUB                                                 
         CLI   WKDSDAY,NUMERIC                                                  
         BNE   *+14                                                             
         MVC   WKDSDAY(1),WKDSDAY+1                                             
         MVI   WKDSDAY+1,SPACE                                                  
                                                                                
         MVI   WKDDAYS,CANUSEDQ                                                 
         MVC   WKDDAYS+1(L'WKDDAYS-1),WKDDAYS                                   
                                                                                
         MVC   GRCDDJ,GRWSDJ       SET CURRENT DATE                             
         LA    R6,WKDDAYS          POINT TO OUTPUT LIST                         
         LHI   R0,L'WKDDAYS                                                     
         MVI   GRNOTCT,0           COUNT OF NUMBER OF UNUSABLE DAYS             
                                                                                
DISCAL14 CLC   GRCDDJ,GRPSDJ       TEST DAY WITHIN THIS PERIOD                  
         BL    DISCAL18                                                         
         CLC   GRCDDJ,GRPEDJ                                                    
         BH    DISCAL18                                                         
         ZAP   PCDUB,GRCDDJD                                                    
         CVB   RE,PCDUB                                                         
         SRDL  RE,3                SHIFT OFF DAY INTO RF                        
         SRL   RF,32-3                                                          
         LA    RE,RFPVRNGE(RE)                                                  
         IC    RF,DAYBITS(RF)      POINT TO BIT MASK FOR DAY                    
         EX    RF,*+8                                                           
         BZ    DISCAL16                                                         
         TM    0(RE),0             TEST DAY BIT IS ON                           
         GOTOR DISDAY,(R6)                                                      
         B     DISCAL20                                                         
                                                                                
DISCAL16 GOTOR AGETWRK,GRCDDJ      TEST IF THIS IS A WORKING DAY                
         BE    DISCAL20            EQUAL MEANS A GOOD DAY                       
         BH    DISCAL18            HIGH MEANS OUT OF CALENDAR RANGE             
         MVC   0(1,R6),GRCALN      LOW MEANS HOLIDAYS ETC.                      
         B     *+8                                                              
DISCAL18 MVI   0(R6),NOTINPDQ      '*' MARKS DAYS NOT IN PERIOD                 
         IC    R1,GRNOTCT          INCREMENT NON-WORKING DAY COUNT              
         AHI   R1,1                                                             
         STC   R1,GRNOTCT                                                       
                                                                                
DISCAL20 GOTOR VDATCON,PCPARM,(6,GRCDDJ),(0,GRWORK)                             
         GOTOR VADDAY,(R1),('ADDDAYQ',GRWORK),GRWORK+6,1                        
         GOTOR VDATCON,(R1),(0,GRWORK+6),(15,GRCDDJ)                            
         AHI   R6,1                                                             
         BCT   R0,DISCAL14                                                      
                                                                                
         CLC   GRWEDB,GRPEDB       TEST REACHED END OF PERIOD                   
         BNL   DISCALX                                                          
         GOTOR VDATCON,PCPARM,(3,GRWEDB),(0,GRWORK)                             
         LHI   R0,1                                                             
         B     DISCAL02                                                         
                                                                                
DISCALX  J     EXITY                                                            
         DROP  R2,R4                                                            
                                                                                
DISDAY   OC    RFPBSYMB,RFPBSYMB   TEST ADJUSTMENT SYMBOLIC PRESENT             
         BNZ   *+12                                                             
         MVC   0(1,R1),GRCALY      NO - OUTPUT 'Y'                              
         BR    RE                                                               
                                                                                
         STM   RE,R1,12(RD)                                                     
         SR    RF,RF               STRIP SIGN FROM PACKED DAY                   
         ICM   RF,3,GRCDDJD                                                     
         SRL   RF,4                                                             
         STH   RF,OVHALF1          AND SAVE FOR COMPARE                         
         LA    RF,RFPBADJS         RE=A(ADJUSTMENT TABLE)                       
         LHI   R0,RFPBADJM         R0=MAXIMUM NUMBER OF ENTRIES                 
DISDAY02 OC    0(L'RFPBADJS,RF),0(RF)                                           
         BZ    DISDAY04                                                         
         MVC   OVHALF2,0(RF)       EXTRACT DAY                                  
         NC    OVHALF2,=AL2(GRPBACAL)                                           
         CLC   OVHALF1,OVHALF2     TEST DAY MATCHES                             
         BE    DISDAY06                                                         
         AHI   RF,L'RFPBADJS       NO - BUMP TO NEXT                            
         BCT   R0,DISDAY02                                                      
                                                                                
DISDAY04 MVC   0(1,R1),GRCALY      OUTPUT 'Y' IF NO ADJUSTMENT                  
         B     DISDAYX                                                          
                                                                                
DISDAY06 SR    RE,RE               ISOLATE ADJUSTMENT VALUE                     
         ICM   RE,3,0(RF)                                                       
         SRL   RE,10                                                            
         LA    RE,DAYTAB-1(RE)     INDEX INTO DAY TABLE                         
         MVC   0(1,R1),0(RE)                                                    
                                                                                
DISDAYX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
         USING REPD,R4             R4=A(REPORT INTERFACE BLOCK)                 
DISSYM   NTR1  LABEL=NO            ** DISPLAY RFP SYMBOLS **                    
         CLI   CSREC,RECGRP                                                     
         BNE   DISSYM04                                                         
         GOTOR SETRFP                                                           
         SR    R2,R2                                                            
         ICM   R2,1,RFPVNUMS       R2=N'RFP SYMBOLS                             
         BZ    DISSYMX                                                          
         GOTOR AGETSYS,GRPLSYST                                                 
         MVC   PCWORK(1),PCWORK+L'SYSLNAME+L'SYSLUSLT                           
         LA    R6,RFPD                                                          
         AHI   R6,RFPVSYMX-RFPD                                                 
         USING RFPVSYME,R6                                                      
DISSYM02 LA    R1,PCPARM                                                        
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNS      ISSUE SINGLE UPPER TRANSLATE                 
         MVI   DDRETN,DDCASEU                                                   
         MVC   DDSYS,PCWORK                                                     
         MVC   DDLANG,CULANG                                                    
         MVC   0(L'RFPVSYME-1,R3),RFPVSYME                                      
         MVI   L'RFPVSYME-1(R3),L'PCMKEYWD                                      
         STCM  R3,7,DDIADR                                                      
         STCM  R3,7,DDOADR                                                      
         GOTOR VDICTAT             RESOLVE RFP SYMBOL                           
         AHI   R6,RFPVSYML         BUMP TO NEXT SYMBOL                          
         AHI   R3,L'REPP1          BUMP TO NEXT OUTPUT LINE                     
         BCT   R2,DISSYM02         DO FOR NUMBER OF SYMBOLS                     
         B     DISSYMX                                                          
         DROP  R6                                                               
                                                                                
DISSYM04 CLI   CSREC,RECXFG        DISPLAY SYMBOLS FOR XFILE GROUP              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    GRSYMV(GRSYMVL),GRSYMV                                           
         L     R2,AIO1                                                          
         AHI   R2,XFFRSTEL-XFILED                                               
         USING XFSGRPD,R2                                                       
         SR    R0,R0                                                            
                                                                                
DISSYM06 CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    DISSYM16                                                         
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   DISSYM14                                                         
                                                                                
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUUSER,GRPLUSER                                                  
         GOTOR AGETSYS,XFSGSYS                                                  
         MVC   GRSYSNUM,PCWORK+L'SYSLNAME+L'SYSLUSLT                            
         MVC   CUSYSL,PCWORK+L'SYSLNAME                                         
         LA    R1,XFSGGRP                                                       
         ICM   R1,8,=AL1(RFPXRDUP+RFPXSYMS)                                     
         GOTOR AVALGRP                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ICM   R0,1,RFPVNUMS       R0=N'RFP SYMBOLS                             
         BZ    DISSYM14                                                         
         LA    R6,RFPD                                                          
         AHI   R6,RFPVSYMX-RFPD                                                 
         USING RFPVSYME,R6                                                      
                                                                                
DISSYM08 MVC   PCWORK+0(L'GRSYMSYS),GRSYSNUM                                    
         MVC   PCWORK+L'GRSYMSYS(L'GRSYMDIC),RFPVSYME                           
         LA    RF,GRSYMLST                                                      
         SR    RE,RE                                                            
         ICM   RE,1,GRSYMNUM                                                    
         BZ    DISSYM10                                                         
         CLC   0(GRSYML,RF),PCWORK                                              
         BE    DISSYM12                                                         
         AHI   RF,GRSYML                                                        
         BCT   RE,*-14                                                          
                                                                                
DISSYM10 MVC   0(GRSYML,RF),PCWORK                                              
         IC    RE,GRSYMNUM                                                      
         AHI   RE,1                                                             
         STC   RE,GRSYMNUM                                                      
                                                                                
DISSYM12 AHI   R6,RFPVSYML         BUMP TO NEXT SYMBOL                          
         BCT   R0,DISSYM08         DO FOR NUMBER OF SYMBOLS                     
         DROP  R6                                                               
                                                                                
DISSYM14 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     DISSYM06                                                         
                                                                                
DISSYM16 LA    R6,GRSYMLST                                                      
         USING GRSYMLST,R6                                                      
         SR    R0,R0                                                            
         ICM   R0,1,GRSYMNUM                                                    
         BZ    DISSYMX                                                          
DISSYM18 GOTOR AGETSYS,GRSYMSYS    LOOK UP SYSTEM NAME                          
         MVC   0(L'SYSLNAME,R3),PCWORK                                          
         LA    R1,PCPARM                                                        
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNS      ISSUE SINGLE UPPER TRANSLATE                 
         MVI   DDRETN,DDCASEU                                                   
         MVC   DDSYS,GRSYMSYS                                                   
         MVC   DDLANG,CULANG                                                    
         LR    RE,R3                                                            
         AHI   RE,L'SYSLNAME-1                                                  
         CLI   0(RE),SPACE                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVC   1(L'PCSLASH,RE),PCSLASH                                          
         AHI   RE,1+L'PCSLASH                                                   
         MVC   0(L'GRSYMDIC,RE),GRSYMDIC                                        
         MVI   L'GRSYMDIC(RE),L'PCMKEYWD                                        
         STCM  RE,7,DDIADR                                                      
         STCM  RE,7,DDOADR                                                      
         GOTOR VDICTAT             RESOLVE RFP SYMBOL                           
                                                                                
         AHI   R6,GRSYML                                                        
         AHI   R3,L'REPP1                                                       
         BCT   R0,DISSYM18                                                      
         DROP  R1,R6                                                            
                                                                                
DISSYMX  J     EXITY                                                            
                                                                                
DISDST   DS    0H                  DISPLAY DESTINATION ID                       
         LR    R0,RE                                                            
         OC    GRPLDEST,GRPLDEST                                                
         BE    DISDSTX                                                          
         GOTOR AGETUID,GRPLDEST                                                 
         MVC   0(L'GIDCODE,R3),PCWORK+(GIDCODE-GIDTABD)                         
DISDSTX  CR    RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
DISOTY   DS    0H                  ** DISPLAY (GROUP) OUTPUT TYPE **            
         MVC   0(L'GRPLOTYP,R3),GRPLOTYP                                        
DISOTYX  CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
DISREQ   NTR1  LABEL=NO            ** DISPLAY REQUEST DETAILS **                
         LHI   R0,REPPNUM          R0=N'AVAILABLE PRINT LINES                   
                                                                                
         TM    GRFLAG,GRFFIRST     TEST FIRST TIME THROUGH                      
         BNZ   DISREQ02                                                         
         XC    GRSVALS(GRSVALSL),GRSVALS                                        
         OI    GRFLAG,GRFFIRST                                                  
                                                                                
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUSYSL,GRPLSYST                                                  
         LA    R1,GRPLGRP                                                       
         ICM   R1,8,=AL1(RFPXRDUP+RFPXSYMS)                                     
         GOTOR AVALGRP                                                          
         BE    DISREQ02                                                         
         DC    H'0'                                                             
                                                                                
DISREQ02 MVC   RFPFRQID,GRSFRQID                                                
         MVC   RFPFSORT,GRSFSORT                                                
         MVC   RFPFSEQN,GRSFSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPMODE,RFPRETRQ                                                 
                                                                                
DISREQ04 GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   GRSFRQID,RFPVRQID   SAVE CURRENT VALUES                          
         MVC   GRSFSORT,RFPVSORT                                                
         MVC   GRSFSEQN,RFPVSEQN                                                
                                                                                
         CLI   RFPRET,RFPEOF       TEST END OF FILE                             
         BE    DISREQ18                                                         
         OC    REQPROG,REQPROG     TEST PROGRAM FILTER                          
         BZ    *+12                                                             
         GOTOR FLTPRG              APPLY PROGRAM FILTER                         
         BNE   DISREQ04                                                         
                                                                                
         CHI   R0,5                TEST 5 LINES AVAILABLE                       
         BL    DISREQ18                                                         
                                                                                
         LR    R2,R3                                                            
         OC    RQHOUT,RQHOUT                                                    
         BZ    DISREQ06                                                         
         MVC   0(L'PCMOTYP,R2),PCMOTYP                                          
         AHI   R2,L'PCMOTYP-1                                                   
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCEQUAL,R2),PCEQUAL                                          
         AHI   R2,1+L'PCEQUAL                                                   
         MVC   0(L'RQHOUT,R2),RQHOUT                                            
         AHI   R2,L'RQHOUT-1                                                    
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA+1                                                   
                                                                                
DISREQ06 OC    RQHDEST,RQHDEST                                                  
         BZ    DISREQ10                                                         
         MVC   0(L'PCMDSTID,R2),PCMDSTID                                        
         AHI   R2,L'PCMDSTID-1                                                  
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCEQUAL,R2),PCEQUAL                                          
         AHI   R2,1+L'PCEQUAL                                                   
         OC    RFPVFXID,RFPVFXID                                                
         BZ    DISREQ08                                                         
         MVC   0(L'PCUFAXP,R2),PCUFAXP                                          
         AHI   R2,L'PCUFAXP                                                     
         MVC   0(L'RFPVFXID,R2),RFPVFXID                                        
         AHI   R2,L'RFPVFXID-1                                                  
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA+1                                                   
         B     DISREQ10                                                         
                                                                                
DISREQ08 GOTOR AGETUID,RQHDEST                                                  
         MVC   0(L'GIDCODE,R2),PCWORK+(GIDCODE-GIDTABD)                         
         AHI   R2,L'GIDCODE-1                                                   
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA+1                                                   
                                                                                
DISREQ10 CLC   RFPVRDSC,PCSPACES                                                
         BNH   DISREQ12                                                         
         MVC   0(L'PCMDESC,R2),PCMDESC                                          
         AHI   R2,L'PCMDESC-1                                                   
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCEQUAL,R2),PCEQUAL                                          
         AHI   R2,1+L'PCEQUAL                                                   
         MVC   0(L'RFPVRDSC,R2),RFPVRDSC                                        
         AHI   R2,L'RFPVRDSC-1                                                  
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA+1                                                   
                                                                                
DISREQ12 MVC   0(L'PCMRUNQ,R2),PCMRUNQ                                          
         AHI   R2,L'PCMRUNQ-1                                                   
         CLI   0(R2),SPACE                                                      
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCEQUAL,R2),PCEQUAL                                          
         AHI   R2,1+L'PCEQUAL                                                   
         LA    RE,PCMYES                                                        
         CLI   RFPVREQD,REQLRSRD                                                
         BNE   *+8                                                              
         LA    RE,PCMNO                                                         
         MVC   0(L'PCMYES,R2),0(RE)                                             
         AHI   R2,L'PCMYES-1                                                    
         CLI   0(R2),SPACE                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCCOMMA,R2),PCCOMMA                                          
         AHI   R2,L'PCCOMMA+1                                                   
                                                                                
         MVC   0(L'PCMREQR,R2),PCMREQR                                          
         AHI   R2,L'PCMREQR-1                                                   
         CLI   0(R2),SPACE                                                      
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVC   1(L'PCEQUAL,R2),PCEQUAL                                          
         AHI   R2,1+L'PCEQUAL                                                   
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,RFPVRQSN                                                    
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         LHI   RF,L'RFPVREQC                                                    
         MR    RE,RE                                                            
         LA    RF,RFPVREQC-1(RF)                                                
         IC    RE,RFPVRQSC                                                      
         AR    RF,RE                                                            
         LHI   RE,L'REQLREQR-1                                                  
         TM    RFPVSTAT,RFPVSPOF                                                
         BZ    *+8                                                              
         LHI   RE,3-1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
                                                                                
         AHI   R3,L'REPP1                                                       
         BCTR  R0,0                                                             
                                                                                
         SR    RF,RF                                                            
         TM    RFPVSTAT,RFPVSPOF   TEST SPOOF REQUEST                           
         BZ    *+8                                                              
         LHI   RF,X'40'                                                         
         GOTOR VREQRFP,PCPARM,((RF),RFPVREQH),(RFPFSYS,ACOM),          *        
               (CULANG,0),(3,0),(X'FF',ARFPBUFF)                                
                                                                                
         LA    R1,RFPVREQC                                                      
         LHI   RF,3                                                             
DISREQ14 CLC   0(L'RFPVREQC,R1),PCSPACES                                        
         BE    DISREQ16                                                         
         MVC   0(L'RFPVREQC-1,R3),0(R1)                                         
         AHI   R3,L'REPP1                                                       
         AHI   R1,L'RFPVREQC                                                    
         BCTR  R0,0                                                             
         BCT   RF,DISREQ14                                                      
                                                                                
DISREQ16 MVC   0(L'RFPVREQC,R3),USCORE                                          
         AHI   R3,L'REPP1                                                       
         BCT   R0,DISREQ04                                                      
         J     EXITN                                                            
                                                                                
DISREQ18 CLI   RFPRET,RFPEOF       TEST END OF FILE                             
         JNE   EXITN                                                            
         SHI   R3,L'REPP1                                                       
         CLC   0(L'RFPVREQC,R3),USCORE                                          
         BNE   *+10                                                             
         MVC   0(L'RFPVREQC,R3),PCSPACES                                        
         J     EXITY                                                            
                                                                                
DISXFG   NTR1  LABEL=NO            ** DISPLAY XFILE GROUP **                    
         MVC   0(L'RFPXFILE,R3),RFPXFILE                                        
         CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BE    DISXFGX                                                          
                                                                                
         CLI   CSREC,RECXFG        TEST XFILE RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,REPPNUM                                                       
         ICM   R2,15,GRSAVGRP                                                   
         BNZ   DISXFG04                                                         
         L     R2,AIO1                                                          
         AHI   R2,XFFRSTEL-XFILED                                               
         USING XFSGRPD,R2                                                       
         SR    R0,R0                                                            
                                                                                
DISXFG02 CLI   XFSGCD,0            TEST END OF RECORD                           
         BE    DISXFGX                                                          
         CLI   XFSGCD,XFSGCDEQ     TEST SYSTEM/GROUP ELEMENT                    
         BNE   DISXFG06                                                         
                                                                                
DISXFG04 BCT   R4,*+12             DO FOR NUMBER OF LINES IN BUFFER             
         ST    R2,GRSAVGRP                                                      
         J     EXITN                                                            
                                                                                
         GOTOR AGETSYS,XFSGSYS     FORMAT 'SYSTEM/GROUP'                        
         LA    RE,0(R3)                                                         
         MVC   0(L'SYSLNAME,RE),PCWORK                                          
         AHI   RE,L'SYSLNAME-1                                                  
         CLI   0(RE),SPACE                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVC   1(L'PCSLASH,RE),PCSLASH                                          
         MVC   2(L'XFSGGRP,RE),XFSGGRP                                          
         AHI   R3,L'REPP1          BUMP TO NEXT PRINT LINE                      
                                                                                
DISXFG06 IC    R0,XFSGLN           BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     DISXFG02                                                         
                                                                                
DISXFGX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SET SUNDRY RFP VALUES FOR GROUP VIA RFPIO                           *         
***********************************************************************         
                                                                                
SETRFP   CLI   CSREC,RECGRP        TEST GROUP RECORD                            
         BNER  RE                                                               
         TM    GRFLAG,GRFVALID     TEST ALREADY VALIDATED                       
         BNZR  RE                                                               
                                                                                
SETRFPN  NTR1  LABEL=NO                                                         
         MVC   CUAALF,GRPLAGY                                                   
         MVC   CUUSER,GRPLUSER                                                  
         MVC   CUSYSL,GRPLSYST                                                  
         LA    R1,GRPLGRP                                                       
         ICM   R1,8,=AL1(RFPXRDUP+RFPXSYMS)                                     
         GOTOR AVALGRP             VALIDATE GROUP (SETS RFP SYMBOLS)            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    RFPFRQID,RFPFRQID                                                
         XC    RFPFSORT,RFPFSORT                                                
         XC    RFPFSEQN,RFPFSEQN                                                
         OI    RFPFFLAG,RFPF1STR                                                
         MVI   RFPMODE,RFPRETRQ    RETURN (FIRST) REQUEST                       
         GOTOR VRFPIO,PCPARM,RFPD                                               
         NI    RFPFFLAG,FF-(RFPF1STR)                                           
         CLI   RFPERROR,RFPNOERR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   RFPVSEQN,0                                                       
         CLI   RFPRET,RFPEOF       TEST END OF REQUESTS                         
         BE    *+8                                                              
         MVI   RFPVSEQN,1          NO - SET N'REQUESTS NON-ZERO                 
                                                                                
SETRFPX  OI    GRFLAG,GRFVALID     SET GROUP VALIDATED                          
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* APPLY PROGRAM FILTER                                                *         
***********************************************************************         
                                                                                
FLTPRG   LA    RF,RFPVRQID                                                      
         CLI   RFPFSYS,RFPFSRPL                                                 
         JNE   FLTPRG04                                                         
         CLI   0(RF),C'R'                                                       
         JNE   FLTPRG04                                                         
         CLI   1(RF),C'X'                                                       
         JE    FLTPRG02                                                         
         CLI   1(RF),C'1'                                                       
         JL    FLTPRG04                                                         
FLTPRG02 LA    RF,RFPVREQC+77                                                   
FLTPRG04 CLC   REQPROG,0(RF)                                                    
         BR    RE                                                               
         EJECT                                                                  
NEXTCOLQ EQU   C'*'                NEXT COLUMN INPUT CHARACTER                  
                                                                                
TWAROWS  EQU   24                  N'ROWS ON SCREEN                             
TWACOLS  EQU   80                  N'COLUMNS ON SCREEN                          
                                                                                
DAYMONQ  EQU   1                   MONDAY                                       
DAYTUEQ  EQU   2                   TUESDAY                                      
DAYWEDQ  EQU   3                   WEDNESDAY                                    
DAYTHUQ  EQU   4                   THURSDAY                                     
DAYFRIQ  EQU   5                   FRIDAY                                       
DAYSATQ  EQU   6                   SATURDAY                                     
DAYSUNQ  EQU   7                   SUNDAY                                       
                                                                                
NOTINPDQ EQU   C'*'                DAY NOT IN PERIOD                            
CANUSEDQ EQU   C'-'                DAY AVAILABLE FOR USE                        
DINK     EQU   C''''               FOR 'YY DISPLAY                              
                                                                                
ADDDAYQ  EQU   C'D'                ADDAY DAYS PARAMETER                         
ADDMONQ  EQU   C'M'                ADDAY MONTHS PARAMETER                       
                                                                                
BOXRTOPQ EQU   C'T'                BOX ROW TOP                                  
BOXRMIDQ EQU   C'M'                BOX ROW MIDDLE                               
BOXRBOTQ EQU   C'B'                BOX ROW BOTTOM                               
BOXCLFTQ EQU   C'L'                BOX COLUMN LEFT                              
BOXCMIDQ EQU   C'C'                BOX COLUMN MIDDLE                            
BOXCRGTQ EQU   C'R'                BOX COLUMN RIGHT                             
                                                                                
XFGREPSQ EQU   X'E5'               DUMMY SCREEN PHASE NUMBER                    
                                                                                
         LTORG                                                                  
                                                                                
USCORE   DC    80X'BF'                                                          
                                                                                
DAYBITS  DC    X'8040201008040201'                                              
                                                                                
DAYTAB   DC    C'123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'                           
                                                                                
PRTSPC   DS    0X                  ** PRINT SPECS **                            
                                                                                
         SPROG 1,2                                                              
         SPEC  H1,2,RUN                                                         
         SPEC  H1,69,REPORT                                                     
         SPEC  H2,69,PAGE                                                       
                                                                                
         SPROG 1                                                                
         SPEC  H1,35,GE#RFPGL,30   RFP GROUP LISTING                            
         SPEC  H2,35,GE#RFPGL,30,LU                                             
                                                                                
         SPROG 2                                                                
         SPEC  H1,35,GE#RFPXL,30   RFP XFILE GROUP LISTING                      
         SPEC  H2,35,GE#RFPXL,30,LU                                             
                                                                                
PRTSPCX  SPEC  END                                                              
         EJECT                                                                  
KEYTGRP  DS    0X                  ** GROUP KEY DRIVER TABLE **                 
                                                                                
         DC    AL1(GRPKSYS-GRPKEY,L'GRPKSYS-1)                                  
         DC    AL1(GRPKSYSQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKSTYP-GRPKEY,L'GRPKSTYP-1)                                
         DC    AL1(GRPKSTYQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKSYST-GRPKEY,L'GRPKSYST-1)                                
         DC    AL2(SYSFLTS-TWAD)                                                
         DC    AL1(SYSFMAXN)                                                    
         DC    AL1(KEYTITAB+KEYTIATB)                                           
         DC    AL1(KEYTLIST)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKSYST-GRPKEY,L'GRPKSYST-1)                                
         DC    AL2(SYSFLTS-TWAD)                                                
         DC    AL1(SYSFMAXN)                                                    
         DC    AL1(KEYTIRNG+KEYTIATB)                                           
         DC    AL1(KEYTRNGE)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKAGY-GRPKEY,L'GRPKAGY-1)                                  
         DC    AL2(AGYFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKUSER-GRPKEY,L'GRPKUSER-1)                                
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG+KEYTIATB)                                           
         DC    AL1(KEYTRNGE)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKUSER-GRPKEY,L'GRPKUSER-1)                                
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(UIDFMAXN)                                                    
         DC    AL1(KEYTITAB+KEYTIATB)                                           
         DC    AL1(KEYTLIST)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(GRPKGRP-GRPKEY,L'GRPKGRP-1)                                  
         DC    AL2(GRPFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
KEYGRPX  DC    AL1(KEYTEOTQ)                                                    
         EJECT                                                                  
KEYTXFG  DS    0X                  ** XFILE KEY DRIVER TABLE **                 
                                                                                
         DC    AL1(XFKSYS-XFILED,L'XFKSYS-1)                                    
         DC    AL1(XFKSYSQ,0)                                                   
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFKSTYP-XFILED,L'XFKSTYP-1)                                  
         DC    AL1(XFKSTYPQ,0)                                                  
         DC    AL1(0)                                                           
         DC    AL1(KEYTILIT)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFAGY-XFKEY,L'XFAGY-1)                                       
         DC    AL2(AGYFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFUSER-XFKEY,L'XFUSER-1)                                     
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG+KEYTIATB)                                           
         DC    AL1(KEYTRNGE)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFUSER-XFKEY,L'XFUSER-1)                                     
         DC    AL2(UIDFLTS-TWAD)                                                
         DC    AL1(UIDFMAXN)                                                    
         DC    AL1(KEYTITAB+KEYTIATB)                                           
         DC    AL1(KEYTLIST)                                                    
         DC    AL1(0)                                                           
                                                                                
         DC    AL1(XFGRP-XFKEY,L'XFGRP-1)                                       
         DC    AL2(GRPFLTS-TWAD)                                                
         DC    AL1(0)                                                           
         DC    AL1(KEYTIRNG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
KEYTXFGX DC    AL1(KEYTEOTQ)                                                    
         EJECT                                                                  
COLTAB   DS    0X                  ** REPORT COLUMN DEFINITIONS **              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(RECGRP)                                                      
         DC    AL2(REPGRPLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALGRP-RLP05)                                                
         DC    AL2(DISGRP-RLP05)                                                
         DC    AL1(L'PCMGRPC1)                                                  
         DC    AL1(L'RFPVGRP)                                                   
         DC    AL2(PCMGRPC1-WORKD)                                              
         DC    AL2(PCMGRPC2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(RECXFG)                                                      
         DC    AL2(REPGRPLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALGRP-RLP05)                                                
         DC    AL2(DISGRP-RLP05)                                                
         DC    AL1(L'PCMXFGC1)                                                  
         DC    AL1(L'RFPXFILE)                                                  
         DC    AL2(PCMXFGC1-WORKD)                                              
         DC    AL2(PCMXFGC2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPDSCLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALDSC-RLP05)                                                
         DC    AL2(DISDSC-RLP05)                                                
         DC    AL1(L'PCMDESC)                                                   
         DC    AL1(L'RFPVDESC)                                                  
         DC    AL2(0)                                                           
         DC    AL2(PCMDESC-WORKD)                                               
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPFRQLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALFRQ-RLP05)                                                
         DC    AL2(DISFRQ-RLP05)                                                
         DC    AL1(L'RFPVFREQ)                                                  
         DC    AL1(L'RFPVFREQ)                                                  
         DC    AL2(0)                                                           
         DC    AL2(PCMFRQCY-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPFRNLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(DISFRN-RLP05)                                                
         DC    AL1(L'PCMFRQCY)                                                  
         DC    AL1(FRQNAMLQ)                                                    
         DC    AL2(0)                                                           
         DC    AL2(PCMFRQCY-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPLRULH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALLRU-RLP05)                                                
         DC    AL2(DISLRU-RLP05)                                                
         DC    AL1(L'PCMLRUN1)                                                  
         DC    AL1(8)                                                           
         DC    AL2(PCMLRUN1-WORKD)                                              
         DC    AL2(PCMLRUN2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPNRULH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALNRU-RLP05)                                                
         DC    AL2(DISNRU-RLP05)                                                
         DC    AL1(L'PCMNRUN1)                                                  
         DC    AL1(8)                                                           
         DC    AL2(PCMNRUN1-WORKD)                                              
         DC    AL2(PCMNRUN2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPERULH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALERU-RLP05)                                                
         DC    AL2(DISERU-RLP05)                                                
         DC    AL1(L'PCMERUN1)                                                  
         DC    AL1(8)                                                           
         DC    AL2(PCMERUN1-WORKD)                                              
         DC    AL2(PCMERUN2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPEFFLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALEFF-RLP05)                                                
         DC    AL2(DISEFF-RLP05)                                                
         DC    AL1(L'PCMEFFDT)                                                  
         DC    AL1(17)                                                          
         DC    AL2(0)                                                           
         DC    AL2(PCMEFFDT-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPFLTLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALFLT-RLP05)                                                
         DC    AL2(DISFLT-RLP05)                                                
         DC    AL1(L'PCMGNAM1)                                                  
         DC    AL1(L'RFPVNAME)                                                  
         DC    AL2(PCMGNAM1-WORKD)                                              
         DC    AL2(PCMGNAM2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPRSHLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALRUN-RLP05)                                                
         DC    AL2(DISRUN-RLP05)                                                
         DC    AL1(L'PCMRUNSH)                                                  
         DC    AL1(27)                                                          
         DC    AL2(0)                                                           
         DC    AL2(PCMRUNSH-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPCALLH-TWAD)                                               
         DC    AL1(COLTIMLT)                                                    
         DC    AL2(VALCAL-RLP05)                                                
         DC    AL2(DISCAL-RLP05)                                                
         DC    AL1(L'PCMCALDR)                                                  
         DC    AL1(76)                                                          
         DC    AL2(0)                                                           
         DC    AL2(PCMCALDR-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(RECGRP)                                                      
         DC    AL2(REPSYMLH-TWAD)                                               
         DC    AL1(COLTIMLT)                                                    
         DC    AL2(VALSYM-RLP05)                                                
         DC    AL2(DISSYM-RLP05)                                                
         DC    AL1(L'PCMKEYWD)                                                  
         DC    AL1(L'PCMKEYWD)                                                  
         DC    AL2(0)                                                           
         DC    AL2(PCMKEYWD-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(RECXFG)                                                      
         DC    AL2(REPSYMLH-TWAD)                                               
         DC    AL1(COLTIMLT)                                                    
         DC    AL2(VALSYM-RLP05)                                                
         DC    AL2(DISSYM-RLP05)                                                
         DC    AL1(L'PCMKEYWD)                                                  
         DC    AL1(L'SYSLNAME+L'PCSLASH+L'PCMKEYWD)                             
         DC    AL2(0)                                                           
         DC    AL2(PCMKEYWD-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPDSTLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALDST-RLP05)                                                
         DC    AL2(DISDST-RLP05)                                                
         DC    AL1(L'PCMDSTID)                                                  
         DC    AL1(L'GIDCODE)                                                   
         DC    AL2(0)                                                           
         DC    AL2(PCMDSTID-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPOTYLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALOTY-RLP05)                                                
         DC    AL2(DISOTY-RLP05)                                                
         DC    AL1(L'PCMOTYP1)                                                  
         DC    AL1(L'RFPVOTYP)                                                  
         DC    AL2(PCMOTYP1-WORKD)                                              
         DC    AL2(PCMOTYP2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(RECGRP)                                                      
         DC    AL2(REPXFGLH-TWAD)                                               
         DC    AL1(0)                                                           
         DC    AL2(VALXFG-RLP05)                                                
         DC    AL2(DISXFG-RLP05)                                                
         DC    AL1(L'PCMXFGC1)                                                  
         DC    AL1(L'RFPXFILE)                                                  
         DC    AL2(PCMXFGC1-WORKD)                                              
         DC    AL2(PCMXFGC2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(RECXFG)                                                      
         DC    AL2(REPXFGLH-TWAD)                                               
         DC    AL1(COLTIMLT)                                                    
         DC    AL2(VALXFG-RLP05)                                                
         DC    AL2(DISXFG-RLP05)                                                
         DC    AL1(L'PCMGRPC1)                                                  
         DC    AL1(L'RFPVGRP+L'PCSLASH+L'SYSLNAME)                              
         DC    AL2(PCMGRPC1-WORKD)                                              
         DC    AL2(PCMGRPC2-WORKD)                                              
                                                                                
         DC    AL1(((*-COLTAB)/COLTABL)+1)                                      
         DC    AL1(0)                                                           
         DC    AL2(REPRQSLH-TWAD)                                               
         DC    AL1(COLTIMLT)                                                    
         DC    AL2(VALREQ-RLP05)                                                
         DC    AL2(DISREQ-RLP05)                                                
         DC    AL1(L'PCMRQDET)                                                  
         DC    AL1(79)                                                          
         DC    AL2(0)                                                           
         DC    AL2(PCMRQDET-WORKD)                                              
                                                                                
COLTABX  DC    AL1(COLTEOTQ)                                                    
         EJECT                                                                  
CALTAB   DS    0X                  ** DISPLACEMENT TABLE **                     
                                                                                
         DC    AL2(REPP1+COL1DISP-REPP1)                                        
         DC    AL2(REPP2+COL1DISP-REPP1)                                        
         DC    AL2(REPP3+COL1DISP-REPP1)                                        
         DC    AL2(REPP4+COL1DISP-REPP1)                                        
         DC    AL2(REPP5+COL1DISP-REPP1)                                        
         DC    AL2(REPP6+COL1DISP-REPP1)                                        
         DC    AL2(REPP7+COL1DISP-REPP1)                                        
         DC    AL2(REPP8+COL1DISP-REPP1)                                        
                                                                                
         DC    AL2(REPP1+COL2DISP-REPP1)                                        
         DC    AL2(REPP2+COL2DISP-REPP1)                                        
         DC    AL2(REPP3+COL2DISP-REPP1)                                        
         DC    AL2(REPP4+COL2DISP-REPP1)                                        
         DC    AL2(REPP5+COL2DISP-REPP1)                                        
         DC    AL2(REPP6+COL2DISP-REPP1)                                        
         DC    AL2(REPP7+COL2DISP-REPP1)                                        
         DC    AL2(REPP8+COL2DISP-REPP1)                                        
                                                                                
         DC    AL2(REPP1+COL3DISP-REPP1)                                        
         DC    AL2(REPP2+COL3DISP-REPP1)                                        
         DC    AL2(REPP3+COL3DISP-REPP1)                                        
         DC    AL2(REPP4+COL3DISP-REPP1)                                        
         DC    AL2(REPP5+COL3DISP-REPP1)                                        
         DC    AL2(REPP6+COL3DISP-REPP1)                                        
         DC    AL2(REPP7+COL3DISP-REPP1)                                        
         DC    AL2(REPP8+COL3DISP-REPP1)                                        
                                                                                
         DC    AL2(REPP1+COL4DISP-REPP1)                                        
         DC    AL2(REPP2+COL4DISP-REPP1)                                        
         DC    AL2(REPP3+COL4DISP-REPP1)                                        
         DC    AL2(REPP4+COL4DISP-REPP1)                                        
         DC    AL2(REPP5+COL4DISP-REPP1)                                        
         DC    AL2(REPP6+COL4DISP-REPP1)                                        
         DC    AL2(REPP7+COL4DISP-REPP1)                                        
         DC    AL2(REPP8+COL4DISP-REPP1)                                        
                                                                                
         DC    AL2(REPP1+COL5DISP-REPP1)                                        
         DC    AL2(REPP2+COL5DISP-REPP1)                                        
         DC    AL2(REPP3+COL5DISP-REPP1)                                        
         DC    AL2(REPP4+COL5DISP-REPP1)                                        
         DC    AL2(REPP5+COL5DISP-REPP1)                                        
         DC    AL2(REPP6+COL5DISP-REPP1)                                        
         DC    AL2(REPP7+COL5DISP-REPP1)                                        
         DC    AL2(REPP8+COL5DISP-REPP1)                                        
                                                                                
         DC    AL2(REPP1+COL6DISP-REPP1)                                        
         DC    AL2(REPP2+COL6DISP-REPP1)                                        
         DC    AL2(REPP3+COL6DISP-REPP1)                                        
         DC    AL2(REPP4+COL6DISP-REPP1)                                        
         DC    AL2(REPP5+COL6DISP-REPP1)                                        
         DC    AL2(REPP6+COL6DISP-REPP1)                                        
         DC    AL2(REPP7+COL6DISP-REPP1)                                        
         DC    AL2(REPP8+COL6DISP-REPP1)                                        
                                                                                
         DC    AL2(REPPA+COL1DISP-REPP1)                                        
         DC    AL2(REPPB+COL1DISP-REPP1)                                        
         DC    AL2(REPPC+COL1DISP-REPP1)                                        
         DC    AL2(REPPD+COL1DISP-REPP1)                                        
         DC    AL2(REPPE+COL1DISP-REPP1)                                        
         DC    AL2(REPPF+COL1DISP-REPP1)                                        
         DC    AL2(REPPG+COL1DISP-REPP1)                                        
         DC    AL2(REPPH+COL1DISP-REPP1)                                        
                                                                                
         DC    AL2(REPPA+COL2DISP-REPP1)                                        
         DC    AL2(REPPB+COL2DISP-REPP1)                                        
         DC    AL2(REPPC+COL2DISP-REPP1)                                        
         DC    AL2(REPPD+COL2DISP-REPP1)                                        
         DC    AL2(REPPE+COL2DISP-REPP1)                                        
         DC    AL2(REPPF+COL2DISP-REPP1)                                        
         DC    AL2(REPPG+COL2DISP-REPP1)                                        
         DC    AL2(REPPH+COL2DISP-REPP1)                                        
                                                                                
         DC    AL2(REPPA+COL3DISP-REPP1)                                        
         DC    AL2(REPPB+COL3DISP-REPP1)                                        
         DC    AL2(REPPC+COL3DISP-REPP1)                                        
         DC    AL2(REPPD+COL3DISP-REPP1)                                        
         DC    AL2(REPPE+COL3DISP-REPP1)                                        
         DC    AL2(REPPF+COL3DISP-REPP1)                                        
         DC    AL2(REPPG+COL3DISP-REPP1)                                        
         DC    AL2(REPPH+COL3DISP-REPP1)                                        
                                                                                
         DC    AL2(REPPA+COL4DISP-REPP1)                                        
         DC    AL2(REPPB+COL4DISP-REPP1)                                        
         DC    AL2(REPPC+COL4DISP-REPP1)                                        
         DC    AL2(REPPD+COL4DISP-REPP1)                                        
         DC    AL2(REPPE+COL4DISP-REPP1)                                        
         DC    AL2(REPPF+COL4DISP-REPP1)                                        
         DC    AL2(REPPG+COL4DISP-REPP1)                                        
         DC    AL2(REPPH+COL4DISP-REPP1)                                        
                                                                                
         DC    AL2(REPPA+COL5DISP-REPP1)                                        
         DC    AL2(REPPB+COL5DISP-REPP1)                                        
         DC    AL2(REPPC+COL5DISP-REPP1)                                        
         DC    AL2(REPPD+COL5DISP-REPP1)                                        
         DC    AL2(REPPE+COL5DISP-REPP1)                                        
         DC    AL2(REPPF+COL5DISP-REPP1)                                        
         DC    AL2(REPPG+COL5DISP-REPP1)                                        
         DC    AL2(REPPH+COL5DISP-REPP1)                                        
                                                                                
         DC    AL2(REPPA+COL6DISP-REPP1)                                        
         DC    AL2(REPPB+COL6DISP-REPP1)                                        
         DC    AL2(REPPC+COL6DISP-REPP1)                                        
         DC    AL2(REPPD+COL6DISP-REPP1)                                        
         DC    AL2(REPPE+COL6DISP-REPP1)                                        
         DC    AL2(REPPF+COL6DISP-REPP1)                                        
         DC    AL2(REPPG+COL6DISP-REPP1)                                        
         DC    AL2(REPPH+COL6DISP-REPP1)                                        
                                                                                
CALTABX  DC    AL2(EOT)            END OF TABLE                                 
         EJECT                                                                  
GRWORKD  DSECT                     ** GRPREP LOCAL W/S **                       
                                                                                
GRSAVAL  DS    0A                  ** PRTREP S/R SAVED VALUES **                
GRSAVREG DS    A                   SAVE REGISTER AREA                           
GRSAVPTR DS    A                   SAVE PRINT LINE POINTER                      
GRSAVDIS DS    A                   SAVE DISPLAY ROUTINE ADDRESS                 
GRSAVGRP DS    A                   A(NEXT SYSTEM/GROUP ELEMENT)                 
GRSAVALL EQU   *-GRSAVAL                                                        
                                                                                
GRLAGY   DS    CL(L'GRPLAGY)       LAST TIME AGENCY ALPHA ID                    
GRLUSER  DS    XL(L'GRPLUSER)      LAST TIME USER-ID NUMBER                     
GRLSYST  DS    CL(L'GRPLSYST)      LAST TIME SYSTEM LETTER                      
                                                                                
GRCALY   DS    C                   YES DAY                                      
GRCALN   DS    C                   NO DAY                                       
                                                                                
GRFLAG   DS    XL1                 FLAG BYTE                                    
GRFDOHED EQU   X'80'               DO FORMAT HEADLINES                          
GRFVALID EQU   X'40'               VALGRP CALLED                                
GRFFIRST EQU   X'20'               USED BY DISREQ ROUTINE                       
GRFBOXLP EQU   X'10'               BOX LINE PENDING                             
GRFMULTI EQU   X'08'               MULTI-LINE COLUMN SELECTED                   
                                                                                
GRCURRY  DS    XL2                 CURRENT YEAR                                 
GRCURRM  DS    XL1                 CURENT MONTH                                 
GRLASTM  DS    XL1                 LAST MONTH                                   
                                                                                
GRLDAY   DS    CL7                 DAYS OF WEEK ('MTWTFSS')                     
                                                                                
GRSYSNUM DS    CL(L'SYSLEQU)       SYSTEM NUMBER                                
                                                                                
GRLSTCUR DS    XL(LSTTABL)         LIST BUILT HERE                              
                                                                                
GRTOTWID DS    XL1                 TOTAL W'REPORT SO FAR                        
                                                                                
GRNXTN   DS    XL1                 N'ENTRIES IN GRNXTC                          
GRNXTC   DS    XL32                                                             
                                                                                
GRUSEC   DS    XL32                USED COLUMNS                                 
                                                                                
GRWORK   DS    XL64                GENERAL WORK AREA                            
                                                                                
GRNOTCT  DS    XL1                 N'NON-WORKING DAYS IN WEEK                   
                                                                                
GRPSDJ   DS    0XL4                PERIOD START DATE (JULIAN)                   
GRPSDJY  DS    XL2                                                              
GRPSDJD  DS    PL2                                                              
                                                                                
GRPEDJ   DS    0XL4                PERIOD END DATE (JULIAN)                     
GRPEDJY  DS    XL2                                                              
GRPEDJD  DS    PL2                                                              
                                                                                
GRWSDJ   DS    0XL4                CURRENT WEEK START DATE (JULIAN)             
GRWSDJY  DS    XL2                                                              
GRWSDJD  DS    PL2                                                              
                                                                                
GRWEDJ   DS    0XL4                CURRENT WEEK END DATE (JULIAN)               
GRWEDJY  DS    XL2                                                              
GRWEDJD  DS    PL2                                                              
                                                                                
GRCDDJ   DS    0XL4                CURRENT DAY (JULIAN)                         
GRCDDJY  DS    XL2                                                              
GRCDDJD  DS    PL2                                                              
                                                                                
GRPSDB   DS    0XL3                PERIOD START DATE (BINARY)                   
GRPSDBY  DS    XL1                                                              
GRPSDBM  DS    XL1                                                              
GRPSDBD  DS    XL1                                                              
                                                                                
GRPEDB   DS    0XL3                PERIOD END DATE (BINARY)                     
GRPEDBY  DS    XL1                                                              
GRPEDBM  DS    XL1                                                              
GRPEDBD  DS    XL1                                                              
                                                                                
GRWSDB   DS    0XL3                CURRENT WEEK START DATE (BINARY)             
GRWSDBY  DS    XL1                                                              
GRWSDBM  DS    XL1                                                              
GRWSDBD  DS    XL1                                                              
                                                                                
GRWEDB   DS    0XL3                CURRENT WEEK END DATE (BINARY)               
GRWEDBY  DS    XL1                                                              
GRWEDBM  DS    XL1                                                              
GRWEDBD  DS    XL1                                                              
                                                                                
GRCDDB   DS    0XL3                CURRENT DAY (BINARY)                         
GRCDDBY  DS    XL1                                                              
GRCDDBM  DS    XL1                                                              
GRCDDBD  DS    XL1                                                              
                                                                                
GRSVALS  DS    0X                  ** DISREQ S/R SAVED VALUES **                
GRSFRQID DS    XL(L'RFPFRQID)      SAVED FREQUENCY ID                           
GRSFSORT DS    CL(L'RFPFSORT)      SAVED SORT SEQUENCE                          
GRSFSEQN DS    XL(L'RFPFSEQN)      SAVED SEQUENCE NUMBER                        
GRSVALSL EQU   *-GRSVALS                                                        
                                                                                
GRSYMV   DS    0X                  ** DISSYM S/R VALUES **                      
GRSYMNUM DS    XL1                 N'ENTRIES IN GRSYMLST                        
GRSYMLST DS    0XL4                SYMBOL TABLE                                 
GRSYMSYS DS    XL(L'SYSLNUM)       SYSTEM NUMBER                                
GRSYMDIC DS    XL3                 DICTIONARY REFERENCE NUMBER                  
GRSYML   EQU   *-GRSYMLST                                                       
GRSYMN   EQU   REPPNUM             NUMBER OF TABLE ENTRIES                      
         ORG   GRSYMLST                                                         
         DS    (GRSYML)XL(GRSYMN)                                               
GRSYMVL  EQU   *-GRSYMV                                                         
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* GERLPWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GERLPWORK                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                     ** TWAD DEFINITIONS **                       
                                                                                
         ORG   RLPOLY1H                                                         
       ++INCLUDE GERLPF7D                                                       
                                                                                
         ORG   OSVALS                                                           
FILTERS  DS    0X                                                               
                                                                                
SYSFLTS  DS    0X                  ** SYSTEM FILTERS **                         
SYSFLTY  DS    XL1                 FILTER TYPE                                  
SYSFSYST DS    (SYSFMAXN)XL(L'GRPKSYS)                                          
SYSFMAXN EQU   8                                                                
SYSFLTSL EQU   *-SYSFLTS                                                        
                                                                                
AGYFLTS  DS    0X                  ** AGENCY ALPHA FILTERS **                   
AGYFSTR  DS    CL(L'GRPKAGY)       AGENCY ALPHA START                           
AGYFEND  DS    CL(L'GRPKAGY)       AGENCY ALPHA END                             
AGYFLTSL EQU   *-AGYFLTS                                                        
                                                                                
UIDFLTS  DS    0X                  ** USER-ID FILTERS **                        
UIDFMAXN EQU   60                  MAXIMUM N'USER IDS IN A LIST                 
UIDFLTY  DS    XL1                 FILTER TYPE                                  
UIDFSTR  DS    XL(L'GRPKUSER)      USER-ID START                                
UIDFEND  DS    XL(L'GRPKUSER)      USER ID END                                  
UIDRNGEL EQU   *-UIDFLTS                                                        
         ORG   UIDFLTS                                                          
         DS    (UIDFMAXN)XL(L'GRPKUSER)                                         
UIDFLTSL EQU   *-UIDFLTS                                                        
                                                                                
GRPFLTS  DS    0C                  ** GROUP CODE FILTERS **                     
GRPFSTR  DS    CL(L'GRPKGRP)       GROUP CODE START VALUE                       
GRPFEND  DS    CL(L'GRPKGRP)       GROUP CODE END VALUE                         
GRPFLTSL EQU   *-GRPFLTS                                                        
                                                                                
DESCFLT  DS    0X                  ** FILTER NAME CODE FILTER **                
DESCFLEN DS    XL1                 L'INPUT FILTER                               
DESCFVAL DS    CL(L'RFPVDESC)      FILTER VALUE                                 
DESCFLTL EQU   *-DESCFLT                                                        
                                                                                
FREQFLT  DS    0X                  ** FREQUENCY FILTER **                       
FREQFCC  DS    XL1                 CONDITION CODE                               
FREQFVAL DS    CL(L'GRPHFREQ)      FREQUENCY CODE                               
FREQFLTL EQU   *-FREQFLT                                                        
                                                                                
OTYPFLT  DS    0X                  ** OUTPUT TYPE FILTER **                     
OTYPFLEN DS    XL1                 L'INPUT FILTER                               
OTYPFVAL DS    CL(L'GRPHOTYP)      FILTER VALUE                                 
OTYPFLTL EQU   *-OTYPFLT                                                        
                                                                                
DSTCFLT  DS    0X                  ** DESTINATION CODE FILTER **                
DSTCFLEN DS    XL1                 L'INPUT FILTER                               
DSTCFVAL DS    CL(L'GIDCODE)       FILTER VALUE                                 
DSTCFLTL EQU   *-DSTCFLT                                                        
                                                                                
NXTRFLT  DS    0X                  ** NEXT RUN DATE FILTER **                   
NXTRSTDT DS    XL(L'GRPHNXTR)      NEXT RUN DATE START                          
NXTRENDT DS    XL(L'GRPHNXTR)      NEXT RUN DATE END                            
NXTRFLTL EQU   *-NXTRFLT                                                        
                                                                                
LSTRFLT  DS    0X                  ** LAST RUN DATE FILTER **                   
LSTRSTDT DS    XL(L'GRPHLSTR)      LAST RUN DATE START                          
LSTRENDT DS    XL(L'GRPHLSTR)      LAST RUN DATE END                            
LSTRFLTL EQU   *-LSTRFLT                                                        
                                                                                
ENDFLT   DS    0X                  ** END RUN DATE FILTER **                    
ENDSTDT  DS    XL(L'GRPHEND)       END RUN DATE START                           
ENDENDT  DS    XL(L'GRPHEND)       END RUN DATE END                             
ENDFLTL  EQU   *-ENDFLT                                                         
                                                                                
EFFFLT   DS    0X                  ** EFFECTIVE DATES FILTER **                 
EFFSTDT  DS    XL(L'GRPHEND)       EFFECTIVE START                              
EFFENDT  DS    XL(L'GRPHEND)       EFFECTIVE END                                
EFFFLTL  EQU   *-EFFFLT                                                         
                                                                                
NAMEFLT  DS    0X                  ** FILTER NAME CODE FILTER **                
NAMEFLEN DS    XL1                 L'INPUT FILTER                               
NAMEFVAL DS    CL(L'RFPVNAME)      FILTER VALUE                                 
NAMEFLTL EQU   *-NAMEFLT                                                        
                                                                                
FLTNOT   EQU   0                   NO FILTER                                    
FLTYES   EQU   1                   INCLUDE ONLY                                 
FLTNO    EQU   2                   EXCLUDE ONLY                                 
FLTVAL   EQU   3                   FILTER ON VALUE                              
                                                                                
XFGFLTS  DS    0X                  ** XFILE GROUP FILTERS **                    
XFGFTYP  DS    XL1                 FILTER TYPE (SEE ABOVE)                      
XFGFTGRP DS    CL(L'RFPXFILE)      XFILE GROUP CODE                             
XFGFLTSL EQU   *-XFGFLTS                                                        
                                                                                
REQFLT   DS    XL1                 ** REQUEST FILTER **                         
RUNFLT   DS    XL1                 ** RUN FILTER **                             
SYMFLT   DS    XL1                 ** SYMBOL FILTER **                          
CALFLT   DS    XL1                 ** CALENDAR FILTER **                        
                                                                                
REQPROG  DS    CL2                 REQUEST PROGRAM FILTER                       
                                                                                
FILTERSL EQU   *-FILTERS                                                        
                                                                                
REPD     DSECT                                                                  
         ORG   REPX                                                             
BOXAREA  DS    XL(BOXLASTL-BOXD)                                                
PQBUFF   DS    XL(14*ONEK)                                                      
REPL     EQU   *-REPD                                                           
         EJECT                                                                  
COLTABD  DSECT                     ** COLUMN DEFINITION TABLE **                
COLTNUM  DS    XL1                 COLUMN NUMBER                                
COLTEOTQ EQU   0                   END OF TABLE INDICATOR                       
COLTREC  DS    XL1                 RECORD TYPE FILTER                           
COLTTDSP DS    AL2                 DISPLACEMENT TO SCREEN LINE                  
COLTINDS DS    XL1                 INDICATOR BYTE                               
COLTIMLT EQU   X'80'               MULTI-LINE COLUMN                            
COLTVDSP DS    AL2                 DISPLACEMENT TO VALIDATION ROUTINE           
COLTFDSP DS    AL2                 DISPLACEMENT TO FORMAT ROUTINE               
COLTHWID DS    XL1                 WIDTH OF HEADLINE                            
COLTDWID DS    XL1                 WIDTH OF DATA                                
COLTHDSP DS    0AL2                DISPLACEMENT TO HEADLINE BUILDER             
COLTHED1 DS    AL2                 DISPLACEMENT TO HEADLINE 1                   
COLTHED2 DS    AL2                 DISPLACEMENT TO HEADLINE 2                   
COLTABL  EQU   *-COLTABD                                                        
                                                                                
                                                                                
LSTD     DSECT                     ** SCREEN LINE **                            
LSTNAMH  DS    XL(L'REPGRPLH)                                                   
LSTNAM   DS    CL(L'REPGRPL)       FIELD NAME                                   
LSTINPH  DS    XL(L'REPGRPCH)                                                   
LSTINP   DS    CL(L'REPGRPC)       COLUMN NUMBER                                
LSTINPX  DS    XL(L'REPGRPCX)                                                   
LSTFLTH  DS    XL(L'REPGRPFH)                                                   
LSTFLT   DS    CL(L'REPGRPF)       FILTER VALUE                                 
LSTFLTX  DS    XL(L'REPGRPFX)                                                   
LSTL     EQU   *-LSTD                                                           
                                                                                
                                                                                
CALTABD  DSECT                     ** DSECT TO COVER CALTAB **                  
                                                                                
COLWIDQ  EQU   13                  COLUMN WIDTH                                 
COL1DISP EQU   00                  COLUMN 1 HEADLINE DISPLACEMENT               
COL2DISP EQU   COL1DISP+COLWIDQ    COLUMN 2 HEADLINE DISPLACEMENT               
COL3DISP EQU   COL2DISP+COLWIDQ    COLUMN 3 HEADLINE DISPLACEMENT               
COL4DISP EQU   COL3DISP+COLWIDQ    COLUMN 4 HEADLINE DISPLACEMENT               
COL5DISP EQU   COL4DISP+COLWIDQ    COLUMN 5 HEADLINE DISPLACEMENT               
COL6DISP EQU   COL5DISP+COLWIDQ    COLUMN 6 HEADLINE DISPLACEMENT               
                                                                                
CALTHEDL DS    AL2                 DISPLACEMENT TO HEADLINE                     
CALTSUBL DS    AL2                 DISPLACEMENT TO SUB-HEADLINE                 
CALTWK1H DS    AL2                 DISPLACEMENT TO WEEK 1 HEADER                
CALTWK2H DS    AL2                 DISPLACEMENT TO WEEK 2 HEADER                
CALTWK3H DS    AL2                 DISPLACEMENT TO WEEK 3 HEADER                
CALTWK4H DS    AL2                 DISPLACEMENT TO WEEK 4 HEADER                
CALTWK5H DS    AL2                 DISPLACEMENT TO WEEK 5 HEADER                
CALTWK6H DS    AL2                 DISPLACEMENT TO WEEK 6 HEADER                
CALTABL  EQU   *-CALTABD                                                        
                                                                                
                                                                                
WKDD     DSECT                     ** WEEK DISPLAY **                           
WKDSDAY  DS    CL3                 START DAY                                    
         DS    CL1                                                              
WKDDAYS  DS    CL7                 DAYS MASK                                    
WKDL     EQU   *-WKDD                                                           
                                                                                
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* GEDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
                                                                                
                                                                                
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009GERLP05   11/04/11'                                      
         END                                                                    
