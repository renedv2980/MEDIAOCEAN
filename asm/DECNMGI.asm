*          DATA SET DECNMGI    AT LEVEL 111 AS OF 06/16/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECNMGIA                                                                 
***********************************************************************         
         TITLE '- DEMO CONVERSION - CABLE MOVIE GOER MIT'                       
*                                                                               
* IPHASE:DECNMGI                                                                
* OPHASE:DECNMGO                                                                
* --------------                                                                
*                                                                               
* INPUT-OUTPUT LOGIC:                                                           
* ------------------                                                            
* '00' - EXTENDED MIT DESCRIPTIVE RECORD                                        
* '01' - UNIVERSES.  ONE FOR EACH CABLE NET            H-P-P                    
*        NO TOTAL US UNIVERSES                                                  
* '03' - HUTS. WE DON'T PROCESS THEM                                            
* '04' - PROGRAM. PRG DEMOS                            H-P-P                    
*                ->  QNN PRG RECDS DEMOS                                        
* '05' - USAGE RECDS FOR EACH CABLE NET.               H-P-P                    
*                ->  PNN NET-C   TIME PERIOD USAGE VIEWG TO CABLE NET           
*        ---------------------------------                                      
* REGISTERS:                                                                    
* ---------                                                                     
*        R2  - AIREC (INTERD)                                                   
*        R8  - DEMCOND GLOBAL WORKING STORAGE                                   
*        R3  - BASE REG                                                         
*        R4  - BASE REG                                                         
*        RA  - BASE REG                                                         
*        RB  - BASE REG                                                         
*        RC  - ARREC (MIRECD) INPUT TAPE RECORD                                 
*                                                                               
*        R0  - WORK                                                             
*        R1  - WORK                                                             
*        R5  - WORK                                                             
*        R6  - WORK                                                             
*        R7  - WORK                                                             
*        RE  - WORK                                                             
*        RF  - WORK                                                             
*                                                                               
*NOTE:   DEDEMTIME CONTAINS THE VHRTOQH ROUTINES                                
**********************************************************************          
         EJECT                                                                  
DECNMGI  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DECNMGI,R3,R4,RA                                               
         USING DPRINT,R7                                                        
         ST    R7,SVR7                                                          
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC = INPUT RECD FROM TAPE (MITREC)           
         USING MITMGD,RC                                                        
         L     R2,AIREC            R2 = INTERIM RECD  (INTERD)                  
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     ENDJOB              E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* READ - GET INPUT TAPE RECORDS ONE AT A TIME AND PROCESS. BUILD                
*        INTERIM RECDS.                                                         
* *********************************************************************         
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
         LH    RE,=AL2(DEMLNQ)     LENGTH OF DEMAREA                            
         A     RE,=A(DEMAREA)      GET A(PACKWT)                                
         ST    RE,APACKWT                                                       
         LA    RE,COMWRK           SAVE ADDR OF COMMON WRK AREA BETWN           
         ST    RE,ACOMWRK            INPUT AND OUTPUT PHASES                    
         MVI   NOTAVAL,0           DATA NOT AVAIL SWITCH                        
         MVI   MYMODE,X'FF'        SET TO 1ST-TIME-THRU (GET RDR 1ST)           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
READ20   DS    0H                  ARE WE IN THE MID OF RELSG RECS              
         CLI   MYMODE,C'A'         UNIVERSE RECD RELEASE                        
         BE    UNVREL                                                           
         CLI   MYMODE,C'U'         USAGE RECD RELEASE                           
         BE    USGREL                                                           
         CLI   MYMODE,C'H'         HALF HR M-F & M-S RECDS RELS                 
         BE    HPTRELS                                                          
         CLI   MYMODE,C'P'         PROGRAM RECD RELEASE                         
         BE    PRGREL                                                           
         CLI   MYMODE,C'N'         PROG NAME RECD FOR TIME PERDS                
         BE    PRGTIME                                                          
         CLI   MYMODE,C'F'                                                      
         BE    FUDGREL                                                          
*                                                                               
READ25   L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'760'    HEADER                                        
         CLI   MYMODE,1            TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
*                                                                               
RDTEST   GET   IN1,(RC)            GET NEXT RECORD                              
         CLI   MYMODE,X'FF'        1ST-TIME-THRU? (SHOULD BE RDR)               
         BNE   READ40                                                           
         CLC   MITSEQ,=C'00'       TEST FOR REPORT DESCR RECORD (RDR)           
         BE    RDR                                                              
*                                                                               
READ40   DS    0H                  DETERMINE/BRANCH TO RECD TYPE                
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         DROP  R1                                                               
         GOTO1 (RF),DMCB,VWTYPTTB                                               
         ICM   R7,15,DMCB                                                       
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING VWTYPTD,R7                                                       
READ41A  CLI   0(R7),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+10                                                             
         AR    R7,R0               BUMP TO NEXT ENTRY                           
         B     READ41A                                                          
         MVC   KEYSRC,VWTKSRC      SAVE KEY SOURCE HERE                         
         DROP  R7                                                               
*                                                                               
*        CLC   MITCOVG,=C'007237'  NEW-STYLE TBS                                
*        BNE   *+10                                                             
*        MVC   MITCOVG,=C'005810'  CHANGE TO A DIFF NO TO DISTINGUISH           
*                                  BTW NEW TBS AND OLD TBS WE USED TO           
*                                  RECEIVE ON SEPARATE FILE                     
*                                                                               
         CLI   MITORIG,C'0'        BYPASS REPROCESSING FOR NOW                  
         BE    READ42                                                           
*        CLC   TAPEWK,MITCORDT+1   INBOOK CORRECTION?                           
*        BNE   RDTEST              NO, BYPASS FOR NOW                           
*                                                                               
READ42   CLC   MITTYPE,SVNET       PRINT OUT FIRST NET CODE/NET #               
         BNE   *+14                                                             
         CLC   SVCVG,MITCOVG                                                    
         BE    READ45                                                           
         CLC   MITSEQ,=C'02'       DON'T PRINT FOR IGNORED RECDS                
         BE    READ45                                                           
*                                                                               
READ45   MVC   SVNET,MITTYPE                                                    
         MVC   SVSEQ,MITSEQ                                                     
         MVC   SVCVG,MITCOVG                                                    
         CLC   SAVEPNUM,MITPRG     WHEN PRG# CHGS RESET- CMP ENTIRE#            
         BE    UNIVERS             SAME                                         
         MVC   SAVEPNUM,MITPRG     SAVE ENTIRE NUMBER                           
         MVI   VARIOUS,0                                                        
         MVC   VARS,ZEROS                                                       
         MVC   DAYS,ZEROS                                                       
         XC    SAVVAR(49),SAVVAR                                                
*                                                                               
UNIVERS  CLC   MITSEQ,=C'01'       UNIVERSE RECORD?                             
         BE    UNV                                                              
         CLC   MITSEQ,=C'02'       SAMPLE COUNTS                                
         BE    READ20              YES, IGNORE                                  
         CLC   MITSEQ,=C'03'                                                    
         BE    READ20              IGNORE HUTS FOR NOW                          
*                                                                               
USG_TVU  DS    0H                  CABLE TVU TO GEOGRAPHIC MARKET               
         CLI   RELS,1              RELEASE LAST UNIVERSE RECD?                  
         BE    UNVREL                                                           
*        CLC   MITSEQ,=C'03'       USAGE RECORDS?                               
*        BNE   PROGRAM             DO NOT SUPPORT                               
*        MVI   RECD,C'3'           THIS IS A USAGE RECD                         
*        CLC   MITTYPE(3),=C'TVU'  VERIFY RECD TYPE=TVU  USAGE RECD             
*        BE    TVUREC              HOUSEHOLD USAGE RECD                         
*        DC    H'0'                                                             
*                                                                               
PROGRAM  DS    0H                                                               
         CLC   MITSEQ,=C'04'       PROGRAM RECORDS?                             
         BNE   USG_BAS             NO                                           
         MVI   RECD,C'4'           THIS IS A PROGRAM RECD                       
*        CLI   RELS,3              RELEASE LAST USAGE RECD?                     
*        BE    USGREL                                                           
         B     PRGREC              PROCESS PRG RECD                             
*                                                                               
USG_BAS  CLC   MITSEQ,=C'05'                                                    
         BNE   PERMUSG                                                          
         MVI   RECD,C'5'           THIS IS A USG-BAS RECD                       
         CLI   RELS,4              RELEASE LAST PRG RECD?                       
         BE    PRGREL                                                           
         CLI   FRELS,0             RELEASE LAST FUDGED RECORD?                  
         BE    USG10                                                            
         BAS   RE,FUDGMER          MERGE LAST ENTRY INTO HHRTAB                 
         MVI   SVMODE,1            PROCESS THIS RECORD AFTER RELEASE            
         MVC   INDEX,=A(HHRTAB)                                                 
         B     FUDGREL             RELEASE FUDGED TIME PERD RECORDS             
USG10    CLC   MITTYPE(3),=C'BAS'  CABLE USAGE?                                 
         BE    TVUREC              MAYBE STATION GROUP                          
         B     READ20              BYPASS IF 'H' MISSING                        
*                                                                               
PERMUSG  CLC   MITSEQ,=C'99'       PERMISSABLE USAGE RECD?                      
         BE    READ20              BYPASS                                       
         DC    H'0'                UNKNOWN RECD TYPE                            
*                                                                               
READ90   MVI   MYMODE,0            DROP RECORD                                  
         MVI   INTAPESW,X'40'                                                   
         B     EXIT                                                             
*                                                                               
RELEASE  DS    0H                  RTN TO CNTLR- WITH A RECD TO RELEASE         
EXIT     XMOD1 1                                                                
XIT      XIT1                      END OF PROCEDURE- RETURN TO CALLER           
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* RDR -  PROCESS REPORT DESCRIPTOR RECORD                                       
* *********************************************************************         
*                                                                               
RDR      DS    0H                                                               
         CLC   =C'NHI MOVIE GOER REPORT',MI0FILE                                
         BE    *+6                                                              
         DC    H'0'                UNKNOWN TAPE TYPE                            
         MVC   TAPEWK,MITSTART     START DATE EXCLUDING CENTURY                 
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   BOOK(1),4(R1)       YEAR                                         
         MVC   BOOK+1(1),8(R1)     HUT BOOK                                     
         MVI   MYMODE,0            SET TO READ NEXT RECORD                      
         MVC   TAPEMKT,=H'320'     320 IS MARKET FOR THIS FILE                  
*                                                                               
         MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
*                                                                               
* EVERY YEAR, WE NEED TO UPDATE THE MOVIEGOU TABLE OF UNIVERSES                 
* IN DEDEMTABS BEFORE WE RUN THIS CONVERSION. THE FOLLOWING CODE                
* CHECKS TO MAKE SURE THAT THE TABLE HAS BEEN UPDATED FOR THE WEEK              
* BEING CONVERTED.                                                              
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         DROP  R1                                                               
         GOTO1 (RF),DMCB,MOVIEGOU                                               
         ICM   RF,15,DMCB                                                       
         JNZ   *+6                                                              
         DC    H'0'                BAD TABLE ID PASSED                          
*                                                                               
         USING MGUD,RF                                                          
         MVC   HALF,MGUYR          FIRST ENTRY IS MOST RECENT YR/WK             
         XC    HALF,=X'FFFF'       ADJUST TO REAL YEAR/WEEK                     
         DROP  RF                                                               
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         ICM   RF,15,CDEMTABS                                                   
         DROP  R1                                                               
         GOTO1 (RF),DMCB,NENACWKS                                               
         ICM   RF,15,DMCB                                                       
         JNZ   *+6                                                              
         DC    H'0'                BAD TABLE ID PASSED                          
         L     R0,DMCB+4           L'TABLE ENTRY                                
*                                                                               
         USING NEWKSD,RF                                                        
RDR10    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BOOK NOT FOUND                               
         CLC   HALF(1),NEWKSYR     COMPARE ON YEAR                              
         BNE   *+14                                                             
         CLC   HALF+1(1),NEWKSLST  COMPARE THE WEEK#                            
         BNH   *+10                GOT IT                                       
         AR    RF,R0               TRY NEXT ENTRY IN THE TABLE                  
         B     RDR10                                                            
*                                                                               
         CLI   NEWKSYR+1,MON_SEP   ENTRY DATE IS ALWAYS SEPTEMBER               
         BE    *+6                                                              
         DC    H'0'                                                             
         AR    RF,R0               BUMP FORWARD TO AUGUST                       
         CLI   NEWKSYR+1,MON_AUG                                                
         BNE   *-6                                                              
*                                                                               
         CLC   BOOK(1),NEWKSYR     COMPARE ON YEAR                              
         BL    RDRX                PRIOR YEAR: WE'RE GOOD                       
         BH    *+14                WE'RE *VERY* LATE UPDATING THE TABLE         
         CLC   BOOK+1(1),NEWKSLST  COMPARE THE LAST WEEK# IN AUGUST             
         BNH   RDRX                WE'RE STILL WITHIN THIS YEAR                 
         DROP  RF                                                               
*                                                                               
         IF (TM,FLAGS1,INHIBIT_WARNING_EMAILS,Z)  ** FOR TESTING ONLY!!         
           GOTO1 VDATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*US-MFDEMOSPROGRAMM+        
               ERS,USOPSUP:**UPDATE THE MOVIEGOU TABLE IN DEDEMTABS**'          
         ENDIF ,                                                                
         DC    H'0'                SEE INSTRUCTIONS IN DEDEMTABS...             
*                                  ...FOR TBL_MOVIEGOU                          
*                                                                               
RDRX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* UNV -  SAVE AWAY UNIVERSE HUTS AND PUTS IN UNVBUFF.                           
* *********************************************************************         
UNV      DS    0H                                                               
         CLC   MITTYPE(3),=C'UES'                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MITREC,C'H'         UNIVERSE RECORDS                             
         BE    *+6                                                              
         DC    H'0'                UNKNOWN RECD TYPE                            
*                                                                               
         CLI   RELS,1              RELEASE PREV UNIV RECD?                      
         BE    UNVREL                                                           
         BAS   RE,HDR              PROCESS HDR INFOR FIRST                      
         ZIC   R1,COVSMPD          CVG DISP IN TABLE                            
         MH    R1,CVGLN            DISP OF CVG INTO BUFFER                      
         A     R1,=A(UNVBUFF)      SAVE IN UNVBUFF                              
         ST    R1,DMCB             DMCB=A(CVG SAVE AREA IN UNVBUFF)             
         LA    R1,MIXDEM           1ST DEMO IN LIST                             
         ST    R1,DMCB+4           A(1ST DEMO ON INPUT)                         
         BAS   RE,SLOTDEM          PROCESS DEMOS-SLOT IN BUFFER                 
*                                                                               
*        MVI   RELS,0              CLEAR FLAG                                   
*        CLC   COVALPH,=C'0000'    TOTAL SAMPLE?                                
*        BE    *+8                 DON'T OUTPUT A UNV RECD                      
         MVI   RELS,1              SET FLAG TO RELEASE UNV RECD                 
         LA    RE,INTKEY                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   INTRTYP,C'P'        TIME PERIOD P-RECD                           
         MVC   INTBOOK,BOOK                                                     
         MVC   INTIBOOK,IBOOK                                                   
         MVC   INTSTA(4),COVALPH                                                
         MVI   INTSTA+4,C'U'       UNIVERSE RECD                                
         MVC   INTMRKT,TAPEMKT                                                  
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
*                                                                               
UNVX     B     READ20              READ NEXT RECORD                             
         EJECT                                                                  
**********************************************************************          
*UNVREL-RELEASE UNIVERSE RECDS                                                  
**********************************************************************          
UNVREL   DS    0H                                                               
         MVI   MYMODE,C'A'         RELEASE UNV MODE                             
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
*                                                                               
UNVRL20  L     RE,=A(CRCOTAB)                                                   
UNVRL22  CLI   0(RE),X'FF'                                                      
         BNE   UNVRL25             DON'T DO SUMMARIES- JUST END                 
         MVI   RELS,0              NOTHING TO RELEASE                           
         MVI   RELSLOT,0                                                        
         MVI   MYMODE,0                                                         
         CLI   RECD,C'Z'           END OF FILE?                                 
         BE    ENDJOB                                                           
         B     READ40              PROCESS CURRENT RECD                         
*                                                                               
UNVRL25  CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,L'CRCOTAB(RE)                                                 
         B     UNVRL22                                                          
         CLI   3(RE),0                                                          
         BNE   UNVRL30                                                          
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     UNVRL20                                                          
*                                                                               
UNVRL30  MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         XC    INTPNUM,INTPNUM                                                  
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         LA    R1,INTACCS                                                       
         CLC   COVALPH,=C'0000'                                                 
         BE    *+12                R1=DESTN IN INTERIM REC FOR UNIVS            
         AH    R1,=Y(RIUNVSQC)     BY COVERAGE AREA                             
         B     *+8                                                              
         AH    R6,=Y(RIUNVSQ)      FOR TOTAL US. JUST IN CASE                   
*                                                                               
         ZIC   RE,COVSMPD          GET UNIVS FOR COVG SAMPLE                    
         MH    RE,CVGLN            BUMP TO CVG IN UNVBUFF                       
         ZIC   RF,RELSLOT          WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS                                
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         CLC   COVALPH,=C'0000'    TOTAL SAMPLE?                                
         BNE   *+10                                                             
         MVC   INTSTA(4),=C'UUUU'  SET STN TO UNIV                              
*                                                                               
         BAS   RE,BLDKEY           BUILD INTERIM RECD KEY                       
         B     RELEASE             RETURN TO CTLR TO RELEASE RECD               
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* TVUREC -HOUSEHOLD USAGE RECORD                                                
*        ONE RECORD PER 1/2HR FOR EACH DAY                                      
*        FOR EACH COVERAGE SAMPLE                                               
*              RECORD SEQUENCE CODE = 3 OR 5                                    
*              DATA TYPE CODE       = TVU                                       
*              RECORD TYPE          = H - P - P                                 
* *********************************************************************         
TVUREC   DS    0H                                                               
         CLI   MITREC,C'H'                                                      
         BE    *+6                 HOUSHOLD USAGE RECD                          
         DC    H'0'                                                             
         CLI   RELS,0              NOTHING TO RELEASE-JUST PROCESS              
         BE    TVU10                                                            
         CLC   PRVMKT,MITMKTBR     TEST MKT BREAK                               
         BE    TVU5                SAME MKT BREAK-> DIFFERENT RECD              
         CLC   PRVHLF,MITHLFID     TEST HALF HR ID/TOTAL PRG                    
         BNE   TVU5                DIFF HLF HR/TOTAL-> DIFFERENT RECD           
         CLC   PRVKEY(MITMKTBR-MITKEY),MITKEY    TEST OTHER KEY FIELDS          
         BE    TVU10               SAME -SAVE MKT BREAK/DON'T RELS              
*                                                                               
TVU5     CLI   RELS,3              RELEASE PREVIOUS '03' USAGE RECD?            
         BE    USGREL              FINISH BLDING USAGE RECD & RELEASE           
         CLI   RELS,5              RELEASE PREVIOUS '05' USAGE RECD?            
         BE    USGREL              FINISH BLDING USAGE RECD & RELEASE           
*                                                                               
TVU10    MVI   RELS,3                                                           
         CLC   MITSEQ,=C'03'                                                    
         BE    *+8                                                              
         MVI   RELS,5              CABLE VIEWING RECD                           
         MVC   PRVMKT,MITMKTBR     SAVE MKT BREAK                               
         MVC   PRVHLF,MITHLFID     SAVE HLF HR ID                               
**       MVC   PRVKEY(MITMKTBR-MITKEY),MITKEY   SAVE OTHER KEY FIELDS           
         MVC   PRVKEY,MITKEY       SAVE OTHER KEY FIELDS                        
         LA    RE,INTKEY                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVC   INTFEED,MITFEED     FEED PATTERN                                 
         MVC   INTAUDES,MITAUDTY   AUD EST TYPE                                 
         MVC   INTCOVSM,MITCOVG    COVERAGE SAMPLE                              
         MVC   INTCOVCL,MITCOVCL   COV SAMPL CALC IND                           
*                                                                               
         BAS   RE,HDR              PROCESS HDR INFO.MKTBRK GETS ITS             
*                                   VALUE FROM MIT                              
         CLI   MKTBMIT,0                                                        
         BNE   *+8                                                              
         BAS   RE,H_REC            PROCESS 'H' RECD                             
         MVI   INTDTYP,X'09'       DUMMY TO REG-ORIG (FIELD IS N/A)             
         MVC   INTSTA(4),COVALPH   USAGE TO GEOGRAPHIC MKT                      
         MVI   INTSTA+4,X'83'      SMALL-(C) - FOR '03' USAGE DATA              
         CLI   RELS,3                                                           
         BE    TVU20                                                            
         MVI   INTSTA+4,C'C'       'C' = FOR '05' HALF HOUR                     
*                                                                               
TVU20    L     R1,=A(DEMAREA)      SAVE IN DEMO AREA                            
         ST    R1,DMCB             DMCB=A(CVG SAVE AREA IN UNVBUFF)             
         LA    R1,MIXDEM           1ST DEMO IN NON-PUT-LIST  (05 RECD)          
         ST    R1,DMCB+4           A(1ST DEMO ON INPUT TAPE)                    
         BAS   RE,SLOTDEM          PROCESS DEMOS-SLOT IN BUFFER                 
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
         MVI   MYMODE,0            READ NEXT RECORD                             
TVUX     B     READ20              GO READ NEXT RECD                            
         EJECT                                                                  
***********************************************************************         
*USGREL- RELEASE REGULAR USAGE RECORDS. MYMODE='U' RELS=3 OR 5                  
***********************************************************************         
USGREL   DS    0H                                                               
         MVI   MYMODE,C'U'         OUTPUT DEMS FOR THIS USAGE RECD              
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         B     USG20               OUTPUT THE USAGE RECS FIRST                  
*                                                                               
USG20    L     RE,=A(CRCOTAB)                                                   
USG22    CLI   0(RE),X'FF'                                                      
         BNE   USG25               DON'T DO SUMMARIES- JUST END                 
         MVI   RELS,0              NOTHING TO RELEASE                           
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         MVI   MYMODE,0                                                         
         CLI   RECD,C'Z'           END OF FILE?                                 
         BE    USG23               RELEASE AVGS BEFORE EXITING                  
         LA    RF,PRVKEY                                                        
         CLC   MITCOVG,MITCOVG-MITMGD(RF)                                       
         BNE   *+14                PROCESS CURRENT RECD                         
         CLC   MITFEED,HPTFEED     SAME FEED?                                   
         BE    READ40                                                           
         OC    HPTSTN,HPTSTN       NOTHING SAVED HERE YET                       
         BZ    READ40              NOTHING TO RELS YET, BYPASS                  
USG23    MVI   MYMODE,C'H'         RELASE HPT AVGS FOR THIS CVG                 
         MVI   QHR,X'00'                                                        
         B     HPTRELS                                                          
*                                                                               
USG25    CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,L'CRCOTAB(RE)                                                 
         B     USG22                                                            
         CLI   3(RE),0                                                          
         BNE   USG30                                                            
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     USG20                                                            
*                                                                               
USG30    MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH    1/4 HR ID                                 
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,MKTDISP          MKT BRK DISPLACEMENT LENGTH                  
         A     RE,=A(DEMAREA)                                                   
         LA    R1,INTACCS                                                       
         LH    RF,=Y(NDEMS*4)      RF=NUMBER BYTES TO MOVE                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),0(RE)      SAVE DEMOS IN INTACCS                        
*                                                                               
         CLI   RELSLOT,0           FIRST RELEASE?                               
         BNE   *+8                 NO. ALREADY PUT TO BUFFER                    
         BAS   RE,HPTFILL          FILL BUFFER FOR M-F/M-S AVGS                 
*                                                                               
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIUNVSQC)     R1=DESTN IN INTERIM REC FOR UNIVS            
*                                                                               
         ZIC   RE,COVSMPD          GET UNIVS FOR COVG SAMPLE                    
         MH    RE,CVGLN            BUMP TO CVG IN UNVBUFF                       
         ZIC   RF,RELSLOT          WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS (AFTER DEMOS)                  
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         XC    INTRSF,INTRSF       NOT DEFINED FOR TIME PERD RECDS              
         CLC   INTSTA(4),=C'0000'  USA LEVEL IS HUT STATION                     
         BNE   USG60                                                            
         MVC   INTSTA(4),=C'HUT '                                               
*        CLI   INTFEED,C'L'        FOR NOW, USE BTYPE FROM CRCOTAB              
*        BNE   *+8                 MAY NEED TO CHANGE IF KEY NOT UNIQUE         
*        MVI   INTBTYP,C'L'                                                     
*                                                                               
USG60    BAS   RE,BLDKEY           BUILD INTERIM RECD KEY                       
         B     RELEASE             RETURN TO CTLR TO RELEASE RECD               
         EJECT                                                                  
* *********************************************************************         
* BLDKEY -     BUILD PAV MAJOR KEY. INTERIM RECD FIELDS ALREADY SET.            
* *********************************************************************         
*                                                                               
BLDKEY   NTR1                                                                   
         CLI   INTRTYP,PRCODEQU    TEST FOR 'P' RECORD                          
         BNE   BLDK20                                                           
         LA    R7,INTKEY           BUILD -P- PAV KEY                            
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     -P- RECORD                                   
         MVI   PRMEDIA,C'C'        'N'=1 WK AVG 'W'=INDIV DAY                   
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    BLDK15                                                           
         MVC   PRCODE(2),INTBOOK   (BOOK FORCES PROPER SORT)                    
         MVI   PRSRC,PRCODEQU      -P-                                          
*                                                                               
BLDK15   MVC   PRSTAT,INTSTA                                                    
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRBTYP+1(1),INTDAYWK  SORT BY: DAY--QTRHR--MKT BRK               
         MVC   PRBTYP+2(2),INTPNUM   QTR HR                                     
         MVC   PRBTYP+4(1),MKTBRK   MKT BREAK                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVC   PRDW+1(1),INTDUR    FORCE HIGHER DURATIONS FIRST                 
         XI    PRDW+1,X'FF'                                                     
         B     BLDKEYX                                                          
         DROP  R7                                                               
         SPACE 2                                                                
*                                                                               
BLDK20   CLI   INTRTYP,PMCODEQU    TEST FOR 'Q' RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R7,INTKEY           BUILD 'Q' RECORD KEY                         
         USING PMKEY,R7                                                         
         MVI   PMCODE,PMCODEQU     -Q-                                          
         MVI   PMMEDIA,C'C'                                                     
         MVI   PMSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         CLI   CORRSW,0            TEST CORRECTION RECORD                       
         BE    BLDK25                                                           
         MVC   PMCODE(2),BOOK      (BOOK FORCES PROPER SORT)                    
         MVI   PMSRC,PMCODEQU      -Q-                                          
*                                                                               
BLDK25   MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMPNUM,INTPNUM                                                   
         MVC   PMBTYP,INTBTYP                                                   
         MVC   PMBTYP+1(2),INTPNUM                                              
         MVC   PMBTYP+3(1),MKTBRK                                               
*                                                                               
         LA    RE,QSORTAB          SORT 'Q' RECORDS BY DAY...                   
         LA    R0,QSORTABS         M-S, M-F, VAR, MON...SUN                     
         MVC   PMRLEN(1),INTDAYWK                                               
         NI    PMRLEN,X'F0'                                                     
         CLC   PMRLEN(1),0(RE)                                                  
         BE    *+14                                                             
         LA    RE,L'QSORTAB(RE)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   PMRLEN(1),1(RE)     USE FIELD FOLLOWING PMPNUM                   
         DROP  R7                                                               
*                                                                               
BLDKEYX  B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
*HDR   - COMMON PROCESSING FOR HEADER RECORDS                                   
* *********************************************************************         
HDR      NTR1                      COMMON H-RECD PROC BETW RECD TYPES           
         MVC   INTORIG,MITORIG                                                  
         PACK  DUB,MITAVG          #DAYS/WEEKS IN AVG                           
         CVB   R1,DUB                                                           
         STC   R1,INTAVG           HUT FOR MKT BREAK                            
         PACK  DUB,MIHPROJ         CONVERT HUT PROJECTION                       
*                                                                               
HDR4     CLC   MITSEQ,=C'04'       FOR PRG RECD, HUT IS IN DIFF LOCTN           
         BNE   *+10                                                             
         PACK  DUB,MIDPROJ         HUT PROJECTION FOR PRG RECD ONLY             
HDR5     CVB   R1,DUB                                                           
         ST    R1,MKTHUT           HUT FOR MKT BREAK                            
         CLC   MITMKTBR,=C'000'    IF TOTAL SAMPLE SAVE AS USHUT ALSO           
         BNE   *+8                                                              
         ST    R1,USHUT            USA HOMES                                    
*                                                                               
         PACK  DUB,MITMKTBR        CONVERT MKT BREAK                            
         CVB   R1,DUB                                                           
         STH   R1,MKTBMIT          HUT FOR MKT BREAK                            
*                                                                               
         MVC   COVSMP,MITCOVG+2                                                 
         CLI   MITCOVG,C'0'                                                     
         BE    *+6                                                              
         DC    H'0'                CVG SMPL = 6 DIGITS                          
         CLI   MITCOVG+1,C'0'                                                   
         BE    HDR8                                                             
         XC    COVSMP,COVSMP       SAVE 5 DIGIT CVGSMP AS BINARY                
         PACK  DUB,MITCOVG                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,COVSMP                                                     
HDR8     SR    R5,R5                                                            
         L     R1,=A(CVGTBL)       CVG CODE--CABLE CODE DEFINITIONS             
*                                                                               
HDR10    CLC   COVSMP,0(R1)        FIND COVG SAMPLE IN CABLE TABLE              
         BE    HDR15                                                            
         OC    0(4,R1),0(R1)       EMPTY BUCKET?                                
         BE    HDR12                                                            
         CLC   0(2,R1),=X'FFFF'                                                 
         BNE   *+6                                                              
         DC    H'0'                TABLE OVERFLOW                               
         LA    R5,1(R5)                                                         
         LA    R1,L'CVGTBL(R1)                                                  
         B     HDR10                                                            
*                                                                               
HDR12    MVC   0(4,R1),COVSMP      SAVE NUMERIC CVG SAMPLE IN TABLE             
*                                                                               
HDR15    STC   R5,COVSMPD          SAVE DISP INTO CVG TABLE                     
         MVC   COVALPH,COVSMP      SAVE ALPHA CABLE CODE                        
         B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
*H_REC - COMMON PROCESSING FOR ALL 'H' RECDS                                    
*        ONE RECORD PER 1/2HR FOR EACH DAY PLUS MON-FRI AND MON-SUN AVG         
*        FOR EACH COVERAGE SAMPLE. APPLIES TO REC SEQ CODES = 3,5               
* *********************************************************************         
*                                                                               
H_REC    NTR1                                                                   
         XC    INTPNAME,INTPNAME                                                
*                                                                               
*DAYS OF WEEK NOT PROVIDED ON MOVIE GOER MIT. COMPUTE THEM IN                   
*WKDAYS=XXXXXXX (X=C'1' FOR CORRESPONDING DAY OF WEEK)                          
*EXAMPLE: WKDAYS=1100001 IS MON,TUE,SUN                                         
         MVC   DAY,MITSTART                                                     
         MVC   WKDAYS,ZEROS                                                     
H_REC5   GOTO1 VGETDAY,DMCB,DAY,FULL     GET DAY OF WEEK                        
         CLI   0(R1),0            INVALID DATE                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                  DAY OF WEEK (1-7)                      
         BCTR  RF,0                      DISPLACEMENT IN WKDAYS                 
         LA    RE,WKDAYS(RF)                                                    
         MVI   0(RE),C'1'                                                       
         CLC   DAY,MITEND                                                       
         BNL   H_REC5X                                                          
         GOTO1 VADDAY,DMCB,(C'D',DAY),(X'20',DUB),F'1'                          
         MVC   DAY,DUB                                                          
         B     H_REC5                                                           
H_REC5X  DS    0H                                                               
*                                                                               
         LA    R0,7                                                             
         SR    RE,RE                                                            
         LA    RF,WKDAYS                                                        
H_REC10  CLI   0(RF),C'1'          CONVERT DAY CODE TO INTERNAL CODE            
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         SLL   RE,1                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,H_REC10                                                       
         SRL   RE,1                                                             
         STC   RE,DUB                                                           
         STC   RE,INTDYBIT                                                      
*                                                                               
*        CLC   MITAVG,=C'05'       NO M-F, M-S AVERAGES                         
*        BNE   *+8                  ON MOVIE GOER TAPE                          
*        MVI   DUB,X'7C'           SET TO A M-F RECD                            
*        CLC   MITAVG,=C'07'                                                    
*        BNE   *+8                                                              
*        MVI   DUB,X'7F'           SET TO A M-S RECD                            
*                                                                               
         LA    RE,DAYTAB                                                        
H_REC20  CLC   DUB(1),1(RE)        CONVERT DAY CODE TO INTERNAL CODE            
         BE    H_REC28                                                          
         LA    RE,L'DAYTAB(RE)                                                  
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   H_REC20                                                          
         DC    H'0'                                                             
*                                                                               
H_REC28  MVC   INTDAYWK,2(RE)      INTERNAL CODE                                
         MVC   INTPNAME(4),3(RE)   MOVE IN ALPHA DAY                            
*                                                                               
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
         MVI   INTDUR,2            HALF HOUR RECORDS (2 QH)                     
         MVC   INTBOOK,BOOK                                                     
         MVI   AVGWKS,0                                                         
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
*                                                                               
         CLC   HALF,BOOK           HAS TO BE CURRENT BOOK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
*                                                                               
*CONVERT START TIME FOR INTPNAME FIELD - CHAR OUTPUT                            
H_REC29  PACK  DUB,MITHOUR(2)                                                   
         CVB   RF,DUB                                                           
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LHI   RF,24               MAKE SURE MIDNIGHT IS NOT HOUR 00            
         STC   RF,DUB              DUB=HOUR 06(6AM)-29(5AM)                     
*                                                                               
         CLI   DUB,24              24-29= 12AM-5AM                              
         BL    H_REC35                                                          
         SH    RF,=H'24'                                                        
         STC   RF,DUB                                                           
         BNZ   H_REC30                                                          
         MVI   DUB,12              24=12 MIDNIGHT(AM)                           
H_REC30  MVI   DUB+1,C'A'          SET TO AM                                    
         B     H_REC38                                                          
*                                                                               
H_REC35  CLI   DUB,12                                                           
         BL    H_REC30             AM                                           
         SH    RF,=H'12'                                                        
         STC   RF,DUB                                                           
         BNZ   *+8                                                              
         MVI   DUB,12              12=NOON (PM)                                 
         MVI   DUB+1,C'P'          SET TO PM                                    
*                                                                               
H_REC38  MVI   DUB+2,C'M'          DUB=HOUR   DUB+1=AM/PM                       
         MVI   INTPNAME+4,C' '                                                  
         MVI   INTPNAME+7,C':'                                                  
         MVC   INTPNAME+8(2),MITMIN    MINUTE                                   
         MVC   INTPNAME+10(2),DUB+1     AM/PM                                   
         ZIC   RE,DUB              RE=START HOUR FOR INTPNAME                   
         LA    RF,INTPNAME+5                                                    
         EDIT  (RE),(2,0(RF)),ZERO=NOBLANK       OUTPUT START HOUR              
         CLI   INTPNAME+5,C' '                                                  
         BNE   *+14                                                             
         MVC   INTPNAME+5(6),INTPNAME+6                                         
         MVI   INTPNAME+11,0                                                    
*                                                                               
*COMPUTE MILITARY TIME FOR NUMERIC MANIPULATIONS                                
         PACK  DUB,MITHOUR(4)      START HOUR                                   
         CVB   RF,DUB                                                           
         STCM  RF,3,INTSTIM        START TIME IN MILITARY                       
*                                                                               
H_REC45  CLC   MITMIN,=C'30'       STARTS ON HALF HOUR                          
         BNE   *+12                                                             
         LA    RF,100(RF)          BUMP HOUR                                    
         SH    RF,=H'30'           ADJ HALF HOUR                                
         CLC   MITMIN,=C'00'       STARTS ON HOUR                               
         BNE   *+8                                                              
         LA    RF,30(RF)           BUMP MINUTES                                 
         STCM  RF,3,INTETIM        END TIME                                     
*                                                                               
H_REC50  PACK  DUB,MIHDUR(4)                                                    
         CVB   RF,DUB                                                           
         STCM  RF,3,INTCOV                                                      
*                                                                               
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         MVC   INTMRKT,TAPEMKT     DEMO SYSTEM MARKET CODE                      
*                                                                               
H_REC55  MVI   INTADUR,30          HALF HOUR RECD                               
         MVI   INTDUR,2                                                         
         MVI   INTDURM,30                                                       
         MVC   INTDURM2,=Y(30)                                                  
*                                                                               
H_RECX   B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
* *********************************************************************         
* SLOTDEM -    SLOT DEMOS IN APPROPRIATE BUCKETS IN BUFFER                      
*        INPUT PARMS:  DMCB   - A(OUTPUT BUFER)                                 
*                      DMCB+4 - A(1ST DEMO)                                     
*                      USHUT  - US HUT PROJECTION                               
*                      MKTHUT - MKT BREAK HUT PROJECTION                        
* *********************************************************************         
*                                                                               
SLOTDEM  NTR1                                                                   
         L     R1,=A(CRCITAB)                                                   
         MVC   DMCB1(8),DMCB                                                    
         B     SLOT10                                                           
SLOT5    LA    R1,L'CRCITAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   SLOT10                                                           
         B     SLOTX                                                            
*                                                                               
SLOT10   ZIC   RE,1(R1)                                                         
         MH    RE,MKTDISP          DISP TO MKT BRK IN BUFFER                    
         A     RE,DMCB             ADD ADDRESS OF OUTPUT BUFFER                 
         ST    RE,DMCB             RE-SAVE                                      
         MVC   RIUSA*4(4,RE),USHUT   SLOT HUT AND MKT BRK HUT                   
         MVC   RIHOMES*4(4,RE),MKTHUT                                           
         LA    R0,MIXDEMQ          NUMBER OF INPUT DEMOS                        
         LA    R5,1                WHICH DEMO WE'RE PROCESSING                  
*                                                                               
SLOT15   L     R6,=A(SLOTTAB)      FIND DEMO NUMBER IN TABLE                    
SLOT18   CLI   0(R6),X'FF'         END TABLE?                                   
         BE    SLOT20              THERE ARE SOME OPEN SLOTS                    
         CH    R5,0(R6)                                                         
         BE    *+12                DEMO TYPE FOUND                              
         LA    R6,4(R6)            BUMP POSITION IN SLOTTAB                     
         B     SLOT18                                                           
*                                                                               
         LH    RE,2(R6)            RE=OUTPUT SLOT                               
         SLL   RE,2                4 BYTE BUCKETS- RE=DISP TO BKT               
         A     RE,DMCB             RE=A(OUTPUT SLOT)                            
         L     RF,DMCB+4           A(INPUT DEMOS)                               
         LA    R6,9                BCT LOOP                                     
SLOT19   TM    0(RF),X'F0'                                                      
         BNO   SLOT19A             BETTER BE A LEGIT NUMBER                     
         LA    RF,1(RF)                                                         
         BCT   R6,SLOT19                                                        
         L     RF,DMCB+4           RESTORE RF                                   
         B     SLOT19B             OK, EVERYTHING CLEAR                         
*                                                                               
SLOT19A  SR    R6,R6                                                            
         B     SLOT19C                                                          
SLOT19B  PACK  DUB,0(9,RF)         CONVERT DEMO TO NUMERIC                      
         CVB   R6,DUB                                                           
SLOT19C  STCM  R6,15,0(RE)         SAVE DEMO IN BUCKET                          
*                                                                               
SLOT20   L     RF,DMCB+4           INPUT DEMO ADDRESS                           
         LA    RF,9(RF)            PT TO NEXT DEMO ON INPUT RECD                
         ST    RF,DMCB+4                                                        
         LA    R5,1(R5)            BUMP DEMO NUMBER                             
         BCT   R0,SLOT15           LOOP THRU ALL INPUT DEMOS                    
         MVC   DMCB(8),DMCB1                                                    
         B     SLOT5                                                            
*                                                                               
SLOTX    B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
* PRGREC-PROGRAM RECORD PROCESSING                                              
*              RECORD SEQUENCE CODE = 4                                         
*              RECORD TYPE          = D - P- P                                  
* *********************************************************************         
PRGREC   DS    0H                                                               
         CLI   MITREC,C'D'         PROCESS PROGRAM DISCRIPTOR REC (PDR)         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MITORIG,C'0'        BYPASS                                       
         BE    PRG3                IF CORRECTION DATE > START DATE              
         CLC   MITCORDT,MITSTDTC                                                
         BH    READ20                                                           
*                                                                               
PRG3     CLI   RELS,0              NOTHING TO RELEASE-JUST PROCESS              
         BE    PRG10                                                            
         CLI   RELS,4              RELEASE PREVIOUS PRG RECD?                   
         BE    PRG5                FINISH BLDING USAGE RECD & RELEASE           
         CLC   PRVMKT,MITMKTBR     TEST MKT BREAK?                              
         BE    PRG5                SAME MKT BREAK-> DIFFERENT RECD              
         CLC   PRVKEY(MITMKTBR-MITKEY),MITKEY    TEST OTHER KEY FIELDS          
         BE    PRG10               SAME -SAVE MKT BREAK/DON'T RELS              
PRG5     B     PRGREL              RELEASE PRG RECD                             
*                                                                               
PRG10    DS    0H                   DUPLICATE PROGRAM AVERAGE?                  
         CLC   MITKEY,PRVKEY                                                    
         BNE   *+10                                                             
         CLC   MITAVG,=C'01'         PROGRAM AVERAGE                            
         BNE   *+10                  = AVERAGE                                  
         CLC   MITTRACK,BLANKS       + NO TRACK                                 
         BNE   *+8                                                              
         B     READ20               IGNORE NEW RECD. THEY ARE SORTED            
*                                   BY REPORTABILITY INDIC, SO BEST             
*                                   WILL STAY                                   
         LA    RE,INTKEY                                                        
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   RELS,4                                                           
         MVC   PRVKEY,MITKEY       SAVE KEY                                     
         BAS   RE,HDR              PROCESS HDR INFO                             
         CLI   MKTBMIT,0                                                        
         BNE   *+8                                                              
         BAS   RE,PDRREC           PROCESS 'D' RECD  -DESCRIPTOR INFO           
         MVI   INTRTYP,PMCODEQU    'Q' RECORD                                   
         MVC   INTSTA(4),COVALPH   USAGE TO GEOGRAPHIC MKT                      
         MVI   INTSTA+4,C'C'       CABLE RECORD                                 
*                                                                               
         CLI   INTORIG,C'2'                                                     
         BE    PRGX                DELETION RECD                                
         L     R1,=A(DEMAREA)      SAVE IN DEMO AREA                            
         ST    R1,DMCB             DMCB= DEST OF DEMOS                          
         LA    R1,MIXDEM           1ST DEMO ON RECD-NON PUT                     
         ST    R1,DMCB+4           A(1ST DEMO ON INPUT TAPE)                    
         BAS   RE,SLOTDEM          PROCESS DEMOS-SLOT IN BUFFER                 
*                                                                               
PRGX     MVC   SAVEINT(L'SAVEINT),INTVALS                                       
         B     READ20              GO READ NEXT RECD                            
         EJECT                                                                  
* ********************************************************************          
* PDRREC - PROGRAM DESCRIPTOR RECORD EXTRACT FIELDS FOR INTERIM RECD            
* ********************************************************************          
*                                                                               
PDRREC   NTR1                                                                   
         MVI   NOTAVAL,0           READ PROG RECDS                              
         CLC   MITHLFID,=C'00'     ONLY PROCESS TOT DURATION RECORDS            
         BNE   PDRX                                                             
         CLI   INTORIG,C'0'                                                     
         BE    PDR10               GO PROCESS ORIGINAL RECDS                    
         MVC   INTCRRSN,MITCORRS   CORRECTION REASON                            
         MVC   INTCRDAT,MITCORDT+1 CORRECTION DATE--NEILSON SEQUENCER           
         CLI   INTORIG,C'2'                                                     
         BNE   PDR10               GO PROCESS ORIGINAL OR CHG/ADD RECS          
*                                                                               
*--DELETION RECORDS ONLY                                                        
*                                                                               
         MVC   INTSTA(4),COVALPH   SAVE NETWORK IN KEY                          
         MVI   INTSTA+4,C'C'       CABLE                                        
         XC    INTPNUM,INTPNUM     PROGRAM NUMBER                               
         MVC   PACK16(10),MITPRG                                                
         MVI   PACK16+11,C'0'      SAVE AS PACKED W/OUT SIGN                    
         PACK  DUB,PACK16(11)      PACK FIELD TO 6 CHARS                        
         MVC   INTPNUM(5),DUB+2    5 CHAR PRG NUM (DROP SIGN)                   
         SR    R1,R1               TRACKAGE NUMBER                              
         CLC   MITTRACK,BLANKS                                                  
         BE    *+14                                                             
         PACK  DUB,MITTRACK                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,INTTRK                                                      
         SR    R1,R1               TELECAST NUMBER                              
         CLC   MITTELC,BLANKS                                                   
         BE    *+14                                                             
         PACK  DUB,MITTELC+1(9)                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,INTTNUM                                                    
         SR    R1,R1               EPISODE NUMBER                               
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY     CORECTN BOOK           
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
*                                                                               
         CLC   MITCORDT,BLANKS                                                  
         BNE   *+16                                                             
         CLC   HALF,BOOK           NO CORRECTION DATE                           
         BE    *+6                                                              
         DC    H'0'                AND NOT INBOOK DELETE                        
*                                                                               
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
         B     PDRX                                                             
*                                                                               
*---ORIGINAL AND CHANGE/ADD RECORDS                                             
PDR10    LA    RE,MIDDAYS          SET UP SAVETIME TABLE FOR PDATA              
         MVC   PHUT,USHUT                                                       
         XC    SAVETIME(56),SAVETIME                                            
         LA    RF,SAVETIME                                                      
         LA    R0,7                                                             
PDR20    CLI   0(RE),C'0'                                                       
         BE    PDR25                                                            
         MVC   0(2,RF),MITHOUR     START HOUR                                   
         MVC   2(2,RF),MITMIN      START MINUTE                                 
         MVC   4(4,RF),MIDDUR      DURATION IN MINUTES                          
PDR25    LA    RE,1(RE)                                                         
         LA    RF,L'SAVETIME(RF)                                                
         BCT   R0,PDR20                                                         
*                                                                               
         MVC   DAYS,ZEROS                                                       
         MVC   VARS,ZEROS                                                       
         XC    SAVVAR(49),SAVVAR                                                
*                                                                               
         LA    RE,MIDDAYS          --INDIV DAY DATA                             
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         SR    R1,R1               CNT # DAYS IN AVERAGE                        
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
PDR30    CLI   0(RE),C'1'                                                       
         BNE   *+12                                                             
         MVI   0(RF),C'1'                                                       
         LA    R1,1(R1)            BUMP CNT ON # DAYS                           
         LA    RE,1(RE)            PT TO NEXT DAY BIT IN MIDDAYS                
         LA    RF,1(RF)            NEXT DAY FIELD IN DAYS                       
         BCT   R0,PDR30                                                         
         CH    R1,=H'1'            HOW MANY DAYS IN AVG?                        
         BE    PDR60               ONLY ONE, NOT VARIOUS                        
         CLC   MITAVG,=C'00'       INDIVIDUAL DAY DATA?                         
         BE    PDR60               YES                                          
*                                                                               
*--1WK AVG                                                                      
         CLC   MIDDAYS,=C'1111100'  ---AVERAGED DATA --M-F?                     
         BE    PDR60                                                            
         CLC   MIDDAYS,=C'1111111'  M-S?                                        
         BE    PDR60                                                            
*                                                                               
PDR40    DS    0H                                                               
         MVI   VARIOUS,1           VARIOUS DAYS                                 
         MVC   VARS,ZEROS          1ST TIME THRU                                
         XC    VARSTIME(56),VARSTIME                                            
         MVC   VARS(7),DAYS                                                     
*                                                                               
PDR60    XC    PVALS,PVALS                                                      
         PACK  DUB,MITPRG+6(4)     PROGRAM NUMBER                               
         CVB   R1,DUB                                                           
         STCM  R1,3,PNUM           SAVE AWAY PRG NUMBER                         
*                                                                               
         PACK  DUB,MIDPRDUR                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,PTOTDUR        DURATION                                     
*                                                                               
*C       MVC   PDPT,MIDAYPT        TYPE DAYPART                                 
         MVC   PDPT2,MIDREPDP                                                   
         MVC   PNET,COVALPH        SET UP R/S CALL LETTERS                      
*C       MVC   PNAME,MIDPNAME                                                   
*C       MVC   PTITLE,MIDEPNAM     EPISODE NAME                                 
         MVC   PTYP,MIDPTYP                                                     
         MVC   PPREM,MIDPREM                                                    
         MVC   PSHORT,MIDSDUR                                                   
         MVC   INTMRKT,TAPEMKT     CABLE MOVIE GOER MKT=310                     
         MVC   INTSTA(4),COVALPH   SAVE NETWORK IN KEY                          
         MVI   INTSTA+4,C'C'       CABLE                                        
*                                                                               
         XC    INTPNUM,INTPNUM                                                  
         MVC   PACK16(10),MITPRG   FORCE NUMBER TO PACKED W/OUT SIGN            
         MVI   PACK16+11,C'0'                                                   
         PACK  DUB,PACK16(11)      PACK FIELD TO 6 CHARS                        
         MVC   INTPNUM(5),DUB+2    5 CHAR PRG NUM (DROP SIGN)                   
*                                                                               
         SR    R1,R1                                                            
         CLC   MITTRACK,BLANKS      TRACKAGE                                    
         BE    *+14                                                             
         PACK  DUB,MITTRACK                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,INTTRK                                                      
*                                    NO EPISODES ON TAPE                        
*        SR    R1,R1                                                            
*C       CLC   MIDEPS,BLANKS       EPISODE NUMBER                               
*        BE    *+18                                                             
*        PACK  DUB,MIDEPS                                                       
*        CVB   R1,DUB                                                           
*        STCM  R1,7,INTEPS         (3 BYTES)                                    
*                                                                               
         SR    R1,R1                                                            
         CLC   MITTELC,BLANKS      TELECAST NUMBER                              
         BE    *+18                                                             
         PACK  DUB,MITTELC+1(9)                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,INTTNUM                                                    
*                                                                               
*                                   NO LIBRARY ID FOR MOVIE GOER                
*        SR    R1,R1                                                            
*C       CLC   MIDLIB,BLANKS       LIBRARY ID                                   
*        BE    *+18                                                             
*        PACK  DUB,MIDLIB+1(9)                                                  
*        CVB   R1,DUB                                                           
*C       STCM  R1,15,INTLIB                                                     
*                                                                               
         MVC   INTANET,MITTYPE     ACN NETWORK CODE                             
         MVC   INTFEED,MIDFDPAT    FEED PATTERN FOR PROGRAM                     
         MVC   INTAUDES,MITAUDTY   AUG EST TYPE                                 
         MVC   INTCOVSM,MITCOVG    COVERAGE SAMPLE                              
         MVC   INTCOVCL,MITCOVCL   COV SAMPL CALC IND                           
         MVC   INTPNAME,MIDPNAME   PROGRAM NAME                                 
         MVC   INTTRNAM,MIDTKNAM   TRACKAGE NAME                                
*C       MVC   INTEPNAM,MIDEPNAM   EPISODE NAME                                 
*C       MVC   INTCLTEP,MIDEPNUM   CLT EPISODE PRG NUMBER                       
         MVC   INTPTYP,MIDPTYP     PROGRAM TYPE ALPHA                           
         MVC   INTSBTYP,MIDSUBPT   SUB-PRG TYPE ALPHA                           
         MVC   INTCMCL,MIDCOMR     COMMERCIAL STATUS                            
*C       MVC   INTLIVE,MIDLIVE     LIVE EVENT INDIC                             
*C       MVC   INTPOA,MIDPRORG     PRG ORIGINAL/ACQUIRED                        
*C       MVC   INTEOA,MIDEPORG     EPISODE  ORIGIAL/ACQUIRED                    
         MVC   INTPREM,MIDPREM     PREMIERE INDICATOR                           
*                                                                               
*C       PACK  DUB,MIDAUPRJ        PROJECTION (XXX,XXX,XXX)                     
*        CVB   R1,DUB                                                           
*        SR    R0,R0                                                            
*        AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
*        D     R0,=F'10000'                                                     
*C       STCM  R1,3,PWK1AUD        REACH                                        
*                                                                               
         TM    MIDPROJ+(L'MIDPROJ-1),X'F0'                                      
         BO    *+10                                                             
         MVC   MIDPROJ,=16C'0'                                                  
         PACK  DUB,MIDPROJ                                                      
         CVB   RF,DUB                                                           
         ST    RF,SVAVGHI          SAVE AVG HOMES IMPS                          
*                                                                               
         LA    RE,TYPTAB           POINT RE AT TELECAST TYPE TABLE              
         LA    R0,TYPES            COUNTER                                      
PDR80    CLC   MITSPC,0(RE)        0=REGULAR   1=SPECIAL                        
         BNE   *+14                                                             
         CLC   MIDREP,1(RE)        IS BLANK FOR NETWORK!                        
         BE    PDR85                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,PDR80                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
*                                                                               
PDR85    MVC   PWK1DTYP,2(RE)      X'09'=REGUALR  X'0C'=SPECIAL                 
*        CLC   MITAVG,=C'00'     INDIV DAY REC?-->ALL AVG RECORDS               
*        BNE   PDR90               NO, AN AVG RECORD                            
*C       CLI   MIDMULT,C' '        MULTI-DAYS?                                  
*        BE    *+8                                                              
*        OI    PWK1DTYP,X'80'      SET OPT BIT IN PHTDTYP                       
*                                                                               
PDR90    MVI   PWK1RSF,0                                                        
         CLI   MITBREAK,C'1'        TEST FOR REDUCED STATION                    
         BNE   *+8                                                              
         MVI   PWK1RSF,X'01'       REDUCED STATION INDICATOR- BREAK OUT         
*                                                                               
*C       MVC   INTDPT,PDPT         PDPT IS ALWAYS ZERO                          
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         MVC   INTPNO,PNUM                                                      
         MVC   INTCOV,PTOTDUR                                                   
         MVI   INTSTYP,0                                                        
         MVI   INTMTYP,0                                                        
*C       MVC   INTAUD,PWK1AUD                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
         MVC   INTBOOK,BOOK        DEFAULT: BOOK=YYMM FOR MULTI WEEK            
         MVC   INTIBOOK,IBOOK         "       "     "                           
*                                                                               
PDR95    DS    0H                                                               
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
*                                                                               
         CLC   MITCORDT,BLANKS                                                  
         BNE   *+16                                                             
         CLC   HALF,BOOK           NO CORRECTION DATE                           
         BE    *+6                                                              
         DC    H'0'                AND DATE OUTSIDE CURRENT BOOK                
*                                                                               
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
*                                                                               
PDR100   MVI   RELS,4                                                           
         LA    RE,MIDDAYS          POINT TO DAY FIELDS                          
PDR105   LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
*                                                                               
PDR110   CLI   0(RE),C'0'          TEST FOR NO ACTIVITY                         
         BE    *+6                                                              
         OR    RF,R5               UPDATE CUMULATIVE BITS                       
         LA    RE,1(RE)                                                         
         SRL   R5,1                NEXT DAY                                     
         BCT   R0,PDR110                                                        
*                                                                               
         STC   RF,INTDAYWK         SAVE BITS                                    
         MVI   BYTE,X'90'          SET INTERNAL DAY CODE TO VAR                 
         LA    RE,NETDAYTB                                                      
         LA    R0,NETDAYS                                                       
         CLC   INTDAYWK,0(RE)      TEST BITS VS. TABLE                          
         BE    *+16                                                             
         LA    RE,L'NETDAYTB(RE)                                                
         BCT   R0,*-14                                                          
         B     *+10                                                             
         MVC   BYTE,1(RE)          INTERNAL DAY CODE                            
*                                                                               
         MVC   PDAY1,BYTE                                                       
         MVC   PDAY1BIT,INTDAYWK   NOT INTERNAL CODE,BUT DAY BITS               
PDR120   GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
         MVI   INTRTYP,PMCODEQU    -Q-                                          
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         MVC   INTDURM,PWK1DURM    DURATION IN MINUTES                          
         MVC   INTDURM2,PWK2DURM   LONG DURATION (2 BYTES)                      
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
*                                                                               
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
*        ZIC   RF,PWK1DURM         DURATION IN MINUTES                          
         SR    RF,RF                                                            
         ICM   RF,3,PWK2DURM                                                    
         LA    RF,8(RF)            ADD 8 BEFORE CNV TO QH                       
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         CH    RF,=H'1'            TEST FOR DURATION OF AT LEAST ONE            
         BH    *+8                                                              
         LA    RF,1                                                             
         STC   RF,INTDUR           DURATION IN QH                               
         AR    R0,RF               SQH PLUS DUR                                 
         STC   R0,INTEQH                                                        
         EJECT                                                                  
*              ADD DURATION IN MINUTES TO START TIME (MILITARY) TO              
*              GET END TIME                                                     
         SR    R0,R0                                                            
         LH    R1,HALF             START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
*        ZIC   RF,INTDURM          DURATION IN MINUTES                          
         SR    RF,RF                                                            
         ICM   RF,3,INTDURM2       2-BYTE DURATION                              
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CH    R0,=H'60'           TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SH    R0,=H'60'           SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MH    R1,=H'100'          HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,INTETIM        END TIME                                     
*                                                                               
         MVC   INTDAYWK,PDAY1                                                   
         OC    INTDAYWK,PWKBIT                                                  
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/P4RTN                         
*                                                                               
PDRX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*PRGREL -      RELEASE PROGRAM RECORDS                                          
***********************************************************************         
PRGREL   MVI   MYMODE,C'P'                                                      
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         XC    P(132),P                                                         
         B     PRGREL10                                                         
*                                                                               
PRGREL5  DS    0H                  MAKE A 'P' REC FOR EA HHR PRG RAN            
         ZIC   RE,INTSQH           ROUND START QTR HR TO THE HALF HR            
         SRL   RE,1                (THE FILE HAS ONLY HHR DATA)                 
         SLL   RE,1                                                             
         STC   RE,SQHR                                                          
         MVI   DAYBIT,1                                                         
         CLI   LSTCODE,C'R'                                                     
         BE    *+8                FOR ORIGINAL AND CURRENT BOOK RECORDS         
         B     PRGTIME             BUILD FUDGED 'P' RECORDS FROM '04'           
         MVI   RELS,0                                                           
         MVI   RELSLOT,0                                                        
         MVI   MYMODE,0                                                         
         CLI   RECD,C'Z'           END OF FILE?                                 
         BE    ENDJOB                                                           
         B     READ40              PROCESS CURRENT RECD                         
*                                                                               
PRGREL10 L     RE,=A(CRCOTAB)                                                   
PRGREL13 CLI   0(RE),X'FF'                                                      
         BE    PRGREL5             WHEN DONE FORMING INTERD'S FOR MKTS          
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,L'CRCOTAB(RE)                                                 
         B     PRGREL13                                                         
         CLI   3(RE),0                                                          
         BNE   PRGREL20                                                         
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     PRGREL10                                                         
*                                                                               
PRGREL20 MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   SAVEINT,INTVALS     UPDATE SAVEINT                               
*                                                                               
         CLI   INTORIG,C'2'                                                     
         BE    PRGREL28                                                         
*                                                                               
         LA    R1,INTACCS                                                       
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,MKTDISP                                                       
         A     RE,=A(DEMAREA)                                                   
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),0(RE)    MOVE IN DEMOS                                  
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                  MOVE IN UNIVS                                
         CLI   INTORIG,C'0'                                                     
         BE    *+14                                                             
         CLC   INTCRDAT,TAPEWK     CORRECTION RECD FOR DIFF WEEK                
         BNE   PRGREL28             DO NOT USE THIS WEEK'S UNVS                 
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIUNVSQC)                                                  
         ZIC   RF,COVSMPD          DISPL TO COVERAGE                            
         MH    RF,CVGLN                                                         
         ZIC   RE,RELSLOT          DISP TO MARKET BREAK                         
         MH    RE,MKTDISP                                                       
         AR    RE,RF                                                            
         A     RE,=A(UNVBUFF)      UNIVERSE BUFFER                              
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                                                               
PRGREL28 ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
*                                                                               
PRGREL30 BAS   RE,PRGKEY                                                        
         LA    RE,INTKEY                                                        
         USING QPRGKD,RE           PROGRAM KEY DSECT                            
         MVC   LSTCODE,QCODE       'Q' OR 'R'                                   
         CLI   INTORIG,C'2'        DELETE REQUEST                               
         BNE   RELEASE                                                          
         OC    QFILNUM,QFILNUM                                                  
         BNZ   RELEASE                                                          
         MVI   RELS,0             IF RECD NOT ON FILE,DROP DELETE RQST          
         MVI   MYMODE,0                                                         
         MVI   RELSLOT,0                                                        
*        B     READ20                                                           
         B     READ40                                                           
         DROP  RE,R9                                                            
         EJECT                                                                  
* *********************************************************************         
* PRGKEY-  SPECIAL KEY FOR PROGRAM RECDS TO FORCE PROPER SORT FOR               
*          '0F' ELEMENTS WHEN RECORDS GET PASSED TO OUTPUT PHASE.               
* *********************************************************************         
PRGKEY   NTR1                                                                   
         LA    RF,INTKEY                                                        
         USING QPRGKD,RF           PROGRAM KEY DSECT                            
         XC    INTKEY,INTKEY                                                    
         MVC   QKSRC,KEYSRC                                                     
         MVI   QCODE,QCODEQ        FUDGE PRG CODE AS 'Q' RECORD                 
*                                                                               
PRGK10   MVC   QBOOK,INTBOOK                                                    
         MVC   QNET,INTANET        NETWORK                                      
         MVC   QNTINUM,INTPNUM     NTI PROGRAM NUMBER (PWOS)                    
         MVC   QTRK,INTTRK         SAVE TRACKAGE                                
         MVI   QAVG,1              INDIV DAY DATA                               
         CLI   INTAVG,0                                                         
         BE    *+8                                                              
         MVI   QAVG,0              WEEKLY AVG (SORT BEFORE DAILY DATA)          
         MVC   QDATE,INTDAYWK      DATE                                         
         MVC   QTELC,INTTNUM       TELECAST NUMBER                              
         MVC   QMKTB,MKTBRK        MARKET BREAK                                 
*                                                                               
         CLI   INTORIG,C'0'        SPECIAL KEY FIELDS FOR REPROCESSING          
         BE    PRGKX                                                            
         CLC   INTCRDAT,TAPEWK     INBK CORRECTION TREATED AS ORIG RECD         
         BNE   *+12                                                             
         MVI   INTORIG,C'0'        CHG INDICATOR FOR OPHASE                     
         B     PRGKX                                                            
         MVI   QCODE,C'R'          R=CORRECTION KEY INDICATOR                   
         MVC   QNET,INTSTA                                                      
         XC    QDATE,QDATE                                                      
         BAS   RE,GETFILN          CONVERT NTI#->DDS#->FILE# FOR BK             
         MVC   QDDSNUM,WORK                                                     
         MVC   QFILNUM,WORK+2                                                   
*                                                                               
PRGKX    B     XIT                 DONE BUILDING KEY                            
         EJECT                                                                  
***********************************************************************         
*PRGTIME - TIME PERIOD RECDS GENERATED FROM PRG RECDS. USED TO                  
* MERGE THE PRG NAME WITH THE '05' HHR RECDS.  MERGE OCCURS IN                  
* CNVWR ROUTINE.                                                                
***********************************************************************         
PRGTIME  DS    0H                                                               
         MVI   FRELS,1                                                          
         MVI   MYMODE,C'N'         PRG NAME MODE                                
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
*                                                                               
         OC    PRVPNUM,PRVPNUM                                                  
         BZ    *+18                FIRST TIME THRU OR ALREADY MERGED            
         CLC   INTPNUM,PRVPNUM     DID THE PROGRAM NUMBER CHANGE?               
         BE    *+8                                                              
         BAS   RE,FUDGMER          YES.MERGE PNAMTAB INTO HHR TABLE             
*                                                                               
         OC    PRVNET,PRVNET                                                    
         BZ    PRGTIM1             FIRST TIME THRU OR ALREADY RELEASED          
         CLC   INTANET,PRVNET      DID THE NETWORK CHANGE?                      
         BE    PRGTIM1                                                          
*RGTIM0  LA    RE,HHRTAB          RELEASE FUDGED RECORDS FOR PREV NETK          
*        ST    RE,INDEX                                                         
PRGTIM0  MVC   INDEX,=A(HHRTAB)                                                 
         MVC   SVMODE,MYMODE                                                    
         B     FUDGREL                                                          
PRGTIM1  DS    0H                                                               
*                                                                               
         MVC   PRVPNUM,INTPNUM                                                  
         MVC   PRVNET,INTANET                                                   
         MVC   PRVINT,SAVEINT                                                   
*                                                                               
PRGTIM5  CLC   SQHR,INTEQH       LOOP THRU QHRS UNTIL END QHR                   
*RGTIM5  CLC   SQHR,EQHR         LOOP THRU QHRS UNTIL END QHR                   
         BNL   PRGTIMX                                                          
*                                                                               
PRGTIM10 ZIC   R7,DAYBIT          BUILD ONE RECORD FOR EACH DAY                 
         EX    R7,*+8                                                           
         B     *+8                                                              
         TM    INTDYBIT,0                                                       
         BO    PRGTIM15           DAY ON                                        
         SLL   R7,1               R7=BIT TO CHECK.DO NOT CHANGE                 
         STC   R7,DAYBIT                                                        
         CHI   R7,X'40'           X'40'-LAST BIT TO CHECK                       
         BH    PRGTIM50          NO MORE DAYS.  PROCESS NEXT QHR                
         B     PRGTIM10                                                         
PRGTIM15 LA    RE,NETDAYTB                                                      
         LA    R0,NETDAYS                                                       
         CLC   DAYBIT,0(RE)      TEST BITS VS. TABLE                            
         BE    *+16                                                             
         LA    RE,L'NETDAYTB(RE)                                                
         BCT   R0,*-14                                                          
         B     *+10                                                             
         MVC   INTDAYWK,1(RE)      INTERNAL DAY CODE (SINGLE DAY)               
*                                                                               
*IF PROGRAM AVERAGE, BUILD TABLE OF PROG NAMES FOR EA DAY/HHR                   
*                                                                               
         OC    INTTRK,INTTRK       PROGRAM AVERAGE?                             
         BNZ   PRGTIM25                                                         
         OC    INTTNUM,INTTNUM                                                  
         BNZ   PRGTIM25                                                         
         L     RE,=A(PNAMTAB)      YES                                          
PRGTIM20 CLC   =X'FFFF',0(RE)                                                   
         BNE   *+6                 TABLE OVERFLOW                               
         DC    H'0'                                                             
         OC    0(L'PNAMTAB,RE),0(RE)  NO ENTRY?                                 
         BZ    *+12                                                             
         LA    RE,L'PNAMTAB(RE)                                                 
         B     PRGTIM20           TRY NEXT ENTRY                                
         USING PNAMED,RE                                                        
*        ZIC   R0,SQHR                                                          
*        CHI   R0,96                                                            
*        BL    *+8                                                              
*        SHI   R0,96                                                            
*        STC   R0,PNQHR                                                         
         MVC   PNQHR,SQHR                                                       
         MVC   PNDAY,INTDAYWK                                                   
         MVI   PNTYPE,C'P'                                                      
         MVC   PNHNAME,INTPNAME                                                 
         MVC   PNPNAME,INTPNAME                                                 
         B     PRGTIM45                                                         
*                                                                               
*IF TRACKAGE OR TELECAST(SPEC/BRKT), ADD NEW DAYS/HHR ENTRIES TO TABLE.         
*FOR SAME DAY,DELETE PROG AVE ENTRIES AND ADD TRACKAGE INFO                     
*                                                                               
PRGTIM25 L     RE,=A(PNAMTAB)                                                   
PRGTIM30 CLC   =X'FFFF',0(RE)                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    0(L'PNAMTAB,RE),0(RE)                                            
         BZ    PRGTIM40                                                         
         CLC   INTDAYWK,PNDAY                                                   
         BNE   PRGTIM35                                                         
         CLI   PNTYPE,C'P'          MAKE SURE THIS IS PROG AVE ENTRY            
         BNE   PRGTIM35                                                         
         MVC   0(L'PNAMTAB,RE),BLANKS                                           
PRGTIM35 LA    RE,L'PNAMTAB(RE)                                                 
         B     PRGTIM30                                                         
PRGTIM40 MVC   PNDAY,INTDAYWK                                                   
         MVC   PNQHR,SQHR                                                       
*        ZIC   R0,SQHR                                                          
*        CHI   R0,96                                                            
*        BL    *+8                                                              
*        SHI   R0,96                                                            
*        STC   R0,PNQHR                                                         
         MVI   PNTYPE,C'T'                                                      
         MVC   PNHNAME,INTTRNAM                                                 
         MVC   PNPNAME,INTPNAME                                                 
         B     PRGTIM45                                                         
*                                                                               
PRGTIM45 ZIC   RE,DAYBIT           UPDATE DAY OF WEEK. PROCESS NEXT DAY         
         SLL   RE,1                                                             
         STC   RE,DAYBIT                                                        
         B     PRGTIM10                                                         
PRGTIM50 ZIC   RE,SQHR              BUMP QHR COUNTER FOR NEXT ITERATION         
         LA    RE,2(RE)                                                         
         STC   RE,SQHR                                                          
         MVI   DAYBIT,1            START WITH FIRST DAY BIT AGAIN               
         B     PRGTIM5                                                          
*                                                                               
PRGTIMX  XC    INTSTA,INTSTA                                                    
         MVI   RELS,0              CLEAR PREV PRG RECD SWITCH                   
         MVI   MYMODE,0                                                         
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         CLI   RECD,C'Z'                                                        
         BNE   READ40              PROCESS CURRENT RECD IN BUFFER               
         BAS   RE,FUDGMER          END OF FILE,RELEASE PREV FUDGD RECDS         
*        MVI   MYMODE,C'N'         PRG NAME MODE                                
         B     PRGTIM0                                                          
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* FUDGMER - MERGE TABLE OF NAMES FOR A PROGRAM WITH HHR TABLE                   
* FOR ALL PROGRAMS FOR THIS NETWORK .                                           
* THE GOAL IS TO END UP WITH A TABLE THAT PROVIDES A PROGRAM NAME FOR           
* EACH HHR AND DAY OF THE WEEK.                                                 
**********************************************************************          
FUDGMER  NTR1                                                                   
         USING PNAMED,R5                  DSECT FOR PNAMTAB                     
HHRT     USING PNAMED,R7                  DSCT FOR HHRTAB                       
*                                                                               
         L     R5,=A(PNAMTAB)                                                   
FDMER5   CLC   =X'FFFF',0(R5)                                                   
         BE    FUDGMERX                                                         
         OC    0(L'PNAMTAB,R5),0(R5)                                            
         BZ    FUDGMERX                                                         
         CLC   0(L'PNAMTAB,R5),BLANKS                                           
         BNE   *+18                                                             
FDMER10  XC    0(L'PNAMTAB,R5),0(R5)    CLEAR TABLE ENTRY                       
         LA    R5,L'PNAMTAB(R5)                                                 
         B     FDMER5                                                           
*                                                                               
         ZIC   R0,PNQHR                                                         
*        CHI   R0,96                                                            
*        BL    *+8                                                              
*        SHI   R0,96                                                            
         SRL   R0,1               HHR=QHR/2 (QHR IS ALREADY ROUNDED UP)         
         MHI   R0,7               7 DAYS A WEEK                                 
         ZIC   R1,PNDAY           EX: M=X'10',TU=X'20'...                       
         SRL   R1,4               M=X'01',TU=X'02'...                           
         BCTR  R1,0               START DAYS FROM 0                             
         AR    R0,R1                                                            
         MHI   R0,L'HHRTAB                                                      
         LA    R7,HHRTAB                                                        
         AR    R7,R0              POINT TO HHR AND DAY IN HHRTAB                
*                                                                               
         OC    0(L'HHRTAB,R7),0(R7)                                             
         BZ    FDMER30             JUST ADD TO TABLE                            
         CLC   PNHNAME,HHRT.PNHNAME  DIFF NAME FOR SAME DAY/HHR?                
         BE    FDMER10                              NO.                         
         CLC   PNPNAME,HHRT.PNPNAME  CHECK PRG AVG NAME                         
         BNE   *+14                       IF THE SAME                           
         MVC   HHRT.PNHNAME,HHRT.PNPNAME  SAVE AS HH NAME                       
         B     FDMER10                                                          
         MVC   HHRT.PNPNAME,ZEROS                                               
         MVC   HHRT.PNHNAME,BLANKS                                              
         MVC   HHRT.PNHNAME(7),=C'VARIOUS'                                      
         B     FDMER10                                                          
*                                                                               
FDMER30  MVC   0(L'HHRTAB,R7),0(R5)                                             
         B     FDMER10              MERGE NEXT ENTRY FROM PNAMTAB               
*                                                                               
FUDGMERX DS    0H                                                               
         XC    PRVPNUM,PRVPNUM                                                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* FUDGREL - BUILD AND RELEASE FUDGED 'P' RECORDS FROM HHRTAB                    
* USED TO GET THE PROGRAM NAME FOR THE TIME PERIOD 'P' RECORDS ('05')           
**********************************************************************          
FUDGREL  DS    0H                                                               
*                                                                               
         MVI   MYMODE,C'F'                                                      
FDREL1   ZIC   RE,FDCOUNT          UPDATE COUNTER                               
         LA    RE,1(RE)            1=M,2=TU...7=SU,8=M-F,9=M-S                  
         STC   RE,FDCOUNT                                                       
*                                                                               
FDREL5   MVC   INTVALS(L'PRVINT),PRVINT                                         
*                                                                               
*                                                                               
*                                                                               
         USING PNAMED,R5                                                        
*                                                                               
         CLI   FDCOUNT,8           TIME TO RELEASE M-F RECORD?                  
         BNE   FDREL10                                                          
*                                                                               
*        L     R9,VCPRINT                                                       
*        USING DPRINT,R9                                                        
*        XC    P(132),P                                                         
*        MVC   P(25),MFHNAME                                                    
*        MVC   P+30(25),MSHNAME                                                 
*        EDIT  FDCOUNT,(1,P+60),ZERO=NOBLANK                                    
*        GOTO1 VPRINTER                                                         
*                                                                               
         OC    MFHNAME,MFHNAME     BLANK. DON'T RELEASE ANYTHING                
         BZ    FDREL1                                                           
         LA    R5,FDENTRY          BUILD A FAKE M-F ENTRY                       
         MVC   PNQHR,SVQHR                                                      
         MVI   PNDAY,X'00'         M-F                                          
         MVC   PNHNAME,MFHNAME                                                  
         XC    MFHNAME,MFHNAME                                                  
         XC    MFPNAME,MFPNAME                                                  
         B     FDREL30             GO BUILD KEY AND RELEASE                     
FDREL10  CLI   FDCOUNT,9           TIME TO RELEASE M-S RECORD?                  
         BL    FDREL15             SINGLE DAY                                   
         BE    *+6                 M-S                                          
         DC    H'0'                                                             
*                                                                               
*        L     R9,VCPRINT                                                       
*        USING DPRINT,R9                                                        
*        XC    P(132),P                                                         
*        MVC   P(25),MFHNAME                                                    
*        MVC   P+30(25),MSHNAME                                                 
*        EDIT  FDCOUNT,(1,P+60),ZERO=NOBLANK                                    
*        GOTO1 VPRINTER                                                         
*                                                                               
         OC    MSHNAME,MSHNAME       BLANK. DON'T RELEASE ANYTHING              
         BNZ   *+12                                                             
         MVI   FDCOUNT,0           RESET COUNTER AFTER M-S                      
         B     FDREL1                                                           
         LA    R5,FDENTRY          BUILD A FAKE M-S ENTRY                       
         MVC   PNQHR,SVQHR                                                      
         MVI   PNDAY,X'80'         M-S                                          
         MVC   PNHNAME,MSHNAME                                                  
         XC    MSHNAME,MSHNAME                                                  
         XC    MSPNAME,MSPNAME                                                  
         B     FDREL30                                                          
*                                                                               
FDREL15  DS    0H                                                               
FDREL20  L     R5,INDEX                                                         
         CLC   =X'FFFF',0(R5)                                                   
         BE    FUDGRELX                                                         
*                                                                               
*        L     R9,VCPRINT                                                       
*        USING DPRINT,R9                                                        
*        XC    P(132),P                                                         
*        MVC   P(53),0(R5)                                                      
*        EDIT  FDCOUNT,(1,P+60),ZERO=NOBLANK                                    
*        GOTO1 VPRINTER                                                         
*                                                                               
         OC    0(L'HHRTAB,R5),0(R5)                                             
         BNZ   FDREL30                                                          
FDREL25  LA    R5,L'HHRTAB(R5)     GO TO NEXT ENTRY IN TABLE                    
         ST    R5,INDEX                                                         
         ZIC   RE,FDCOUNT          UPDATE COUNTER                               
         LA    RE,1(RE)                                                         
         STC   RE,FDCOUNT                                                       
         CLI   FDCOUNT,8           LAST DAY OF THE WEEK                         
         BE    FDREL5              GO RELEASE M-F, M-S RECORDS                  
         B     FDREL20                                                          
*                                                                               
FDREL30  XC    INTKEY,INTKEY                                                    
         LA    R7,INTKEY           BUILD -P- PAV KEY                            
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     -P- RECORD                                   
         MVI   PRMEDIA,C'C'                                                     
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRBTYP+1(1),PNDAY                                                
         MVC   PRBTYP+3(1),PNQHR     MOVE IN QHR                                
         MVI   PRBTYP+4,X'00'        FORCE BEFORE ORIGINAL RECORDS              
         MVC   SVQHR,PNQHR                                                      
         MVC   PRDW,PNDAY                                                       
*                                                                               
         LA    RE,INTVALS          CLEAR ALL BUT INTKEY                         
         LA    RF,1000-L'INTKEY                                                 
         XCEF                                                                   
*                                                                               
         MVI   INTRTYP,C'P'        BUILD KEY                                    
         MVC   INTPNAME,PNHNAME                                                 
*                                                                               
         CLI   FDCOUNT,8           RELEASING M-F, M-S RECORD                    
         BE    RELEASE                                                          
         CLI   FDCOUNT,9           AFTER M-S,                                   
         BNE   *+12                                                             
         MVI   FDCOUNT,0           RESET COUNTER                                
         B     RELEASE                                                          
*                                  GET M-F,M-S NAME AVERAGES                    
         CLI   PNDAY,X'60'         EXCLUDE SAT,SUN FROM M-F NAME AVG            
         BE    FDREL50                                                          
         CLI   PNDAY,X'70'                                                      
         BE    FDREL50                                                          
         OC    MFHNAME,MFHNAME         -- M-F --                                
         BNZ   FDREL40              IF BLANK                                    
         MVC   MFHNAME,PNHNAME      MOVE IN HH NAME FROM TABLE                  
         MVC   MFPNAME,PNPNAME              PRG AVG NAME                        
         B     FDREL50                                                          
FDREL40  CLC   MFHNAME,PNHNAME      SAME HH NAME?                               
         BE    FDREL50              YES.OTHERWISE,                              
         CLC   MFPNAME,PNPNAME      SAME PRG AVG NAME?                          
         BNE   *+14                 NO - VARIOUS                                
         MVC   MFHNAME,MFPNAME      YES. KEEP PRG AVG NAME AS HH NAME           
         B     FDREL50                                                          
         MVC   MFPNAME,ZEROS                                                    
         MVC   MFHNAME,BLANKS                                                   
         MVC   MFHNAME(7),=C'VARIOUS'                                           
*                                                                               
FDREL50  OC    MSHNAME,MSHNAME         -- M-S --                                
         BNZ   FDREL55             IF BLANK                                     
         MVC   MSHNAME,PNHNAME      MOVE IN NAME FROM TABLE                     
         MVC   MSPNAME,PNPNAME                                                  
         B     FDREL60                                                          
FDREL55  CLC   MSHNAME,PNHNAME     SAME HH NAME?                                
         BE    FDREL60             YES. OTHERWISE,                              
         CLC   MSPNAME,PNPNAME     SAME PRG AVG NAME?                           
         BNE   *+14                NO - VARIOUS                                 
         MVC   MSHNAME,MSPNAME     YES. KEEP PRG AVG NAME AS HH NAME            
         B     FDREL60                                                          
         MVC   MSPNAME,ZEROS                                                    
         MVC   MSHNAME,BLANKS       NO.                                         
         MVC   MSHNAME(7),=C'VARIOUS'                                           
FDREL60  DS    0H                                                               
*                                                                               
         XC    0(L'HHRTAB,R5),0(R5)   CLEAR TABLE ENTRY WHEN DONE               
*                                                                               
         L     R5,INDEX                                                         
         LA    R5,L'HHRTAB(R5)                                                  
         ST    R5,INDEX                                                         
*                                                                               
         B     RELEASE                                                          
         DROP  R7                                                               
         DROP  R5                                                               
FUDGRELX DS    0H                                                               
*                                                                               
*        L     R9,VCPRINT                                                       
*        XC    P(132),P                                                         
*        MVC   P(27),=C'-------END OF RELEASE------'                            
*        GOTO1 VPRINTER                                                         
*                                                                               
         MVI   FDCOUNT,0         RESTORE COUNTER                                
         MVC   MYMODE,SVMODE                                                    
         MVI   FRELS,0                                                          
         XC    PRVNET,PRVNET                                                    
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         CLI   RECD,C'Z'           END OF FILE?                                 
         BNE   READ20                                                           
         CLI   MYMODE,0                                                         
         BE    ENDJOB                                                           
         B     READ20              RELEASE LAST BATCH                           
         EJECT                                                                  
**********************************************************************          
* GETFILN - FOR REPROCESSING TO GET PROPER SORT ORDER IN QPRGK KEY              
*   CONVERT NTI NUMBER TO DDS NUMBER THEN LOOK UP FILE NUMBER                   
*   BASED ON BOOK.  IF PRG# OR FILE NUMBER DOESN'T EXIST, SET A HIGH            
*   VALUE INSTEAD.                                                              
*   FILENUM RETURNED IN WORK+2(2), DDS NUMBER IN WORK                           
**********************************************************************          
GETFILN  NTR1                                                                   
         LA    R5,KEY              LOOK UP NTI PRG CODE PASSIVE RECD            
         XC    KEY,KEY                                                          
         XC    WORK(4),WORK                                                     
         USING PJKEY,R5                                                         
         MVI   PJCODE,PJCODEQU                                                  
         MVI   PJMEDIA,C'C'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         MVC   PJSTAT(4),INTSTA    NETWORK                                      
         MVI   PJSTAT+4,C'C'       CABLE                                        
         MVC   PJEXTNUM,INTPNUM    NTI NUMBER                                   
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(PJINTNUM-PJKEY),KEYSAVE   WAS NTI-DDS NUMBER FOUND           
         BNE   GETFILX             <- NOT FOUND, BUMP FILE#                     
         MVC   WORK(2),PJINTNUM       DDS NUMBER                                
         XC    KEY,KEY                                                          
         MVC   PJCODE(3),=C'JCN'                                                
         MVC   PJSTAT(4),INTSTA                                                 
         MVI   PJSTAT+4,C'C'                                                    
         MVC   PJBTYPE,INTBTYP                                                  
         MVC   PJBOOK,INTBOOK                                                   
         MVC   PJEXTNUM+3(2),WORK                                               
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,HIGH             LK FOR THE PRG FOR BK ON THE FILE            
         CLC   KEYSAVE(PJINTNUM-PJKEY),KEY                                      
         BNE   GETFILX                                                          
         MVC   HALF,PJINTNUM       JKEY HAS FILE NUM IN REVERSE ORDER           
         XC    HALF,=X'FFFF'       ADJUST TO REAL VALUE                         
         MVC   WORK+2(2),HALF      2 BYTE FILE NUMBER RETURNED                  
         B     GETFILX                                                          
*                                                                               
GETFILX  B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*HIGH - DATAMGR CALL TO READ NTIDIR RECD INTO WREC                              
**********************************************************************          
HIGH     NTR1                                                                   
         L     R6,AWREC            READ WITH 'KEY' INTO WREC                    
         LA    R6,4(R6)                                                         
         XC    0(L'KEY,R6),0(R6)                                                
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI ',=C'NTIDIR',KEY,(R6)                    
         L     RE,AWREC                                                         
         LA    RE,4(RE)                                                         
         MVC   KEY,0(RE)                                                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CNVWR - SORT RECDS HOOK.  AFTER ALL IRECS BUILT DEMCNV COMES HERE             
* BEFORE IT GOES TO THE OUTPUT PHASE.  LAST CHANCE TO PROCESS BEFORE            
* OUPUT PHASE HANDLING.                                                         
*  - MERGE PRG NAME (SPECIAL 'P-RECDS) WITH (05) TIME PERD RECDS                
**********************************************************************          
CNVWR    DS    0H                                                               
         MVI   BYPSORT,0           RELEASE RECD                                 
         L     R2,ASREC                                                         
         CLI   CNV1ST,0            1ST TIME IN CNVWR ROUTINE                    
         BNE   CNVWR10                                                          
         XC    PRVKEY,PRVKEY                                                    
         MVC   SVPNAME,BLANKS                                                   
*                                                                               
CNVWR10  CLI   INTRTYP,C'P'        TIME PERIOD RECDS ONLY                       
         BNE   CNVWRX              RELEASE                                      
         LA    RE,INTKEY                                                        
         USING PRKEY,RE                                                         
         CLI   PRBTYP+4,X'00'      FUDGED TIME PER REC W/PRG NAME               
*        CLI   PRDW+1,X'00'                                                     
         BNE   CNVWR20             NO                                           
         MVI   BYPSORT,X'80'       DON'T RELEASE FUDGE RECD                     
*        CLC   INTKEY(PRDW-PRKEY+1),PRVKEY   SAME AS LAST KEY?                  
         CLC   INTKEY(PRSTIM-PRKEY),PRVKEY    SAME AS LAST KEY?                 
         BE    *+14                BYPASS                                       
         MVC   SVPNAME,INTPNAME    DIFF KEY: SAVE THIS PRG NAME                 
         B     CNVWRX                                                           
*                                                                               
*        CLI   PRDW,X'00'          M-F RECD?                                    
*        BE    *+12                                                             
*        CLI   PRDW,X'80'          M-S RECD?                                    
*        BNE   CNVWRX              FOR INDIV DAYS,JUST TAKE 1ST RECD            
*        CLC   SVPNAME,INTPNAME     SAME PRG NAME?                              
*        BE    CNVWRX              YES                                          
*        MVC   SVPNAME,BLANKS       DIFF PRGS AIRED FOR THIS QHR                
*        MVC   SVPNAME(7),=C'VARIOUS'                                           
*        B     CNVWRX                                                           
         DROP  RE                                                               
*                                                                               
CNVWR20  CLI   CNV1ST,0                                                         
         BE    CNVWRX                                                           
         CLC   INTKEY(PRBTYP-PRKEY+4),PRVKEY    MATCHING UP TO PRBTYP+3         
         BNE   CNVWRX              DIFF KEY FROM SAVED PRG NAME                 
         MVC   INTPNAME,SVPNAME    MOVE IN PRG NAME FOR THIS QHR                
*                                                                               
CNVWRX   L     RF,AWREC                                                         
         LR    RE,R2                                                            
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   PRVKEY,INTKEY                                                    
         MVI   CNV1ST,1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND ACTUAL RUN TIME AND AVERAGE DURATION                          
*              P1 =  A(SAVETIME)                                                
*              P2 =  A(DAY BITS FOR WEEK)                                       
***********************************************************************         
PDATA    NTR1                                                                   
         L     R6,0(R1)            ADDRESS OF SAVETIME                          
         L     RE,4(R1)            ADDRESS OF DAY BITS                          
         MVC   BYTE,0(RE)          DAY BITS                                     
         XC    FULL,FULL           CLEAR DAY COUNT FOR DURATION                 
         MVI   TMP,X'40'                                                        
         LA    R5,7                COUNTER                                      
         SR    R7,R7               CLEAR DURATION ACCUM                         
         XC    TIMTAB(7*L'TIMTAB),TIMTAB                                        
         XC    FULL1,FULL1         CLEAR TABLE ENTRY COUNT                      
*                                                                               
PDATA2   ZIC   RF,TMP                                                           
         EX    RF,*+8              TEST IF PROGRAM RAN ON DAY                   
         B     *+8                                                              
         TM    BYTE,0                                                           
         BZ    PDATA4              NO                                           
         OC    0(8,R6),0(R6)       BYPASS IF NO TIMES                           
         BZ    PDATA4                                                           
*                                                                               
         PACK  DUB,0(2,R6)         START TIME                                   
         CVB   R1,DUB                                                           
         MH    R1,=H'100'          HOURS X 100                                  
         PACK  DUB,2(2,R6)         START MINUTE                                 
         CVB   R0,DUB              MINUTES                                      
         AR    R1,R0                                                            
         CH    R1,=H'2400'         TEST FOR PAST MIDNIGHT                       
         BNH   *+8                 BEFORE OR AT MIDNIGHT                        
         SH    R1,=H'2400'         SUBTRACT FOR MIL TIME                        
         STH   R1,HALF             SAVE MIL TIME                                
         PACK  DUB,4(4,R6)         DURATION IN MINUTES                          
         CVB   R0,DUB                                                           
         AR    R7,R0               UPDATE TOTAL DURATION                        
         L     RE,FULL                                                          
         LA    RE,1(RE)            UPDATE DAY COUNT                             
         ST    RE,FULL                                                          
         BAS   RE,UPTIME           UPDATE START TIME TABLE                      
*                                                                               
PDATA4   ZIC   RF,TMP              GET DAY                                      
         SRL   RF,1                NEXT DAY                                     
         STC   RF,TMP                                                           
         LA    R6,L'SAVETIME(R6)   NEXT DAY'S TIME DATA                         
         BCT   R5,PDATA2                                                        
*                                                                               
PDATA6   SR    RE,RE               CALCULATE AVERAGE DURATION                   
         LR    RF,R7               DURATION                                     
         SLDA  RE,1                                                             
         D     RE,FULL                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         CH    RF,=H'3600'         60 HRS MAX.TO FIT #OF QH'S IN 1 BYTE         
         BNH   *+8                 (SHOULD NEVER REACH THIS HIGH!)              
         LH    RF,=H'3600'                                                      
         STCM  RF,3,PWK2DURM       2-BYTE DURATION                              
         CH    RF,=H'240'          4-HOUR MAXIMUM DURATION SUPPORTED            
         BNH   *+8                                                              
         LH    RF,=H'240'                                                       
         STC   RF,PWK1DURM         RETURN AVE DURATION                          
*                                  SORT TIMES IN DESCENDING ORDER               
         L     R0,FULL1            NUMBER OF ENTRIES IN TABLE                   
         GOTO1 VXSORT,DMCB,(X'FF',TIMTAB),(R0),3,1,2                            
         MVC   PWK1STIM,TIMTAB     FIRST OR MOST FREQUENT TIME                  
         B     EXIT                                                             
         SPACE 3                                                                
***********************************************************************         
* ROUTINE TO UPDATE RUN TIME TABLE                                              
*              HALF CONTAINS TIME                                               
*              FULL1 CONTAINS TABLE ENTRY COUNT                                 
***********************************************************************         
UPTIME   DS    0H                                                               
         LR    R0,RE                                                            
         LA    R1,7                COUNTER                                      
         LA    RE,TIMTAB                                                        
*                                                                               
UPTIME2  OC    0(L'TIMTAB,RE),0(RE)     TEST FOR E-O-T                          
         BZ    UPTIME4                  INSERT ENTRY                            
         CLC   HALF,0(RE)               TEST IF TIME IS IN TABLE                
         BE    *+12                     YES-BUMP FREQUENCY COUNT                
         LA    RE,L'TIMTAB(RE)                                                  
         BCT   R1,UPTIME2                                                       
*                                                                               
         ZIC   RF,2(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,2(RE)                                                         
         B     UPTIMEX                                                          
*                                                                               
UPTIME4  MVC   0(2,RE),HALF        ADD NEW ENTRY                                
         MVI   2(RE),1             SET FREQUENCY TO 1                           
         L     R1,FULL1                                                         
         LA    R1,1(R1)            INCREMENT TABLE ENTRY COUNT                  
         ST    R1,FULL1                                                         
*                                                                               
UPTIMEX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*HPTFILL- FILL IN INITIAL HPT TABLE ENTRY                                       
**********************************************************************          
HPTFILL  NTR1                                                                   
         SR    R7,R7                                                            
         USING HPTD,R7                                                          
         IC    R7,INTSQH                                                        
         SR    R6,R6                                                            
         MHI   R7,HPTDLN       LENGTH OF EACH QHR ENTRY                         
*                                                                               
HPTF10   A     R7,VHLFBUF          1/2 HOUR TABLE                               
         MVC   HPTHST5,INTSTA+4    SET HLF HR CHAR                              
*                                                                               
HPTF20   MVC   HPTSTN,INTSTA                                                    
         MVC   HPTREC,RELS                                                      
         MVC   HPTFEED,INTFEED                                                  
         MVC   HPTNAME,INTPNAME+4                                               
         MVC   HPTSQHR,INTSQH                                                   
         MVC   HPTEQHR,INTEQH                                                   
         MVC   HPTSTIM,INTSTIM                                                  
         MVC   HPTETIM,INTETIM                                                  
         CLI   INTDAYWK,X'60'      DON'T INCLUDE SAT IN M-F AVG                 
         BE    HPTMSAVG                                                         
         CLI   INTDAYWK,X'70'      DON'T INCLUDE SUN IN M-F AVG                 
         BE    HPTMSAVG                                                         
*                                                                               
HPTMFAVG DS    0H                                                               
         LA    R1,HPTMFWGT         M-F WGT                                      
         LA    R9,HPTMFDEM         M-F DEMOS                                    
         BAS   RE,HPTADD           ACCUMULATE DEMOS IN BUFFER                   
         ICM   RF,15,HPTMFWGT                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,HPTMFWGT                                                   
*                                                                               
HPTMSAVG DS    0H                                                               
         LA    R1,HPTMSWGT         M-S WGT                                      
         LA    R9,HPTMSDEM         MON-SUN AVG                                  
         BAS   RE,HPTADD           ACCUMULATE DEMOS IN BUFFER                   
         ICM   RF,15,HPTMSWGT                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,HPTMSWGT                                                   
*                                                                               
HPTFILLX B     XIT                                                              
         DROP  R7                                                               
         SPACE 3                                                                
**********************************************************************          
*HPTADD - R9=ACCUM VALUES IN HPT DEMO BUFFER                                    
*         R1->WGT BUCKET                                                        
**********************************************************************          
HPTADD   DS    0H                  R9=ACCUMULATOR AREA                          
         LA    R5,INTACCS                                                       
         LA    R0,NDEMS                                                         
*                                                                               
HPTADD5  XR    R6,R6                                                            
         OC    0(4,R1),0(R1)       ANYTHING PREVIOUSLY IN BUFFER?               
         BZ    *+8                                                              
         ICM   R6,15,0(R9)         ACCUM VALUE IN BUFFER                        
         ICM   RF,15,0(R5)         DEMO IN INTERIM RECD                         
         AR    R6,RF                                                            
         STCM  R6,15,0(R9)         NEW SUM                                      
         LA    R5,4(R5)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,HPTADD5                                                       
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*HPTRELS - RELEASE M-F AND M-SU AVG RECDS FOR EACH MARKET BREAK                 
**********************************************************************          
HPTRELS  DS    0H                                                               
         CLI   HPTFRST,1                                                        
         BNE   HPTR01                                                           
         MVI   HPTFRST,0                                                        
         MVI   QHR,0               WHICH QHR CURRENTLY PROCESSING               
         MVI   QHRFLG,0                                                         
*                                                                               
HPTR01   MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         XC    INTPNAME,INTPNAME                                                
*                                                                               
         USING HPTD,R7                                                          
HPTR05   DS    0H                                                               
         CLI   QHR,96                                                           
         BH    HPTRELX                                                          
HPTR10   CLI   QHRFLG,0            0: HLFHR BUFFER M-F RECD                     
         BNE   HPTR20                                                           
         ZIC   R7,QHR              QHR CURRENTLY PROCESSING                     
         SR    R6,R6                                                            
         M     R6,=A(HPTDLN)       OFFSET INTO BUFFER                           
         A     R7,VHLFBUF                                                       
         CLI   RELSLOT,0           -FIRST TIME THRU-                            
         BNE   HPTR15                                                           
         OC    HPTMFWGT,HPTMFWGT   BYPASS QHRFLG RELS IF NO DAYS TO AVG         
         BZ    HPTR80                                                           
         LA    R6,HPTMFWGT                                                      
         BAS   RE,HPTDIV           DIVIDE DEMOS BY WEIGHT (ALL MKTBRKS)         
*                                                                               
HPTR15   LA    R6,HPTMFDEM                                                      
         MVC   INTSTA+4(1),HPTHST5                                              
         MVI   INTDAYWK,X'00'      M-F                                          
         MVI   INTDYBIT,X'7C'                                                   
         MVC   INTPNAME(4),=C'M-F '                                             
         MVI   INTDUR,2                                                         
         MVI   INTADUR,30                                                       
         MVC   INTCOV,=H'30'                                                    
         B     HPTR21                                                           
*                                                                               
HPTR20   CLI   QHRFLG,1            1: HLFHR BUFFER M-SU RECD                    
         BE    *+6                                                              
         DC    H'0'                SOMETHING IS WRONG                           
         ZIC   R7,QHR              QHR CURRENTLY PROCESSING                     
         SR    R6,R6                                                            
         M     R6,=A(HPTDLN)       OFFSET INTO BUFFER                           
         A     R7,VHLFBUF                                                       
         CLI   RELSLOT,0           -FIRST TIME THRU-                            
         BNE   HPTR20A             AVERAGES ALREADY COMPUTED                    
         OC    HPTMSWGT,HPTMSWGT                                                
         BZ    HPTR80              BYPASS THIS QHRFLG RELS                      
         LA    R6,HPTMSWGT                                                      
         BAS   RE,HPTDIV                                                        
*                                                                               
HPTR20A  LA    R6,HPTMSDEM                                                      
         MVI   INTDAYWK,X'80'      M-S                                          
         MVI   INTDYBIT,X'7F'                                                   
         MVC   INTPNAME(4),=C'M-SU'                                             
         MVI   INTDUR,2                                                         
         MVI   INTADUR,30                                                       
         MVC   INTCOV,=H'30'                                                    
         MVC   INTSTA+4(1),HPTHST5                                              
         B     HPTR21                                                           
*                                                                               
HPTR21   L     RE,=A(CRCOTAB)                                                   
HPTR22   CLI   0(RE),X'FF'                                                      
         BNE   HPTR25             DON'T DO SUMMARIES- JUST END                  
         MVI   RELS,0              NOTHING TO RELEASE                           
         MVI   RELSLOT,0                                                        
*                                                                               
         MVI   MYMODE,C'H'                                                      
         SR    R1,R1               BUMP OUTPUT LEVEL FOR NXT RELEASE            
         IC    R1,QHRFLG                                                        
         LA    R1,1(R1)                                                         
         STC   R1,QHRFLG                                                        
         CLI   QHRFLG,2            DONE RELEASING RECDS FOR THIS QHR            
         BL    HPTR22A                                                          
         MVI   QHRFLG,0                                                         
         IC    R1,QHR              BUMP QHR CTR FOR NEXT TIME                   
         LA    R1,2(R1)            ONLY HALF HOUR RECORDS                       
         STC   R1,QHR                                                           
HPTR22A  B     HPTR05              START OVER                                   
*                                                                               
HPTR25   CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,L'CRCOTAB(RE)                                                 
         B     HPTR22                                                           
         CLI   3(RE),0                                                          
         BNE   HPTR28                                                           
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     HPTR21                                                           
*                                                                               
HPTR28   MVC   MKTBRK,3(RE)                                                     
         MVC   INTBTYP,4(RE)                                                    
*                                                                               
         LR    RE,R6                                                            
         LA    R1,INTACCS                                                       
         LH    RF,=Y(NDEMS*4)      RF=NUMBER BYTES TO MOVE                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),0(RE)      SAVE DEMOS IN INTACCS                        
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIUNVSQC)      R1=DESTN IN INTERIM REC FOR UNIVS           
         ZIC   RE,COVSMPD          GET UNIVS FOR COVG SAMPLE                    
         MH    RE,CVGLN            BUMP TO CVG IN UNVBUFF                       
         ZIC   RF,RELSLOT          WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS                                
         CLI   MKTBRK,COMGP        PRINCIPAL                                    
         BNE   *+10                                                             
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGF        FREQUENT                                     
         BNE   *+16                                                             
         MVC   8(48,R1),56(R1)                                                  
         XC    56(96,R1),56(R1)                                                 
         CLI   MKTBRK,COMGA        AVID                                         
         BNE   *+16                                                             
         MVC   8(48,R1),56+48(R1)                                               
         XC    56(96,R1),56(R1)                                                 
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         MVI   MYMODE,C'H'                                                      
*                                                                               
         MVI   INTRTYP,PRCODEQU    SET 'P' RECORD                               
         MVC   INTFEED,HPTFEED                                                  
         MVC   INTMRKT,TAPEMKT                                                  
         MVC   INTSQH,HPTSQHR                                                   
         MVC   INTEQH,HPTEQHR                                                   
         MVC   INTSTIM,HPTSTIM                                                  
         MVC   INTETIM,HPTETIM                                                  
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH                                              
         MVC   INTPNAME+4(8),HPTNAME                                            
*        MVI   INTBTYP,0                                                        
         CLC   INTSTA(4),=C'0000'  USA LEVEL IS HUT STATION                     
         BNE   *+10                                                             
         MVC   INTSTA(4),=C'HUT '                                               
         CLC   INTSTA(4),=C'HUT '                                               
         BNE   HPTR55                                                           
*        CLI   INTFEED,C'L'                                                     
*        BNE   *+8                                                              
*        MVI   INTBTYP,C'L'                                                     
*                                                                               
HPTR55   SR    RE,RE                                                            
         IC    RE,INTDUR                                                        
         MH    RE,=H'15'                                                        
         STC   RE,INTDURM          SET DURATION MINUTES                         
*                                                                               
HPTR60   DS    0H                                                               
         BAS   RE,BLDKEY                                                        
         B     RELEASE             RELEASE AVG REC TO SORT                      
*                                                                               
HPTR80   DS    0H                  BYPASS THIS DAY/QHR                          
         MVI   RELSLOT,0                                                        
         SR    R1,R1                                                            
         IC    R1,QHRFLG                                                        
         LA    R1,1(R1)                                                         
         STC   R1,QHRFLG                                                        
         CLI   QHRFLG,2                                                         
         BL    HPTR05              START FROM TOP                               
         MVI   QHRFLG,0                                                         
         IC    R1,QHR                                                           
         LA    R1,2(R1)                                                         
         STC   R1,QHR                                                           
         B     HPTR05              START FROM TOP                               
*                                                                               
HPTRELX  DS    0H                  DONE RELEASING SUMMARY'S FOR STN             
         MVI   HPTFRST,1           RESET FOR NEXT CVGS RELS                     
         MVI   MYMODE,0                                                         
         CLI   RECD,C'Z'           END OF FILE?                                 
         BE    ENDJOB                                                           
         B     READ40                                                           
         DROP  R7                                                               
         SPACE 3                                                                
**********************************************************************          
*HPTDIV  DIVIDE M-F OR M-S SUM BY WEIGHT                                        
*        R6 -> WEIGHT FOLLOWED BY DEMOS                                         
**********************************************************************          
HPTDIV   NTR1                                                                   
         LA    R0,NDEMS                                                         
         ICM   R1,15,0(R6)         PICK UP WEIGHT                               
         XC    0(4,R6),0(R6)       CLEAR WGT-INITS BUFFER FOR NEXT CVGS         
         LA    R6,4(R6)            PT TO START OF DEMOS                         
HPTDIV5  XR    RF,RF                                                            
         ICM   RE,15,0(R6)                                                      
         LTR   RE,RE                                                            
         BZ    *+10                PROTECT FROM DIVIDE BY 0                     
         SRDA  RE,31               MULTIPLY BY 2                                
         DR    RE,R1               DIVIDE BY WEIGHT FACTOR                      
         AHI   RF,1                ADD ONE TO ROUND                             
         SRA   RF,1                DIVIDE BY 2                                  
         STCM  RF,15,0(R6)                                                      
         LA    R6,4(R6)                                                         
         BCT   R0,HPTDIV5                                                       
HPTDIVX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
*                                                                               
DONE     DS    0H                                                               
         MVI   RECD,C'Z'           DONE                                         
         CLI   RELS,3              RELEASE LAST TVU RECD?                       
         BE    USGREL                                                           
         CLI   RELS,4              RELEASE LAST PRG RECD?                       
         BE    PRGREL              YES                                          
         CLI   RELS,5              RELEASE LAST STA GRP RECD?                   
         BE    USGREL                                                           
*        CLI   FRELS,0             RELEASE LAST FUDGED RECORD?                  
*        BE    ENDJOB                                                           
*        LA    RE,HHRTAB                                                        
*        ST    RE,INDEX                                                         
*        MVC   SVMODE,MYMODE                                                    
*        B     FUDGREL                                                          
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         EJECT                                                                  
* --------------------------------------------------------------------          
* LITERAL POOL                                                                  
* --------------------------------------------------------------------          
         LTORG                                                                  
         SPACE 2                                                                
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=760,                                              X        
               MACRF=GM,                                               X        
               EODAD=DONE                                                       
         EJECT                                                                  
**********************************************************************          
* ACOMWRK- COMMON WORK AREA BETWEEN INPUT AND OUTPUT PHASE                      
*          DO NOT MESS AROUND WITH ORDER OF VARIABLES--MAKE SURE                
*          OUTPUT PHASE AGREES WITH ANY CHANGES                                 
**********************************************************************          
COMWRK   DS    0H                                                               
AVGWKS   DC    X'00'               # WEEKS IN AVG: '00','01','04','05'          
STDATE   DS    CL6                 START DATE OF IREC                           
         SPACE 3                                                                
**********************************************************************          
* TABLE OF DAY BIT SETTINGS AND DAY CODES                                       
**********************************************************************          
NETDAYTB DS    0CL2                                                             
         DC    X'4010'             MON                                          
         DC    X'2020'             TUE                                          
         DC    X'1030'             WED                                          
         DC    X'0840'             THU                                          
         DC    X'0450'             FRI                                          
         DC    X'0260'             SAT                                          
         DC    X'0170'             SUN                                          
         DC    X'7C00'             M-F                                          
         DC    X'7F80'             M-SUN                                        
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                          
         SPACE 3                                                                
**********************************************************************          
*DAYTAB  - INPUT TAPE DAY CODE TO INTERIM DAY CODE                              
**********************************************************************          
DAYTAB   DS    0CL7                       DAY CONVERSION TABLE                  
         DC    C'1',X'40',X'10',CL4'MON'                                        
         DC    C'2',X'20',X'20',CL4'TUE'                                        
         DC    C'3',X'10',X'30',CL4'WED'                                        
         DC    C'4',X'08',X'40',CL4'THU'                                        
         DC    C'5',X'04',X'50',CL4'FRI'                                        
         DC    C'6',X'02',X'60',CL4'SAT'                                        
         DC    C'7',X'01',X'70',CL4'SUN'                                        
         DC    C'8',X'7C',X'00',CL4'M-F'                                        
         DC    C'9',X'7F',X'80',CL4'M-SU'                                       
         DC    X'FF',X'80',X'90',CL4'VAR'                                       
         SPACE 3                                                                
**********************************************************************          
* TABLE OF DAY CODES AND SORT VALUES FOR 'Q' RECORDS                            
**********************************************************************          
QSORTAB  DS    0XL2                                                             
         DC    X'8000'             M-S                                          
         DC    X'0001'             M-F                                          
         DC    X'9002'             VAR                                          
         DC    X'1003'             MON                                          
         DC    X'2004'             TUE                                          
         DC    X'3005'             WED                                          
         DC    X'4006'             THU                                          
         DC    X'5007'             FRI                                          
         DC    X'6008'             SAT                                          
         DC    X'7009'             SUN                                          
QSORTABS EQU   (*-QSORTAB)/L'QSORTAB                                            
         SPACE 3                                                                
**********************************************************************          
* TABLE OF TELECAST TYPES AND THEIR BIT SETTINGS USED FOR INTDTYP               
**********************************************************************          
TYPTAB   DS    0CL3                                                             
         DC    C'  ',X'09'         REGULAR - ORIGINAL (PROTECT)                 
         DC    C'0 ',X'09'         REGULAR - ORIGINAL                           
         DC    C' Y',X'11'         REGULAR - REPEAT   (PROTECT)                 
         DC    C'0Y',X'11'         REGULAR - REPEAT                             
         DC    C'1 ',X'0C'         SPECIAL - ORIGINAL                           
         DC    C'1Y',X'14'         SPECIAL - REPEAT                             
         DC    C'2 ',X'14'         SPECIAL - REPEAT                             
         DC    C'2Y',X'14'         SPECIAL - REPEAT                             
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
*                                                                               
***********************************************************************         
*                    WORKING STORAGE                                            
**********************************************************************          
         DS    0D                                                               
PACK8    DS    PL8                                                              
PACK16   DS    PL16                                                             
*                                                                               
LSTCODE  DS    CL(L'QCODE)                                                      
DAY      DS    CL6                                                              
WKDAYS   DS    CL7                                                              
INDEX    DS    A                   ADDRESS INTO TABLE                           
MKTBRK   DS    X                   MKT BREAK                                    
MKTBMIT  DS    H                   MKT BREAK VALUE FROM MIT TAPE                
BYTE     DS    C                                                                
DAYBIT   DS    X                   STORE DAY OF WEEK (1 BIT)                    
TMP      DS    C                                                                
LEVEL    DS    X                                                                
WKBOOK   DS    CL2                 SAVED NETWEEK BOOK                           
WKIBOOK  DS    CL2                 SAVED NETWEEK IBOOK                          
WKDAYWK  DS    CL1                 SAVED INTDAYWK                               
TAPEMKT  DS    H                   CABLE MOVIE GOER MKT=320                     
TAPEWK   DS    CL6                                                              
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
VARIOUS  DS    X                   1=VARIOUS                                    
SAVVAR   DS    7XL7                                                             
VARSTIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND MIDDUR            
SAVETIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND MIDDUR            
PHUT     DS    CL9                                                              
TIMTAB   DS    7CL3                                                             
BOOK     DS    XL2                 KEY BOOK                                     
IBOOK    DS    XL2                 INTERNAL BOOK                                
BOOKS    DS    XL2                 KEY BOOK (SAVE AREA)                         
IBOOKS   DS    XL2                 INTERNAL BOOK (SAVE AREA)                    
USHUT    DS    F                   HALF HOUR US HUT PROJ (XXX,XXX,XXX)          
MKTHUT   DS    F                   MKT BREAK HUT  PROJ   (XXX,XXX,XXX)          
SVAVGHI  DS    F                   SAVE PROGRAM HH                              
SVQHR    DS    X                                                                
SVR7     DS    A                                                                
APACKWT  DS    A                   A(PACKWT) AREA                               
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
FDENTRY  DS    XL(PNAMEDLQ)                                                     
*                                                                               
MFHNAME  DC    XL(L'INTPNAME)'00'      MOST SPECIFIC PROG NAME FOR HH           
MFPNAME  DC    XL(L'INTPNAME)'00'      SAVED NAME FROM PROGRAM AVERAGE          
MSHNAME  DC    XL(L'INTPNAME)'00'      MOST SPECIFIC PROG NAME FOR HH           
MSPNAME  DC    XL(L'INTPNAME)'00'      SAVED NAME FROM PROGRAM AVERAGE          
FDCOUNT  DC    X'00'                                                            
PRVPNUM  DC    XL(L'INTPNUM)'00'                                                
PRVNET   DC    XL(L'INTANET)'00'                                                
SVSEQ    DC    CL2' '                                                           
SVNET    DC    CL6' '                                                           
SVCVG    DC    CL6' '                                                           
PRVMKBK  DC    X'00'                                                            
CNV1ST   DC    X'00'                                                            
CURRDUR  DC    H'00'                                                            
MYMODE   DC    X'00'                                                            
SVMODE   DC    X'00'                                                            
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
RECD     DC    X'00'               TYPE OF RECD JUST READ FROM TAPE             
RELSLOT  DC    X'00'               RELEASE SLOT                                 
SUMSLOT  DC    X'00'               SUMMARY SLOT                                 
SVPNAME  DS    CL25                PRG NAME                                     
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
BLANKS   DC    256C' '                                                          
*                                                                               
KEYSRC   DS    C                                                                
*                                                                               
         DS    0H                                                               
CVGLN    DC    AL2(NDEMS*NMKTS*4)  SIZE OF CVG IN UNV BUFFER (#BYTES)           
COVALPH  DS    CL4                                                              
COVSMP   DS    CL4                                                              
COVSMPD  DS    X                                                                
RELS     DC    X'00'                                                            
FRELS    DC    X'00'                                                            
PRVKEY   DS    CL(L'MITKEY)        PREVIOUS RECD'S KEY                          
PRVMKT   DS    CL(L'MITMKTBR)      PREVIOUS MKT BREAK                           
PRVHLF   DS    CL(L'MITHLFID)      PREVIOUS HLF HOUR ID                         
*RVQTR   DS    CL(L'MITQTRID)      PREVIOUS QTR HOUR ID                         
*                                                                               
HPTSTN   DC    XL5'00'             STN ACCUM AVGS FOR                           
HPTQST5  DS    CL1                 QHR INTSTA+4 CHAR                            
HPTHST5  DS    CL1                 HALF HOUR INTSTA+4 CHAR                      
HPTREC   DC    XL1'00'             3 OR 5                                       
HPTFEED  DC    XL1'00'             FEED PATTERN FOR  03'S                       
HPTFRST  DC    XL1'00'             1ST TIME IN FOR CVG SAMPLE                   
QHR      DC    XL1'00'             QHR PROCESSING                               
SQHR     DC    XL1'00'             START QHR                                    
EQHR     DC    XL1'00'             END QHR                                      
QHRFLG   DC    XL1'00'             WHICH RECD GETS RELSD M-F/QHR..              
*                                                                               
KEY      DS    XL23                                                             
KEYSAVE  DS    XL23                                                             
SVKEY    DS    XL40                                                             
COMMAND  DS    CL7                                                              
*                                                                               
*-----------------------------                                                  
* SAVE INTERIM RECD KEY                                                         
*-----------------------------                                                  
         DS    0F                                                               
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD KEY                      
PRVINT   DS    CL(INTACCS-INTVALS)                                              
SAVENET  DS    CL3                 SAVE NETWORK                                 
SAVEPNUM DS    CL10                SAVE ENTIRE PROGRAM NUMBER                   
SAVESTYP DS    C                   SAVE AUDIENCE EST TYPE                       
SAVESYND DS    CL3                 SAVE SYNDICATOR                              
*                                                                               
         DS    0F                                                               
         DC    C'*STSREC*'                                                      
SAVERREC DS    CL150                                                            
         DC    C'*ENDREC*'                                                      
*                                                                               
*-----------------------------                                                  
* PROGRAM DESCRIPTOR SAVE AREA                                                  
*-----------------------------                                                  
         DS    0F                                                               
PVALS    DS    0CL85                                                            
PNUM     DS    XL2                 PROGRAM NUMBER                               
PNET     DS    CL4                 NETWORK                                      
PDPT     DS    C                   NSI DAYPART (ALWAYS ZERO)                    
PDPT2    DS    CL2                 NTI DAYPART                                  
PTYP     DS    CL2                 PROGRAM TYPE                                 
PPREM    DS    C                   PREMIERE CODE                                
PSHORT   DS    C                   SHORT DURATION INDICATOR                     
PNAME    DS    CL25                PROGRAM NAME                                 
PTITLE   DS    CL32                EPISODE TITLE                                
*                                                                               
PWKBIT   DS    X                   WEEK BITS                                    
PDAY1    DS    X                   DAY CODE                                     
PDAY1BIT DS    X                   DAY BITS                                     
PTOTDUR  DS    XL2                 TOTAL DURATION                               
*                                                                               
PWK1STIM DS    XL2                 START TIME                                   
PWK1DURM DS    X                   DURATION IN MINUTES                          
PWK2DURM DS    XL2                 DURATION IN MINUTES (LONG=2 BYTES)           
PWK1AUD  DS    XL2                                                              
PWK1STAC DS    XL2                                                              
PWK1COV  DS    XL2                                                              
PWK1DTYP DS    X                                                                
PWK1RSF  DS    X                                                                
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
*UTDISP  DC    AL2(NDEMS*4)        DISP TO DEMO PUTS                            
*KTDISP  DC    AL2(NDEMS*4*2)      DISP TO NEXT MKT BRK DEMOS                   
MKTDISP  DC    AL2(NDEMS*4)        DISP TO NEXT MKT BRK DEMOS                   
SAVEKEY  DS    CL(L'INTKEY)        KEY OF ACCUMULATED DEMOS                     
         DS    0F                                                               
SAVEREC  DS    XL(NDEMS*4*2)       4BYTE DEMOS + UNIVS                          
*                                                                               
         DC    C'***HHRTAB***'                                                  
HHRTAB   DC    (56*7)XL(PNAMEDLQ)'00'   56 HLF HRS X 7 DAYS                     
         DC    X'FFFF'                    56=(24H/DAY+4EXTRA)*2                 
         DC    C'***PNAMTAB***'               4H=MAX LENGTH OF PROG             
PNAMTAB  DC    900XL(PNAMEDLQ)'00'                 SUPPORTED                    
         DC    X'FFFF'                                                          
*-------------------------------------------------------------------            
         DS    0F                                                               
UNVBUFF  DS    (NDEMS*2*NMKTS*NCBLS)XL4    DEMO UNIVS                           
*-------------------------------------------------------------------            
         DC    CL24'***DEMAREA****DEMAREA***'                                   
         DS    0F                                                               
DEMAREA  DS    (NDEMS*3*NMKTS)XL4    DEMOS (LAST PRG HAD 80 MKT BRKS)           
DEMLNQ   EQU   *-DEMAREA                                                        
PACKWT   DS    (NDEMS*3)PL8    (DEMOS+UNIVS+PUTS) WGHTING FACTOR PCKED          
*                                                                               
         EJECT                                                                  
MSU      EQU   8                   BIT MASK                                     
MFR      EQU   0                   BIT MASK                                     
NMKTS    EQU   3                                                                
NCBLS    EQU   150                 NUMBER OF CABLE NETS                         
         EJECT                                                                  
**********************************************************************          
* RT..=  INPUT TAPE  DEMO BUCKET DEFINITIONS                                    
**********************************************************************          
*DEMO        BUCKET                                                             
*-----       ------                                                             
* FIRST SET OF DEMOS (IF POS. 92-94 = 061)                                      
RTPF0211 EQU   1               PRINCIPAL MOVIE GOER FEMALES  2-11               
RTPF1217 EQU   2               PRINCIPAL MOVIE GOER FEMALES  12-17              
RTPF1824 EQU   3               PRINCIPAL MOVIE GOER FEMALES  18-24              
RTPF2534 EQU   4               PRINCIPAL MOVIE GOER FEMALES  25-34              
RTPF3549 EQU   5               PRINCIPAL MOVIE GOER FEMALES  35-49              
RTPF50   EQU   6               PRINCIPAL MOVIE GOER FEMALES  50+                
RTFF0211 EQU   7               FREQUENT MOVIE GOER FEMALES 2-11                 
RTFF1217 EQU   8               FREQUENT MOVIE GOER FEMALES 12-17                
RTFF1824 EQU   9               FREQUENT MOVIE GOER FEMALES 18-24                
RTFF2534 EQU   10              FREQUENT MOVIE GOER FEMALES 25-34                
RTFF3549 EQU   11              FREQUENT MOVIE GOER FEMALES 35-49                
RTFF50   EQU   12              FREQUENT MOVIE GOER FEMALES 50+                  
RTAF0211 EQU   13              AVID MOVIE GOER FEMALES 2-11                     
RTAF1217 EQU   14              AVID MOVIE GOER FEMALES 12-17                    
RTAF1824 EQU   15              AVID MOVIE GOER FEMALES 18-24                    
RTAF2534 EQU   16              AVID MOVIE GOER FEMALES 25-34                    
RTAF3549 EQU   17              AVID MOVIE GOER FEMALES 35-49                    
RTAF50   EQU   18              AVID MOVIE GOER FEMALES 50+                      
*OPEN    EQU   19              OPEN - ZERO FILLED                               
*OPEN    EQU   20              OPEN - ZERO FILLED                               
*                                                                               
* SECOND SET OF DEMOS (IF POS. 92-94 = 081)                                     
RTPM0211 EQU   21              PRINCIPAL MOVIE GOER MALES 2-11                  
RTPM1217 EQU   22              PRINCIPAL MOVIE GOER MALES 12-17                 
RTPM1824 EQU   23              PRINCIPAL MOVIE GOER MALES 18-24                 
RTPM2534 EQU   24              PRINCIPAL MOVIE GOER MALES 25-34                 
RTPM3549 EQU   25              PRINCIPAL MOVIE GOER MALES 35-49                 
RTPM50   EQU   26              PRINCIPAL MOVIE GOER MALES 50+                   
RTFM0211 EQU   27              FREQUENT MOVIE GOER MALES 2-11                   
RTFM1217 EQU   28              FREQUENT MOVIE GOER MALES 12-17                  
RTFM1824 EQU   29              FREQUENT MOVIE GOER MALES 18-24                  
RTFM2534 EQU   30              FREQUENT MOVIE GOER MALES 25-34                  
RTFM3549 EQU   31              FREQUENT MOVIE GOER MALES 35-49                  
RTFM50   EQU   32              FREQUENT MOVIE GOER MALES 50+                    
RTAM0211 EQU   33              AVID MOVIE GOER MALES 2-11                       
RTAM1217 EQU   34              AVID MOVIE GOER MALES 12-17                      
RTAM1824 EQU   35              AVID MOVIE GOER MALES 18-24                      
RTAM2534 EQU   36              AVID MOVIE GOER MALES 25-34                      
RTAM3549 EQU   37              AVID MOVIE GOER MALES 35-49                      
RTAM50   EQU   38              AVID MOVIE GOER MALES 50+                        
*OPEN    EQU   39              OPEN - ZERO FILLED                               
*OPEN    EQU   40              OPEN - ZERO FILLED                               
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* EQUATE DEMO SLOT NUMBERS                                                      
* ?T =  TOTAL SAMPLE RECORDS                                                    
* ?I =  INTERIM RECD SLOTS FOR DEMOS  (ALMOST THE SAME AS OUTPUT)               
**********************************************************************          
RIUSA    EQU   0                                                                
RIHOMES  EQU   1                                                                
PIF211   EQU   2                  PW2-11                                        
PIF1217  EQU   3                  PW12-17                                       
PIF1824  EQU   4                  PW18-24                                       
PIF2534  EQU   5                  PW25-34                                       
PIF3549  EQU   6                  PW35-49                                       
PIF50O   EQU   7                  PW50+                                         
PIM211   EQU   8                  PM2-11                                        
PIM1217  EQU   9                  PM12-17                                       
PIM1824  EQU   10                 PM18-24                                       
PIM2534  EQU   11                 PM25-34                                       
PIM3549  EQU   12                 PM35-49                                       
PIM50O   EQU   13                 PM50+                                         
*                                                                               
FIF211   EQU   14                 FW2-11                                        
FIF1217  EQU   15                 FW12-17                                       
FIF1824  EQU   16                 FW18-24                                       
FIF2534  EQU   17                 FW25-34                                       
FIF3549  EQU   18                 FW35-49                                       
FIF50O   EQU   19                 FW50+                                         
FIM211   EQU   20                 FM2-11                                        
FIM1217  EQU   21                 FM12-17                                       
FIM1824  EQU   22                 FM18-24                                       
FIM2534  EQU   23                 FM25-34                                       
FIM3549  EQU   24                 FM35-49                                       
FIM50O   EQU   25                 FM50+                                         
*                                                                               
AIF211   EQU   26                 AW2-11                                        
AIF1217  EQU   27                 AW12-17                                       
AIF1824  EQU   28                 AW18-24                                       
AIF2534  EQU   29                 AW25-34                                       
AIF3549  EQU   30                 AW35-49                                       
AIF50O   EQU   31                 AW50+                                         
AIM211   EQU   32                 AM2-11                                        
AIM1217  EQU   33                 AM12-17                                       
AIM1824  EQU   34                 AM18-24                                       
AIM2534  EQU   35                 AM25-34                                       
AIM3549  EQU   36                 AM35-49                                       
AIM50O   EQU   37                 AM50+                                         
*                                                                               
**********************************************************************          
* ?T..=  INPUT TAPE DISPLACEMENTS FOR  --TOTAL SAMPLE--  DEMOS                  
**********************************************************************          
*                                                                               
PTF211   EQU   1                 PW2-11                                         
PTF1217  EQU   2                 PW12-17                                        
PTF1824  EQU   3                 PW18-24                                        
PTF2534  EQU   4                 PW25-34                                        
PTF3549  EQU   5                 PW35-49                                        
PTF50O   EQU   6                 PW50+                                          
PTM211   EQU   21                 PM2-11                                        
PTM1217  EQU   22                 PM12-17                                       
PTM1824  EQU   23                 PM18-24                                       
PTM2534  EQU   24                 PM25-34                                       
PTM3549  EQU   25                 PM35-49                                       
PTM50O   EQU   26                 PM50+                                         
*                                                                               
FTF211   EQU   7                 FW2-11                                         
FTF1217  EQU   8                 FW12-17                                        
FTF1824  EQU   9                 FW18-24                                        
FTF2534  EQU   10                FW25-34                                        
FTF3549  EQU   11                FW35-49                                        
FTF50O   EQU   12                FW50+                                          
FTM211   EQU   27                 FM2-11                                        
FTM1217  EQU   28                 FM12-17                                       
FTM1824  EQU   29                 FM18-24                                       
FTM2534  EQU   30                 FM25-34                                       
FTM3549  EQU   31                 FM35-49                                       
FTM50O   EQU   32                 FM50+                                         
*                                                                               
ATF211   EQU   13                 AW2-11                                        
ATF1217  EQU   14                 AW12-17                                       
ATF1824  EQU   15                 AW18-24                                       
ATF2534  EQU   16                 AW25-34                                       
ATF3549  EQU   17                 AW35-49                                       
ATF50O   EQU   18                 AW50+                                         
ATM211   EQU   33                 AM2-11                                        
ATM1217  EQU   34                 AM12-17                                       
ATM1824  EQU   35                 AM18-24                                       
ATM2534  EQU   36                 AM25-34                                       
ATM3549  EQU   37                 AM35-49                                       
ATM50O   EQU   38                 AM50+                                         
         EJECT                                                                  
*                                                                               
*DISPS TO PUTS AND UNIVERSES IN DEMO DISP TABLE                                 
*                                                                               
NDEMS    EQU   38                  TOTAL NUMBER DEMOS ON OUTPUT FILE            
RIUNVSQ  EQU   (14+44)*4           DISPLACEMENT TO TOTAL US UNVS                
RIUNVSQC EQU   (14+44+14+14)*4     DISPLACEMENT TO COVG UNVS - FROM             
*                                                             DEMDISP           
*                                                                               
         EJECT                                                                  
**********************************************************************          
*SLOTTAB-TRANSLATE INPUT DEMOS TO INTERIM RECORD SLOTS:                         
*        DEMS STORED AT: INTACCS + 0                                            
*        UNVS STORED AT: INTACCS + RIUNVSQC                                     
**********************************************************************          
SLOTTAB  DS    0H                                                               
         DC    AL2(PTF211,PIF211)                                               
         DC    AL2(PTM211,PIM211)                                               
         DC    AL2(PTF1217,PIF1217)                                             
         DC    AL2(PTM1217,PIM1217)                                             
         DC    AL2(PTF1824,PIF1824)                                             
         DC    AL2(PTM1824,PIM1824)                                             
         DC    AL2(PTF2534,PIF2534)                                             
         DC    AL2(PTM2534,PIM2534)                                             
         DC    AL2(PTF3549,PIF3549)                                             
         DC    AL2(PTM3549,PIM3549)                                             
         DC    AL2(PTF50O,PIF50O)                                               
         DC    AL2(PTM50O,PIM50O)                                               
*                                                                               
         DC    AL2(FTF211,FIF211)                                               
         DC    AL2(FTM211,FIM211)                                               
         DC    AL2(FTF1217,FIF1217)                                             
         DC    AL2(FTM1217,FIM1217)                                             
         DC    AL2(FTF1824,FIF1824)                                             
         DC    AL2(FTM1824,FIM1824)                                             
         DC    AL2(FTF2534,FIF2534)                                             
         DC    AL2(FTM2534,FIM2534)                                             
         DC    AL2(FTF3549,FIF3549)                                             
         DC    AL2(FTM3549,FIM3549)                                             
         DC    AL2(FTF50O,FIF50O)                                               
         DC    AL2(FTM50O,FIM50O)                                               
*                                                                               
         DC    AL2(ATF211,AIF211)                                               
         DC    AL2(ATM211,AIM211)                                               
         DC    AL2(ATF1217,AIF1217)                                             
         DC    AL2(ATM1217,AIM1217)                                             
         DC    AL2(ATF1824,AIF1824)                                             
         DC    AL2(ATM1824,AIM1824)                                             
         DC    AL2(ATF2534,AIF2534)                                             
         DC    AL2(ATM2534,AIM2534)                                             
         DC    AL2(ATF3549,AIF3549)                                             
         DC    AL2(ATM3549,AIM3549)                                             
         DC    AL2(ATF50O,AIF50O)                                               
         DC    AL2(ATM50O,AIM50O)                                               
*                                                                               
         DC    X'FFFF',X'FFFF'                                                  
         EJECT                                                                  
***********************************************************************         
* MKT BREAK TABLE SLOTS EQUATES                                                 
***********************************************************************         
***!!!!!!!!!! WARNING !!!!!!!!                                                  
*ANY CHANGES TO THE TABLES OR EQUATES MUST BE APPLIED TO OPHASE TOO!            
*--------------------------------------------------------------------           
* TABLE OF INTERIM AREA SLOTS FOR MARKET BREAKS                                 
*--------------------------------------------------------------------           
*                                 MOVIEGOER                                     
CRMGP    EQU   0                    PRINCIPAL                                   
CRMGF    EQU   1                    FREQUENT                                    
CRMGA    EQU   2                    AVID                                        
         SPACE 2                                                                
*                                                                               
*--------------------------------------------------------------------           
* TABLE OF INTERIM AREA SLOTS FOR MARKET BREAKS                                 
*--------------------------------------------------------------------           
*                                 MOVIE GOERS                                   
CIMGP    EQU   0                    PRINCIPAL                                   
CIMGF    EQU   1                    FREQUENT                                    
CIMGA    EQU   2                    AVID                                        
         SPACE 2                                                                
*                                                                               
*--------------------------------------------------------------------           
* TABLE OF OUTPUT SECTIONS FOR MARKET BREAKS                                    
*--------------------------------------------------------------------           
*                                 MOVIE GOERS                                   
COMGP    EQU   167                PRINCIPAL                                     
COMGF    EQU   168                  FREQUENT                                    
COMGA    EQU   169                  AVID                                        
         SPACE 2                                                                
*--------------------------------------------------------------------           
* CRCITAB -    TABLE TO CONVERT MARKET BREAKS TO INTERNAL SLOTS                 
*--------------------------------------------------------------------           
CRCITAB  DS    0XL4                                                             
***!!!!!!!!!! WARNING !!!!!!!!                                                  
*ANY CHANGES TO THIS TABLE OR EQUATES MUST BE APPLIED TO OPHASE TOO!            
         DS    0H                                                               
*                                 MOVIE GOER                                    
         DC    AL2(CRMGP,CIMGP)    PRINCIPAL                                    
         DC    AL2(CRMGF,CIMGF)    FREQUENT                                     
         DC    AL2(CRMGA,CIMGA)    AVID                                         
         DC    X'FFFF',X'FFFF'                                                  
         SPACE 2                                                                
*--------------------------------------------------------------------           
* CRCOTAB -     TABLE TO CONVERT MARKET BREAKS TO OUTPUT SECTIONS               
*--------------------------------------------------------------------           
CRCOTAB  DS    0XL5                                                             
***!!!!!!!!!! WARNING !!!!!!!!                                                  
*ANY CHANGES TO THIS TABLE OR EQUATES MUST BE APPLIED TO OPHASE TOO!            
         DS    0H                                                               
*                                 MOVIE GOER                                    
         DC    AL2(CIMGP,COMGP),C'M'   PRINCIPAL                                
         DC    AL2(CIMGF,COMGF),C'M'   FREQUENT                                 
         DC    AL2(CIMGA,COMGA),C'M'   AVID                                     
         DC    X'FFFF',X'FFFF',X'FF'                                            
         SPACE 2                                                                
***********************************************************************         
* CVGTBL  -    CABLE NET CODES AND THEIR COVERAGE SAMPLE CODES                  
***********************************************************************         
CVGTBL   DC    (NCBLS)XL4'0000'         4 CHAR COVERAGE SAMPLE SAVED            
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
HPTD     DSECT                                                                  
HPTNAME  DS    CL8                 INTPNAME+4                                   
HPTSQHR  DS    X                   INTSQH PROCESSED                             
HPTEQHR  DS    X                   INTEQH PROCESSED                             
HPTSTIM  DS    XL2                 INTSTIM                                      
HPTETIM  DS    XL2                 INTETIM                                      
HPTMFWGT DS    XL4                 M-F # RECORDS ACCRUED                        
HPTMFDEM DS    (NDEMS)CL4        M-F INTACCS DEMOS                              
HPTMSWGT DS    XL4                 M-SU NUMBER RECDS ACCRUED                    
HPTMSDEM DS    (NDEMS)CL4        M-SU INTACCS DEMOS                             
HPTDLN   EQU   *-HPTD                                                           
*                                                                               
PNAMED   DSECT                    DSECT TO COVER PROG NAME TABLE                
PNQHR    DS    XL1                QTR HR (ALWAYS EVEN.ONLY HLF HR DATA)         
PNDAY    DS    XL1                DAY OF WEEK. M=X'10',TU=X'20'...              
PNTYPE   DS    CL1                P=PROG AVG, T=TRACKAGE OR TELECAST            
PNHNAME  DS    CL(L'INTPNAME)     MOST SPECIFIC NAME FOR THE HALF HR            
PNPNAME  DS    CL(L'INTPNAME)     SAVED PROGRAM AVERAGE NAME                    
PNAMEDLQ EQU   *-PNAMED                                                         
*                                                                               
         EJECT                                                                  
*        CABLE MOVIEGOER MIT DSECT                                              
       ++INCLUDE DEMITCMD                                                       
         SPACE 1                                                                
*        DEINTD                                                                 
         PRINT GEN                                                              
       ++INCLUDE DEINTD                                                         
         SPACE 1                                                                
*        DEINTMITD                                                              
       ++INCLUDE DEINTMI2D                                                      
         PRINT NOGEN                                                            
         EJECT                                                                  
*        DECALVPHD                                                              
       ++INCLUDE DECALVPHD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*        DEDEMFILE                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMCNVD                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DDCOMFACS                                                              
        PRINT OFF                                                               
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        DEDEMTABD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
*        DDMONYREQU                                                             
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111DECNMGI   06/16/20'                                      
         END                                                                    
