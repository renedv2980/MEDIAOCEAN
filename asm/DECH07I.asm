*          DATA SET DECH07I    AT LEVEL 105 AS OF 03/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECH07IA                                                                 
         TITLE '- DEMO CONVERSION - HISPANIC CABLE MIT'                         
*                                                                               
**********************************************************************          
*  !!!  THIS VERSION IS TO BE USED FOR DATA STARTING FROM 8/27/07 !!!           
*  !!!  FOR DATA PRIOR TO 8/27/07, PLEASE USE DECH0002 & DECNVCHN !!!           
**********************************************************************          
*                                                                               
* INPUT-OUTPUT LOGIC:                                                           
* ------------------                                                            
* '00' - EXTENDED CABLE MIT                                                     
* '01' - UNIVERSES.  ONE FOR EACH CABLE NET            H-P0-P0                  
* '03' - HUTS. ONE FOR EACH CABLE NET.                 H-P2-P2                  
*             P2 ->  PCN HUT*C  '*'= UNIQUE FOR EACH CABLE NET                  
* '04' - PROGRAM. PRG AUDIENCES (Q'S) AND PRG PUTS     H-P0-P0-P2-P2            
*             P0 ->  QCN PRG RECDS AUDIENCE DEMOS                               
*             P2 ->  APPEND PUTS ON Q PRG RECD AFTER AUD AND UNIVS              
* '05' - USAGE RECDS FOR EACH CABLE NET.               H-P0-P0                  
*             P2 ->  PNN NET-C   TIME PERIOD USAGE VIEWG TO CABLE NET           
*            NOTE:  IGNORE '05'-P2 RECORDS WHICH HAVE NO H'S SINCE              
*                   THEY APPEAR TO BE DUPLICATES OF '03'-P2'S                   
*        ---------------------------------                                      
* REGISTERS:                                                                    
* ---------                                                                     
*        R2  - AIREC (INTERD)                                                   
*        R8  - DEMCOND GLOBAL WORKING STORAGE                                   
*        R3  - WORK                                                             
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
DECH07I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DECH07I,R4,RA                                                  
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC = INPUT RECD FROM TAPE (MITREC)           
         USING MITD,RC                                                          
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
         CLI   MYMODE,C'H'         HALF & QHR M-F & M-S RECDS RELS              
         BE    HPTRELS                                                          
         CLI   MYMODE,C'P'         PROGRAM RECD RELEASE                         
         BE    PRGREL                                                           
         CLI   MYMODE,C'N'         PROG NAME RECD FOR TIME PERDS                
         BE    PRGTIME                                                          
*                                                                               
READ25   L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'1120'    HEADER                                       
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
*                                                                               
READ41   ICM   RF,15,ACOMFACS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,VWTYPTTB  GET A(VIEWING TYPES DESCRIPTION)             
         ICM   R5,15,DMCB          A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD TABLEID PASSED                           
         USING VWTYPTD,R5                                                       
READ41A  CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                VIEWING TYPE NOT FOUND                       
         CLC   MIVWTYP,VWTYPE                                                   
         BE    *+12                                                             
         LA    R5,VWTYPTL(R5)                                                   
         B     READ41A                                                          
         MVC   KEYSRC,VWTKSRC      SAVE KEY SOURCE HERE                         
*                                                                               
         CLC   MITTYPE,SVNET       PRINT OUT FIRST NET CODE/NET #               
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
*                                                                               
USG_TVU  DS    0H                  CABLE TVU TO GEOGRAPHIC MARKET               
         CLI   RELS,1              RELEASE LAST UNIVERSE RECD?                  
         BE    UNVREL                                                           
         CLC   MITSEQ,=C'03'       USAGE RECORDS?                               
         BNE   PROGRAM                                                          
         MVI   RECD,C'3'           THIS IS A USAGE RECD                         
         CLC   MITTYPE(3),=C'TVU'  VERIFY RECD TYPE=TVU  USAGE RECD             
         BE    TVUREC              HOUSEHOLD USAGE RECD                         
         DC    H'0'                                                             
*                                                                               
PROGRAM  DS    0H                                                               
         CLC   MITSEQ,=C'04'       PROGRAM RECORDS?                             
         BNE   USG_BAS             NO                                           
         MVI   RECD,C'4'           THIS IS A PROGRAM RECD                       
         CLI   RELS,3              RELEASE LAST USAGE RECD?                     
         BE    USGREL                                                           
         B     PRGREC              PROCESS PRG RECD                             
*                                                                               
USG_BAS  CLC   MITSEQ,=C'05'                                                    
         BNE   PERMUSG                                                          
         MVI   RECD,C'5'           THIS IS A USG-BAS RECD                       
         CLI   RELS,4              RELEASE LAST PRG RECD?                       
         BE    PRGREL                                                           
         CLC   MITTYPE(3),=C'BAS'  CABLE USAGE?                                 
         BE    TVUREC              MAYBE STATION GROUP                          
         B     READ20              BYPASS IF 'H' MISSING                        
*                                                                               
PERMUSG  CLC   MITSEQ,=C'99'       PERMISSABLE USAGE RECD?                      
         BE    READ20              BYPASS                                       
         DC    H'0'                UNKNOWN RECD TYPE                            
*                                                                               
READ90   MVI   MYMODE,0                                                         
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
         CLC   =C'EXTENDED CABLE MIT',MI0FILE                                   
         BE    *+6                                                              
         DC    H'0'                UNKNOWN TAPE TYPE                            
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   BOOK(1),4(R1)       YEAR                                         
         MVC   BOOK+1(1),8(R1)     HUT BOOK                                     
*                                                                               
         CLC   BOOK,=AL1(107,35)   FOR DATA PRIOR TO 8/27/07                    
         BNL   *+6                                                              
         DC    H'0'                USE A DIFFERENT CONVERSION                   
*                                                                               
         MVI   MYMODE,0            SET TO READ NEXT RECORD                      
         MVC   TAPEMKT,=H'516'                                                  
*                                                                               
RDR10    MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
         B     EXIT                                                             
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
         LA    RE,5(RE)                                                         
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
         AH    R1,=Y(RIUNVSQ)      R1=DESTN IN INTERIM REC FOR UNIVS            
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
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIUSASQ)      POINT TO USA DEMO BUCKET                     
         LA    RE,0                USA DEMO ALWAYS WILL BE THE 1ST ENTY         
         ZIC   RF,RELSLOT          WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)      SIZE OF MOVE                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN USA UNIVERSE                         
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         CLC   COVALPH,=C'0000'    TOTAL SAMPLE?                                
         BNE   *+10                                                             
         MVC   INTSTA(4),=C'HUUU'  SET STN TO UNIV                              
         BAS   RE,BLDKEY           BUILD INTERIM RECD KEY                       
         B     RELEASE             RETURN TO CTLR TO RELEASE RECD               
*                                                                               
         EJECT                                                                  
* *********************************************************************         
* TVUREC -HOUSEHOLD USAGE RECORD                                                
*        ONE RECORD PER 1/2HR FOR EACH DAY PLUS MON-FRI AND MON-SUN AVG         
*        FOR EACH COVERAGE SAMPLE                                               
*              RECORD SEQUENCE CODE = 3                                         
*              DATA TYPE CODE       = TVU                                       
*              RECORD TYPE          = H - P - P                                 
* *********************************************************************         
TVUREC   DS    0H                                                               
         CLI   MITREC,C'H'                                                      
         BE    *+6                 HOUSHOLD USAGE RECD                          
         DC    H'0'                                                             
         CLI   RELS,0              NOTHING TO RELEASE-JUST PROCESS              
         BE    TVU10                                                            
         CLC   PRVMKT,MITMKTBR     TEST MKT BREAK?                              
         BE    TVU5                SAME MKT BREAK-> DIFFERENT RECD              
         CLC   PRVHLF,MITHLFID     TEST HALF HR ID                              
         BNE   TVU5                DIFF HLF HR   -> DIFFERENT RECD              
         CLC   PRVQTR,MITQTRID     TEST QHR?                                    
         BNE   TVU5                DIFF QTR HR   -> DIFFERENT RECD              
         CLC   PRVKEY(MITMKTBR-MITKEY),MITKEY    TEST OTHER KEY FIELDS          
         BE    TVU10               SAME -SAVE MKT BREAK/DON'T RELS              
*                                                                               
TVU5     CLI   RELS,3              RELEASE PREVIOUS USAGE RECD?                 
         BE    USGREL              FINISH BLDING USAGE RECD & RELEASE           
         CLI   RELS,5              RELEASE PREVIOUS USAGE RECD?                 
         BE    USGREL              FINISH BLDING USAGE RECD & RELEASE           
*                                                                               
TVU10    MVI   RELS,3                                                           
         CLC   MITSEQ,=C'03'                                                    
         BE    *+8                                                              
         MVI   RELS,5              CABLE VIEWING RECD                           
         MVC   PRVMKT,MITMKTBR     SAVE MKT BREAK                               
         MVC   PRVHLF,MITHLFID     SAVE HLF HR ID                               
         MVC   PRVQTR,MITQTRID     SAVE QHR HR ID                               
**       MVC   PRVKEY(MITMKTBR-MITKEY),MITKEY   SAVE OTHER KEY FIELDS           
         MVC   PRVKEY,MITKEY       SAVE OTHER KEY FIELDS                        
         MVC   INTFEED,MITFEED     FEED PATTERN                                 
         MVC   INTAUDES,MITAUDTY   AUG EST TYPE                                 
         MVC   INTCOVSM,MITCOVG    COVERAGE SAMPLE                              
         MVC   INTCOVCL,MITCOVCL   COV SAMPL CALC IND                           
*                                                                               
         BAS   RE,HDR              PROCESS HDR INFO                             
         CLI   MKTBRK,0                                                         
         BNE   *+8                                                              
         BAS   RE,H_REC            PROCESS 'H' RECD                             
         MVI   INTDTYP,X'09'       DUMMY TO REG-ORIG (FIELD IS N/A)             
         MVC   INTSTA(4),COVALPH   USAGE TO GEOGRAPHIC MKT                      
         MVI   INTSTA+4,X'83'      SMALL-(C) - FOR '03' USAGE DATA              
         CLC   MITQTRID,=C'  '     QUARTER HOUR DATA?                           
         BE    *+8                                                              
         MVI   INTSTA+4,X'98'      SMALL-'Q' = FOR '03' QTR HOUR DATA           
         CLI   RELS,3                                                           
         BE    TVU20                                                            
         MVI   INTSTA+4,C'C'       'C' = FOR '05' HALF HOUR                     
         CLC   MITQTRID,=C'  '     QUARTER HOUR DATA?                           
         BE    *+8                                                              
         MVI   INTSTA+4,C'Q'       'Q' = FOR '05' QUARTER HOUR DATA             
*                                                                               
TVU20    L     R1,=A(DEMAREA)      SAVE IN DEMO AREA                            
         CLI   RELS,5                                                           
         BE    *+8                                                              
         AH    R1,PUTDISP          '03'-SAVE IN PUT SECTION                     
         ST    R1,DMCB             DMCB=A(CVG SAVE AREA IN UNVBUFF)             
         LA    R1,MIXDEM           1ST DEMO IN NON-PUT-LIST  (05 RECD)          
         CLI   RELS,5                                                           
         BE    *+8                                                              
         LA    R1,MIXPDEM          1ST DEMO IN PUT LIST  (03 RECD)              
         ST    R1,DMCB+4           A(1ST DEMO ON INPUT TAPE)                    
         BAS   RE,SLOTDEM          PROCESS DEMOS-SLOT IN BUFFER                 
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
         MVI   MYMODE,0            READ NEXT RECORD                             
TVUX     B     READ20              GO READ NEXT RECD                            
         EJECT                                                                  
***********************************************************************         
*USGREL- RELEASE REGULAR USAGE RECORDS. MYMODE='U' RELS=3                       
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
         CLC   MITCOVG,MITCOVG-MITD(RF)                                         
         BNE   *+14                PROCESS CURRENT RECD                         
         CLC   MITFEED,HPTFEED     SAME FEED?                                   
         BE    READ40                                                           
         OC    HPTSTN,HPTSTN       NOTHING SAVED HERE YET                       
         BZ    READ40              NOTHING TO RELS YET, BYPASS                  
USG23    MVI   MYMODE,C'H'         RELASE HPT AVGS FOR THIS CVG                 
         B     HPTRELS                                                          
*                                                                               
USG25    CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
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
         CLI   RELS,5                                                           
         BE    *+12                                                             
         AH    RE,PUTDISP          IF '03' RECD, SLOT PUTS ACCORDINGLY          
         AH    R1,=Y(RIPUTSQ)                                                   
         LH    RF,=Y(NDEMS*4)      RF=NUMBER BYTES TO MOVE                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),0(RE)      SAVE DEMOS IN INTACCS                        
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIUNVSQ)      R1=DESTN IN INTERIM REC FOR UNIVS            
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
*                                                                               
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIUSASQ)      DISP OF USA DEMOS IN INTACCS                 
         LA    RE,0                USA DEMO WILL ALWAYS BE FIRST ENTRY          
         ZIC   RF,RELSLOT          WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN USA DEMOS, IN THE 2ND BUCKET         
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         XC    INTRSF,INTRSF       NOT DEFINED FOR TIME PERD RECDS              
         CLC   INTSTA(4),=C'0000'  USA LEVEL IS HUT STATION                     
         BNE   USG60                                                            
         MVC   INTSTA(4),=C'HHUT'                                               
         CLI   INTFEED,C'L'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'L'                                                     
*                                                                               
USG60    BAS   RE,BLDKEY           BUILD INTERIM RECD KEY                       
         BAS   RE,HPTFILL          FILL BUFFER FOR M-F/M-S AVGS                 
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
         MVC   PRBTYP+2(2),INTPNUM                                              
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
         CLC   MITQTRID,=C'  '     IS THIS A QTR HOUR RECD?                     
         BE    HDR4                NO                                           
         CLC   MITSEQ,=C'05'       TIME PERIOD DATA ONLY                        
         BE    *+10                                                             
         CLC   MITSEQ,=C'03'                                                    
         BNE   HDR4                                                             
         CLI   MITMIN+1,C'0'       FIRST QTR HOUR--GRAB QTRHR HUT               
         BNE   *+10                                                             
         PACK  DUB,MIHPROJ1        1ST QTR HOUR'S HUT                           
         CLI   MITMIN+1,C'5'                                                    
         BNE   *+10                                                             
         PACK  DUB,MIHPROJ2        2ND QTR HOUR'S HUT                           
         B     HDR5                                                             
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
         STH   R1,MKTBRK           HUT FOR MKT BREAK                            
*                                                                               
         CLC   =C'000000',MITCOVG                                               
         BNE   *+14                                                             
         MVC   COVSMP,=C'0000'                                                  
         B     HDR8                                                             
*                                                                               
         CLI   MITCOVG,C'0'                                                     
         BE    *+6                                                              
         DC    H'0'                EXPECTING MAX 5 DIGITS                       
         XC    COVSMP,COVSMP                                                    
         MVC   PACK8,ZEROS                                                      
         MVC   PACK8(6),MITCOVG                                                 
         PACK  DUB(5),PACK8+1(6)                                                
         MVC   COVSMP,DUB          PACKED WITHOUT SIGN                          
*                                                                               
HDR8     SR    R5,R5                                                            
         L     R1,=A(CVGTBL)       CVG CODE--CABLE CODE DEFINITIONS             
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
         LA    R0,7                                                             
         SR    RE,RE                                                            
         LA    RF,MIHDAYS                                                       
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
         CLC   MITAVG,=C'05'                                                    
         BNE   *+8                                                              
         MVI   DUB,X'7C'           SET TO A M-F RECD                            
         CLC   MITAVG,=C'07'                                                    
         BNE   *+8                                                              
         MVI   DUB,X'7F'           SET TO A M-S RECD                            
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
         MVI   INTRTYP,C'P'        USAGE RECS = 'P' RECS                        
         MVI   INTDUR,2                                                         
         CLC   MITQTRID,=C'  '                                                  
         BE    *+8                                                              
         MVI   INTDUR,1                                                         
         MVC   INTBOOK,BOOK                                                     
         MVI   AVGWKS,0                                                         
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
*                                                                               
*CONVERT START TIME FOR INTPNAME FIELD - CHAR OUTPUT                            
H_REC29  PACK  DUB,MITHOUR(2)                                                   
         CVB   RF,DUB                                                           
         STC   RF,DUB              DUB=HOUR 06(6AM)-29(5AM)                     
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
         MVI   DUB+1,C'P'          SET TO AM                                    
*                                                                               
H_REC38  MVI   DUB+2,C'M'          DUB=HOUR   DUB+1=AM/PM                       
         MVI   INTPNAME+4,C' '                                                  
         MVI   INTPNAME+7,C':'                                                  
         MVC   INTPNAME+8(2),MITMIN    MINUTE                                   
         MVC   INTPNAME+10(2),DUB+1     AM/PM                                   
         ZIC   RE,DUB              RE=START HOUR FOR INTPNAME                   
         LA    RF,INTPNAME+5                                                    
         EDIT  (RE),(2,0(RF))       OUTPUT START HOUR                           
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
         CLC   MITQTRID,=C'  '     CALCULATE END TIME                           
         BE    H_REC45             IS THIS A QTR HOUR RECD? --NO                
         CLC   MITMIN,=C'45'       END TIME WILL BE A NEW HOUR                  
         BNE   *+12                                                             
         LA    RF,100(RF)          BUMP HOUR                                    
         SH    RF,=H'45'           ADJ MINUTES                                  
         CLC   MITMIN,=C'45'                                                    
         BE    *+8                                                              
         LA    RF,15(RF)           BUMP QTR HOUR DURATION                       
         STCM  RF,3,INTETIM        END TIME                                     
         B     H_REC50                                                          
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
         CLC   MITQTRID,=C'  '     QTR HR DUR WILL BE 15MIN!                    
         BE    *+10                                                             
         MVC   INTCOV,=H'15'                                                    
*                                                                               
         GOTO1 VHRTOQH,DMCB,INTSTIM,INTSQH                                      
         GOTO1 VHRTOQH,DMCB,INTETIM,INTEQH                                      
         MVC   INTMRKT,TAPEMKT     DEMO SYSTEM MARKET CODE                      
         CLC   MITQTRID,=C'  '                                                  
         BE    H_REC55                                                          
         MVI   INTADUR,15          QTR HOUR RECD                                
         MVI   INTDUR,1                                                         
         MVI   INTDURM,15                                                       
         MVC   INTDURM2,=Y(15)     2-BYTE DURATION FIELD                        
         B     H_RECX                                                           
*                                                                               
H_REC55  MVI   INTADUR,30          HALF HOUR RECD                               
         MVI   INTDUR,2                                                         
         MVI   INTDURM,30                                                       
         MVC   INTDURM2,=Y(30)     2-BYTE DURATION FIELD                        
*                                                                               
H_RECX   B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
* *********************************************************************         
* SLOTDEM -    SLOT DEMOS IN APPROPRIATE BUCKETS IN BUFFER                      
*        INPUT PARMS:  DMCB   - A(OUTPUT BUFER)                                 
*                      DMCB+4 - A(1ST DEMO)                                     
*                      MKTBRK - MKT BREAK                                       
*                      USHUT  - US HUT PROJECTION                               
*                      MKTHUT - MKT BREAK HUT PROJECTION                        
* *********************************************************************         
*                                                                               
SLOTDEM  NTR1                                                                   
         L     R1,=A(CRCITAB)      FIND MKT BRK IN TABLE                        
         SR    RE,RE                                                            
SLOT5    CLC   MKTBRK,0(R1)                                                     
         BE    SLOT10                                                           
         LA    RE,1(RE)                                                         
         LA    R1,L'CRCITAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   SLOT5                                                            
         DC    H'0'                                                             
*                                                                               
SLOT10   MH    RE,MKTDISP          DISP TO MKT BRK IN BUFFER                    
         A     RE,DMCB             ADD ADDRESS OF OUTPUT BUFFER                 
         ST    RE,DMCB             RE-SAVE                                      
         MVC   RIUSA*4(4,RE),USHUT   SLOT HUT AND MKT BRK HUT                   
         MVC   RIHOMES*4(4,RE),MKTHUT                                           
         LA    R0,MIXDEMQ          NUMBER OF INPUT DEMOS                        
         LA    R5,1                WHICH DEMO WE'RE PROCESSING                  
*                                                                               
SLOT15   LAY   R1,SLOTTAB          FIND DEMO NUMBER IN TABLE                    
SLOT18   CLI   0(R1),X'FF'         END TABLE?                                   
         BE    SLOT20              THERE ARE SOME OPEN SLOTS                    
         CH    R5,0(R1)                                                         
         BE    *+12                DEMO TYPE FOUND                              
         LA    R1,4(R1)            BUMP POSITION IN SLOTTAB                     
         B     SLOT18                                                           
*                                                                               
         LH    RE,2(R1)            RE=OUTPUT SLOT                               
         SLL   RE,2                4 BYTE BUCKETS- RE=DISP TO BKT               
         A     RE,DMCB             RE=A(OUTPUT SLOT)                            
         L     RF,DMCB+4           A(INPUT DEMOS)                               
         OC    0(9,RF),=9C'0'                                                   
         PACK  DUB,0(9,RF)         CONVERT DEMO TO NUMERIC                      
         CVB   R1,DUB                                                           
         STCM  R1,15,0(RE)         SAVE DEMO IN BUCKET                          
*                                                                               
SLOT20   L     RF,DMCB+4           INPUT DEMO ADDRESS                           
         LA    RF,9(RF)            PT TO NEXT DEMO ON INPUT RECD                
         ST    RF,DMCB+4                                                        
         LA    R5,1(R5)            BUMP DEMO NUMBER                             
         BCT   R0,SLOT15           LOOP THRU ALL INPUT DEMOS                    
         B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
* PRGREC-PROGRAM RECORD PROCESSING                                              
*              RECORD SEQUENCE CODE = 4                                         
*              RECORD TYPE          = D - P0- P0- P2 - P2                       
* *********************************************************************         
PRGREC   DS    0H                                                               
         CLI   MITREC,C'D'         PROCESS PROGRAM DISCRIPTOR REC (PDR)         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RELS,0              NOTHING TO RELEASE-JUST PROCESS              
         BE    PRG10                                                            
         CLI   RELS,4              RELEASE PREVIOUS USAGE RECD?                 
         BE    PRG5                FINISH BLDING USAGE RECD & RELEASE           
         CLC   PRVMKT,MITMKTBR     TEST MKT BREAK?                              
         BE    PRG5                SAME MKT BREAK-> DIFFERENT RECD              
         CLC   PRVKEY(MITMKTBR-MITKEY),MITKEY    TEST OTHER KEY FIELDS          
         BE    PRG10               SAME -SAVE MKT BREAK/DON'T RELS              
PRG5     B     PRGREL              RELEASE PRG RECD                             
*                                                                               
PRG10    MVI   RELS,4                                                           
         MVC   PRVKEY,MITKEY       SAVE KEY                                     
**       MVC   PRVKEY(MITMKTBR-MITKEY),MITKEY    SAVE KEY,                      
         BAS   RE,HDR              PROCESS HDR INFO                             
         CLI   MKTBRK,0                                                         
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
         L     R1,=A(DEMAREA)      SAVE IN DEMO AREA                            
         AH    R1,PUTDISP          DISP TO PUTS IN DEMAREA BUFFER               
         ST    R1,DMCB             DMCB= DEST OF DEMOS                          
         LA    R1,MIXPDEM          1ST DEMO ON PUT LIST                         
         ST    R1,DMCB+4           A(1ST DEMO ON INPUT TAPE)                    
         BAS   RE,SLOTDEM          PROCESS DEMOS-SLOT IN BUFFER                 
*                                                                               
PRGX     MVC   SAVEINT(L'SAVEINT),INTVALS                                       
         B     READ20              GO READ NEXT RECD                            
         EJECT                                                                  
* ********************************************************************          
* PDRREC - PROGRAM DESCRIPTOR RECORD EXTRACT FIELDS FOR INTERIM RECD            
*        ONLY PROCESS PDRS FOR TOTAL SAMPLE FOR EACH PROGRAM                    
*        IGNORE PDRS FOR EACH MKT BREAK                                         
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
         CLC   MIDEPS,BLANKS                                                    
         BE    *+18                                                             
         PACK  DUB,MIDEPS                                                       
         CVB   R1,DUB                                                           
         STCM  R1,7,INTEPS         (3 BYTES)                                    
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY     CORECTN BOOK           
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
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
PDR30    CLI   0(RE),C'0'                                                       
         BE    *+12                                                             
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
PDR40    MVI   VARIOUS,1           VARIOUS DAYS                                 
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
         MVC   PDPT,MIDAYPT        TYPE DAYPART                                 
         MVC   PDPT2,MIDREPDP                                                   
         MVC   PNET,COVALPH        SET UP R/S CALL LETTERS                      
         MVC   PNAME,MIDPNAME                                                   
         MVC   PTITLE,MIDEPNAM     EPISODE NAME                                 
         MVC   PTYP,MIDPTYP                                                     
         MVC   PPREM,MIDPREM                                                    
         MVC   PSHORT,MIDSDUR                                                   
         MVC   INTMRKT,TAPEMKT     CABLE MKT=510                                
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
*                                                                               
         SR    R1,R1                                                            
         CLC   MIDEPS,BLANKS       EPISODE NUMBER                               
         BE    *+18                                                             
         PACK  DUB,MIDEPS                                                       
         CVB   R1,DUB                                                           
         STCM  R1,7,INTEPS         (3 BYTES)                                    
*                                                                               
         SR    R1,R1                                                            
         CLC   MITTELC,BLANKS      TELECAST NUMBER                              
         BE    *+18                                                             
         PACK  DUB,MITTELC+1(9)                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,INTTNUM                                                    
*                                                                               
         SR    R1,R1                                                            
         CLC   MIDLIB,BLANKS       LIBRARY ID                                   
         BE    *+18                                                             
         PACK  DUB,MIDLIB+1(9)                                                  
         CVB   R1,DUB                                                           
         STCM  R1,15,INTLIB                                                     
*                                                                               
         MVC   INTANET,MITTYPE     ACN NETWORK CODE                             
         MVC   INTFEED,MIDFDPAT    FEED PATTERN FOR PROGRAM                     
         MVC   INTAUDES,MITAUDTY   AUG EST TYPE                                 
         MVC   INTCOVSM,MITCOVG    COVERAGE SAMPLE                              
         MVC   INTCOVCL,MITCOVCL   COV SAMPL CALC IND                           
         MVC   INTPNAME,MIDPNAME   PROGRAM NAME                                 
         MVC   INTTRNAM,MIDTKNAM   TRACKAGE NAME                                
         MVC   INTEPNAM,MIDEPNAM   EPISODE NAME                                 
         MVC   INTCLTEP,MIDEPNUM   CLT EPISODE PRG NUMBER                       
         MVC   INTPTYP,MIDPTYP     PROGRAM TYPE ALPHA                           
         MVC   INTSBTYP,MIDSUBPT   SUB-PRG TYPE ALPHA                           
         MVC   INTCMCL,MIDCOMR     COMMERCIAL STATUS                            
         MVC   INTLIVE,MIDLIVE     LIVE EVENT INDIC                             
         MVC   INTPOA,MIDPRORG     PRG ORIGINAL/ACQUIRED                        
         MVC   INTEOA,MIDEPORG     EPISODE  ORIGIAL/ACQUIRED                    
         MVC   INTPREM,MIDPREM     PREMIERE INDICATOR                           
*                                                                               
         PACK  DUB,MIDAUPRJ        PROJECTION (XXX,XXX,XXX)                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO TEN THOUSANDS                       
         D     R0,=F'10000'                                                     
         STCM  R1,3,PWK1AUD        REACH                                        
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
         CLC   MIDREP,1(RE)        WILL BE BLANK FOR NETWORK!                   
         BE    PDR85                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,PDR80                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
*                                                                               
PDR85    MVC   PWK1DTYP,2(RE)      X'09'=REGUALR  X'0C'=SPECIAL                 
         CLC   MITAVG,=C'00'     INDIV DAY REC?                                 
         BNE   PDR90               NO, AN AVG RECORD                            
         CLI   MIDMULT,C' '        MULTI-DAYS?                                  
         BE    *+8                                                              
         OI    PWK1DTYP,X'80'      SET OPT BIT IN PHTDTYP                       
*                                                                               
PDR90    MVI   PWK1RSF,0                                                        
         CLI   MITBREAK,C'1'        TEST FOR REDUCED STATION                    
         BNE   *+8                                                              
         MVI   PWK1RSF,X'01'       REDUCED STATION INDICATOR- BREAK OUT         
*                                                                               
         MVC   INTDPT,PDPT         PDPT IS ALWAYS ZERO                          
         MVC   INTDPT2,PDPT2                                                    
         MVC   INTPREM,PPREM                                                    
         MVC   INTPNO,PNUM                                                      
         MVC   INTCOV,PTOTDUR                                                   
         MVI   INTSTYP,0                                                        
         MVI   INTMTYP,0                                                        
         MVC   INTAUD,PWK1AUD                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
         MVC   INTBOOK,BOOK        DEFAULT: BOOK=YYMM FOR MULTI WEEK            
         MVC   INTIBOOK,IBOOK         "       "     "                           
*                                                                               
PDR95    DS    0H                                                               
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
*                                                                               
         CLC   INTBOOK,=X'681B'    USE INDICATORS AFTER JUN28/04                
         BL    *+16                                                             
         MVC   INTMOVIE,MIDMOVIE   MOVIE INDICATOR                              
         MVC   INTTMSPT,MIDTMSPT   TEAM SPORTS INDICATOR                        
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
         MVC   PDAY1BIT,INTDAYWK   NOT INTERNAL CODE/DAY BITS                   
PDR120   GOTO1 PDATA,DMCB,SAVETIME,PDAY1BIT                                     
         MVI   INTRTYP,PMCODEQU    -Q-                                          
         MVC   HALF,PWK1STIM                                                    
         MVC   INTSTIM,HALF        START TIME                                   
         MVC   INTDURM,PWK1DURM    DURATION IN MINUTES                          
         MVC   INTDURM2,PWK2DURM   2-BYTE DURATION FIELD                        
         GOTO1 VHRTOQH,DMCB,HALF,INTSQH       START QH                          
*                                                                               
*        DEVELOP END QH (INTEQH) AND DURATION IN QH'S (INTDUR)                  
*                           INTDUR = 1 ( 1-22 MIN.)                             
*                                  = 2 (23-37 MIN.)                             
*                                  = 3 (38-52 MIN.)...ETC.                      
         ZIC   R0,INTSQH                                                        
*        ZIC   RF,PWK1DURM         DURATION IN MINUTES                          
         SR    RF,RF                                                            
         ICM   RF,3,PWK2DURM       2-BYTE DURATION                              
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
PRGREL5  DS    0H                  MAKE A 'P' REC FOR EA QHR PRG RAN TO         
         MVI   MYMODE,C'N'         MERGE PRG NAME WITH TIME PERD DATA           
         LA    RE,PRGTM            LOOP THRU TABLE                              
         ST    RE,INDEX                                                         
         MVC   RELSLOT,INTSQH                                                   
         B     PRGTIME             BUILD KEY ETC                                
*                                                                               
PRGREL10 L     RE,=A(CRCOTAB)                                                   
PRGREL13 CLI   0(RE),X'FF'                                                      
         BE    PRGREL5             WHEN DONE FORMING INTERD'S FOR MKTS          
         CLC   1(1,RE),RELSLOT                                                  
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
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
         CLI   INTORIG,C'2'                                                     
         BE    PRGREL28                                                         
*                                                                               
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,MKTDISP                                                       
         A     RE,=A(DEMAREA)                                                   
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EXMVC RF,INTACCS,0(RE)    MOVE IN DEMOS                                
*                                                                               
         AH    RE,=Y(NDEMS*4)      MOVE IN PUTS AFTER PRG AUD'S                 
         LA    R1,INTACCS                                                       
         AH    R1,=Y(RIPUTSQ)      NEXT FREE SLOT AFTER DEMOS                   
         EXMVC RF,0(R1),0(RE)      MOVE IN PUTS                                 
*                                                                               
         LA    R1,INTACCS          MOVE IN UNIVS AFTER PUTS                     
         AH    R1,=Y(RIUNVSQ)                                                   
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
*                                                                               
         LA    R1,INTACCS          USA UNIVERSE DEMOS                           
         AH    R1,=Y(RIUSASQ)                                                   
         LA    RF,0                                                             
         ZIC   RE,RELSLOT          DISP TO MARKET BREAK                         
         MH    RE,MKTDISP                                                       
         AR    RE,RF                                                            
         A     RE,=A(UNVBUFF)      UNIVERSE BUFFER                              
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN USA UNIVERSE DEMOS                   
*                                                                               
PRGREL28 ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
*                                                                               
PRGREL30 BAS   RE,PRGKEY                                                        
         B     RELEASE                                                          
         DROP  R9                                                               
         EJECT                                                                  
* *******************************************************************           
* PRGKEY-  SPECIAL KEY FOR PROGRAM RECDS TO FORCE PROPER SORT FOR               
*          '1F' ELEMENTS WHEN RECORDS GET PASSED TO OUTPUT PHASE.               
* *******************************************************************           
PRGKEY   NTR1                                                                   
         LA    RF,INTKEY                                                        
         USING QPRGKD,RF           PROGRAM KEY DSECT                            
         XC    INTKEY,INTKEY                                                    
         MVI   QCODE,QCODEQ        FUDGE PRG CODE AS 'Q' RECORD                 
         CLI   INTORIG,C'0'        ORIGINAL DATA?                               
         BE    *+8                                                              
         MVI   QCODE,C'R'          NO, CORRECTION TYPE RECORD                   
         MVC   QBOOK,INTBOOK                                                    
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
         MVC   INTKEY+26(1),KEYSRC                                              
*                                                                               
PRGKX    B     XIT                 DONE BUILDING KEY                            
         EJECT                                                                  
********************************************************************            
*PRGTIME - TIME PERIOD RECDS GENERATED FROM PRG RECDS IN ORDER TO               
* MERGE THE PRG NAME WITH THE '05' QHR RECDS.  MERGE OCCURS IN                  
* CNVWR ROUTINE.                                                                
********************************************************************            
PRGTIME  DS    0H                                                               
         MVI   MYMODE,C'N'         PRG NAME MODE                                
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         CLC   RELSLOT,INTEQH      LOOP THRU QHRS UNTIL END QHR                 
         BE    PRGTIM30                                                         
         MVI   INTRTYP,C'P'        BUILD KEY                                    
         XC    INTKEY,INTKEY                                                    
         LA    R7,INTKEY           BUILD -P- PAV KEY                            
         USING PRKEY,R7                                                         
         MVI   PRCODE,PRCODEQU     -P- RECORD                                   
         MVI   PRMEDIA,C'C'        'N'=1 WK AVG 'W'=INDIV DAY                   
         MVI   PRSRC,C'N'                                                       
         MVC   INTKSRC,KEYSRC                                                   
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRBTYP+3(1),RELSLOT   MOVE IN QHR                                
         MVC   PRBTYP+4(1),MKTBRK    MKT BREAK                                  
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRSTIM,RELSLOT                                                   
         MVI   PRDW+1,X'00'        FORCE TO SORT BEFORE REG T.P. RECD           
         MVC   PRDW+2(1),INTDUR    SORT HIGHER DUR FIRST                        
         XI    PRDW+2,X'FF'                                                     
         L     RE,INDEX                                                         
         MVC   PRSTAT+4(1),0(RE)   'C'=HLFHR 'Q'=QTRHR                          
         CLI   1(RE),X'FF'         USE DAY OF THIS RECD?                        
         BE    *+10                                                             
         MVC   INTDAYWK,1(RE)      USE TABLE OVERRIDE                           
         MVC   PRBTYP+1(1),INTDAYWK  SORT BY: DAY--QTRHR--MKT BRK               
         MVC   PRDW,INTDAYWK                                                    
         DROP  R7                                                               
*                                                                               
         ZIC   RE,RELSLOT          BUMP QHR COUNTER FOR NEXT ITERATION          
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         B     RELEASE             RELEASE CURRENT 'P' QHR RECD                 
*                                                                               
PRGTIM30 DS    0H                  BUMP TO NEXT ENTRY IN PRGTM TABLE            
         L     RE,INDEX            DIFF TYPES OF RECDS TO RELEASE               
         LA    RE,L'PRGTM(RE)      NEXT ENTRY IN TABLE                          
         ST    RE,INDEX                                                         
         CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    PRGTIMX                                                          
         MVC   RELSLOT,INTSQH      RESET QHR LOOP                               
*                                                                               
         CLI   1(RE),X'00'         GENERATE M-F RECD?                           
         BNE   PRGTIME             NO, JUST GO PROCESS                          
         CLI   INTDAYWK,X'60'      FOR M-F TEST PRG NOT SAT/SUN                 
         BE    PRGTIM30                                                         
         CLI   INTDAYWK,X'70'                                                   
         BE    PRGTIM30                                                         
         B     PRGTIME             START OVER                                   
*                                                                               
PRGTIMX  XC    INTSTA,INTSTA                                                    
         MVI   RELS,0              CLEAR PREV PRG RECD SWITCH                   
         MVI   MYMODE,0                                                         
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         CLI   RECD,C'Z'           END OF TAPE                                  
         BE    ENDJOB                                                           
         B     READ40              PROCESS CURRENT RECD IN BUFFER               
         EJECT                                                                  
********************************************************************            
* CNVWR - SORT RECDS HOOK.  AFTER ALL IRECS BUILT DEMCNV COMES HERE             
* BEFORE IT GOES TO THE OUTPUT PHASE.  LAST CHANCE TO PROCESS BEFORE            
* OUPUT PHASE HANDLING.                                                         
*  - MERGE PRG NAME (SPECIAL 'P-RECDS) WITH (05) TIME PERD RECDS                
********************************************************************            
CNVWR    DS    0H                                                               
         LA    R1,UNVBUFF                                                       
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
         MVC   INTKSRC,KEYSRC                                                   
         CLI   PRDW+1,X'00'        FUDGED TIME PER REC W/PRG NAME               
         BNE   CNVWR20             NO                                           
         MVI   BYPSORT,X'80'       DON'T RELEASE FUDGE RECD                     
         CLC   INTKEY(PRDW-PRKEY+1),PRVKEY   SAME AS LAST KEY?                  
         BE    *+14                BYPASS                                       
         MVC   SVPNAME,INTPNAME    DIFF KEY: SAVE THIS PRG NAME                 
         B     CNVWRX                                                           
*                                                                               
         CLI   PRDW,X'00'          M-F RECD?                                    
         BE    *+12                                                             
         CLI   PRDW,X'80'          M-S RECD?                                    
         BNE   CNVWRX              FOR INDIV DAYS,JUST TAKE 1ST RECD            
         CLC   SVPNAME,INTPNAME     SAME PRG NAME?                              
         BE    CNVWRX              YES                                          
         MVC   SVPNAME,BLANKS       DIFF PRGS AIRED FOR THIS QHR                
         MVC   SVPNAME(7),=C'VARIOUS'                                           
         B     CNVWRX                                                           
         DROP  RE                                                               
*                                                                               
CNVWR20  CLI   CNV1ST,0                                                         
         BE    CNVWRX                                                           
         CLC   INTKEY(PRDW-PRKEY+1),PRVKEY SAME AS LAST KEY?                    
         BNE   CNVWRX              DIFF KEY FROM SAVED PRG NAME                 
         MVC   INTPNAME,SVPNAME    MOVE IN PRG NAME FOR THIS QHR                
*                                                                               
CNVWRX   MVC   PRVKEY,INTKEY                                                    
         MVI   CNV1ST,1                                                         
         L     RF,AWREC                                                         
         LR    RE,R2                                                            
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
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
         MHI   R7,HPTDLN           LENGTH OF EACH QHR ENTRY                     
         CLI   INTDUR,2            1/2HR RECD?                                  
         BE    HPTF10                                                           
         A     R7,VQHRBUF          NO, SET QHR TABLE                            
         MVC   HPTQST5,INTSTA+4    SET QHR CHAR                                 
         B     HPTF20                                                           
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
         MVC   HPTETIM,INTSTIM                                                  
         CLI   INTDAYWK,X'60'      DON'T INCLUDE SAT IN M-F AVG                 
         BE    HPTMSAVG                                                         
         CLI   INTDAYWK,X'70'                                                   
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
         CLI   HPTREC,5                                                         
         BE    *+8                                                              
         AHI   R5,RIPUTSQ          START AT POSN FOR PUTS                       
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
*HPTRELS - RELEASE M-F AND M-SU AVG RECDS                                       
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
HPTR05   CLI   QHR,96                                                           
         BH    HPTRELX                                                          
         CLI   QHRFLG,0            0: QTRHR BUFFER M-F RECD                     
         BNE   HPTR10                                                           
         SR    R7,R7                                                            
         IC    R7,QHR              QHR CURRENTLY PROCESSING                     
         MHI   R7,HPTDLN           OFFSET INTO BUFFER                           
         A     R7,VQHRBUF                                                       
         OC    HPTMFWGT,HPTMFWGT                                                
         BZ    HPTR80              BYPASS THIS QHRFLG RELS                      
         LA    R6,HPTMFWGT         CLEARED AFTER HPTDIV                         
         BAS   RE,HPTDIV                                                        
         LA    R6,HPTMFDEM                                                      
         MVI   INTDUR,1                                                         
         MVI   INTADUR,15                                                       
         MVC   INTCOV,=H'15'                                                    
         MVC   INTSTA(4),HPTSTN                                                 
         MVC   INTSTA+4(1),HPTQST5                                              
         MVI   INTDAYWK,X'00'      M-F                                          
         MVI   INTDYBIT,X'7C'                                                   
         MVC   INTPNAME(4),=C'M-F '                                             
         B     HPTR50                                                           
*                                                                               
HPTR10   CLI   QHRFLG,1            1: QTRHR BUFFER M-SU RECD                    
         BNE   HPTR20                                                           
         SR    R7,R7                                                            
         IC    R7,QHR              QHR CURRENTLY PROCESSING                     
         MHI   R7,HPTDLN           OFFSET INTO BUFFER                           
         A     R7,VQHRBUF                                                       
         OC    HPTMSWGT,HPTMSWGT                                                
         BZ    HPTR80              BYPASS THIS QHRFLG RELS                      
         LA    R6,HPTMSWGT                                                      
         BAS   RE,HPTDIV                                                        
         LA    R6,HPTMSDEM                                                      
         MVC   INTSTA+4(1),HPTQST5                                              
         MVI   INTDAYWK,X'80'      M-SU                                         
         MVI   INTDYBIT,X'7F'                                                   
         MVC   INTPNAME(4),=C'M-SU'                                             
         MVI   INTDUR,1                                                         
         MVI   INTADUR,15                                                       
         MVC   INTCOV,=H'15'                                                    
         B     HPTR50                                                           
*                                                                               
HPTR20   CLI   QHRFLG,2            2: HLFHR BUFFER M-F RECD                     
         BNE   HPTR30                                                           
         SR    R7,R7                                                            
         IC    R7,QHR              QHR CURRENTLY PROCESSING                     
         MHI   R7,HPTDLN           OFFSET INTO BUFFER                           
         A     R7,VHLFBUF                                                       
         OC    HPTMFWGT,HPTMFWGT                                                
         BZ    HPTR80              BYPASS THIS QHRFLG RELS                      
         LA    R6,HPTMFWGT                                                      
         BAS   RE,HPTDIV                                                        
         LA    R6,HPTMFDEM                                                      
         MVC   INTSTA+4(1),HPTHST5                                              
         MVI   INTDAYWK,X'00'      M-F                                          
         MVI   INTDYBIT,X'7C'                                                   
         MVC   INTPNAME(4),=C'M-F '                                             
         MVI   INTDUR,2                                                         
         MVI   INTADUR,30                                                       
         MVC   INTCOV,=H'30'                                                    
         B     HPTR50                                                           
*                                                                               
HPTR30   CLI   QHRFLG,3            3: HLFHR BUFFER M-SU RECD                    
         BE    *+6                                                              
         DC    H'0'                SOMETHING IS WRONG                           
         SR    R7,R7                                                            
         IC    R7,QHR              QHR CURRENTLY PROCESSING                     
         MHI   R7,HPTDLN           OFFSET INTO BUFFER                           
         A     R7,VHLFBUF                                                       
         OC    HPTMSWGT,HPTMSWGT                                                
         BZ    HPTR80              BYPASS THIS QHRFLG RELS                      
         LA    R6,HPTMSWGT                                                      
         BAS   RE,HPTDIV                                                        
         LA    R6,HPTMSDEM                                                      
         MVI   INTDAYWK,X'80'      M-S                                          
         MVI   INTDYBIT,X'7F'                                                   
         MVC   INTPNAME(4),=C'M-SU'                                             
         MVI   INTDUR,2                                                         
         MVI   INTADUR,30                                                       
         MVC   INTCOV,=H'30'                                                    
         MVC   INTSTA+4(1),HPTHST5                                              
         B     HPTR50                                                           
*                                                                               
HPTR50   DS    0H                  FILL IN UNIVS, INT FIELDS & RELEASE          
         LA    R1,INTACCS          COPY DEMOS TO INTERIM RECD                   
         CLI   HPTREC,5                                                         
         BE    *+8                                                              
         AH    R1,=Y(RIPUTSQ)                                                   
         LH    RF,=Y(NDEMS*4)      RF=NUMBER BYTES TO MOVE                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),0(R6)      SAVE DEMOS IN INTACCS                        
         LA    R1,INTACCS          SLOT UNIVS                                   
         AH    R1,=Y(RIUNVSQ)      R1=DESTN IN INTERIM REC FOR UNIVS            
         ZIC   RE,COVSMPD          GET UNIVS FOR COVG SAMPLE                    
         MH    RE,CVGLN            BUMP TO CVG IN UNVBUFF                       
         SR    RF,RF               WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN UNIVS (AFTER DEMOS)                  
*                                                                               
         LA    R1,INTACCS          SLOT UNIVS                                   
         AH    R1,=Y(RIUSASQ)      R1=DESTN IN INTERIM REC FOR UNIVS            
         LA    RE,0                                                             
         SR    RF,RF               WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF               COVG DISP + MKT DISP                         
         A     RE,=A(UNVBUFF)                       +START OF BUFFER            
         LH    RF,=Y(NDEMS*4)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE IN USA UNIVERSE DEMOS.                  
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
         MVI   INTBTYP,0                                                        
         CLC   INTSTA(4),=C'0000'  USA LEVEL IS HUT STATION                     
         BNE   *+10                                                             
         MVC   INTSTA(4),=C'HHUT'                                               
         CLC   INTSTA(4),=C'HHUT'                                               
         BNE   HPTR55                                                           
         CLI   INTFEED,C'L'                                                     
         BNE   *+8                                                              
         MVI   INTBTYP,C'L'                                                     
*                                                                               
HPTR55   SR    RE,RE                                                            
         IC    RE,INTDUR                                                        
         MH    RE,=H'15'                                                        
         STC   RE,INTDURM          SET DURATION MINUTES                         
         STCM  RE,3,INTDURM2       2-BYTE DURATION FIELD                        
         MVI   MYMODE,C'H'                                                      
         SR    R1,R1               BUMP OUTPUT LEVEL FOR NXT RELEASE            
         IC    R1,QHRFLG                                                        
         LA    R1,1(R1)                                                         
         STC   R1,QHRFLG                                                        
         CLI   QHRFLG,4            DONE RELEASING RECDS FOR THIS QHR            
         BL    HPTR60                                                           
         MVI   QHRFLG,0                                                         
         IC    R1,QHR              BUMP QHR CTR FOR NEXT TIME                   
         LA    R1,1(R1)                                                         
         STC   R1,QHR                                                           
*                                                                               
HPTR60   DS    0H                                                               
         BAS   RE,BLDKEY                                                        
         B     RELEASE             RELEASE AVG REC TO SORT                      
*                                                                               
HPTR80   DS    0H                  BYPASS THIS DAY/QHR                          
         SR    R1,R1                                                            
         IC    R1,QHRFLG                                                        
         LA    R1,1(R1)                                                         
         STC   R1,QHRFLG                                                        
         CLI   QHRFLG,4                                                         
         BL    HPTR05              START FROM TOP                               
         MVI   QHRFLG,0                                                         
         IC    R1,QHR                                                           
         LA    R1,1(R1)                                                         
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
               LRECL=1120,                                             X        
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
**********************************************************************          
*PRGTM - CREATES MULTIPLE TIME PERIOD RECDS OFF OF PRG RECD TO                  
*        LINK PROGRAM NAME WITH TIME PERIOD DATA.  THESE RECDS ONLY             
*        HOLD PRG NAME AND ARE DISCARDED IN CNVWR SORT.                         
**********************************************************************          
PRGTM    DS    0CL2                                                             
         DC    C'C',X'FF'          REC'D DAY   'C' = HLF HOUR RECD              
         DC    C'Q',X'FF'          REC'D DAY   'Q' = QTR HOUR RECD              
         DC    C'C',X'00'          MON-FRI     'C' = HLF HOUR RECD              
         DC    C'Q',X'00'          MON-FRI     'Q' = QTR HOUR RECD              
         DC    C'C',X'80'          MON-SUN     'C' = HLF HOUR RECD              
         DC    C'Q',X'80'          MON-SUN     'Q' = QTR HOUR RECD              
         DC    X'FF'                                                            
PRGTMQ   EQU   (*-PRGTM)/L'PRGTM                                                
         EJECT                                                                  
***********************************************************************         
*                    WORKING STORAGE                                            
**********************************************************************          
         DS    0D                                                               
PACK8    DS    PL8                                                              
PACK16   DS    PL16                                                             
*                                                                               
INDEX    DS    A                   ADDRESS INTO TABLE                           
MKTBRK   DS    X                   MKT BREAK NUMBER                             
BYTE     DS    C                                                                
TMP      DS    C                                                                
VARIOUS  DS    X                   1=VARIOUS                                    
WKBOOK   DS    CL2                 SAVED NETWEEK BOOK                           
WKIBOOK  DS    CL2                 SAVED NETWEEK IBOOK                          
WKDAYWK  DS    CL1                 SAVED INTDAYWK                               
TAPEMKT  DS    H                   CABLE MKT=510                                
KEYSRC   DS    C                                                                
SAVETIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND MIDDUR            
VARSTIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND MIDDUR            
SAVVAR   DS    7XL7                SAVE VAR INFO FROM INDIVIDUAL DAYS           
PHUT     DS    CL9                                                              
TIMTAB   DS    7CL3                                                             
BOOK     DS    XL2                 KEY BOOK                                     
IBOOK    DS    XL2                 INTERNAL BOOK                                
BOOKS    DS    XL2                 KEY BOOK (SAVE AREA)                         
IBOOKS   DS    XL2                 INTERNAL BOOK (SAVE AREA)                    
USHUT    DS    H                   HALF HOUR US HUT PROJ (XXX,XXX,XXX)          
MKTHUT   DS    F                   MKT BREAK HUT  PROJ   (XXX,XXX,XXX)          
SVAVGHI  DS    F                   SAVE PROGRAM HH                              
APACKWT  DS    A                   A(PACKWT) AREA                               
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
*                                                                               
SVSEQ    DC    CL2' '                                                           
SVNET    DC    CL6' '                                                           
SVCVG    DC    CL6' '                                                           
PRVMKBK  DC    X'00'                                                            
CNV1ST   DC    X'00'                                                            
CURRDUR  DC    H'00'                                                            
MYMODE   DC    X'00'                                                            
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
RECD     DC    X'00'               TYPE OF RECD JUST READ FROM TAPE             
RELSLOT  DC    X'00'               RELEASE SLOT                                 
SUMSLOT  DC    X'00'               SUMMARY SLOT                                 
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
SVPNAME  DS    CL25                PRG NAME                                     
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
BLANKS   DC    256C' '                                                          
*                                                                               
         DS    0H                                                               
CVGLN    DC    AL2(NDEMS*NMKTS*4)  SIZE OF CVG IN UNV BUFFER (#BYTES)           
COVALPH  DS    CL4                                                              
COVSMP   DS    CL4                                                              
COVSMPD  DS    X                                                                
RELS     DC    X'00'                                                            
PRVKEY   DS    CL(L'MITKEY)        PREVIOUS RECD'S KEY                          
PRVMKT   DS    CL(L'MITMKTBR)      PREVIOUS MKT BREAK                           
PRVHLF   DS    CL(L'MITHLFID)      PREVIOUS HLF HOUR ID                         
PRVQTR   DS    CL(L'MITQTRID)      PREVIOUS QTR HOUR ID                         
*                                                                               
HPTSTN   DC    XL5'00'             STN ACCUM AVGS FOR                           
HPTQST5  DS    CL1                 QHR INTSTA+4 CHAR                            
HPTHST5  DS    CL1                 HALF HOUR INTSTA+4 CHAR                      
HPTREC   DC    XL1'00'             3 OR 5                                       
HPTFEED  DC    XL1'00'             FEED PATTERN FOR  03'S                       
HPTFRST  DC    XL1'00'             1ST TIME IN FOR CVG SAMPLE                   
QHR      DC    XL1'00'             QHR PROCESSING                               
QHRFLG   DC    XL1'00'             WHICH RECD GETS RELSD M-F/QHR..              
*                                                                               
*-----------------------------                                                  
* SAVE INTERIM RECD KEY                                                         
*-----------------------------                                                  
         DS    0F                                                               
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD KEY                      
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
PWK2DURM DS    XL2                 DURATION IN MINUTES (2 BYTES)                
PWK1AUD  DS    XL2                                                              
PWK1STAC DS    XL2                                                              
PWK1COV  DS    XL2                                                              
PWK1DTYP DS    X                                                                
PWK1RSF  DS    X                                                                
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
PUTDISP  DC    AL2(NDEMS*4)        DISP TO DEMO PUTS                            
MKTDISP  DC    AL2(NDEMS*4*3)      DISP TO NEXT MKT BRK DEMOS                   
*KTDISP  DC    AL2(NDEMS*4*2)      CHANGED FROM NDEMS*4+2 > NDEM*4*3            
*                                                                               
SAVEKEY  DS    CL(L'INTKEY)        KEY OF ACCUMULATED DEMOS                     
         DS    0F                                                               
SAVEREC  DS    XL(NDEMS*4*3)       4BYTE DEMOS + UNIVS                          
*AVEREC  DS    XL(NDEMS*4*2)       4BYTE DEMOS + UNIVS                          
*-------------------------------------------------------------------            
         DS    0F                                                               
UNVBUFF  DS    (NDEMS*NMKTS*NCBLS)XL4    DEMO UNIVS                             
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
NMKTS    EQU   1                   INCLUDES TOTAL SAMPLE=000                    
NCBLS    EQU   200                 NUMBER OF CABLE NETS                         
         EJECT                                                                  
**********************************************************************          
* RT..=  INPUT TAPE  DEMO BUCKET DEFINITIONS                                    
**********************************************************************          
*DEMO        BUCKET                                                             
*-----       ------                                                             
RTF25    EQU   1                                                                
RTF68    EQU   2                                                                
RTF911   EQU   3                                                                
RTF1214  EQU   4                                                                
RTF1517  EQU   5                                                                
RTF1820  EQU   6                                                                
RTF2124  EQU   7                                                                
RTF2529  EQU   8                                                                
RTF3034  EQU   9                                                                
RTF3539  EQU   10                                                               
RTF4044  EQU   11                                                               
RTF4549  EQU   12                                                               
RTF5054  EQU   13                                                               
RTF5564  EQU   14                                                               
RTF65O   EQU   15                                                               
RTM25    EQU   16                                                               
RTM68    EQU   17                                                               
RTM911   EQU   18                                                               
RTM1214  EQU   19                                                               
RTM1517  EQU   20                                                               
RTM1820  EQU   21                                                               
RTM2124  EQU   22                                                               
RTM2529  EQU   23                                                               
RTM3034  EQU   24                                                               
RTM3539  EQU   25                                                               
RTM4044  EQU   26                                                               
RTM4549  EQU   27                                                               
RTM5054  EQU   28                                                               
RTM5564  EQU   29                                                               
RTM65O   EQU   30                                                               
*                                                                               
RTOPEN   EQU   31                  <== OPEN DEMO BUCKET IN LIST                 
RTL1849  EQU   32                                                               
*                                                                               
RTWW1820 EQU   33                                                               
RTWW2124 EQU   34                                                               
RTWW2534 EQU   35                                                               
RTWW3544 EQU   36                                                               
RTWW4549 EQU   37                                                               
RTWW5054 EQU   38                                                               
RTWW55O  EQU   39                                                               
RTL50O   EQU   40                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* RI  =  INTERIM RECD DEMO DEFINITIONS                                          
*--NOTE--THIS SET OF BUCKET POSITION EQUATES MUST BE IN SYNC WITH THE           
*        DEMO DISPLACEMENT TABLE FOR THIS FILE.                                 
**********************************************************************          
*                                                                               
RIF1214  EQU   0                   W12-14                                       
RIF1517  EQU   1                   W15-17                                       
RIF1820  EQU   2                   W18-20                                       
RIF2124  EQU   3                   W21-24                                       
RIF2529  EQU   4                   W25-29                                       
RIF3034  EQU   5                   W30-34                                       
RIF3539  EQU   6                   W35-39                                       
RIF4044  EQU   7                   W40-44                                       
RIF4549  EQU   8                   W45-49                                       
RIF5054  EQU   9                   W50-54                                       
RIF5564  EQU   10                  W55-64                                       
RIF65O   EQU   11                  W65+                                         
*                                                                               
RIM1214  EQU   12                  M12-14                                       
RIM1517  EQU   13                  M15-17                                       
RIM1820  EQU   14                  M18-20                                       
RIM2124  EQU   15                  M21-24                                       
RIM2529  EQU   16                  M25-29                                       
RIM3034  EQU   17                  M30-34                                       
RIM3539  EQU   18                  M35-39                                       
RIM4044  EQU   19                  M40-44                                       
RIM4549  EQU   20                  M45-49                                       
RIM5054  EQU   21                  M50-54                                       
RIM5564  EQU   22                  M55-64                                       
RIM65O   EQU   23                  M65+                                         
*                                                                               
RIF911   EQU   24                  W9-11                                        
RIM911   EQU   25                  M9-11                                        
RIF68    EQU   26                  W6-8                                         
RIM68    EQU   27                  M6-8                                         
RIF25    EQU   28                  W2-5                                         
RIM25    EQU   29                  M2-5                                         
*                                                                               
RIHOMES  EQU   30                  HOMES (CABLE)                                
RIUSA    EQU   31                  HOMES (USA)                                  
RIL1849  EQU   32                  LOH1849   (LADY OF HOUSE)                    
RIL50O   EQU   33                  LOH50+                                       
RIWW1820 EQU   34                  WW1820                                       
RIWW2124 EQU   35                  WW2124                                       
RIWW2534 EQU   36                  WW2534                                       
RIWW3544 EQU   37                  WW3544                                       
RIWW4549 EQU   38                  WW4549                                       
RIWW5054 EQU   39                  WW5054                                       
RIWW55O  EQU   40                  WW55+                                        
*                                                                               
*DISPS TO PUTS AND UNIVERSES IN DEMO DISP TABLE                                 
*                                                                               
NDEMS    EQU   41                  TOTAL NUMBER DEMOS ON OUTPUT FILE            
RIUSASQ  EQU   NDEMS*4*1           DISP OF USA DEMOS INTO INTACCS               
RIPUTSQ  EQU   NDEMS*4*2           NUMBER DEMOS* 4BYTE BUCKETS                  
RIUNVSQ  EQU   NDEMS*4*3           *3  (AFTER PUTS)                             
*                                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
*SLOTTAB-TRANSLATE INPUT DEMOS TO INTERIM RECORD SLOTS:                         
*        DEMS STORED AT: INTACCS + 0                                            
*        PUTS STORED AT: INTACCS + RIPUTSQ                                      
*        UNVS STORED AT: INTACCS + RIUNVSQ                                      
**********************************************************************          
SLOTTAB  DS    0H                                                               
         DC    AL2(RTF25,RIF25)                                                 
         DC    AL2(RTM25,RIM25)                                                 
         DC    AL2(RTF68,RIF68)                                                 
         DC    AL2(RTM68,RIM68)                                                 
         DC    AL2(RTF911,RIF911)                                               
         DC    AL2(RTM911,RIM911)                                               
         DC    AL2(RTF1214,RIF1214)                                             
         DC    AL2(RTM1214,RIM1214)                                             
         DC    AL2(RTF1517,RIF1517)                                             
         DC    AL2(RTM1517,RIM1517)                                             
         DC    AL2(RTF1820,RIF1820)                                             
         DC    AL2(RTM1820,RIM1820)                                             
         DC    AL2(RTF2124,RIF2124)                                             
         DC    AL2(RTM2124,RIM2124)                                             
         DC    AL2(RTF2529,RIF2529)                                             
         DC    AL2(RTM2529,RIM2529)                                             
         DC    AL2(RTF3034,RIF3034)                                             
         DC    AL2(RTM3034,RIM3034)                                             
         DC    AL2(RTF3539,RIF3539)                                             
         DC    AL2(RTM3539,RIM3539)                                             
         DC    AL2(RTF4044,RIF4044)                                             
         DC    AL2(RTM4044,RIM4044)                                             
         DC    AL2(RTF4549,RIF4549)                                             
         DC    AL2(RTM4549,RIM4549)                                             
         DC    AL2(RTF5054,RIF5054)                                             
         DC    AL2(RTM5054,RIM5054)                                             
         DC    AL2(RTF5564,RIF5564)                                             
         DC    AL2(RTM5564,RIM5564)                                             
         DC    AL2(RTF65O,RIF65O)                                               
         DC    AL2(RTM65O,RIM65O)                                               
*                                                                               
         DC    AL2(RTWW1820,RIWW1820)                                           
         DC    AL2(RTWW2124,RIWW2124)                                           
         DC    AL2(RTWW2534,RIWW2534)                                           
         DC    AL2(RTWW3544,RIWW3544)                                           
         DC    AL2(RTWW4549,RIWW4549)                                           
         DC    AL2(RTWW5054,RIWW5054)                                           
         DC    AL2(RTWW55O,RIWW55O)                                             
*                                                                               
         DC    AL2(RTL1849,RIL1849)                                             
         DC    AL2(RTL50O,RIL50O)                                               
         DC    X'FFFF',X'FFFF'                                                  
         EJECT                                                                  
***********************************************************************         
* MKT BREAK TABLE SLOTS EQUATES                                                 
***********************************************************************         
         SPACE 2                                                                
*--------------------------------------------------------------------           
* TABLE OF INTERIM AREA SLOTS FOR MARKET BREAKS                                 
*--------------------------------------------------------------------           
CRUSA    EQU   0                  TOTAL USA                                     
         SPACE 2                                                                
*                                                                               
*--------------------------------------------------------------------           
* TABLE OF INTERIM AREA SLOTS FOR MARKET BREAKS                                 
*--------------------------------------------------------------------           
CIUSA    EQU   0                  TOTAL USA                                     
         SPACE 2                                                                
*                                                                               
*--------------------------------------------------------------------           
* TABLE OF OUTPUT SECTIONS FOR MARKET BREAKS                                    
*--------------------------------------------------------------------           
COUSA    EQU   1                  TOTAL USA                                     
         SPACE 2                                                                
*--------------------------------------------------------------------           
* CRCITAB -    TABLE TO CONVERT MARKET BREAKS TO INTERNAL SLOTS                 
*--------------------------------------------------------------------           
CRCITAB  DS    0H                                                               
         DC    AL2(CRUSA,CIUSA)    TOTAL USA                                    
         DC    X'FFFF',X'FFFF'                                                  
         SPACE 2                                                                
*--------------------------------------------------------------------           
* CRCOTAB -     TABLE TO CONVERT MARKET BREAKS TO OUTPUT SECTIONS               
*--------------------------------------------------------------------           
CRCOTAB  DS    0H                                                               
         DC    AL2(CIUSA,COUSA),X'00' TOTAL USA                                 
         DC    X'FFFF',X'FFFF',X'FF'                                            
         SPACE 2                                                                
*********************************************************************           
* CVGTBL  -    CABLE NET CODES AND THEIR COVERAGE SAMPLE CODES                  
*********************************************************************           
CVGTBL   DC    (NCBLS)XL4'0000'    4 CHAR COVERAGE SAMPLE SAVED                 
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
HPTMFDEM DS    CL(NDEMS*4)         M-F INTACCS DEMOS                            
HPTMSWGT DS    XL4                 M-SU NUMBER RECDS ACCRUED                    
HPTMSDEM DS    CL(NDEMS*4)         M-SU INTACCS DEMOS                           
HPTDLN   EQU   *-HPTD                                                           
*                                                                               
         EJECT                                                                  
*        DEMITD                                                                 
       ++INCLUDE DEMITD                                                         
         SPACE 1                                                                
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
         SPACE 1                                                                
*        DEINTCMITD                                                             
       ++INCLUDE DEINTCMITD                                                     
         EJECT                                                                  
*        DECALVPHD                                                              
       ++INCLUDE DECALVPHD                                                      
         EJECT                                                                  
*        DDDPRINT                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*                                                                               
*        ENDCODE                                                                
*        TITLE '- DEMO CONVERSION - NTI POCKETPIECE'                            
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIA                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIB                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIC                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTID                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIE                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIF                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIG                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIH                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTII                           
*        PROC  CONTROL=WRKD,ORDER=R/I/S/J,SUBORD=NTIJ                           
*        CODE                                                                   
         PRINT ON                                                               
*        DEDEMFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMCNVD                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMTABS                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105DECH07I   03/21/14'                                      
         END                                                                    
