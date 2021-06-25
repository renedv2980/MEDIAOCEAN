*          DATA SET DECNADI    AT LEVEL 113 AS OF 03/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECNADIA                                                                 
***********************************************************************         
         TITLE '- DEMO CONVERSION - CABLE NAD MONTHLY MIT'                      
*                                                                               
* IPHASE: DECNADI                                                               
* OPHASE: DECNADO                                                               
* ---------------                                                               
*                                                                               
* INPUT-OUTPUT LOGIC:                                                           
* ------------------                                                            
* CABLE NAD DATA COMES ON 2 SEPARATE FILES:                                     
*  - TIME PERIOD FILE - (TO BE PROCESSED FIRST)                                 
* '00' - REPORT DESCRIPTOR RECORD                                               
* '01' - UNIVERSES.  TOTAL US AND COVERAGE UNIVERSES          H-P-P             
* '02' - INTAB SAMPLE COUNTS. DON'T USE THEM                                    
* '05' - TIME PERIOD RECDS BY HALF HOUR                       H-P-P             
*  - PROGRAM FILE - (TO BE PROCESSED AFTER TIME PERIOD FILE)                    
* '04' - PROGRAM RECORDS. PRG DEMOS                           H-P-P             
*  -----                                                                        
* '03' - RESERVED FOR FUTURE USE                                                
*  ---------------------------------------                                      
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
DECNNDI  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DECNNDI,R3,R4,RA                                               
         USING DPRINT,R7                                                        
         ST    R7,SVR7                                                          
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC = INPUT RECD FROM TAPE (MITREC)           
         USING MITCND,RC                                                        
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
         L     RE,=A(DEMLNQ)       LENGTH OF DEMAREA                            
         A     RE,=A(DEMAREA)      GET A(PACKWT)                                
         ST    RE,APACKWT                                                       
         LA    RE,COMWRK           SAVE ADDR OF COMMON WRK AREA BETWN           
         ST    RE,ACOMWRK            INPUT AND OUTPUT PHASES                    
         MVI   NOTAVAL,0           DATA NOT AVAIL SWITCH                        
         MVI   MYMODE,X'FF'        SET TO 1ST-TIME-THRU (GET RDR 1ST)           
         OPEN  (IN1,(INPUT))                                                    
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS               
         BNE   READ20                                                           
         MVI   RELOFRST,0                                                       
*                                                                               
READ20   DS    0H                  ARE WE IN THE MID OF RELSG RECS              
         CLI   MYMODE,C'A'         UNIVERSE RECD RELEASE                        
         BE    UNVREL                                                           
         CLI   MYMODE,C'U'         USAGE RECD RELEASE                           
         BE    USGREL                                                           
         CLI   MYMODE,C'P'         PROGRAM RECD RELEASE                         
         BE    PRGREL                                                           
*        CLI   MYMODE,C'N'         PROG NAME RECD FOR TIME PERDS                
*        BE    PRGTIME             -CAN'T DO. COMES ON SEPARATE FILE-           
*        CLI   MYMODE,C'F'         RELEASE FUDGED TIME PRED RECDS               
*        BE    FUDGREL             -CAN'T DO. COMES ON SEPARATE FILE-           
*                                                                               
READ25   L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)       INSERT VARIABLE LENGTH RECORD                
         MVC   0(2,RE),=H'761'     HEADER                                       
         CLI   MYMODE,1            TEST FOR BYPASSING INPUT READ                
         BE    READ40                                                           
*                                                                               
RDTEST   GET   IN1,(RC)            GET NEXT RECORD                              
         MVC   OPTIONL,760(RC)                                                  
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
         MVC   SVVCR,VWTVCR                                                     
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
         CLC   MITSEQ,=C'02'       SAMPLE COUNTS - IGNORE                       
         BE    READ20                                                           
         CLC   MITSEQ,=C'03'       FOR FUTURE USE                               
         BE    READ20                                                           
*                                                                               
USG_TVU  DS    0H                  CABLE TVU TO GEOGRAPHIC MARKET               
         CLI   RELS,1              RELEASE LAST UNIVERSE RECD?                  
         BE    UNVREL                                                           
*                                                                               
PROGRAM  DS    0H                                                               
         CLC   MITSEQ,=C'04'       PROGRAM RECORDS?                             
         BNE   USG_BAS             NO                                           
         MVI   RECD,C'4'           THIS IS A PROGRAM RECD                       
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
         CLC   =C'CABLE NAD TIME PERIOD',MI0FILE                                
         BE    RDR5                                                             
         CLC   =C'CABLE NAD PROGRAM',MI0FILE                                    
         BE    RDR5                                                             
         DC    H'0'                UNKNOWN TAPE TYPE                            
RDR5     MVC   TAPEWK,MITSTART     START DATE EXCLUDING CENTURY                 
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   BOOK(1),4(R1)       YEAR                                         
         MVC   BOOK+1(1),8(R1)     BOOK                                         
         MVI   MYMODE,0            SET TO READ NEXT RECORD                      
         MVC   TAPEMKT,=H'321'     321 IS MARKET FOR THIS FILE                  
*                                                                               
* GET STORAGE FOR THE UNIVERSE BUFFER                                           
         LHI   R0,NDEMS            NUMBER OF DEMOS *                            
         MHI   R0,NMKTS            NUMBER OF MARKETS *                          
         MHI   R0,NCBLS            MAX NUMBER OF STATIONS *                     
         MHI   R0,4                4 BYTES FOR EA DEMO                          
         GETMAIN RU,LV=(0),BNDRY=PAGE                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                GETMAIN FAILURE - SEE RF FOR RC              
         ST    R1,AUNVBUFF         RETURNED A(UNIVERSE BUFFER)                  
*                                                                               
RDR10    MVI   INTAPESW,X'40'      DROP THIS RECORD                             
         CLI   RPRINT,0            TEST FOR PRINT OF RAT SER REC                
         BE    *+8                 NO                                           
         OI    INTAPESW,X'03'      YES-SET INDICATOR FOR CONTROLLER             
         B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* UNV -  SAVE AWAY UNIVERSES UNVBUFF.                                           
* *********************************************************************         
UNV      DS    0H                                                               
         CLC   MITTYPE(3),=C'UES'                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MITREC,C'H'         UNIVERSE RECORDS                             
         BE    *+6                                                              
         DC    H'0'                UNKNOWN RECD TYPE                            
*                                                                               
         CLI   UNVFRST,0           FIRST UNIVERSE RECORD?                       
         BE    UNV10               YES. NO UNIVERSES TO RELEASE YET             
         CLC   MITCOVG,PRVCOV      IF SAME COVERAGE,                            
         BE    UNV10                 DON'T RELEASE UNVS YET                     
         CLI   RELS,1              RELEASE UNVERSES FOR PREV COVERAGE?          
         BE    UNVREL              YES.                                         
*                                                                               
UNV10    MVC   PRVCOV,MITCOVG      RELEASE ALL MKT BRKS AT ONCE                 
         MVI   UNVFRST,1                                                        
         BAS   RE,HDR              PROCESS HDR INFOR. SET MKTBRK                
         ZIC   R1,COVSMPD          CVG DISP IN TABLE                            
         MH    R1,CVGLN            DISP OF CVG INTO BUFFER                      
         A     R1,AUNVBUFF         SAVE IN UNVBUFF                              
         ST    R1,DMCB             DMCB=A(CVG SAVE AREA IN UNVBUFF)             
         LA    R1,MIXDEM           1ST DEMO IN LIST                             
         ST    R1,DMCB+4           A(1ST DEMO ON INPUT)                         
         BAS   RE,SLOTDEM          PROCESS DEMOS-SLOT IN BUFFER                 
*                                                                               
         MVI   RELS,1              SET FLAG TO RELEASE UNV RECD                 
*                                                                               
         LA    RE,INTKEY                                                        
         LA    RF,1500                                                          
         XCEF                                                                   
         MVI   INTRTYP,C'P'        TIME PERIOD P-RECD                           
         MVC   INTBOOK,BOOK                                                     
         MVC   INTIBOOK,IBOOK                                                   
         MVC   INTSTA(4),COVALPH                                                
         MVI   INTSTA+4,C'U'       UNIVERSE RECD                                
         MVC   INTMRKT,TAPEMKT                                                  
         MVC   SAVEINT(L'SAVEINT),INTVALS                                       
*                                                                               
UNVX     B     READ20             READ NEXT RECORD                              
         EJECT                                                                  
**********************************************************************          
*UNVREL-RELEASE UNIVERSE RECDS                                                  
**********************************************************************          
UNVREL   DS    0H                                                               
         MVI   MYMODE,C'A'         RELEASE UNV MODE                             
         BAS   RE,GENCATS                                                       
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
*                                                                               
UNVRL20  BAS   RE,LDCRCO           GET CORRECT CRCOTAB INTO FULL                
         L     RE,FULL                                                          
UNVRL22  CLI   0(RE),X'FF'                                                      
         BE    UNVRLEND                                                         
         CLI   0(RE),X'F0'         END OF SUB-TABLE?                            
         BNE   *+8                                                              
         LA    RE,7(RE)            YES.GO PAST HEADER OF NEXT SUB-TABLE         
         B     UNVRL25                                                          
*                                                                               
UNVRLEND MVI   RELS,0              NOTHING TO RELEASE                           
         MVI   RELSLOT,0                                                        
         MVI   MYMODE,0                                                         
         CLI   RECD,C'Z'           END OF FILE?                                 
         BE    ENDJOB                                                           
         LA    RE,OPTBUFF          CLEAR OPTIONAL BITS BUFFER                   
         LA    RF,OPTBUFLN                                                      
         XCEF                                                                   
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
         MVI   INTBTYP+1,0                                                      
         ZIC   R1,RELSLOT                                                       
         LA    RF,OPTBUFF                                                       
         AR    RF,R1                                                            
         CLI   0(RF),0             IF ENTRY NOT EMPTY                           
         BE    UNVREL40                                                         
         CLI   0(RF),C'2'          CHECK OPTIONAL BYTE                          
         BNE   *+12                                                             
         MVI   INTBTYP+1,C'O'                                                   
         B     UNVREL40                                                         
         CLI   0(RF),C'4'                                                       
         BNE   *+12                                                             
         MVI   INTBTYP+1,C'Q'                                                   
         B     UNVREL40                                                         
         CLI   0(RF),C'0'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UNVREL40 MVI   0(RF),0              CLEAR FIELD WHEN DONE                       
         XC    INTPNUM,INTPNUM                                                  
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         LA    R6,INTACCS                                                       
         CLC   COVALPH,=C'0000'                                                 
         BE    *+12                R6=DESTN IN INTERIM REC FOR UNIVS            
         AH    R6,=Y(RIUNVSQC)     BY COVERAGE AREA                             
         B     *+8                                                              
         AH    R6,=Y(RIUNVSQ)      FOR TOTAL US                                 
         ZIC   RE,COVSMPD          GET UNIVS FOR COVG SAMPLE                    
         MH    RE,CVGLN            BUMP TO CVG IN UNVBUFF                       
         ZIC   RF,RELSLOT          WHICH MKT BRK IS THIS                        
         MH    RF,MKTDISP                                                       
         AR    RE,RF                                                            
         A     RE,AUNVBUFF         RE=ADDRESSS OF UNIVS IN UNVBUFF              
         LH    RF,=Y(NDEMS*4)                                                   
         LH    R7,=Y(NDEMS*4)                                                   
         MVCL  R6,RE                                                            
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
*                                                                               
         CLC   COVALPH,=C'0000'    TOTAL SAMPLE?                                
         BNE   *+10                                                             
         MVC   INTSTA(4),=C'UUUU'    SET STN TO UNIV                            
         BAS   RE,BLDKEY           BUILD INTERIM RECD KEY                       
         B     RELEASE             RETURN TO CTLR TO RELEASE RECD               
         EJECT                                                                  
* *********************************************************************         
* TVUREC -HOUSEHOLD USAGE RECORD                                                
*        ONE RECORD PER 1/2HR FOR EACH DAY                                      
*        FOR EACH COVERAGE SAMPLE                                               
*              RECORD SEQUENCE CODE = 5                                         
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
         CLC   PRVKEY(MITCOVCL-MITKEY),MITKEY    TEST OTHER KEY FIELDS          
         BE    TVU10               SAME -SAVE MKT BREAK/DON'T RELS              
*                                                                               
TVU5     CLI   RELS,5              RELEASE PREVIOUS '05' USAGE RECD?            
         BE    USGREL              FINISH BLDING USAGE RECD & RELEASE           
*                                                                               
TVU10    MVI   RELS,5              CABLE VIEWING RECD                           
         MVC   PRVMKT,MITMKTBR     SAVE MKT BREAK                               
         MVC   PRVHLF,MITHLFID     SAVE HLF HR ID                               
         MVC   PRVKEY,MITKEY       SAVE OTHER KEY FIELDS                        
*                                                                               
         CLC   MITMKTBR,=C'000'                                                 
         BNE   TVU15                                                            
         L     RE,=A(DEMAREA)      CLEAR DEMAREA TO START OVER                  
         L     RF,=A(DEMLNQ)       LENGTH OF DEMAREA                            
         XCEF                                                                   
*                                                                               
TVU15    LA    RE,INTKEY                                                        
         LA    RF,1500                                                          
         XCEF                                                                   
         MVC   INTFEED,MITFEED     FEED PATTERN                                 
         MVC   INTAUDES,MITAUDTY   AUD EST TYPE                                 
         MVC   INTCOVSM,MITCOVG    COVERAGE SAMPLE                              
         MVC   INTCOVCL,MITCOVCL   COV SAMPL CALC IND                           
*                                                                               
         BAS   RE,HDR              PROCESS HDR INFO.MKTBMIT GETS ITS            
*                                   VALUE FROM MIT                              
*        CLI   MKTBMIT,0                                                        
*        BNE   *+8                                                              
         BAS   RE,H_REC            PROCESS 'H' RECD                             
         MVI   INTDTYP,X'09'       DUMMY TO REG-ORIG (FIELD IS N/A)             
         MVC   INTSTA(4),COVALPH   USAGE TO GEOGRAPHIC MKT                      
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
*USGREL- RELEASE REGULAR USAGE RECORDS. MYMODE='U' RELS=5                       
***********************************************************************         
USGREL   DS    0H                                                               
         MVI   MYMODE,C'U'         OUTPUT DEMS FOR THIS USAGE RECD              
         BAS   RE,GENCATS                                                       
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
*                                                                               
USG20    BAS   RE,LDCRCO           GET CORRECT CRCOTAB INTO FULL                
         L     RE,FULL                                                          
USG22    CLI   0(RE),X'FF'                                                      
         BE    USGEND                                                           
         CLI   0(RE),X'F0'         END OF SUB-TABLE?                            
         BNE   *+8                                                              
         LA    RE,7(RE)            YES.GO PAST HEADER OF NEXT SUB-TABLE         
         B     USG25                                                            
*                                                                               
USGEND   MVI   RELS,0              NOTHING TO RELEASE                           
         MVI   RELSLOT,0                                                        
         MVI   SUMSLOT,0                                                        
         MVI   MYMODE,0                                                         
         CLI   RECD,C'Z'           END OF FILE?                                 
*        BE    USG23               RELEASE AVGS BEFORE EXITING                  
         BE    ENDJOB                                                           
         LA    RE,OPTBUFF          CLEAR OPTIONAL BITS BUFFER                   
         LA    RF,OPTBUFLN                                                      
         XCEF                                                                   
         B     READ40                                                           
         B     READ40                                                           
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
         MVI   INTBTYP+1,0                                                      
         ZIC   R1,RELSLOT                                                       
         LA    RF,OPTBUFF                                                       
         AR    RF,R1                                                            
         CLI   0(RF),0             IF ENTRY NOT EMPTY                           
         BE    USG40                                                            
         CLI   0(RF),C'2'          CHECK OPTIONAL BYTE                          
         BNE   *+12                                                             
         MVI   INTBTYP+1,C'O'                                                   
         B     USG40                                                            
         CLI   0(RF),C'4'                                                       
         BNE   *+12                                                             
         MVI   INTBTYP+1,C'Q'                                                   
         B     USG40                                                            
         CLI   0(RF),C'0'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
USG40    MVI   0(RF),0                CLEAR FIELD AFTER USE                     
         XC    INTPNUM,INTPNUM                                                  
         MVC   INTPNUM+1(1),INTSQH    1/4 HR ID                                 
         ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,MKTDISP          MKT BRK DISPLACEMENT LENGTH                  
         A     RE,=A(DEMAREA)                                                   
*        L     RE,ASVDEMS          A(MKT BRK DEMOS) IN BUFFER                   
         LA    R1,INTACCS                                                       
         LH    RF,=Y(NDEMS*4)      RF=NUMBER BYTES TO MOVE                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),0(RE)      SAVE DEMOS IN INTACCS                        
*                                                                               
         ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
         XC    INTRSF,INTRSF       NOT DEFINED FOR TIME PERD RECDS              
         CLC   INTSTA(4),=C'0000'  USA LEVEL IS HUT STATION                     
         BNE   USG60                                                            
         MVC   INTSTA(4),=C'HUT '                                               
*                                                                               
USG60    BAS   RE,BLDKEY           BUILD INTERIM RECD KEY                       
         B     RELEASE                                                          
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
*        MVI   PRBTYP,0                                                         
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
*        MVI   PMBTYP,0                                                         
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
         STH   R1,MKTBMIT                                                       
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
*        FOR EACH COVERAGE SAMPLE. APPLIES TO REC SEQ CODES = 5                 
* *********************************************************************         
*                                                                               
H_REC    NTR1                                                                   
         XC    INTPNAME,INTPNAME                                                
*                                                                               
*DAYS OF WEEK NOT PROVIDED ON CABLE NAD MIT. COMPUTE THEM IN                    
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
         MVI   AVGWKS,0                                                         
         MVC   INTBOOK,BOOK        DEFAULT                                      
*                                                                               
*        CLC   MITAVG,=C'00'       INDIVIDUAL DAY AVG OR SINGLE TELC            
*        BNE   H_REC29                                                          
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
*                                                                               
         CLC   HALF,BOOK           HAS TO BE CURRENT MONTH                      
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   MITAVG,=C'00'       INDIVIDUAL DAY AVG OR SINGLE TELC            
         BNE   H_REC29                                                          
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
*                      OPTIONL- OPTIONAL BYTE                                   
*                      MKTBMIT - MKT BREAK                                      
*                      USHUT  - US HUT PROJECTION                               
*                      MKTHUT - MKT BREAK HUT PROJECTION                        
* *********************************************************************         
*                                                                               
SLOTDEM  NTR1                                                                   
         L     R1,=A(CRCITAB)      FIND MKT BRK IN TABLE                        
SLOT5    CLC   MKTBMIT,0(R1)                                                    
         BE    SLOT10                                                           
         LA    R1,L'CRCITAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   SLOT5                                                            
         DC    H'0'                                                             
SLOT10   ZIC   RE,L'CRCITAB-1(R1)                                               
         LA    RF,OPTBUFF          OPTIONAL BYTE                                
         AR    RF,RE                                                            
         MVC   0(L'OPTIONL,RF),OPTIONL                                          
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
*                                                                               
SLOTX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*GENCATS- GENERATE MISSING BOOK CATAGORIES                                      
**********************************************************************          
GENCATS  NTR1                      GENERATE MISSING BOOK CATS                   
         L     R9,=A(CIGENTAB)                                                  
GENCATS2 CLI   0(R9),X'FF'                                                      
         BE    GENCATSX                                                         
         SR    R7,R7                                                            
         ICM   R7,3,0(R9)                                                       
         MH    R7,SLOTLN                                                        
         CLI   MYMODE,C'A'                                                      
         BNE   GENCATS5                                                         
         ZIC   R0,COVSMPD           BUMP TO COVERAGE                            
         MH    R0,CVGLN                                                         
         AR    R7,R0                                                            
         A     R7,AUNVBUFF                                                      
         B     *+8                                                              
GENCATS5 A     R7,=A(DEMAREA)                                                   
         XC    0(NDEMS*4,R7),0(R7)                                              
         SR    R1,R1                                                            
         ICM   R1,3,2(R9)            SOURCE SLOTS                               
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,4(R9)                                                       
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,6(R9)                                                       
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,8(R9)                                                       
         BAS   RE,SUMIT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,10(R9)                                                      
         BAS   RE,SUMIT                                                         
         LA    R9,12(R9)                                                        
         B     GENCATS2                                                         
GENCATSX XIT1                                                                   
**********************************************************************          
*SUMIT - ADDING CATAGORIES FOR GENCATS ROUTINE                                  
**********************************************************************          
SUMIT    LTR   R1,R1               NOT ACTIVE                                   
         BZR   RE                                                               
         LA    R6,0                                                             
         LA    R0,NDEMS                                                         
         MH    R1,SLOTLN                                                        
         CLI   MYMODE,C'A'                                                      
         BNE   SUMIT0                                                           
         ZIC   RF,COVSMPD                                                       
         MH    RF,CVGLN                                                         
         AR    R1,RF                                                            
         A     R1,AUNVBUFF                                                      
         B     *+8                                                              
SUMIT0   A     R1,=A(DEMAREA)                                                   
         MVC   USAHOME,RIUSA*4(R1)                                              
*                                                                               
SUMIT1   DS    0H                  ADD  CATS TOGETHER                           
         LR    R5,R7                                                            
         AR    R5,R6                                                            
         ICM   RF,15,0(R5)         PICK UP RUNNING TOTAL                        
         A     RF,0(R1,R6)         ADD TO CURRENT CAT                           
         STCM  RF,15,0(R5)         UPDATE RUNNING TOTAL                         
SUMIT2   LA    R6,4(R6)                                                         
         BCT   R0,SUMIT1                                                        
         MVC   RIUSA*4(4,R7),USAHOME  SLOT HOMES (USA HOMES NOT ADDED)          
         BR    RE                                                               
         EJECT                                                                  
* *********************************************************************         
* PRGREC-PROGRAM RECORD PROCESSING                                              
*              RECORD SEQUENCE CODE = 4                                         
*              RECORD TYPE          = D - P- P                                  
* *********************************************************************         
PRGREC   DS    0H                                                               
*                                                                               
*        DC    H'0'                PATCH TO PREVENT PRG LOADS                   
*                                                                               
         CLI   MITREC,C'D'         PROCESS PROGRAM DESCRIPTOR REC (PDR)         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRG1     CLI   MITORIG,C'0'        BYPASS                                       
         BE    PRG3                IF CORRECTION DATE > START DATE              
*IGNORE CORRECTIONS. TO GET MORE INFO FROM NEILSEN                              
         B     READ20                                                           
         CLC   MITCORDT,MITSTDTC                                                
         BH    READ20                                                           
*                                                                               
PRG3     CLC   MITTELC,ZEROS       ALL TELECASTS SHOULD BE BREAKOUTS            
         BE    PRG4                OR SECIALS                                   
         CLI   MITBREAK,C'1'                                                    
         BE    PRG4                                                             
         CLI   MITSPC,C'1'                                                      
         BE    PRG4                                                             
         DC    H'0'                NOT A BREAKOUT OR A SPECIAL - ERROR          
*                                                                               
PRG4     CLI   RELS,0              NOTHING TO RELEASE-JUST PROCESS              
         BE    PRG8                                                             
         CLC   PRVMKT,MITMKTBR     TEST MKT BREAK?                              
         BE    PRG5                SAME MKT BREAK-> DIFFERENT RECD              
         CLC   PRVKEY(MITCOVCL-MITKEY),MITKEY    TEST OTHER KEY FIELDS          
         BE    PRG10               SAME -SAVE MKT BREAK/DON'T RELS              
PRG5     CLI   RELS,4              RELEASE PREVIOUS PRG RECD?                   
         BNE   *+8                 FINISH BLDING USAGE RECD & RELEASE           
         B     PRGREL              RELEASE PRG RECD                             
*                                                                               
PRG8     DS    0H                                                               
         CLC   MITMKTBR,=C'000'                                                 
         BNE   PRG10                                                            
         L     RE,=A(DEMAREA)      CLEAR DEMAREA TO START OVER                  
         L     RF,=A(DEMLNQ)       LENGTH OF DEMAREA                            
         XCEF                                                                   
*                                                                               
PRG10    LA    RE,INTKEY           CLEAR KEY                                    
         LA    RF,1500                                                          
         XCEF                                                                   
         MVC   INTKSRC,KEYSRC                                                   
         MVI   RELS,4                                                           
         MVC   PRVKEY,MITKEY       SAVE KEY                                     
         BAS   RE,HDR              PROCESS HDR INFO                             
*        CLI   MKTBMIT,0                                                        
*        BNE   *+8                                                              
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
**********************************************************************          
* GET ADDRESS OF OUTPUT SLOT TABLE BY BOOK                                      
**********************************************************************          
                                                                                
LDCRCO   NTR1                                                                   
         L     RE,=A(CRCOTAB)                                                   
                                                                                
LDCRCO1  CLC   BOOK,0(RE)          START WEEK OF THE TAPE                       
         BNL   LDCRCO5             USE IT                                       
         OC    2(4,RE),2(RE)       LAST TABLE?                                  
         BZ    LDCRCO5             USE IT                                       
         ICM   RF,15,2(RE)         LENGTH IF TABLE                              
         AR    RE,RF               ADVANCE TO NEXT TABLE                        
         B     LDCRCO1                                                          
                                                                                
LDCRCO5  LA    RE,6(RE)            GO PAST BOOK AND LENGTH                      
         ST    RE,FULL                                                          
         XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* PDRREC - PROGRAM DESCRIPTOR RECORD EXTRACT FIELDS FOR INTERIM RECD            
*        ONLY PROCESS PDRS FOR TOTAL SAMPLE FOR EACH PROGRAM                    
*        IGNORE PDRS FOR EACH MKT BREAK                                         
* ********************************************************************          
*                                                                               
PDRREC   NTR1                                                                   
*SET INTSTYP FOR BREAKOUTS AND SPECIALS.                                        
*IF BOTH A BKOUT AND A SPEC, SET STYP TO SPECIAL                                
PDR3     MVI   INTSTYP,0                                                        
         MVC   INTKSRC,KEYSRC                                                   
         CLI   MITBREAK,C'1'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,C'B'                                                     
         CLI   MITSPC,C'1'                                                      
         BNE   PDR5                                                             
         MVI   INTSTYP,C'S'                                                     
PDR5     MVI   NOTAVAL,0           READ PROG RECDS                              
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
*                                                                               
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY     CORECTN BOOK           
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
*                                                                               
         CLC   MITCORDT,BLANKS                                                  
         BNE   *+16                                                             
         CLC   HALF,BOOK           NO CORRECTION DATE                           
         BNL   *+6                                                              
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
PDR20    CLI   0(RE),C'1'                                                       
         BNE   PDR25                                                            
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
         CLC   MITAVG,=C'00'       INDIVIDUAL DAY DATA?                         
         BNE   PDR35               NO,  AVERAGE DATA                            
*                                                                               
*--INDIVIDUAL DAYS: 00 WEEKS                                                    
         CLI   MITBREAK,C'1'       BREAKOUT?                                    
         BE    PDR60               YES, DIFFERENT PROCESSING                    
         CLI   MITSPC,C'1'         SPECIAL?                                     
         BE    PDR60                                                            
*                                                                               
         LA    RE,MIDDAYS          --INDIV DAY DATA (NOT BRKOUT)--              
         LA    RF,DAYS             SET ON POINTERS FOR INDIVIDUAL DAYS          
         LA    R0,7                (NEEDED FOR PROCESSING VARIOUS)              
PDR27    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PDR27                                                         
         B     PDR60                                                            
*                                                                               
*--AVERAGES: 04,05 WEEKS                                                        
PDR35    CLC   MIDDAYS,=C'1111100'  M-F?                                        
         BE    PDR60                                                            
         CLC   MIDDAYS,=C'1111111'  M-S?                                        
         BE    PDR60                                                            
         LA    RE,MIDDAYS          MULTI WEEK-SEE IF MORE THAN 1 DAY            
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CH    RF,=H'1'            MULTI WK AVG ACROSS 1-DAY?                   
         BE    PDR60               YES, TREAT LIKE  M-F/M-S RECDS               
*                                                                               
PDR40    DS    0H                                                               
         CLI   VARIOUS,1                                                        
         BE    PDR50                                                            
         MVI   VARIOUS,1           VARIOUS DAYS                                 
         MVC   VARS,ZEROS          1ST TIME THRU                                
         XC    VARSTIME(56),VARSTIME                                            
*                                                                               
         CLC   DAYS,ZEROS          IF NOT YET SET, JUST GO SET IT               
         BE    PDR50               VARS W/OUT INDIV DAY RECDS                   
         LA    RE,DAYS             TEST NUMBER OF DAYS IN AVERAGE               
         SR    RF,RF                                                            
         LA    R0,7                                                             
         CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         CH    RF,=H'1'            IGNORE AVG WITH ONLY 1 DAY IN IT             
         BH    PDR50                                                            
         MVI   VARIOUS,0                                                        
*        OC    INTVAR(56),INTVAR    FOR MITAVG=01 -> NEVER IN CNAD              
*        BNZ   PDRX                                                             
*?       MVI   BYPASS01,1                                                       
*        MVC   SAVEDATE,MITSTART   SAVE START-END DATES                         
         B     PDRX                                                             
*                                                                               
PDR50    LA    RE,MIDDAYS                                                       
         LA    RF,VARS                                                          
         LA    R0,7                                                             
PDR55    CLI   0(RE),C'1'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PDR55                                                         
*        CLC   MITTYPE(3),=C'SYN'                                               
*        BNE   *+10                                                             
*        MVC   DAYS,VARS                                                        
         CLC   DAYS,ZEROS                                                       
         BNE   *+10                                                             
         MVC   DAYS,VARS                                                        
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
         MVC   INTMRKT,TAPEMKT     CABLE NAD MKT=                               
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
*                                   NO LIBRARY ID FOR CABLE NAD                 
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
         CLC   MIDREP,1(RE)                                                     
         BE    PDR85                                                            
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,PDR80                                                         
         DC    H'0'                BLOW UP IF TYPE IS NOT IN TABLE              
*                                                                               
PDR85    MVC   PWK1DTYP,2(RE)      X'09'=REGUALR  X'0C'=SPECIAL                 
*        CLC   MITAVG,=C'00'     INDIV DAY REC?                                 
*        BNE   PDR90               NO, AN AVG RECORD                            
*C       CLI   MIDMULT,C' '        MULTI-DAYS?- NOT ON CNAD                     
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
*        MVI   INTSTYP,0                                                        
         MVI   INTMTYP,0                                                        
*C       MVC   INTAUD,PWK1AUD                                                   
         MVC   INTDTYP,PWK1DTYP                                                 
         MVC   INTRSF,PWK1RSF                                                   
*                                                                               
         MVC   INTBOOK,BOOK        DEFAULT: BOOK=FIRST WEEK OF MONTH            
         MVC   INTIBOOK,IBOOK         "       "     "                           
*                                                                               
         GOTO1 VNETWEEK,DMCB,MITSTART,VGETDAY,VADDAY                            
         MVC   HALF(1),4(R1)       NETWORK YEAR                                 
         MVC   HALF+1(1),8(R1)     NETWORK WEEK                                 
*                                                                               
         CLC   MITCORDT,BLANKS                                                  
         BNE   *+16                                                             
         CLC   HALF,BOOK           NO CORRECTION DATE                           
         BNL   *+6                                                              
         DC    H'0'                AND DATE OUTSIDE CURRENT BOOK                
*                                                                               
         CLC   MITAVG,=C'00'       FOR IND DAY DATA, KEEP GIVEN WEEK            
         BNE   PDR100                                                           
         MVC   INTBOOK,HALF        BOOK SAVED AS YEAR & NETWORK WEEK            
         MVC   INTIBOOK,HALF       BOOK SAVED AS YEAR & NETWORK WEEK            
*                                                                               
PDR100   MVI   RELS,4                                                           
         LA    RE,MIDDAYS          POINT TO DAY FIELDS                          
PDR105   LA    R0,7                COUNTER                                      
         LA    R5,X'40'            START AT MONDAY                              
         SR    RF,RF               CLEAR CUMULATIVE BITS                        
*                                                                               
PDR110   CLI   0(RE),C'1'          TEST FOR NO ACTIVITY                         
         BNE   *+6                                                              
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
*                                                                               
**       CLC   MITAVG,=C'04'      SET FOR IND TELECASTS TOO                     
*    --MULTI-WEEK ONLY (04/05)--  SET ACTIVE WEEKS NIBBLE                       
**       BL    PDR120              NO (00/01)                                   
         LA    R1,MIDWKS                                                        
         CLI   0(R1),C'0'          WEEK 1                                       
         BE    *+12                                                             
         OI    PWKBIT,X'08'                                                     
         OI    PWKBIT2,X'80'                                                    
         CLI   1(R1),C'0'          WEEK 2                                       
         BE    *+12                                                             
         OI    PWKBIT,X'04'                                                     
         OI    PWKBIT2,X'40'                                                    
         CLI   2(R1),C'0'          WEEK 3                                       
         BE    *+12                                                             
         OI    PWKBIT,X'02'                                                     
         OI    PWKBIT2,X'20'                                                    
         CLI   3(R1),C'0'          WEEK 4                                       
         BE    *+12                                                             
         OI    PWKBIT2,X'10'                                                    
         OI    PWKBIT,X'01'                                                     
         CLI   4(R1),C'0'          WEEK 5                                       
         BE    *+8                                                              
         OI    PWKBIT2,X'08'                                                    
         OC    INTDAYWK,PWKBIT                                                  
         MVC   INTWEEKS,PWKBIT2                                                 
*                                                                               
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
         CLC   MITAVG,=C'00'       ---INDIVIDUAL DAY DATA ONLY----              
         BNE   PDR140                                                           
         MVC   VARS,ZEROS                                                       
         LA    RE,MIDDAYS          USE INDIVIDUAL DAY(S) FOR INTVAR             
         LA    RF,SAVVAR           SET UP START TIMES IN VAR SAVE AREA          
         LA    R0,7                                                             
PDR130   CLI   0(RE),C'1'                                                       
         BNE   PDR135                                                           
         MVC   0(1,RF),INTSQH      START QH                                     
         MVC   1(1,RF),INTEQH      END QH                                       
         PACK  DUB,MIDDUR                                                       
         CVB   R1,DUB              DURATION IN MINUTES                          
         CH    R1,=H'3600'         2-BYTE DURATION                              
         BNH   *+8                 60 HRS MAX TO FIT #OF QH'S IN 1 BYTE         
         LH    R1,=H'3600'         (SHOULD NEVER REACH THIS HIGH!)              
         STCM  R1,3,7(RF)          KEEP FULL DURATION - 2 BYTE                  
         CH    R1,=H'240'                                                       
         BNH   *+8                                                              
         LH    R1,=H'240'                                                       
         STC   R1,2(RF)            DURATION IN MINUTES < 240                    
*                                  (4 HRS WAS MAXIMUM SUPPORTED)                
         MVC   3(2,RF),INTSTIM     START TIME                                   
         MVC   5(2,RF),INTETIM     END TIME                                     
PDR135   LA    RE,1(RE)                                                         
*        LA    RF,7(RF)                                                         
         LA    RF,L'SAVVAR(RF)                                                  
         BCT   R0,PDR130                                                        
*        CLC   MITTYPE(3),=C'SYN'  CAN BE VAR FOR SYND.                         
*        BNE   PDR170                                                           
         B     PDR170                                                           
*                                                                               
PDR140   CLI   VARIOUS,1           VARIOUS--FOR AVGS                            
         BNE   PDR170                                                           
         LA    RE,SAVETIME         ACCUMULATE INDIVIDUAL VAR DAY(S)             
         LA    RF,VARSTIME             INTO VARSTIME                            
         LA    R0,7                                                             
PDR145   OC    0(8,RE),0(RE)                                                    
         BZ    *+10                                                             
         MVC   0(8,RF),0(RE)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,PDR145                                                        
*                                                                               
         CLC   VARS,DAYS           HAVE ALL VARIOUS DAYS PROCESSED              
         BNE   PDR170                                                           
         MVI   VARIOUS,0           RESET                                        
         MVC   DAYS,ZEROS          RESET                                        
*        MVC   INTVAR(49),SAVVAR                                                
*        XC    SAVVAR(49),SAVVAR                                                
         MVC   INTVAR(63),SAVVAR                                                
         XC    SAVVAR(63),SAVVAR                                                
         MVC   SAVETIME(56),VARSTIME                                            
         XC    VARSTIME(56),VARSTIME                                            
         LA    RE,VARS             GENERATE VAR DATA USING                      
         B     PDR105                  ACCUMULATED DAYS                         
*                                                                               
PDR170   MVC   INTDAYWK,PDAY1                                                   
         OC    INTDAYWK,PWKBIT                                                  
         MVC   INTWEEKS,PWKBIT2                                                 
         MVC   INTDYBIT,PDAY1BIT                                                
         MVC   SAVEINT,INTVALS     SAVE FOR H4RTN/P4RTN                         
*                                                                               
PDRX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*PRGREL -      RELEASE PROGRAM RECORDS                                          
***********************************************************************         
PRGREL   DS    0H                                                               
         MVI   MYMODE,C'P'                                                      
         BAS   RE,GENCATS                                                       
         MVC   INTVALS(L'SAVEINT),SAVEINT                                       
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         XC    P(132),P                                                         
*                                                                               
PRGREL10 BAS   RE,LDCRCO           GET CORRECT CRCOTAB INTO FULL                
         L     RE,FULL                                                          
PRGREL13 CLI   0(RE),X'FF'                                                      
         BE    PRGRLEND                                                         
         CLI   0(RE),X'F0'         END OF SUB-TABLE?                            
         BNE   *+8                                                              
         LA    RE,7(RE)            YES.GO PAST HEADER OF NEXT SUB-TABLE         
         B     PRGREL15                                                         
*                                                                               
PRGRLEND MVI   RELS,0              CLEAR PREV PRG RECD SWITCH                   
         MVI   MYMODE,0                                                         
         MVI   RELSLOT,0                                                        
         CLI   RECD,C'Z'           END OF TAPE                                  
         BE    ENDJOB                                                           
         LA    RE,OPTBUFF          CLEAR OPTIONAL BITS BUFFER                   
         LA    RF,OPTBUFLN                                                      
         XCEF                                                                   
         B     READ40              PROCESS CURRENT RECD IN BUFFER               
*                                                                               
PRGREL15 CLC   1(1,RE),RELSLOT                                                  
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
         MVI   INTBTYP+1,0                                                      
         ZIC   R1,RELSLOT                                                       
         LA    RF,OPTBUFF                                                       
         AR    RF,R1                                                            
         CLI   0(RF),0             IF ENTRY NOT EMPTY                           
         BE    PRGREL25                                                         
         CLI   0(RF),C'2'          CHECK OPTIONAL BYTE                          
         BNE   *+12                                                             
         MVI   INTBTYP+1,C'O'                                                   
         B     PRGREL25                                                         
         CLI   0(RF),C'4'                                                       
         BNE   *+12                                                             
         MVI   INTBTYP+1,C'Q'                                                   
         B     PRGREL25                                                         
         CLI   0(RF),C'0'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRGREL25 ZIC   RE,MKTBRK                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   SAVEINT,INTVALS     UPDATE SAVEINT                               
*                                                                               
         CLI   INTORIG,C'2'                                                     
         BE    PRGREL28                                                         
*                                                                               
         LA    R6,INTACCS                                                       
         LH    R7,=Y(NDEMS*4)                                                   
         ZIC   RE,RELSLOT          MOVE DEMOS TO INTERIM RECD                   
         MH    RE,MKTDISP                                                       
         A     RE,=A(DEMAREA)                                                   
         LH    RF,=Y(NDEMS*4)                                                   
         MVCL  R6,RE                                                            
*                                                                               
PRGREL28 ZIC   RE,RELSLOT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,RELSLOT                                                       
*                                                                               
PRGREL30 BAS   RE,PRGKEY                                                        
         LA    RE,INTKEY                                                        
         USING QPRGKD,RE           PROGRAM KEY DSECT                            
         CLI   INTORIG,C'2'        DELETE REQUEST                               
         BNE   RELEASE                                                          
         OC    QFILNUM,QFILNUM                                                  
         BNZ   RELEASE                                                          
         MVI   RELS,0                                                           
         MVI   MYMODE,0                                                         
         MVI   RELSLOT,0                                                        
         CLI   RECD,C'Z'                                                        
         BE    ENDJOB                                                           
         B     READ40              IF RECD NOT ON FILE,DROP DELETE RQST         
*        B     READ20                                                           
         DROP  RE,R9                                                            
         EJECT                                                                  
* *********************************************************************         
* PRGKEY-  SPECIAL KEY FOR PROGRAM RECDS TO FORCE PROPER SORT FOR               
*          '1F' ELEMENTS WHEN RECORDS GET PASSED TO OUTPUT PHASE.               
* *********************************************************************         
PRGKEY   NTR1                                                                   
         LA    RF,INTKEY                                                        
         USING QPRGKD,RF           PROGRAM KEY DSECT                            
         XC    INTKEY,INTKEY                                                    
         MVI   QCODE,QCODEQ        FUDGE PRG CODE AS 'Q' RECORD                 
         MVC   INTKSRC,KEYSRC                                                   
*                                                                               
PRGK10   MVC   QBOOK,INTBOOK                                                    
         MVC   QNET,INTANET        NETWORK                                      
         MVC   QNTINUM,INTPNUM     NTI PROGRAM NUMBER (PWOS)                    
         MVC   QSTYP,INTSTYP                                                    
         MVC   QTRK,INTTRK         SAVE TRACKAGE                                
         MVI   QAVG,1              INDIV DAY DATA                               
         CLI   INTAVG,0                                                         
         BE    *+8                                                              
         MVI   QAVG,0              MONTHLY AVG (SORT BEFORE DAILY DATA)         
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
         MVC   PJEXTNUM(1),INTSTYP                                              
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
**********************************************************************          
CNVWR    DS    0H                                                               
         MVI   BYPSORT,0           RELEASE RECD                                 
         L     R2,ASREC                                                         
         CLI   CNV1ST,0            1ST TIME IN CNVWR ROUTINE                    
         BNE   *+10                                                             
         XC    PRVKEY,PRVKEY                                                    
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
* EOF ON INPUT FILE                                                             
**********************************************************************          
*                                                                               
DONE     DS    0H                                                               
         MVI   RECD,C'Z'           DONE                                         
         CLI   RELS,1              RELEASE LAST UNIVERSE RECORDS?               
         BE    UNVREL                                                           
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
               LRECL=0761,                                             X        
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
DAY      DS    CL6                                                              
WKDAYS   DS    CL7                                                              
INDEX    DS    A                   ADDRESS INTO TABLE                           
AUNVBUFF DS    A                   A(UNIVERSE  BUFFER)                          
MKTBRK   DS    X                   MKT BREAK                                    
MKTBMIT  DS    H                   MKT BREAK VALUE FROM MIT TAPE                
SVCIEQ   DS    AL2                                                              
ASVDEMS  DS    A                                                                
BYTE     DS    C                                                                
DAYBIT   DS    X                   STORE DAY OF WEEK (1 BIT)                    
TMP      DS    C                                                                
LEVEL    DS    X                                                                
WKBOOK   DS    CL2                 SAVED NETWEEK BOOK                           
WKIBOOK  DS    CL2                 SAVED NETWEEK IBOOK                          
WKDAYWK  DS    CL1                 SAVED INTDAYWK                               
TAPEMKT  DS    H                   CABLE NAD MKT=                               
TAPEWK   DS    CL6                                                              
PHUT     DS    CL9                                                              
TIMTAB   DS    7CL3                                                             
BOOK     DS    XL2                 KEY BOOK                                     
IBOOK    DS    XL2                 INTERNAL BOOK                                
BOOKS    DS    XL2                 KEY BOOK (SAVE AREA)                         
IBOOKS   DS    XL2                 INTERNAL BOOK (SAVE AREA)                    
USAHOME  DS    XL4                                                              
USHUT    DS    F                   HALF HOUR US HUT PROJ (XXX,XXX,XXX)          
MKTHUT   DS    F                   MKT BREAK HUT  PROJ   (XXX,XXX,XXX)          
SVAVGHI  DS    F                   SAVE PROGRAM HH                              
SVQHR    DS    X                                                                
SVR7     DS    A                                                                
APACKWT  DS    A                   A(PACKWT) AREA                               
NOTAVAL  DS    C                   DATA NOT AVAILABLE                           
FDENTRY  DS    XL(PNAMEDLQ)                                                     
VARIOUS  DS    X                   1=VARIOUS                                    
SAVETIME DS    7XL8                SAVE MITHOUR, MITMIN AND MIDDUR              
VARSTIME DS    7XL8                SAVE MITHOUR, MITMIN AND MIDDUR              
SAVVAR   DS    7XL(L'INTVAR)       SAVE VAR INFO FROM INDIVIDUAL DAYS           
*SAVVAR   DS    7XL7                SAVE VAR INFO FROM INDIVIDUAL DAYS          
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)                  
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)           
*                                                                               
KEYSRC   DS    C                                                                
SVVCR    DS    C                                                                
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
RELOFRST DC    X'01'                                                            
MYMODE   DC    X'00'                                                            
SVMODE   DC    X'00'                                                            
WKSFLG   DC    X'00'                                                            
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH                    
RECD     DC    X'00'               TYPE OF RECD JUST READ FROM TAPE             
RELSLOT  DC    X'00'               RELEASE SLOT                                 
SUMSLOT  DC    X'00'               SUMMARY SLOT                                 
UNVFRST  DC    X'00'                                                            
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS          
BLANKS   DC    256C' '                                                          
*                                                                               
         DS    0H                                                               
CVGLN    DC    AL2(NDEMS*NMKTS*4)  SIZE OF CVG IN UNV BUFFER (#BYTES)           
COVALPH  DS    CL4                                                              
COVSMP   DS    CL4                                                              
MKTSLOT  DS    X                                                                
COVSMPD  DS    X                                                                
RELS     DC    X'00'                                                            
FRELS    DC    X'00'                                                            
PRVOPT   DS    C                                                                
PRVKEY   DS    CL(L'MITKEY)        PREVIOUS RECD'S KEY                          
PRVMKT   DS    CL(L'MITMKTBR)      PREVIOUS MKT BREAK                           
PRVHLF   DS    CL(L'MITHLFID)      PREVIOUS HLF HOUR ID                         
PRVCOV   DS    CL(L'MITCOVG)                                                    
*RVQTR   DS    CL(L'MITQTRID)      PREVIOUS QTR HOUR ID                         
*                                                                               
QHR      DC    XL1'00'             QHR PROCESSING                               
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
PVALS    DS    0CL89                                                            
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
PWKBIT2  DS    X                                                                
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
OPTIONL  DS    C                                                                
         DC    C'OPTBUFF'                                                       
OPTBUFF  DS    (NMKTS)XL1                                                       
OPTBUFLN EQU   *-OPTBUFF                                                        
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
SLOTLN   DC    AL2(NDEMS*4)                                                     
*KTDISP  DC    AL2(NDEMS*4*2)      DISP TO NEXT MKT BRK DEMOS                   
MKTDISP  DC    AL2(NDEMS*4)        DISP TO NEXT MKT BRK DEMOS                   
SAVEKEY  DS    CL(L'INTKEY)        KEY OF ACCUMULATED DEMOS                     
         DS    0F                                                               
SAVEREC  DS    XL(NDEMS*4*2)       4BYTE DEMOS + UNIVS                          
*                                                                               
         DC    C'***HHRTAB***'                                                  
HHRTAB   DC    (48*7)XL(PNAMEDLQ)'00'   48 HLF HRS X 7 DAYS                     
         DC    X'FFFF'                                                          
         DC    C'***PNAMTAB***'                                                 
PNAMTAB  DC    500XL(PNAMEDLQ)'00'                                              
         DC    X'FFFF'                                                          
*-------------------------------------------------------------------            
         DC    CL24'***DEMAREA****DEMAREA***'                                   
         DS    0F                                                               
DEMAREA  DS    (NDEMS*3*NMKTS)XL4    DEMOS                                      
DEMLNQ   EQU   *-DEMAREA                                                        
PACKWT   DS    (NDEMS*3)PL8    (DEMOS+UNIVS+PUTS) WGHTING FACTOR PCKED          
*                                                                               
         EJECT                                                                  
MSU      EQU   8                   BIT MASK                                     
MFR      EQU   0                   BIT MASK                                     
NCBLS    EQU   150                 NUMBER OF CABLE NETS                         
         EJECT                                                                  
* ********************************************************************          
* EQUATE DEMO SLOT NUMBERS                                                      
* RT = TOTAL SAMPLE RECORDS                                                     
* RG = DEMO GROUP RECORDS  ?                                                    
* RI =  INTERIM RECD SLOTS FOR DEMOS                                            
**********************************************************************          
RIF25    EQU   0                   W2-5                                         
RIF68    EQU   1                   W6-8                                         
RIF911   EQU   2                   W9-11                                        
RIF1214  EQU   3                   W12-14                                       
RIF1517  EQU   4                   W15-17                                       
RIF1820  EQU   5                   W18-20                                       
RIF2124  EQU   6                   W21-24                                       
RIF2529  EQU   7                   W25-29                                       
RIF3034  EQU   8                   W30-34                                       
RIF3539  EQU   9                   W35-39                                       
RIF4044  EQU   10                  W40-44                                       
RIF4549  EQU   11                  W45-49                                       
RIF5054  EQU   12                  WW50-54                                      
RIF5564  EQU   13                  WW55-64                                      
RIF65O   EQU   14                  WW65+                                        
*                                                                               
RIM25    EQU   15                  M2-5                                         
RIM68    EQU   16                  M6-8                                         
RIM911   EQU   17                  M9-11                                        
RIM1214  EQU   18                  M12-14                                       
RIM1517  EQU   19                  M15-17                                       
RIM1820  EQU   20                  M18-20                                       
RIM2124  EQU   21                  M21-24                                       
RIM2529  EQU   22                  M25-29                                       
RIM3034  EQU   23                  M30-34                                       
RIM3539  EQU   24                  M35-39                                       
RIM4044  EQU   25                  M40-44                                       
RIM4549  EQU   26                  M45-49                                       
RIM5054  EQU   27                  M50-54                                       
RIM5564  EQU   28                  M55-64                                       
RIM65O   EQU   29                  M65+                                         
*                                                                               
RIF1217  EQU   30                  W1217                                        
RIM1217  EQU   31                  M1217                                        
RIHOMES  EQU   32                  HOMES                                        
RIUSA    EQU   33                  USA HOMES                                    
RIL1849  EQU   34                  LOH1849   (LADY OF HOUSE)                    
RIL50O   EQU   35                  LOH50+                                       
RIWW1849 EQU   36                  WW1849                                       
RIWW50O  EQU   37                  WW50+                                        
RIPW1849 EQU   38                  PTWW1849  (PART TIME WORKING WOMEN)          
RIPW50O  EQU   39                  PTWW50+                                      
RIMOMS   EQU   40                  WMOMS  (LOH18-49 W/CH <3 )                   
RIF611   EQU   41                  W6-11                                        
RIM611   EQU   42                  M6-11                                        
RIWW1820 EQU   43                  WW1820                                       
RIWW2124 EQU   44                  WW2124                                       
RIWW2534 EQU   45                  WW2534                                       
RIWW3544 EQU   46                  WW3544                                       
RIWW4549 EQU   47                  WW4549                                       
RIWW5054 EQU   48                  WW5054                                       
RIWW55O  EQU   49                  WW55O                                        
RIL1824  EQU   50                  L1824                                        
RIL2534  EQU   51                  L2534                                        
RIL3544  EQU   52                  L3544                                        
RIL4549  EQU   53                  L4549                                        
RIL5054  EQU   54                  L5054                                        
RIL55O   EQU   55                  L55+                                         
         EJECT                                                                  
*---------------------------------------------------------------------          
* RT..=  INPUT TAPE DISPLACEMENTS FOR  --TOTAL SAMPLE--  DEMOS                  
*---------------------------------------------------------------------          
*                                                                               
RTF25    EQU   1                                                                
RTM25    EQU   16                                                               
RTF611   EQU   2                                                                
RTM611   EQU   17                                                               
RTF1217  EQU   4                                                                
RTM1217  EQU   19                                                               
RTF1820  EQU   6                                                                
RTM1820  EQU   21                                                               
RTF2124  EQU   7                                                                
RTM2124  EQU   22                                                               
RTF2529  EQU   8                                                                
RTM2529  EQU   23                                                               
RTF3034  EQU   9                                                                
RTM3034  EQU   24                                                               
RTF3539  EQU   10                                                               
RTM3539  EQU   25                                                               
RTF4044  EQU   11                                                               
RTM4044  EQU   26                                                               
RTF4549  EQU   12                                                               
RTM4549  EQU   27                                                               
RTF5054  EQU   13                                                               
RTM5054  EQU   28                                                               
RTF5564  EQU   14                                                               
RTM5564  EQU   29                                                               
RTF65O   EQU   15                                                               
RTM65O   EQU   30                                                               
*                                                                               
RTWW1849 EQU   33                                                               
RTWW50O  EQU   34                                                               
RTL1849  EQU   35                                                               
RTL50O   EQU   36                                                               
         EJECT                                                                  
**********************************************************************          
*SLOTTAB-TRANSLATE                                                              
*INPUT DEMOS TO INTERIM SLOTS                                                   
*        STORED IN THE INTERIM RECORD BEGINING AT: INTACCS + 0                  
**********************************************************************          
SLOTTAB  DS    0H                                                               
         DC    AL2(RTF25,RIF25)                                                 
         DC    AL2(RTM25,RIM25)                                                 
         DC    AL2(RTF611,RIF611)                                               
         DC    AL2(RTM611,RIM611)                                               
*        DC    AL2(RTF68,RIF68)                                                 
*        DC    AL2(RTM68,RIM68)                                                 
*        DC    AL2(RTF911,RIF911)                                               
*        DC    AL2(RTM911,RIM911)                                               
*        DC    AL2(RTF1214,RIF1214)                                             
*        DC    AL2(RTM1214,RIM1214)                                             
*        DC    AL2(RTF1517,RIF1517)                                             
*        DC    AL2(RTM1517,RIM1517)                                             
         DC    AL2(RTF1217,RIF1217)                                             
         DC    AL2(RTM1217,RIM1217)                                             
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
         DC    AL2(RTWW1849,RIWW1849)                                           
         DC    AL2(RTWW50O,RIWW50O)                                             
         DC    AL2(RTL1849,RIL1849)                                             
         DC    AL2(RTL50O,RIL50O)                                               
         DC    X'FFFF',X'FFFF'                                                  
         EJECT                                                                  
*DISPS TO UNIVERSES IN DEMO DISP TABLE                                          
*                                                                               
NDEMS    EQU   56                  TOTAL NUMBER DEMOS ON OUTPUT FILE            
*IPUTSQ  EQU   NDEMS*4             NUMBER DEMOS* 4BYTE BUCKETS                  
RIUNVSQ  EQU   (56+44)*4           DISPL TO TOTAL US UNIVERSES                  
RIUNVSQC EQU   (56+44+56+56)*4     DISPL TO UNIVERSES BY COVG AREA              
*                                   (FROM DEMDISP)                              
***********************************************************************         
* MKT BREAK TABLE SLOTS EQUATES                                                 
***********************************************************************         
*------------------------------------------------                               
* TABLE OF RATING SERVICE MARKET BREAK CODES                                    
*------------------------------------------------                               
CRUSA    EQU   0                  TOTAL USA                                     
*                                 TERRITORY                                     
CRNE     EQU   1                   NORTHEAST                                    
CREC     EQU   2                   EAST CENTRAL                                 
CRWC     EQU   3                   WEST CENTRAL                                 
CRSE     EQU   4                   SOUTHEAST                                    
CRSW     EQU   5                   SOUTHWEST                                    
CRPAC    EQU   6                   PACIFIC                                      
*                                 COUNTY SIZE                                   
CRCSA    EQU   20                  CS A                                         
CRCSB    EQU   21                  CS B                                         
CRCSCD   EQU   22                  CS C+D                                       
*                                 CABLE                                         
CRPCBL   EQU   60                  PAY CABLE                                    
CRBCBL   EQU   61                  BASIC CABLE                                  
CRNCBL   EQU   62                  NO CABLE                                     
CRCPPAY  EQU   66                  CABLE PLUS & PAY       (9/96)                
CRCPNOPY EQU   67                  CABLE PLUS W/OUT PAY   (9/96)                
CRBRDCST EQU   68                  BROADCAST ONLY         (9/96)                
CRDBS    EQU   69                  DBS                    (9/2003)              
CRCWPAY  EQU   71                  WIRED DIGITAL CABLE W/PAY (12/27/04)         
CRCWNOPY EQU   72                  WIRED DIGITAL CABLE W/O PY(12/27/04)         
CRDVR    EQU   73                  DVR HOUSEHOLD          (5/2007)              
*                                 HOUSEHOLD SIZE                                
CRHHS1   EQU   140                 1                                            
CRHHS2   EQU   141                 2                                            
CRHHS3   EQU   142                 3                                            
CRHHS4O  EQU   143                 4+                                           
*                                 INCOME                                        
CRIU20   EQU   240                 UNDER 20K                                    
CRIU30   EQU   241                 20-29,999                                    
CRIU40   EQU   242                 30-39,999                                    
CRIU50   EQU   243                 40-49,999                                    
CRIU60   EQU   244                 50-59,999                                    
CRIO60   EQU   245                 OVER  60K                                    
CRIU75   EQU   247                 60-74,999   (9/96)                           
CRIO75   EQU   248                 OVER  75K   (9/96)                           
CRIU100  EQU   249                 75,000-99,999  (9/2003)                      
CRIO100  EQU   250                 100,000 PLUS   (9/2003)                      
CRIO125  EQU   251                 125,000 PLUS   (12/27/04)                    
*                                 SELECTED UPPER DEMOS                          
CRIUDNA  EQU   260                 40K+ & NON-ADULTS                            
CRIUDPOM EQU   261                 40K+ & POM                                   
CRIUDCOL EQU   262                 40K+ & COLLEGE                               
*                                  UPPER DEMOS W/50K & ..                       
CRIUDNA5 EQU   266                 50K+ & NON-ADULTS                            
CRIUDPM5 EQU   267                 50K+ & POM                                   
CRIUDCL5 EQU   268                 50K+ & COLLEGE                               
CRIUDNA7 EQU   272                 75K+ & NON-ADULTS  (9/2003)                  
CRIUDPM7 EQU   273                 75K+ & HOH IS POM  (9/2003)                  
CRIUDCL7 EQU   274                 75K+ & COLLEGE     (9/2003)                  
*                                 PRESENCE OF NON-ADULTS                        
CRNAU18  EQU   350                 ANY UNDER 18                                 
CRNAU12  EQU   351                 ANY UNDER 12                                 
CRNAU06  EQU   352                 ANY UNDER 6                                  
CRNAU03  EQU   353                 ANY UNDER 3                                  
CRNA611  EQU   354                 ANY 6-11                                     
CRNA1217 EQU   355                 ANY 12-17                                    
*                                 TIME ZONE                                     
CRTZEAST EQU   40                  EASTERN                                      
CRTZCENT EQU   41                  CENTRAL                                      
CRTZMP   EQU   42                  MOUNTAIN AND PACIFIC                         
*                                                                               
CRVCR    EQU   80                  PRESENCE OF VCR                              
CRDVD    EQU   90                  PRESENCE OF DVD   (9/2003)                   
CRVGO    EQU   85                  VIDEO GAME OWNER  (12/27/04)                 
*                                                                               
CROGHSP  EQU   94                 HISPANIC ORIGIN                               
CROGNHSP EQU   95                 NON-HISPANIC ORIGIN                           
*                                                                               
CRREMOTE EQU   100                 PRESENCE OF REMOTE CONTROL                   
*                                 NUMBER OF TV SETS                             
CRTV1    EQU   120                 1                                            
CRTV2    EQU   121                 2                                            
CRTV3    EQU   122                 3                                            
CRTV4O   EQU   123                 4+                                           
*                                                                               
CRRENTH  EQU   150                HOME IS RENTED (12/27/04)                     
CROWNH   EQU   151                HOME IS OWNED  (12/27/04)                     
*                                 AGE OF HOH                                    
CRHO25U  EQU   160                 UNDER 25                                     
CRHO2534 EQU   161                 25-34                                        
CRHO3554 EQU   162                 35-54                                        
CRHO5564 EQU   163                 55-64                                        
CRHO65O  EQU   164                 65+                                          
*                                 AGE OF HOH WITHIN HH SIZE                     
CRHO50U  EQU   180                 1-2 PERSONS, HOH UNDER 50                    
CRHO50O  EQU   181                 1-2 PERSONS, HOH 50+                         
*                                 EDUCATION OF HOH                              
CRED8U   EQU   200                 UNDER 8 YEARS                                
CRED12U  EQU   201                 1-3 YEARS HS                                 
CRED12   EQU   202                 4 YEARS HS                                   
CRED16U  EQU   203                 1-3 YEARS COL                                
CRED16O  EQU   204                 4+ YEARS COL                                 
*                                 OCCUPATION OF HOH                             
CROCWC   EQU   220                 WHITE COLLAR                                 
CROCBC   EQU   221                 BLUE COLLAR                                  
CROCNO   EQU   222                 NOT IN LABOR FORCE                           
*                                 RACE                                          
CRRACEB  EQU   320                 BLACK                                        
CRRACEW  EQU   321                 WHITE                                        
*                                 CAR OWNERSHIP                                 
CRCARANY EQU   400                 ANY OWNERS                                   
CRCAR1   EQU   401                 1 CAR                                        
CRCAR2O  EQU   402                 2+ CARS                                      
CRCARNEW EQU   403                 ANY CAR, NEW PROSPECT                        
*                                 TRUCK OWNERSHIP                               
CRTRKANY EQU   420                 ANY OWNERS                                   
CRTRK1   EQU   421                 1 TRUCK                                      
CRTRK2O  EQU   422                 2+ TRUCKS                                    
CRTRKNEW EQU   423                 ANY TRUCK, NEW PROSPECT                      
*                                 PET OWNERSHIP                                 
CRDOG    EQU   440                 DOG                                          
CRCAT    EQU   441                 CAT                                          
*                                                                               
CRPC     EQU   450                 PC OWNERS                                    
CRNOPC   EQU   451                 NON-PC OWNER                                 
CRPCIA   EQU   452                 PC OWNER WITH INTERNET ACCESS                
CRPCNOIA EQU   453                 PC OWNER WITHOUT INTERNET ACCESS             
*                                                                               
CRDUAL   EQU   501                 DUAL INCOME HH 40K+                          
CRDUAL50 EQU   502                 DUAL INCOME HH 50K+                          
CRDUAL75 EQU   503                 DUAL INCOME HH 75K+   (9/2003)               
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* CI.. - SLOTTED INTO INTERIM RECD AND LATER CONVERTED TO CO..                  
*        TABLE OF INTERIM AREA SLOTS FOR MARKET BREAKS                          
*---------------------------------------------------------------------          
CIUSA    EQU   0                  TOTAL USA                                     
*                                 TERRITORY                                     
CINE     EQU   1                   NORTHEAST                                    
CIEC     EQU   2                   EAST CENTRAL                                 
CIWC     EQU   3                   WEST CENTRAL                                 
CISE     EQU   4                   SOUTHEAST                                    
CISW     EQU   5                   SOUTHWEST                                    
CIPAC    EQU   6                   PACIFIC                                      
*                                 COUNTY SIZE                                   
CICSA    EQU   7                   CS A                                         
CICSB    EQU   8                   CS B                                         
CICSCD   EQU   9                   CS C+D                                       
*                                 CABLE                                         
CIPCBL   EQU   13                  PAY CABLE                                    
CIBCBL   EQU   14                  BASIC CABLE                                  
CINCBL   EQU   15                  NO CABLE                                     
CICPPAY  EQU   77                  CABLE PLUS WITH PAY      (9/96)              
CICPNOPY EQU   78                  CABLE PLUS WITH OUT PAY  (9/96)              
CIBRDCST EQU   79                  BROADCAST ONLY           (9/96)              
CIDBS    EQU   91                  DBS                      (9/2003)            
CICWPAY  EQU   103                 WIRED DIGITAL CABLE W/PAY (12/27/04)         
CICWNOPY EQU   104                 WIRED DIGITAL CABLE W/O PY(12/27/04)         
*                                 HOUSEHOLD SIZE                                
CIHHS1   EQU   16                  1                                            
CIHHS2   EQU   17                  2                                            
CIHHS3   EQU   18                  3                                            
CIHHS4O  EQU   19                  4+                                           
*                                 INCOME                                        
CIIU20   EQU   20                  UNDER 20K                                    
CIIU30   EQU   21                  20-29,999                                    
CIIU40   EQU   22                  30-39,999                                    
CIIU50   EQU   23                  40-49,999                                    
CIIU60   EQU   24                  50-59,999                                    
CIIO60   EQU   25                  OVER  60K                                    
CIIU75   EQU   80                  60-74,999   (9/96)                           
CIIO75   EQU   81                  OVER  75K   (9/96)                           
CIIU100  EQU   93                  75,000-99,999       (9/2003)                 
CIIO100  EQU   94                  100,000 PLUS        (9/2003)                 
CIIO125  EQU   100                 125,000 PLUS        (12/27/04)               
*                                 SELECTED UPPER DEMOS                          
CIIUDNA  EQU   26                  40K+ & NON-ADULTS                            
CIIUDPOM EQU   27                  40K+ & POM                                   
CIIUDCOL EQU   28                  40K+ & COLLEGE                               
CIIUDNA5 EQU   82                  50K+ & NON-ADULTS                            
CIIUDPM5 EQU   83                  50K+ & POM                                   
CIIUDCL5 EQU   84                  50K+ & COLLEGE                               
CIIUDNA7 EQU   95                  75K+ & NON-ADULTS   (9/2003)                 
CIIUDPM7 EQU   96                  75K+ & HOH IS POM   (9/2003)                 
CIIUDCL7 EQU   97                  75K+ & COLLEGE      (9/2003)                 
*                                 PRESENCE OF NON-ADULTS                        
CINAU18  EQU   30                  ANY UNDER 18                                 
CINAU12  EQU   31                  ANY UNDER 12                                 
CINAU06  EQU   32                  ANY UNDER 6                                  
CINAU03  EQU   33                  ANY UNDER 3                                  
CINA611  EQU   34                  ANY 6-11                                     
CINA1217 EQU   35                  ANY 12-17                                    
*                                 TIME ZONE                                     
CITZEAST EQU   10                  EASTERN                                      
CITZCENT EQU   11                  CENTRAL                                      
CITZMP   EQU   12                  MOUNTAIN AND PACIFIC                         
*                                                                               
CIVCR    EQU   36                  PRESENCE OF VCR                              
CIDVD    EQU   92                  PRESENCE OF DVD     (9/2003)                 
CIVGO    EQU   99                  VIDEO GAME OWNER (12/27/04)                  
*                                                                               
CIREMOTE EQU   37                  PRESENCE OF REMOTE CONTROL                   
*                                 NUMBER OF TV SETS                             
CITV1    EQU   38                  1                                            
CITV2    EQU   39                  2                                            
CITV3    EQU   40                  3                                            
CITV4O   EQU   41                  4+                                           
*                                                                               
CIRENTH  EQU   101                HOME IS RENTED (12/27/04)                     
CIOWNH   EQU   102                HOME IS OWNED  (12/27/04)                     
*                                 AGE OF HOH                                    
CIHO25U  EQU   42                  UNDER 25                                     
CIHO2534 EQU   43                  25-34                                        
CIHO3554 EQU   44                  35-54                                        
CIHO5564 EQU   45                  55-64                                        
CIHO65O  EQU   46                  65+                                          
*                                 AGE OF HOH WITHIN HH SIZE                     
CIHO50U  EQU   47                  1-2 PERSONS, HOH UNDER 50                    
CIHO50O  EQU   48                  1-2 PERSONS, HOH 50+                         
*                                 EDUCATION OF HOH                              
CIED8U   EQU   49                  UNDER 8 YEARS                                
CIED12U  EQU   50                  1-3 YEARS HS                                 
CIED12   EQU   51                  4 YEARS HS                                   
CIED16U  EQU   52                  1-3 YEARS COL                                
CIED16O  EQU   53                  4+ YEARS COL                                 
*                                 OCCUPATION OF HOH                             
CIOCWC   EQU   54                  WHITE COLLAR                                 
CIOCBC   EQU   55                  BLUE COLLAR                                  
CIOCNO   EQU   56                  NOT IN LABOR FORCE                           
*                                 RACE                                          
CIRACEB  EQU   57                  BLACK                                        
CIRACEW  EQU   58                  WHITE                                        
*                                 CAR OWNERSHIP                                 
CICARANY EQU   59                  ANY OWNERS                                   
CICAR1   EQU   60                  1 CAR                                        
CICAR2O  EQU   61                  2+ CARS                                      
CICARNEW EQU   62                  ANY CAR, NEW PROSPECT                        
*                                 TRUCK OWNERSHIP                               
CITRKANY EQU   63                  ANY OWNERS                                   
CITRK1   EQU   64                  1 TRUCK                                      
CITRK2O  EQU   65                  2+ TRUCKS                                    
CITRKNEW EQU   66                  ANY TRUCK, NEW PROSPECT                      
*                                 PET OWNERSHIP                                 
CIDOG    EQU   67                  DOG                                          
CICAT    EQU   68                  CAT                                          
*                                                                               
CIDUAL   EQU   29                  DUAL INCOME HH 40K+                          
CIDUAL50 EQU   85                  DUAL INCOME HH 50K+  (SEP/96)                
CIDUAL75 EQU   98                  DUAL INCOME HH 75K+ (9/2003)                 
*                                                                               
CIPC     EQU   87                  PC OWNERS                                    
CINOPC   EQU   88                  NON-PC OWNER                                 
CIPCIA   EQU   89                  PC OWNER WITH INTERNET ACCESS                
CIPCNOIA EQU   90                  PC OWNER WITHOUT INTERNET ACCESS             
*                                                                               
*        GENERATED CELLS                                                        
CISO     EQU   69                  SOUTH                                        
CIACBL   EQU   70                  ANY CABLE                                    
CIIO20   EQU   71                  20K+                                         
CIIO30   EQU   72                  30K+                                         
CIIO40   EQU   73                  40K+                                         
CIIO50   EQU   74                  50K+                                         
CIIU30U  EQU   75                  30K-                                         
CIHHS3O  EQU   76                  HHS3+                                        
CICBLP   EQU   86                  CBL+ = CBL+ W/PAY + CBL+ W/OUT PAY           
*                                                                               
CIDVR    EQU   105                DVR HOUSEHOLD   (5/2007)                      
*                                                                               
CIOGHSP  EQU   106                HISPANIC ORIGIN                               
CIOGNHSP EQU   107                NON-HISPANIC ORIGIN                           
*                                                                               
*!! UPDATE NMKTS BELOW EVERY TIME YOU ADD A NEW MARKET BREAK                    
*                                                                               
NMKTS    EQU   108                 INCLUDES TOTAL SAMPLE=000                    
         EJECT                                                                  
*---------------------------------------------------------------------          
* CO.. - THE MKT BRK#'S ACTUALLY APPEAR ON THE FILE IN 23 ELEMENT               
*        TABLE OF OUTPUT SECTIONS FOR MARKET BREAKS                             
*---------------------------------------------------------------------          
COUSA    EQU   1                  TOTAL USA                                     
*                                 TERRITORY                                     
CONE     EQU   11                  NORTHEAST                                    
COEC     EQU   12                  EAST CENTRAL                                 
COWC     EQU   13                  WEST CENTRAL                                 
COSE     EQU   16   *              SOUTHEAST                                    
COSW     EQU   17   *              SOUTHWEST                                    
COPAC    EQU   15                  PACIFIC                                      
*                                 COUNTY SIZE                                   
COCSA    EQU   21                  CS A                                         
COCSB    EQU   22                  CS B                                         
COCSCD   EQU   23                  CS C+D                                       
*                                 CABLE                                         
COPCBL   EQU   32                  PAY CABLE                                    
COBCBL   EQU   33                  BASIC CABLE                                  
CONCBL   EQU   34                  NO CABLE                                     
COCPPAY  EQU   35                  CABLE PLUS WITH PAY      (9/96)              
COCPNOPY EQU   36                  CABLE PLUS WITH OUT PAY  (9/96)              
COBRDCST EQU   37                  BROADCAST ONLY           (9/96)              
CODBS    EQU   39                  DBS                      (9/2003)            
COCWPAY  EQU   29                  WIRED DIGITAL CABLE W/ PAY(12/27/04)         
COCWNOPY EQU   30                  WIRED DIGITAL CABLE W/O PY(12/27/04)         
CODVR    EQU   40                  DVR HOUSEHOLD (5/2007)                       
*                                 HOUSEHOLD SIZE                                
COHHS1   EQU   41                  1                                            
COHHS2   EQU   42                  2                                            
****COHHS3O  EQU   43                  3+                                       
COHHS4O  EQU   44                  4+                                           
COHHS3   EQU   45                  3                                            
*                                 INCOME                                        
COIU20   EQU   68                  UNDER 20K                                    
COIU30   EQU   80                  20-29,999                                    
COIU40   EQU   81                  30-39,999                                    
COIU50   EQU   82                  40-49,999                                    
COIU60   EQU   83                  50-59,999                                    
COIO60   EQU   66                  OVER  60K                                    
COIU75   EQU   84                  60-74,999   (9/96)                           
COIO75   EQU   85                  OVER  75K   (9/96)                           
COIU100  EQU   94                  75,000-99,999       (9/2003)                 
COIO100  EQU   95                  100,000 PLUS        (9/2003)                 
COIO125  EQU   60                  125,000 PLUS        (12/27/04)               
*                                                                               
*                                 SELECTED UPPER DEMOS                          
COIUDNA  EQU   74                  40K+ & NON-ADULTS                            
COIUDPOM EQU   75                  40K+ & POM                                   
COIUDCOL EQU   76                  40K+ & COLLEGE                               
COIUDNA5 EQU   86                  50K+ & NON-ADULTS                            
COIUDPM5 EQU   87                  50K+ & POM                                   
COIUDCL5 EQU   88                  50K+ & COLLEGE                               
COIUDNA7 EQU   96                  75K+ & NON-ADULTS   (9/2003)                 
COIUDPM7 EQU   97                  75K+ & HOH IS POM   (9/2003)                 
COIUDCL7 EQU   98                  75K+ & COLLEGE      (9/2003)                 
*                                 PRESENCE OF NON-ADULTS                        
CONAU18  EQU   51                  ANY UNDER 18                                 
CONAU12  EQU   52                  ANY UNDER 12                                 
CONAU06  EQU   53                  ANY UNDER 6                                  
CONAU03  EQU   54                  ANY UNDER 3                                  
CONA611  EQU   55                  ANY 6-11                                     
CONA1217 EQU   56                  ANY 12-17                                    
*                                 TIME ZONE                                     
COTZEAST EQU   91                  EASTERN                                      
COTZCENT EQU   92                  CENTRAL                                      
COTZMP   EQU   93                  MOUNTAIN AND PACIFIC                         
*                                                                               
CODVD    EQU   100                 PRESENCE OF DVD     (9/2003)                 
COVCR    EQU   101                 PRESENCE OF VCR                              
COVGO    EQU   107                 VIDEO GAME OWNER (12/27/04)                  
*                                                                               
COOGHSP  EQU   89                 HISPANIC ORIGIN                               
COOGNHSP EQU   90                 NON-HISPANIC ORIGIN                           
*                                                                               
COREMOTE EQU   102                 PRESENCE OF REMOTE CONTROL                   
*                                 NUMBER OF TV SETS                             
COTV1    EQU   103                 1                                            
COTV2    EQU   104                 2                                            
COTV3    EQU   105                 3                                            
COTV4O   EQU   106                 4+                                           
*                                                                               
CORENTH  EQU   148                HOME IS RENTED (12/27/04)                     
COOWNH   EQU   149                HOME IS OWNED (12/27/04)                      
*                                 AGE OF HOH                                    
COHO25U  EQU   111                 UNDER 25                                     
COHO2534 EQU   112                 25-34                                        
COHO3554 EQU   113                 35-54                                        
COHO5564 EQU   114                 55-64                                        
COHO65O  EQU   115                 65+                                          
*                                 AGE OF HOH WITHIN HH SIZE                     
COHO50U  EQU   118                 1-2 PERSONS, HOH UNDER 50                    
COHO50O  EQU   119                 1-2 PERSONS, HOH 50+                         
*                                 EDUCATION OF HOH                              
COED8U   EQU   121                 UNDER 8 YEARS                                
COED12U  EQU   122                 1-3 YEARS HS                                 
COED12   EQU   123                 4 YEARS HS                                   
COED16U  EQU   124                 1-3 YEARS COL                                
COED16O  EQU   125                 4+ YEARS COL                                 
*                                 OCCUPATION OF HOH                             
COOCWC   EQU   131                 WHITE COLLAR                                 
COOCBC   EQU   132                 BLUE COLLAR                                  
COOCNO   EQU   133                 NOT IN LABOR FORCE                           
*                                 RACE                                          
CORACEB  EQU   141                 BLACK                                        
CORACEW  EQU   142                 WHITE                                        
*                                 CAR OWNERSHIP                                 
COCARANY EQU   151                 ANY OWNERS                                   
COCAR1   EQU   152                 1 CAR                                        
COCAR2O  EQU   153                 2+ CARS                                      
COCARNEW EQU   154                 ANY CAR, NEW PROSPECT                        
*                                 TRUCK OWNERSHIP                               
COTRKANY EQU   156                 ANY OWNERS                                   
COTRK1   EQU   157                 1 TRUCK                                      
COTRK2O  EQU   158                 2+ TRUCKS                                    
COTRKNEW EQU   159                 ANY TRUCK, NEW PROSPECT                      
*                                 PET OWNERSHIP                                 
CODOG    EQU   161                 DOG                                          
COCAT    EQU   162                 CAT                                          
COPC     EQU   163                 PC OWNERS                                    
CONOPC   EQU   164                 NON-PC OWNER                                 
COPCIA   EQU   165                 PC OWNER WITH INTERNET ACCESS                
COPCNOIA EQU   166                 PC OWNER WITHOUT INTERNET ACCESS             
*                                                                               
*                                                                               
CODUAL   EQU   77                  DUAL INCOME HH 40K+                          
CODUAL50 EQU   78                  DUAL INCOME HH 50K+                          
CODUAL75 EQU   99                  DUAL INCOME HH 75K+ (9/2003)                 
*                                                                               
*        GENERATED CELLS                                                        
COSO     EQU   14                  SOUTH                                        
COACBL   EQU   31                  ANY CABLE                                    
COCBLP   EQU   38                  CABLE PLUS W/PAY + W/OUT PAY                 
COHHS3O  EQU   43                  HOUSEHOLD SIZE 3+                            
COIO20   EQU   62                  20K+                                         
COIO30   EQU   63                  30K+                                         
COIO40   EQU   64                  40K+                                         
COIO50   EQU   65                  50K+                                         
COIU30U  EQU   67                  30K-                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
* TABLE TO CONVERT MARKET BREAKS TO INTERNAL SLOTS                              
*----------------------------------------------------------------------         
CRCITAB  DS    0XL4                                                             
         DS    0H                                                               
         DC    AL2(CRUSA,CIUSA)    TOTAL USA                                    
*                                 TERRITORY                                     
         DC    AL2(CRNE,CINE)      NORTHEAST                                    
         DC    AL2(CREC,CIEC)      EAST CENTRAL                                 
         DC    AL2(CRWC,CIWC)      WEST CENTRAL                                 
         DC    AL2(CRSE,CISE)      SOUTHEAST                                    
         DC    AL2(CRSW,CISW)      SOUTHWEST                                    
         DC    AL2(CRPAC,CIPAC)    PACIFIC                                      
*                                 COUNTY SIZE                                   
         DC    AL2(CRCSA,CICSA)    CS A                                         
         DC    AL2(CRCSB,CICSB)    CS B                                         
         DC    AL2(CRCSCD,CICSCD)  CS C+D                                       
*                                 CABLE                                         
         DC    AL2(CRPCBL,CIPCBL)  PAY CABLE                                    
         DC    AL2(CRBCBL,CIBCBL)  BASIC CABLE                                  
         DC    AL2(CRNCBL,CINCBL)  NO CABLE                                     
         DC    AL2(CRCPPAY,CICPPAY) CABLE PLUS & PAY                            
         DC    AL2(CRCPNOPY,CICPNOPY) CABLE PLUS WITHOUT PAY                    
         DC    AL2(CRBRDCST,CIBRDCST) BROADCAST ONLY                            
         DC    AL2(CRDBS,CIDBS)       DBS   (9/2003)                            
         DC    AL2(CRCWPAY,CICWPAY)   WIRED DIG CABLE W/ PAY (12/27/04)         
         DC    AL2(CRCWNOPY,CICWNOPY) WIRED DIG CABLE W/O PAY(12/27/04)         
         DC    AL2(CRDVR,CIDVR)       DVR (5/2007)                              
*                                 HOUSEHOLD SIZE                                
         DC    AL2(CRHHS1,CIHHS1)  1                                            
         DC    AL2(CRHHS2,CIHHS2)  2                                            
         DC    AL2(CRHHS3,CIHHS3)  3                                            
         DC    AL2(CRHHS4O,CIHHS4O) 4+                                          
*                                 INCOME                                        
         DC    AL2(CRIU20,CIIU20)  UNDER 20K                                    
         DC    AL2(CRIU30,CIIU30)  20-29,999                                    
         DC    AL2(CRIU40,CIIU40)  30-39,999                                    
         DC    AL2(CRIU50,CIIU50)  40-49,999                                    
         DC    AL2(CRIU60,CIIU60)  50-59,999                                    
         DC    AL2(CRIO60,CIIO60)  OVER  60K                                    
         DC    AL2(CRIU75,CIIU75)  60-74,999                                    
         DC    AL2(CRIO75,CIIO75)  OVER  75K                                    
         DC    AL2(CRIU100,CIIU100)  75,000-99,999       (9/2003)               
         DC    AL2(CRIO100,CIIO100)  100,000 PLUS        (9/2003)               
         DC    AL2(CRIO125,CIIO125)  125,000 PLUS        (12/27/04)             
*                                 SELECTED UPPER DEMOS                          
         DC    AL2(CRIUDNA,CIIUDNA) 40K+ & NON-ADULTS                           
         DC    AL2(CRIUDPOM,CIIUDPOM) 40K+ & POM                                
         DC    AL2(CRIUDCOL,CIIUDCOL) 40K+ & COLLEGE                            
         DC    AL2(CRIUDNA5,CIIUDNA5) 50K+ & NON-ADULTS   (9/96)                
         DC    AL2(CRIUDPM5,CIIUDPM5) 50K+ & POM          (9/96)                
         DC    AL2(CRIUDCL5,CIIUDCL5) 50K+ & COLLEGE      (9/96)                
         DC    AL2(CRIUDNA7,CIIUDNA7)  75K+ & NON-ADULTS   (9/2003)             
         DC    AL2(CRIUDPM7,CIIUDPM7)  75K+ & HOH IS POM   (9/2003)             
         DC    AL2(CRIUDCL7,CIIUDCL7)  75K+ & COLLEGE      (9/2003)             
*                                 PRESENCE OF NON-ADULTS                        
         DC    AL2(CRNAU18,CINAU18) ANY UNDER 18                                
         DC    AL2(CRNAU12,CINAU12) ANY UNDER 12                                
         DC    AL2(CRNAU06,CINAU06) ANY UNDER 6                                 
         DC    AL2(CRNAU03,CINAU03) ANY UNDER 3                                 
         DC    AL2(CRNA611,CINA611) ANY 6-11                                    
         DC    AL2(CRNA1217,CINA1217) ANY 12-17                                 
*                                 TIME ZONE                                     
         DC    AL2(CRTZEAST,CITZEAST) EASTERN                                   
         DC    AL2(CRTZCENT,CITZCENT) CENTRAL                                   
         DC    AL2(CRTZMP,CITZMP)  MOUNTAIN AND PACIFIC                         
*                                                                               
         DC    AL2(CRVCR,CIVCR)    PRESENCE OF VCR                              
         DC    AL2(CRDVD,CIDVD)    PRESENCE OF DVD   (9/2003)                   
         DC    AL2(CRVGO,CIVGO)    VIDEO GAME OWNER (12/27/04)                  
*                                                                               
         DC    AL2(CROGHSP,CIOGHSP)    HISPANIC ORIGIN (7/2007)                 
         DC    AL2(CROGNHSP,CIOGNHSP)  NON-HISPANIC ORIGIN (7/2007)             
*                                                                               
         DC    AL2(CRREMOTE,CIREMOTE) PRESENCE OF REMOTE CONTROL                
*                                 NUMBER OF TV SETS                             
         DC    AL2(CRTV1,CITV1)    1                                            
         DC    AL2(CRTV2,CITV2)    2                                            
         DC    AL2(CRTV3,CITV3)    3                                            
         DC    AL2(CRTV4O,CITV4O)  4+                                           
*                                                                               
         DC    AL2(CRRENTH,CIRENTH)  HOME IS RENTED (12/27/04)                  
         DC    AL2(CROWNH,CIOWNH)    HOME IS OWNED  (12/27/04)                  
*                                 AGE OF HOH                                    
         DC    AL2(CRHO25U,CIHO25U) UNDER 25                                    
         DC    AL2(CRHO2534,CIHO2534) 25-34                                     
         DC    AL2(CRHO3554,CIHO3554) 35-54                                     
         DC    AL2(CRHO5564,CIHO5564) 55-64                                     
         DC    AL2(CRHO65O,CIHO65O) 65+                                         
*                                 AGE OF HOH WITHIN HH SIZE                     
         DC    AL2(CRHO50U,CIHO50U) 1-2 PERSONS, HOH UNDER 50                   
         DC    AL2(CRHO50O,CIHO50O) 1-2 PERSONS, HOH 50+                        
*                                 EDUCATION OF HOH                              
         DC    AL2(CRED8U,CIED8U)  UNDER 8 YEARS                                
         DC    AL2(CRED12U,CIED12U) 1-3 YEARS HS                                
         DC    AL2(CRED12,CIED12)  4 YEARS HS                                   
         DC    AL2(CRED16U,CIED16U) 1-3 YEARS COL                               
         DC    AL2(CRED16O,CIED16O) 4+ YEARS COL                                
*                                 OCCUPATION OF HOH                             
         DC    AL2(CROCWC,CIOCWC)  WHITE COLLAR                                 
         DC    AL2(CROCBC,CIOCBC)  BLUE COLLAR                                  
         DC    AL2(CROCNO,CIOCNO)  NOT IN LABOR FORCE                           
*                                 RACE                                          
         DC    AL2(CRRACEW,CIRACEW) WHITE                                       
         DC    AL2(CRRACEB,CIRACEB) BLACK                                       
*                                 CAR OWNERSHIP                                 
         DC    AL2(CRCARANY,CICARANY) ANY OWNERS                                
         DC    AL2(CRCAR1,CICAR1)  1 CAR                                        
         DC    AL2(CRCAR2O,CICAR2O) 2+ CARS                                     
         DC    AL2(CRCARNEW,CICARNEW) ANY CAR, NEW PROSPECT                     
*                                 TRUCK OWNERSHIP                               
         DC    AL2(CRTRKANY,CITRKANY) ANY OWNERS                                
         DC    AL2(CRTRK1,CITRK1)  1 TRUCK                                      
         DC    AL2(CRTRK2O,CITRK2O) 2+ TRUCKS                                   
         DC    AL2(CRTRKNEW,CITRKNEW) ANY TRUCK, NEW PROSPECT                   
*                                 PET OWNERSHIP                                 
         DC    AL2(CRDOG,CIDOG)    DOG                                          
         DC    AL2(CRCAT,CICAT)    CAT                                          
*                                                                               
         DC    AL2(CRPC,CIPC)      PC OWNER                                     
         DC    AL2(CRNOPC,CINOPC)  NOPC                                         
         DC    AL2(CRPCIA,CIPCIA)  PC W/INTERNET ACCESS                         
         DC    AL2(CRPCNOIA,CIPCNOIA)  PC W/INTERNET ACCESS                     
*                                                                               
         DC    AL2(CRDUAL,CIDUAL)      DUAL INCOME HH 40K+                      
         DC    AL2(CRDUAL50,CIDUAL50)  DUAL INCOME HH 50K+                      
         DC    AL2(CRDUAL75,CIDUAL75)  DUAL INCOME HH 75K+  (9/2003)            
         DC    X'FFFF',X'FFFF'                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
* TABLE TO CONVERT MARKET BREAKS TO OUTPUT SECTIONS                             
*----------------------------------------------------------------------         
CRCOTAB  DS    0XL5                                                             
         DS    0H                                                               
*                                                                               
CRCO6B1B DC    X'6B1B'             NEW MRKT BREAKS EFFECTIVE JUL/2007           
         DC    AL4(CRCO6B12-CRCO6B1B)     LENGTH OF TABLE                       
                                                                                
         DC    AL2(CIOGHSP,COOGHSP),C'N'      HISPANIC ORIGIN                   
         DC    AL2(CIOGNHSP,COOGNHSP),C'N'    NON-HISPANIC ORIGIN               
         DC    X'F0'                                                            
*                                                                               
CRCO6B12 DC    X'6B12'             NEW MRKT BREAKS EFFECTIVE MAY/2007           
         DC    AL4(CRCO6901-CRCO6B12)     LENGTH OF TABLE                       
                                                                                
         DC    AL2(CIDVR,CODVR),C'N'      DVR HOUSEHOLD                         
         DC    X'F0'                                                            
*                                                                               
CRCO6901 DC    X'6901'             NEW MRKT BREAKS EFFECTIVE JAN/2005           
         DC    AL4(CRCO6724-CRCO6901)     LENGTH OF TABLE                       
                                                                                
         DC    AL2(CIVGO,COVGO),C'N'        VIDEO GAME OWNER                    
         DC    AL2(CIIO125,COIO125),C'N'    125,000 PLUS                        
         DC    AL2(CIRENTH,CORENTH),C'N'    HOME IS RENTED                      
         DC    AL2(CIOWNH,COOWNH),C'N'      HOME IS OWNED                       
         DC    AL2(CICWPAY,COCWPAY),C'N'    WIRED DG CBL W/PAY                  
         DC    AL2(CICWNOPY,COCWNOPY),C'N'  WIRED DG CBL W/O PY                 
         DC    X'F0'                                                            
*                                                                               
CRCO6724 DC    X'6724'                    MKT BREAKS EFFECTIVE SEP/2003         
         DC    AL4(CRCO0000-CRCO6724)     LENGTH OF TABLE                       
                                                                                
         DC    AL2(CIDBS,CODBS),C'N'       DBS                                  
         DC    AL2(CIDVD,CODVD),C'N'       PRESENCE OF DVD                      
         DC    AL2(CIIU100,COIU100),C'N'   75,000-99,999                        
         DC    AL2(CIIO100,COIO100),C'N'   100,000 PLUS                         
         DC    AL2(CIIUDNA7,COIUDNA7),C'N' 75K+ & NON-ADULTS                    
         DC    AL2(CIIUDPM7,COIUDPM7),C'N' 75K+ & HOH IS POM                    
         DC    AL2(CIIUDCL7,COIUDCL7),C'N' 75K+ & COLLEGE                       
         DC    AL2(CIDUAL75,CODUAL75),C'N' DUAL INCOME HH 75K+                  
         DC    X'F0'                                                            
                                                                                
CRCO0000 DC    X'0000'                    ORIGINAL MARKET BREAKS                
         DC    AL4(0)                     LAST TABLE                            
                                                                                
         DC    AL2(CIUSA,COUSA),C'N'      TOTAL USA                             
*                                          TERRITORY                            
         DC    AL2(CINE,CONE),C'N'         NORTHEAST                            
         DC    AL2(CIEC,COEC),C'N'         EAST CENTRAL                         
         DC    AL2(CIWC,COWC),C'N'         WEST CENTRAL                         
         DC    AL2(CISE,COSE),C'N'         SOUTHEAST                            
         DC    AL2(CISW,COSW),C'N'         SOUTHWEST                            
         DC    AL2(CIPAC,COPAC),C'N'       PACIFIC                              
*                                         COUNTY SIZE                           
         DC    AL2(CICSA,COCSA),C'N'       CS A                                 
         DC    AL2(CICSB,COCSB),C'N'       CS B                                 
         DC    AL2(CICSCD,COCSCD),C'N'     CS C+D                               
*                                         CABLE                                 
         DC    AL2(CIPCBL,COPCBL),C'N'     PAY CABLE                            
         DC    AL2(CIBCBL,COBCBL),C'N'     BASIC CABLE                          
         DC    AL2(CINCBL,CONCBL),C'N'     NO CABLE                             
         DC    AL2(CICPPAY,COCPPAY),C'N'   CABLE PLUS & PAY                     
         DC    AL2(CICPNOPY,COCPNOPY),C'N' CABLE PLUS WITHOUT PAY               
         DC    AL2(CIBRDCST,COBRDCST),C'N' BROADCAST ONLY                       
*                                         HOUSEHOLD SIZE                        
         DC    AL2(CIHHS1,COHHS1),C'N'     1                                    
         DC    AL2(CIHHS2,COHHS2),C'N'     2                                    
         DC    AL2(CIHHS3O,COHHS3O),C'N'   3+                                   
         DC    AL2(CIHHS4O,COHHS4O),C'N'   4+                                   
         DC    AL2(CIHHS3,COHHS3),C'N'     3                                    
*                                         INCOME                                
         DC    AL2(CIIU20,COIU20),C'N'     UNDER 20K                            
         DC    AL2(CIIU30,COIU30),C'N'     20-29,999                            
         DC    AL2(CIIU40,COIU40),C'N'     30-39,999                            
         DC    AL2(CIIU50,COIU50),C'N'     40-49,999                            
         DC    AL2(CIIU60,COIU60),C'N'     50-59,999                            
         DC    AL2(CIIO60,COIO60),C'N'     OVER 60K                             
         DC    AL2(CIIU75,COIU75),C'N'     60-74,999   --NEW 9/96               
         DC    AL2(CIIO75,COIO75),C'N'     OVER 75K    --NEW 9/96               
*                                         SELECTED UPPER DEMOS                  
         DC    AL2(CIIUDNA,COIUDNA),C'N'   40K+ & NON-ADULTS                    
         DC    AL2(CIIUDPOM,COIUDPOM),C'N' 40K+ & POM                           
         DC    AL2(CIIUDCOL,COIUDCOL),C'N' 40K+ & COLLEGE                       
         DC    AL2(CIIUDNA5,COIUDNA5),C'N' 50K+ & NON-ADULTS (9/96)             
         DC    AL2(CIIUDPM5,COIUDPM5),C'N' 50K+ & POM        (9/96)             
         DC    AL2(CIIUDCL5,COIUDCL5),C'N' 50K+ & COLLEGE    (9/96)             
*                                         PRESENCE OF NON-ADULTS                
         DC    AL2(CINAU18,CONAU18),C'N'   ANY UNDER 18                         
         DC    AL2(CINAU12,CONAU12),C'N'   ANY UNDER 12                         
         DC    AL2(CINAU06,CONAU06),C'N'   ANY UNDER 6                          
         DC    AL2(CINAU03,CONAU03),C'N'   ANY UNDER 3                          
         DC    AL2(CINA611,CONA611),C'N'   ANY 6-11                             
         DC    AL2(CINA1217,CONA1217),C'N' ANY 12-17                            
*                                         TIME ZONE                             
         DC    AL2(CITZEAST,COTZEAST),C'N' EASTERN                              
         DC    AL2(CITZCENT,COTZCENT),C'N' CENTRAL                              
         DC    AL2(CITZMP,COTZMP),C'N'     MOUNTAIN AND PACIFIC                 
*                                                                               
         DC    AL2(CIVCR,COVCR),C'N'      PRESENCE OF VCR                       
*                                                                               
         DC    AL2(CIREMOTE,COREMOTE),C'N' PRESENCE OF REMOTE CONTROL           
*                                         NUMBER OF TV SETS                     
         DC    AL2(CITV1,COTV1),C'N'       1                                    
         DC    AL2(CITV2,COTV2),C'N'       2                                    
         DC    AL2(CITV3,COTV3),C'N'       3                                    
         DC    AL2(CITV4O,COTV4O),C'N'     4+                                   
*                                         AGE OF HOH                            
         DC    AL2(CIHO25U,COHO25U),C'N'   UNDER 25                             
         DC    AL2(CIHO2534,COHO2534),C'N' 25-34                                
         DC    AL2(CIHO3554,COHO3554),C'N' 35-54                                
         DC    AL2(CIHO5564,COHO5564),C'N' 55-64                                
         DC    AL2(CIHO65O,COHO65O),C'N'   65+                                  
*                                         AGE OF HOH WITHIN HH SIZE             
         DC    AL2(CIHO50U,COHO50U),C'N'   1-2 PERSONS, HOH UNDER 50            
         DC    AL2(CIHO50O,COHO50O),C'N'   1-2 PERSONS, HOH 50+                 
*                                         EDUCATION OF HOH                      
         DC    AL2(CIED8U,COED8U),C'N'     UNDER 8 YEARS                        
         DC    AL2(CIED12U,COED12U),C'N'   1-3 YEARS HS                         
         DC    AL2(CIED12,COED12),C'N'     4 YEARS HS                           
         DC    AL2(CIED16U,COED16U),C'N'   1-3 YEARS COL                        
         DC    AL2(CIED16O,COED16O),C'N'   4+ YEARS COL                         
*                                         OCCUPATION OF HOH                     
         DC    AL2(CIOCWC,COOCWC),C'N'     WHITE COLLAR                         
         DC    AL2(CIOCBC,COOCBC),C'N'     BLUE COLLAR                          
         DC    AL2(CIOCNO,COOCNO),C'N'     NOT IN LABOR FORCE                   
*                                         RACE                                  
         DC    AL2(CIRACEW,CORACEW),C'N'   WHITE                                
         DC    AL2(CIRACEB,CORACEB),C'N'   BLACK                                
*OPTN'L MKT BRKS:                          CAR OWNERSHIP                        
         DC    AL2(CICARANY,COCARANY),C'N'  ANY OWNERS                          
         DC    AL2(CICAR1,COCAR1),C'N'      1 CAR                               
         DC    AL2(CICAR2O,COCAR2O),C'N'    2+ CARS                             
         DC    AL2(CICARNEW,COCARNEW),C'N'  ANY CAR, NEW PROSPECT               
*                                          TRUCK OWNERSHIP                      
         DC    AL2(CITRKANY,COTRKANY),C'N'  ANY OWNERS                          
         DC    AL2(CITRK1,COTRK1),C'N'      1 TRUCK                             
         DC    AL2(CITRK2O,COTRK2O),C'N'    2+ TRUCKS                           
         DC    AL2(CITRKNEW,COTRKNEW),C'N'  ANY TRUCK, NEW PROSPECT             
*                                          CATS AND DOGS OWNERSHIP              
         DC    AL2(CIDOG,CODOG),C'N'        DOG                                 
         DC    AL2(CICAT,COCAT),C'N'        CAT                                 
*                                          PC OWNERSHIP                         
         DC    AL2(CIPC,COPC),C'N'          PC OWNER                            
         DC    AL2(CINOPC,CONOPC),C'N'      NOPC                                
         DC    AL2(CIPCIA,COPCIA),C'N'      PC W/INTERNET ACCESS                
         DC    AL2(CIPCNOIA,COPCNOIA),C'N'  PC W/INTERNET ACCESSSS              
*                                                                               
         DC    AL2(CIDUAL,CODUAL),C'N'      DUAL INCOME HH 40K+                 
         DC    AL2(CIDUAL50,CODUAL50),C'N'  DUAL INCOME HH 50K+                 
* COMPUTED SLOTS                                                                
         DC    AL2(CISO,COSO),C'N'            SOUTH                             
         DC    AL2(CIACBL,COACBL),C'N'        ANY CABLE                         
         DC    AL2(CIHHS3O,COHHS3O),C'N'      HH SIZE 3+                        
         DC    AL2(CIIO20,COIO20),C'N'        OVER 20K                          
         DC    AL2(CIIO30,COIO30),C'N'        OVER 30K                          
         DC    AL2(CIIO40,COIO40),C'N'        OVER 40K                          
         DC    AL2(CIIO50,COIO50),C'N'        OVER 50K                          
         DC    AL2(CIIU30U,COIU30U),C'N'      UNDER 30                          
         DC    AL2(CICBLP,COCBLP),C'N'        CABLE+                            
         DC    X'FF'                                                            
                                                                                
                                                                                
CIGENTAB DS    0H                                                               
         DC    AL2(CISO,CISE,CISW,0,0,0)                                        
         DC    AL2(CIACBL,CIPCBL,CIBCBL,0,0,0)                                  
         DC    AL2(CIHHS3O,CIHHS3,CIHHS4O,0,0,0)                                
         DC    AL2(CIIO20,CIIU30,CIIU40,CIIU50,CIIU60,CIIO60)                   
         DC    AL2(CIIO30,CIIU40,CIIU50,CIIU60,CIIO60,0)                        
         DC    AL2(CIIO40,CIIU50,CIIU60,CIIO60,0,0)                             
         DC    AL2(CIIO50,CIIU60,CIIO60,0,0,0)                                  
         DC    AL2(CIIU30U,CIIU20,CIIU30,0,0,0)                                 
         DC    AL2(CICBLP,CICPPAY,CICPNOPY,0,0,0)                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* CVGTBL  -    CABLE NET CODES AND THEIR COVERAGE SAMPLE CODES                  
***********************************************************************         
CVGTBL   DC    (NCBLS)XL4'0000'         4 CHAR COVERAGE SAMPLE SAVED            
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
PNAMED   DSECT                    DSECT TO COVER PROG NAME TABLE                
PNQHR    DS    XL1                QTR HR (ALWAYS EVEN.ONLY HLF HR DATA)         
PNDAY    DS    XL1                DAY OF WEEK. M=X'10',TU=X'20'...              
PNTYPE   DS    CL1                P=PROG AVG, T=TRACKAGE                        
PNHNAME  DS    CL(L'INTPNAME)     MOST SPECIFIC NAME FOR THE HALF HR            
PNPNAME  DS    CL(L'INTTRNAM)     SAVED PROGRAM AVERAGE NAME                    
PNAMEDLQ EQU   *-PNAMED                                                         
*                                                                               
         EJECT                                                                  
*        CABLE NAD MIT DSECT                                                    
       ++INCLUDE DEMITCND                                                       
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
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*        DEDEMTABD                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113DECNADI   03/21/14'                                      
         END                                                                    
