*          DATA SET PPBUY12    AT LEVEL 113 AS OF 06/02/20                      
*PHASE T41112A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'CHANGE LOG FOR PPBUY12 - NEWSPAPERS   BUY/CHA/DEL'              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 03/02/20 SPEC-37363 NO RATE CHANGE FOR FROZEN CLIENTS                    
*                                                                               
* JSAY 08/30/19 SKIP VALIDATION OF EXTENDED ALLOCATIONS                         
*                                                                               
* KWAN 03/31/17 ALLOW MAX RATE OF 8,499,999.99                                  
*                                                                               
* KWAN 05/21/15 PROCESS TOTAL COST FOR PRISMA BUYS                              
*                                                                               
* KWAN 01/07/13 FULLY PAID BY PROFILE TO CONTROL PREMIUM CHANGES                
*                                                                               
* KWAN 10/17/11 RATE CHANGE INDICATE ADJUSTMENT                                 
*                                                                               
* KWAN 06/07/11 FULLY PAID BY PROFILE ADJUSTMENT FOR CANADA                     
*                                                                               
* KWAN 10/26/10 PREVENT NEW BUYS FOR LOCKED PUBS                                
*                                                                               
* KWAN 10/14/10 COS2 $ RATE                                                     
*                                                                               
* KWAN 10/12/10 BYPASS BILLABLE DATE HEX SECURITY FOR PBU                       
*                                                                               
* KWAN 07/21/10 ADBUYER FIX - ZZZ INSERTIONS WITH TEARSHEET UPDATES             
*                                                                               
* KWAN 07/13/10 ADJUST NEGATIVE TEN THOUSAND RATE CHANGE LOGIC                  
*                                                                               
* SMYE  01/10   ALLOW FOR RETENTION AND DISPLAY OF "S" COST INDICATOR           
*                 IN "FREE" BUYS                                                
*                                                                               
* KWAN 10/21/09 FREE INSERTION, RATE CHANGE INDICATOR ADJUSTMENT                
*                                                                               
* KWAN 08/07/09 FULLY PAID CHANGES FOR CANADIAN BUYS                            
*                                                                               
* KWAN 04/08/08 PBU FAILED DUE TO LOWER CASE LETTERS IN SPACE DESP.             
*                                                                               
* BOBY 05/00/06 ADD DATE PASSIVE POINTERS                                       
*                                                                               
* KWAN 09/29/06 PURCHASE ORDER #                                                
*                                                                               
* KWAN 06/08/06 ALLOW ZERO VALUE PAGE VIEW AND CLICK THRU ELEMENTS              
*                                                                               
* KWAN 12/16/05 DELETING CLEARED INSERTIONS W/ INVOICE DATA VIA ADBUYER         
*                                                                               
* SMYE  12/05   "ADID=XXX.." OPTION FOR AD-ID ALONE (NO NRML JOB CODE))         
*                                                                               
* KWAN 10/03/05 DEL MAT= REPEAT PASSIVE FOR DEL, INS DATE CHG & AD CHG          
*                                                                               
* KWAN 08/19/05 COS2 FACTOR ROUNDING OPTION IN F0 PROFILE                       
*                                                                               
* KWAN 04/19/05 DROP PENNIES FOR LARGE RATE TO DISPLAY MINUS SIGN               
*                                                                               
* KWAN 12/14/04 ADBUYER COMMENTS UPLOAD CHANGE INDICATOR FIX                    
*                                                                               
* SMYE 09/04    FOR PBDCDATE, AND PBDMDATE CHANGE NON-WORK DAY DATES            
*                 TO FIRST PRIOR WORK DAY IN LITTLE BRAS ROUTINE                
*                 "CGWRKDT" IF PROFILE SO INDICATES                             
*                                                                               
* SMYE 09/02/04 ADD WEBIO ELEM (X'71') TO CKIOEL ERROR EDITING                  
*                                                                               
* KWAN 06/28/04 USE CLE OVERRRIDE CALCULATION                                   
*                                                                               
* KWAN 03/01/04 ADBUYER CUSTOM COLUMN UPLOAD                                    
*                                                                               
* KWAN 01/15/04 GST AND PST BUG FIX FOR ACTION CHANGE                           
*                                                                               
* KWAN 10/31/03 FIX UNIT INDICATOR COMPARISON  FOR CHG TRANSACTION              
*                                                                               
* KWAN 09/11/03 INVOICE MATCHING STATUSES ELEM FOR AB 2.0                       
*                                                                               
* KWAN 03/05/03 FIX COMMENTS VALIDATION                                         
*                                                                               
* KWAN 03/03/03 CORRECT SPACE CLOSING DATE VALIDATION                           
*                                                                               
* KWAN 12/19/02 GIVE CLEAR ERROR MSG FOR CLE='BLANK' INPUT                      
*                                                                               
* YKAP 09/10/02 ISS= ISSUE NAME                                                 
*                                                                               
* KWAN 07/24/02 FIX ADBUYER ELEM CHANGES                                        
*                                                                               
* KWAN 07/24/02 FIX MANIO (EDTMAN), BUG LEFT BY REMOVING CSECTS                 
*                                                                               
* KWAN 07/22/02 FIX MASTER/SUB CLIENT RECORD LOCKING BUG                        
*                                                                               
* KWAN 07/19/02 ADBUYER ELEMS, REMOVE ONLY IF PRESENT IN NEWREC                 
*                                                                               
* KWAN 12/10/01 ADBUYER - ROUTINE IN CHG LOGIC, NEED TO HANDLE OTHER            
*               ELEMS, BECAUSE REC GETS REREAD AND CHANGES TO REC MADE          
*               IN PPBUY18 HAS NO EFFECT (SEE CKADBELM IN PPBUY03)              
*                                                                               
* KWAN 11/30/01 ADBUYER - USE T41118 TO PROCESS OPTIONAL DATA FIELDS            
*                                                                               
* KWAN 09/28/01 ADD/CHG SPACE DESCRIPTION (USE STD SPC DESP REC, X'5A')         
*                                                                               
* KWAN 08/16/01 "NO TRAFFIC" OVERRIDES (PBDSTAT BIT IS X'20')                   
*                                                                               
* KWAN 07/16/01 LEGAL WARNINGS (NEW OPTIONAL DATA AND ELEM, X'94')              
*                                                                               
* KWAN 05/15/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* KWAN 02/16/01 EDTDT AND EDTCDT, LEAP YEARS CHKS ARE NO GOOD FOR 2001+         
*                                                                               
* KWAN 02/09/01 BUY RECS ARE EXPANED TO 4000 BYTES                              
*                                                                               
* KWAN 01/29/01 EXDATE (EXTENSION DATE), USES SAME LOGIC AS EDTCSHDT            
*                                                                               
* SMYE 12/00    ADD CLIENT-FROZEN-BY-DATE LOGIC AT LABEL CHG2B                  
*                                                                               
* SMYE 11/28/00 DISABLE (*NOP*) "IMPS=" IN KEYWTAB - "IMPS=" WILL               
*               NOW BE TREATED AS A STANDARD COMMENT                            
*                                                                               
* KWAN 05/00    EIMPS IS SAME AS IMPS, ALLOW IMPS=0 INPUTS                      
*               IMPS ELEMS CAN ALSO BE REMOVED BY IMPS=DEL                      
*                                                                               
* KWAN 05/00    ADD AIMPS OPTION (ACTUAL IMPRESSION)                            
*                                                                               
* KWAN 02/15/99 PRD EXCLUSION FOR LIST BUYS                                     
*                                                                               
* KWAN 02/08/00 FIX D2=NONE PROBLEM ON A NEW BUY (EDTCD2)                       
*                                                                               
* KWAN 02/02/00 NO RATE CHANGES AND BUY DELETIONS FOR FULLY                     
*               PAID BUYS (CONTROLLED BY BYPROF+7)                              
*                                                                               
* KWAN 12/09/99 VALIDATING BUYS WITH EXCLUSION CLASS RESTRICTIONS               
*                                                                               
* KWAN 11/23/99 DISALLOW ADDING EMPTY ELEM WHEN BUYING (CAUSE DUMP)             
*                                                                               
* KWAN 06/30/99 UPDATE CHANGE ELEM (X'24') FOR CT, PV AND IMPS CHANGES          
*                                                                               
* KWAN 06/25/99 ADD OPTIONS OSD= AND CLD= (ON SALE AND CLOSE DATE)              
*                                                                               
* KWAN 05/20/99 REMOVE "SJ ONLY" FROM IMPRESSION OPTION                         
*                                                                               
* BPLA 5/99     ALLOW COMMENT CHANGES TO DELETED BUYS                           
*                                                                               
* KWAN 04/15/99 ADD CODES FOR IMPRESSION OPTION (SJ ONLY)                       
*                                                                               
* KWAN 02/24/99 SET UP KEYWORD TABLE FOR LOOK UPS                               
*                                                                               
* KWAN 01/11/99 ADDED CODES FOR EXTENSION DAYS ELEMENT                          
*               FIXREC SUBROUTINE TO GET RID OF REPEATED CODES                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY12 - NEWSPAPERS   BUY/CHA/DEL'                             
*                                                                               
T41112   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41112,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T41112+4096,R8      **NOTE - SECOND BASE REGISTER **             
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        INIT ADDRESS                                 
*                                                                               
         MVC   VT41103,0(R1)                                                    
         XC    OLDINS(12),OLDINS                                                
*                                                                               
         BRAS  RE,NXTTR            FIND FIRST TR FIELD                          
         BNZ   *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,NOTRERR                                                       
         J     ERROR                                                            
         L     RE,TRADDR                                                        
         MVC   SVTRCODE,8(RE)      SAVE TRANSACTION CODE                        
*                                                                               
         CLI   SVTRCODE,C'B'       BUYING?                                      
         BNE   MAIN_30                                                          
         TM    BYPLCKSW,BPLOCKDQ   PUB IS LOCKED?                               
         BZ    MAIN_30                                                          
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         LA    R3,PLCKDERQ         PUB IS LOCKED - NO NEW BUYS ERROR            
         J     ERROR                                                            
*                                                                               
MAIN_30  CLI   SVSCRN,X'FA'                                                     
         BE    BUYN                                                             
         CLI   SVSCRN,X'FB'                                                     
         BE    BUYN                                                             
         CLI   SVSCRN,X'F8'                                                     
         BE    BUYL                                                             
         CLI   SVSCRN,X'F9'                                                     
         BE    BUYL                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUYN     CLC   =C'DL',8(R2)                                                     
         BE    DEL                                                              
         BRAS  RE,BLDREC                                                        
         CLI   8(R2),C'C'                                                       
         BNE   BUYN5                                                            
*                                                                               
         BRAS  RE,FNDINS                                                        
*                                                                               
         TM    PBDSTAT-NEWREC+REC,X'20'                                         
         BZ    *+8                                                              
         OI    PBDSTAT,X'20'       RETAIN OLD RECORD'S STATUS                   
*                                                                               
* WHEN CHANGING, REC HAS OVERRIDE OR DEFAULT GST CODE                           
*                                                                               
         MVC   PBDGST,PBDGST-NEWREC+REC                                         
*                                                                               
         CLI   DDLINKSW,C'C'       ADBUYER CHANGE                               
         BNE   BUYN5               SAVE PBDBFD (STATUS)                         
*                                                                               
* MAY BE RESET IF STATUS CHANGE PAIR IS SENT                                    
*                                                                               
         MVC   PBDBFD,PBDBFD-NEWREC+REC                                         
*                                                                               
BUYN5    BRAS  RE,EDTINS           EDIT INSERTION DATE                          
         BRAS  RE,EDTJOB           EDIT JOB                                     
         BRAS  RE,EDTLNS           EDIT NUMBER OF LINES                         
         BRAS  RE,EDTRTN           EDIT RATE                                    
         BRAS  RE,EDTPR                                                         
         BRAS  RE,EDTDT            MAT CLOSING DATE                             
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDMDATE,DUB                                                     
         BRAS  RE,EDTCOM                                                        
         BRAS  RE,RTSUB                                                         
         BRAS  RE,FSILOOK          DO FSI LOOK-UP                               
*                                                                               
         CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE ESTIMATE                     
         BNE   BUYN1H                                                           
         CLI   PBDCOSIN,C'C'       SEE IF 'C' RATE BUY                          
         BE    BUYN1A                                                           
         CLI   PBDCOSIN,C' '                                                    
         BNE   BUYNERR                                                          
         MVI   PBDCOSIN,C'C'                                                    
*                                                                               
BUYN1A   CLI   PBDCL,0             CHK FOR PREMIUM                              
         BE    BUYN1B                                                           
         CLI   PBDPRIN,C'C'        MUST ALSO BE 'C'                             
         BE    BUYN1B                                                           
         CLI   PBDPRIN,C' '        SEE IF INPUT                                 
         BNE   BUYPERR                                                          
         MVI   PBDPRIN,C'C'        SET 'C' PREMIUM                              
*                                                                               
* SPECIAL FOR 'C' RATE LOOKED-UP OR ENTERED                                     
*                                                                               
BUYN1B   L     R2,TRADDR                                                        
         CLI   8(R2),C'C'          SEE IF CHANGE                                
         BE    BUYN1D                                                           
         ZAP   PBDCD,=P'0'         SET CD TO ZERO                               
         XC    PBDTAX,PBDTAX                                                    
         B     BUYN1X                                                           
*                                                                               
BUYN1D   ZAP   PBDCD,=P'1'         WILL BE CHANGED TO ZERO                      
         MVC   PBDTAX,=X'000001'   WILL BE SET TO ZERO                          
         B     BUYN1X                                                           
*                                                                               
BUYN1H   CLI   PBDCOSIN,C'C'       ONLY 'C' RATE ESTIMATES                      
         BNE   BUYN1X              CAN HAVE 'C' RATES                           
BUYNERR  LA    R3,INVRTERR                                                      
         L     R2,TRADDR                                                        
         LHI   RF,4                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
BUYPERR  LA    R3,INVRTERR         PREMIUM ERROR                                
         L     R2,TRADDR                                                        
         LHI   RF,5                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
BUYN1X   DS    0H                                                               
         OC    PBDMDATE,PBDMDATE                                                
         BNZ   BUYN3                                                            
         CLI   SVAGPROF+24,C'M'    REQUIRE MATERIALS CLOSING DATE               
         BE    BUYN2                                                            
         CLI   SVAGPROF+24,C'B'                                                 
         BNE   BUYN3                                                            
*                                                                               
BUYN2    LA    R3,MSSNGERR                                                      
         L     R2,TRADDR                                                        
         LHI   RF,6                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
BUYN3    DS    0H                                                               
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'                                                       
         BE    CHG                                                              
*                                                                               
         BRAS  RE,BEXCL            CKING FOR PRODUCT EXCLUSION                  
         BNE   EXIT                EXCLUSION CONFLICT                           
*                                                                               
         BRAS  RE,EDIODAT                                                       
*                                                                               
         BRAS  RE,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BRAS  RE,ASC              AUTO SCHEDULE CHECKING                       
*                                                                               
         BRAS  RE,UPD              MAY NEED TO ADD UPLOAD ELEM                  
*                                                                               
         CLC   =C'ZZZ',BUYPR                                                    
         BE    POL                                                              
*                                                                               
         TM    GLBVALSW,BUYPO#VQ   PURCHASE ORDER # ALREADY VALIDATED?          
         BNZ   *+16                                                             
         GOTOR VALBYPO#,1          DO PURCHASE ORDER # LOOKUP                   
         JNE   ERROR                                                            
*                                                                               
         TM    GLBVALSW,BUYCOS2Q   COS2 $ ALREADY VALIDATED?                    
         BNZ   *+16                                                             
         GOTOR VAL_COS2,1          DO COS2 $ COPY (COPY FROM RATE)              
         JNE   ERROR                                                            
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),BYPO#ELQ                                  
*                                                                               
         BRAS  RE,MUP              MIGHT NEED TO UPDATE MINIO REC               
*                                                                               
         BRAS  RE,ADDLINE                                                       
*                                                                               
         BRAS  RE,MUP              MIGHT NEED TO UPDATE MINIO REC               
*                                                                               
         BRAS  RE,ASR              FOR AUTO SPACE RESV                          
*                                                                               
BUYNX    BRAS  RE,FMTTR                                                         
         BRAS  RE,FMTINS                                                        
         CLC   =C'DL',TRCODE                                                    
         BE    BUYNX5                                                           
         BRAS  RE,FMTRTN                                                        
         BRAS  RE,FMTPR                                                         
         BRAS  RE,FMTDTES                                                       
*                                                                               
BUYNX5   DS    0H                                                               
         XC    UPID,UPID                                                        
         MVI   UPLSW,0                                                          
         B     ALLDONE                                                          
*                                                                               
BUYL     DS    0H                  LIST BUYING                                  
         XC    UPID,UPID                                                        
         MVI   UPLSW,0                                                          
         L     RE,LSTPTR           SET PUB NUMBER                               
         MVC   BPUB,0(RE)                                                       
*                                                                               
         MVI   BFREQ,0             SET TO REQUIRE MONTH DAY                     
         CLC   =C'DL',8(R2)                                                     
         BE    DEL                                                              
         BRAS  RE,BLDREC                                                        
         CLI   8(R2),C'C'                                                       
         BNE   *+8                                                              
         BRAS  RE,FNDINS                                                        
         LA    R3,OLDINS                                                        
         BRAS  R9,TSTDATA                                                       
         BRAS  RE,EDTINS                                                        
         BRAS  RE,BUMPFLD                                                       
         LA    R3,OLDJOB                                                        
         BRAS  R9,TSTDATA                                                       
         BRAS  RE,EDTJOB                                                        
*                                                                               
         CLI   5(R2),0             IF NO JOB OR JOB 'NONE' TRY TO USE           
         BE    *+14                PREVIOUS LINE - OTHERWISE USE JOB            
         CLC   =C'NONE',8(R2)      DATA                                         
         BNE   *+12                                                             
*                                                                               
         LA    R3,OLDLNS                                                        
         BRAS  R9,TSTDATA                                                       
         BRAS  RE,EDTLNS                                                        
         BRAS  RE,EDTRTN                                                        
         BRAS  RE,EDTPR                                                         
         BRAS  RE,EDTDT            MAT CLOSING DATE                             
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDMDATE,DUB                                                     
         BRAS  RE,RTSUB                                                         
         BRAS  RE,FSILOOK          DO FSI LOOK-UP                               
*                                                                               
         CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE ESTIMATE                     
         BNE   BUYL1H                                                           
         CLI   PBDCOSIN,C'C'       SEE IF 'C' RATE BUY                          
         BE    BUYL1A                                                           
         CLI   PBDCOSIN,C' '                                                    
         BNE   BUYLERR                                                          
         MVI   PBDCOSIN,C'C'                                                    
*                                                                               
BUYL1A   CLI   PBDCL,0             CHK FOR PREMIUM                              
         BE    BUYL1B                                                           
         CLI   PBDPRIN,C'C'                                                     
         BE    BUYL1B                                                           
         CLI   PBDPRIN,C' '                                                     
         BNE   BUYLPERR                                                         
         MVI   PBDPRIN,C'C'                                                     
*                                                                               
* SPECIAL FOR 'C' RATE LOOKED-UP OR ENTERED                                     
*                                                                               
BUYL1B   L     R2,TRADDR                                                        
         CLI   8(R2),C'C'          SEE IF CHANGE                                
         BE    BUYL1D                                                           
         ZAP   PBDCD,=P'0'         SET CD TO ZERO                               
         XC    PBDTAX,PBDTAX                                                    
         B     BUYL1X                                                           
*                                                                               
BUYL1D   ZAP   PBDCD,=P'1'         WILL BE CHANGED TO ZERO                      
         MVC   PBDTAX,=X'000001'   WILL BE SET TO ZERO                          
         B     BUYL1X                                                           
*                                                                               
BUYL1H   CLI   PBDCOSIN,C'C'       ONLY 'C' RATE ESTIMATES                      
         BE    BUYLERR             CAN HAVE 'C' RATES                           
         CLI   PBDPRIN,C'C'        ONLY 'C' RATE ESTIMATES                      
         BE    BUYLPERR            CAN HAVE 'C' RATES                           
         B     BUYL1X                                                           
*                                                                               
BUYLERR  LA    R3,INVRTERR                                                      
         L     R2,TRADDR                                                        
         LHI   RF,5                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
BUYLPERR LA    R3,INVRTERR         PREMIUM ERROR                                
         L     R2,TRADDR                                                        
         LHI   RF,6                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
BUYL1X   DS    0H                                                               
         OC    PBDMDATE,PBDMDATE                                                
         BNZ   BUYL5                                                            
         CLI   SVAGPROF+24,C'M'    SEE IF MATERIALS CLOSING REQ                 
         BE    BUYL3                                                            
         CLI   SVAGPROF+24,C'B'    OR BOTH                                      
         BNE   BUYL5                                                            
BUYL3    LA    R3,MSSNGERR                                                      
         L     R2,TRADDR                                                        
         LHI   RF,7                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
BUYL5    DS    0H                  NOTE COMMENTS NOT INPUT                      
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'                                                       
         BE    CHG                                                              
*                                                                               
         BRAS  RE,BEXCL            CKING FOR PRODUCT EXCLUSION                  
         BNE   EXIT                EXCLUSION CONFLICT                           
*                                                                               
         BRAS  RE,EDIODAT                                                       
*                                                                               
         BRAS  RE,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BRAS  RE,ASC              DO AUTOMATIC SCHEDULE CHECKING NOW           
*                                                                               
         CLC   =C'ZZZ',BUYPR                                                    
         BE    POL                                                              
*                                                                               
         BRAS  RE,ADDLINE                                                       
*                                                                               
         BRAS  RE,ASR              AUTOMATIC SPACE RESERVATION                  
*                                                                               
BUYLX    BRAS  RE,FMTTR                                                         
         BRAS  RE,FMTINS                                                        
         CLC   =C'DL',TRCODE                                                    
         BE    BUYLX5                                                           
         BRAS  RE,FMTRTN                                                        
         BRAS  RE,FMTPR                                                         
         BRAS  RE,FMTDTES                                                       
BUYLX5   BRAS  RE,NXTTR                                                         
         BNZ   BUYL                                                             
         B     ALLDONE                                                          
*                                                                               
* SUBROUTINE TO MOVE DEFAULT DATA TO FIELDS WITH NO INPUT.                      
* R3 HAS OLD DATA ADDRESS ON ENTRY.                                             
*                                                                               
TSTDATA  SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R4,R2               POINT TO NEXT FIELD                          
         CLI   5(R4),0             TEST DATA ENTERED                            
         BE    *+10                                                             
         ST    R4,0(R3)                                                         
         BR    R9                                                               
*                                                                               
* NO DATA - SEE IF HAD ANY BEFORE                                               
*                                                                               
         OC    0(4,R3),0(R3)                                                    
         BCR   8,R9                NO-GET OUT TO ERROR ON EDIT                  
         SR    RE,RE               SET TO MOVE DATA TO FIELD                    
         IC    RE,0(R4)                                                         
         AHI   RE,-5               4 + 1 FOR EXECUTE                            
         L     R3,0(R3)            SET DATA ADDRESS                             
         EX    RE,MVDATA                                                        
         FOUT  (R4)                                                             
         BR    R9                                                               
*                                                                               
MVDATA   MVC   4(0,R4),4(R3)       LAST 4 CNTRL BYTES + DATA                    
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHG      DS    0H                                                               
         BRAS  RE,FNDINS                                                        
*                                                                               
* MUST SAVE OLD GROSS,AC,CD                                                     
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
*                                                                               
         MVC   SVGROSS(12),GROSS   GROSS,AC,CD                                  
*                                                                               
         MVI   MATSW,0             MATCHED STATUS BYTE                          
*                                                                               
         TM    PBDSTAT-NEWREC+REC,X'40'                                         
         BZ    CHG0                NOT MATCHED - SKIP PAID CHECK                
         OI    MATSW,X'40'         MATCHED                                      
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
CHG00C   BRAS  RE,NXTELEM                                                       
         BNE   CHG0                                                             
         OC    2(3,R5),2(R5)       SEE IF PAID                                  
         BZ    CHG00C                                                           
         OI    MATSW,X'80'         PAID                                         
*                                                                               
CHG0     MVI   WSJIND,0                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'35'        LOOK FOR WSJ ELEM                            
         BRAS  RE,NXTELEM                                                       
         BNE   CHG1                                                             
         MVI   WSJIND,1                                                         
*                                                                               
* FOR WSJ BUYS - CAN'T CHANGE DATE,JOB,SPACE,RATE,ALLOC                         
*                                                                               
CHG1     LA    R7,PBDDATE-NEWREC+REC                                            
         CLC   0(3,R7),BTODAY                                                   
         BE    CHG1B                                                            
         TM    REC+(PBUYCNTL-PBUYREC),X'80'                                     
         BNZ   *+10                                                             
         MVC   0(3,R7),BTODAY                                                   
         MVI   PBDDTIND-NEWREC+REC,0                                            
         MVI   PBDDTIN2-NEWREC+REC,0                                            
         MVI   PBDDTIN3-NEWREC+REC,0                                            
*                                                                               
CHG1B    DS    0H                                                               
         CLI   DDLINKSW,C'C'       ADBUYER CHANGE INSERTION UPLOAD?             
         BE    CHG1B1              INDICATORS ARE SET IN T41118                 
*                                                                               
         MVI   CHGIND1,0           NEED TO CLEAR FOR LIST BUYS                  
         MVI   CHGIND2,0                                                        
         MVI   CHGIND3,0                                                        
         MVI   CHGIND4,0                                                        
*                                                                               
CHG1B1   TM    PBUYCNTL-NEWREC+REC,X'80'                                        
         BZ    CHG1B2                                                           
         CLI   BYPROF+5,C'Y'       SEE IF CHANGE ALLOWED                        
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         B     CHG2X               SKIP TO COMMENT CHANGE LOGIC                 
*                                                                               
CHG1B2   DS    0H                                                               
         LA    R7,PBDFREQ-NEWREC+REC                                            
         MVC   0(1,R7),PBDFREQ                                                  
*                                                                               
         LA    R7,PBDBUYER-NEWREC+REC                                           
         MVC   0(3,R7),BUYNM       SET CURRENT BUYER ID IN REC                  
*                                                                               
         CLI   BUYNM,C'*'          IF STARTS WITH * DON'T MOVE *                
         BNE   *+10                                                             
         MVC   0(3,R7),BUYNM+1                                                  
*                                                                               
         LA    R7,PBDJOB-NEWREC+REC                                             
         CLC   PBDJOB,0(R7)                                                     
         BE    CHG1D                                                            
*                                                                               
         CLI   WSJIND,1            CAN'T CHG JOB IF WSJ BUY                     
         BNE   CHG1C                                                            
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG1C    OC    0(6,R7),0(R7)       DONT SET CHNG IND IF THERE                   
         BNZ   CHG1C5              WAS NO OLD AD                                
         OI    CHGIND3,X'40'       SET AD CODE ADDED                            
         B     CHG1C6                                                           
CHG1C5   OI    CHGIND2,X'08'                                                    
CHG1C6   MVC   0(6,R7),PBDJOB                                                   
         OC    PBDJOB,PBDJOB                                                    
         BNZ   CHG1D                                                            
         MVI   ELCODE,X'70'        NON-WEB INSERTION ORDER ELEM                 
         BRAS  RE,CKIOEL                                                        
         BE    CHG1C8              ERROR                                        
         MVI   ELCODE,X'71'        WEB INSERTION ORDER ELEM                     
         BRAS  RE,CKIOEL                                                        
         BNE   CHG1D                                                            
CHG1C8   LA    R3,JBDERR           CANNOT REMOVE AD NO IF IO THERE              
         J     ERROR                                                            
*                                                                               
CHG1D    DS    0H                                                               
         CLC   REC+16(3),NEWREC+16 TEST NEW INSERTION DATE                      
         BE    CHG2                                                             
*                                                                               
* NOTE THAT DATE CHANGES FOR MATCHED INSERTIONS IS CHECKED IN PPBUY00           
*                                                                               
         CLI   WSJIND,1            CAN'T CHG DATE IF WSJ BUY                    
         BNE   CHG1D5                                                           
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG1D5   BRAS  RE,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BNE   CHG1D8                                                           
         LA    R3,NOCHGERR                                                      
         CLI   MADSW,C'Y'          IF PBU, NEED TO UNWIND UPLOAD REC            
         BNE   CHG1D7                                                           
         GOTOR VT41103,DMCB,(RC),(RA),FIXDATQ                                   
CHG1D7   L     R2,TRADDR                                                        
         J     ERROR                                                            
CHG1D8   MVI   8(RE),C'C'          RESET TO CHANGE FOR POL ROUTINES             
*                                                                               
         BRAS  RE,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BRAS  RE,ASC              AUTOMATIC SCHEDULE CHECKING                  
*                                                                               
         OI    CHGIND1,X'08'       DATE CHANGE                                  
*                                                                               
CHG2     DS    0H                                                               
         LA    R7,PBDRLIND-PBUYREC+REC                                          
         MVC   0(4,R7),PBDRLIND                                                 
*                                                                               
CHG2A    CLI   PBDBFD-NEWREC+REC,C'T'                                           
         BE    CHG2B                                                            
*                                                                               
         CLI   PBDBFD,C'T'                                                      
         BNE   CHG2B5                                                           
*                                                                               
CHG2ERR  LA    R3,INVERR           CAN'T MAKE LIVE BUY TEST                     
         L     R2,TRADDR                                                        
         BRAS  RE,BUMPFLD                                                       
         J     ERROR                                                            
*                                                                               
* FREEZE-BY-DATE LOGIC                                                          
*                                                                               
CHG2ERR1 LA    R3,FRZERR           CLIENT FROZEN - CAN'T MAKE LIVE              
         B     CHG2ERRX                                                         
CHG2ERR2 LA    R3,FDTERR           ERROR 139 - DATE FROZEN FOR CLIENT           
*                                                                               
CHG2ERRX L     R2,TRADDR                                                        
         BRAS  RE,BUMPFLD                                                       
         J     ERROR                                                            
*                                                                               
CHG2B    CLI   PBDBFD,C'T'         SEE IF STILL TEST                            
         BE    CHG2B5                                                           
*                                                                               
* NOTE - SVCLPROF+30 IS REALLY PCLTSTAT SAVED THERE IN PPBUY01                  
*                                                                               
         TM    SVCLPROF+30,X'02'   SEE IF CLIENT FROZEN                         
         BNO   C2BOK               NO                                           
         TM    SVCLPROF+30,X'10'   FROZEN WITH DATE ?                           
         BNO   CHG2ERR1            NO                                           
*                                                                               
* SVCLPROF+27 CONTAINS INDICATOR FROM FREEZE STATUS ELEM IN PCLTREC             
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         BO    C2BFORW             YES                                          
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         BO    C2BPAST             YES                                          
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         BO    *+6                 YES                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
* SVCLPROF+28 CONTAINS DATE (YM) FROM FREEZE STATUS ELEM IN PCLTREC             
*                                                                               
         CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BNE   C2BOK                                                            
         B     CHG2ERR2            NO BUYING FOR THIS MONTH                     
*                                                                               
C2BFORW  CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BL    C2BOK                                                            
         B     CHG2ERR2            NO BUYING FOR THIS MONTH & FORWARD           
*                                                                               
C2BPAST  CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BH    C2BOK                                                            
         B     CHG2ERR2            NO BUYING FOR THIS MONTH & PRIOR             
*                                                                               
C2BOK    DS    0H                                                               
         TM    SVESPROF+29,X'80'   SEE IF TEST ESTIMATE                         
         BNZ   CHG2ERR             THEN MUST STILL BE A TEST BUY                
         CLI   WSJIND,1            SEE IF WSJ BUY                               
         BE    CHG2WERR                                                         
*                                                                               
         BRAS  RE,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BRAS  RE,ASC              DO ASC WHEN MAKING A TEST BUY LIVE           
*                                                                               
         OI    CHGIND3,X'04'       SET MADE LIVE INDICATOR                      
         MVI   PBDDTIND-NEWREC+REC,0                                            
         MVI   PBDDTIN2-NEWREC+REC,0                                            
         MVI   PBDDTIN3-NEWREC+REC,0                                            
         MVC   PBDBUYDT-NEWREC+REC(3),BTODAY                                    
         MVI   ELCODE,X'24'                                                     
*                                                                               
CHG2B2   LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   CHG2B5                                                           
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
         B     CHG2B2                                                           
*                                                                               
CHG2B5   CLI   WSJIND,1            WSJ BUY CAN'T MAKE LIVE                      
         BNE   CHG2B6                                                           
         CLC   PBDBFD,PBDBFD-NEWREC+REC                                         
         BE    CHG2B6                                                           
CHG2WERR LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG2B6   LA    R7,PBDBFD-NEWREC+REC                                             
         MVC   0(1,R7),PBDBFD                                                   
*                                                                               
         LA    R7,PBDSPACE-NEWREC+REC                                           
         OC    PBDSPACE,=CL17' '   SPACE FILL SPACE DESCRIPTION                 
         OC    0(17,R7),=CL17' '   SPACE FILL SPACE DESCRIPTION                 
         CLC   PBDSPACE,0(R7)                                                   
         BE    CHG2C                                                            
*                                                                               
         TM    MATSW,X'40'         SEE IF MATCHED                               
         BNO   CHG2B7                                                           
*                                                                               
MCHGERR  LA    R3,MATERR                                                        
         J     ERROR                                                            
*                                                                               
CHG2B7   CLI   WSJIND,1            FOR WSJ BUYS - CAN'T CHG SPACE               
         BNE   CHG2B8                                                           
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG2B8   DS    0H                                                               
         CLI   0(R7),C'*'          SEE IF IT WAS A "REAL" INSERTION             
         BNE   CHG2B8C                                                          
         CLI   PBDSPACE,C'*'       IS IT STILL '*'                              
         BE    CHG2B9                                                           
         CLI   PBDSPACE,C'#'       OR CHANGED TO '#'                            
         BE    CHG2B9                                                           
*                                                                               
         BRAS  RE,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BRAS  RE,ASC              MUST DO ASC WHEN REMOVING A "*"              
         B     CHG2B9                                                           
*                                                                               
CHG2B8C  DS    0H                                                               
         CLI   0(R7),C'#'          SEE IF IT WAS TO BE IGNORED FOR ASC          
         BNE   CHG2B9                                                           
         CLI   PBDSPACE,C'#'       IS IT STILL '#'                              
         BE    CHG2B9                                                           
         CLI   PBDSPACE,C'*'       OR CHANGED TO '*'                            
         BE    CHG2B9                                                           
*                                                                               
         BRAS  RE,COM              FIRST DO COMPETITIVE BRAND CHECK             
*                                                                               
         BRAS  RE,ASC              MUST DO ASC WHEN REMOVING A "#"              
*                                                                               
CHG2B9   MVC   0(L'PBDSPACE,R7),PBDSPACE                                        
         OI    CHGIND1,X'10'       SPACE DESCRIPTION CHANGE                     
*                                                                               
CHG2C    LA    R7,PBDCOSIN-NEWREC+REC                                           
         CLC   PBDCOSIN(7),0(R7)   COST FIELD + INDICATORS                      
         BE    CHG2E                                                            
*                                                                               
         CLI   T411FFD+12,X'FF'    NO HEX SECURITY FOR BUY?                     
         BE    CHG2C6                                                           
         TM    T411FFD+12,X'01'    DISPLAY ONLY?                                
         BNZ   *+12                                                             
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         BZ    CHG2C6                                                           
         CLC   PBDCOSIN(5),0(R7)   COST FIELD + INDICATORS (NO PENNIES)         
         BNE   CHG2C6                                                           
         CP    PBDCOS,=P'-999999'  NEGATIVE TEN THOUSANDS?                      
         BH    CHG2C6                                                           
         MVC   BYTE2,PBDCOSIN+5                                                 
         OI    BYTE2,X'0F'         IGNORING CENT                                
         MVC   BYTE3,5(R7)                                                      
         OI    BYTE3,X'0F'         IGNORING CENT                                
         CLC   BYTE2,BYTE3                                                      
         JNE   CHG2C6              RATE IS CHANGED                              
         MVC   PBDCOSIN(7),0(R7)   PUT PENNIES BACK                             
         B     CHG2E               TREAT IT NO RATE CHANGE                      
*                                                                               
* EVEN IF COST NOT CHANGED GO CHK FOR PBDRCODE CHANGE IN CHG2E                  
*                                                                               
CHG2C6   TM    MATSW,X'C0'                                                      
         BM    MCHGERR             MIXED MUST BE MATCHED/NOT PAID               
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE CHANGING                   
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
         CLI   WSJIND,1            FOR WSJ BUYS - CAN'T CHG RATE/COST           
         BNE   CHG2D                                                            
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG2D    MVC   0(7,R7),PBDCOSIN                                                 
         LA    R7,PBDRCODE-NEWREC+REC                                           
         MVC   0(3,R7),PBDRCODE    ON RATE CHANGE - SAVE NEW RATECODE           
*                                                                               
         CLI   DDLINKSW,0          ADBUYER OR IDESK?                            
         BE    CHG2D4                                                           
         CP    PBDCOS,=P'0'        FREE INSERTION?                              
         BNE   CHG2D4                                                           
         CP    PBDCOS,REC+(PBDCOS-PBUYREC)(L'PBDCOS)                            
         BE    *+8                 RATE IS FREE, NOT CHANGED                    
CHG2D4   OI    CHGIND1,X'40'       COST CHANGE                                  
*                                                                               
         MVC   TRCODE,=C'RZ'                                                    
         B     CHG2F                                                            
*                                                                               
CHG2E    OC    PBDRCODE,PBDRCODE   SEE IF NEW RCODE INPUT                       
         BZ    CHG2F               NO - LEAVE PBDRCODE ALONE                    
         LA    R7,PBDRCODE-NEWREC+REC                                           
         CLC   PBDRCODE,0(R7)                                                   
         BE    CHG2F                                                            
*                                                                               
         TM    MATSW,X'C0'                                                      
         BM    MCHGERR             MIXED - MATCHED/NOT PAID                     
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE CHANGING                   
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
         CLI   WSJIND,1            WSJ BUY CAN'T CHANGE RCODE                   
         BNE   CHG2E5                                                           
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG2E5   MVC   0(3,R7),PBDRCODE    SAVE NEW RCODE - EVEN IF RATE                
         MVC   TRCODE,=C'RZ'       NOT CHANGED                                  
         CLI   SVESPROF+30,C'Y'    SEE IF FINANCIAL CLIENT                      
         BNE   CHG2F                                                            
         CLI   DDLINKSW,0          ADBUYER OR IDESK?                            
         BE    CHG2E7                                                           
         CP    PBDCOS,=P'0'        FREE INSERTION?                              
         BNE   CHG2E7                                                           
         CP    PBDCOS,REC+(PBDCOS-PBUYREC)(L'PBDCOS)                            
         BE    *+8                 RATE IS FREE, NOT CHANGED                    
CHG2E7   OI    CHGIND1,X'40'                                                    
*                                                                               
* SO JUST INPUTTING RCODE WILL CAUSE OPEN RATE LOOK-UP                          
* EVEN THOUGH CONTRACT RATE NOT CHANGED                                         
*                                                                               
CHG2F    LA    R7,PBDCTYP-NEWREC+REC                                            
         CLC   PBDCTYP(1),0(R7)                                                 
         BE    CHG2K                                                            
*                                                                               
         CLI   DDLINKSW,0          ADBUYER OR IDESK?                            
         BE    CHG2F2                                                           
         CP    PBDCOS,=P'0'        FREE INSERTION?                              
         BNE   CHG2F2                                                           
         CP    PBDCOS,REC+(PBDCOS-PBUYREC)(L'PBDCOS)                            
         BE    CHG2K               RATE IS FREE, NOT CHANGED                    
*                                                                               
CHG2F2   TM    MATSW,X'C0'                                                      
         BM    MCHGERR             MIXED - MATCHED/NOT PAID                     
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE CHANGING                   
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
         CLI   WSJIND,1            FOR WSJ BUYS - CAN'T CHG COST TYPE           
         BNE   CHG2G                                                            
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG2G    MVC   0(1,R7),PBDCTYP     NET COST CHANGE                              
         OI    CHGIND1,X'40'       SET COST CHANGE IND                          
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG2K    LA    R7,PBDCDATE-NEWREC+REC                                           
         CLC   PBDCDATE,0(R7)      CLOSE DATE                                   
         BE    CHG2P                                                            
         OC    PBDCDATE,PBDCDATE                                                
         BZ    CHG2P                                                            
         OI    CHGIND2,X'80'       CLOSE DATE CHANGE                            
         CLI   PBDCDATE,X'FF'                                                   
         BNE   *+10                                                             
         XC    PBDCDATE,PBDCDATE                                                
         MVC   0(3,R7),PBDCDATE                                                 
*                                                                               
CHG2P    LA    R7,PBDSDATE-NEWREC+REC                                           
         CLC   PBDSDATE,0(R7)      SALE DATE                                    
         BE    CHG2R                                                            
         OC    PBDSDATE,PBDSDATE                                                
         BZ    CHG2R                                                            
         OI    CHGIND2,X'40'       SALE DATE CHANGE                             
         CLI   PBDSDATE,X'FF'                                                   
         BNE   *+10                                                             
         XC    PBDSDATE,PBDSDATE                                                
         MVC   0(3,R7),PBDSDATE                                                 
*                                                                               
CHG2R    DS    0H                                                               
         LA    R7,PBDMDATE-NEWREC+REC                                           
         CLC   PBDMDATE,0(R7)      MAT CLOSING DATE                             
         BE    *+14                                                             
         MVC   0(3,R7),PBDMDATE                                                 
         OI    CHGIND3,X'02'       MAT CLOSING DATE CHANGE                      
*                                                                               
         CLC   BUYPB(2),=C'L='     TEST LIST SCREEN                             
         BE    CHG4                SKIP COMMENTS                                
*                                                                               
CHG2X    GOTOR VT41103,DMCB,(RC),(RA),RCONABEQ                                  
         BE    CHG4                ADBUYER ELEMS RECONSTRUCTED                  
*                                                                               
         L     R7,ACOMWRK                                                       
         LA    R4,10               FOR BCT                                      
CHG3     CLI   0(R7),X'FF'                                                      
         BE    NXTDUM1             GET NEXT ELEM                                
         MVC   ELCODE,0(R7)                                                     
CHG3A    LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   NXTDUM1             NO ELEMENTS IN OLD REC                       
*                                                                               
CHG3B    GOTO1 VRECUP,DMCB,(1,REC),(R5)                                         
         B     CHG3A                                                            
*                                                                               
NXTDUM1  LA    R7,60(R7)                                                        
         BCT   R4,CHG3                                                          
*                                                                               
CHG3C    DS    0H                                                               
         L     R7,ACOMWRK                                                       
         LA    R4,10                                                            
*                                                                               
* NOW ADD COMWRK ELEMENTS                                                       
*                                                                               
CHG3C1   CLI   0(R7),X'FF'                                                      
         BE    NXTDUM                                                           
         CLC   0(2,R7),=X'6602'    MEANS COMMENT WAS DELETED                    
         BE    CHG3D               GO SET CHANGE INDICATOR                      
         CLC   0(2,R7),=X'6702'                                                 
         BE    CHG3D                                                            
*                                                                               
         CLC   0(2,R7),=X'6802'    POSITION INSTRUCTIONS                        
         BE    CHG3D                                                            
*                                                                               
*                                                                               
         CLC   0(2,R7),=X'6A02'    SRC COMMENT                                  
         BE    CHG3D                                                            
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'F9'                                                     
         BRAS  RE,NXTELEM          SHOULD GET ME TO END OF REC                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ADD COMMENT ELEMENT                                                           
*                                                                               
         GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
*                                                                               
CHG3D    DS    0H                                                               
         CLI   0(R7),X'6A'         SRC COMMENT                                  
         BE    CHG3F                                                            
         CLI   0(R7),X'68'         POSITION INSTRUCTIONS                        
         BNE   CHG3E                                                            
         OI    CHGIND3,X'01'       POSITION INSTRUCTION CHG                     
         B     NXTDUM                                                           
*                                                                               
CHG3E    MVI   BYTE,X'02'          COMMENT CHANGE                               
         CLI   0(R7),X'66'                                                      
         BE    *+8                                                              
         MVI   BYTE,X'01'                                                       
         OC    CHGIND1,BYTE                                                     
         B     NXTDUM                                                           
*                                                                               
CHG3F    DS    0H                                                               
         MVI   BYTE,X'04'                                                       
         OC    CHGIND4,BYTE         SRC CHANGE                                  
         B     NXTDUM                                                           
*                                                                               
NXTDUM   LA    R7,60(R7)                                                        
         BCT   R4,CHG3C1                                                        
*                                                                               
CHG4     DS    0H                                                               
         TM    PBUYCNTL-NEWREC+REC,X'80'                                        
         BZ    *+14                                                             
         MVC   TRCODE(2),=C'RZ'    FORCE RECALL WHEN CHANGING                   
         B     CHG20               SKIP TO X'24' CHG ELEM LOGIC                 
*                                                                               
         TM    REC+(PBDSTAT2-PBUYREC),X'20'  SKIP IF NOT ADDED BY IDESK         
         BNO   CHG41                                                            
*                                                                               
         TM    CHGIND5,PCHGTRKQ    SKIP IF NOTHING TO TRACK                     
         BNO   CHG41                                                            
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        RESTORE ADDRESS                              
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),TRKCC_Q  TRACK STDCOLS                    
*                                                                               
CHG41    DS    0H                                                               
*                                                                               
         LA    R7,PBDPDATE-NEWREC+REC                                           
         CLC   PBDPDATE,0(R7)      TEST PAYABLE DATE CHANGE                     
         BE    CHG4A               NO                                           
         OC    PBDPDATE,PBDPDATE   TEST OVERRIDE                                
         BZ    CHG4A               NO                                           
         OI    CHGIND2,X'10'       PAYABLE DATE CHANGE                          
         MVC   0(3,R7),PBDPDATE                                                 
         MVI   ELCODE,X'25'        TEST PAID                                    
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   CHG4A                                                            
         OC    2(3,R5),2(R5)                                                    
         BZ    *-14                                                             
CHG4ERR  L     R2,TRADDR                                                        
         LA    R3,OVRDERR                                                       
         J     ERROR                                                            
CHG4A    CLC   PBDBDATE,3(R7)                                                   
         BE    CHG4B                                                            
         OC    PBDBDATE,PBDBDATE   TEST OVERRIDE                                
         BZ    CHG4B               NO                                           
         OI    CHGIND2,X'20'       BILLABLE DATE CHANGE                         
         MVC   3(3,R7),PBDBDATE                                                 
         MVI   ELCODE,X'26'        TEST BILLED                                  
         LA    R5,REC+33                                                        
CHG4A5   BRAS  RE,NXTELEM                                                       
         BNE   CHG4A6                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         TM    10(R5),X'C0'        IGNORE REVERSALS AND REVERSED ELEMS          
         BM    CHG4A5                                                           
         B     CHG4ERR                                                          
*                                                                               
CHG4A6   DS    0H                                                               
         MVI   ELCODE,X'28'        OPEN BILLING                                 
         LA    R5,REC+33                                                        
CHG4A7   BRAS  RE,NXTELEM                                                       
         BNE   CHG4A8                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         TM    10(R5),X'C0'        IGNORE REVERSALS AND REVERSED ELEMS          
         BNZ   CHG4A7                                                           
         B     CHG4ERR                                                          
*                                                                               
CHG4A8   DS    0H                                                               
         B     CHG4B                                                            
*                                                                               
CHG4B    CP    PBDCD,=P'0'         TEST IF CD PRESENT                           
         BE    CHG4D               NO - DIDN'T DO RATELOOK                      
         CP    PBDCD,=P'1'         TEST CD OVERRIDE OF 0                        
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'         RESET TO REAL VALUE                          
         LA    R7,PBDCD-NEWREC+REC                                              
         CLC   PBDCD,0(R7)         CASH DISCOUNT                                
         BE    CHG4D                                                            
*                                                                               
         TM    MATSW,X'C0'         SEE IF MATCHED                               
         BM    MCHGERR             MIXED - MATCHED/NOT PAID                     
*                                                                               
         MVC   0(L'PBDCD,R7),PBDCD                                              
         OI    CHGIND2,X'02'       C.D. CHANGE                                  
         MVC   TRCODE,=C'RZ'                                                    
*                                  AC CHANGE                                    
CHG4D    DS    0H                                                               
         CP    PBDACP,=P'0'                                                     
         BE    CHG5                                                             
         CP    PBDACP,=P'1'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'0'                                                     
         LA    R7,PBDACP-NEWREC+REC                                             
         CLC   PBDACP,0(R7)                                                     
         BE    CHG5                                                             
*                                                                               
         TM    MATSW,X'C0'         SEE IF MATCHED                               
         BM    MCHGERR             MIXED - MATCHED/NOT PAID                     
*                                                                               
         CLI   PBDCOSIN,C'C'       SEE IF 'C' RATE BUY                          
         BNE   CHG4F                                                            
         BRAS  RE,FMTTR            SEE IF BILLED/PAID                           
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BNE   CHG4G                                                            
         L     R2,TRADDR           CAN'T CHANGE AC IF BILLED/PAID               
         LA    R3,NOACCHG                                                       
         J     ERROR                                                            
*                                                                               
CHG4G    MVI   8(RE),C'C'          RESET TO CHANGE                              
*                                                                               
CHG4F    DS    0H                                                               
         MVC   0(L'PBDACP,R7),PBDACP                                            
         OI    CHGIND2,X'04'                                                    
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG5     DS    0H                                                               
         LA    R7,PBDPLCOS-NEWREC+REC                                           
         CLC   PBDPLCOS,0(R7)      TEST PLANNED COST                            
         BE    CHG5D                                                            
         OC    PBDPLCOS,PBDPLCOS                                                
         BE    CHG5D                                                            
         OI    CHGIND3,X'10'                                                    
         CLI   PBDPLCOS,X'FF'      TEST 'NONE'                                  
         BNE   *+10                                                             
         XC    PBDPLCOS,PBDPLCOS                                                
         MVC   0(4,R7),PBDPLCOS                                                 
*                                                                               
CHG5D    DS    0H                                                               
         OC    PBDTAX,PBDTAX                                                    
         BE    CHG5K                                                            
         CLC   PBDTAX,=X'000001'   TEST 'NONE'                                  
         BNE   *+10                                                             
         XC    PBDTAX,PBDTAX                                                    
         LA    R7,PBDTAX-NEWREC+REC                                             
         CLC   PBDTAX,0(R7)        TEST TAX                                     
         BE    CHG5K                                                            
*                                                                               
         BRAS  RE,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BNE   CHG5E                                                            
         MVI   8(RE),C'C'          RESET TO CHANGE                              
         L     R2,TRADDR                                                        
         LA    R3,TAXERR                                                        
         J     ERROR                                                            
*                                                                               
CHG5E    MVI   8(RE),C'C'          RESET TO CHANGE                              
*                                                                               
         TM    MATSW,X'40'         SEE IF MATCHED                               
         BO    MCHGERR             NO TAX CHANGE                                
*                                                                               
         OI    CHGIND3,X'08'       TAX CHANGE                                   
         MVC   0(3,R7),PBDTAX                                                   
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG5K    DS    0H                                                               
         OC    PBDCU,PBDCU         CONTRACT UNITS OVERRIDE                      
         BZ    CHG6                                                             
*                                                                               
         LA    R7,PBDCU-NEWREC+REC                                              
         MVC   0(3,R7),PBDCU                                                    
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
CHG6     DS    0H                                                               
         LA    R7,PBDIDAT2-NEWREC+REC                                           
         CLC   PBDIDAT2(4),0(R7)   TEST 2ND INS DATE AND E/M IND                
         BE    CHG6D                                                            
         OC    PBDIDAT2,PBDIDAT2                                                
         BE    CHG6D                                                            
         OI    CHGIND3,X'80'                                                    
         CLI   PBDIDAT2,X'FF'      TEST 'NONE' FOR DATE                         
         BNE   *+10                                                             
         XC    PBDIDAT2,PBDIDAT2                                                
         MVC   0(4,R7),PBDIDAT2                                                 
*                                                                               
CHG6D    DS    0H                                                               
CHG7     DS    0H                                                               
         CLC   PBDSPACE(2),=C'* '  TEST SPACE BUY                               
         BNH   CHG7A               NO - TREAT LINES NORMALLY                    
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    CHG7A               TREAT LINES NORMALLY                         
         CLC   PBDSPACE(2),=C'# '                                               
         BE    CHG7A               TREAT LINES NORMALLY                         
*                                                                               
         CP    PBDUNITS,=P'0'      TEST LINES INPUT                             
         BE    CHG7A5              NO - NO CHANGE                               
         CP    PBDUNITS,=P'-1'                                                  
         BNE   *+16                                                             
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
* BYTE2 WILL BE USED TO CHECK FOR INDICATOR CHANGES IN CHG7A3                   
*                                                                               
CHG7A    DS    0H                  COMPARING UNITS                              
         MVI   BYTE2,0                                                          
         LA    R7,PBDUIND-NEWREC+REC                                            
         CLC   PBDUIND,0(R7)       UNIT INDICATOR IS SAME?                      
         BE    CHG7A1_4            SAME INDICATOR, COMPARE UNIT                 
*                                                                               
* WORK+0(PL5) WILL HAVE NEWREC'S UNIT VALUE (2 DECIMALS)                        
* WORK+5(PL5) WILL HAVE REC'S    UNIT VALUE (2 DECIMALS)                        
*                                                                               
         ZAP   WORK(5),PBDUNITS                                                 
         CLI   PBDUIND,C'I'-C' '   LOWER CASE I?                                
         BE    CHG7A1_2                                                         
         CLI   PBDUIND,C'I'                                                     
         BNE   CHG7A1_4                                                         
         MP    WORK(5),=P'100'                                                  
CHG7A1_2 LA    R7,PBDUNITS-NEWREC+REC                                           
         ZAP   WORK+5(5),0(L'PBDUNITS,R7)                                       
         CLI   PBDUIND-NEWREC+REC,C'I'-C' '                                     
         BE    CHG7A1_3                                                         
         CLI   PBDUIND-NEWREC+REC,C'I'                                          
         BNE   CHG7A1_4                                                         
         MP    WORK+5(5),=P'100'                                                
CHG7A1_3 CP    WORK(5),WORK+5(5)                                                
         BNE   CHG7A1_6                                                         
         MVI   BYTE2,C'Y'          NO REAL CHANGE IN UNIT INDICATOR             
         B     CHG7A3              UNIT IS NOT CHANGED                          
*                                                                               
CHG7A1_4 LA    R7,PBDUNITS-NEWREC+REC                                           
         CLC   PBDUNITS,0(R7)                                                   
         BE    CHG7A3              UNIT IS NOT CHANGED                          
         CLI   MADSW,C'Y'          FROM $MAD?                                   
         BNE   CHG7A1_6                                                         
         CLC   PBDSPACE,PBDSPACE-NEWREC+REC                                     
         BE    CHG7A3                                                           
*                                                                               
CHG7A1_6 TM    MATSW,X'40'         MATCHED                                      
         BO    MCHGERR                                                          
*                                                                               
         CLI   WSJIND,1            FOR WSJ BUYS CAN'T CHG UNITS                 
         BNE   CHG7A2                                                           
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG7A2   MVC   0(L'PBDUNITS,R7),PBDUNITS                                        
         OI    CHGIND1,X'20'       UNITS CHANGE                                 
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
* BYTE2 IS SET EARLIER FROM CHECKING UNITS                                      
*                                                                               
CHG7A3   CLI   BYTE2,C'Y'          UNIT INDICATOR IS REALLY CHANGED?            
         BE    CHG7A5              NO                                           
         LA    R7,PBDUIND-NEWREC+REC                                            
         CLC   PBDUIND,0(R7)                                                    
         BE    CHG7A5                                                           
         CLI   MADSW,C'Y'          FROM $MAD?                                   
         BNE   *+14                                                             
         CLC   PBDSPACE,PBDSPACE-NEWREC+REC                                     
         BE    CHG7A5                                                           
*                                                                               
         TM    MATSW,X'40'         MATCHED                                      
         BO    MCHGERR                                                          
*                                                                               
         CLI   WSJIND,1            FOR WSJ BUYS CAN'T CHG UNITS                 
         BNE   CHG7A4                                                           
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
CHG7A4   MVC   0(1,R7),PBDUIND                                                  
         OI    CHGIND1,X'20'       UNITS CHANGE                                 
*                                                                               
CHG7A5   DS    0H                                                               
         LA    R7,PBDCL-NEWREC+REC                                              
         CLC   PBDCL(7),0(R7)      CLRS+IND+PR COST                             
         BE    CHG7A7X                                                          
*                                                                               
         CLC   PBDCL,0(R7)                                                      
         BE    CHG7A6                                                           
         TM    MATSW,X'40'         MATCHED - NO CHANGE TO COLOR                 
         BO    MCHGERR                                                          
*                                                                               
CHG7A6   CLC   PBDPRIN(6),1(R7)    CHECK IND AND CHARGE (PBDPRCOS)              
         BE    CHG7A7                                                           
         TM    MATSW,X'C0'         ALLOW CHANGE IF MATCHED AND PAID             
         BM    MCHGERR             MIXED - MATCHED AND NOT PAID                 
*                                                                               
         MVI   BYTE,C'C'           INDICATE "CHANGING"                          
         BRAS  RE,CKRATE           CHECK BY PROFILE BEFORE CHANGING             
         BNE   EXIT                RATE CHANGES ARE NOT ALLOWED                 
*                                                                               
CHG7A7   MVC   0(7,R7),PBDCL                                                    
         OI    CHGIND1,X'04'       PREMIUM CHANGE                               
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
* SEE IF PREMIUM AT NET CHANGED                                                 
* NOTE - A CHANGE IS OK EVEN IF MATCHED SINCE A                                 
* CHANGE IN PREM COST WAS CAUGHT AT CHG7A6                                      
* THE PREMIUM CHANGE INDICATOR NEED NOT BE SET EITHER                           
*                                                                               
CHG7A7X  DS    0H                                                               
         LA    R7,PBDPCTYP-NEWREC+REC                                           
         CLC   PBDPCTYP,0(R7)                                                   
         BE    CHG7A8                                                           
         MVC   0(1,R7),PBDPCTYP    STORE PREM INPUT AT NET IND                  
*                                                                               
CHG7A8   LA    R7,PBDCLMS-NEWREC+REC                                            
         CLC   PBDCLMS,0(R7)                                                    
         BE    CHG7B                                                            
         CLI   MADSW,C'Y'          FROM $MAD?                                   
         BNE   *+14                                                             
         CLC   PBDSPACE,PBDSPACE-NEWREC+REC                                     
         BE    CHG7B                                                            
*                                                                               
         TM    MATSW,X'40'         MATCHED                                      
         BO    MCHGERR                                                          
*                                                                               
         MVC   0(2,R7),PBDCLMS                                                  
         OI    CHGIND1,X'20'       COLUMNS                                      
*                                                                               
CHG7B    DS    0H                                                               
         LA    R7,PBDIODAT-NEWREC+REC                                           
         CLC   PBDIODAT,0(R7)                                                   
         BE    CHG8                                                             
         OC    PBDIODAT,PBDIODAT                                                
         BZ    CHG8                                                             
         OI    CHGIND2,X'01'       IO DATE CHANGE                               
         MVC   0(3,R7),PBDIODAT                                                 
*                                                                               
CHG8     BRAS  RE,EDIODAT                                                       
*                                                                               
CHG9     DS    0H                  SEARCH NEWREC FOR MANIO ELEMS                
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'70'                                                     
CHG9B    BRAS  RE,NXTELEM                                                       
         BNE   CHG10               DONE                                         
         OC    2(3,R5),2(R5)       CHK FOR DATE                                 
         BZ    CHG9B               NONE - IGNORE                                
         CLI   10(R5),C'X'         SPECIAL DELETE CODE                          
         BE    CHG9P                                                            
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHGMXERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG9D    BRAS  RE,NXTELEM                                                       
         BNE   CHG9J                                                            
         OC    2(3,R5),2(R5)       SEE IF NOT USED                              
         BNZ   CHG9E                                                            
         MVC   0(50,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG9W                                                            
*                                                                               
CHG9E    CLC   2(3,R5),2(R7)       COMPARE DATES                                
         BL    CHG9D               LOW - GO CHK NEXT OLD ELEM                   
*                                  EQU OR HIGH - ADD NEW MANIO HERE             
CHG9J    GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG9W                                                            
*                                                                               
CHG9P    LR    R7,R5               SAVE NEWREC'S R5                             
         SR    R4,R4                                                            
         LA    R5,REC+33                                                        
CHG9Q    BRAS  RE,NXTELEM                                                       
         BE    CHG9S                                                            
         LTR   R4,R4               WILL BE 0 IF MANIO ELEM NOT FND              
         BZ    CHG9ER2                                                          
         B     CHG9U               GO DELETE ELEM                               
*                                                                               
CHG9S    CLC   2(3,R5),2(R7)       CHK DATE                                     
         BL    CHG9Q               LOW SKIP                                     
         BE    CHG9T                                                            
         SR    R4,R4               SUBSEQUENT IO ISSUED CAN'T DELETE            
         B     CHG9Q                                                            
*                                                                               
CHG9T    CLC   5(5,R5),5(R7)       CHK IO NUMBER                                
         BNE   CHG9Q                                                            
         CLI   11(R5),C'M'         MUST BE MANUAL IO                            
         BNE   CHG9Q                                                            
         LR    R4,R5               SAVE ADDR OF MANIO ELEM TO DELETE            
         B     CHG9Q                                                            
*                                                                               
CHG9U    GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
CHG9W    LR    R5,R7               RESTORE NEWREC'S R5                          
         B     CHG9B               GO GET NEXT MANIO ELEM                       
*                                                                               
CHG9ER2  LA    R3,IOTYPER          MANIO NOT FOUND OR SUBSEQUENT                
         L     R2,TRADDR                                                        
         LHI   RF,8                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
         EJECT                                                                  
*                                                                               
CHG10    DS    0H                  PAGE VIEW ELEMENT                            
         MVI   ELCODE,X'87'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG11    DS    0H                  CLICK THROUGHS ELEMENT                       
         MVI   ELCODE,X'88'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG12    DS    0H                  SHIP DATE ELEMENT                            
         MVI   ELCODE,X'86'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG13    DS    0H                  EXTENSION DAYS ELEMENT                       
         MVI   ELCODE,X'89'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG13H   DS    0H                  EXTENSION DATE ELEMENT                       
         MVI   ELCODE,X'96'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG13M   DS    0H                  IMPRESSION ELEMENT                           
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG13P   DS    0H                  ACTUAL IMPRESSION ELEMENT                    
         MVI   ELCODE,X'93'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG13Q   DS    0H                  LEGAL WARNING ELEMENT                        
         MVI   ELCODE,X'94'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG14    DS    0H                  REFERENCE NUMBER ELEMENT                     
         MVI   ELCODE,X'83'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG14G   DS    0H                  ISSUE NAME                                   
         MVI   ELCODE,X'A6'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
         MVI   ELCODE,PORELMEQ     COS2 $ RATE (OPEN RATE) ELEMENT              
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG15    DS    0H                  SEARCH NEWREC REP ELEMS                      
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'                                                     
CHG15B   BRAS  RE,NXTELEM                                                       
         BNE   CHG16               DONE                                         
         CLI   2(R5),C'X'          SPECIAL DELETE CODE                          
         BE    CHG15P                                                           
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHGMXERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG15D   BRAS  RE,NXTELEM                                                       
         BNE   CHG15J                                                           
         CLC   0(6,R5),0(R7)       SEE IF SAME REP                              
         BE    CHG16               YES - SKIP TO CHG17                          
*                                  OLD REP ELEM FOUND JUST REPLACE IT           
         MVC   0(10,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG15W                                                           
*                                                                               
* ADD NEW REP ELEM HERE                                                         
*                                                                               
CHG15J   GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG15W                                                           
*                                                                               
CHG15P   LR    R7,R5               SAVE NEWREC'S R5                             
         SR    R4,R4                                                            
         LA    R5,REC+33                                                        
CHG15Q   BRAS  RE,NXTELEM                                                       
         BE    CHG15S                                                           
         LTR   R4,R4               WILL BE 0 IF REP ELEM NOT FND                
         BZ    CHG15ER2                                                         
         B     CHG15U              GO DELETE ELEM                               
*                                                                               
CHG15S   LR    R4,R5               SAVE ADDR OF REP ELEM TO DELETE              
         B     CHG15Q                                                           
*                                                                               
CHG15U   GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
CHG15W   LR    R5,R7               RESTORE NEWREC'S R5                          
         OI    CHGIND3,X'20'       REP CHANGED                                  
         B     CHG15B              GO GET REP ELEM - SHOULD NOT                 
*                                  FIND ANY MORE                                
CHG15ER2 LA    R3,NFNDERR          SEP REP ELEM NOT FOUND                       
         L     R2,TRADDR                                                        
         LHI   RF,8                CUSROR TO COMMENTS                           
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
CHG16    DS    0H                  FREE STANDING INSERTS ELEMENT                
         MVI   ELCODE,X'82'                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
*                                                                               
CHG17    DS    0H                  SEARCH NEWREC OPEN RATE ELEMS                
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'                                                     
CHG17B   BRAS  RE,NXTELEM                                                       
         BNE   CHG18               DONE                                         
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHGMXERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
CHG17D   BRAS  RE,NXTELEM                                                       
         BNE   CHG17J                                                           
         CLC   0(13,R5),0(R7)      SEE IF SAME RATE                             
         BE    CHG18               YES - SKIP TO CHG18                          
*                                  OLD RATE ELEM FOUND JUST REPLACE IT          
         CLI   8(R7),C'O'          MEANS I'M OVERRIDING OPEN RATE               
         BNE   CHG17F                                                           
         MVI   8(R7),0                                                          
         B     CHG17H                                                           
*                                                                               
CHG17F   TM    CHGIND1,X'40'       TEST FOR CONTRACT RATE CHG                   
         BNZ   CHG17H              YES THEN CHANGE OPEN                         
         LA    R1,PBDRCODE-NEWREC+REC                                           
         OC    0(3,R1),0(R1)                                                    
         BNZ   CHG18                                                            
*                                                                               
* SEE IF I HAD A RCODE SO I WON'T CHANGE OPEN BY                                
* ITSELF IF I HAVE A RCODE UNLESS I'M OVERRIDING                                
*                                                                               
CHG17H   MVC   0(13,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     CHG17W                                                           
*                                                                               
* ADD NEW OPEN RATE ELEM HERE                                                   
*                                                                               
CHG17J   GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG17W                                                           
*                                                                               
CHG17W   LR    R5,R7               RESTORE NEWREC'S R5                          
         OI    CHGIND1,X'40'       RATE CHANGED                                 
         MVC   TRCODE,=C'RZ'                                                    
         B     CHG17B              GET RATE ELEM, SHOULDN'T BE ANYMORE          
*                                                                               
CHG18    DS    0H                  PST ELEM OVERRIDE                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'84'                                                     
CHG18B   BRAS  RE,NXTELEM                                                       
         BNE   CHG19                                                            
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   CHGMXERR                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
CHG18B5  BRAS  RE,NXTELEM                                                       
         BNE   CHG18BX                                                          
         OC    2(3,R5),2(R5)       CHK FOR DATED PAY ELEM                       
         BZ    CHG18B5                                                          
         LA    R3,TAXERR                                                        
         J     ERROR                                                            
*                                                                               
CHG18BX  DS    0H                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'84'                                                     
CHG18D   BRAS  RE,NXTELEM                                                       
         BNE   CHG18J                                                           
         CLC   0(12,R5),0(R7)      SEE IF SAME PST DATA                         
         BE    CHG19               YES                                          
*                                                                               
         MVC   0(12,R5),0(R7)      PUT REC'S VALUE IN NEWREC                    
         B     CHG18W                                                           
*                                                                               
* ADD NEW PST ELEM HERE                                                         
*                                                                               
CHG18J   GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     CHG18W                                                           
*                                                                               
CHG18W   LR    R5,R7               RESTORE NEWREC'S R5                          
         B     CHG19                                                            
*                                                                               
CHG19    DS    0H                  SEE IF GST CHANGED                           
         LA    R7,PBDGST-NEWREC+REC                                             
         CLC   PBDGST,0(R7)                                                     
         BE    CHG19D             NO CHANGE                                     
*                                                                               
         LA    R5,REC+33           SEE IF PAID                                  
         MVI   ELCODE,X'25'                                                     
CHG19B5  BRAS  RE,NXTELEM                                                       
         BNE   CHG19BX                                                          
         OC    2(3,R5),2(R5)       CHK FOR DATED PAY ELEM                       
         BZ    CHG19B5                                                          
         LA    R3,TAXERR                                                        
         J     ERROR                                                            
*                                                                               
CHG19BX  DS    0H                                                               
         LA    R7,PBDGST-NEWREC+REC                                             
         MVC   0(1,R7),PBDGST                                                   
*                                                                               
CHG19D   DS    0H                                                               
         TM    PBDSTAT,X'0C'       SEE IF SFH ENTERED IN NEWREC                 
         BZ    CHG20                                                            
         MVC   BYTE,PBDSTAT-NEWREC+REC                                          
         NI    BYTE,X'0C'          SET-OFF ALL BUT X'08' AND X'04'              
         TM    PBDSTAT-NEWREC+REC,X'08'                                         
         BNO   CHG19D5                                                          
         TM    PBDSTAT,X'08'       IS HELD ON IN NEWREC                         
         BO    CHG19D5                                                          
         NI    PBDSTAT-NEWREC+REC,X'F7'                                         
*                                                                               
CHG19D5  OC    PBDSTAT-NEWREC+REC(1),PBDSTAT                                    
         MVC   BYTE2,PBDSTAT                                                    
         NI    BYTE2,X'0C'                                                      
         CLC   BYTE,BYTE2          CHECK NEWREC VS OLD PBDSTAT                  
         BE    *+8                                                              
         OI    CHGIND4,X'80'       SFH STATUS CHANGE                            
*                                                                               
CHG20    TM    PBDSTAT,X'20'       NO TRAFFIC?                                  
         BO    *+12                                                             
         NI    PBDSTAT-NEWREC+REC,X'FF'-X'20'                                   
         B     *+8                                                              
         OI    PBDSTAT-NEWREC+REC,X'20'                                         
*                                                                               
         LA    R7,PBDDTIND-NEWREC+REC                                           
         OC    0(1,R7),CHGIND1                                                  
         LA    R7,PBDDTIN2-NEWREC+REC                                           
         OC    0(1,R7),CHGIND2                                                  
         LA    R7,PBDDTIN3-NEWREC+REC                                           
         OC    0(1,R7),CHGIND3                                                  
*                                                                               
         TM    CHGIND1,X'FC'       ESTIMATE PRINT CHANGE?                       
         BNZ   CHGPUT2                                                          
         TM    CHGIND2,X'06'       CD OR AC                                     
         BNZ   CHGPUT2                                                          
         TM    CHGIND3,X'FF'                                                    
         BNZ   CHGPUT2                                                          
         MVC   BYTE,SVCLPROF+31    TEST JOB NO TO PRINT                         
         OC    BYTE,SVESPROF+31                                                 
         CLI   BYTE,C'0'                                                        
         BE    CHGPUT4                                                          
         TM    PBDDTIN2,X'08'      JOB NO.                                      
         BZ    CHGPUT4                                                          
*                                                                               
CHGPUT2  MVC   PBDCHGDT-NEWREC+REC(3),BTODAY                                    
*                                                                               
CHGPUT4  BRAS  RE,UPD              NEED TO ADD UPLOAD ELEM TO PBUYREC           
*                                                                               
         CLI   DDLINKSW,0          ADBUYER?                                     
         JE    CHGPUT4M                                                         
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   CHGPUT4M                                                         
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         JZ    CHGPUT4M                                                         
         J     *+14                NO NEED TO CHECK ZZZ                         
*                                                                               
CHGPUT4M CLC   =C'ZZZ',BUYPR                                                    
         BE    POL                                                              
*                                                                               
         BRAS  RE,MUP              NEED TO UPDATE UPLOAD MINIO REC              
*                                                                               
         TM    GENBYSW1,ADJCO2$Q   ADJUST COS2 RATE TO BUY RATE?                
         JZ    CHGPUT5P                                                         
         TM    GLBVALSW,BUYCOS2Q   COS2 $ ALREADY VALIDATED?                    
         BNZ   *+16                                                             
         GOTOR VAL_COS2,1          DO COS2 $ COPY (COPY FROM RATE)              
         JNE   ERROR                                                            
*                                                                               
CHGPUT5P TM    CHGIND1,X'08'       DATE CHANGE?                                 
         BZ    CHGPUT5             NO                                           
*                                                                               
* THIS CODE MOVED HERE FROM CHG2 TO PREVENT                                     
* CHANGING DIRECTORY WITHOUT CHANGING                                           
* THE BUYREC, WHICH COULD HAVE HAPPENED                                         
* IF ERROR WAS FOUND AFTER DATE CHANGE (POL NOW GOES TO CHGPUT5)                
*                                                                               
         TM    GLBVALSW,BUYPO#VQ   PURCHASE ORDER # ALREADY VALIDATED?          
         BNZ   *+16                                                             
         GOTOR VALBYPO#,1          DO PURCHASE ORDER # LOOKUP                   
         JNE   ERROR                                                            
         GOTOR VT41103,DMCB,(RC),(RA),CHGBRECQ                                  
         JNE   EXIT                                                             
         BRAS  RE,TESTERR                                                       
*                                                                               
CHGPUT5  GOTOR VT41103,DMCB,(RC),(RA),BYPO#ELQ                                  
         GOTOR VT41103,DMCB,(RC),(RA),CHGELEMQ                                  
         JNE   EXIT                                                             
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERROR                                                            
*                                                                               
         BRAS  RE,CKEIOELM         CK EIO ELEM                                  
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),EXCGET_Q  GET FX RATE/AMOUNT              
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    *+8                                                              
         BRAS  RE,PUTREC                                                        
*                                                                               
         CLI   BUYNM,C'*'          MEANS NO ASR ON CHANGES                      
         BE    CHGPUT9                                                          
*                                                                               
         TM    CHGIND1,X'7C'       CHK FOR DATE,SPACE,RATE CHANGE               
         BNZ   *+12                UNITS,PREMIUM                                
         TM    CHGIND3,X'04'       SEE IF TEST BUY MADE LIVE                    
         BZ    *+8                                                              
         BRAS  RE,ASR              DO AUTO SPACE RESV                           
*                                                                               
CHGPUT9  CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    CHGX                                                             
         TM    CHGIND1,X'08'       DATE CHANGED?                                
         BNZ   *+12                                                             
         TM    CHGIND2,X'08'       AD CODE IS CHANGED?                          
         BZ    *+8                                                              
         BRAS  RE,CKMATPTR         CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
CHGX     DS    0H                                                               
         MVC   NEWREC(25),REC      MOVE KEY TO NEWREC FOR FMTINS                
         CLI   SVSCRN,X'FA'                                                     
         BNL   BUYNX                                                            
         CLI   SVSCRN,X'F8'                                                     
         BNL   BUYLX                                                            
         DC    H'0'                                                             
*                                                                               
CHGMXERR LA    R3,MAXSERR          MAX SIZE ERROR                               
         B     ERROR                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DEL      DS    0H                                                               
         BRAS  RE,FNDINS           RETRIEVE INSERTION                           
*                                                                               
         MVI   BYTE,C'D'           INDICATE "DELETING"                          
         BRAS  RE,CKRATE           CHECK RATE BEFORE DELETION                   
         BNE   EXIT                BUY DELETIONS ARE NOT ALLOWED                
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),CKIDKC_Q  CK FOR IDESK CONTROL            
         JNE   EXIT                                                             
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBNVELQ      INVOICE ELEM PRESENT?                        
         BRAS  RE,NXTELEM                                                       
         BNE   DEL05                                                            
         CLI   DDLINKSW,0          ADBUYER?                                     
         BE    DEL03                                                            
         LA    R5,REC+33           CK FOR CLEARANCE STATUS                      
         MVI   ELCODE,X'25'        PAY ELEM                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DEL03                                                            
         OC    2(3,R5),2(R5)       PAID DATE PRESENT?                           
         BZ    *-14                                                             
         B     DEL05               CLEARED - ALLOW DELETION                     
*                                                                               
DEL03    LHI   R3,DLINVERR         CANNOT DELETE INSERTION W/ INV DATA          
         BRAS  RE,GET_ETXT         NOTE: "FULL" IS USED                         
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,BUYTR1H                                                       
         B     EXIT                                                             
*                                                                               
DEL05    MVI   WSJIND,0                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'35'        LOOK FOR WSJ ELEM                            
         BRAS  RE,NXTELEM                                                       
         BNE   DEL10                                                            
         MVI   WSJIND,1                                                         
*                                                                               
* FOR WSJ BUYS - CAN'T CHANGE DATE,SPACE,RATE,ALLOC OR DELETE                   
*                                                                               
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
DEL10    BRAS  RE,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
         MVI   DMOUTBTS,0          RESET FOR NO ERROR TESTS                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         BRAS  RE,READ                                                          
         BRAS  RE,CHECK                                                         
         OI    KEY+25,X'80'        SET DELETED IND                              
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERROR                                                            
         BRAS  RE,WRITE                                                         
         BRAS  RE,CHECK                                                         
*                                                                               
         MVI   KEY+3,X'21'         DELETE CLT/PUB POINTER                       
         MVC   KEY+7(6),REC+10     PUB                                          
         MVC   KEY+13(3),REC+7     PRD                                          
         BRAS  RE,READ                                                          
         BRAS  RE,CHECK                                                         
         OI    KEY+25,X'80'                                                     
*                                                                               
         BRAS  RE,WRITE            LOCK CHECKED EARLIER                         
         BRAS  RE,CHECK                                                         
         BRAS  RE,GETREC                                                        
         BRAS  RE,CHECK                                                         
*                                                                               
         OI    REC+27,X'80'                                                     
         LA    R7,PBDDATE-NEWREC+REC                                            
         MVC   0(3,R7),BTODAY      SET CHANGE DATE IN REC                       
*                                                                               
         LA    R7,PBDBUYER-NEWREC+REC                                           
         MVC   0(3,R7),BUYNM       SET BUYER ID IN REC                          
*                                                                               
         CLI   BUYNM,C'*'          IF STARTS WITH * DON'T MOVE *                
         BNE   *+10                                                             
         MVC   0(3,R7),BUYNM+1                                                  
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),DELINS_Q                                  
*                                                                               
         BRAS  RE,CKEIOELM         CK EIO ELEM                                  
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    *+16                                                             
         BRAS  RE,PUTREC           LOCK IS CHECKED EARLIER                      
         BRAS  RE,CHECK                                                         
         BRAS  RE,CKMATPTR         CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
         MVI   DMOUTBTS,X'FD'      RESET DMOUTBTS                               
         GOTOR VT41103,DMCB,(RC),(RA),CLEARBRQ                                  
         BRAS  RE,TESTERR                                                       
         CLC   =C'ZZZ',BUYPR       GO DELETE PASSIVE POINTERS                   
         BE    POL1A               SKIP ALLOC EDIT WHEN DELETING                
         BRAS  RE,ASR              DO AUTO SPACE RESV                           
*                                                                               
         BRAS  RE,MUP              GO SEE IF I NEED TO UPDATE MINIO             
*                                                                               
         B     CHGX                                                             
*                                                                               
CHECK    CLI   DMCB+8,0                                                         
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POL      DS    0H                  PRODUCT ALLOCATION                           
         L     R2,TRADDR                                                        
         LH    RE,SVNTRNS                                                       
         BCTR  RE,0                                                             
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   5(R2),0             TEST INPUT                                   
         BNE   POLA                                                             
         CLI   SVESTALO,C' '                                                    
         BNH   POLA                                                             
         MVC   8(47,R2),SVESTALO   USE EST ALO                                  
         FOUT  (R2)                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)            LENGHT                                       
*                                                                               
POLA     DS    0H                                                               
         MVI   X,C' '                                                           
         MVC   X+1(46),X                                                        
         OC    8(L'PJOBALO,R2),X                                                
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    POL1                                                             
         L     R4,AJOBIO                                                        
         CLI   PJOBALO-PJOBREC(R4),C' '                                         
         BNH   POL1                                                             
         CLC   8(L'PJOBALO,R2),PJOBALO-PJOBREC(R4)                              
         BE    POL1                                                             
         LA    R3,ALOERR                                                        
         J     ERROR                                                            
POL1     DS    0H                                                               
         TM    GENBYSW1,ALLOEXTQ   EXTENDED ALLOCS NOT VALIDATED                
         JNZ   POL01               ALREADY?                                     
         CLI   SVESTALO,C' '                                                    
         BNH   POL01                                                            
*                                                                               
         CLC   8(47,R2),SVESTALO   TEST VS EST ALO                              
         BE    POL01                                                            
         LA    R3,ALOERR2                                                       
         J     ERROR                                                            
*                                                                               
POL01    DS    0H                                                               
         BRAS  RE,MUP              MIGHT NEED TO UPDATE MINIO RECORD            
*                                                                               
* WHEN BUYING THIS TIME WILL JUST FIND ERRORS                                   
* AND ADD MINIO RECORD IF NOT FOUND                                             
*                                                                               
         L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          SEE IF DOING CHANGE                          
         BNE   POL1A                                                            
         TM    CHGIND1,X'08'       DATE CHANGE                                  
         BO    POL1A               YES - MUST REDO ALLOCATIONS                  
         TM    4(R2),X'20'         TEST FIELD MODIFIED                          
         BO    CHGPUT5             NO - SKIP EDIT                               
***                                NO ALLOC CHG - ASR (AUTO SP RESV)            
***                                                                             
****                                                                            
         CLI   WSJIND,1            NO ALLO CHG FOR WSJ BUYS                     
         BNE   POL1A                                                            
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
****                                                                            
POL1A    DS    0H                                                               
         MVI   DMCB,X'04'          T41104 - PRODUCT ALLOCATION                  
         BRAS  RE,CBUYOVLY                                                      
                                                                                
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         MVI   DMCB,X'01'          HEADER VALIDATION PHASE (RESTORE IT)         
         BRAS  RE,LOADOVLY                                                      
*                                                                               
         L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          CHANGE?                                      
         BNE   POL5                                                             
*                                                                               
         CLI   BUYNM,C'*'          MEANS NO ASR ON CHANGES                      
         BE    POL6                                                             
*                                                                               
         TM    CHGIND1,X'7C'       CHG IN DATE,SPACE,RATE,UNITS,PREM?           
         BNZ   POL5                                                             
         TM    CHGIND3,X'04'       OR MADE LIVE?                                
         BZ    POL6                                                             
*                                                                               
POL5     BRAS  RE,ASR              AUTO SPACE RESV                              
*                                                                               
POL6     L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          CHANGE?                                      
         BE    POL7                                                             
*                                                                               
         BRAS  RE,MUP              MIGHT NEED TO UPDATE MINIO RECORD            
*                                                                               
POL7     B     CHGX                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
EDTLNS   LR    R0,RE               LINES EDIT                                   
         BRAS  RE,BUMPFLD                                                       
         L     RF,RELO                                                          
         BRAS  RE,EDTLIN                                                        
         CLI   ERRAREA,0           ERROR?                                       
         JNE   EXXMOD                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EDTRTN   LR    R0,RE              RATE EDIT                                     
         ST    R9,FULL                                                          
         BRAS  RE,BUMPFLD                                                       
         L     RF,RELO                                                          
         BRAS  RE,EDTRAT                                                        
         CLI   ERRAREA,0          ERROR?                                        
         JNE   EXXMOD                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EDTPR    LR    R0,RE              PREMIUM EDIT                                  
         BRAS  RE,BUMPFLD                                                       
         BRAS  RE,EDTPREM                                                       
         CLI   ERRAREA,0          ERROR?                                        
         JNE   EXXMOD                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EDTCOM   LR    R0,RE                                                            
         L     RF,RELO                                                          
         BRAS  RE,EDTC                                                          
         CLI   ERRAREA,0          ERROR?                                        
         JNE   EXXMOD                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EDTDT    LR    R0,RE                                                            
         LA    R3,INVDTERR         DATES EDIT                                   
         BRAS  RE,BUMPFLD                                                       
         XC    DUB,DUB                                                          
         CLI   5(R2),0                                                          
         JE    EDTDTX                                                           
         GOTO1 VDATVAL,DMCB,(1,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR                                                            
         GOTO1 VDATCON,(R1),(0,WORK),(3,DUB)                                    
         MVC   DUB(1),NEWREC+16    MOVE INSERTION YEAR                          
         CLC   DUB+1(1),NEWREC+17  COMPARE THIS M TO INS MO.                    
         JNH   EDTDT5              IF LOW USE THIS YEAR                         
         IC    RE,DUB              ELSE USE PREVIOUS YEAR                       
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
*                                                                               
EDTDT5   GOTO1 VDATCON,(R1),(3,DUB),(5,WORK)                                    
         GOTO1 VDATVAL,(R1),(0,WORK),WORK+10                                    
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR               CATCH ERR ESCAPED FROM PREVIOUS DVAL         
*                                                                               
EDTDTX   LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
EDIODAT  OC    PBDIODAT,PBDIODAT   INSERTION ORDER DATE EDIT                    
         BZR   RE                                                               
         CLI   PBDIODAT,X'FF'                                                   
         BER   RE                  'NONE'                                       
         LA    R3,IODERR1                                                       
         CLC   PBDIODAT,BTODAY                                                  
         JL    ERROR               BEFORE TODAY                                 
         LA    R3,IODERR2                                                       
         J     EDIOD4              **NOP DATE TESTS**                           
         CLC   PBDIODAT,PBUYKDAT                                                
         JNL   ERROR               AFTER INS DATE                               
         OC    PBDCDATE,PBDCDATE                                                
         JZ    EDIOD4                                                           
         LA    R3,IODERR3                                                       
         CLC   PBDIODAT,PBDCDATE                                                
         JNL   ERROR               AFTER CLOSING DATE                           
EDIOD4   LA    R3,IODERR4                                                       
         OC    PBDJOB,PBDJOB                                                    
         JZ    ERROR               NO JOB                                       
         LA    R3,IODERR5                                                       
         CLI   PBDSPACE,C'*'       NO I.O. IF SPACE = *                         
         JE    ERROR                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIOEL   LR    R0,RE               TEST ANY IO'S PRINTED                        
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         JNE   X_RE_NEQ                                                         
         OC    2(3,R5),2(R5)                                                    
         JZ    *-14                                                             
         J      X_RE_EQ            FOUND TRAFFICKED INSERTION ORDER             
*                                                                               
X_RE_NEQ LHI   RE,1                SET CC NOT EQUAL                             
         J     X_RE_CC                                                          
X_RE_EQ  SR    RE,RE               SET CC EQUAL                                 
X_RE_CC  LTR   RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTINS   LR    R0,RE                                                            
         GOTO1 VEDTINS,DMCB,(RC),(RA)                                           
         CLC   PBUYKDAT,BKILL                                                   
         JL    *+12                                                             
         LA    R3,KDATERR                                                       
         J     ERROR                                                            
         BRAS  RE,BUMPFLD          POINT TO NEXT FIELD                          
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
FMTINS   LR    R0,RE                                                            
         GOTO1 VFMTINS,DMCB,NEWREC                                              
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
FMTTR    LR    R0,RE                                                            
         GOTO1 VFMTTR,DMCB,REC     FORMAT TR FIELD AT TRADDR                    
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
FMTRTN   LR    R0,RE                                                            
         L     RF,VFMTRTN                                                       
         LA    RE,4                SET COUNT FROM TR FIELD                      
         J     FMTBCT                                                           
*                                                                               
FMTPR    LR    R0,RE                                                            
         L     RF,VFMTPR                                                        
         LA    RE,5                SET COUNT FROM TR FIELD                      
*                                                                               
FMTBCT   CLC   BUYPB(2),=C'L='                                                  
         JE    *+6                 DO NOT ADJUST FOR LIST BUYING                
         BCTR  RE,0                                                             
         L     R2,TRADDR                                                        
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   RE,*-6                                                           
         BASR  RE,RF                                                            
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
FMTDTES  LR    R0,RE                                                            
         L     R2,TRADDR                                                        
         LHI   RF,6                                                             
         CLC   BUYPB(2),=C'L='     LIST BUY?                                    
         JNE   *+8                                                              
         LHI   RF,7                                                             
         BRAS  RE,BUMPFLDS         CURSOR TO MATERIALS CLOSE                    
         L     RF,VDATCON                                                       
         LA    R1,DMCB                                                          
         CLI   5(R2),0                                                          
         JNE   FMTDTES2                                                         
         OC    PBDMDATE,PBDMDATE                                                
         JZ    FMTDTES2                                                         
         GOTO1 (RF),(R1),(3,PBDMDATE),(7,8(R2))                                 
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
FMTDTES2 LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
NXTTR    LR    R0,RE                                                            
         GOTO1 VNXTTR                                                           
         LR    RE,R0                                                            
         CLI   ERRAREA,0                                                        
         JNE   TESTERR                                                          
         XC    INSDA,INSDA                                                      
         XC    INSKEY,INSKEY                                                    
         XC    INSADR,INSADR                                                    
         XC    BINSDT,BINSDT                                                    
         MVI   BSUBLN,0                                                         
         L     R2,TRADDR           GET NEW TR ADDR                              
         MVC   TRCODE,8(R2)                                                     
         LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
TESTERR  CLI   ERRAREA,0                                                        
         BER   RE                                                               
         J     EXXMOD                                                           
*                                                                               
BLDREC   LA    RF,BLDBRECQ                                                      
         B     G_T41103                                                         
*                                                                               
RTSUB    LA    RF,RTLOOKUQ                                                      
         B     G_T41103                                                         
*                                                                               
FSILOOK  LA    RF,FSILOOKQ                                                      
         B     G_T41103                                                         
*                                                                               
ADDLINE  LA    RF,ADDINSRQ                                                      
         B     G_T41103                                                         
*                                                                               
FNDINS   LA    RF,FINDINSQ                                                      
         B     G_T41103                                                         
*                                                                               
ASR      LA    RF,AUTOSRKQ                                                      
         B     G_T41103                                                         
*                                                                               
UPD      LA    RF,UPLELEMQ                                                      
         B     G_T41103                                                         
*                                                                               
ASC      LA    RF,AUTOSCKQ                                                      
         B     G_T41103                                                         
*                                                                               
COM      LA    RF,COMPBCKQ                                                      
         B     G_T41103                                                         
*                                                                               
MK_POREC LA    RF,MARKPORQ                                                      
         B     G_T41103                                                         
*                                                                               
MUP      LA    RF,MINIOULQ                                                      
*                                                                               
G_T41103 LR    R0,RE                                                            
         GOTOR VT41103,DMCB,(RC),(RA),(RF)                                      
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
EDTJOB   LR    R0,RE                                                            
         BRAS  RE,BUMPFLD                                                       
         CLI   5(R2),0                                                          
         JNE   EDTJOB2                                                          
         CLI   SVESTJOB,C' '       EST AD CODE PRESENT?                         
         JNH   EXIT_VRE                                                         
         MVC   8(6,R2),SVESTJOB                                                 
         MVI   5(R2),6             LENGTH                                       
         FOUT  (R2)                                                             
         J     EDTJOB4                                                          
EDTJOB2  CLC   =C'NONE',8(R2)                                                   
         JE    EXIT_VRE                                                         
         CLI   SVESTJOB,C' '                                                    
         JNH   EDTJOB4                                                          
         MVI   DUB,C' '                                                         
         MVC   DUB+1(5),DUB                                                     
         OC    DUB(6),8(R2)                                                     
         CLC   DUB(6),SVESTJOB     INPUT IS SAME AS EST AD CODE?                
         JE    EDTJOB4                                                          
         LA    R3,JOBERR4                                                       
         J     ERROR                                                            
EDTJOB4  GOTOR VT41103,DMCB,(RC),(RA),EDTADCDQ                                  
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
EXIT_VRE LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUMPFLDS SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         JH    BUMPFLDS                                                         
         BR    RE                                                               
*                                                                               
BUMPFLD  SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ALLDONE  BRAS  RE,MK_POREC         MARK PURCHASE ORDER RECORD                   
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),UPDPSV_Q  UPDATE DATE PASSIVES            
*                                                                               
         XC    BUYMSG,BUYMSG                                                    
         FOUT  BUYMSGH,CMPMSG                                                   
         MVI   ERRAREA,C'D'        FAKE ERROR TO SEND THIS MSG                  
         CLI   SVSCRN,X'FA'        TEST LIST BUYING                             
         BL    ALLDONE2                                                         
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         B     EXIT                                                             
*                                                                               
ALLDONE2 MVC   BUYMSGH+8+L'CMPMSG+1(L'LSTMSG),LSTMSG                            
         LA    R2,BUYTR1H          PUT CURSOR TO TR                             
         B     EXIT                                                             
*                                                                               
CMPMSG   DC    C'** ACTION COMPLETED **'                                        
LSTMSG   DC    C'ENTER ''R'' TO CONTINUE LIST'                                  
*                                                                               
         EJECT                                                                  
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR1                                                                   
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         J     EXIT_X                                                           
*                                                                               
DMERRS   L     RD,4(RD)            UNWIND WITHOUT XIT1                          
         LM    RE,RC,12(RD)                                                     
         LHI   R3,NRVDERR          NON-RECOVERABLE DISK ERROR                   
         J     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
         BRAS  RE,GET_ETXT                                                      
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         ST    R2,ADBERRFD         IN CASE CALLER NEEDS IT                      
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT_X   XIT1                                                                   
*                                                                               
SCCEQ_R  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SCCNEQ_R LTR   RB,RB               NOT EQUAL                                    
X_R2R3   XIT1  REGS=(R2,R3)        ERROR MSG AND CURSOR POSITION                
*                                                                               
CGWRKDT  LR    R0,RE                                                            
         CLI   BYPROF+10,C'Y'      NEED TO ADJUST TO PRIOR WORKDAY?             
         JNE   CGWRKDTX                                                         
         GOTOR VPPWKDAY,DMCB,(NATION,DUB),DUB,ACOMFACS                          
CGWRKDTX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FIXREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,NEWREC+33                                                     
FIXREC05 BRAS  RE,NXTELEM                                                       
         JNE   SCCEQ_R                                                          
*                                                                               
* CHECKING FOR SPECIAL DELETE CODES                                             
*                                                                               
         CLI   ELCODE,X'86'                                                     
         BNE   *+18                                                             
         CLC   2(3,R5),=3X'FF'                                                  
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'96'        EXTENSION DATE ELEM                          
         BNE   *+18                                                             
         CLC   2(3,R5),=3X'FF'                                                  
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'83'                                                     
         BNE   *+16                                                             
         CLI   2(R5),X'FF'                                                      
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'94'        LEGAL WARNINGS ELEM?                         
         BNE   *+16                                                             
         CLI   2(R5),X'FF'         DELETION CODE PRESENT?                       
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'A6'        ISSUE NAME ELEM?                             
         BNE   *+16                                                             
         CLI   2(R5),X'FF'         DELETION CODE PRESENT?                       
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   ELCODE,X'82'                                                     
         BNE   *+16                                                             
         USING PBFSIELD,R5                                                      
         CLI   PBFSI,C'X'                                                       
         BE    FIXREC25                                                         
         B     FIXREC10                                                         
*                                                                               
         CLI   2(R5),C'X'          SPECIAL DELETE CODE?                         
         BE    FIXREC25                                                         
*                                                                               
FIXREC10 CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   FIXERR1                                                          
         LR    R7,R5               SAVE R5, WHICH POINTS TO NEWREC              
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   FIXREC15                                                         
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,1(R5)          ELEM LENGTH                                  
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R7)       NO CHANGE?                                   
         JE    SCCEQ_R             YES                                          
*                                                                               
         CLI   ELCODE,X'82'                  FIS ELEMENT?                       
         BNE   *+12                                                             
         CLI   PBFSIIND-PBFSIEL(R7),X'02'    OVERRIDING?                        
         JNE   SCCEQ_R                       NO, LEAVE FSI DATA ALONE           
         DROP  R5                                                               
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R7)                                                    
*                                                                               
         CLI   ELCODE,X'87'        PAGE VIEW?                                   
         BE    FIXREC12                                                         
         CLI   ELCODE,X'88'        CLICK THROUGH?                               
         BE    FIXREC12                                                         
         CLI   ELCODE,X'92'        IMPRESSION?                                  
         BE    FIXREC12                                                         
         CLI   ELCODE,X'93'        ACTUAL IMPRESSION?                           
         BNE   *+8                                                              
FIXREC12 OI    CHGIND4,X'20'       CT, PV, IMPS OR AIMPS IS CHANGED             
*                                                                               
         CLI   ELCODE,X'94'        LEGAL WARNINGS?                              
         BNE   *+8                                                              
         OI    CHGIND4,X'08'       LEGAL WARNING IS CHANGED                     
*                                                                               
         B     FIXREC70                                                         
*                                                                               
FIXREC15 CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   FIXERR1                                                          
         GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)           ADDING ELEMENT           
*                                                                               
         CLI   ELCODE,X'87'        PAGE VIEW?                                   
         BE    FIXREC17                                                         
         CLI   ELCODE,X'88'        CLICK THROUGH?                               
         BE    FIXREC17                                                         
         CLI   ELCODE,X'92'        IMPRESSION?                                  
         BE    FIXREC17                                                         
         CLI   ELCODE,X'93'        ACTUAL IMPRESSION?                           
         BNE   *+8                                                              
FIXREC17 OI    CHGIND4,X'20'       CT, PV, IMPS OR AIMPS IS CHANGED             
*                                                                               
         CLI   ELCODE,X'94'        LEGAL WARNINGS?                              
         BNE   *+8                                                              
         OI    CHGIND4,X'08'       LEGAL WARNING IS CHANGED                     
*                                                                               
         B     FIXREC70                                                         
*                                                                               
FIXREC25 LR    R7,R5               SAVE R5, WHICH POINTS TO NEWREC              
         SR    R4,R4                                                            
         LA    R5,REC+33           POINT R5 TO REC                              
FIXREC28 BRAS  RE,NXTELEM                                                       
         BE    FIXREC30                                                         
         LTR   R4,R4                                                            
         BZ    FIXERR2             ELEM NOT FOUND                               
         B     FIXREC50            GO DELETE ELEM                               
*                                                                               
FIXREC30 LR    R4,R5                                                            
         B     FIXREC28                                                         
*                                                                               
FIXREC50 GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
         CLI   ELCODE,X'87'        PAGE VIEW?                                   
         BE    FIXREC55                                                         
         CLI   ELCODE,X'88'        CLICK THROUGH?                               
         BE    FIXREC55                                                         
         CLI   ELCODE,X'92'        IMPRESSION?                                  
         BE    FIXREC55                                                         
         CLI   ELCODE,X'93'        ACTUAL IMPRESSION?                           
         BNE   *+8                                                              
FIXREC55 OI    CHGIND4,X'20'       CT, PV,IMPS OR AIMPS IS CHANGED              
*                                                                               
         CLI   ELCODE,X'94'        LEGAL WARNINGS?                              
         BNE   *+8                                                              
         OI    CHGIND4,X'08'       LEGAL WARNING IS CHANGED                     
*                                                                               
FIXREC70 LR    R5,R7               RESTORE R5 TO POINT TO NEWREC                
         B     FIXREC05            NEXT ELEM, SHOULDN'T BE ANY MORE             
*                                                                               
FIXERR1  LA    R3,MAXSERR          ELEM MAXIMUM LENGTH ERROR                    
         J     SCCNEQ_R                                                         
*                                                                               
FIXERR2  LA    R3,INVERR           NO ELEMENT IS FOUND                          
         L     R2,TRADDR                                                        
         LA    RF,8                                                             
         BRAS  RE,BUMPFLDS         POINT R2 TO ERROR FIELD                      
         J     SCCNEQ_R                                                         
*                                                                               
FIXRECX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
FIXRERR  LTR   RB,RB               NOT EQUAL (ERROR)                            
         J     X_R2R3              ERROR MSG AND CURSOR POSITION                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PRODUCT EXCLUSION RESTRICTIONS (BYPROF+6 N,X,W)                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BEXCL    NTR1  BASE=*,LABEL=*      BEGINNING OF EXCLUSION CLASS VAL             
*                                                                               
         CLI   BYPROF+6,0                                                       
         BE    BEXCLX              NOTHING IN PROFILE, DONE                     
         CLI   BYPROF+6,C'N'                                                    
         BE    BEXCLX              NO NEED TO CHECK FOR EXCL CLASS              
*                                                                               
         CLI   SVPEXCL,0                                                        
         BE    BEXCLX              NO EXCL CLASS, DONE                          
*                                                                               
         L     R5,APUBIO                                                        
         LA    R5,33(R5)           POINT TO ELEMENTS                            
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NXTELEM          LOOKING FOR PRODUCTION ELEM                  
         BNE   BEXCLX              NOT FOUND, DONE WITH EXCL CLASS              
         USING PUBGENEL,R5                                                      
         CLI   PUBEXCL,0                                                        
         BE    BEXCLX              NO EXCL CLASS, DONE                          
         MVC   BYTE,PUBEXCL                                                     
         NC    BYTE,SVPEXCL                                                     
         BZ    BEXCLX              NO CONFLICTS IN EXCL CLASS, DONE             
*                                                                               
         CLI   BYPROF+6,C'X'                                                    
         BE    BEXCLER1            EXCL CLASS IS NOT ALLOWED IN BUY             
         CLI   BYPROF+6,C'W'                                                    
         BNE   BEXCLX              NO OTHER VALUES IN PROF+6                    
         OI    WARN,X'10'          TURN ON EXCL CLASS WARNING BIT               
         B     BEXCLX                                                           
         DROP  R5                                                               
*                                                                               
BEXCLER1 LHI   R3,XCLCNFER         EXCLUSION CONFLICT ERROR                     
         BRAS  RE,GET_ETXT         NOTE: "FULL" IS USED                         
         LA    R2,BUYTR1H                                                       
         MVI   ERRAREA,X'FF'                                                    
         B     BEXCLERR                                                         
*                                                                               
BEXCLX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
BEXCLERR LTR   RB,RB               NOT EQUAL (ERROR)CLASS VALIDATION            
         J     EXIT_X                                                           
*                                                                               
GET_ETXT LR    R0,RE               R3 HAS ERROR NUMBER                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NO RATE CHANGES FOR FULLY PAID BUYS  (BYPROF+7 IS "R" OR "B")                 
* NO BUY DELETIONS FOR FULLY PAID BUYS (BYPROF+7 IS "D" OR "B")                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKRATE   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   CKR50                                                            
*                                                                               
         CLI   BYTE,C'C'           CHANGING?                                    
         BNE   CKR10                                                            
         CLI   BYPROF+7,C'R'       CKING PROFILE                                
         BE    CKR20                                                            
         CLI   BYPROF+7,C'B'                                                    
         BE    CKR20                                                            
         J     SETCCEQ                                                          
*                                                                               
CKR10    CLI   BYTE,C'D'           DELETING?                                    
         BNE   CKR15                                                            
         CLI   BYPROF+7,C'D'       CKING PROFILE                                
         BE    CKR20                                                            
         CLI   BYPROF+7,C'B'                                                    
         BE    CKR20                                                            
         J     SETCCEQ                                                          
*                                                                               
CKR15    DC    H'0'                THERE'S NO OTHER ACTION AT THIS TIME         
*                                                                               
CKR20    DS    0H                                                               
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
*                                                                               
         CLC   PGROSS(12),GROSS    COMPARE UP TO CASH DISCOUNT                  
         JE    CKR30                                                            
*                                                                               
         J     SETCCEQ             DON'T CHECK FOR CANADA ANYMORE               
*                                                                               
         CLI   NATION,C'C'         CANADIAN AGY?                                
         JNE   CKR30                                                            
         OC    PAID,PAID           ANYTHING IN PAID AMOUNT?                     
         JZ    CKR30                                                            
         CLC   PAID,PYABLE         FULLY PAID?                                  
         JNE   SETCCEQ                                                          
CKR30    LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'        PAY ELEM EXIST?                              
         BRAS  RE,NXTELEM                                                       
         JNE   SETCCEQ             ALLOW RATE CHG/DEL                           
         OC    2(3,R5),2(R5)                                                    
         JZ    SETCCEQ             NO DATE, ALLOW RATE CHG/DEL                  
*                                                                               
CKR38ERR CLI   BYTE,C'C'                                                        
         BNE   CKR40                                                            
         LHI   R3,PRFRTCER         RATE CHANGES NOT ALLOWED                     
         B     CKR50                                                            
CKR40    LHI   R3,PRFIDLER         INSERTION DELETION NOT ALLOWED               
*                                                                               
CKR50    BRAS  RE,GET_ETXT         NOTE: "FULL" IS USED                         
         LA    R2,BUYTR1H                                                       
         MVI   ERRAREA,X'FF'                                                    
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCLTFRZ NTR1  BASE=*,LABEL=*      CHECK CLIENT FROZEN OPTION                   
*                                                                               
         CLI   BYPROF+14,C'Y'      NO CHANGE TO FRZ CLIENT?                     
         JNE   CKCFRZ_X                                                         
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT?                               
         JZ    CKCFRZ_X                                                         
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         JNZ   CKCFRZ20                                                         
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         JNZ   CKCFRZ40                                                         
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         JNZ   CKCFRZ60                                                         
*                                                                               
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ20 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JL    CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ40 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JH    CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ60 CLC   PBUYKDAT(2),SVCLPROF+28                                          
         JNE   CKCFRZ_X                                                         
         J     CKCFRZER            NO FINANCIAL CHANGES TO FRZ CLIENT           
*                                                                               
CKCFRZ_X J     SETCCEQ                                                          
CKCFRZER J     SETCCNEQ                                                         
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKEIOELM NTR1  BASE=*,LABEL=*      CK EIO ELEM                                  
*                                                                               
         LA    R5,REC+33                                                        
         USING PWIOELEM,R5                                                      
         MVI   ELCODE,PWIOELCQ                                                  
CKEIO10  BRAS  RE,NXTELEM                                                       
         BNE   CKEIO20                                                          
         LR    RF,R5                                                            
         B     CKEIO10                                                          
*                                                                               
CKEIO20  LR    R5,RF                                                            
         CLI   PWIOELCO,PWIOELCQ   LAST EIO ELEM FOUND?                         
         BNE   CKEIO_X                                                          
         CLI   PWIOINSL,0          NO INSERTION LINE NUMBER?                    
         BNE   CKEIO_X                                                          
         MVC   PWIOINSL,REC+24     SAVE INSERTION LINE NUMBER                   
*                                                                               
CKEIO_X  J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMATPTR NTR1  BASE=*,LABEL=*      CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
         MVC   BYTE4,DMINBTS                                                    
         LA    R5,REC+33                                                        
         USING PWIOELEM,R5                                                      
         MVI   ELCODE,PWIOELCQ                                                  
CKMPP10  BRAS  RE,NXTELEM                                                       
         BNE   CKMPP20                                                          
         LR    RF,R5                                                            
         B     CKMPP10                                                          
*                                                                               
CKMPP20  LR    R5,RF                                                            
         CLI   PWIOELCO,PWIOELCQ   LAST EIO ELEM FOUND?                         
         BNE   CKMPP_X                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PWIOINDT),(2,DUB)                                
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING PMTPKEY,RF                                                       
         MVC   KEY(10),REC         AGY/MED/X/CLT/PRD                            
         MVI   PMTPKRCD,PMTPKRCQ   MAT= REPEAT PASSIVE CODE                     
         MVC   PMTPKADC,PWIOADCD   AD CODE                                      
         MVC   PMTPKPUB,REC+10     PUB                                          
         TM    PWIOSTAT,PWIOSMAQ   MAT= REPEAT ACROSS ZONE/EDITION?             
         BNZ   *+10                                                             
         MVC   PMTPKZON(2),REC+14                                               
         MVC   PMTPKDAT,DUB        COMPRESSED INSERTION DATE                    
         MVC   PMTPKLIN,PWIOINSL   INSERTION LINE NUMBER                        
         CLI   PMTPKLIN,0                                                       
         BH    *+6                                                              
         DC    H'0'                INSERTION LINE NUMBER IS NOT SET             
         NI    DMINBTS,X'FF'-X'08' NO DELETES                                   
         BRAS  RE,HIGH                                                          
         CLC   KEY(L'PMTPKEY),KEYSAVE                                           
         BNE   CKMPP_X                                                          
         OI    KEY+25,PMTPC1DQ     SET TO DELETE                                
         BRAS  RE,WRITE                                                         
         BRAS  RE,CHECK                                                         
*                                                                               
CKMPP_X  MVC   DMINBTS,BYTE4                                                    
         J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB,R5,RF                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
*                                                                               
TSTLK2   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         JE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         JE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK3                                                           
*                                                                               
TSTLK4   XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
         MVC   L.LOCKPUB,REC+10    PACKED BASE PUB NUMBER                       
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
TSTLK5   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         JE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         JNE   SETCCEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         JE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         DROP  L                                                                
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO EDIT NEWSPAPER RATES                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTRAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    RF,ERRELO           IN CASE AN INCLUDED MODULE NEED IT           
*                                                                               
         LA    R3,INVRTERR                                                      
         XC    PBDRCODE,PBDRCODE   CLEAR RATE CODE                              
         MVI   BYTE,5              PRESET FOR 5 DEC.                            
         MVI   PBDCOSTY,C'U'       PRESET RATE TYPE                             
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BE    EDTR2               SKIP SPACE CHECK                             
*                                                                               
         CLI   RATIND,1            MAY ALSO HAVE SPACE                          
         BE    EDTR2                                                            
*                                                                               
         CLC   PBDSPACE(2),=C'* '  UNLESS SPACE BUY                             
         BNH   EDTR2                                                            
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    EDTR2               TREAT AS NON-SPACE BUY                       
         CLC   PBDSPACE(2),=C'# '                                               
         BE    EDTR2               TREAT AS NON-SPACE BUY                       
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   PRISMA INSERTION UPLOAD?                     
         JZ    EDTR_20                                                          
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
EDTR_10E CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         JE    EDTR_10X            MUST FIND TOTAL COST MAP CODE                
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         JE    *+12                                                             
EDTR_10H BRAS  RE,NXTWFELM                                                      
         J     EDTR_10E                                                         
         CLC   =AL2(D#PNTOTC),3(R3)                                             
         JNE   EDTR_10H                                                         
         SR    RF,RF                                                            
         ICM   RF,3,1(R3)                                                       
         AHI   RF,-6               NEW RATE INPUT LENGTH                        
         LLC   RE,5(R2)            OLD RATE INPUT LENGTH                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         XC    8(,R2),8(R2)        CLEAR OLD FIELD DATA                         
         NI    4(R2),X'FF'-X'20'   RESET PREVIOUSLY VALIDATED                   
         STC   RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   8(0,R2),6(R3)       NEW FIELD DATA                               
         OI    6(R2),X'80'         TRANSMIT                                     
         J     EDTR_20                                                          
EDTR_10X LA    R3,INVMAPCD         INVALID MAP CODE (NO TOTAL COST)             
         J     ERROR                                                            
*                                                                               
EDTR_20  MVI   PBDCOSTY,C'T'                                                    
         MVI   BYTE,2                                                           
*                                                                               
EDTR2    MVI   PBDCOSIN,C' '                                                    
         MVI   PBDCTYP,0                                                        
*                                                                               
         CLI   FDMISS,C'Y'         SEE IF FD NOT FOUND FOR PUB                  
         BNE   EDTR2C                                                           
         CLI   8(R2),C'T'          MUST ENTER TOTAL RATE                        
         BE    EDTR2C                                                           
         CLC   8(2,R2),=C'NT'      OR NET TOTAL RATE                            
         BE    EDTR2C                                                           
         CLC   8(2,R2),=C'ST'      OR S TOTAL RATE                              
         JNE   ERROR                                                            
*                                                                               
EDTR2C   CLI   5(R2),0                                                          
         BE    EDTRATX                                                          
         SR    R7,R7                                                            
         IC    R7,5(R2)                                                         
         LA    R6,8(R2)                                                         
         TM    4(R2),X'04'         TEST VALID ALPHA                             
         BNZ   EDTR7               (FREE, ETC)                                  
*                                                                               
EDTR3    CLI   0(R6),C'S'          GROSS=NET                                    
         BNE   EDTR3A                                                           
         ZAP   PBDACP,=P'1'        TO PREVENT AC LOOK-UP                        
         MVI   PBDCOSIN,C'S'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
*                                                                               
EDTR3A   CLI   0(R6),C'R'          ROADSIDE?                                    
         BNE   EDTR3C                                                           
         ZAP   PBDACP,=P'16667'    SET AC RATE                                  
         MVI   PBDCOSIN,C'R'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
*                                                                               
EDTR3C   CLI   0(R6),C'C'          COMMISSION RATE                              
         BNE   EDTR4                                                            
         CLI   PBDCOSIN,C' '                                                    
         JNE   ERROR               CAN'T ALREADY HAVE                           
         MVI   PBDCOSIN,C'C'       GETINS SETS GROSS TO AGYCOM                  
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
*                                                                               
EDTR4    CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   EDTR5                                                            
         MVI   PBDCTYP,C'N'                                                     
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
         B     EDTR3                                                            
*                                                                               
EDTR5    CLI   0(R6),C'T'          TOTAL RATE                                   
         BNE   EDTR6                                                            
         MVI   PBDCOSTY,C'T'                                                    
         MVI   BYTE,2              DECIMALS                                     
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
         B     EDTR3                                                            
*                                                                               
EDTR6    CLI   0(R6),C'*'          FROZEN RATE                                  
         BNE   EDTR6D                                                           
         OI    PBDRLIND,X'08'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
         B     EDTR3                                                            
*                                                                               
EDTR6D   CLC   0(2,R6),=C'R='      RATE CODE                                    
         BNE   EDTR7                                                            
         LA    R6,2(R6)                                                         
         BCTR  R7,0                                                             
         BCTR  R7,0                                                             
         CLI   0(R6),C' '                                                       
         JNH   ERROR                                                            
         CHI   R7,3                MAX 3 CHARS                                  
         JH    ERROR                                                            
         MVC   PBDRCODE,0(R6)      SAVE CODE IN PBUYREC                         
         OC    PBDRCODE,=3C' '                                                  
*                                  NEW RATE LOOK WILL FIND RATE                 
         B     EDTRATX                                                          
*                                                                               
EDTR7    CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE EST                          
         BNE   EDTR8                                                            
         CLI   PBDCOSIN,C'C'       'C' INPUT                                    
         BE    EDTR10                                                           
         CLI   PBDCOSIN,C' '       NOTHING INPUT  - SET TO 'C'                  
         JNE   ERROR                                                            
         MVI   PBDCOSIN,C'C'                                                    
         B     EDTR10                                                           
*                                                                               
EDTR8    CLI   PBDCOSIN,C'C'       'C' RATE ON NON 'C' RATE EST                 
         JE    ERROR                                                            
*                                                                               
EDTR10   DS    0H                  SPECIAL HANDLING FOR "SFREE" BUYS            
         CHI   R7,5                INPUT LENGTH 5 ?                             
         BNE   EDTR12              NO                                           
         CLC   8(5,R2),=C'SFREE'   IS INPUT "SFREE"                             
         BNE   EDTR12              NO                                           
         LA    R6,1(R6)            POINT TO "FREE"                              
         BCTR  R7,0                REDUCE LENGTH FROM 5 TO 4                    
         MVI   PBDCOSIN,C'S'       RETAIN THE S ON A FREE BUY                   
*                                                                               
EDTR12   DS    0H                  SPECIAL HANDLING FOR "RFREE" BUYS            
         CHI   R7,5                INPUT LENGTH 5 ?                             
         BNE   EDTR14              NO                                           
         CLC   8(5,R2),=C'RFREE'   IS INPUT "RFREE"                             
         BNE   EDTR14              NO                                           
         LA    R6,1(R6)            POINT TO "FREE"                              
         BCTR  R7,0                REDUCE LENGTH FROM 5 TO 4                    
         MVI   PBDCOSIN,C'R'       RETAIN THE R ON A FREE BUY                   
         ZAP   PBDACP,=P'16667'    SET AC RATE                                  
*                                                                               
EDTR14   DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,(BYTE,(R6)),(X'40',(R7))                           
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         L     RE,4(R1)                                                         
         CVD   RE,DUB                                                           
         CP    DUB,=P'849999999'   MAX FOR PDBCOS                               
         JH    ERROR                                                            
         CLI   BYTE,5              SEE IF 5 DECIMALS                            
         BNE   EDTR14D                                                          
         CP    DUB,=P'299999999'   MAX UNIT RATE IS 2999.99999                  
         JH    ERROR               ** THEY HAVE PROBABLY ENTERED A              
*                                  TOTAL COST BY MISTAKE **                     
EDTR14D  ZAP   PBDCOS,DUB                                                       
         LTR   RE,RE               IF RATE IS FREE, SET COST TO                 
         BNZ   *+10                .00001 AND RESET AFTER                       
         ZAP   PBDCOS,=P'1'        RATE LOOK-UP                                 
*                                                                               
EDTRATX  BRAS  RE,CHKGRS                                                        
*                                                                               
EDTRATXX J     EXIT_X                                                           
*                                                                               
CHKGRS   DS    0H                  CHECK NEWS GROSS NOT TOO LARGE               
         CLI   PBDCOSTY,C'U'                                                    
         BNER  RE                                                               
         CP    PBDCOS,=P'300000000'                                             
         BH    CHKGRS8             PROBABLY TOTAL COST                          
         CP    PBDCOS,=P'-300000000'                                            
         BL    CHKGRS8             PROBABLY TOTAL COST                          
*                                                                               
* NOT LINE OR INCH RATE, TRY TO PROTECT MULTILPY PACKED INS                     
*                                                                               
         ZAP   WORK(16),PBDUNITS                                                
         MP    WORK(16),PBDCOS     PBDCOS HAS 5 DECIMALS                        
         DP    WORK(16),=P'100'                                                 
         ZAP   WORK(16),WORK(14)                                                
         DP    WORK(16),=P'10'                                                  
         CP    WORK(14),=P'-2100000000'                                         
         BL    CHKGRS8                                                          
         CP    WORK(14),=P'2100000000'                                          
         BLR   RE                                                               
*                                                                               
CHKGRS8  LA    R3,INVRTERR                                                      
         J     ERROR                                                            
*                                                                               
NXTWFELM SR    R0,R0                                                            
         ICM   R0,3,(LQ_LN-LQ_D)(R3)                                            
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
ERRELO   DS    F                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTLIN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    RF,ERELO            IN CASE AN INCLUDED MODULE NEED IT           
*                                                                               
         MVI   FDMISS,0                                                         
         MVI   RATIND,0                                                         
         MVI   TMPSPCE,C'N'                                                     
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         JE    ERROR                                                            
         CLI   8(R2),C' '          CAN'T START WITH A SPACE                     
         JE    ERROR                                                            
*                                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LA    R4,8(R2)                                                         
*                                                                               
         CLI   0(R4),C'*'          TEST NOT 'REAL' INS                          
         BE    EDTLNS0                                                          
         CLI   0(R4),C'#'          OR NO ASC INS                                
         BNE   EDTLNS1                                                          
*                                                                               
EDTLNS0  MVC   PBDSPACE(1),0(R4)   SAVE IND                                     
         LA    R4,1(R4)            BUMP POINTER                                 
         BCTR  R5,0                AND LENGTH                                   
         LTR   R5,R5                                                            
         BZ    EDTLNS8                                                          
*                                                                               
EDTLNS1  STM   R4,R5,DUB           SAVE PTR/LEN                                 
EDTLNS2  CLI   0(R4),C'0'          VERIFY NUMERIC                               
         BL    EDTLNS3                                                          
         CLI   0(R4),C'9'                                                       
         JH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,EDTLNS2                                                       
*                                                                               
EDTLNS3  L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BNP   EDTL9                                                            
         BCTR  R4,0                                                             
         EX    R4,PACKLN                                                        
         CP    WORK(5),=P'99999'   MAX LINES OR INCHES                          
         JH    ERROR               THAT CAN BE CARRIED IN PBDUNITS              
         ZAP   PBDUNITS,WORK(5)                                                 
*                                                                               
         MVI   PBDUIND,C'L'                                                     
         LTR   R5,R5                                                            
         BZ    EDTLNS8             NO MORE INPUT                                
         LA    R4,1(RE,R4)         R4 = BYTE PAST NUMBER                        
         CLI   0(R4),C'L'                                                       
         BE    EDTLNS4                                                          
         CLI   0(R4),C'/'                                                       
         BE    EDTLNS5                                                          
         MVI   PBDUIND,C'I'                                                     
         CLI   0(R4),C'I'                                                       
         BE    EDTLNS4                                                          
         CLI   0(R4),C'X'          NEW SAU NNXNN.NN                             
*                                  COLUMNS X INCHES (2 DECIMALS)                
         BNE   EDTLNS3C            NO                                           
         CP    PBDUNITS,=P'14'     MAX COLS =14                                 
         BH    EDTL9               TREAT AS SPACE                               
         CP    PBDUNITS,=P'0'                                                   
         BNH   EDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,*+8              NO MORE INPUT                                
         B     EDTL9               NO MUST BE SPACE                             
         CLC   0(2,R4),=C'FD'                                                   
         BNE   EDTLNS3B                                                         
         CLI   2(R4),C' '                                                       
         BNL   EDTL9               INPUT AFTER FD                               
         L     R5,APUBIO                                                        
         CLI   0(R5),0             SEE IF PUB THERE                             
         BNE   EDTLNS32                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),SVPUBDA   MUST REREAD PUB                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',         X        
               KEY+27,APUBIO,(TERMNAL,DMWORK)                                   
*                                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND PUB                                
*                                                                               
EDTLNS32 LA    R5,33(R5)                                                        
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NXTELEM                                                       
         BE    EDTLNS3A                                                         
         B     NOFD                CAN'T FIND PROD ELEM                         
*                                                                               
         USING PUBGENEL,R5                                                      
EDTLNS3A OC    PUBFD,PUBFD         SEE IF I HAVE FD                             
         BZ    NOFD                NO - TREAT AS SPACE                          
         CP    PUBFD,=P'0'         FD IS ZERO?                                  
         BE    NOFD                                                             
         ZAP   PBDCLMS,PBDUNITS                                                 
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,PUBFD                                                        
         CP    DUB,=P'99999'       MAX IS 999.99 COL INCHES                     
         JH    ERROR                                                            
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE                            
         B     EDTL9B                                                           
*                                                                               
NOFD     MVI   FDMISS,C'Y'         SET FD NOT FOUND SWITCH                      
         B     EDTL9                                                            
         DROP  R5                                                               
*                                                                               
EDTLNS3B LR    R1,R4                                                            
         AR    R1,R5                                                            
         BCTR  R1,0                                                             
         CLI   0(R1),C'L'          WILL BE L FOR NNXNNNL FORMAT                 
         BNE   EDTLN5                                                           
         CHI   R5,2                MUST BE AT LEAST NL                          
         BL    EDTL9                                                            
         BCTR  R5,0                                                             
         GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    EDTL9                                                            
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    EDTL9                                                            
         ZAP   PBDCLMS,PBDUNITS                                                 
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   EDTL9                                                            
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER ZERO                      
         BNE   EDTL9                                                            
         ZAP   DUB,DUB(6)                                                       
         MP    DUB,PBDCLMS                                                      
         CP    DUB,=P'99999'       MAX LINES                                    
         BH    EDTL9                                                            
         MVI   PBDUIND,C'L'        RESET TO LINES - L                           
         ZAP   PBDUNITS,DUB                                                     
         MVI   RATIND,1                                                         
         B     EDTL9B              SAVES SPACE                                  
*                                                                               
EDTLN5   CLI   0(R1),C'I'                                                       
         BNE   EDTLN5B                                                          
         BCTR  R5,0                                                             
*                                                                               
EDTLN5B  GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    EDTL9                                                            
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    EDTL9                                                            
         ZAP   PBDCLMS,PBDUNITS                                                 
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   EDTL9                                                            
         MP    DUB,PBDCLMS                                                      
         CP    DUB,=P'99999'       MAX COLUMN INCHES                            
         BH    EDTL9                                                            
         NI    PBDUIND,X'BF'       MAKE LOWER CASE I                            
         ZAP   PBDUNITS,DUB                                                     
         B     EDTL9B              SAVES SPACE                                  
*                                                                               
EDTLNS3C CLI   0(R4),C'.'          CK FOR DECIMAL POINT                         
         BNE   EDTL9               TREAT AS SPACE                               
         BCT   R5,*+8                                                           
         B     EDTL9               NO MORE INPUT                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'I'                                                       
         BNE   EDTLNS3F                                                         
         B     EDTLNS4             CHK FOR NN.I/XX                              
*                                                                               
EDTLNS3F CLI   0(R4),C'I'                                                       
         BE    EDTLNS3G                                                         
         CLI   0(R4),C'0'                                                       
         BL    EDTL9                                                            
         CLI   0(R4),C'9'                                                       
         BH    EDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,EDTLNS3F                                                      
         B     EDTL9               IF DOESN'T END WITH I ASSUME SPACE           
*                                                                               
EDTLNS3G LR    R3,R4               MUST DECREMENT R5                            
         L     RE,DUB              START OF WHOLE NUMBER                        
         SR    R3,RE               LENGHT OF WHOLE NUMBER                       
         XC    WORK(13),WORK       BUILD LINE FOR CASHVAL                       
*                                                                               
EDTLNS3I BCTR  R3,0                SO I WON'T MOVE THE I                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         LA    R3,1(R3)            RESTORE FROM THE EX                          
*                                                                               
         GOTO1 VCASHVAL,DMCB,(2,WORK),(R3)                                      
         LA    R3,INVERR                                                        
         CLI   DMCB,X'FF'                                                       
         BE    EDTL9               IF ERROR TREAT AS SPACE                      
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         CP    DUB,=P'99999'                                                    
         JH    ERROR               CAN'T FIT IN PBDUNITS                        
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE 'I'                        
         BCT   R5,EDTLNS4B                                                      
         MVC   PBDSPACE(8),8(R2)   NO MORE INPUT AFTER I                        
         MVI   TMPSPCE,C'Y'                                                     
         B     EDTLNS8             NO MORE INPUT                                
*                                                                               
EDTLNS4  BCT   R5,*+8              NO MORE INPUT AFTER L OR I                   
         B     EDTLNS8                                                          
EDTLNS4B CLI   1(R4),C'/'          '/'  MUST BE NEXT                            
         BNE   EDTL9                                                            
         LA    R4,1(R4)                                                         
*                                                                               
EDTLNS5  LA    R4,1(R4)                                                         
*                                                                               
EDTLNS6  BCT   R5,*+8              NO MORE INPUT                                
         B     EDTL9                                                            
         ST    R4,DUB              START OF NUMBER                              
EDTLNS6A CLI   0(R4),C'0'                                                       
         BL    EDTL9                                                            
         CLI   0(R4),C'9'                                                       
         BH    EDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,EDTLNS6A                                                      
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,0                                                             
         EX    R4,PACKLN                                                        
         CP    WORK(5),=P'999'     MAX COLUMNS IS 999                           
         JH    ERROR               CAN'T FIT IN PDBCLMS                         
         LA    R3,DVSERR           DIVISIBILITY ERROR                           
         ZAP   PBDCLMS,WORK(5)                                                  
         JZ    ERROR                                                            
         ZAP   DUB,PBDUNITS                                                     
         JZ    ERROR                                                            
         MVC   PBDSPACE(8),8(R2)                                                
         MVI   TMPSPCE,C'Y'                                                     
         CLI   PBDUIND,C'I'        INCHES DOESN'T HAVE TO DIVIDE                
         BE    EDTLNS8             EVENLY                                       
*                                                                               
         DP    DUB,PBDCLMS                                                      
         CP    DUB+8-L'PBDCLMS(L'PBDCLMS),=P'0'                                 
         JNE   ERROR                                                            
*                                  CHECK VS JOB RECORD                          
EDTLNS8  CLI   0(R2),20            SKIP IF CLE EDIT                             
         BH    EDTLNS8X                                                         
         OC    PBDJOB,PBDJOB                                                    
         BZ    EDTLNS8X                                                         
         L     R4,AJOBIO                                                        
         CLC   PBDSPACE(2),=C'* '  TEST SPACE BUY                               
         BNH   EDTLNS8M            NO                                           
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    EDTLNS8M                                                         
         CLC   PBDSPACE(2),=C'# '                                               
         BE    EDTLNS8M                                                         
*                                                                               
* SEE IF I TEMPORARILY STORED SPACE FOR JOBREC CHECK                            
*                                                                               
         CLI   TMPSPCE,C'Y'                                                     
         BNE   EDTLNS8C                                                         
         CLI   PJOBSPC-PJOBREC(R4),C' '                                         
         BH    EDTLNS8C            STILL ONLY CHK IF JOB AS SPACE               
*                                                                               
         B     EDTLNS8M                                                         
*                                                                               
EDTLNS8C MVI   X,C' '                                                           
         MVC   X+1(16),X                                                        
         OC    X(17),PBDSPACE                                                   
         CLC   X(17),PJOBSPC-PJOBREC(R4)                                        
         BNE   EDTLNS8W                                                         
         CP    PJOBTUNS-PJOBREC(3,R4),=P'0'                                     
         BE    EDTLNS8X                                                         
*                                                                               
EDTLNS8M CP    PBDUNITS,PJOBTUNS-PJOBREC(3,R4)                                  
         BNE   EDTLNS8W                                                         
         CP    PBDCLMS,PJOBCOLS-PJOBREC(3,R4)                                   
         BNE   EDTLNS8W                                                         
         CLC   PBDUIND,PJOBUIND-PJOBREC(R4)                                     
         BNE   EDTLNS8W                                                         
         BE    EDTLNS8X                                                         
*                                                                               
EDTLNS8W OI    WARN,X'80'          SPACE WARNING                                
*                                                                               
EDTLNS8X CLI   TMPSPCE,C'Y'        SEE IF SPACE ONLY TEMPORARY                  
         BNE   ELEXIT                                                           
         XC    PBDSPACE(8),PBDSPACE                                             
         CLI   8(R2),C'#'          SEE IF INPUT BEGAN WITH #                    
         BE    EDTLS8XX            SAVE IT                                      
         CLI   8(R2),C'*'          OR *                                         
         BE    EDTLS8XX            SAVE IT                                      
         B     ELEXIT                                                           
*                                                                               
EDTLS8XX MVC   PBDSPACE(1),8(R2)   SAVE # OR *                                  
         B     ELEXIT                                                           
*                                                                               
EDTL9    DS    0H                  TREAT AS SPACE DESC                          
         CLI   0(R2),20            ERROR IF EDITING CLE                         
         JH    ERROR                                                            
         CLI   5(R2),8             MAX LEN                                      
         JH    ERROR                                                            
         MVI   PBDUIND,0           SPACES SHOULDN'T HAVE                        
         ZAP   PBDUNITS,=P'0'      THESE FIELDS SET                             
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
         BRAS  RE,CKSPDESP         CHECK AGAINST SPACE DESP RECORD              
         CHI   R3,SPDSPERR         SPACE DESCRIPTION NOT FOUND?                 
         JE    ERROR                                                            
*                                                                               
* IF SPACE NOT CHANGED, CARRY OVER UNITS                                        
*                                                                               
         TM    4(R2),X'20'                                                      
         BZ    EDTL9B                                                           
         L     RF,TRADDR                                                        
         CLI   8(RF),C'C'          ONLY FOR CHANGE                              
         BNE   EDTL9B              SO CLE WILL BE SAVED                         
         ZAP   PBDUNITS,PBDUNITS-NEWREC+REC                                     
         ZAP   PBDCLMS,PBDCLMS-NEWREC+REC                                       
         MVC   PBDUIND,PBDUIND-NEWREC+REC                                       
*                                                                               
EDTL9B   OI    4(R2),X'20'                                                      
         MVC   PBDSPACE(8),8(R2)                                                
         B     EDTLNS8                                                          
*                                                                               
PACKLN   PACK  WORK(5),0(0,RE)                                                  
*                                                                               
ELEXIT   J     EXIT_X                                                           
*                                                                               
ERELO    DS    F                                                                
TMPSPCE  DS    CL10                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSPDESP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LH    R6,=Y(WKBLK_01-GENOLD)                                           
         AR    R6,RC                                                            
         USING WKBLK_01,R6                                                      
S        USING CKSPD_D,WKBLK_01                                                 
*                                                                               
         CLI   BYPROF+09,C'Y'      PROFILE ALLOWS SPACE DESP LOOK UP?           
         JNE   SCCNEQ_R            NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         CLI   TRCODE,C'C'         CHANING?                                     
         BE    CKSPD20                                                          
         CLI   TRCODE,C'B'         BUYING?                                      
         BE    CKSPD40                                                          
*                                                                               
         J     SCCNEQ_R            NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
* SEE IF SPACE DESP ENTERED IS SAME AS IN RECORD NOW                            
*                                                                               
CKSPD20  XC    S.CKSPWORK,S.CKSPWORK                                            
         MVC   S.CKSPWORK(8),8(R2)                                              
         OC    S.CKSPWORK(8),=17C' '                                            
         CLC   S.CKSPWORK(8),PBDSPACE-NEWREC+REC                                
         JE    SCCNEQ_R            NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
CKSPD40  CLI   PBDSPACE,C'*'       START WITH "*"? IF SO, NO NEED TO CK         
         JE    SCCNEQ_R            NO NEED TO LOOK UP SPC DESP RECORD           
         XC    S.CKSPWORK,S.CKSPWORK                                            
         MVC   S.CKSPWORK(8),8(R2)                                              
*                                                                               
         CLI   PBDSPACE,C'#'       START WITH "#"? IF SO, IGNORE IT             
         BNE   *+16                                                             
         XC    S.CKSPWORK,S.CKSPWORK                                            
         MVC   S.CKSPWORK(7),9(R2)                                              
*                                                                               
         MVC   S.CKSPKEY(L'KEY),KEY                                             
         MVC   S.CKSPKEY+L'KEY(L'KEYSAVE),KEYSAVE                               
*                                                                               
         XC    KEY,KEY             BUILD KEY TO READ SPC DESP RECORD            
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),BUYMD                                                 
         MVI   KEY+03,X'5A'        STANDARD SPACE DESCRIPTION REC CODE          
         MVC   KEY+04(08),S.CKSPWORK                                            
         OC    KEY+04(17),=17C' '  MAKE SURE IS SPACE PADDED                    
*                                                                               
         MVC   KEYSAVE,KEY         FOR COMPARISON LATER                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(21),KEYSAVE     INPUT MATCHED THAT OF RECORD?                
         BE    CKSPD50             YES                                          
         MVC   KEY,S.CKSPKEY                                                    
         MVC   KEYSAVE,S.CKSPKEY+L'KEY                                          
         LA    R3,SPDSPERR                                                      
         J     SCCNEQ_R                                                         
*                                                                               
CKSPD50  MVC   KEY,S.CKSPKEY                                                    
         MVC   KEYSAVE,S.CKSPKEY+L'KEY                                          
*                                                                               
         J     SCCEQ_R                                                          
*                                                                               
         DROP  S                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPREM  NTR1  BASE=*,LABEL=*      SUBROUTINE TO EDIT PREMIUM DESC              
*                                                                               
         MVI   PBDPCTYP,0                                                       
*                                                                               
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         BC    8,EPEXIT                                                         
         CLC   =C'NONE',8(R2)                                                   
         BE    EDTPR3                                                           
         LA    R6,8(R2)                                                         
         SR    R7,R7                                                            
         IC    R7,5(R2)                                                         
         CLI   9(R2),C'C'                                                       
         BNE   EDTPR2                                                           
         CLI   8(R2),C'1'                                                       
         JL    ERROR                                                            
         CLI   8(R2),C'4'                                                       
         JH    ERROR                                                            
         MVC   PBDCL,8(R2)                                                      
         MVC   PBDPRIN,PBDCOSIN    SET DEFAULT PREMIUM RATE IND                 
         CLI   5(R2),2             TEST INPUT LENGTH                            
         BE    EDTPR3                                                           
         CLI   10(R2),C'/'                                                      
         JNE   ERROR                                                            
         LA    R6,11(R2)                                                        
         AHI   R7,-3                                                            
         JZ    ERROR                                                            
*                                                                               
EDTPR2   CLI   0(R6),C'S'          TEST NON-COMM OVERRIDE                       
         BNE   EDTPR2C                                                          
         LA    R6,1(R6)            ADJUST POINTER                               
         BCTR  R7,0                AND LEN                                      
         MVI   PBDPRIN,C'S'        SET IND IN PBDELEM                           
         CLI   PBDCOSIN,C'S'       MUST BE THE SAME                             
         JNE   ERROR                                                            
         B     EDTPR2D                                                          
*                                                                               
EDTPR2C  CLC   PBUYKAGY,=C'SJ'     ONLY OR SJ                                   
         BNE   EDTPR2D                                                          
         CLI   0(R6),C'C'          TEST NON-COMM OVERRIDE                       
         BNE   EDTPR2D                                                          
         LA    R6,1(R6)            ADJUST POINTER                               
         BCTR  R7,0                AND LEN                                      
         MVI   PBDPRIN,C'C'        SET IND IN PBDELEM                           
         CLI   PBDCOSIN,C'C'       MUST BE THE SAME                             
         JNE   ERROR                                                            
*                                                                               
EDTPR2D  CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE EST                          
         BNE   EDTPR2F                                                          
         CLI   PBDPRIN,C'C'        SEE IF 'C' INPUT                             
         BE    EDTPR2G                                                          
         CLI   PBDPRIN,C' '                                                     
         JNE   ERROR                                                            
         MVI   PBDPRIN,C'C'        SET TO 'C'                                   
         B     EDTPR2G                                                          
*                                                                               
EDTPR2F  CLI   PBDPRIN,C'C'        NO 'C' PREMIUM FOR NON 'C' RATE EST          
         JE    ERROR                                                            
*                                                                               
EDTPR2G  CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   EDTPR2H                                                          
         CLI   PBDPRIN,C' '        CAN'T BE ENTERED IF PBDPRIN PRESENT          
         JH    ERROR                                                            
         MVI   PBDPCTYP,C'N'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,0                                                             
         B     EDTPR2                                                           
*                                                                               
EDTPR2H  GOTO1 VCASHVAL,DMCB,(R6),(R7)                                          
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         ZAP   PBDPRCOS,=P'1'      SET TO .01 IF FREE                           
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BZ    EDTPR3                                                           
         CVD   RE,DUB                                                           
         ZAP   PBDPRCOS,DUB                                                     
*                                                                               
EDTPR3   OC    PBDJOB,PBDJOB                                                    
         BZ    EPEXIT                                                           
         L     R4,AJOBIO                                                        
         MVC   DUB(1),PBDCL                                                     
         MVC   DUB+1(1),PJOBPRM-PJOBREC(R4)                                     
         OC    DUB(2),=C'  '       OR BOTH WITH SPACES                          
         CLC   DUB(1),DUB+1        AND THEN COMPARE                             
         BE    EPEXIT                                                           
         OI    WARN,X'80'          SPACE WARNING                                
*                                                                               
EPEXIT   J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTC     NTR1  BASE=*,LABEL=*      EDIT COMMENT LINES                           
*                                                                               
         ST    RF,CRELO                                                         
*                                                                               
         BRAS  RE,BUMPFLD          NEXT FIELD                                   
*                                                                               
         XC    UPID,UPID                                                        
         MVI   UPLSW,0                                                          
         XC    UPCURSOR,UPCURSOR                                                
*                                                                               
         MVI   COPYSW,0                                                         
         ZAP   CAPCNT,=P'0'                                                     
*                                                                               
         BRAS  RE,CKADBYER         ADBUYER - PROCESS WORKER ELEM                
         BE    NEXTD1                                                           
*                                                                               
         CLI   TRCODE,C'B'         SEE IF BUYING                                
         BNE   EDTC0                                                            
         XC    PRVTAB,PRVTAB                                                    
         B     EDTC1B              YES - UNVALIDATE ALL COMMENT LINES           
*                                                                               
EDTC0    LA    R9,5                                                             
         LA    R7,PRVTAB           TABLE OF PREVIOUSLY VALIDATED COMM           
         LR    R4,R2                                                            
         SR    RE,RE               GET PAST PROTECTED FIELD                     
         IC    RE,0(R4)                                                         
         AR    R4,RE                                                            
EDTC1    CLI   0(R7),X'20'                                                      
         BNE   EDTC1A                                                           
         TM    4(R4),X'20'                                                      
         BO    NXTFLD                                                           
*                                  SEE IF COMMENT WAS OVERWRITTEN WITH          
         B     EDTC1A1                                                          
*                                                                               
* A SPECIAL CODE (AC=,ETC.)                                                     
* IF SO DON'T DELETE IT UNLESS THERE WAS OTHER COMMENT ACTIVITY                 
*                                                                               
EDTC1A   CLI   5(R4),0                                                          
         BE    NXTFLD                                                           
         CLC   8(3,R4),=C'IC='     NEW IC= COMMENT  - SO UNVALIDATE             
         BE    EDTC1B              ALL OTHERS SO THEY WON'T BE LOST             
*                                                                               
         CLC   8(3,R4),=C'PI='     NEW PI= COMMENT  - SO UNVALIDATE             
         BE    EDTC1B              ALL OTHERS SO THEY WON'T BE LOST             
*                                                                               
         CLC   8(4,R4),=C'SRC='    NEW SRC= COMMENT  - SO UNVALIDATE            
         BE    EDTC1B              ALL OTHERS SO THEY WON'T BE LOST             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTC1A1  DS    0H                                                               
         ST    R4,FULL             CURRENT FLD POINTER                          
         BRAS  RE,CKKEYW                                                        
         BE    NXTFLD                                                           
*                                                                               
* UNVALIDATE ALL COMMENT LINES                                                  
*                                                                               
EDTC1B   LA    R9,5                                                             
         LR    R4,R2                                                            
         SR    RE,RE               GET PAST PROTECTED FIELD                     
         IC    RE,0(R4)                                                         
         AR    R4,RE                                                            
EDTC2    NI    4(R4),X'DF'                                                      
         LA    R4,COMLEN(R4)                                                    
         BCT   R9,EDTC2                                                         
         B     EDTC3                                                            
*                                                                               
NXTFLD   LA    R7,1(R7)                                                         
         LA    R4,COMLEN(R4)                                                    
         BCT   R9,EDTC1                                                         
         B     EDTC3                                                            
*                                                                               
* ALL COMMENTS DISLPAYED WERE NOT CHANGED AND NO NEW ONES WERE ADDED            
*                                                                               
EDTC3    DS    0H                  INITALIZE COMMENT TABLE                      
         L     R7,ACOMWRK                                                       
         LA    R9,10                                                            
EDTC3A   MVI   0(R7),X'FF'                                                      
         LA    R7,60(R7)                                                        
         BCT   R9,EDTC3A                                                        
         L     R7,ACOMWRK                                                       
         LA    R6,PRVTAB                                                        
         LA    R9,5                FOR BCT                                      
EDTCOM1  BRAS  RE,BUMPFLD                                                       
         TM    4(R2),X'20'                                                      
         BO    NEXTD               NEXT COMMENT LINE                            
*                                                                               
         CLI   5(R2),0             INPUT THIS TIME?                             
         BNE   EDTCOM1A                                                         
         CLI   0(R6),X'20'         THERE IS A COMMENT?                          
         BNE   NEXTD                                                            
         CLI   MADSW,C'Y'          PBU? DON'T DELETE COMMENTS FOR PBU           
         JE    *+10                                                             
         MVC   0(2,R7),=X'6602'    SO OLD COMMENT WILL BE DELETED               
         B     NEXTD                                                            
*                                                                               
EDTCOM1A DS    0H                                                               
         ST    R2,FULL             CURRENT FLD POINTER                          
         BRAS  RE,CKKEYW                                                        
         BNE   EDTCM1                                                           
         L     RF,FULL             FULL HAS ADDRESS OF ROUTINE                  
         A     RF,CRELO                                                         
         BR    RF                                                               
*                                                                               
EDTCM1   CLC   =C'COM=',8(R2)      COM= ONLY FOR IC=                            
         BE    FLDINV                                                           
         CLC   =C'IB=COM=',8(R2)   NO COM= FOR IB=                              
         BE    FLDINV                                                           
*                                                                               
* COMMENT ENTERED - SAVE IT IN COMWRK (NEW 03/25/88)                            
*                                                                               
         CLC   =C'COPY=',8(R2)     SPECIAL COMMENTS                             
         BNE   EDTCM2                                                           
         CLI   COPYSW,1            SEE IF COPY ALREADY ENTERED                  
         BE    FLDINV                                                           
         CLI   5(R2),22            COPY MAX OF 22 (17+5)                        
         BH    FLDINV                                                           
         OI    COPYSW,1                                                         
         B     EDTCMX                                                           
*                                                                               
EDTCM2   CLC   =C'CAP=',8(R2)      SPECIAL COMMENTS                             
         BNE   EDTCMX                                                           
         CP    CAPCNT,=P'1'                                                     
         BH    FLDINV              MAX 2 CAP= COMMENTS                          
         CLI   5(R2),29            CAP MAX OF 29 (25+4)                         
         BH    FLDINV                                                           
         AP    CAPCNT,=P'1'                                                     
         B     EDTCMX                                                           
*                                                                               
EDTCMX   DS    0H                                                               
         SR    RE,RE                                                            
         MVI   0(R7),X'66'                                                      
         IC    RE,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLC   =C'IC=',8(R2)                                                    
         BE    EDTCOM1B                                                         
         CLC   =C'IB=',8(R2)                                                    
         BE    EDTCOM1B                                                         
*                                                                               
         CLC   =C'SRC=',8(R2)      SRC COMMENTS                                 
         BE    EDTCOMAA                                                         
**PI                                                                            
         CLC   =C'PI=',8(R2)                                                    
         BNE   EDTCOM2                                                          
         MVI   0(R7),X'68'                                                      
         B     EDTCOM1C                                                         
*                                                                               
EDTCOMAA DS    0H                                                               
         ZIC   R1,5(R2)            40 IS MAX THAT ALLOWED FOR SRC               
         CHI   R1,44               PLUS 4 CHAR "SRC="                           
         BH    FLDINV                                                           
*                                                                               
         MVI   0(R7),X'6A'         SRC ELEMENT                                  
         AHI   RE,-4                                                            
         LA    R4,12(R2)           12= 8 HEADER + 4 "SRC="                      
         B     EDTCOM2                                                          
*                                                                               
EDTCOM1B MVI   0(R7),X'67'                                                      
EDTCOM1C AHI   RE,-3                                                            
*                                                                               
         LA    R4,11(R2)                                                        
         CLC   0(4,R4),=C'COM='                                                 
         BNE   EDTCOM2                                                          
*                                  STANDARD COMMENT                             
         CHI   RE,10               1-6 CHARS                                    
         BH    FLDINV                                                           
         CHI   RE,5                                                             
         BL    FLDINV                                                           
         ST    RE,FULL             SAVE RE IN FULL  NEEDED IN EDTCOM2           
*                                                                               
         AHI   RE,-4                                                            
         MVC   WORK(6),=6C' '                                                   
         LA    R5,WORK+6                                                        
         SR    R5,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),4(R4)       RIGHT ALIGN IN WORK                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BRAS  RE,HIGH                                                          
         L     RE,FULL             RESTORE RE FOR EDTCOM2                       
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDTCOM2             FOUND - OK                                   
*                                                                               
         LA    R3,NFNDERR                                                       
         J     ERROR                                                            
*                                                                               
EDTCOM2  DS    0H                                                               
         LA    RF,2(RE)                                                         
         STC   RF,1(R7)                                                         
         LTR   RE,RE                                                            
         BNZ   EDTCOM2D                                                         
         CLC   =C'IB=',8(R2)                                                    
         BNE   NEXTD                                                            
         MVC   60(2,R7),=X'6602'   SO IB= WILL DELETE BOTH 67 AND 66            
EDTCOM2B LA    R7,60(R7)                                                        
         B     NEXTD                                                            
*                                                                               
EDTCOM2D BCTR  RE,0                                                             
         EX    RE,MVCOM                                                         
         CLC   =C'IB=',8(R2)         IB=  SO GENERATE BOTH 66 AND 67            
         BNE   EDTCOM3                                                          
         LA    R3,2         FOR BCT                                             
         MVI   60(R7),X'66'                                                     
         STC   RF,61(R7)        STORE LENGHT                                    
         EX    RE,MVCOM2                                                        
EDTCOM3  OC    INSDA,INSDA         TEST ADD                                     
         BC    7,EDTCOM2B      NEXT COMMENT LINE                                
EDTCOM4  MVI   ELCODE,X'FF'      TO GET TO END OF REC                           
         LA    R5,NEWREC+33                                                     
         BRAS  RE,NXTELEM                                                       
         GOTO1 VRECUP,DMCB,(1,NEWREC),0(R7),(R5)                                
         CLC   =C'IB=',8(R2)                                                    
         BNE   NEXTD                                                            
         LA    R7,60(R7)        TO GET PAST 67 ELEMENT                          
         BCT   R3,EDTCOM4            TO ADD THE OTHER ELEMENT                   
         B     NEXTD+4           +4 SO I WON'T BUMP R7 TWICE                    
*                                                                               
*                                                                               
NEXTD    LA    R7,60(R7)                                                        
         LA    R6,1(R6)                                                         
         BCT   R9,EDTCOM1                                                       
*                                                                               
* NOW SEE IF ESTHDR HAD REP WAS SAVED IN SVESPROF+15                            
*                                                                               
NEXTD1   CLI   SVESPROF+15,C' '   NO EST REP                                    
         BE    NEXTD5                                                           
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'        CHK NEWREC FOR SREP ELEM                     
         BRAS  RE,NXTELEM                                                       
         BE    NEXTD5              FOUND -  SO I'M DONE                         
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BNE   NEXTD5              NO - DON'T ADD EST SREP ELEM                 
*                                                                               
         XC    WORK(10),WORK       NOT FOUND -  MUST ADD ELEM                   
         MVC   WORK(2),=X'800A'                                                 
         MVC   WORK+2(4),SVESPROF+15                                            
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
*                                                                               
NEXTD5   BRAS  RE,PRCC2FAC         PROCESS COS2 FACTOR                          
*                                                                               
NEXTDX   J     EXIT_X                                                           
*                                                                               
MVCOM    MVC   2(0,R7),0(R4)                                                    
MVCOM2   MVC   62(0,R7),0(R4)                                                   
*                                                                               
* EDIT PLANNED COST                                                             
*                                                                               
EDTPLCOS DS    0H                                                               
         MVI   PBDPLCOS,X'FF'      SET PC=NONE                                  
         CLC   =C'NONE',11(R2)                                                  
         BE    EDTPLC4                                                          
*                                                                               
         LA    R3,INVERR                                                        
         SR    RF,RF                                                            
         IC    RF,5(R2)            LENGTH OF FIELD                              
         SHI   RF,3                SUBTRACT PREFIX LENGTH                       
         JNP   ERROR                                                            
         GOTO1 VCASHVAL,DMCB,(2,11(R2)),(RF)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         ICM   RF,15,4(R1)                                                      
         JZ    ERROR                                                            
         STCM  RF,15,PBDPLCOS                                                   
*                                                                               
EDTPLC4  DS    0H                                                               
         MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         B     NEXTD               NEXT COMMENT LINE                            
         EJECT                                                                  
*                                                                               
* EDIT TAX OVERRIDE                                                             
*                                                                               
EDTTAX   DS    0H                                                               
         OC    PBDTAX,PBDTAX                                                    
         JNZ   ERROR                                                            
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
*                                                                               
         LA    R3,INVERR                                                        
         MVI   PBDTAX+2,X'01'    SET TAX=NONE                                   
         CLC   =C'NONE',12(R2)                                                  
         BE    EDTTAX4                                                          
*                                                                               
         ZIC   RF,5(R2)            LENGTH OF FIELD                              
         SHI   RF,4                SUBTRACT PREFIX LENGTH                       
         JNP   ERROR                                                            
         GOTOR VCASHVAL,DMCB,(4,12(R2)),(RF)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         MVI   PBDTAX+2,X'01'                                                   
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    EDTTAX4                                                          
         JM    ERROR                                                            
         C     RF,=F'1000000'    SHOULD NOT EXCEED 100 PCT                      
         JH    ERROR                                                            
         MVC   PBDTAX,5(R1)                                                     
*                                                                               
EDTTAX4  DS    0H                                                               
**NEW 3/30/89                                                                   
         CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE ESTIMATE                     
         BNE   EDTTAX6                                                          
         CLC   PBDTAX,=X'000001'   TAX MUST BE ZERO                             
         JNE   ERROR                                                            
EDTTAX6  DS    0H                                                               
**NEW 3/30/89                                                                   
         MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         B     NEXTD               NEXT COMMENT LINE                            
         EJECT                                                                  
*                           REFERENCE NUMBER                                    
*                           GOES INTO PBREFEL                                   
EDTREF   DS    0H                                                               
*                                                                               
EDTREF2  LA    R4,8+4(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(10),WORK                                                    
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4               ADJUST INPUT LENGTH (FOR REF=)               
         BP    EDTREF4                                                          
*                                                                               
         MVI   WORK,X'FF'         SPECIAL DELETE CODE                           
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         JE    ERROR                                                            
         B     EDTREF10                                                         
*                                                                               
EDTREF4  CLC   AGYALPHA,=C'BS'                                                  
         BNE   EDTREF6                                                          
         CHI   R5,6                MAX IS CHARACTERS FOR BACKER                 
         JH    ERROR                                                            
*                                                                               
EDTREF6  CHI   R5,10               SJR + OTHERS - MAX IS 10 CHARS               
         JH    ERROR                                                            
*                                                                               
         MVC   WORK(10),0(R4)                                                   
*                                                                               
EDTREF10 DS    0H                  NOW STORE IN PBREFEL                         
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'83'                                                     
         ST    R9,FULL                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTREF20             MUST ADD ONE                                
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE REF ELEM                  
*                                                                               
*                                                                               
EDTREF20 L     R9,FULL             MUST RESTORE R9                              
         XC    WORK+15(15),WORK+15                                              
         MVC   WORK+15(2),=X'830F'    CODE AND LENGHT                           
         MVC   WORK+17(10),WORK       REF NUMBER OR X'FF'                       
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTREF30 B     NEXTD                                                            
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
EDTPST   DS    0H                 EDIT PST OVERRIDE                             
*                                 FORMAT IS PST/PP=X                            
*                                 PP = PROVINCE CODE                            
*                                 X=PST CODE                                    
         LA    R3,INVERR                                                        
         CLI   NATION,C'C'        SEE IF CANADIAN                               
         JNE   ERROR                                                            
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
*                                                                               
         LA    R3,INVERR                                                        
         LA    R4,8+4(R2)         POINT TO INPUT                                
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4                                                            
         JNP   ERROR                                                            
*                                                                               
         CHI   R5,4               LENGTH AFTER PST/                             
         JH    ERROR              ONLY ONE PROVINCE CAN BE ENTERED              
*                                                                               
         XC    WORK,WORK          BUILD FIELD HEADER FOR PSTVAL                 
         MVI   WORK,48                                                          
         STC   R5,WORK+5                                                        
         AHI   R5,-1                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(R4)                                                  
*                                                                               
         XC    X(PSTLNQ),X                                                      
         LA    R5,X                                                             
         USING PSTBLKD,R5                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    RE,WORK                                                          
         ST    RE,PSTADIN          INPUT ADDRESS                                
         XC    X+50(20),X+50                                                    
         LA    RE,X+50                                                          
         ST    RE,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R5)      VALIDATE PST                                 
         CLI   PSTERR,0                                                         
         JNE   ERROR                                                            
         DROP  R5                                                               
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'84'                                                     
         ST    R9,FULL             SAVE R9                                      
         BRAS  RE,NXTELEM                                                       
         BNE   EDTPST20                                                         
*                                                                               
         L     R9,FULL             RESTORE R9                                   
         MVC   2(10,R5),X+50       ELEM EXISTS - JUST MOVE IN NEW DATA          
         B     EDTPST30                                                         
*                                                                               
EDTPST20 DS    0H                                                               
         L     R9,FULL             RESTORE R9                                   
         XC    X(25),X                                                          
         MVC   X(2),=X'840C'                                                    
         MVC   X+2(10),X+50                                                     
         GOTO1 VRECUP,DMCB,(1,NEWREC),X,0(R5)                                   
*                                                                               
EDTPST30 B     NEXTD                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTGST   DS    0H                  EDIT GST OVERRIDE                            
         LA    R3,INVERR                                                        
         CLI   NATION,C'C'         SEE IF CANADIAN AGY                          
         JNE   ERROR                                                            
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
*                                                                               
         LA    R3,INVERR                                                        
         LA    R4,8+4(R2)         POINT TO INPUT                                
         ZIC   R5,5(R2)                                                         
         CHI   R5,5                                                             
         JNE   ERROR                                                            
         LA    R5,GSTTAB                                                        
EDTG2    CLC   0(1,R4),0(R5)                                                    
         BE    EDTG5                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         JE    ERROR                                                            
         B     EDTG2                                                            
*                                                                               
EDTG5    MVC   PBDGST,0(R4)                                                     
*                                                                               
EDTGSTX  B     NEXTD                                                            
*                                                                               
GSTTAB   DC    C'SXZT',X'FF'    TABLE OF VALID GST CODES                        
         EJECT                                                                  
EDTSFH   DS    0H                EDIT SPECIAL FINANCIAL HANDLING                
*                                                                               
         LA    R3,INVERR                                                        
*                                                                               
         LA    R4,8+4(R2)         POINT TO INPUT                                
         ZIC   R5,5(R2)                                                         
         CHI   R5,5                                                             
         JL    ERROR                                                            
         CLC   0(4,R4),=C'HOLD'                                                 
         BE    EDTSFHH                                                          
         CLC   0(4,R4),=C'HELD'                                                 
         BE    EDTSFHH                                                          
         CLI   0(R4),C'H'                                                       
         BE    EDTSFHH                                                          
         CLC   0(3,R4),=C'REL'                                                  
         BE    EDTSFHR                                                          
         CLI   0(R4),C'R'                                                       
         BE    EDTSFHR                                                          
         J     ERROR                                                            
*                                                                               
EDTSFHH  DS    0H                                                               
         OI    PBDSTAT,X'0C'      SET ON SFH AND HOLD BITS                      
         L     R1,TRADDR         SEE IF BUYING                                  
         CLI   8(R1),C'B'                                                       
         BE    EDTSFHX                                                          
*                                ON CHANGES LOOK FOR I/O ELEMEMT                
*                                WITH A DATE                                    
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'                                                     
         ST    R9,FULL                                                          
EDTSFHH1 BRAS  RE,NXTELEM                                                       
         BNE   EDTSFHHX                                                         
         OC    2(3,R5),2(R5)      CHECK FOR DATE                                
         BZ    EDTSFHH1                                                         
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(34),=C'CAN''T HOLD- BUY HAS BEEN ON AN I/O'               
         MVI   ERRAREA,X'FF'                                                    
         MVC   TRCODE,=C'RI'      SET FOR I/O RECALL                            
         B     EXIT                                                             
*                                                                               
EDTSFHHX L     R9,FULL            MUST RESTORE R9                               
         B     EDTSFHX                                                          
*                                                                               
EDTSFHR  DS    0H                                                               
         OI    PBDSTAT,X'04'      SFH BIT                                       
         NI    PBDSTAT,X'F7'     SET OFF X'08' HOLD BIT                         
*                                 IF NEW BUY COULD ALREADY BE ON                
EDTSFHX  B     NEXTD                                                            
         EJECT                                                                  
*        ROUTINE TO EDIT UPLOAD ID - SAVE IT IN UPID                            
*                                                                               
EDTUPID  DS    0H                                                               
         LA    R3,INVERR                                                        
         CLI   MADSW,C'Y'      CAN'T HAVE UPID= DATA                            
         JE    ERROR           FROM A MAD UPLOAD                                
*                                                                               
         OC    UPID,UPID       CAN'T ALREADY HAVE                               
         JNZ   ERROR                                                            
         CLI   5(R2),5                                                          
         JNH   ERROR                                                            
         CLI   5(R2),13       MAX IS UPID=XXXXXXXX                              
         JH    ERROR                                                            
         LLC   R1,5(R2)                                                         
         AHI   R1,-(5+1)          5 FOR 'UPID=' AND 1 FOR EX                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   UPID(0),13(R2)                                                   
         OC    UPID,=CL8' '                                                     
         MVI   UPLSW,C'Y'                                                       
         ST    R2,FULL                                                          
         MVC   UPCURSOR,FULL   SAVE ADDRESS OF OPTIONAL DATA LINE               
         B     NEXTD                                                            
         EJECT                                                                  
*        EDIT CONTRACT UNITS                                                    
EDTCU    DS    0H                                                               
         OC    PBDCU,PBDCU                                                      
         JNZ   ERROR                                                            
         MVI   PBDCU+2,X'01'     SET CU=NONE                                    
         CLC   =C'NONE',11(R2)                                                  
         BE    EDTCU4                                                           
*                                                                               
         LA    R3,INVERR                                                        
         ZIC   RF,5(R2)            LENGTH OF FIELD                              
         SHI   RF,3                SUBTRACT PREFIX LENGTH                       
         JNP   ERROR                                                            
         GOTO1 VCASHVAL,DMCB,(4,11(R2)),(RF)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         MVI   PBDCU+2,X'01'      .0001 = ZERO UNITS                            
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    EDTCU4                                                           
         JM    ERROR                                                            
         CLI   PBDSPACE,C'*'   * BUYS CAN NOT HAVE CU=                          
         JE    ERROR                                                            
         C     RF,=F'1'       CANNOT INPUT .0001 SINCE 0 IS CARRIED             
         JE    ERROR          AS X'000001'                                      
         C     RF,=F'16777215' CANNOT EXCEED 1,677.7215  MAX 3 BYTES            
         JH    ERROR                                                            
         MVC   PBDCU,5(R1)                                                      
*                                                                               
EDTCU4   DS    0H                                                               
         MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         B     NEXTD               NEXT COMMENT LINE                            
*                                                                               
* EDIT CASH DISCOUNT OVERRIDE. FORMAT IS CD=1.5                                 
*                                                                               
EDTCCD   DS    0H                                                               
         CP    PBDCD,=P'0'                                                      
         BNE   FLDINV                                                           
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
         LHI   R4,10000                                                         
         LA    R5,PBDCD                                                         
         LA    R6,(L'PBDCD-1)*16   HIGH NIBBLE                                  
*                                                                               
         MVI   BYTE3,1             CASH DISCOUNT HAS ONE DECIMAL                
         B     EDTPCT                                                           
*                                                                               
EDTCAC   DS    0H                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
         CLI   PBDCOSIN,C'S'                                                    
         BNE   *+12                                                             
         LA    R3,ACERR                                                         
         J     ERROR                                                            
*                                                                               
         CP    PBDACP,=P'0'        TEST ALREADY INUT                            
         BNE   FLDINV                                                           
         CLC   11(3,R2),=C'100'    TEST INPUT OF 100                            
         BNE   EDTCAC4                                                          
         ZAP   PBDACP,=P'-1'       SET 100 PCT TO -1 PCT                        
         B     EDTPCT4                                                          
*                                                                               
EDTCAC4  DS    0H                                                               
         LA    R4,100                                                           
         LA    R5,PBDACP                                                        
         LA    R6,(L'PBDACP-1)*16  HIGH NIBBLE                                  
         MVI   BYTE3,3             AGY COMM HAS THREE DECIMALS                  
*                                                                               
EDTPCT   DS    0H                                                               
         LA    R3,INVERR                                                        
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         AHI   R0,-3                                                            
*                                                                               
         GOTOR VCASHVAL,DMCB,(BYTE3,11(R2)),(X'40',(R0))                        
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
*                                                                               
         GOTOR VCASHVAL,DMCB,(5,11(R2)),(R0)                                    
*                                                                               
         L     R1,4(R1)                                                         
         LTR   R1,R1                                                            
         JM    ERROR                                                            
         SR    R0,R0                                                            
         DR    R0,R4                                                            
         CVD   R1,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNE   *+10                                                             
         ZAP   DUB,=P'1'                                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         ZAP   0(0,R5),DUB                                                      
         EX    R6,*+8                                                           
         B     *+10                                                             
         CP    0(0,R5),DUB          TEST INPUT NUMBER TOO LARGE                 
         JNE   ERROR                                                            
*                                                                               
EDTPCT4  DS    0H                                                               
         MVC   TRCODE,=C'RZ'                                                    
         B     NEXTD         NEXT COMMENT LINE                                  
*                                                                               
FUNCERR  LA    R3,FACCERR            FUNCTION LIMIT ACCESS                      
         J     ERROR                                                            
*                                                                               
FLDINV   LA    R3,INVERR                                                        
         J     ERROR                                                            
*                                                                               
EDTCLE   DS    0H                                                               
         CLI   BUYMD,C'N'                                                       
         BNE   EDTCLE2                                                          
         CLC   PBDSPACE(2),=C'* '                                               
         BNH   EDTCLE2                                                          
         CLC   PBDSPACE(2),=X'7B00'    # AND BINARY ZERO                        
         BE    EDTCLE2             TREAT AS NONE-SPACE BUY                      
         CLC   PBDSPACE(2),=C'# '      # AND SPACE                              
         BE    EDTCLE2                 TREAT AS NON-SPACE BUY                   
*                                                                               
         LA    R3,CLERQERR                                                      
*                                                                               
         LA    R4,8+4(R2)          POINT TO INPUT                               
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4               ADJUST INPUT LENGTH                          
         JNP   ERROR                                                            
         ST    R9,FULL             SAVE R9                                      
         L     RF,CRELO                                                         
         BRAS  RE,CSTLNS1                                                       
         L     R9,FULL                                                          
         CP    PBDUNITS,=P'0'                                                   
         BNE   *+10                                                             
         ZAP   PBDUNITS,=P'-1'     SET 0 TO -1 TO PREVENT LOOKUP                
         B     NEXTD                                                            
*                                                                               
EDTCLE2  DS    0H                                                               
         LA    R3,CLEERR                                                        
         J     ERROR                                                            
         EJECT                                                                  
*                                                                               
EDTFSI   DS    0H                                                               
*                                                                               
EDTFSI2  LA    R4,8+5(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4               ADJUST INPUT LENGTH (FOR FSI=)               
         BP    EDTFSI4                                                          
*                                                                               
         MVI   DUB+1,C'X'         SPECIAL DELETE CODE                           
         CLI   TRCODE,C'B'                                                      
         BE    EDTFSI12            CANNOT HAVE BLANK ON NEW BUYS                
         B     EDTFSI10                                                         
*                                                                               
EDTFSI4  CHI   R5,8                MAX IS 9,999,999                             
         JH    ERROR                                                            
*                                                                               
         ZAP   DUB(6),=P'0'                                                     
*                                                                               
         CHI   R5,1                                                             
         BNE   EDTFSI6                                                          
         CLI   12(R2),C'N'                                                      
         BE    EDTFSI10                                                         
         CLI   12(R2),C'Y'                                                      
         BNE   EDTFSI6                                                          
EDTFSI5  MVI   DUB+1,C'Y'          SET FOR LOOK-UP                              
         B     EDTFSI10                                                         
*                                                                               
EDTFSI6  DS    0H                                                               
         CLC   12(2,R2),=C'NO'                                                  
         BE    EDTFSI10                                                         
         CLC   12(3,R2),=C'YES'                                                 
         BE    EDTFSI5             SET FOR LOOK-UP                              
*                                                                               
         LA    R3,INVERR                                                        
         ZIC   R0,5(R2)            LENGTH OF FIELD                              
         AHI   R0,-4               ADJUST FOR FSI=                              
*                                                                               
         GOTO1 VCASHVAL,DMCB,(2,12(R2)),(R0)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         JL    ERROR                                                            
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'                                                   
         JNE   ERROR              MUST GET REMAINDER 0                          
*                                                                               
EDTFSI10 DS    0H                  NOW STORE IN PBDECEL                         
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'82'                                                     
         ST    R9,FULL                                                          
         BRAS  RE,NXTELEM                                                       
         BNE   EDTFSI20             MUST ADD ONE                                
*                                                                               
EDTFSI12 LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE FSI ELEM                  
*                                                                               
*                                                                               
EDTFSI20 L     R9,FULL             MUST RESTORE R9                              
         LA    R4,WORK+10                                                       
         USING PBFSIELD,R4                                                      
         XC    PBFSIEL(20),PBFSIEL                                              
         MVC   PBFSIEL(2),=X'820D'    CODE AND LENGHT                           
         MVI   PBFSIIND,X'02'      OVERRIDDEN                                   
         CLI   DUB+1,C'Y'                                                       
         BNE   *+8                                                              
         MVI   PBFSIIND,X'01'      LOOKED-UP                                    
*                                                                               
         MVC   PBFSILDT(3),BTODAY                                               
         MVC   PBFSI(5),DUB+1      FSI DATA OR 'Y' (OR 'X' TO DELETE)           
         MVC   PBFSIPID,SVPID      PERSONAL ID                                  
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
EDTFSI30 B     NEXTD                                                            
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
EDTMANIO DS    0H                                                               
         ST    R9,FULL                                                          
         L     RF,CRELO                                                         
         BRAS  RE,EDTMAN                                                        
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTCSHDT DS    0H                                                               
         ST    R9,FULL                                                          
         L     RF,CRELO                                                         
         BRAS  RE,EDTSHIP                                                       
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTORATE DS    0H                                                               
         ST    R9,FULL                                                          
         L     RF,CRELO                                                         
         BRAS  RE,EDTORAT                                                       
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
         EJECT                                                                  
EDTSREP  DS    0H                                                               
         ST    R9,FULL                                                          
         L     RF,CRELO                                                         
         BRAS  RE,EDTSR                                                         
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTLW    DS    0H                  LEGAL WARNINGS                               
         MVI   WORK,X'94'          PASS ELEMENT CODE                            
         ST    R9,FULL                                                          
         BRAS  RE,CHKLW                                                         
         JNE   ERROR               MSG AND CURSOR IS SET IN CHKLW               
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTTRAFF DS    0H                  NO TRAFFIC                                   
         ST    R9,FULL                                                          
         BRAS  RE,CHKNTRA                                                       
         JNE   ERROR               MSG AND CURSOR IS SET IN CHKNTRA             
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTPVWS  DS    0H                                                               
         MVI   WORK,X'87'          PASS ELEMENT CODE                            
         B     EDTIM50             SAME AS IMPRESSION                           
*                                                                               
EDTCTUS  DS    0H                                                               
         MVI   WORK,X'88'          PASS ELEMENT CODE                            
         B     EDTIM50             SAME AS IMPRESSION                           
*                                                                               
EDTEXD   DS    0H                                                               
         ST    R9,FULL                                                          
         LA    R3,EXDERR2          EXDAYS/EXDATE ERROR                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'96'        IF DOING EXDAYS, CANNOT HAVE EXDATE          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR                                                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'96'        IF DOING EXDAYS, CANNOT HAVE EXDATE          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR                                                            
         MVI   WORK,X'89'          PASS EXTENSION DAYS ELEM CODE                
         L     R9,FULL                                                          
         B     EDTIM50             SAME AS IMPRESSION                           
*                                                                               
EDTAIMP  DS    0H                                                               
         MVI   WORK,X'93'          PASS ELEMENT CODE                            
         B     EDTIM50             SAME AS IMPRESSION                           
*                                                                               
EDTIMP   DS    0H                                                               
         MVI   WORK,X'92'          PASS ELEMENT CODE                            
EDTIM50  ST    R9,FULL                                                          
         L     RF,CRELO                                                         
         BRAS  RE,EDTPACK                                                       
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTEXDT  DS    0H                                                               
         ST    R9,FULL                                                          
         LA    R3,EXDERR2          EXDAYS/EXDATE ERROR                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'89'        IF DOING EXDATE, CANNOT HAVE EXDAYS          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR               NO GOOD, EXDAYS ELEM EXIST                   
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'89'        IF DOING EXDATE, CANNOT HAVE EXDAYS          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR               NO GOOD, EXDAYS ELEM EXIST                   
         L     R9,FULL                                                          
         MVI   BYTE2,X'96'         FLAG FOR DOING EXTENSION DATE                
         ST    R9,FULL                                                          
         L     RF,CRELO                                                         
         BRAS  RE,EDTSHIP                                                       
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
         EJECT                                                                  
EDTCPDDT LA    R4,PBDPDATE         PAYABLE DATE FORMAT IS MMMDD/YY              
         OC    PBDPDATE,PBDPDATE                                                
         BNZ   FLDINV                                                           
         LA    R5,11(R2)                                                        
         MVI   BYTE,0                                                           
         B     EDTCDT                                                           
*                                                                               
EDTCBLDT LA    R4,PBDBDATE         BILLABLE DATE FORMAT IS MMMDD/YY             
         OC    PBDBDATE,PBDBDATE                                                
         BNZ   FLDINV                                                           
*                                                                               
         CLI   MADSW,C'Y'          MAD UPLOAD?                                  
         JE    *+12                                                             
         TM    T411FFD+12,X'80'    CHK BD= LIMIT ACCESS                         
         BO    FUNCERR                                                          
*                                                                               
         LA    R5,11(R2)                                                        
         MVI   BYTE,0                                                           
         B     EDTCDT                                                           
*                                                                               
*                                                                               
*                                                                               
EDTCLDT  LA    R4,PBDCDATE         CLOSING DATE FORMAT IS MMMDD/YY              
         OC    PBDCDATE,PBDCDATE                                                
         BNZ   FLDINV                                                           
         MVI   PBDCDATE,X'FF'      SET FOR 'NONE'                               
         CLC   12(4,R2),=C'NONE'                                                
         BE    NEXTD                                                            
         LA    R5,12(R2)           OVERHEAD FOR CLD=                            
         MVI   BYTE,0                                                           
         B     EDTCDT                                                           
*                                                                               
EDTOSDT  LA    R4,PBDSDATE         ON SALE DATE FORMAT IS MMMDD/YY              
         OC    PBDSDATE,PBDSDATE                                                
         BNZ   FLDINV                                                           
         MVI   PBDSDATE,X'FF'      SET FOR 'NONE'                               
         CLC   12(4,R2),=C'NONE'                                                
         BE    NEXTD                                                            
         LA    R5,12(R2)           OVERHEAD FOR OSD=                            
         MVI   BYTE,0                                                           
         B     EDTCDT                                                           
*                                                                               
*                                                                               
*                                                                               
EDTCIODT LA    R4,PBDIODAT                                                      
         OC    PBDIODAT,PBDIODAT                                                
         BNZ   FLDINV                                                           
         MVC   PBDIODAT,=3X'FF'    IOD = 'NONE'                                 
         CLC   11(4,R2),=C'NONE'                                                
         BE    NEXTD               NEXT COMMENT LINE                            
         LA    R5,11(R2)                                                        
         MVI   BYTE,0                                                           
*                                                                               
EDTCDT   DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(BYTE,00(R5)),WORK                                  
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR                                                            
*                                                                               
* PUT DATE IN RECORD                                                            
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK),(3,(R4))                                   
*                                                                               
         CLC   WORK(2),=C'00'      TEST HAVE YEAR                               
         BNE   EDTCDT4                                                          
         MVC   0(1,R4),PBUYKDAT    SET YEAR FROM INSERTION DATE                 
         CLC   0(3,R4),PBUYKDAT                                                 
         BNL   EDTCDT2                                                          
         IC    RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,0(R4)                                                         
*                                                                               
EDTCDT2  XC    WORK+10(20),WORK+10                                              
         GOTO1 VDATCON,(R1),(3,0(R4)),(5,WORK+10)                               
         GOTO1 VDATVAL,(R1),(0,WORK+10),WORK+20                                 
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR               CATCH ERR ESCAPED FROM PREVIOUS DVAL         
*                                                                               
EDTCDT4  DS    0H                                                               
         MVC   DUB(3),PBDCDATE                                                  
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDCDATE,DUB                                                     
         CLC   PBUYKDAT+0(1),PBDCDATE+0                                         
         BH    EDTCDTX                                                          
         BL    *+14                                                             
         CLC   PBUYKDAT+1(1),PBDCDATE+1                                         
         BNL   EDTCDTX                                                          
         LA    R3,109              CLOSING DATE MUST PRECEED INS DATE           
         J     ERROR               (NOTE: SAME YEAR, INS MONTH IS USED)         
*                                                                               
EDTCDTX  MVC   TRCODE,=C'RI'                                                    
         CLC   =C'ID=',8(R2)                                                    
         BE    NEXTD               NEXT COMMENT LINE                            
         MVC   TRCODE,=C'RZ'                                                    
         B     NEXTD               NEXT COMMENT LINE                            
*                                                                               
* ROUTINE FOR SECOND INSERTION DATE                                             
*                                                                               
EDTCD2   DS    0H                                                               
         MVI   PBDEMIND,0          M/E IND                                      
*                                                                               
         OC    PBDIDAT2,PBDIDAT2                                                
         JNZ   ERROR               CAN'T HAVE ANYTHING IN THERE YET             
*                                                                               
         LA    R5,REC+33                                                        
         ST    R9,FULL                                                          
         MVI   ELCODE,X'A6'        ISSUE NAME ELEM                              
         BRAS  RE,NXTELEM                                                       
         L     R9,FULL                                                          
         BNE   *+12                                                             
         LA    R3,ISSNMERR                                                      
         B     ERROR               CAN'T HAVE BOTH                              
*                                                                               
         MVI   PBDIDAT2,X'FF'      SET FOR 'NONE'                               
         CLC   11(4,R2),=C'NONE'                                                
         BNE   EDTCD2A                                                          
*                                                                               
         CLI   TRCODE,C'B'         SEE IF BUYING                                
         BNE   *+8                                                              
         MVI   PBDIDAT2,0          SET FOR 'NONE' ON A NEW BUY                  
         B     NEXTD                                                            
*                                                                               
EDTCD2A  DS    0H                                                               
         MVC   WORK(6),11(R2)                                                   
         CLI   PBDFREQ,C'M'        FOR 'MONTHLIES' ONLY MMM                     
         BNE   EDTCD2B                                                          
         CLI   WORK+3,C' '                                                      
         JH    ERROR                                                            
         MVC   WORK+3(2),=C'01'                                                 
         B     EDTCD2D                                                          
*                                                                               
EDTCD2B  DS    0H                                                               
         LA    RF,WORK+5                                                        
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         CLI   0(RF),C'0'                                                       
         BNL   EDTCD2D                                                          
         MVC   PBDEMIND,0(RF)                                                   
         CLI   PBDEMIND,C'E'                                                    
         BE    EDTCD2D                                                          
         CLI   PBDEMIND,C'M'                                                    
         JNE   ERROR                                                            
*                                                                               
EDTCD2D  DS    0H                                                               
         LA    R4,PBDIDAT2                                                      
         LA    R5,WORK                                                          
         MVI   BYTE,1                                                           
         B     EDTCDT                                                           
*                                                                               
CRELO    DS    F                                                                
*                                                                               
*                                                                               
EDTISSNM DS    0H                                                               
         ST    R9,FULL                                                          
         BRAS  RE,EDTIS00                                                       
         BNE   ERROR               MSG AND CURSOR IS SET IN EDTIS00             
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTADID  DS    0H                                                               
         ST    R9,FULL                                                          
         BRAS  RE,EDTADIDP         AD-ID "PROCESSING"                           
         L     R9,FULL                                                          
         B     NEXTD                                                            
*                                                                               
EDTPO#   LR    R0,R9                                                            
         GOTOR VALBYPO#,0          VALIDATE PURCHASE ORDER #                    
         JNE   ERROR                                                            
         LR    R9,R0                                                            
         B     NEXTD                                                            
*                                                                               
EDTCOS2  GOTOR VAL_COS2,0          VALIDATE COS2                                
         JNE   ERROR                                                            
         J     NEXTD                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKKEYW   NTR1  BASE=*,LABEL=*      R2 POINTS TO FIELD DATA                      
*                                                                               
         L     R2,FULL             POINT TO CURRENT FLD                         
         LA    RE,KEYWTAB                                                       
CKKW30   CLI   0(RE),X'FF'         END OF KEYWORD TAB?                          
         JE    SETCCNEQ            YES, KEYWORD NOT FOUND                       
*                                                                               
         ZIC   RF,0(RE)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),1(RE)       KEYWORD MATCHED?                             
         BNE   CKKW60              NO, TRY NEXT ENTRY IN TABLE                  
*                                                                               
         MVC   FULL,8(RE)          ADDRESS FOR KEYWORD EDIT ROUTINE             
         OC    FULL,FULL                                                        
         JZ    SETCCNEQ            ADDRESS IS NOT IN TABLE                      
         J     SETCCEQ             PASS BY ADDRESS OF ROUTINE IN FULL           
*                                                                               
CKKW60   LA    RE,12(RE)           NEXT ENTRY IN KEYWORD TAB                    
         B     CKKW30                                                           
*                                                                               
KEYWTAB  DC    AL1(02),CL7'IC=    ',AL4(00)                                     
         DC    AL1(02),CL7'PI=    ',AL4(00)                                     
         DC    AL1(02),CL7'SRC=   ',AL4(00)                                     
         DC    AL1(02),CL7'AC=    ',AL4(EDTCAC)                                 
         DC    AL1(02),CL7'PC=    ',AL4(EDTPLCOS)                               
         DC    AL1(02),CL7'CD=    ',AL4(EDTCCD)                                 
         DC    AL1(02),CL7'BD=    ',AL4(EDTCBLDT)                               
         DC    AL1(02),CL7'PD=    ',AL4(EDTCPDDT)                               
         DC    AL1(02),CL7'ID=    ',AL4(EDTCIODT)                               
         DC    AL1(02),CL7'D2=    ',AL4(EDTCD2)                                 
         DC    AL1(02),CL7'SD=    ',AL4(EDTCSHDT)                               
         DC    AL1(04),CL7'SREP=  ',AL4(EDTSREP)                                
         DC    AL1(03),CL7'TAX=   ',AL4(EDTTAX)                                 
         DC    AL1(03),CL7'FSI=   ',AL4(EDTFSI)                                 
         DC    AL1(02),CL7'OR=    ',AL4(EDTORATE)                               
         DC    AL1(03),CL7'CLE=   ',AL4(EDTCLE)                                 
         DC    AL1(05),CL7'MANIO= ',AL4(EDTMANIO)                               
         DC    AL1(02),CL7'CU=    ',AL4(EDTCU)                                  
         DC    AL1(04),CL7'UPID=  ',AL4(EDTUPID)                                
         DC    AL1(03),CL7'REF=   ',AL4(EDTREF)                                 
         DC    AL1(03),CL7'PST/   ',AL4(EDTPST)                                 
         DC    AL1(02),CL7'PV=    ',AL4(EDTPVWS)                                
         DC    AL1(02),CL7'CT=    ',AL4(EDTCTUS)                                
         DC    AL1(03),CL7'GST=   ',AL4(EDTGST)                                 
         DC    AL1(03),CL7'SFH=   ',AL4(EDTSFH)                                 
         DC    AL1(06),CL7'EXDAYS=',AL4(EDTEXD)                                 
******** DC    AL1(04),CL7'IMPS=  ',AL4(EDTIMP)                                 
         DC    AL1(05),CL7'EIMPS= ',AL4(EDTIMP)        SAME AS IMPS             
         DC    AL1(05),CL7'AIMPS= ',AL4(EDTAIMP)                                
         DC    AL1(03),CL7'CLD=   ',AL4(EDTCLDT)                                
         DC    AL1(03),CL7'OSD=   ',AL4(EDTOSDT)                                
         DC    AL1(06),CL7'EXDATE=',AL4(EDTEXDT)                                
******** DC    AL1(02),CL7'LW=    ',AL4(EDTLW)                                  
         DC    AL1(05),CL7'TRAFF= ',AL4(EDTTRAFF)                               
         DC    AL1(03),CL7'ISS=   ',AL4(EDTISSNM)                               
         DC    AL1(04),CL7'ADID=  ',AL4(EDTADID)                                
         DC    AL1(02),CL7'PO=    ',AL4(EDTPO#)                                 
         DC    AL1(02),CL7'COS2=  ',AL4(EDTCOS2)                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*               CHECK FOR VALID AD-ID CHANGE                          *         
*                MUST BE "AD-ID ONLY" AD-ID.                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTADIDP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,8+5(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,5(R2)                                                         
         AHI   R5,-5               ADJUST INPUT LENGTH                          
         JNP   ERROR                                                            
         CHI   R5,12                                                            
         JNE   ERROR               MUST BE EXACTLY 12 CHARACTERS                
*                                                                               
         MVC   WORK(L'KEY+L'KEY),KEY     SAVE KEY AND KEYSAVE                   
*                                                                               
         LA    R3,NFNDERR                                                       
         XC    KEY,KEY             LOOK FOR PASSIVE POINTER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'C1'         AD ID PASSIVE                                
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         MVC   KEY+10(12),0(R4)                                                 
         BRAS  RE,HIGH                                                          
         CLC   KEY(22),KEYSAVE                                                  
         JNE   ERROR               RECORD NOT FOUND                             
*                                                                               
         L     R6,AJOBIO           ADDRESS OF JOBREC                            
         USING PJOBRECD,R6                                                      
*                                                                               
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEY+27,0(R6),(TERMNAL,DMWORK)                                    
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND JOB RECORD                         
*                                                                               
         MVC   KEY(L'KEY+L'KEY),WORK   RESTORE KEY AND KEYSAVE                  
         XC    WORK,WORK           CLEAR                                        
*                                                                               
         LA    R3,INVERR                                                        
         CLI   PJOBKJOB,X'FF'      AD-ID WITHOUT JOB CODE ?                     
         JNE   ERROR               NO - INVALID OPTIONAL ENTRY                  
*                                                                               
         LA    R7,PBDJOB-NEWREC+REC                                             
         CLC   PBDJOB,PJOBKJOB   BUYREC JOB CODE NE TO JOBREC JOB CODE?         
         BE    EDTADX              DONE - NO CHANGE                             
*                                                                               
         CLI   WSJIND,1            CAN'T CHG JOB IF WSJ BUY                     
         BNE   EDTAD10                                                          
         LA    R3,WSJERR                                                        
         J     ERROR                                                            
*                                                                               
EDTAD10  OC    0(6,R7),0(R7)       DONT SET CHNG IND IF THERE                   
         BNZ   EDTAD20             WAS NO OLD AD                                
         OI    CHGIND3,X'40'       SET AD CODE ADDED                            
         B     EDTAD30                                                          
EDTAD20  OI    CHGIND2,X'08'                                                    
*                                                                               
EDTAD30  MVC   0(6,R7),PJOBKJOB    "NEW" JOB CODE (X'FF'....)                   
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BNZ   EDTAD90                                                          
         MVI   ELCODE,X'70'        NON-WEB INSERTION ORDER ELEM                 
         BRAS  RE,CKIOEL                                                        
         BE    EDTAD40             ERROR                                        
         MVI   ELCODE,X'71'        WEB INSERTION ORDER ELEM                     
         BRAS  RE,CKIOEL                                                        
         BNE   EDTAD90                                                          
EDTAD40  LA    R3,JBDERR           CANNOT REMOVE AD NO IF IO THERE              
         J     ERROR                                                            
*                                                                               
EDTAD90  DS    0H                  SEND JOBCODE FOR PPBUY03 EDITING             
         MVC   WORK(6),PJOBKJOB    "NEW" JOB CODE (X'FF'....)                   
         MVI   WORK+6,OPT_JOBQ                                                  
         GOTOR VT41103,DMCB,(RC),(RA),EDTADCDQ,WORK                             
*                                                                               
EDTADX   J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  R6,RB                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALBYPO# NTR1  BASE=*,LABEL=*      VALIDATE PURCHASE ORDER #                    
*                                                                               
         LHI   R3,PO#OVRNO         PO# OVERRIDE NOT ALLOWED FOR ZZZ             
         CLC   =C'ZZZ',BUYPR                                                    
         JE    SCCNEQ_R                                                         
*                                                                               
         CHI   R1,0                PO# OVERRIDE VALIDATION?                     
         BE    *+10                                                             
         SR    R3,R3                                                            
         B     VBYPO#20                                                         
*                                                                               
         ZIC   R5,5(R2)                                                         
         SHI   R5,3                ADJUST INPUT LENGTH                          
         CHI   R5,PO#DMXLQ         GREATER THAN ALLOWED MAX VALUE?              
         BNH   *+12                                                             
         LHI   R3,DEMAXERR                                                      
         J     SCCNEQ_R                                                         
*                                                                               
         LA    R3,3+8(R2)          POINT TO START OF PO# INPUT                  
VBYPO#20 MVI   DMCB,X'18'                                                       
         BRAS  RE,LOADOVLY                                                      
         GOTOR (RF),DMCB,(RC),(RA),('VAL_PO#Q',(R3))                            
         ICM   R3,15,DMCB+8                                                     
         LTR   R3,R3               CAME BACK WITH ERROR?                        
         JNZ   SCCNEQ_R                                                         
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        RESTORE ADDRESS                              
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
CBUYOVLY LR    R0,RE                                                            
         XC    DMCB+1(23),DMCB+1                                                
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LOADOVLY LR    R0,RE                                                            
         XC    DMCB+1(23),DMCB+1                                                
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VAL_COS2 NTR1  BASE=*,LABEL=*      VALIDATE COS2 $ OR FACTOR                    
*                                                                               
         CHI   R1,0                COS2 OVERRIDE VALIDATION?                    
         JE    *+10                                                             
         SR    R3,R3                                                            
         J     VBYC2$20                                                         
*                                                                               
         LA    R3,5+8(R2)          POINT TO START OF COS2= INPUT                
VBYC2$20 MVI   DMCB,X'18'                                                       
         BRAS  RE,LOADOVLY                                                      
         GOTOR (RF),DMCB,(RC),(RA),('VAL_CO2Q',(R3))                            
         ICM   R3,15,DMCB+8                                                     
         LTR   R3,R3               CAME BACK WITH ERROR?                        
         JNZ   SCCNEQ_R                                                         
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        RESTORE ADDRESS                              
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTIS00  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,12(R2)           POINT TO INPUT (8+4, ISS=)                   
         LA    R3,INVERR           ERR MSG                                      
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4               MINUS 4 BYTES ANNOTATION                     
         XC    DUB,DUB                                                          
*                                                                               
         OC    PBDIDAT2-NEWREC+REC,PBDIDAT2-NEWREC+REC                          
         BNZ   EDTISRRR            CAN'T HAVE BOTH D2= AND ISS=                 
*                                                                               
         CHI   R5,04               REMOVING ISSUE NAME?                         
         BNE   *+14                                                             
*                                                                               
         CLC   =C'NONE',12(R2)                                                  
         BE    EDTIS05                                                          
*                                                                               
         CHI   R5,0                                                             
         BE    EDTIS05             INPUT 0                                      
         BH    EDTIS10             INPUT PRESENT, GO CHECK IT                   
         J     SCCNEQ_R                                                         
*                                                                               
EDTIS05  DS    0H                                                               
         MVI   DUB+1,X'FF'         SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         JE    SCCNEQ_R                                                         
         B     EDTIS20             MUST BE REMOVING ELEM THEN                   
*                                                                               
EDTIS10  DS    0H                                                               
         CHI   R5,11               GREATER THAN MAX OF 11?                      
         JH    SCCNEQ_R                                                         
*                                                                               
EDTIS20  DS    0H                                                               
         MVI   WORK+0,X'A6'        ISSUE NAME    ELEM CODE                      
         MVI   WORK+1,13           ELEM LENGTH                                  
         MVC   WORK+2(11),=11C' '                                               
         CHI   R5,0                                                             
         BE    EDTIS30                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+2(0),0(R4)     GET ISSUE NAME    FROM INPUT                 
*                                                                               
EDTIS30  DS    0H                                                               
         LA    R5,NEWREC+33        START BUILDING ELEMENT                       
         MVI   ELCODE,X'A6'        ISSUE NAME    ELEM CODE                      
         BRAS  RE,NXTELEM                                                       
         JE    SCCNEQ_R            CAN'T ALREADY HAVE ELEM                      
*                                                                               
         CLI   DUB+1,X'FF'         SPECIAL CODE PRESENT?                        
         BNE   *+8                                                              
         MVI   WORK+2,X'FF'        MOVE IN DELETION CODE                        
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
*                                                                               
         J     SCCEQ_R                                                          
*                                                                               
EDTISRRR DS    0H                                                               
         LA    R3,ISSNMERR                                                      
         J     SCCNEQ_R                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKLW    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,11(R2)           POINT TO INPUT                               
         LA    R3,INVERR           ERR MSG                                      
         ZIC   R5,5(R2)                                                         
         AHI   R5,-3               MINUS 3 BYTE ANNOTATION                      
         XC    DUB,DUB             FOR WORKING STORAGE                          
*                                                                               
         CHI   R5,0                                                             
         BE    CHKLW10             INPUT IS ZERO                                
         BH    CHKLW30             INPUT PRESENT, GO CHECK IT                   
         J     SCCNEQ_R            NEGATIVE INPUT LENGTH, ERROR                 
*                                                                               
CHKLW10  MVI   DUB+1,X'FF'         SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         JE    SCCNEQ_R                                                         
         B     CHKLW60             MUST BE REMOVING ELEM THEN                   
*                                                                               
CHKLW30  CHI   R5,3                CHECK IF "DEL" IS ENTERED                    
         BH    CHKLW30H                                                         
         CLC   =C'DEL',0(R4)       DEL IS ENTERED?                              
         BE    CHKLW10                                                          
*                                                                               
CHKLW30H CHI   R5,5                MORE THAN 5 CHARS?                           
         JH    SCCNEQ_R                                                         
         CHI   R5,4                NONE IS ENTERED?                             
         BNE   CHKLW30L                                                         
         CLC   =C'NONE',0(R4)      X=NONE FOR LEGAL WARNING CODE?               
         JNE   SCCNEQ_R                                                         
         MVI   DUB+2,C'X'          X FOR NONE                                   
         B     CHKLW60                                                          
*                                                                               
CHKLW30L CLI   0(R4),C'A'                                                       
         BE    CHKLW35                                                          
         CLI   0(R4),C'B'                                                       
         BE    CHKLW35                                                          
         CLI   0(R4),C'C'                                                       
         BE    CHKLW35                                                          
         CLI   0(R4),C'D'                                                       
         BE    CHKLW35                                                          
         CLI   0(R4),C'X'          X=NONE?                                      
         JNE   SCCNEQ_R                                                         
         CHI   R5,1                                                             
         JH    SCCNEQ_R            ONLY 1 CHAR INPUT FOR "X"                    
         B     CHKLW37                                                          
*                                                                               
CHKLW35  CHI   R5,1                INPUT LENGTH IS ONE?                         
         BE    CHKLW37             YES, NO NEED TO CHK QUARTERLY CODE           
         CHI   R5,2                INPUT LENGTH IS TWO?                         
         BE    CHKLW35F            YES, GO CHECK QUARTERLY CODE                 
         CLC   =C'NONE',1(R4)      X=NONE FOR QUARTERLY CODE?                   
         JNE   SCCNEQ_R                                                         
         MVC   DUB+2(1),0(R4)      LEGAL WARNING CODE                           
         MVI   DUB+3,C'X'          QUARTERLY CODE (X)                           
         B     CHKLW60                                                          
*                                                                               
CHKLW35F CLI   1(R4),C'X'          X=NONE?                                      
         BE    CHKLW35H                                                         
         CLI   1(R4),C'1'                                                       
         JL    SCCNEQ_R            LOWER THAN CHAR 1 IS NO GOOD                 
         CLI   1(R4),C'4'                                                       
         JH    SCCNEQ_R            HIGHER THAN CHAR 4 IS ALSO NO GOOD           
CHKLW35H MVC   DUB+2(2),0(R4)      SAVE VALIDATED LW & QUARTERLY CODE           
         B     CHKLW60                                                          
*                                                                               
CHKLW37  MVC   DUB+2(1),0(R4)      SAVE VALIDATED LW CODE                       
*                                                                               
CHKLW60  CLI   WORK,X'94'          LEGAL WARNINGS ELEM CODE?                    
         BE    *+6                                                              
         DC    H'0'                NO OTHER ELEM CODE GOES HERE YET             
*                                                                               
         LA    R5,NEWREC+33        START BUILDING ELEMENT                       
         MVC   ELCODE,WORK                                                      
         BRAS  RE,NXTELEM                                                       
         JE    SCCNEQ_R            CAN'T ALREADY HAVE ELEM                      
*                                                                               
         XC    WORK+10(10),WORK+10                                              
         MVC   WORK+10(1),WORK     SET ELEMENT CODE                             
         MVI   WORK+11,X'04'       ELEMENT LENGTH                               
*                                                                               
         CLI   DUB+1,X'FF'         SPECIAL CODE PRESENT?                        
         BE    *+14                                                             
         MVC   WORK+12(2),DUB+2    ALPHA AND DIGIT (IF PRESENT)                 
         B     *+10                                                             
         MVC   WORK+12(1),DUB+1    MOVE IN DELETION CODE                        
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
         J     SCCEQ_R             EQUAL                                        
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKNTRA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    PBDJOB,PBDJOB       ADCODE PRESENT?                              
         BZ    *+12                                                             
         LA    R3,182              CANNOT CHANGE "NO TRAFFIC" STATUS            
         J     SCCNEQ_R                                                         
*                                                                               
         LA    R4,14(R2)           POINT TO INPUT                               
         LA    R3,INVERR           INVALID INPUT ERROR                          
         ZIC   R5,5(R2)                                                         
         AHI   R5,-6               MINUS 6 BYTE ANNOTATION (TRAFF=)             
*                                                                               
         CHI   R5,0                ANY INPUTS?                                  
         BNE   *+8                                                              
         J     SCCNEQ_R                                                         
*                                                                               
         CHI   R5,3                                                             
         BNE   *+18                                                             
         CLC   =C'YES',0(R4)       TRAFF=YES?                                   
         BE    CHKNT50                                                          
         J     SCCNEQ_R                                                         
*                                                                               
         CHI   R5,2                                                             
         BNE   *+18                                                             
         CLC   =C'NO',0(R4)        TRAFF=NO?                                    
         BE    CHKNT50                                                          
         J     SCCNEQ_R                                                         
*                                                                               
         CHI   R5,1                                                             
         JNE   SCCNEQ_R            NO OTHER VALID LENGTH                        
         CLI   0(R4),C'Y'          TRAFF=Y?                                     
         BE    CHKNT50                                                          
         CLI   0(R4),C'N'          TRAFF=N?                                     
         BE    CHKNT50                                                          
         J     SCCNEQ_R                                                         
*                                                                               
CHKNT50  CLI   0(R4),C'Y'          TRAFFIC STATUS IS YES?                       
         BNE   *+12                                                             
         NI    PBDSTAT,X'FF'-X'20'                                              
         J     SCCEQ_R                                                          
         OI    PBDSTAT,X'20'       STATUS IS NO TRAFFIC                         
*                                                                               
         J     SCCEQ_R             EQUAL                                        
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADBYER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DDLINKSW,C'D'       DELETE INSERTION UPLOAD?                     
         JE    SETCCNEQ            NO NEED TO PROC OPTIONAL DATA FLDS           
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    CKADB50                                                          
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    CKADB50                                                          
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         JNE   SETCCNEQ                                                         
*                                                                               
CKADB50  MVI   DMCB,X'18'          PROCESS OPTIONAL DATA UPLOAD                 
         BRAS  RE,CBUYOVLY                                                      
*                                                                               
         MVI   DMCB,X'19'          PROCESS CUSTOM COLUMN UPLOAD                 
         BRAS  RE,CBUYOVLY                                                      
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        RESTORE ADDRESS                              
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCC2FAC NTR1  BASE=*,LABEL=*      PROCESS COS2 FACTOR                          
*                                                                               
         TM    SVCLPROF+30,X'08'   SEE IF FACTOR TYPE COST2 CLT                 
         BNO   P_C2F_X                                                          
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'91'        CK FOR NEWREC FOR FACTOR ELEM                
         BRAS  RE,NXTELEM                                                       
         BE    P_C2F_X             FOUND                                        
*                                                                               
         L     R1,TRADDR                                                        
         CLI   8(R1),C'C'          CHANGE?                                      
         BNE   P_C2F46                                                          
         TM    GENBYSW1,C2FOVRDQ   FACTOR IS OVERRIDDEN IN BUY?                 
         BNZ   P_C2F46                                                          
         LR    RF,R5               SAVE NEWREC POINTER                          
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   P_C2F36                                                          
         USING PCOS2FEL,R5                                                      
         TM    PCOS2FLG,PCOS2OVR   FACTOR IS OVERRIDDEN IN BUY?                 
         BZ    P_C2F36                                                          
         ZAP   SVE2FAC,PCOS2FAC                                                 
         ZAP   SVC2FAC,PCOS2FAC                                                 
         OI    GENBYSW1,C2FOVRDQ                                                
         DROP  R5                                                               
P_C2F36  LR    R5,RF               RESTORE NEWREC POINTER                       
*                                                                               
P_C2F46  LA    R1,SVE2FAC                                                       
         OC    SVE2FAC,SVE2FAC                                                  
         BZ    *+14                                                             
         CP    SVE2FAC,=P'0'                                                    
         BNE   *+8                                                              
         LA    R1,SVC2FAC                                                       
*                                                                               
         XC    WORK(10),WORK       NOT FOUND -  MUST ADD ELEM                   
         LA    RE,WORK                                                          
         USING PCOS2FEL,RE                                                      
         MVC   WORK(2),=X'9108'                                                 
         MVC   PCOS2FAC,0(R1)                                                   
         TM    GENBYSW1,C2ROUNDQ   COS2 FACTOR ROUNDING?                        
         BZ    *+8                                                              
         OI    PCOS2FLG,PCOS2RND                                                
         TM    GENBYSW1,C2FOVRDQ   FACTOR IS OVERRIDDEN IN BUY?                 
         BZ    *+8                                                              
         OI    PCOS2FLG,PCOS2OVR                                                
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
         NI    GENBYSW1,X'FF'-C2FOVRDQ                                          
         DROP  RE                                                               
*                                                                               
P_C2F_X  J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SHIP DATE      -  GOES INTO PBSHPDEL (X'86')                                  
* EXTENSION DATE -  GOES INTO PEXDATEL (X'96')                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSHIP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    RF,SRELO                                                         
*                                                                               
EDTSHP2  LA    R4,8+3(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(20),WORK                                                    
         ZIC   R5,5(R2)                                                         
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   EDTSHP2H                                                         
         LA    R4,8+7(R2)          POINT TO INPUT                               
         AHI   R5,-7               ADJUST INPUT LENGTH (FOR EXDATE=)            
         B     *+8                                                              
EDTSHP2H AHI   R5,-3               ADJUST INPUT LENGTH (FOR SD=)                
         BP    EDTSHP4                                                          
*                                                                               
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BE    EDTSHP30                                                         
*                                                                               
EDTSHP3H MVC   WORK(3),=3X'FF'     SPECIAL DELETE CODE                          
         B     EDTSHP10                                                         
*                                                                               
EDTSHP4  DS    0H                                                               
         CHI   R5,8                MAX IS MMMDD/YY (8 CHARS)                    
         JH    ERROR                                                            
*                                                                               
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   *+14                                                             
         CLC   =C'NONE',0(R4)                                                   
         BE    EDTSHP3H            DELETE EXTENSION DATE ELEM                   
*                                                                               
         MVC   WORK(8),0(R4)                                                    
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+10                                    
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR                                                            
*                                                                               
* NOTE - DATVAL RETURNS 00 IN YEAR IF NOT PRESENT                               
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK+10),(3,WORK)                                
*                                                                               
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   EDTSHP4H                                                         
         CLC   WORK(3),PBUYKDAT                                                 
         BNH   EDTSHP4H                                                         
         LA    R3,EXDERR1          EXDATE MUST BE ON/BEFORE INS DATE            
         J     ERROR                                                            
*                                                                               
EDTSHP4H XC    WORK+3(2),WORK+3                                                 
         CLC   WORK+10(2),=C'00'   TEST HAVE YEAR                               
         BNE   EDTSHP4X                                                         
         MVC   WORK+0(1),PBUYKDAT  SET YEAR                                     
         CLC   WORK(3),PBUYKDAT                                                 
         BH    EDTSHP4X                                                         
         ZIC   RF,PBUYKDAT                                                      
         LA    RF,1(RF)            DATE IN NEXT YEAR                            
         STC   RF,WORK                                                          
*                                                                               
EDTSHP4X DS    0H                                                               
*                                                                               
EDTSHP10 DS    0H                  NOW STORE IN ELEM                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'86'                                                     
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   *+8                                                              
         MVI   ELCODE,X'96'                                                     
         ST    R9,DUB              FULL MIGHT BE IN USE                         
         BRAS  RE,NXTELEM                                                       
         BNE   EDTSHP20            MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE DATE ELEM                 
*                                                                               
EDTSHP20 L     R9,DUB              MUST RESTORE R9                              
         XC    WORK+15(15),WORK+15                                              
         MVC   WORK+15(2),=X'8607' CODE AND LENGHT                              
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         BNE   *+10                                                             
         MVC   WORK+15(2),=X'9605' CODE AND LENGHT FOR EXTENSION DATE           
         MVC   WORK+17(5),WORK                                                  
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTSHP30 J     EXIT_X                                                           
*                                                                               
SRELO    DS    F                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTORAT  NTR1  BASE=*,LABEL=*      EDIT OPEN RATE                               
*                                                                               
         ST    RF,ORELO                                                         
*                                                                               
         LA    R3,INVERR                                                        
         CLI   SVESPROF+30,C'Y'   SET TO 'Y' FOR FINANCIAL CLIENTS              
         JNE   ERROR              IN T41101                                     
*                                                                               
* ONLY FINANCIAL CLTS CAN HAVE OPEN RATES                                       
*                                                                               
         LA    R3,INVRTERR                                                      
         MVI   BYTE,5              PRESET FOR 5 DEC.                            
         CLI   PBDCOSTY,C'U'       SEE IF UNIT COST IN BUYREC                   
         BE    EDTOR7                                                           
         MVI   BYTE,2              ELSE MUST BE TOTAL RATE                      
*                                                                               
EDTOR7   DS    0H                                                               
         ZIC   R0,5(R2)                                                         
         AHI   R0,-3                                                            
         JNP   ERROR                                                            
         CLC   11(3,R2),=C'CON'           MEANS USE CONTRACT RATE               
         BE    EDTOR8D                                                          
         LA    R5,11(R2)                                                        
         CLI   0(R5),C'T'                                                       
         BNE   EDTOR8                                                           
         LA    R5,1(R5)                                                         
         AHI   R0,-1                                                            
         JNP   ERROR                                                            
         MVI   BYTE,2                                                           
EDTOR8   GOTO1 VCASHVAL,DMCB,(BYTE,(R5)),(X'40',(R0))                           
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'849999999'   MAX FOR PDBCOS                               
         JH    ERROR                                                            
EDTOR8D  XC    X(15),X                                                          
         MVC   X(2),=X'300D'                                                    
         LA    R3,X                                                             
         USING PORELEMD,R3                                                      
         CLC   11(3,R2),=C'CON'         MEANS USE CONTRACT RATE                 
         BNE   EDTOR8G                                                          
         MVC   PORCOS,PBDCOS                                                    
         MVI   PORCOSTY,C'C'            TELLS PPBUY03 NOT TO LOOK=UP            
         B     EDTOR10                   OPEN RATE - JUST SET IT TO             
*                                        CONTRACT                               
EDTOR8G  ZAP   PORCOS,DUB                                                       
         LTR   R0,R0         IF OPEN RATE IS FREE, PBDCOS MUST ALSO             
         BNZ   EDTOR9              BE FREE  (=.00001)                           
         ZAP   PORCOS,=P'1'                                                     
         CP    PBDCOS,=P'1'                                                     
         BE    EDTOR9                                                           
         LA    R3,INVRTERR                                                      
         J     ERROR                                                            
*                                                                               
EDTOR9   MVC   PORCOSTY,PBDCOSTY                                                
         CLI   11(R2),C'T'          SEE IF TOTAL COST INPUT                     
         BNE   *+8                                                              
         MVI   PORCOSTY,C'T'                                                    
*                                                                               
EDTOR10  MVI   PORCOS+5,C'O'     SO CHG17 WILL KNOW OPEN RATE WAS               
*                                OVERRIDDEN                                     
*                                RESET TO X'00' IN CHG17                        
*                                                                               
EDTOR11  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'      SEE IF OPEN RATE ELEM ALREADY THERE            
         ST    R9,DUB                                                           
         BRAS  RE,NXTELEM                                                       
         BNE   EDTOR12                                                          
         LA    R3,INVERR                                                        
         J     ERROR                                                            
*                                                                               
EDTOR12  L     R9,DUB                                                           
         GOTO1 VRECUP,DMCB,(1,NEWREC),X,(R5)                                    
*                                                                               
EDTORX   J     EXIT_X                                                           
         DROP  R3                                                               
*                                                                               
ORELO    DS    F                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTMAN   NTR1  BASE=*,LABEL=*      EDIT MANUAL IO                               
*                                                                               
         ST    RF,MRELO                                                         
*                                                                               
         CLI   PBDJOB,C' '                                                      
         BNH   EDTMANE2                                                         
         XC    X(80),X             BUILD IO ELEM IN X                           
         MVC   X(2),=X'7032'                                                    
         LA    R3,X                                                             
         USING PIOELEM,R3                                                       
*                                  SCAN FOR DATE                                
         LA    R5,14(R2)           GET PAST MANIO=                              
         LA    R4,X+60                                                          
EDTMN3   CLI   0(R5),C','          DELIMITED BY ,                               
         BE    EDTMN5                                                           
         CLI   0(R5),C' '                                                       
         BNH   EDTMANE3            IO DATE INVALID                              
         MVC   0(1,R4),0(R5)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     EDTMN3                                                           
*                                                                               
EDTMN5   DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(0,X+60),WORK                                       
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    EDTMANE3            IO DATE INVALID                              
* PUT DATE IN RECORD                                                            
         GOTO1 VDATCON,(R1),(0,WORK),(3,PIODATE)                                
*                                                                               
EDTMN10  DS    0H                  NOW EDIT IO NUMBER 4 DIGITS                  
         LA    R5,1(R5)            GET PAST ,                                   
         SR    R1,R1                                                            
         XC    X+60(5),X+60                                                     
         LA    R4,X+60                                                          
EDTMN15  CLI   0(R5),C','                                                       
         BE    EDTMN20            IO NUMBER INVALID                             
         CLI   0(R5),C'9'                                                       
         BH    EDTMANE4                                                         
         CLI   0(R5),C'0'                                                       
         BL    EDTMANE4            NON-NUMERIC                                  
         MVC   0(1,R4),0(R5)                                                    
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     EDTMN15                                                          
*                                                                               
EDTMN20  CHI   R1,4                                                             
         BH    EDTMANE4            IO NUMBER INVALID                            
         LTR   R1,R1                                                            
         BZ    EDTMANE4                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,X+60(0)                                                      
         CP    DUB,=P'0'                                                        
         BNH   EDTMANE4                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   PIONUM(2),HALF                                                   
         MVC   PIONUM+2(2),=X'0101'   SET POSITIONAL NUMBER                     
*                                  AND NUMBER OF BUYS TO 1                      
EDTMN30  LA    R5,1(R5)                                                         
         MVI   PIOTYP,C'C'                                                      
         CLC   0(3,R5),=C'CHA'                                                  
         BE    EDTMN40                                                          
         MVI   PIOTYP,C'N'                                                      
         CLC   0(3,R5),=C'NEW'                                                  
         BE    EDTMN40                                                          
         MVI   PIOTYP,C'D'         CANCEL                                       
         CLC   0(3,R5),=C'CAN'                                                  
         BE    EDTMN40                                                          
         MVI   PIOTYP,C'X'         SPECIAL DELETE CODE                          
         CLC   0(3,R5),=C'DEL'                                                  
         BNE   EDTMANE5            INVALID TYPE                                 
*                                                                               
*                                                                               
EDTMN40  TM    PBUYCNTL,X'80'                                                   
         BNZ   EDTMANE5            CAN'T BE DELETED BUY                         
EDTMN50  MVI   PIOTURN,C'M'        MANUAL                                       
         MVC   PIOIDATE,PBUYKDAT                                                
         MVC   PIOJOB,PBDJOB                                                    
*                                  FOLLOWING ONLY FOR NEWSPAPERS                
         MVC   PIOSAU,PBDSPACE                                                  
         MVC   PIOPRM(1),PBDCL                                                  
         MVC   PIOUIND,PBDUIND                                                  
         ZAP   PIOUNITS,PBDUNITS                                                
         ZAP   PIOCLMS,PBDCLMS                                                  
*                                                                               
         XC    PBDIODAT,PBDIODAT                                                
*                                                                               
*              NOTE - MANIO ELEMS GO INTO NEWREC                                
*              ON CHANGES CHG ROUTINE WILL FIND AND MOVE THEM                   
*                                                                               
         LA    R4,NEWREC                                                        
         LA    R5,33(R4)                                                        
         MVI   ELCODE,X'70'                                                     
*                                  NOW ADD ELEM TO RECORD                       
         LR    RE,R9                                                            
EDTMN55  BRAS  RE,NXTELEM                                                       
         BNE   EDTMN60                                                          
         OC    2(3,R5),2(R5)       CHK FOR REAL IO                              
         BNZ   EDTMN55             YES                                          
         MVC   0(50,R5),X          NO JUST MOVE IN MANUAL IO                    
         LR    R9,RE                                                            
         B     EDTMN70                                                          
*                                                                               
EDTMN60  LR    R9,RE                                                            
         GOTO1 VRECUP,DMCB,(1,0(R4)),X,(R5)                                     
         B     EDTMN70                                                          
*                                                                               
EDTMN70  MVC   TRCODE,=C'RI'        SET FOR INSERTION RECALL                    
         B     EDTMANX                                                          
*                                                                               
EDTMANE2 LA    R3,IODERR4          MUST HAVE JOB                                
         J     ERROR                                                            
EDTMANE3 LA    R3,INVDTERR         IO DATE ERROR                                
         J     ERROR                                                            
EDTMANE4 LA    R3,IONUMER          IO NUMBER ERROR                              
         J     ERROR                                                            
EDTMANE5 LA    R3,IOTYPER          TYPE ERROR                                   
         J     ERROR                                                            
*                                                                               
EDTMANX  J     EXIT_X                                                           
*                                                                               
MRELO    DS    F                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSR    NTR1  BASE=*,LABEL=*      EDIT SPECIAL REP CODE (PBSREPEL)             
*                                                                               
         ST    RF,SRRELO                                                        
*                                                                               
         L     R1,TRADDR        SEE IF BUYING                                   
         CLI   8(R1),C'B'                                                       
         BE    EDTSR2           YES                                             
*                                  SHOULD BE SURE PAYMENTS NET TO ZERO          
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         OC    PGROSS,PGROSS                                                    
         BZ    EDTSR2                                                           
         LA    R3,SRPDERR                                                       
         J     ERROR                                                            
*                                                                               
*                                                                               
EDTSR2   LA    R4,8+5(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,5(R2)                                                         
         AHI   R5,-5               ADJUST INPUT LENGTH                          
         JNP   ERROR                                                            
         CHI   R5,4                                                             
         JH    ERROR                                                            
         LR    R1,R5               SAVE LENGHT                                  
EDTSR5   CLI   0(R4),C'0'          CHK FOR NUMERICS                             
         JL    ERROR                                                            
         CLI   0(R4),C'9'                                                       
         JH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R1,EDTSR5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8+5(0,R2)                                                    
         OI    DUB,X'0F'                                                        
         UNPK  WORK(4),DUB                                                      
         CLC   WORK(4),=C'0000'                                                 
         BNE   EDTSR8                                                           
         MVI   WORK,C'X'           SPECIAL DELETE CODE                          
         B     EDTSR10                                                          
*                                                                               
EDTSR8   XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),WORK                                                    
         BRAS  RE,HIGH                                                          
         CLC   KEY(9),KEYSAVE                                                   
         BE    EDTSR10             FOUND - OK                                   
*                                                                               
         LA    R3,NFNDERR                                                       
         J     ERROR                                                            
*                                                                               
EDTSR10  DS    0H                  NOW STORE IN PBSREPEL                        
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'                                                     
         LR    R1,R9             MUST SAVE R9                                   
         BRAS  RE,NXTELEM                                                       
         BNE   EDTSR20              MUST ADD ONE                                
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE REP ELEM                  
*                                                                               
EDTSR20  LR    R9,R1               MUST RESTORE R9                              
*                                                                               
         XC    WORK+10(10),WORK+10                                              
         MVC   WORK+10(2),=X'800A'    CODE AND LENGHT                           
         MVC   WORK+12(4),WORK     REP CODE OR C'X'                             
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
EDTSR30  J     EXIT_X                                                           
*                                                                               
SRRELO   DS    F                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUILD ELEM WITH ONE PACKED DATA FIELD, WORK+0 HAS ELEMENT CODE                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPACK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,8+5(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,5(R2)                                                         
         AHI   R5,-3               MINUS 3 BYTE ANNOTATION                      
*                                                                               
         CLI   WORK,X'89'          SEE IF EXTENSION DAYS                        
         BNE   *+8                                                              
         AHI   R5,-4               TOTAL OF 7 FOR EXT DAYS                      
*                                                                               
         CLI   WORK,X'92'          SEE IF IMPRESSION                            
         BNE   EDTPAC2H                                                         
         AHI   R5,-2               TOTAL OF 5 FOR IMPRESSION                    
         CLI   8(R2),C'E'          SEE IF EIMPS IS ENTERED                      
         BNE   *+8                                                              
         AHI   R5,-1               TOTAL OF 6 FOR ESTIMATED IMPRESSION          
*                                                                               
EDTPAC2H CLI   WORK,X'93'          SEE IF ACTUAL IMPRESSION                     
         BNE   *+8                                                              
         AHI   R5,-3               TOTAL OF 6 FOR ACTUAL IMPRESSION             
*                                                                               
         LTR   R5,R5                                                            
         BP    EDTPAC4                                                          
*                                                                               
EDTPAC2X MVI   DUB+1,C'X'          SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         JE    ERROR                                                            
         B     EDTPAC10                                                         
*                                                                               
EDTPAC4  CHI   R5,13               VALIDATE UP TO 13 CHARS INPUT                
         JH    ERROR                                                            
*                                                                               
         LA    R3,INVERR                                                        
         ZIC   R0,5(R2)            LENGTH OF FIELD                              
         AHI   R0,-3               ADJUST FOR 3 BYTE ANNOTATION                 
         LA    R5,11(R2)           POINT TO INPUT (NO OVERHEAD)                 
*                                                                               
         CLI   WORK,X'87'          PAGE VIEW?                                   
         BE    EDTPAC4B            CK IF DEL IS ENTERED                         
         CLI   WORK,X'88'          CLICK THRU?                                  
         BE    EDTPAC4B            CK IF DEL IS ENTERED                         
*                                                                               
         CLI   WORK,X'92'          SEE IF IMPRESSION                            
         BNE   EDTPAC4C                                                         
         AHI   R0,-2               TOTAL OF 5 FOR IMPRESSION                    
         LA    R5,2(R5)                                                         
         CLI   8(R2),C'E'          SEE IF EIMPS IS ENTERED                      
         BNE   *+12                                                             
         AHI   R0,-1                                                            
         LA    R5,1(R5)                                                         
EDTPAC4B CLC   =C'DEL',0(R5)       'DEL' ENTERED (TO REMOVE ELEM)?              
         BE    EDTPAC2X            NO NEED TO VALIDATE FURTHER                  
         B     EDTPAC4J                                                         
*                                                                               
EDTPAC4C CLI   WORK,X'93'          SEE IF ACTUAL IMPRESSION                     
         BNE   EDTPAC4I                                                         
         AHI   R0,-3               TOTAL OF 6 FOR ACTUAL IMPRESSION             
         LA    R5,3(R5)                                                         
         B     EDTPAC4B            CK IF DEL IS ENTERED                         
*                                                                               
EDTPAC4I CLI   WORK,X'89'          SEE IF EXTENSION DAYS                        
         BNE   EDTPAC4J                                                         
         AHI   R0,-4               TOTAL OF 7 FOR EXT DAYS                      
         LA    R5,4(R5)                                                         
*                                                                               
EDTPAC4J GOTOR VCASHVAL,DMCB,(0,(R5)),(X'40',(R0))                              
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'                                                        
         JL    ERROR                                                            
         BNE   EDTPAC05                                                         
         CLI   WORK,X'87'          PAGE VIEW ALLOWS ZERO                        
         BE    EDTPAC10                                                         
         CLI   WORK,X'88'          CLICK THRU ALLOWS ZERO                       
         BE    EDTPAC10                                                         
         CLI   WORK,X'92'          IMPS OR EIMPS ALLOWS ZERO                    
         BE    EDTPAC10                                                         
         CLI   WORK,X'93'          AIMPS ALLOWS ZERO ALSO                       
         BE    EDTPAC10                                                         
         B     EDTPAC2X            ZERO ENTERED - SET SPECIAL DEL CODE          
*                                                                               
EDTPAC05 CP    DUB,=P'999999999'   MAX IS 999,999,999 FOR PL5                   
         JH    ERROR                                                            
*                                                                               
         CLI   WORK,X'89'          SEE IF EXTENSION DAYS                        
         BNE   EDTPAC10                                                         
         CP    DUB,=P'999'         MAX IS 999                                   
         JH    ERROR                                                            
*                                                                               
EDTPAC10 LA    R5,NEWREC+33        BUILD ELEM                                   
         MVC   ELCODE,WORK                                                      
         ST    R9,WORK+4                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   EDTPAC20            MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE ELEM                      
*                                                                               
EDTPAC20 L     R9,WORK+4           MUST RESTORE R9                              
         XC    WORK+10(10),WORK+10                                              
         MVC   WORK+10(1),WORK     SET ELEMENT CODE                             
         MVI   WORK+11,X'07'                                                    
*                                                                               
         CLI   DUB+1,C'X'          SPECIAL CODE PRESENT?                        
         BE    *+14                                                             
         MVC   WORK+12(5),DUB+3    PACKED NUMBER (PL5)                          
         B     *+10                                                             
         MVC   WORK+12(1),DUB+1    MOVE IN DELETION CODE                        
*                                                                               
         CLI   WORK,X'89'          SEE IF EXTENSION DAYS                        
         BNE   EDTPAC25                                                         
         MVI   WORK+11,X'04'       LENGTH IS 4                                  
         CLI   DUB+1,C'X'          SPECIAL CODE PRESENT?                        
         BNE   EDTPAC24                                                         
         MVC   WORK+12(1),DUB+1    SPECIAL DELETION CODE 'X'                    
         XC    WORK+13(4),WORK+13  JUST IN CASE                                 
         B     EDTPAC25                                                         
*                                                                               
EDTPAC24 MVC   WORK+12(2),DUB+6    PACK DIGITS (PL2)                            
         XC    WORK+14(3),WORK+14  JUST IN CASE                                 
*                                                                               
EDTPAC25 GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
EDTPAC30 J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CSTLNS1  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   PBDUIND,0           RESET CALCULATION FROM SPACE DESP            
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
         LA    R3,INVERR           SET ERROR CODE                               
CDTLNS1  STM   R4,R5,DUB           SAVE PTR/LEN                                 
CDTLNS2  CLI   0(R4),C'0'          VERIFY NUMERIC                               
         BL    CDTLNS3                                                          
         CLI   0(R4),C'9'                                                       
         JH    ERROR                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,CDTLNS2                                                       
*                                                                               
CDTLNS3  DS    0H                                                               
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BNP   CDTL9                                                            
         BCTR  R4,0                                                             
         EX    R4,CPACKLN                                                       
         CP    WORK(5),=P'99999'   MAX LINES OR INCHES                          
         JH    ERROR               THAT CAN BE CARRIED IN PBDUNITS              
         ZAP   PBDUNITS,WORK(5)                                                 
*                                                                               
         MVI   PBDUIND,C'L'                                                     
         LTR   R5,R5                                                            
         BZ    CDTLNS8             NO MORE INPUT                                
         LA    R4,1(RE,R4)         R4 = BYTE PAST NUMBER                        
         CLI   0(R4),C'L'                                                       
         BE    CDTLNS4                                                          
         CLI   0(R4),C'/'                                                       
         BE    CDTLNS5                                                          
         MVI   PBDUIND,C'I'                                                     
         CLI   0(R4),C'I'                                                       
         BE    CDTLNS4                                                          
         CLI   0(R4),C'X'          NEW SAU NNXNN.NN                             
*                                  COLUMNS X INCHES (2 DECIMALS)                
         BNE   CDTLNS3C            NO                                           
         CP    PBDUNITS,=P'14'   MAX COLS =14 (WAS 13 UNTIL 11/23/94)           
         BH    CDTL9               TREAT AS SPACE                               
         CP    PBDUNITS,=P'0'                                                   
         BNH   CDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,*+8              NO MORE INPUT                                
         B     CDTL9               NO MUST BE SPACE                             
         CLC   0(2,R4),=C'FD'                                                   
         BNE   CDTLNS3B                                                         
         CLI   2(R4),C' '                                                       
         BNL   CDTL9               INPUT AFTER FD                               
         L     R5,APUBIO                                                        
         CLI   0(R5),0             SEE IF PUB THERE                             
         BNE   CDTLNS32                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),SVPUBDA                                                
*                                  MUST REREAD PUB                              
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',         X        
               KEY+27,APUBIO,(TERMNAL,DMWORK)                                   
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND PUB                                
*                                                                               
CDTLNS32 LA    R5,33(R5)                                                        
         MVI   ELCODE,X'20'                                                     
         ST    R9,DUB              SAVE R9                                      
         BRAS  RE,NXTELEM                                                       
         BE    CDTLNS3A                                                         
         L     R9,DUB              RESTORE R9                                   
         B     CDTL9               CAN'T FIND PROD ELEM                         
*                                                                               
         USING PUBGENEL,R5                                                      
CDTLNS3A L     R9,DUB              RESTORE R9                                   
         OC    PUBFD,PUBFD         SEE IF I HAVE FD                             
         BZ    CDTL9               NO - TREAT AS SPACE                          
         ZAP   PBDCLMS,PBDUNITS                                                 
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,PUBFD                                                        
         CP    DUB,=P'99999'       MAX IS 999.99 COL INCHES                     
         JH    ERROR                                                            
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE                            
         B     CDTLNS8                                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
CDTLNS3B GOTO1 VCASHVAL,DMCB,(2,0(R4)),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    CDTL9                                                            
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    CDTL9                                                            
         ZAP   PBDCLMS,PBDUNITS                                                 
         L     RE,DMCB+4                                                        
         CVD   RE,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   CDTL9                                                            
         MP    DUB,PBDCLMS                                                      
         CP    DUB,=P'99999'       MAX COLUMN INCHES                            
         BH    CDTL9                                                            
         NI    PBDUIND,X'BF'       MAKE LOWER CASE I                            
         ZAP   PBDUNITS,DUB                                                     
         B     CDTLNS8                                                          
*                                                                               
CDTLNS3C DS    0H                  CHK FOR DECIMAL POINT                        
         CLI   0(R4),C'.'                                                       
         BNE   CDTL9               TREAT AS SPACE                               
         BCT   R5,*+8                                                           
         B     CDTL9               NO MORE INPUT                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'I'                                                       
         BNE   CDTLNS3E                                                         
         BCT   R5,*+8                                                           
         B     CDTLNS8             TREAT NN.I AS NN (NO DECIMALS)               
         B     CDTL9               INPUT AFTER I TREAT AS SPACE                 
*                                                                               
CDTLNS3E ST    R4,DUB                                                           
CDTLNS3F CLI   0(R4),C'I'                                                       
         BE    CDTLNS3G                                                         
         CLI   0(R4),C'0'                                                       
         BL    CDTL9                                                            
         CLI   0(R4),C'9'                                                       
         BH    CDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,CDTLNS3F                                                      
         B     CDTL9               IF DOESN'T END WITH I ASSUME SPACE           
*                                                                               
CDTLNS3G BCTR  R5,0                MUST DECREMENT R5                            
         LTR   R5,R5                                                            
         BNZ   CDTL9               MORE INPUT - TREAT AS SPACE                  
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,0                ADJUST FOR I                                 
         EX    R4,CPACKLN                                                       
         ZAP   DUB,WORK(5)                                                      
         CP    DUB,=P'0'           SEE IF NN.00  INPUT                          
         BE    CDTLNS8             TREAT AS NNI                                 
         XC    WORK(13),WORK       BUILD LINE FOR CASHVAL                       
         ZIC   R5,5(R2)            INPUT LENGHT                                 
         LA    R4,8(R2)                                                         
         CLI   0(R2),20            SEE IF DOING CLE                             
         BNH   CDTLNS3I                                                         
         AHI   R5,-4               ADJUST FOR CLE=                              
         LA    R4,4(R4)            BUMP PAST CLE=                               
*                                                                               
CDTLNS3I BCTR  R5,0                                                             
         BCTR  R5,0                SO I WON'T MOVE THE I                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
         LA    R5,1(R5)            RESTORE FROM THE EX                          
         GOTO1 VCASHVAL,DMCB,(2,WORK),(R5)                                      
         CLI   DMCB,X'FF'                                                       
         BE    CDTL9               IF ERROR TREAT AS SPACE                      
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'99999'                                                    
         JH    ERROR               CAN'T FIT IN PBDUNITS                        
         ZAP   PBDUNITS,DUB                                                     
         NI    PBDUIND,X'BF'       MAKE I LOWER CASE 'I'                        
*                                  INCHES TO 2 DECIMALS                         
         B     CDTLNS8                                                          
*                                                                               
CDTLNS4  DS    0H                                                               
         BCT   R5,*+8              NO MORE INPUT AFTER L OR I                   
         B     CDTLNS8                                                          
         CLI   1(R4),C'/'          '/'  MUST BE NEXT                            
         BNE   CDTL9                                                            
         LA    R4,1(R4)                                                         
*                                                                               
CDTLNS5  DS    0H                                                               
         LA    R4,1(R4)                                                         
*                                                                               
CDTLNS6  DS    0H                                                               
         BCT   R5,*+8              NO MORE INPUT                                
         B     CDTL9                                                            
         ST    R4,DUB              START OF NUMBER                              
CDTLNS6A CLI   0(R4),C'0'                                                       
         BL    CDTL9                                                            
         CLI   0(R4),C'9'                                                       
         BH    CDTL9                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,CDTLNS6A                                                      
         L     RE,DUB              START OF NUMBER                              
         SR    R4,RE               R4 = LENGTH                                  
         BCTR  R4,0                                                             
         EX    R4,CPACKLN                                                       
         CP    WORK(5),=P'999'     MAX COLUMNS IS 999                           
         JH    ERROR               CAN'T FIT IN PDBCLMS                         
         LA    R3,DVSERR           DIVISIBILITY ERROR                           
         ZAP   PBDCLMS,WORK(5)                                                  
         JZ    ERROR                                                            
         ZAP   DUB,PBDUNITS                                                     
         JZ    ERROR                                                            
         DP    DUB,PBDCLMS                                                      
         CP    DUB+8-L'PBDCLMS(L'PBDCLMS),=P'0'                                 
         JNE   ERROR                                                            
*                                  CHECK VS JOB RECORD                          
CDTLNS8  DS    0H                                                               
CDTLNSX  J     EXIT_X                                                           
*                                                                               
CDTL9    DS    0H                                                               
         J     ERROR               NO SPACE FOR CLE                             
*                                                                               
CDTL9B   DS    0H                  NEVER GET HERE                               
         OI    4(R2),X'20'                                                      
         MVC   PBDSPACE(8),8(R2)                                                
         B     CDTLNS8                                                          
*                                                                               
CPACKLN  PACK  WORK(5),0(0,RE)                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113PPBUY12   06/02/20'                                      
         END                                                                    
