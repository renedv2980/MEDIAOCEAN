*          DATA SET PPBUY11    AT LEVEL 108 AS OF 06/02/20                      
*PHASE T41111A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY11 - MAGAZINE BUY/CHA/DEL'                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 03/02/20 SPEC-37363 NO RATE CHANGE FOR FROZEN CLIENTS                    
*                                                                               
* JSAY 08/30/19 SKIP VALIDATION OF EXTENDED ALLOCATIONS                         
*                                                                               
* KWAN 03/31/17 ALLOW MAX RATE OF 8,499,999.99                                  
*                                                                               
* KWAN 07/23/14 ADJUST RATE CHANGE INDICATOR FOR DDLINK UPDATES                 
*                                                                               
* KWAN 04/04/14 ENABLE NEW PLANNED COST ELEMENT FOR MEDIA S (SEARCH)            
*                                                                               
* KWAN 10/11/11 FIX ACTIVITY FOR ZZZ WHEN ALLOCATION IS NOT CHANGED             
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
* SMYE  01/10   ALLOW FOR RETENTION AND DISPLAY OF "S" COST INDICATOR           
*                 IN "FREE" BUYS                                                
*                                                                               
* KWAN 10/21/09 FREE INSERTION, RATE CHANGE INDICATOR ADJUSTMENT                
*                                                                               
* KWAN 08/07/09 FULLY PAID CHANGES FOR CANADIAN BUYS                            
*                                                                               
* KWAN 04/23/09 NO RATE TYPE CHANGE FOR FREE IDESK INSERTIONS                   
*                                                                               
* BOBY 05/00/07 ADD DATE PASSIVE POINTERS                                       
*                                                                               
* KWAN 03/16/07 FIX REGISTER PROBLEM FOR OPTIONAL DATA COMMENT ROUTINE          
*                                                                               
* KWAN 11/30/06 PURCHASE ORDER #                                                
*                                                                               
* KWAN 06/08/06 ALLOW ZERO VALUE PAGE VIEW AND CLICK THRU ELEMENTS              
*                                                                               
* KWAN 12/15/05 DELETING CLEARED INSERTIONS W/ INVOICE DATA VIA ADBUYER         
*                                                                               
* SMYE  12/05   "ADID=XXX.." OPTION FOR AD-ID ALONE (NO NRML JOB CODE)          
*                                                                               
* KWAN 10/03/05 DEL MAT= REPEAT PASSIVE FOR DEL, INS DATE CHG & AD CHG          
*                                                                               
* KWAN 08/19/05 COS2 FACTOR ROUNDING OPTION IN F0 PROFILE                       
*                                                                               
* KWAN 12/13/04 ADBUYER COMMENTS UPLOAD CHANGE INDICATOR FIX                    
*                                                                               
* SMYE 09/04    FOR PBDCDATE, AND PBDMDATE CHANGE NON-WORK DAY DATES            
*                 TO FIRST PRIOR WORK DAY IN LITTLE BRAS ROUTINE                
*                 "CGWRKDT" IF PROFILE SO INDICATES                             
*                                                                               
* SMYE 09/02/04 ADD WEBIO ELEM (X'71') TO CKIOEL ERROR EDITING                  
*                                                                               
* KWAN 03/01/04 ADBUYER CUSTOM COLUMN UPLOAD                                    
*                                                                               
* KWAN 01/15/04 GST AND PST BUG FIX FOR ACTION CHANGE                           
*                                                                               
* KWAN 09/10/03 INVOICE MATCHING STATUSES ELEM FOR AB 2.0                       
*                                                                               
* KWAN 03/05/03 FIX COMMENTS VALIDATION                                         
*                                                                               
* YKAP 09/10/02 ISS= ISSUE NAME                                                 
*                                                                               
* KWAN 07/24/02 FIX ADBUYER ELEM CHANGES                                        
*                                                                               
* KWAN 07/22/02 FIX MASTER/SUB CLIENT RECORD LOCKING BUG                        
*                                                                               
* KWAN 07/19/02 ADBUYER ELEMS, REMOVE ONLY IF PRESENT IN NEWREC                 
*                                                                               
* KWAN 01/09/02 ADBUYER - ROUTINE IN CHG LOGIC, NEED TO HANDLE OTHER            
*               ELEMS, BECAUSE REC GETS REREAD AND CHANGES TO REC MADE          
*               IN PPBUY18 HAS NO EFFECT (SEE CKADBELM IN PPBUY03)              
*                                                                               
* KWAN 11/30/01 ADBUYER - USE T41118 TO PROCESS OPTIONAL DATA FIELDS            
*                                                                               
* KWAN 09/12/01 ADD/CHG SPACE DESCRIPTION (USE STD SPC DESP REC, X'5A')         
*                                                                               
* KWAN 08/16/01 "NO TRAFFIC" OVERRIDES (PBDSTAT BIT IS X'20')                   
*                                                                               
* KWAN 07/16/01 LEGAL WARNINGS (NEW OPTIONAL DATA AND ELEM, X'94')              
*                                                                               
* KWAN 05/02/01 BEFORE CHANGING REC, NEED TO CHECK FOR LOCKS                    
*                                                                               
* KWAN 02/16/01 EDTDT AND EDTCDT, LEAP YEARS CHKS ARE NO GOOD FOR 2001+         
*                                                                               
* KWAN 02/09/01 BUY RECS ARE EXPANED TO 4000 BYTES                              
*                                                                               
* KWAN 01/26/01 EXDATE (EXTENSION DATE), USES SAME LOGIC AS EDTCSHDT            
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
* KWAN 12/01/99 VALIDATING BUYS WITH EXCLUSION CLASS RESTRICTIONS               
*                                                                               
* KWAN 11/23/99 DISALLOW ADDING EMPTY ELEM WHEN BUYING (CAUSE DUMP)             
*                                                                               
* KWAN 07/06/99 CHANGE ELEM (X'24') FOR CT, PV AND IMPS CHANGES                 
*                                                                               
* KWAN 05/20/99 REMOVE "SJ ONLY" FROM IMPRESSION OPTION                         
*                                                                               
* BPLA 5/99     WHEN CHANGING DELETED BUY ONLY CHANGE COMMENTS                  
*               ANY OTHER CHANGE IS IGNORED                                     
*                                                                               
* KWAN 04/12/99 ADD CODES FOR IMPRESSION OPTION (SJ ONLY)                       
*                                                                               
* KWAN 02/25/99 ADD CODES FOR COST FACTOR RECUPS                                
*                                                                               
* KWAN 02/22/99 SET UP KEYWORD TABLE FOR LOOK UPS                               
*                                                                               
* KWAN 01/06/99 ADDED CODES FOR EXTENSION DAYS ELEMENT                          
*               FIXREC SUBROUTINE TO GET RID OF REPEATED CODES                  
*                                                                               
* BPLA 1/99     CHANGES FOR EXDAYS                                              
*                                                                               
* BPLA 8/98     CHANGES FOR LIST BUYING                                         
*                                                                               
* BPLA 3/98     SFH FEATURE OR ALL AGENCIES                                     
*                                                                               
* BPLA 11/97    RECORD CHANGE IN SFH STATUS IN PCHGELEM                         
*                                                                               
* BPLA 11/97    USE CORE RESIDENT GETINS                                        
*                                                                               
* BPLA 10/97    ALLOW SFH=HOLD AND SFH=REL IN OPTIONAL DATA                     
*               (ONLY FOR SJR FOR TESTING)                                      
*                                                                               
* BPLA 5/97     CHANGE TO EDTCDT AND EDTSHIP TO ACCOMDATE NEW                   
*               DATCON (5/97)                                                   
*                                                                               
* BPLA 4/97     ALLOW GST= OVERRIDE IN OPTIONAL DATA                            
*               ALLOW ONLY ONE PST/XX= ENTRY                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41111   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK11X-WORK11D,T41111,RR=RE,CLEAR=YES                           
*                                                                               
         LR    R9,RC                                                            
         USING WORK11D,R9          R9 = A(GLOBAL STORAGE)                       
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          R8 = A(GLOBAL LITERALS)                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         ST    RE,RELOBY11                                                      
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        INIT ADDRESS                                 
*                                                                               
         XC    OLDINS(12),OLDINS                                                
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
MAIN_30  CLI   SVSCRN,X'FC'                                                     
         BNL   GO_BUY                                                           
*                                                                               
         CLI   SVSCRN,X'E8'        LIST SCREEN                                  
         BE    GO_BUYL                                                          
         CLI   SVSCRN,X'E9'        LIST SCREEN - ZZZ                            
         BE    GO_BUYL                                                          
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GO_BUY   CLC   =C'DL',8(R2)                                                     
         BE    DEL                                                              
*                                                                               
         BRAS  RE,P_BUYCHG         PROCESS NEW BUY & CHANGE BUY                 
         JNE   EXIT                                                             
*                                                                               
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'                                                       
         BE    CHG                                                              
*                                                                               
         BRAS  RE,BEXCL            CK FOR PRD EXCLUSION                         
         JNE   EXIT                                                             
         BRAS  RE,EDIODAT                                                       
         BRAS  RE,COM              FIRST DO COMPETIVE BRAND CHECK               
         BRAS  RE,ASC              AUTO SCHEDULE CHECKING                       
         BRAS  RE,UPD              POSSIBLY ADD UPLOAD ELEM                     
*                                                                               
         TM    GLBVALSW,BUYPLCVQ   PLANNED COST ALREADY VALIDATED?              
         BNZ   BUY_30                                                           
         GOTOR PLANCOST,1          ADD PLANNED COST ELEM                        
         JNE   EXIT                                                             
*                                                                               
BUY_30   CLC   =C'ZZZ',BUYPR                                                    
         BNE   *+16                                                             
         BRAS  RE,POL_P1                                                        
         JNE   EXIT                                                             
         B     CHGX                                                             
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
         BRAS  RE,ADDLINE                                                       
         BRAS  RE,MUP              MIGHT NEED TO UPDATE MINIO REC               
         BRAS  RE,ASR              AUTO SPACE RESERVATION                       
*                                                                               
BUY_X    BRAS  RE,FMTTR                                                         
         BRAS  RE,FMTINS           REFORMAT INSERTION DATE/SUBLINE              
         CLC   =C'DL',TRCODE                                                    
         BE    *+12                                                             
         BRAS  RE,FMTRTM                                                        
         BRAS  RE,FMTDTES          FORMAT CLOSE + SALE DATES                    
*                                                                               
         XC    UPID,UPID                                                        
         MVI   UPLSW,0                                                          
         B     ALLDONE                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GO_BUYL  XC    UPID,UPID           LIST BUYING                                  
         MVI   UPLSW,0                                                          
         L     RE,LSTPTR           SET PUB NUMBER                               
         MVC   BPUB,0(RE)                                                       
         MVI   BFREQ,0             SET TO REQUIRE MONTH DAY                     
*                                                                               
         CLC   =C'DL',8(R2)                                                     
         BE    DEL                                                              
*                                                                               
         BRAS  RE,P_BUYLST         PROCESS LIST BUYING                          
         JNE   EXIT                                                             
*                                                                               
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'                                                       
         BE    CHG                                                              
*                                                                               
         BRAS  RE,BEXCL            CK FOR PRD EXCLUSION                         
         JNE   EXIT                                                             
         BRAS  RE,EDIODAT                                                       
         BRAS  RE,COM              COMPETITIVE BRAND CHECK                      
         BRAS  RE,ASC              AUTOMATIC SCHEDULE CHECK                     
*                                                                               
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   *+16                                                             
         BRAS  RE,POL_P1                                                        
         JNE   EXIT                                                             
         B     CHGX                                                             
*                                                                               
         BRAS  RE,ADDLINE                                                       
         BRAS  RE,ASR              AUTOMATIC SPACE RESERVATION                  
*                                                                               
BUYLX    BRAS  RE,FMTTR                                                         
         BRAS  RE,FMTINS                                                        
         CLC   =C'DL',TRCODE       DELETE?                                      
         BE    *+12                                                             
         BRAS  RE,FMTRTM                                                        
         BRAS  RE,LMTDTES          FORMAT DATES ON LIST SCR                     
         BRAS  RE,NXTTR                                                         
         BNZ   GO_BUYL                                                          
         B     ALLDONE                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHG      BRAS  RE,P_CHANGE         PROCESS BUY CHANGE                           
         JNE   EXIT                                                             
*                                                                               
         CLI   DDLINKSW,0          ADBUYER?                                     
         JE    CHGPUT5K                                                         
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   CHGPUT5K                                                         
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         JZ    CHGPUT5K                                                         
         J     CHGPUT5M            NO NEED TO CHECK ZZZ                         
*                                                                               
CHGPUT5K CLC   =C'ZZZ',BUYPR                                                    
         BNE   *+16                                                             
         BRAS  RE,POL_P1                                                        
         JNE   EXIT                                                             
         B     CHGX                                                             
*                                                                               
CHGPUT5M BRAS  RE,MUP              NEED TO UPDATE UPLOAD MINIO REC              
*                                                                               
         TM    GENBYSW1,ADJCO2$Q   ADJUST COS2 RATE TO BUY RATE?                
         JZ    CHGPUT5P                                                         
         TM    GLBVALSW,BUYCOS2Q   COS2 $ ALREADY VALIDATED?                    
         BNZ   *+16                                                             
         GOTOR VAL_COS2,1          DO COS2 $ COPY (COPY FROM RATE)              
         JNE   ERROR                                                            
*                                                                               
CHGPUT5P TM    CHGIND1,X'08'       DATE CHANGE?                                 
         BZ    CHGPUT5                                                          
         TM    GLBVALSW,BUYPO#VQ   PURCHASE ORDER # ALREADY VALIDATED?          
         BNZ   *+16                                                             
         GOTOR VALBYPO#,1          DO PURCHASE ORDER # LOOKUP                   
         JNE   ERROR                                                            
         GOTOR VT41103,DMCB,(RC),(RA),CHGBRECQ                                  
         JNE   EXIT                                                             
         BRAS  RE,TESTERR                                                       
         TM    GLBVALSW,BUYPO#VQ   PURCHASE ORDER # ALREADY VALIDATED?          
         BNZ   CHGPUT5                                                          
         GOTOR VALBYPO#,1          DO PURCHASE ORDER # LOOKUP                   
         JNE   ERROR                                                            
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
         GOTOR VT41103,DMCB,(RC),(RA),EXCGET_Q  GET FX RATE/AMOUNT              
*                                                                               
         BRAS  RE,CHG_PUTR         UPDATE CHANGED BUY RECORD                    
*                                                                               
CHGX     MVC   NEWREC(25),REC      MOVE KEY TO NEWREC FOR FMTINS                
         CLI   SVSCRN,X'FC'                                                     
         BNL   BUY_X                                                            
         CLI   SVSCRN,X'E8'        LIST BUYING                                  
         BE    BUYLX                                                            
         CLI   SVSCRN,X'E9'        LIST BUYING                                  
         BE    BUYLX                                                            
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DEL      BRAS  RE,FNDINS           RETRIEVE INSERTION                           
         BRAS  RE,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
*                                                                               
         GOTOR CKRATE,2            CHECK RATE BEFORE DELETION                   
         JNE   EXIT                                                             
         GOTOR VT41103,DMCB,(RC),(RA),CKIDKC_Q  CK FOR IDESK CONTROL            
         JNE   EXIT                                                             
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBNVELQ      INVOICE ELEM PRESENT?                        
         BRAS  RE,NXTELEM                                                       
         BNE   DEL10                                                            
         CLI   DDLINKSW,0          ADBUYER?                                     
         BE    DEL05                                                            
         LA    R5,REC+33           CK FOR CLEARANCE STATUS                      
         MVI   ELCODE,X'25'        PAY ELEM                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DEL05                                                            
         OC    2(3,R5),2(R5)       PAID DATE PRESENT?                           
         BZ    *-14                                                             
         B     DEL10               CLEARED - ALLOW DELETION                     
*                                                                               
DEL05    XC    BUYMSG,BUYMSG                                                    
         LHI   R3,DLINVERR         CANNOT DELETE INSERTION W/ INV DATA          
         BRAS  RE,GET_ETXT                                                      
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,BUYTR1H                                                       
         J     EXIT                                                             
*                                                                               
DEL10    MVI   DMOUTBTS,0          RESET FOR NO ERROR TESTS                     
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         BRAS  RE,PRT_READ                                                      
         BRAS  RE,CHECK                                                         
         OI    KEY+25,X'80'        SET DELETED IND                              
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERROR                                                            
         BRAS  RE,PRT_WRIT                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
         MVI   KEY+3,X'21'         DELETE CLT/PUB POINTER                       
         MVC   KEY+7(6),REC+10     PUB                                          
         MVC   KEY+13(3),REC+7     PRD                                          
         BRAS  RE,PRT_READ                                                      
         BRAS  RE,CHECK                                                         
         OI    KEY+25,X'80'                                                     
*                                                                               
         BRAS  RE,PRT_WRIT         LOCK IS CHECKED EARLIER                      
         BRAS  RE,CHECK                                                         
         BRAS  RE,PRT_GETR                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
         OI    REC+27,X'80'                                                     
         LA    R7,PBDDATE-NEWREC+REC                                            
         MVC   0(3,R7),BTODAY      SET CHANGE DATE IN REC                       
*                                                                               
         LA    R7,PBDBUYER-NEWREC+REC                                           
         MVC   0(3,R7),BUYNM       SET BUYER ID IN REC                          
*                                                                               
         CLI   BUYNM,C'*'          IF STARTS WITH * - DON'T MOVE *              
         BNE   *+10                                                             
         MVC   0(3,R7),BUYNM+1                                                  
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),DELINS_Q                                  
*                                                                               
         BRAS  RE,CKEIOELM         CK EIO ELEM                                  
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BE    *+16                                                             
         BRAS  RE,PRT_PUTR         LOCK IS CHECKED EARLIER                      
         BRAS  RE,CHECK                                                         
         BRAS  RE,CKMATPTR         CK FOR MAT= REPEAT PASSIVE PTR               
*                                                                               
         MVI   DMOUTBTS,X'FD'      RESET DMOUTBTS                               
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),CLEARBRQ                                  
         BRAS  RE,TESTERR                                                       
         CLC   =C'ZZZ',BUYPR       GO DELETE PASSIVE POINTERS                   
         BNE   *+16                                                             
         BRAS  RE,POL_P2           DO ASR FOR ZZZ BUYS                          
         JNE   EXIT                                                             
         B     CHGX                                                             
*                                                                               
         BRAS  RE,ASR              AUTO SPACE RESERVATION                       
*                                                                               
         BRAS  RE,MUP              CHK MINIO UPLOAD RECORD                      
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
EDTSP    LR    R0,RE               EDIT SPACE DESCRIPTION                       
         LA    R3,MSSNGERR                                                      
         BRAS  RE,BUMPFLD                                                       
         CLI   5(R2),0                                                          
         JE    ERROR                                                            
         LA    R3,INVERR                                                        
         CLI   8(R2),C' '                                                       
         JE    ERROR                                                            
         MVC   PBDSPACE,8(R2)                                                   
         MVI   X,C' '                                                           
         MVC   X+1(16),X                                                        
         OC    X(17),8(R2)                                                      
         BRAS  RE,CKSPDESP         CHECK AGAINST SPACE DESP RECORD              
         JE    EXIT_VRE                                                         
         CHI   R3,SPDSPERR         SPACE DESCRIPTION NOT FOUND?                 
         JE    ERROR                                                            
         OC    PBDJOB,PBDJOB                                                    
         JZ    EXIT_VRE                                                         
         L     R4,AJOBIO                                                        
         CLC   X(17),PJOBSPC-PJOBREC(R4)                                        
         JE    EXIT_VRE                                                         
         OI    WARN,X'80'          SPACE WARNING                                
         J     EXIT_VRE                                                         
*                                                                               
*                                                                               
EDTCOM   LR    R0,RE                                                            
         BRAS  RE,EDTC                                                          
         CLI   ERRAREA,0           ERROR?                                       
         JNE   EXXMOD                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTDT    LR    R0,RE               DATES EDIT                                   
         LA    R3,INVDTERR                                                      
         BRAS  RE,BUMPFLD                                                       
         XC    DUB,DUB                                                          
         CLI   5(R2),0             ANY INPUTS?                                  
         JE    EDTDTX                                                           
         GOTO1 VDATVAL,DMCB,(1,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR                                                            
         GOTO1 VDATCON,(R1),(0,WORK),(3,DUB)                                    
         MVC   DUB(1),NEWREC+16    INSERTION YEAR                               
         CLC   DUB+1(1),NEWREC+17  INPUT MONTH > INSERTION MONTH?               
         JNH   EDTDT5                                                           
         IC    RE,DUB                                                           
         BCTR  RE,0                USE PREVIOUS YEAR                            
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDIODAT  OC    PBDIODAT,PBDIODAT   INSERTION ORDER DATE EDIT                    
         BZR   RE                                                               
         CLI   PBDIODAT,X'FF'      NONE?                                        
         BER   RE                                                               
         LA    R3,IODERR1                                                       
         CLC   PBDIODAT,BTODAY     BEFORE TODAY?                                
         JL    ERROR                                                            
         LA    R3,IODERR2                                                       
         J     EDIOD4              ** SKIP DATE CHECKING BELOW **               
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
         BRAS  RE,BUMPFLD                                                       
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
FMTINS   LR    R0,RE                                                            
         GOTO1 VFMTINS,DMCB,NEWREC                                              
         LR    RE,R0                                                            
         J     TESTERR                                                          
*                                                                               
FMTTR    LR    R0,RE                                                            
         GOTO1 VFMTTR,DMCB,REC     REFORMAT FIELD AT TRADDR                     
         LR    RE,R0                                                            
         J     TESTERR                                                          
         EJECT                                                                  
*                                                                               
FMTDTES  LR    R0,RE                                                            
         L     R2,TRADDR                                                        
         LHI   RF,5                                                             
         BRAS  RE,BUMPFLDS         TO CLOSING DATE                              
         CLI   5(R2),0                                                          
         JNE   FMTDTES2                                                         
         OC    PBDCDATE,PBDCDATE                                                
         JZ    FMTDTES2                                                         
         GOTO1 VDATCON,DMCB,(3,PBDCDATE),(7,8(R2))                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
FMTDTES2 BRAS  RE,BUMPFLD          POINT TO SALE DATE                           
         CLI   5(R2),0                                                          
         JNE   FMTDTES3                                                         
         OC    PBDSDATE,PBDSDATE                                                
         JZ    FMTDTES3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDSDATE),(7,8(R2))                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
FMTDTES3 BRAS  RE,BUMPFLD          POINT TO MATERIALS CLOSING DATE              
         CLI   5(R2),0                                                          
         JNE   FMTDTESX                                                         
         OC    PBDMDATE,PBDMDATE                                                
         JZ    FMTDTESX                                                         
         GOTO1 VDATCON,DMCB,(3,PBDMDATE),(7,8(R2))                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
FMTDTESX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
LMTDTES  LR    R0,RE               FORMAT DATES ON LIST SCR                     
         L     R2,TRADDR                                                        
         LHI   RF,6                                                             
         BRAS  RE,BUMPFLDS           TO MAT CLOSING DATE                        
         CLI   5(R2),0                                                          
         JNE   LMTDTES2                                                         
         OC    PBDMDATE,PBDMDATE                                                
         JZ    LMTDTES2                                                         
         GOTO1 VDATCON,DMCB,(3,PBDMDATE),(7,8(R2))                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
LMTDTES2 BRAS  RE,BUMPFLD          PAST MAT CLOSING DATE                        
         BRAS  RE,BUMPFLD          PAST PUB NAME                                
         BRAS  RE,BUMPFLD          PAST CLOSING DATE ANNO                       
         CLI   5(R2),0                                                          
         JNE   LMTDTES3                                                         
         OC    PBDCDATE,PBDCDATE                                                
         JZ    LMTDTES3                                                         
         GOTO1 VDATCON,DMCB,(3,PBDCDATE),(7,8(R2))                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
LMTDTES3 BRAS  RE,BUMPFLD          PAST CLOSING DATE                            
         BRAS  RE,BUMPFLD          PAST ON-SALE DATE ANNO                       
         CLI   5(R2),0                                                          
         JNE   LMTDTESX                                                         
         OC    PBDSDATE,PBDSDATE                                                
         JZ    LMTDTESX                                                         
         GOTO1 VDATCON,DMCB,(3,PBDSDATE),(7,8(R2))                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATED                                    
LMTDTESX LR    RE,R0                                                            
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
         J     G_T41103                                                         
RTSUB    LA    RF,RTLOOKUQ                                                      
         J     G_T41103                                                         
FSILOOK  LA    RF,FSILOOKQ                                                      
         J     G_T41103                                                         
ADDLINE  LA    RF,ADDINSRQ                                                      
         J     G_T41103                                                         
FNDINS   LA    RF,FINDINSQ                                                      
         J     G_T41103                                                         
ASR      LA    RF,AUTOSRKQ                                                      
         J     G_T41103                                                         
UPD      LA    RF,UPLELEMQ                                                      
         J     G_T41103                                                         
ASC      LA    RF,AUTOSCKQ                                                      
         J     G_T41103                                                         
COM      LA    RF,COMPBCKQ                                                      
         J     G_T41103                                                         
MUP      LA    RF,MINIOULQ                                                      
         J     G_T41103                                                         
MK_POREC LA    RF,MARKPORQ                                                      
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
         CLC   DUB(6),SVESTJOB      INPUT IS SAME AS EST AD CODE?               
         JE    EDTJOB4                                                          
         LA    R3,JOBERR4                                                       
         J     ERROR                                                            
EDTJOB4  GOTOR VT41103,DMCB,(RC),(RA),EDTADCDQ                                  
         LR    RE,R0                                                            
         J     TESTERR                                                          
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
NXTELEM  ZIC   R1,1(R5)                                                         
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
         CLI   SVSCRN,X'EA'        TEST LIST BUYING                             
         BL    ALLDONE2                                                         
         LA    R2,BUYPBH           PUT CURSOR TO PUB                            
         B     EXIT                                                             
*                                                                               
ALLDONE2 MVC   BUYMSGH+8+L'CMPMSG+1(L'LSTMSG),LSTMSG                            
         LA    R2,BUYTR1H          PUT CURSOR TO TR                             
         B     EXIT                                                             
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         BRAS  RE,GET_ETXT                                                      
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
         ST    R2,ADBERRFD         ADDRESS OF ERROR FIELD                       
         CLI   ERRAREA,X'FF'       ERROR OCCURED?                               
         JE    SETCCNEQ                                                         
         J     SETCCEQ                                                          
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CGWRKDT  LR    R0,RE                                                            
         CLI   BYPROF+10,C'Y'      NEED TO ADJUST TO PRIOR WORKDAY ?            
         JNE   CGWRKDTX                                                         
         GOTOR VPPWKDAY,DMCB,(NATION,DUB),DUB,ACOMFACS                          
CGWRKDTX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET_ETXT LR    R0,RE               R3 HAS ERROR NUMBER                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PRT_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_WRIT LR    R0,RE                                                            
         MVC   COMMAND,=C'DMWRT '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RSEQ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRSEQ'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_ADD_ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMADD '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PRT_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PRT_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_ADDR LR    R0,RE                                                            
         MVC   COMMAND,=C'ADDREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_PUTR LR    R0,RE                                                            
         MVC   COMMAND,=C'PUTREC'                                               
*                                                                               
PRT_DFIL LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE ',           +        
               (RF),AREC,(TERMNAL,DMWORK)                                       
*                                                                               
EXIT_VRE LR    RE,R0               EXIT VIA SAVED RE                            
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P_BUYCHG NTR1  BASE=*,LABEL=*      PROCESS BUY CHANGE                           
*                                                                               
         BRAS  RE,BLDREC                                                        
         CLI   8(R2),C'C'                                                       
         BNE   P_BC20                                                           
         BRAS  RE,FNDINS                                                        
         TM    PBDSTAT-NEWREC+REC,X'20'                                         
         BZ    *+8                                                              
         OI    PBDSTAT,X'20'       RETAIN OLD RECORD'S STATUS                   
         MVC   PBDGST,PBDGST-NEWREC+REC                                         
         CLI   DDLINKSW,C'C'       ADBUYER CHANGE?                              
         BNE   P_BC20                                                           
         MVC   PBDBFD,PBDBFD-NEWREC+REC                                         
*                                                                               
P_BC20   BRAS  RE,EDTINS           EDIT INSERTION DATE                          
         BRAS  RE,EDTJOB           EDIT JOB                                     
         BRAS  RE,EDTSP            EDIT SPACE DESC                              
         BRAS  RE,EDTRTM           EDIT RATE                                    
         JNE   EXIT                                                             
         BRAS  RE,EDTDT            EDIT CLOSE DATE                              
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDCDATE,DUB                                                     
         BRAS  RE,EDTDT            EDIT ON SALE DATE                            
         MVC   PBDSDATE,DUB                                                     
         BRAS  RE,EDTDT            EDIT MATERIALS CLOSING DATE                  
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDMDATE,DUB                                                     
         BRAS  RE,EDTCOM                                                        
         BRAS  RE,RTSUB                                                         
         BRAS  RE,FSILOOK                                                       
*                                                                               
         CLI   SVESPROF+28,C'C'    'C' RATE ESTIMATE?                           
         BNE   P_BC40                                                           
         CLI   PBDCOSIN,C'C'       'C' RATE BUY?                                
         BE    P_BC30                                                           
         CLI   PBDCOSIN,C' '                                                    
         BNE   P_BC_ER1            RATE ERROR                                   
         MVI   PBDCOSIN,C'C'                                                    
*                                                                               
P_BC30   L     R2,TRADDR                                                        
         CLI   8(R2),C'C'          CHANGE?                                      
         BE    P_BC34                                                           
         ZAP   PBDCD,=P'0'         ZERO CD                                      
         XC    PBDTAX,PBDTAX       NO TAX                                       
         B     P_BC50                                                           
*                                                                               
P_BC34   ZAP   PBDCD,=P'1'         WILL BE CHANGED TO ZERO                      
         MVC   PBDTAX,=X'000001'   WILL BE CHANGED TO ZERO                      
         B     P_BC50                                                           
P_BC40   CLI   PBDCOSIN,C'C'       'C' RATE EST FOR 'C' RATE BUYS?              
         BE    P_BC_ER1                                                         
P_BC50   OC    PBDCDATE,PBDCDATE                                                
         BNZ   P_BC60                                                           
         CLI   SVAGPROF+24,C'R'    REQUIRE CLOSE DATE                           
         BE    P_BC_ER2                                                         
         CLI   SVAGPROF+24,C'B'    OR BOTH                                      
         BE    P_BC_ER2                                                         
*                                                                               
P_BC60   OC    PBDMDATE,PBDMDATE                                                
         BNZ   P_BC_X                                                           
         CLI   SVAGPROF+24,C'M'    REQUIRE MATERIALS CLOSE DATE?                
         BE    P_BC_ER3                                                         
         CLI   SVAGPROF+24,C'B'    OR BOTH?                                     
         BE    P_BC_ER3                                                         
*                                                                               
P_BC_X   J     SETCCEQ                                                          
*                                                                               
P_BC_ER1 LA    R3,INVRTERR                                                      
         LHI   RF,4                                                             
         B     P_BC_ERR                                                         
*                                                                               
P_BC_ER2 LA    R3,MSSNGERR                                                      
         LHI   RF,5                                                             
         B     P_BC_ERR                                                         
*                                                                               
P_BC_ER3 LA    R3,MSSNGERR                                                      
         LHI   RF,7                                                             
*                                                                               
P_BC_ERR L     R2,TRADDR                                                        
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P_BUYLST NTR1  BASE=*,LABEL=*      PROCESS LIST BUYING                          
*                                                                               
         BRAS  RE,BLDREC                                                        
         CLI   8(R2),C'C'                                                       
         BNE   *+8                                                              
         BRAS  RE,FNDINS                                                        
         LA    R3,OLDINS                                                        
         BRAS  RE,TSTDATA                                                       
         BRAS  RE,EDTINS                                                        
         BRAS  RE,BUMPFLD                                                       
         LA    R3,OLDJOB                                                        
         BRAS  RE,TSTDATA                                                       
         BRAS  RE,EDTJOB                                                        
*                                                                               
         CLI   5(R2),0             AD CODE ENTERED?                             
         BE    *+14                                                             
         CLC   =C'NONE',8(R2)      IGNORE ENTERED AD CODE DATA?                 
         BNE   *+12                                                             
         LA    R3,OLDLNS                                                        
         BRAS  RE,TSTDATA                                                       
*                                                                               
         BRAS  RE,EDTSP                                                         
         BRAS  RE,EDTRTM                                                        
         JNE   EXIT                                                             
         BRAS  RE,EDTDT            MAT CLOSING DATE                             
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDMDATE,DUB                                                     
         BRAS  RE,BUMPFLD          PAST PUB NAME                                
         BRAS  RE,BUMPFLD          PAST CLOSING DATE ANNO                       
         BRAS  RE,EDTDT            CLOSING DATE                                 
         BRAS  RE,CGWRKDT          NON-WORK DAY TEST AND ADJUST                 
         MVC   PBDCDATE,DUB                                                     
         BRAS  RE,BUMPFLD          PAST ON-SALE DATE ANNO                       
         BRAS  RE,EDTDT            ON-SALE DATE                                 
         MVC   PBDSDATE,DUB                                                     
         BRAS  RE,RTSUB                                                         
         BRAS  RE,FSILOOK          DO FSI LOOK-UP                               
*                                                                               
         CLI   SVESPROF+28,C'C'    'C' RATE ESTIMATE?                           
         BNE   P_BL30                                                           
         CLI   PBDCOSIN,C'C'       'C' RATE BUY?                                
         BE    *+16                                                             
         CLI   PBDCOSIN,C' '                                                    
         BNE   P_BL_ER1                                                         
         MVI   PBDCOSIN,C'C'                                                    
*                                                                               
         L     R2,TRADDR                                                        
         CLI   8(R2),C'C'          CHANGE?                                      
         BE    P_BL20                                                           
         ZAP   PBDCD,=P'0'         SET CD TO ZERO                               
         XC    PBDTAX,PBDTAX                                                    
         B     P_BL40                                                           
*                                                                               
P_BL20   ZAP   PBDCD,=P'1'         WILL BE CHANGED TO ZERO                      
         MVC   PBDTAX,=X'000001'   WILL BE SET TO ZERO                          
         B     P_BL40                                                           
*                                                                               
P_BL30   CLI   PBDCOSIN,C'C'       'C' RATE EST FOR 'C' RATES?                  
         BE    P_BL_ER1                                                         
*                                                                               
P_BL40   OC    PBDMDATE,PBDMDATE                                                
         BNZ   P_BL_X                                                           
         CLI   SVAGPROF+24,C'M'    MATERIALS CLOSING REQUIRED?                  
         BE    P_BL_ER2                                                         
         CLI   SVAGPROF+24,C'B'    OR BOTH                                      
         BE    P_BL_ER2                                                         
*                                                                               
P_BL_X   J     SETCCEQ                                                          
*                                                                               
P_BL_ER1 LA    R3,INVRTERR                                                      
         LHI   RF,5                                                             
         B     P_BL_ERR                                                         
*                                                                               
P_BL_ER2 LA    R3,MSSNGERR                                                      
         LHI   RF,6                                                             
*                                                                               
P_BL_ERR L     R2,TRADDR                                                        
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
TSTDATA  SR    R4,R4               SET DEFAULT DATA FOR EMPTY INPUT FLD         
         IC    R4,0(R2)                                                         
         AR    R4,R2               POINT TO NEXT FIELD                          
         CLI   5(R4),0             DATA ENTERED?                                
         JE    *+10                                                             
         ST    R4,0(R3)                                                         
         BR    RE                                                               
         OC    0(4,R3),0(R3)       HAS DATA BEFORE?                             
         BZR   RE                                                               
         SR    R1,R1               SET TO MOVE DATA TO FIELD                    
         IC    R1,0(R4)                                                         
         AHI   R1,-5               4 + 1 FOR EXECUTE                            
         L     R3,0(R3)            SET DATA ADDRESS                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),4(R3)       LAST 4 CNTRL BYTES + DATA                    
         FOUT  (R4)                                                             
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P_CHANGE NTR1  BASE=*,LABEL=*      PROCESS BUY CHANGE                           
*                                                                               
         BRAS  RE,FNDINS                                                        
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         MVC   SVGROSS(12),GROSS                                                
         MVI   MATSW,0             MATCHED STATUS BYTE                          
*                                                                               
         TM    PBDSTAT-NEWREC+REC,X'40'                                         
         BZ    P_CHG10             NOT MATCHED - SKIP PAID CHECK                
         OI    MATSW,X'40'         MATCHED                                      
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
P_CHG04  BRAS  RE,NXTELEM          PAY ELEM FOUND?                              
         BNE   P_CHG10                                                          
         OC    2(3,R5),2(R5)       PAID?                                        
         BZ    P_CHG04                                                          
         OI    MATSW,X'80'         PAID                                         
*                                                                               
P_CHG10  LA    R7,PBDDATE-NEWREC+REC                                            
         CLC   0(3,R7),BTODAY                                                   
         BE    P_CHG14                                                          
         TM    REC+(PBUYCNTL-PBUYREC),X'80'                                     
         BNZ   *+10                                                             
         MVC   0(3,R7),BTODAY                                                   
         MVI   PBDDTIND-NEWREC+REC,0                                            
         MVI   PBDDTIN2-NEWREC+REC,0                                            
         MVI   PBDDTIN3-NEWREC+REC,0                                            
*                                                                               
P_CHG14  CLI   DDLINKSW,C'C'       ADBUYER CHANGE INSERTION UPLOAD?             
         BE    P_CHG18                                                          
         MVI   CHGIND1,0           NEED TO CLEAR FOR LIST BUYING                
         MVI   CHGIND2,0                                                        
         MVI   CHGIND3,0                                                        
         MVI   CHGIND4,0                                                        
*                                                                               
P_CHG18  TM    PBUYCNTL-NEWREC+REC,X'80'                                        
         BZ    P_CHG24                                                          
         CLI   BYPROF+5,C'Y'       CHANGE ALLOWED?                              
         BE    P_CHG30             SKIP TO COMMENT CHANGE LOGIC                 
         DC    H'0'                                                             
*                                                                               
P_CHG24  LA    R7,PBDFREQ-NEWREC+REC                                            
         MVC   0(1,R7),PBDFREQ                                                  
         LA    R7,PBDJOB-NEWREC+REC                                             
         CLC   PBDJOB,0(R7)                                                     
         BE    P_CHG24H                                                         
         OC    0(6,R7),0(R7)       AD CODE PRESENT IN REC?                      
         BNZ   *+12                                                             
         OI    CHGIND3,X'40'       SET AD CODE ADDED                            
         B     *+8                                                              
         OI    CHGIND2,X'08'                                                    
         MVC   0(6,R7),PBDJOB                                                   
         OC    PBDJOB,PBDJOB                                                    
         BNZ   P_CHG24H                                                         
         MVI   ELCODE,X'70'        NON-WEB INSERTION ORDER ELEM                 
         BRAS  RE,CKIOEL                                                        
         BE    P_CHGE_1                                                         
         MVI   ELCODE,X'71'        WEB INSERTION ORDER ELEM                     
         BRAS  RE,CKIOEL                                                        
         BE    P_CHGE_1                                                         
*                                                                               
P_CHG24H LA    R7,PBDBUYER-NEWREC+REC                                           
         MVC   0(3,R7),BUYNM       SET CURRENT BUYER ID IN REC                  
         CLI   BUYNM,C'*'          STARTS WITH C'*'?                            
         BNE   *+10                                                             
         MVC   0(3,R7),BUYNM+1                                                  
         CLC   REC+16(3),NEWREC+16 NEW INSERTION DATE?                          
         BE    P_CHG24M                                                         
*                                                                               
* NOTE THAT DATE CHANGES ON MATCHED INS. WILL BE CHECKED IN PPBUY00             
*                                                                               
         BRAS  RE,FMTTR            FORMAT TR CODE TO TEST BILLED/PAID           
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BE    P_CHGE_2                                                         
         MVI   8(RE),C'C'          RESET TO CHANGE                              
         BRAS  RE,COM              COMPETIVE BRAND CHECK                        
         BRAS  RE,ASC              AUTO SCHEDULE CHECK                          
         OI    CHGIND1,X'08'       DATE CHANGE                                  
*                                                                               
P_CHG24M LA    R7,PBDRLIND-PBUYREC+REC RATE LOOK IND AND DATE                   
         MVC   0(4,R7),PBDRLIND                                                 
         CLI   PBDBFD-NEWREC+REC,C'T'                                           
         BE    *+16                YES, TEST BUY, CAN MAKE LIVE                 
         CLI   PBDBFD,C'T'                                                      
         BNE   P_CHG26                                                          
         B     P_CHGE_3                                                         
*                                                                               
         CLI   PBDBFD,C'T'         TEST INSERTION (NEWREC)?                     
         BE    P_CHG26                                                          
         TM    SVCLPROF+30,X'02'   FROZEN CLIENT?                               
         BNO   P_CHG24U                                                         
         TM    SVCLPROF+30,X'10'   FROZEN WITH DATE ?                           
         BNO   P_CHGE_4                                                         
*                                                                               
         TM    SVCLPROF+27,X'08'   LOCK THIS MONTH AND ALL FORWARD?             
         BO    P_CHG24P                                                         
         TM    SVCLPROF+27,X'04'   LOCK THIS MONTH AND ALL PRIOR?               
         BO    P_CHG24T                                                         
         TM    SVCLPROF+27,X'02'   LOCK THIS MONTH ONLY?                        
         BO    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
         CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BNE   P_CHG24U                                                         
         B     P_CHGE_5            NO BUYING FOR THIS MONTH                     
*                                                                               
P_CHG24P CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BL    P_CHG24U                                                         
         B     P_CHGE_5            NO BUYING FOR THIS MONTH & FORWARD           
*                                                                               
P_CHG24T CLC   PBUYKDAT(2),SVCLPROF+28                                          
         BH    P_CHG24U                                                         
         B     P_CHGE_5            NO BUYING FOR THIS MONTH & PRIOR             
*                                                                               
P_CHG24U TM    SVESPROF+29,X'80'   TEST ESTIMATE?                               
         BNZ   P_CHGE_3                                                         
*                                                                               
         BRAS  RE,COM              COMPETIVE BRAND CHECK                        
         BRAS  RE,ASC              AUTO SCHEDULE CHECK                          
*                                                                               
         OI    CHGIND3,X'04'       MADE LIVE BIT                                
         MVI   PBDDTIND-NEWREC+REC,0                                            
         MVI   PBDDTIN2-NEWREC+REC,0                                            
         MVI   PBDDTIN3-NEWREC+REC,0                                            
         MVC   PBDBUYDT-NEWREC+REC(3),BTODAY                                    
*                                                                               
         MVI   ELCODE,X'24'        DELETE OLD X'24' ELEMS                       
P_CHG25H LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   P_CHG26                                                          
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
         B     P_CHG25H                                                         
*                                                                               
P_CHG26  LA    R7,PBDBFD-NEWREC+REC                                             
         MVC   0(1,R7),PBDBFD                                                   
         LA    R7,PBDSPACE-NEWREC+REC                                           
         MVC   WORK(17),PBDSPACE                                                
         MVC   WORK+17(17),0(R7)                                                
         OC    WORK(34),SPACES                                                  
         CLC   WORK(17),WORK+17    SPACE IS CHANGED?                            
         BE    P_CHG26K                                                         
         CLI   WORK+17,C'*'        OLD SPACE STARTS WITH C'*'?                  
         BNE   P_CHG26D                                                         
         CLI   WORK,C'*'           NEW SPACE STARTS WITH C'*'?                  
         BE    P_CHG26F                                                         
         CLI   WORK,C'#'           NEW SPACE STARTS WITH C'#'?                  
         BE    P_CHG26F                                                         
         BRAS  RE,COM              COMPETIVE BRAND CHECK                        
         BRAS  RE,ASC              AUTO SCHEDULE CHECK                          
         B     P_CHG26F                                                         
*                                                                               
P_CHG26D CLI   WORK+17,C'#'        OLD SPACE STARTS WITH C'#'?                  
         BNE   P_CHG26F                                                         
         CLI   WORK,C'#'           NEW SPACE STARTS WITH C'#'?                  
         BE    P_CHG26F                                                         
         CLI   WORK,C'*'           NEW SPACE STARTS WITH C'*'?                  
         BE    P_CHG26F                                                         
         BRAS  RE,COM              COMPETIVE BRAND CHECK                        
         BRAS  RE,ASC              AUTO SCHEDULE CHECK                          
*                                                                               
P_CHG26F MVC   0(L'PBDSPACE,R7),PBDSPACE                                        
         OI    CHGIND1,X'10'       SPACE DESCRIPTION CHANGE                     
         TM    MATSW,X'40'                                                      
         BNZ   P_CHGE_6                                                         
*                                                                               
P_CHG26K LA    R7,PBDCOSIN-NEWREC+REC                                           
         CLC   PBDCOSIN(7),0(R7)   COST FIELD + INDICATORS                      
         BE    P_CHG27                                                          
         TM    MATSW,X'C0'         MATCHED AND PAID?                            
         BM    P_CHGE_6            MIXED - MUST BE MATCHED/NOT PAID             
*                                                                               
         GOTOR CKRATE,1            CHECK RATE BEFORE CHANGING                   
         JNE   EXIT                                                             
*                                                                               
         CLI   DDLINKSW,C'C'       DDLINK CHANGE MODE?                          
         JNE   P_CHG26M                                                         
         CLC   PBDCOSIN(1),0(R7)   COST INDICATOR CHANGED?                      
         JNE   P_CHG26M                                                         
         TM    ABUPLDSW,RATEMAPQ   RATE MAP CODE PRESENT?                       
         JZ    P_CHG27                                                          
*                                                                               
P_CHG26M MVC   0(7,R7),PBDCOSIN                                                 
         OI    CHGIND1,X'40'       COST CHANGE                                  
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
P_CHG27  LA    R7,PBDCTYP-NEWREC+REC                                            
         CLC   PBDCTYP(1),0(R7)                                                 
         BE    P_CHG27K                                                         
*                                                                               
         CLI   DDLINKSW,0          ADBUYER OR IDESK?                            
         BE    P_CHG27F                                                         
         CP    PBDCOS,=P'0'        FREE INSERTION?                              
         BNE   P_CHG27F                                                         
         CP    PBDCOS,REC+(PBDCOS-PBUYREC)(L'PBDCOS)                            
         BE    P_CHG27K            RATE IS FREE, NOT CHANGED                    
*                                                                               
P_CHG27F TM    MATSW,X'C0'                                                      
         BM    P_CHGE_6            MIXED - MATCHED/NOT PAID                     
*                                                                               
         GOTOR CKRATE,1            CHECK RATE BEFORE CHANGING                   
         JNE   EXIT                                                             
*                                                                               
         MVC   0(1,R7),PBDCTYP     NET COST CHANGE                              
         OI    CHGIND1,X'40'       SET COST CHANGE IND                          
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
P_CHG27K LA    R7,PBDCDATE-NEWREC+REC                                           
         CLC   PBDCDATE,0(R7)      CLOSE DATE CHANGED?                          
         BE    *+14                                                             
         MVC   0(3,R7),PBDCDATE                                                 
         OI    CHGIND2,X'80'       CLOSE DATE CHANGE                            
*                                                                               
         LA    R7,PBDSDATE-NEWREC+REC                                           
         CLC   PBDSDATE,0(R7)      SALE DATE CHANGED?                           
         BE    *+14                                                             
         MVC   0(3,R7),PBDSDATE                                                 
         OI    CHGIND2,X'40'       SALE DATE CHANGE                             
*                                                                               
         LA    R7,PBDMDATE-NEWREC+REC                                           
         CLC   PBDMDATE,0(R7)      MAT CLOSE DATE CHANGED?                      
         BE    *+14                                                             
         MVC   0(3,R7),PBDMDATE                                                 
         OI    CHGIND3,X'02'       MAT CLOSE DATE CHANGE                        
*                                                                               
         CLC   BUYPB(2),=C'L='     LIST BUYING?                                 
         BE    P_CHG40                                                          
*                                                                               
P_CHG30  GOTOR VT41103,DMCB,(RC),(RA),RCONABEQ                                  
         BE    P_CHG40             ADBUYER ELEMS RECONSTRUCTED                  
*                                                                               
         L     R7,ACOMWRK          POINT TO COMMENT TABLE                       
         LA    R4,10                                                            
P_CHG32  CLI   0(R7),X'FF'         IGNORE TABLE ENTRY?                          
         BE    P_CHG32H                                                         
         MVC   ELCODE,0(R7)                                                     
P_CHG32D LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM          NEED TO REMOVE ELEM?                         
         BNE   P_CHG32H                                                         
         GOTO1 VRECUP,DMCB,(1,REC),(R5)                                         
         B     P_CHG32D                                                         
P_CHG32H LA    R7,60(R7)           POINT TO NEXT TABLE ENTRY                    
         BCT   R4,P_CHG32                                                       
*                                                                               
         L     R7,ACOMWRK          POINT TO COMMENT TABLE                       
         LA    R4,10                                                            
P_CHG34  CLI   0(R7),X'FF'         IGNORE TABLE ENTRY?                          
         BE    P_CHG34H                                                         
         CLC   0(2,R7),=X'6602'    REGULAR COMMENT IS DELETED?                  
         BE    P_CHG34D                                                         
         CLC   0(2,R7),=X'6702'    INSERTION ORDER COMMENT IS DELETED?          
         BE    P_CHG34D                                                         
         CLC   0(2,R7),=X'6802'    POSITION INSTRUCTION COMMENT IS DEL?         
         BE    P_CHG34D                                                         
         CLC   0(2,R7),=X'6A02'    SRC COMMENT IS DELETED?                      
         BE    P_CHG34D                                                         
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'FF'        POINT TO END OF REC                          
         BRAS  RE,NXTELEM                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
*                                                                               
P_CHG34D CLI   0(R7),X'6A'         SRC COMMENT?                                 
         BE    P_CHG34F                                                         
         CLI   0(R7),X'68'         POSITION INSTRUCTION COMMENT?                
         BNE   *+12                                                             
         OI    CHGIND3,X'01'       POSITION INSTRUCTION COMMENT CHG'D           
         B     P_CHG34H                                                         
*                                                                               
         MVI   BYTE,X'02'          COMMENT CHANGE                               
         CLI   0(R7),X'66'                                                      
         BE    *+8                                                              
         MVI   BYTE,X'01'          INSERTION ORDER COMMENT CHANGE               
         OC    CHGIND1,BYTE                                                     
         B     P_CHG34H                                                         
*                                                                               
P_CHG34F MVI   BYTE,X'04'                                                       
         OC    CHGIND4,BYTE         SRC CHANGE                                  
*                                                                               
P_CHG34H LA    R7,60(R7)                                                        
         BCT   R4,P_CHG34                                                       
*                                                                               
P_CHG40  TM    PBUYCNTL-NEWREC+REC,X'80'                                        
         BZ    *+14                                                             
         MVC   TRCODE(2),=C'RZ'     FORCE RECALL WHEN CHANGING                  
         B     P_CHG80              SKIP TO X'24' CHG ELEM LOGIC                
*                                                                               
         TM    REC+(PBDSTAT2-PBUYREC),X'20'  SKIP IF NOT ADDED BY IDESK         
         BNO   P_CHG41                                                          
*                                                                               
         TM    CHGIND5,PCHGTRKQ    SKIP IF NOTHING TO TRACK                     
         BNO   P_CHG41                                                          
*                                                                               
         MVI   DMCB,X'03'          BUY COMMON ROUTINES                          
         BRAS  RE,LOADOVLY                                                      
         MVC   VT41103,DMCB        RESTORE ADDRESS                              
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),TRKCC_Q  TRACK STDCOLS                    
*                                                                               
P_CHG41  DS    0H                                                               
*                                                                               
         LA    R7,PBDPDATE-NEWREC+REC                                           
         CLC   PBDPDATE,0(R7)      PAYABLE DATE CHANGED?                        
         BE    P_CHG42                                                          
         OC    PBDPDATE,PBDPDATE   OVERRIDE?                                    
         BZ    P_CHG42                                                          
         OI    CHGIND2,X'10'       PAYABLE DATE CHANGE                          
         MVC   0(3,R7),PBDPDATE                                                 
         MVI   ELCODE,X'25'                                                     
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   P_CHG42                                                          
         OC    2(3,R5),2(R5)       PAID?                                        
         BZ    *-14                                                             
         B     P_CHGE_7                                                         
*                                                                               
P_CHG42  CLC   PBDBDATE,3(R7)                                                   
         BE    P_CHG42B                                                         
         OC    PBDBDATE,PBDBDATE   OVERRIDE?                                    
         BZ    P_CHG42B                                                         
         OI    CHGIND2,X'20'       BILLABLE DATE CHANGE                         
         MVC   3(3,R7),PBDBDATE                                                 
         MVI   ELCODE,X'26'                                                     
         LA    R5,REC+33                                                        
P_CHG42A BRAS  RE,NXTELEM                                                       
         BNE   P_CHG42B                                                         
         OC    5(3,R5),5(R5)       BILLED?                                      
         BZ    *-14                                                             
         TM    10(R5),X'C0'        IGNORE REVERSALS AND REVERSED ELEMS          
         BM    P_CHG42A                                                         
         B     P_CHGE_7                                                         
*                                                                               
P_CHG42B CP    PBDCD,=P'0'         CD PRESENT?                                  
         BE    P_CHG42D                                                         
         CP    PBDCD,=P'1'         CD OVERRIDE OF 0?                            
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'         RESET TO REAL VALUE                          
         LA    R7,PBDCD-NEWREC+REC                                              
         CLC   PBDCD,0(R7)         CASH DISCOUNT CHANGE?                        
         BE    P_CHG42D                                                         
*                                                                               
         TM    MATSW,X'C0'                                                      
         BM    P_CHGE_6            MIXED - MUST BE MATCHED/NOT PAID             
         MVC   0(L'PBDCD,R7),PBDCD                                              
         OI    CHGIND2,X'02'       CD CHANGE                                    
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
P_CHG42D CP    PBDACP,=P'0'        AC PRESENT?                                  
         BE    P_CHG44                                                          
         CP    PBDACP,=P'1'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'0'                                                     
         LA    R7,PBDACP-NEWREC+REC                                             
         CLC   PBDACP,0(R7)        AC CHANGE?                                   
         BE    P_CHG44                                                          
*                                                                               
         TM    MATSW,X'C0'                                                      
         BM    P_CHGE_6            MIXED - MUST BE MATCHED/NOT PAID             
         CLI   PBDCOSIN,C'C'       'C' RATE BUY?                                
         BNE   P_CHG42G                                                         
         BRAS  RE,FMTTR            NO CHANGE OF AC PCT IF BILLED/PAID           
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BE    P_CHGE_8                                                         
         MVI   8(RE),C'C'          RESET TO CHANGE                              
*                                                                               
P_CHG42G MVC   0(L'PBDACP,R7),PBDACP                                            
         OI    CHGIND2,X'04'                                                    
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
P_CHG44  BRAS  RE,SETPCCHG         SET PLANNED COST CHANGE BITS                 
*                                                                               
P_CHG44F OC    PBDTAX,PBDTAX       TAX PRESENT?                                 
         BZ    P_CHG44K                                                         
         CLC   PBDTAX,=X'000001'   'NONE' OR LOOK-UP OF 0.0?                    
         BNE   *+10                                                             
         XC    PBDTAX,PBDTAX                                                    
         LA    R7,PBDTAX-NEWREC+REC                                             
         CLC   PBDTAX,0(R7)        TAX CHANGED?                                 
         BE    P_CHG44K                                                         
*                                                                               
         BRAS  RE,FMTTR                                                         
         L     RE,TRADDR                                                        
         CLC   =C'**',8(RE)                                                     
         BNE   *+12                                                             
         MVI   8(RE),C'C'          RESET TO CHANGE                              
         B     P_CHGE_9            CAN'T CHG TAX FOR BILLED/PAID BUYS           
         MVI   8(RE),C'C'          RESET TO CHANGE                              
         TM    MATSW,X'40'                                                      
         BO    P_CHGE_6            MATCHED - NO TAX CHANGE                      
*                                                                               
         MVC   0(3,R7),PBDTAX                                                   
         OI    CHGIND3,X'08'                                                    
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
P_CHG44K OC    PBDCU,PBDCU         CU PRESENT?                                  
         BZ    *+20                                                             
         LA    R7,PBDCU-NEWREC+REC                                              
         MVC   0(3,R7),PBDCU                                                    
         MVC   TRCODE,=C'RZ'                                                    
*                                                                               
         LA    R7,PBDIDAT2-NEWREC+REC                                           
         CLC   PBDIDAT2(4),0(R7)   2ND INS DATE & EVE/MORN IND CHANGED?         
         BE    P_CHG46D                                                         
         OC    PBDIDAT2,PBDIDAT2                                                
         BE    P_CHG46D                                                         
         OI    CHGIND3,X'80'                                                    
         CLI   PBDIDAT2,X'FF'      'NONE'?                                      
         BNE   *+10                                                             
         XC    PBDIDAT2,PBDIDAT2                                                
         MVC   0(4,R7),PBDIDAT2                                                 
*                                                                               
P_CHG46D LA    R7,PBDIODAT-NEWREC+REC                                           
         CLC   PBDIODAT,0(R7)                                                   
         BE    P_CHG48                                                          
         OC    PBDIODAT,PBDIODAT                                                
         BZ    P_CHG48                                                          
         OI    CHGIND2,X'01'       IO DATE CHANGE                               
         MVC   0(3,R7),PBDIODAT                                                 
*                                                                               
P_CHG48  BRAS  RE,EDIODAT                                                       
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'70'                                                     
P_CHG48B BRAS  RE,NXTELEM          MANIO ELEM?                                  
         BNE   P_CHG52                                                          
         OC    2(3,R5),2(R5)       DATE PRESENT?                                
         BZ    P_CHG48B                                                         
         CLI   10(R5),C'X'         SPECIAL MANIO DELETE                         
         BE    P_CHG48M                                                         
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   P_CHGE_B                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'        ADD TO REC IN DATE ORDER                     
P_CHG48D BRAS  RE,NXTELEM                                                       
         BNE   P_CHG48J                                                         
         OC    2(3,R5),2(R5)       NO DATE?                                     
         BNZ   P_CHG48E                                                         
         MVC   0(50,R5),0(R7)      CAN JUST MOVE IN NEW ELEM                    
         B     P_CHG48W                                                         
P_CHG48E CLC   2(3,R5),2(R7)       SAME DATE?                                   
         BL    P_CHG48D                                                         
P_CHG48J GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     P_CHG48W                                                         
P_CHG48M LR    R7,R5               SAVE NEWREC'S R5                             
         SR    R4,R4                                                            
         LA    R5,REC+33                                                        
P_CHG48P BRAS  RE,NXTELEM                                                       
         BE    *+14                                                             
         LTR   R4,R4                                                            
         BZ    P_CHGE_C            MANIO TO BE DELETED NOT FOUND                
         B     P_CHG48U                                                         
         CLC   2(3,R5),2(R7)       MUST MATCH DATE                              
         BL    P_CHG48P                                                         
         BE    *+8                                                              
         B     P_CHGE_C            SUBSEQUENT IO ISSUED                         
         CLC   5(5,R5),5(R7)       NUMBER MUST ALSO MATCH                       
         BNE   P_CHG48P                                                         
         CLI   11(R5),C'M'         MUST HAVE BEEN MANUAL                        
         BNE   P_CHG48P                                                         
         LR    R4,R5               SAVE ADDRESS OF MANIO TO BE DELETED          
         B     P_CHG48P                                                         
P_CHG48U GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
P_CHG48W LR    R5,R7               RESTORE NEWREC'S R5                          
         B     P_CHG48B            GO GET NEXT MANIO ELEM                       
*                                                                               
P_CHG52  LA    RF,FIXELTAB                                                      
P_CHG52D CLI   0(RF),0             END OF TABLE?                                
         BE    P_CHG54                                                          
         MVC   ELCODE,0(RF)                                                     
         BRAS  RE,FIXREC                                                        
         JNE   ERROR                                                            
         LA    RF,1(RF)            POINT TO NEXT TABLE ENTRY                    
         B     P_CHG52D                                                         
*                                                                               
P_CHG54  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'                                                     
P_CHG54B BRAS  RE,NXTELEM          REP ELEM FOUND?                              
         BNE   P_CHG60                                                          
         CLI   2(R5),C'X'          SPECIAL DELETE CODE?                         
         BE    P_CHG54P                                                         
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   P_CHGE_B                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   P_CHG54J                                                         
         CLC   0(6,R5),0(R7)       SAME REP?                                    
         BE    P_CHG60                                                          
         MVC   0(10,R5),0(R7)      GET REP ELEM FROM NEWREC                     
         B     P_CHG54W                                                         
*                                                                               
P_CHG54J GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
         B     P_CHG54W                                                         
*                                                                               
P_CHG54P LR    R7,R5               SAVE NEWREC'S R5                             
         SR    R4,R4                                                            
         LA    R5,REC+33                                                        
P_CHG54Q BRAS  RE,NXTELEM                                                       
         BE    P_CHG54S                                                         
         LTR   R4,R4               WILL BE 0 IF REP ELEM NOT FOUND              
         BZ    P_CHGE_D                                                         
         B     P_CHG54U            GO DELETE ELEM                               
P_CHG54S LR    R4,R5               SAVE ADDR OF REP ELEM TO DELETE              
         B     P_CHG54Q                                                         
*                                                                               
P_CHG54U GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
P_CHG54W LR    R5,R7               RESTORE NEWREC'S R5                          
         OI    CHGIND3,X'20'       REP CHANGED                                  
         B     P_CHG54B                                                         
*                                                                               
P_CHG60  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'        OPEN RATE ELEM CODE                          
P_CHG60B BRAS  RE,NXTELEM                                                       
         BNE   P_CHG64                                                          
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   P_CHGE_B                                                         
         LR    R7,R5               SAVE NEWREC'S R5                             
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   P_CHG60J                                                         
         CLC   0(13,R5),0(R7)      SAME RATE?                                   
         BE    P_CHG64                                                          
         MVC   0(13,R5),0(R7)      GET NEW OPEN RATE ELEM IN NEWREC             
         B     P_CHG60W                                                         
*                                                                               
P_CHG60J GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
*                                                                               
P_CHG60W LR    R5,R7               RESTORE NEWREC'S R5                          
         OI    CHGIND1,X'40'       RATE CHANGED                                 
         MVC   TRCODE,=C'RZ'       TO GET RECALL                                
         B     P_CHG60B            GO GET RATE ELEM                             
*                                                                               
P_CHG64  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'84'        PST OVERRIDE ELEM CODE                       
         BRAS  RE,NXTELEM                                                       
         BNE   P_CHG68                                                          
         CLC   REC+25(2),=H'3900'  ALMOST MAXIMUM REC SIZE?                     
         BNL   P_CHGE_B                                                         
         LR    R7,R5               SAVE NEWREC POINTER                          
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
P_CHG64B BRAS  RE,NXTELEM                                                       
         BNE   P_CHG64F                                                         
         OC    2(3,R5),2(R5)       DATED PAY ELEM?                              
         BZ    P_CHG64B                                                         
         B     P_CHGE_A            CAN'T CHG TAX FOR PAID BUYS                  
*                                                                               
P_CHG64F LA    R5,REC+33                                                        
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   P_CHG64J                                                         
         CLC   0(12,R5),0(R7)      SAME PST DATA?                               
         BE    P_CHG68                                                          
         MVC   0(12,R5),0(R7)                                                   
         B     P_CHG64W                                                         
*                                                                               
P_CHG64J GOTO1 VRECUP,DMCB,(1,REC),(R7),(R5)                                    
*                                                                               
P_CHG64W LR    R5,R7               RESTORE NEWREC POINTER                       
         B     P_CHG68                                                          
*                                                                               
P_CHG68  LA    R7,PBDGST-NEWREC+REC                                             
         CLC   PBDGST,0(R7)        GST CHANGED?                                 
         BE    P_CHG68H                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
P_CHG68B BRAS  RE,NXTELEM                                                       
         BNE   P_CHG68D                                                         
         OC    2(3,R5),2(R5)       CHK FOR DATED PAY ELEM                       
         BZ    P_CHG68B                                                         
         B     P_CHGE_A            CAN'T CHG TAX FOR PAID BUYS                  
*                                                                               
P_CHG68D LA    R7,PBDGST-NEWREC+REC                                             
         MVC   0(1,R7),PBDGST                                                   
P_CHG68H TM    PBDSTAT,X'0C'       SFH ENTERED IN NEWREC?                       
         BZ    P_CHG80                                                          
         MVC   BYTE,PBDSTAT-NEWREC+REC                                          
         NI    BYTE,X'0C'                                                       
         TM    PBDSTAT-NEWREC+REC,X'08'                                         
         BNO   P_CHG68M                                                         
         TM    PBDSTAT,X'08'       HELD ON IN NEWREC?                           
         BO    P_CHG68M                                                         
         NI    PBDSTAT-NEWREC+REC,X'F7'                                         
*                                                                               
P_CHG68M OC    PBDSTAT-NEWREC+REC(1),PBDSTAT                                    
         MVC   BYTE2,PBDSTAT                                                    
         NI    BYTE2,X'0C'                                                      
         CLC   BYTE,BYTE2          REC'S PBDSTAT MATCH THAT OF NEWREC?          
         BE    *+8                                                              
         OI    CHGIND4,X'80'       SFH STATUS CHANGE                            
*                                                                               
P_CHG80  TM    PBDSTAT,X'20'       NO TRAFFIC?                                  
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
         BNZ   P_CHG82                                                          
         TM    CHGIND2,X'06'       CD OR AC                                     
         BNZ   P_CHG82                                                          
         TM    CHGIND3,X'FF'                                                    
         BNZ   P_CHG82                                                          
         MVC   BYTE,SVCLPROF+31    TEST JOB NO TO PRINT                         
         OC    BYTE,SVESPROF+31                                                 
         CLI   BYTE,C'0'                                                        
         BE    P_CHG84                                                          
         TM    PBDDTIN2,X'08'      JOB NUMBER                                   
         BZ    P_CHG84                                                          
*                                                                               
P_CHG82  MVC   PBDCHGDT-NEWREC+REC(3),BTODAY                                    
*                                                                               
P_CHG84  BRAS  RE,UPD              ADD UPLOAD ELEM                              
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
P_CHGE_1 LA    R3,JBDERR           TRAFFIC'D IO, CANNOT REMOVE AD CODE          
         J     ERROR                                                            
*                                                                               
P_CHGE_2 LA    R3,NOCHGERR                                                      
         CLI   MADSW,C'Y'          IF PBU, NEED TO UNWIND UPLOAD REC            
         BNE   P_CHGE2X                                                         
         GOTOR VT41103,DMCB,(RC),(RA),FIXDATQ                                   
P_CHGE2X L     R2,TRADDR                                                        
         J     ERROR                                                            
*                                                                               
P_CHGE_3 LA    R3,INVERR           CAN'T MAKE LIVE BUY TEST                     
         B     P_CHGE_X                                                         
*                                                                               
P_CHGE_4 LA    R3,FRZERR           CLIENT FROZEN - CAN'T MAKE LIVE              
         B     P_CHGE_X                                                         
*                                                                               
P_CHGE_5 LA    R3,FDTERR           DATE FROZEN FOR CLIENT                       
         B     P_CHGE_X                                                         
*                                                                               
P_CHGE_6 LA    R3,MATERR                                                        
         J     ERROR                                                            
*                                                                               
P_CHGE_7 LA    R3,OVRDERR                                                       
         B     P_CHGE2X                                                         
*                                                                               
P_CHGE_8 LA    R3,NOACCHG          NO AGENCY COMMISSION CHANGE                  
         B     P_CHGE2X                                                         
*                                                                               
P_CHGE_9 LA    R3,TAXERR           NO TAX CHG IF PAID/BILLED                    
         B     P_CHGE2X                                                         
*                                                                               
P_CHGE_A LA    R3,TAXERR           NO TAX CHG IF PAID/BILLED                    
         J     ERROR                                                            
*                                                                               
P_CHGE_B LA    R3,MAXSERR          MAX SIZE ERROR                               
         J     ERROR                                                            
*                                                                               
P_CHGE_C LA    R3,IOTYPER          MANIO TO BE DELETED NOT FOUND                
P_CHGECX L     R2,TRADDR           OR SUBSEQUENT IO ISSUED                      
         LHI   RF,9                                                             
         BRAS  RE,BUMPFLDS                                                      
         J     ERROR                                                            
*                                                                               
P_CHGE_D LA    R3,NFNDERR          SEP REP ELEM NOT FOUND                       
         B     P_CHGECX                                                         
*                                                                               
P_CHGE_X L     R2,TRADDR                                                        
         BRAS  RE,BUMPFLD                                                       
         J     ERROR                                                            
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
*                                                                               
FIXELTAB DC    AL1(BYPCIDQ)        PLANNED COST ELEMENT                         
         DC    AL1(PORELMEQ)       COS2 $ RATE (OPEN RATE) ELEMENT              
         DC    X'82'               FREE STANDING INSERTS ELEMENT                
         DC    X'83'               REFERENCE NUMBER ELEMENT                     
         DC    X'86'               SHIP DATE ELEMENT                            
         DC    X'87'               PAGE VIEW ELEMENT                            
         DC    X'88'               CLICK THROUGHS ELEMENT                       
         DC    X'89'               EXTENSION DAYS ELEMENT                       
         DC    X'92'               IMPRESSION ELEMENT                           
         DC    X'93'               ACTUAL IMPRESSION ELEMENT                    
         DC    X'94'               LEGAL WARNING ELEMENT                        
         DC    X'96'               EXTENSION DATE ELEMENT                       
         DC    X'98'               SITE LOCATION                                
         DC    X'A0'               ESTIMATED CPM ELEMENT                        
         DC    X'A1'               ACTUAL CPM                                   
         DC    X'A6'               ISSUE NAME ELEMENT                           
         DC    X'00'               END OF TABLE                                 
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETPCCHG NTR1  BASE=*,LABEL=*      SET PLANNED COST CHANGE BITS                 
*                                                                               
         CLI   BUYMD,C'S'          SEARCH?                                      
         JNE   CKPCC05             MAG/TRADE STILL USE OLD STYLED PC            
*                                                                               
         CLI   DDLINKSW,0          ADBUYER UPLOAD?                              
         BE    CKPCC10                                                          
         CLC   PCVERSN#,=AL1(03,04,00,31)                                       
         BNL   CKPCC10                                                          
*                                                                               
CKPCC05  LA    RE,PBDPLCOS-NEWREC+REC                                           
         CLC   PBDPLCOS,0(RE)      PLANNED COST CHANGED?                        
         BE    CKPCC_X                                                          
         OC    PBDPLCOS,PBDPLCOS   HAVE PLANNED COST?                           
         BZ    CKPCC_X                                                          
         OI    CHGIND3,X'10'                                                    
         CLI   PBDPLCOS,X'FF'      PLANNED COST IS 'NONE'?                      
         BNE   *+10                                                             
         XC    PBDPLCOS,PBDPLCOS                                                
         MVC   0(4,RE),PBDPLCOS                                                 
         B     CKPCC_X                                                          
*                                                                               
CKPCC10  XC    WKELEM,WKELEM                                                    
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM          FOUND PLANNED COST ELEM?                     
         BNE   CKPCC20                                                          
         ZIC   RE,1(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WKELEM(0),0(R5)     SAVE PLANNED COST ELEM                       
*                                                                               
CKPCC20  OC    WKELEM,WKELEM       HAVE PLANNED COST ELEM?                      
         BZ    CKPCC_X                                                          
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM          FOUND PLANNED COST ELEM?                     
         BE    CKPCC40                                                          
CKPCC30  OI    CHGIND3,X'10'       PLANNED COST HAS BEEN CHANGED                
         OI    PBDDTIN3,X'10'      PLANNED COST LAST CHANGED                    
         B     CKPCC_X                                                          
*                                                                               
CKPCC40  ZIC   RE,1(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WKELEM(0),0(R5)     PLANNED COST CHANGED?                        
         BNE   CKPCC30                                                          
*                                                                               
CKPCC_X  J     EXIT_X                                                           
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POL_P1   NTR1  BASE=*,LABEL=*      PRD ALLOCATION - PART 1                      
*                                                                               
         L     R2,TRADDR                                                        
         LH    RE,SVNTRNS                                                       
         BCTR  RE,R0                                                            
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   POL1_10                                                          
         CLI   SVESTALO,C' '                                                    
         BNH   POL1_10                                                          
         MVC   8(47,R2),SVESTALO   USE EST ALLOCATION                           
         FOUT  (R2)                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)            LENGTH                                       
*                                                                               
POL1_10  MVI   X,C' '                                                           
         MVC   X+1(46),X                                                        
         OC    8(L'PJOBALO,R2),X                                                
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    POL1_20                                                          
         L     R4,AJOBIO           CK IF AD CODE HAS ALLOCATION                 
         CLI   PJOBALO-PJOBREC(R4),C' '                                         
         BNH   POL1_20                                                          
         CLC   8(L'PJOBALO,R2),PJOBALO-PJOBREC(R4)                              
         BE    POL1_20                                                          
         LA    R3,ALOERR                                                        
         J     ERROR                                                            
*                                                                               
POL1_20  BRAS  RE,MUP              UPDATE MINIO REC & FIND ERRORS               
         TM    GENBYSW1,ALLOEXTQ   EXTENDED ALLOCS NOT VALIDATED                
         JNZ   POL1_30             ALREADY?                                     
         CLI   SVESTALO,C' '                                                    
         BNH   POL1_30                                                          
         CLC   8(47,R2),SVESTALO   MATCH THAT OF EST ALLOCATION?                
         BE    POL1_30                                                          
         LA    R3,ALOERR2                                                       
         J     ERROR                                                            
*                                                                               
POL1_30  L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          CHANGE?                                      
         BNE   POL1_40                                                          
         TM    CHGIND1,X'08'       DATE CHANGED?                                
         BO    POL1_40                                                          
         TM    4(R2),X'20'         MODIFIED?                                    
         BZ    POL1_40                                                          
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERROR                                                            
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),EXCGET_Q  GET FX RATE/AMOUNT              
*                                                                               
         GOTOR VT41103,DMCB,(RC),(RA),CHGELEMQ                                  
*                                                                               
         BRAS  RE,CHG_PUTR         UPDATE CHANGED BUY RECORD                    
         J     SETCCEQ                                                          
*                                                                               
POL1_40  BRAS  RE,POL_P2           PRD ALLOCATION - PART 2                      
         JNE   SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CHG_PUTR STCM  RE,15,SV_REG_E      UPDATE CHANGED BUY                           
         BRAS  RE,CKEIOELM         CK EIO ELEM                                  
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         JE    *+8                                                              
         BRAS  RE,PRT_PUTR                                                      
         CLI   BUYNM,C'*'          NO ASR ON CHANGES?                           
         JE    C_PUTR10                                                         
         TM    CHGIND1,X'58'       SPACE,DATE,RATE CHANGED?                     
         JNZ   *+12                                                             
         TM    CHGIND3,X'04'       MADE LIVE?                                   
         JZ    *+8                                                              
         BRAS  RE,ASR              AUTO SPACE RESERVATION                       
C_PUTR10 CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         JE    C_PUTR_X                                                         
         TM    CHGIND1,X'08'       DATE CHANGED?                                
         JNZ   *+12                                                             
         TM    CHGIND2,X'08'       AD CODE CHANGED?                             
         JZ    *+8                                                              
         BRAS  RE,CKMATPTR         CK FOR MAT= REPEAT PASSIVE PTR               
C_PUTR_X ICM   RE,15,SV_REG_E                                                   
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POL_P2   NTR1  BASE=*,LABEL=*      PRD ALLOCATION - PART 2                      
*                                                                               
         MVI   DMCB,X'04'          T41104 - PRODUCT ALLOCATION                  
         BRAS  RE,CBUYOVLY                                                      
         CLI   ERRAREA,0                                                        
         JNE   SETCCNEQ                                                         
*                                                                               
         MVI   DMCB,X'01'          HEADER VALIDATION PHASE (RESTORE IT)         
         BRAS  RE,LOADOVLY                                                      
*                                                                               
         L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          CHANGE?                                      
         BNE   POL2_20                                                          
         CLI   BUYNM,C'*'          NO ASR ON CHANGES?                           
         BE    POL2_30                                                          
         TM    CHGIND1,X'58'       DATE/SPACE/RATE CHANGED?                     
         BNZ   POL2_20                                                          
         TM    CHGIND3,X'04'       MADE LIVE?                                   
         BZ    POL2_30                                                          
*                                                                               
POL2_20  BRAS  RE,ASR              AUTO SPACE RESV. FOR ZZZ BUYS                
*                                                                               
POL2_30  L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          CHANGE?                                      
         BE    *+8                                                              
         BRAS  RE,MUP              MINIO UPLOAD REC, WILL HAVE LINE #           
*                                                                               
         J     SETCCEQ                                                          
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
         BNE   CKSPDNO             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
         CLI   TRCODE,C'C'         CHANING?                                     
         BE    CKSPD20                                                          
         CLI   TRCODE,C'B'         BUYING?                                      
         BE    CKSPD40                                                          
*                                                                               
         B     CKSPDNO             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
* SEE IF SPACE DESP ENTERED IS SAME AS IN RECORD NOW                            
*                                                                               
CKSPD20  CLC   PBDSPACE,PBDSPACE-NEWREC+REC                                     
         BE    CKSPDNO             NO NEED TO LOOK UP SPC DESP RECORD           
*                                                                               
CKSPD40  CLI   PBDSPACE,C'*'       START WITH "*"? IF SO, NO NEED TO CK         
         BE    CKSPDNO             NO NEED TO LOOK UP SPC DESP RECORD           
         XC    S.CKSPWORK,S.CKSPWORK                                            
         MVC   S.CKSPWORK(L'PBDSPACE),PBDSPACE                                  
*                                                                               
         CLI   PBDSPACE,C'#'       START WITH "#"? IF SO, IGNORE IT             
         BNE   *+16                                                             
         XC    S.CKSPWORK,S.CKSPWORK                                            
         MVC   S.CKSPWORK(L'PBDSPACE-1),PBDSPACE+1                              
*                                                                               
         MVC   S.CKSPKEY(L'KEY),KEY                                             
         MVC   S.CKSPKEY+L'KEY(L'KEYSAVE),KEYSAVE                               
*                                                                               
         XC    KEY,KEY             BUILD KEY TO READ SPC DESP RECORD            
         MVC   KEY+00(02),AGYALPHA                                              
         MVC   KEY+02(01),BUYMD                                                 
         MVI   KEY+03,X'5A'        STANDARD SPACE DESCRIPTION REC CODE          
         MVC   KEY+04(17),S.CKSPWORK                                            
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
         B     CKSPDNO                                                          
*                                                                               
CKSPD50  MVC   KEY,S.CKSPKEY                                                    
         MVC   KEYSAVE,S.CKSPKEY+L'KEY                                          
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    CKSPDX                                                           
         L     R4,AJOBIO                                                        
         CLC   X(17),PJOBSPC-PJOBREC(R4)                                        
         BE    CKSPDX                                                           
         OI    WARN,X'80'          SPACE WARNING                                
*                                                                               
CKSPDX   J     SCCEQ_R             EQUAL (SPC DESP ENTERED MATCH REC)           
CKSPDNO  J     SCCNEQ_R            NOT EQUAL (SPC DESP REC NOT CHKED)           
*                                                                               
         DROP  S                                                                
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FIXREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,NEWREC+33                                                     
FIXREC05 BRAS  RE,NXTELEM                                                       
         BNE   FIXRECX                                                          
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
         CLI   ELCODE,X'A6'                                                     
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
         BE    FIXRECX             YES                                          
*                                                                               
         CLI   ELCODE,X'82'                  FIS ELEMENT?                       
         BNE   *+12                                                             
         CLI   PBFSIIND-PBFSIEL(R7),X'02'    OVERRIDING?                        
         BNE   FIXRECX                       NO, LEAVE FSI DATA ALONE           
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
         LTR   R4,R4               HAVE ELEMENT TO DELETE?                      
         JNZ   FIXREC50                                                         
         CLI   ELCODE,BYPCIDQ      PLANNED COST?                                
         BNE   FIXERR2                                                          
         CLI   BUYMD,C'S'          SEARCH?                                      
         JE    *+6                                                              
         DC    H'0'                CANNOT HAVE MAG/TRADE USE NEW PC             
         LA    RE,REC+33+(PBDPLCOS-PBDELEM)                                     
         XC    0(L'PBDPLCOS,RE),0(RE)                                           
         B     FIXREC05            NEXT ELEM, SHOULDN'T BE ANY MORE             
*                                                                               
FIXREC30 LR    R4,R5                                                            
         B     FIXREC28                                                         
*                                                                               
FIXREC50 GOTO1 VRECUP,DMCB,(1,REC),(R4)                                         
*                                                                               
         MVC   HALF,REC+25                                                      
         SR    R1,R1                                                            
         LH    R1,HALF                                                          
         LA    RE,REC                                                           
         AR    RE,R1                                                            
         LA    RF,REC                                                           
         AHI   RF,4000                                                          
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         CLI   ELCODE,X'87'        PAGE VIEW?                                   
         BE    FIXREC55                                                         
         CLI   ELCODE,X'88'        CLICK THROUGH?                               
         BE    FIXREC55                                                         
         CLI   ELCODE,X'92'        IMPRESSION?                                  
         BE    FIXREC55                                                         
         CLI   ELCODE,X'93'        ACTUAL IMPRESSION?                           
         BNE   *+8                                                              
FIXREC55 OI    CHGIND4,X'20'       CT, PV, IMPS OR AIMPS IS CHANGED             
*                                                                               
         CLI   ELCODE,X'94'        LEGAL WARNINGS?                              
         BNE   *+8                                                              
         OI    CHGIND4,X'08'       LEGAL WARNING IS CHANGED                     
*                                                                               
FIXREC70 LR    R5,R7               RESTORE R5 TO POINT TO NEWREC                
         B     FIXREC05            NEXTEL AGAIN, SHOULDN'T BE ANY MORE          
*                                                                               
FIXERR1  LA    R3,MAXSERR          ELEM MAXIMUM LENGTH ERROR                    
         B     FIXRERR                                                          
*                                                                               
FIXERR2  LA    R3,INVERR           NO ELEMENTS IS FOUND                         
         L     R2,TRADDR                                                        
         LA    RF,9                                                             
         BRAS  RE,BUMPFLDS         POINT R2 TO ERROR FIELD                      
         B     FIXRERR                                                          
*                                                                               
FIXRECX  J     SETCCEQ             EQUAL                                        
FIXRERR  J     SCCNEQ_R                                                         
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BEXCL    NTR1  BASE=*,LABEL=*      PRD EXCULSION CLASS VALIDATION               
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
         XC    BUYMSG,BUYMSG                                                    
         CLI   BYPROF+6,C'X'                                                    
         BE    BEXCLER1            EXCL CLASS IS NOT ALLOWED IN BUY             
         CLI   BYPROF+6,C'W'                                                    
         BNE   BEXCLX              NO OTHER VALUES IN PROF+6                    
         OI    WARN,X'10'          TURN ON EXCL CLASS WARNING BIT               
         B     BEXCLX                                                           
*                                                                               
BEXCLER1 LHI   R3,XCLCNFER         EXCLUSION CONFLICT ERROR                     
         LA    R2,BUYTR1H                                                       
         B     BEXCLERR                                                         
*                                                                               
BEXCLX   J     SETCCEQ             EQUAL                                        
BEXCLERR J     ERROR                                                            
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKRATE   NTR1  BASE=*,LABEL=*      RATE CHANGE VALIDATION                       
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND??                 
         JNE   CKR90ERR                                                         
*                                                                               
         CHI   R1,1                CHANGING?                                    
         BNE   CKR10                                                            
         LHI   R3,PRFRTCER         RATE CHANGES NOT ALLOWED                     
         CLI   BYPROF+7,C'R'       CKING PROFILE                                
         BE    CKR20                                                            
CKR05    CLI   BYPROF+7,C'B'                                                    
         BE    CKR20                                                            
         B     CKRXX                                                            
*                                                                               
CKR10    CHI   R1,2                DELETING?                                    
         BNE   CKR15                                                            
         LHI   R3,PRFIDLER         INSERTION DELETION NOT ALLOWED               
         CLI   BYPROF+7,C'D'       CKING PROFILE                                
         BE    CKR20                                                            
         B     CKR05                                                            
*                                                                               
CKR15    DC    H'0'                THERE'S NO OTHER ACTION AT THIS TIME         
*                                                                               
CKR20    GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         CLC   PGROSS(12),GROSS    UP TO CASH DISCOUNT SAME?                    
         BE    CKR30                                                            
*                                                                               
         J     CKRXX               DON'T CHECK FOR CANADA ANYMORE               
*                                                                               
         CLI   NATION,C'C'         CANADIAN AGY?                                
         BNE   CKR30                                                            
         OC    PAID,PAID           ANYTHING IN PAID AMOUNT?                     
         BZ    CKR30                                                            
         CLC   PAID,PYABLE         FULLY PAID?                                  
         BNE   CKRXX                                                            
CKR30    LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'        PAY ELEM EXIST?                              
         BRAS  RE,NXTELEM                                                       
         BNE   CKRXX               ALLOW RATE CHG/DEL                           
         OC    2(3,R5),2(R5)                                                    
         BZ    CKRXX               NO DATE, ALLOW RATE CHG/DEL                  
*                                                                               
CKR90ERR BRAS  RE,GET_ETXT                                                      
         MVI   ERRAREA,X'FF'                                                    
         B     CKRERR                                                           
*                                                                               
CKRXX    J     SETCCEQ                                                          
CKRERR   J     SETCCNEQ                                                         
*                                                                               
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
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(L'PMTPKEY),KEYSAVE                                           
         BNE   CKMPP_X                                                          
         OI    KEY+25,PMTPC1DQ     SET TO DELETE                                
         BRAS  RE,PRT_WRIT                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
CKMPP_X  MVC   DMINBTS,BYTE4                                                    
         J     EXIT_X                                                           
*                                                                               
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
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
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
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLKEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
TSTLKEQ  J     SETCCEQ             EQUAL                                        
*                                                                               
TSTLKNEQ J     SETCCNEQ            NOT EQUAL                                    
*                                                                               
         DROP  RB,L                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTC     NTR1  BASE=*,LABEL=*      EDIT COMMENT LINES                           
*                                                                               
         BRAS  RE,BUMPFLD          NEXT FIELD                                   
         MVI   UPLSW,0                                                          
         XC    UPID,UPID                                                        
         XC    UPCURSOR,UPCURSOR                                                
         MVI   COPYSW,0                                                         
         ZAP   CAPCNT,=P'0'                                                     
*                                                                               
         BRAS  RE,CKADBYER         ADBUYER?                                     
         BE    NEXTD1                                                           
         CLI   TRCODE,C'B'         BUYING?                                      
         BNE   EDTC0                                                            
         XC    PRVTAB,PRVTAB                                                    
         B     EDTC1B              YES - UNVALIDATE ALL COMMENT LINES           
*                                                                               
EDTC0    LA    R3,5                                                             
         LA    R7,PRVTAB           TABLE OF PREVIOUSLY VALIDATED COMM           
         LR    R4,R2                                                            
         SR    R0,R0               GET PAST PROTECTED FIELD                     
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
EDTC1    CLI   0(R7),X'20'                                                      
         BNE   EDTC1A                                                           
         TM    4(R4),X'20'                                                      
         BO    NXTFLD                                                           
         B     EDTC1A1             CK IF COMMENT IS OVERWRITTEN                 
*                                                                               
EDTC1A   CLI   5(R4),0                                                          
         BE    NXTFLD                                                           
         CLC   8(3,R4),=C'IC='     IC= COMMENT?                                 
         BE    EDTC1B                                                           
         CLC   8(3,R4),=C'PI='     PI= COMMENT?                                 
         BE    EDTC1B                                                           
         CLC   8(4,R4),=C'SRC='    SRC= COMMENT?                                
         BE    EDTC1B                                                           
*                                                                               
EDTC1A1  ST    R4,FULL             CURRENT FLD POINTER                          
         BRAS  RE,CKKEYW                                                        
         BE    NXTFLD                                                           
*                                                                               
EDTC1B   LA    R3,5                UNVALIDATE ALL COMMENT LINES                 
         LR    R4,R2                                                            
         SR    R0,R0               GET PAST PROTECTED FIELD                     
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
EDTC2    NI    4(R4),X'DF'                                                      
         LA    R4,COMLEN(R4)                                                    
         BCT   R3,EDTC2                                                         
         B     EDTC3                                                            
*                                                                               
NXTFLD   LA    R7,1(R7)                                                         
         LA    R4,COMLEN(R4)                                                    
         BCT   R3,EDTC1                                                         
*                                                                               
* ALL COMMENTS DISLPAYED WERE NOT CHANGED AND NO NEW ONES WERE ADDED            
*                                                                               
EDTC3    L     R7,ACOMWRK                                                       
         LA    RE,10                                                            
EDTC3A   MVI   0(R7),X'FF'         INIT COMMENT TABLE                           
         LA    R7,60(R7)                                                        
         BCT   RE,EDTC3A                                                        
*                                                                               
         L     R7,ACOMWRK                                                       
         LA    R6,PRVTAB                                                        
         LA    R3,5                                                             
EDTCOM1  ST    R3,SV_REG_3         NUMBER OF OPTIONAL DATA LINES                
         BRAS  RE,BUMPFLD                                                       
         TM    4(R2),X'20'                                                      
         BO    NEXTD               NEXT COMMENT LINE                            
*                                                                               
         CLI   5(R2),0             INPUT THIS TIME?                             
         BNE   EDTCOM1A                                                         
         CLI   0(R6),X'20'         THERE IS A COMMENT?                          
         BNE   NEXTD                                                            
         CLI   MADSW,C'Y'          PBU? DON'T DELETE COMMENTS FOR PBU           
         JE    *+10                                                             
         MVC   0(2,R7),=X'6602'    SET DUMEL ENTRY TO DEL OLD COMMENT           
         B     NEXTD                                                            
*                                                                               
EDTCOM1A ST    R2,FULL             CURRENT FLD POINTER                          
         BRAS  RE,CKKEYW                                                        
         BNE   EDTCM1                                                           
         L     RF,FULL             FULL HAS ADDRESS OF ROUTINE                  
         A     RF,RELOBY11                                                      
         BR    RF                                                               
*                                                                               
EDTCM1   CLC   =C'COM=',8(R2)      COM= ONLY FOR IC=                            
         JE    FLDINV                                                           
         CLC   =C'IB=COM=',8(R2)   NO COM= FOR IB=                              
         JE    FLDINV                                                           
*                                                                               
         CLC   =C'COPY=',8(R2)     SPECIAL COMMENTS?                            
         BNE   EDTCM2                                                           
         CLI   COPYSW,1            COPY ALREADY ENTERED?                        
         JE    FLDINV                                                           
         CLI   5(R2),22            COPY MAX OF 22 (17+5)?                       
         JH    FLDINV                                                           
         OI    COPYSW,1                                                         
         B     EDTCMX                                                           
*                                                                               
EDTCM2   CLC   =C'CAP=',8(R2)      SPECIAL COMMENTS?                            
         BNE   EDTCMX                                                           
         CP    CAPCNT,=P'1'        MAX OF TWO CAP= COMMENT REACHED?             
         JH    FLDINV                                                           
         CLI   5(R2),29            CAP MAX OF 29 (25+4)?                        
         JH    FLDINV                                                           
         AP    CAPCNT,=P'1'                                                     
         B     EDTCMX                                                           
*                                                                               
EDTCMX   SR    RE,RE                                                            
         MVI   0(R7),X'66'                                                      
         IC    RE,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLC   =C'IC=',8(R2)                                                    
         BE    EDTCOM1B                                                         
         CLC   =C'IB=',8(R2)                                                    
         BE    EDTCOM1B                                                         
         CLC   =C'SRC=',8(R2)      SRC COMMENTS?                                
         BE    EDTCOMAA                                                         
         CLC   =C'PI=',8(R2)                                                    
         BNE   EDTCOM2                                                          
         MVI   0(R7),X'68'         POSITION COMMENTS                            
         B     EDTCOM1C                                                         
*                                                                               
EDTCOMAA ZIC   R1,5(R2)                                                         
         CHI   R1,44               MAX OF 40 CHARS FOR SRC REACHED?             
         JH    FLDINV                                                           
         MVI   0(R7),X'6A'         SRC ELEMENT                                  
         AHI   RE,-4                                                            
         LA    R4,8+4(R2)          PASS SRC= AND POINT TO COMMENT               
         B     EDTCOM2                                                          
*                                                                               
EDTCOM1B MVI   0(R7),X'67'                                                      
EDTCOM1C AHI   RE,-3                                                            
*                                                                               
         LA    R4,11(R2)                                                        
         CLC   0(4,R4),=C'COM='                                                 
         BNE   EDTCOM2                                                          
         CHI   RE,10               COMMENT CODE IS 1-6 CHARS?                   
         JH    FLDINV                                                           
         CHI   RE,5                                                             
         JL    FLDINV                                                           
         ST    RE,FULL             SAVE INPUT LENGTH                            
         AHI   RE,-4               MINUS COM= OVERHEAD                          
         MVC   WORK(6),=6C' '                                                   
         LA    RF,WORK+6                                                        
         SR    RF,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),4(R4)       RIGHT ALIGN IN WORK                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BRAS  RE,PRT_RDHI                                                      
         L     RE,FULL             RESTORE INPUT LENGTH                         
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDTCOM2                                                          
         LA    R3,NFNDERR                                                       
         J     ERROR                                                            
*                                                                               
EDTCOM2  LR    R0,RE                                                            
         AHI   R0,2                FOR ELEM CODE & LENGTH                       
         STC   R0,1(R7)                                                         
         LTR   RE,RE                                                            
         BNZ   EDTCOM2D                                                         
         CLC   =C'IB=',8(R2)                                                    
         BNE   NEXTD                                                            
         MVC   60(2,R7),=X'6602'   SO IB= WILL DELETE BOTH 67 AND 66            
EDTCOM2B LA    R7,60(R7)                                                        
         B     NEXTD                                                            
*                                                                               
EDTCOM2D BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R7),0(R4)                                                    
         CLC   =C'IB=',8(R2)       IB= (GENERATE BOTH 66 AND 67)?               
         BNE   EDTCOM3                                                          
         LA    RF,2                FOR LOOPING                                  
         MVI   60(R7),X'66'                                                     
         STC   R0,61(R7)           STORE ELEM LENGTH                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   62(0,R7),0(R4)                                                   
EDTCOM3  OC    INSDA,INSDA         ADD?                                         
         BNZ   EDTCOM2B            NEXT COMMENT LINE                            
EDTCOM4  MVI   ELCODE,X'FF'        TO GET TO END OF REC                         
         LA    R5,NEWREC+33                                                     
         BRAS  RE,NXTELEM                                                       
         LR    R0,RF                                                            
         GOTO1 VRECUP,DMCB,(1,NEWREC),0(R7),(R5)                                
         LR    RF,R0                                                            
         CLC   =C'IB=',8(R2)                                                    
         BNE   NEXTD                                                            
         LA    R7,60(R7)           BUMP TO NEXT ELEMENT                         
         BCT   RF,EDTCOM4          ADD OTHER ELEMENT                            
         B     NEXTD0                                                           
*                                                                               
NEXTD    LA    R7,60(R7)                                                        
NEXTD0   LA    R6,1(R6)                                                         
         L     R3,SV_REG_3         # OF OPTIONAL DATA LINES TO PROCESS          
         BCT   R3,EDTCOM1                                                       
*                                                                               
NEXTD1   CLI   SVESPROF+15,C' '    HAVE EST REP?                                
         BE    NEXTD5                                                           
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'        SPECIAL REP ELEM CODE                        
         BRAS  RE,NXTELEM                                                       
         BE    NEXTD5                                                           
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          BUYING?                                      
         BNE   NEXTD5                                                           
         XC    WORK(10),WORK       MUST ADD A SPECIAL REP ELEM                  
         MVC   WORK(2),=X'800A'                                                 
         MVC   WORK+2(4),SVESPROF+15                                            
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
*                                                                               
NEXTD5   BRAS  RE,PRCC2FAC         PROCESS COS2 FACTOR                          
*                                                                               
NEXTDX   J     EXIT_X                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTORATE LA    R3,INVERR           EDIT OPEN RATE OVERRIDE, DEFAULT ERR         
         CLI   SVESPROF+30,C'Y'    FINANCIAL CLT (SET IN PPBUY01)?              
         JNE   ERROR                                                            
         LA    R3,INVRTERR         MUST BE TOTAL RATE FOR FINANCIAL CLT         
         MVI   BYTE,2                                                           
*                                                                               
EDTOR7   ZIC   R0,5(R2)                                                         
         AHI   R0,-3                                                            
         JNP   ERROR                                                            
         CLC   11(3,R2),=C'CON'    USE CONTRACT RATE?                           
         JE    EDTOR8D                                                          
         LA    R5,11(R2)                                                        
         CLI   0(R5),C'T'                                                       
         JNE   EDTOR8                                                           
         LA    R5,1(R5)                                                         
         AHI   R0,-1                                                            
         JNP   ERROR                                                            
         MVI   BYTE,2                                                           
EDTOR8   GOTO1 VCASHVAL,DMCB,(BYTE,(R5)),(X'40',(R0))                           
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'849999999'   MAX FOR PDBCOS?                              
         JH    ERROR                                                            
EDTOR8D  XC    WKELEM,WKELEM                                                    
         MVC   WKELEM(2),=X'300D'                                               
         LA    R3,WKELEM                                                        
         USING PORELEMD,R3                                                      
         CLC   11(3,R2),=C'CON'    USE CONTRACT RATE?                           
         JNE   EDTOR8G                                                          
         MVC   PORCOS,PBDCOS                                                    
         MVI   PORCOSTY,C'C'       WILL SET CONTRACT VALUES                     
         J     EDTOR10                                                          
*                                                                               
EDTOR8G  ZAP   PORCOS,DUB                                                       
         LTR   R0,R0               OPEN RATE IS FREE, PBDCOS ALSO FREE?         
         JNZ   EDTOR9                                                           
         ZAP   PORCOS,=P'1'                                                     
         CP    PBDCOS,=P'1'                                                     
         JE    EDTOR9                                                           
         LA    R3,INVRTERR                                                      
         J     ERROR                                                            
*                                                                               
EDTOR9   MVC   PORCOSTY,PBDCOSTY                                                
         CLI   11(R2),C'T'         TOTAL COST INPUT?                            
         JNE   *+8                                                              
         MVI   PORCOSTY,C'T'                                                    
EDTOR10  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NXTELEM          OPEN RATE ELEM ALREADY THERE?                
         JNE   EDTOR12                                                          
         LA    R3,INVERR                                                        
         J     ERROR                                                            
*                                                                               
EDTOR12  GOTO1 VRECUP,DMCB,(1,NEWREC),WKELEM,(R5)                               
*                                                                               
EDTORX   J     NEXTD                                                            
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTUPID  BRAS  RE,CKUPLID          CHECK UPLOAD ID                              
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTTAX   DS    0H                  EDIT TAX OVERRIDE                            
         OC    PBDTAX,PBDTAX                                                    
         JNZ   ERROR                                                            
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
         LA    R3,INVERR                                                        
         MVI   PBDTAX+2,X'01'      SET TAX=NONE                                 
         CLC   =C'NONE',12(R2)                                                  
         JE    EDTTAX4                                                          
*                                                                               
         LA    R3,INVERR                                                        
         ZIC   R0,5(R2)            LENGTH OF FIELD                              
         AHI   R0,-4               SUBTRACT FOR PREFIX                          
         JNP   ERROR                                                            
         GOTOR VCASHVAL,DMCB,(4,12(R2)),(R0)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         MVI   PBDTAX+2,X'01'                                                   
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         JZ    EDTTAX4                                                          
         JM    ERROR                                                            
         C     RF,=F'1000000'      SHOULD NOT EXCEED 100 PCT                    
         JH    ERROR                                                            
         MVC   PBDTAX,5(R1)                                                     
*                                                                               
EDTTAX4  CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE EST                          
         JNE   EDTTAX6                                                          
         CLC   PBDTAX,=X'000001'   TAX MUST BE ZERO                             
         JNE   ERROR                                                            
*                                                                               
EDTTAX6  MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         J     NEXTD               NEXT COMMENT LINE                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTREF   BRAS  RE,EDTREF2          EDIT REFERENCE NUMBER                        
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCSHDT DS    0H                  EDIT DATES                                   
*                                                                               
EDTSHP2  LA    R4,8+3(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(20),WORK                                                    
         ZIC   R5,5(R2)                                                         
         AHI   R5,-3               ADJUST INPUT LENGTH (FOR REF=)               
         JP    EDTSHP4                                                          
*                                                                               
EDTSHP3  L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          BUYING?                                      
         JE    EDTSHP30            JUST EXIT                                    
*                                                                               
EDTSHP3H MVC   WORK(3),=3X'FF'     SPECIAL DELETE CODE                          
         J     EDTSHP10                                                         
*                                                                               
EDTSHP4  LA    R3,INVERR           RESET DEFAULT ERR MSG, JUST IN CASE          
         CHI   R5,8                MAX IS MMMDD/YY (8 CHARS)                    
         JH    ERROR                                                            
*                                                                               
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         JNE   *+14                                                             
         CLC   =C'NONE',0(R4)                                                   
         JE    EDTSHP3H            DELETE EXTENSION DATE ELEM                   
*                                                                               
         MVC   WORK(8),0(R4)                                                    
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+10                                    
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR                                                            
*                                                                               
* NOTE - DATVAL SETS 00 IN YEAR IF NOT INPUT                                    
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK+10),(3,WORK)                                
*                                                                               
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         JNE   EDTSHP4H                                                         
         CLC   WORK(3),PBUYKDAT                                                 
         BNH   EDTSHP4H                                                         
         LA    R3,EXDERR1          EXDATE MUST BE ON/BEFORE INS DATE            
         J     ERROR                                                            
*                                                                               
EDTSHP4H XC    WORK+3(2),WORK+3                                                 
         CLC   WORK+10(2),=C'00'   TEST HAVE YEAR                               
         JNE   EDTSHP4X                                                         
         MVC   WORK+0(1),PBUYKDAT  SET YEAR                                     
         CLC   WORK(3),PBUYKDAT                                                 
         JH    EDTSHP4X                                                         
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
         JNE   *+8                                                              
         MVI   ELCODE,X'96'                                                     
         BRAS  RE,NXTELEM                                                       
         JNE   EDTSHP20            MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE DATE ELEM                 
*                                                                               
EDTSHP20 XC    WORK+15(15),WORK+15                                              
         MVC   WORK+15(2),=X'8607' CODE AND LENGHT                              
         CLI   BYTE2,X'96'         EXTENSION DATE?                              
         JNE   *+10                                                             
         MVC   WORK+15(2),=X'9605' CODE AND LENGHT FOR EXTENSION DATE           
         MVC   WORK+17(5),WORK                                                  
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTSHP30 J     NEXTD                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPST   BRAS  RE,CHKPST                                                        
         JNE   EXIT                                                             
         J     NEXTD                                                            
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
         LA    R3,INVERR                                                        
*                                                                               
         LA    R4,8+4(R2)          POINT TO INPUT                               
         ZIC   R5,5(R2)                                                         
         CHI   R5,5                                                             
         JNE   ERROR                                                            
         LA    R5,GSTTAB                                                        
EDTG2    CLC   0(1,R4),0(R5)                                                    
         JE    EDTG5                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         JE    ERROR                                                            
         J     EDTG2                                                            
*                                                                               
EDTG5    MVC   PBDGST,0(R4)                                                     
*                                                                               
EDTGSTX  J     NEXTD                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSFH   DS    0H                  EDIT SPECIAL FINANCIAL HANDLING              
         LA    R3,INVERR                                                        
         LA    R4,8+4(R2)          POINT TO INPUT                               
         ZIC   R5,5(R2)                                                         
         CHI   R5,5                                                             
         JL    ERROR                                                            
         CLC   0(4,R4),=C'HOLD'                                                 
         JE    EDTSFHH                                                          
         CLC   0(4,R4),=C'HELD'                                                 
         JE    EDTSFHH                                                          
         CLI   0(R4),C'H'                                                       
         JE    EDTSFHH                                                          
         CLC   0(3,R4),=C'REL'                                                  
         JE    EDTSFHR                                                          
         CLI   0(R4),C'R'                                                       
         JE    EDTSFHR                                                          
         J     ERROR                                                            
*                                                                               
EDTSFHH  OI    PBDSTAT,X'0C'       SET ON SFH AND HOLD BITS                     
         L     R1,TRADDR           SEE IF BUYING                                
         CLI   8(R1),C'B'                                                       
         JE    EDTSFHX                                                          
*                                                                               
* ON CHANGES LOOK FOR I/O ELEMEMT WITH A DATE                                   
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'                                                     
EDTSFHH1 BRAS  RE,NXTELEM                                                       
         JNE   EDTSFHX                                                          
         OC    2(3,R5),2(R5)       CHECK FOR DATE                               
         JZ    EDTSFHH1                                                         
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(34),=C'CAN''T HOLD- BUY HAS BEEN ON AN I/O'               
         MVI   ERRAREA,X'FF'                                                    
         MVC   TRCODE,=C'RI'       SET FOR I/O RECALL                           
         J     EXIT                                                             
*                                                                               
EDTSFHR  OI    PBDSTAT,X'04'       SFH BIT                                      
         NI    PBDSTAT,X'F7'       SET OFF X'08' HOLD BIT                       
*                                  IF NEW BUY COULD ALREADY BE ON               
EDTSFHX  J     NEXTD                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCU    DS    0H                  EDIT CONTRACT UNITS                          
         OC    PBDCU,PBDCU                                                      
         JNZ   ERROR                                                            
         MVI   PBDCU+2,X'01'       SET CU=NONE                                  
         CLC   =C'NONE',11(R2)                                                  
         JE    EDTCU4                                                           
*                                                                               
         LA    R3,INVERR                                                        
         ZIC   R0,5(R2)            LENGTH OF FIELD                              
         AHI   R0,-3               SUBTRACT FOR PREFIX                          
         JNP   ERROR                                                            
         GOTO1 VCASHVAL,DMCB,(4,11(R2)),(R0)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         MVI   PBDCU+2,X'01'       .0001 = ZERO UNITS                           
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         JZ    EDTCU4                                                           
         JM    ERROR                                                            
         CLI   PBDSPACE,C'*'       * BUYS CAN NOT HAVE CU=                      
         JE    ERROR                                                            
         C     RF,=F'1'            CANNOT INPUT .0001 SINCE 0                   
         JE    ERROR               IS CARRIED AS X'000001'                      
         C     RF,=F'16777215'     CANNOT EXCEED 1,677.7215                     
         JH    ERROR               MAX IS 3 BYTES                               
         MVC   PBDCU,5(R1)                                                      
*                                                                               
EDTCU4   MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         J     NEXTD               NEXT COMMENT LINE                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCCD   DS    0H                  EDIT CASH DISCOUNT OVERRIDE                  
         CP    PBDCD,=P'0'                                                      
         JNE   FLDINV                                                           
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
         LHI   R4,10000                                                         
         LA    R5,PBDCD                                                         
         LA    R6,(L'PBDCD-1)*16   HIGH NIBBLE                                  
*                                                                               
         MVI   BYTE3,1             CASH DISCOUNT HAS ONE DECIMAL                
         BRAS  RE,EDTPCT                                                        
         JNE   EXIT                                                             
         J     NEXTD               NEXT COMMENT LINE                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCAC   DS    0H                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
         CLI   PBDCOSIN,C'S'                                                    
         JNE   *+12                                                             
         LA    R3,ACERR                                                         
         J     ERROR                                                            
*                                                                               
         CP    PBDACP,=P'0'        TEST ALREADY INUT                            
         JNE   FLDINV                                                           
         CLC   11(3,R2),=C'100'    TEST INPUT OF 100                            
         JNE   EDTCAC4                                                          
         ZAP   PBDACP,=P'-1'       SET 100 PCT TO -1 PCT                        
         MVC   TRCODE,=C'RZ'                                                    
         J     NEXTD               NEXT COMMENT LINE                            
*                                                                               
EDTCAC4  LA    R4,100                                                           
         LA    R5,PBDACP                                                        
         LA    R6,(L'PBDACP-1)*16  HIGH NIBBLE                                  
         MVI   BYTE3,3             AGY COMM HAS THREE DECIMALS                  
         BRAS  RE,EDTPCT                                                        
         JNE   EXIT                                                             
         J     NEXTD               NEXT COMMENT LINE                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTMANIO BRAS  RE,EDTMAN                                                        
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDITFSI  BRAS  RE,EDTFSI                                                        
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTPVWS  MVI   WORK,X'87'          PAGE VIEW ELEM CODE                          
         J     EDTIM50                                                          
*                                                                               
EDTCTUS  MVI   WORK,X'88'          CLICK THROUGH ELEM CODE                      
         J     EDTIM50                                                          
*                                                                               
EDTEXD   LA    R3,EXDERR2          EXDAYS/EXDATE ERROR                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'96'        IF DOING EXDAYS, CANNOT HAVE EXDATE          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR                                                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'96'        IF DOING EXDAYS, CANNOT HAVE EXDATE          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR                                                            
         MVI   WORK,X'89'          ELEMENT CODE FOR EXTENSION DAYS              
         J     EDTIM50                                                          
*                                                                               
EDTAIMP  MVI   WORK,X'93'          ACUTAL IMPRESSION ELEM CODE                  
         J     EDTIM50             SAME AS IMPRESSION                           
*                                                                               
EDTIMP   MVI   WORK,X'92'          IMPRESSION ELEM CODE                         
EDTIM50  BRAS  RE,EDTPACK                                                       
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTEXDT  LA    R3,EXDERR2          EXDAYS/EXDATE ERROR                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'89'        IF DOING EXDATE, CANNOT HAVE EXDAYS          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR               NO GOOD, EXDAYS ELEM EXIST                   
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'89'        IF DOING EXDATE, CANNOT HAVE EXDAYS          
         BRAS  RE,NXTELEM                                                       
         JE    ERROR               NO GOOD, EXDAYS ELEM EXIST                   
         MVI   BYTE2,X'96'         FLAG FOR DOING EXTENSION DATE                
         LA    R4,8+7(R2)          POINT TO INPUT                               
         ZIC   R5,5(R2)                                                         
         AHI   R5,-7               ADJUST INPUT LENGTH (EXDATE=)                
         JP    EDTSHP4             SHARE CODE WITH SHIPPING DATE                
         J     EDTSHP3             NO DATE IS ENTERED                           
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSREP  BRAS  RE,EDTSR                                                         
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTLW    MVI   WORK,X'94'          LEGAL WARNING ELEM CODE                      
         BRAS  RE,CHKLW                                                         
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTTRAFF BRAS  RE,CHKNTRA          NO TRAFFIC                                   
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTCPDDT LA    R4,PBDPDATE         PAYABLE DATE FORMAT IS MMMDD/YY              
         OC    PBDPDATE,PBDPDATE                                                
         JNZ   FLDINV                                                           
         LA    R5,11(R2)                                                        
         MVI   BYTE,0                                                           
         J     EDTCDT                                                           
*                                                                               
EDTCBLDT LA    R4,PBDBDATE         BILLABLE DATE FORMAT IS MMMDD/YY             
         OC    PBDBDATE,PBDBDATE                                                
         JNZ   FLDINV                                                           
*                                                                               
         CLI   MADSW,C'Y'          MAD UPLOAD?                                  
         JE    *+12                                                             
         TM    T411FFD+12,X'80'    NO BD= CHANGES                               
         JO    ACSSERR                                                          
*                                                                               
         LA    R5,11(R2)                                                        
         MVI   BYTE,0                                                           
         J     EDTCDT                                                           
*                                                                               
EDTCIODT LA    R4,PBDIODAT                                                      
         OC    PBDIODAT,PBDIODAT                                                
         JNZ   FLDINV                                                           
         MVC   PBDIODAT,=3X'FF'    IOD = 'NONE'                                 
         CLC   11(4,R2),=C'NONE'                                                
         JE    NEXTD               NEXT COMMENT LINE                            
         LA    R5,11(R2)                                                        
         MVI   BYTE,0                                                           
*                                                                               
EDTCDT   GOTO1 VDATVAL,DMCB,(BYTE,00(R5)),WORK                                  
         OC    0(4,R1),0(R1)                                                    
         JZ    ERROR                                                            
*                                                                               
* PUT DATE IN RECORD                                                            
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK),(3,(R4))                                   
*                                                                               
         CLC   WORK(2),=C'00'      TEST HAVE YEAR                               
         JNE   EDTCDT4                                                          
*                                                                               
         MVC   0(1,R4),PBUYKDAT    SET YEAR                                     
         CLC   0(3,R4),PBUYKDAT                                                 
         JH    EDTCDT2                                                          
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
EDTCDT4  MVC   TRCODE,=C'RI'                                                    
         CLC   =C'ID=',8(R2)                                                    
         JE    NEXTD               NEXT COMMENT LINE                            
         MVC   TRCODE,=C'RZ'                                                    
         J     NEXTD               NEXT COMMENT LINE                            
*                                                                               
* ROUTINE FOR SECOND INSERTION DATE                                             
*                                                                               
EDTCD2   MVI   PBDEMIND,0          M/E IND                                      
*                                                                               
         OC    PBDIDAT2,PBDIDAT2                                                
         JNZ   ERROR               CAN'T HAVE ANYTHING IN THERE YET             
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'A6'        ISSUE NAME ELEM                              
         BRAS  RE,NXTELEM                                                       
         JNE   *+12                                                             
         LA    R3,ISSNMERR                                                      
         J     ERROR               CAN'T HAVE BOTH                              
*                                                                               
         MVI   PBDIDAT2,X'FF'      SET FOR 'NONE'                               
         CLC   11(4,R2),=C'NONE'                                                
         JNE   EDTCD2A                                                          
*                                                                               
         CLI   TRCODE,C'B'         SEE IF BUYING                                
         JNE   *+8                                                              
         MVI   PBDIDAT2,0          SET FOR 'NONE' ON A NEW BUY                  
         J     NEXTD                                                            
*                                                                               
EDTCD2A  MVC   WORK(6),11(R2)                                                   
         CLI   PBDFREQ,C'M'        FOR 'MONTHLIES' ONLY MMM                     
         JNE   EDTCD2B                                                          
         CLI   WORK+3,C' '                                                      
         JH    ERROR                                                            
         MVC   WORK+3(2),=C'01'                                                 
         J     EDTCD2D                                                          
*                                                                               
EDTCD2B  LA    RF,WORK+5                                                        
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-10                                                             
         CLI   0(RF),C'0'                                                       
         JNL   EDTCD2D                                                          
         MVC   PBDEMIND,0(RF)                                                   
         CLI   PBDEMIND,C'E'                                                    
         JE    EDTCD2D                                                          
         CLI   PBDEMIND,C'M'                                                    
         JNE   ERROR                                                            
*                                                                               
EDTCD2D  LA    R4,PBDIDAT2                                                      
         LA    R5,WORK                                                          
         MVI   BYTE,1                                                           
         J     EDTCDT                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTISSNM BRAS  RE,EDTIS00          EDIT ISSUE NAME                              
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTADID  BRAS  RE,EDTADIDP         AD-ID "PROCESSING"                           
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTPLCOS GOTOR PLANCOST,0          PROCESS PLANNED COST                         
         JNE   EXIT                                                             
         J     NEXTD                                                            
*                                                                               
EDTPO#   GOTOR VALBYPO#,0          VALIDATE PURCHASE ORDER #                    
         JNE   ERROR                                                            
         J     NEXTD                                                            
*                                                                               
EDTCOS2  GOTOR VAL_COS2,0          VALIDATE COS2                                
         JNE   ERROR                                                            
         J     NEXTD                                                            
*                                                                               
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
         BE    CKKWERR             YES, KEYWORD NOT FOUND                       
*                                                                               
         ZIC   RF,0(RE)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),1(RE)       KEYWORD MATCHED?                             
         BNE   CKKW60              NO, TRY NEXT ENTRY IN TABLE                  
*                                                                               
         MVC   FULL,8(RE)          ADDRESS FOR KEYWORD EDIT ROUTINE             
         OC    FULL,FULL                                                        
         BZ    CKKWERR             ADDRESS IS NOT IN TABLE                      
         B     CKKWX               PASS BY ADDRESS OF ROUTINE IN FULL           
*                                                                               
CKKW60   LA    RE,12(RE)           NEXT ENTRY IN KEYWORD TAB                    
         B     CKKW30                                                           
*                                                                               
CKKWX    J     SETCCEQ             EQUAL (PASS BACK ADDRESS OF ROUTINE)         
CKKWERR  J     SETCCNEQ            NOT EQUAL                                    
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
         DC    AL1(03),CL7'FSI=   ',AL4(EDITFSI)                                
         DC    AL1(02),CL7'OR=    ',AL4(EDTORATE)                               
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
*NOP*    DC    AL1(04),CL7'IMPS=  ',AL4(EDTIMP)                                 
         DC    AL1(05),CL7'EIMPS= ',AL4(EDTIMP)        SAME AS IMPS             
         DC    AL1(05),CL7'AIMPS= ',AL4(EDTAIMP)                                
         DC    AL1(06),CL7'EXDATE=',AL4(EDTEXDT)                                
******** DC    AL1(02),CL7'LW=    ',AL4(EDTLW)                                  
         DC    AL1(05),CL7'TRAFF= ',AL4(EDTTRAFF)                               
         DC    AL1(03),CL7'ISS=   ',AL4(EDTISSNM)                               
         DC    AL1(04),CL7'ADID=  ',AL4(EDTADID)                                
         DC    AL1(02),CL7'PO=    ',AL4(EDTPO#)                                 
         DC    AL1(02),CL7'COS2=  ',AL4(EDTCOS2)                                
         DC    X'FF'                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTADIDP NTR1  BASE=*,LABEL=*      VALIDATE AD-ID CHANGE (AD-ID ONLY)           
*                                                                               
         LA    R4,8+5(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,5(R2)                                                         
         AHI   R5,-5               ADJUST INPUT LENGTH                          
         BNP   EDTADERR                                                         
         CHI   R5,12                                                            
         BNE   EDTADERR            MUST BE EXACTLY 12 CHARACTERS                
*                                                                               
         MVC   WORK(L'KEY+L'KEY),KEY         SAVE KEY AND KEYSAVE               
*                                                                               
         LA    R3,NFNDERR                                                       
         XC    KEY,KEY             LOOK FOR PASSIVE POINTER                     
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'C1'         AD ID PASSIVE                                
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         MVC   KEY+10(12),0(R4)    12 BYTES FOLLOWING ADID=                     
*                                                                               
         MVC   KEYSAVE,KEY         FOR COMPARISON LATER                         
*                                                                               
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEY                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   EDTADERR            MUST BE FOUND                                
*                                                                               
         L     R6,AJOBIO           ADDRESS OF JOBREC                            
         USING PJOBRECD,R6                                                      
*                                  GET THE JOB RECORD                           
         GOTOR VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEY+27,0(R6),(TERMNAL,DMWORK)                                    
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                MUST FIND JOB RECORD                         
*                                                                               
         MVC   KEY(L'KEY+L'KEY),WORK     RESTORE KEY AND KEYSAVE                
         XC    WORK,WORK           CLEAR                                        
*                                                                               
         LA    R3,INVERR                                                        
         CLI   PJOBKJOB,X'FF'      AD-ID WITHOUT JOB CODE ?                     
         BNE   EDTADERR            NO - INVALID OPTIONAL ENTRY                  
*                                                                               
         LA    R7,PBDJOB-NEWREC+REC                                             
         CLC   PBDJOB,PJOBKJOB   BUYREC JOB CODE EQ TO JOBREC JOB CODE?         
         BE    EDTADX              YES - DONE - NO CHANGE TO JOB CODE           
*                                                                               
         CLI   WSJIND,1            CAN'T CHG JOB IF WSJ BUY                     
         BNE   EDTAD10                                                          
         LA    R3,WSJERR                                                        
         B     EDTADERR                                                         
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
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'70'        NON-WEB INSERTION ORDER ELEM                 
         BRAS  RE,NXTELEM                                                       
         BE    EDTAD40             ERROR                                        
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'71'        WEB INSERTION ORDER ELEM                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTAD90                                                          
EDTAD40  LA    R3,JBDERR           CANNOT REMOVE AD NO IF IO THERE              
         B     EDTADERR                                                         
*                                                                               
EDTAD90  DS    0H                  SEND JOBCODE FOR PPBUY03 EDITING             
         MVC   WORK(6),PJOBKJOB    "NEW" JOB CODE (X'FF'....)                   
         MVI   WORK+6,OPT_JOBQ                                                  
         GOTOR VT41103,DMCB,(RC),(RA),EDTADCDQ,WORK                             
*                                                                               
EDTADX   J     SETCCEQ                                                          
*                                                                               
EDTADERR J     ERROR                                                            
*                                                                               
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* REFERENCE NUMBER GOES INTO PBREFEL                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTREF2  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,8+4(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         XC    WORK(10),WORK                                                    
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4               ADJUST INPUT LENGTH (FOR REF=)               
         BP    EDTREF4                                                          
*                                                                               
         MVI   WORK,X'FF'          SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         BE    EDTRFERR                                                         
         B     EDTREF10                                                         
*                                                                               
EDTREF4  CLC   AGYALPHA,=C'BS'                                                  
         BNE   EDTREF6                                                          
         CHI   R5,6                MAX IS CHARACTERS FOR BACKER                 
         BH    EDTRFERR                                                         
*                                                                               
EDTREF6  CHI   R5,10               SJR + OTHERS - MAX IS 10 CHARS               
         BH    EDTRFERR                                                         
         MVC   WORK(10),0(R4)                                                   
*                                                                               
EDTREF10 LA    R5,NEWREC+33        PREPARE TO BUILD ELEM                        
         MVI   ELCODE,X'83'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTREF20            MUST ADD ONE                                 
         LA    R3,INVERR                                                        
         B     EDTRFERR            CAN'T ALREADY HAVE REF ELEM                  
*                                                                               
EDTREF20 XC    WORK+15(15),WORK+15                                              
         MVC   WORK+15(2),=X'830F' CODE AND LENGHT                              
         MVC   WORK+17(10),WORK    REF NUMBER OR X'FF'                          
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+15,(R5)                              
*                                                                               
EDTRFX   J     SETCCEQ             EQUAL                                        
EDTRFERR J     ERROR                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PLANCOST NTR1  BASE=*,LABEL=*      PROCESS PLANNED COST                         
*                                                                               
         CLI   BUYMD,C'S'          SEARCH?                                      
         JE    VBYPLC10                                                         
         CHI   R1,0                PLANNED COST OVERRIDE?                       
         JNE   SETCCEQ                                                          
*                                                                               
* MAGAZINE AND TRADE USE OLD PLANNED COST STYLE                                 
*                                                                               
         MVI   PBDPLCOS,X'FF'      EDIT PLANNED COST, SET PC=NONE               
         CLC   =C'NONE',11(R2)                                                  
         JE    VBYPLC06                                                         
         LA    R3,INVERR                                                        
         ZIC   R0,5(R2)            LENGTH OF FIELD                              
         AHI   R0,-3               SUBTRACT FOR PREFIX                          
         JNP   ERROR                                                            
         GOTO1 VCASHVAL,DMCB,(2,11(R2)),(R0)                                    
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         ICM   RF,15,4(R1)                                                      
         JZ    ERROR                                                            
         STCM  RF,15,PBDPLCOS                                                   
*                                                                               
VBYPLC06 MVC   TRCODE,=C'RZ'       SET FOR EXPANDED RECALL                      
         J     SETCCEQ                                                          
         EJECT                                                                  
*                                                                               
VBYPLC10 CHI   R1,0                PLANNED COST OVERRIDE?                       
         BE    *+10                                                             
         SR    R3,R3                                                            
         B     VBYPLC20                                                         
*                                                                               
         LHI   R3,INVERR                                                        
         ZIC   RE,5(R2)                                                         
         SHI   RE,3                ADJUST INPUT LENGTH                          
         CHI   RE,0                SUPPRESSING OR REMOVING PC?                  
         JL    ERROR                                                            
         CHI   RE,15+3             GREATER THAN ALLOWED MAX VALUE?              
         BNH   *+12                                                             
         LHI   R3,DEMAXERR                                                      
         J     ERROR                                                            
*                                                                               
         LA    R3,3+8(R2)          POINT TO START OF PC INPUT                   
VBYPLC20 MVI   DMCB,X'18'                                                       
         BRAS  RE,LOADOVLY                                                      
         GOTOR (RF),DMCB,(RC),(RA),('VAL_PLCQ',(R3))                            
         ICM   R3,15,DMCB+8                                                     
         LTR   R3,R3               CAME BACK WITH ERROR?                        
         JNZ   ERROR                                                            
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
         DROP  RB                                                               
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
         B     EDTISERR                                                         
*                                                                               
EDTIS05  DS    0H                                                               
         MVI   DUB+1,X'FF'         SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         BE    EDTISERR                                                         
         B     EDTIS20             MUST BE REMOVING ELEM THEN                   
*                                                                               
EDTIS10  DS    0H                                                               
         CHI   R5,11               GREATER THAN MAX OF 11?                      
         BH    EDTISERR                                                         
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
         BE    EDTISERR            CAN'T ALREADY HAVE ELEM                      
*                                                                               
         CLI   DUB+1,X'FF'         SPECIAL CODE PRESENT?                        
         BNE   *+8                                                              
         MVI   WORK+2,X'FF'        MOVE IN DELETION CODE                        
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
*                                                                               
EDTISX   J     SETCCEQ             EQUAL                                        
EDTISERR J     ERROR                                                            
*                                                                               
EDTISRRR LA    R3,ISSNMERR                                                      
         B     EDTISERR                                                         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
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
         B     CHKLWERR            NEGATIVE INPUT LENGTH, ERROR                 
*                                                                               
CHKLW10  MVI   DUB+1,X'FF'         SPECIAL DELETE CODE                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'          ADDING EMPTY ELEM ON NEW BUY?                
         BE    CHKLWERR                                                         
         B     CHKLW60             MUST BE REMOVING ELEM THEN                   
*                                                                               
CHKLW30  CHI   R5,3                CHECK IF "DEL" IS ENTERED                    
         BH    CHKLW30H                                                         
         CLC   =C'DEL',0(R4)       DEL IS ENTERED?                              
         BE    CHKLW10                                                          
*                                                                               
CHKLW30H CHI   R5,5                MORE THAN 5 CHARS?                           
         BH    CHKLWERR                                                         
         CHI   R5,4                NONE IS ENTERED?                             
         BNE   CHKLW30L                                                         
         CLC   =C'NONE',0(R4)      X=NONE FOR LEGAL WARNING CODE?               
         BNE   CHKLWERR                                                         
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
         BNE   CHKLWERR                                                         
         CHI   R5,1                                                             
         BH    CHKLWERR            ONLY 1 CHAR INPUT FOR "X"                    
         B     CHKLW37                                                          
*                                                                               
CHKLW35  CHI   R5,1                INPUT LENGTH IS ONE?                         
         BE    CHKLW37             YES, NO NEED TO CHK QUARTERLY CODE           
         CHI   R5,2                INPUT LENGTH IS TWO?                         
         BE    CHKLW35F            YES, GO CHECK QUARTERLY CODE                 
         CLC   =C'NONE',1(R4)      X=NONE FOR QUARTERLY CODE?                   
         BNE   CHKLWERR                                                         
         MVC   DUB+2(1),0(R4)      LEGAL WARNING CODE                           
         MVI   DUB+3,C'X'          QUARTERLY CODE (X)                           
         B     CHKLW60                                                          
*                                                                               
CHKLW35F CLI   1(R4),C'X'          X=NONE?                                      
         BE    CHKLW35H                                                         
         CLI   1(R4),C'1'                                                       
         BL    CHKLWERR            LOWER THAN CHAR 1 IS NO GOOD                 
         CLI   1(R4),C'4'                                                       
         BH    CHKLWERR            HIGHER THAN CHAR 4 IS ALSO NO GOOD           
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
         BE    CHKLWERR            CAN'T ALREADY HAVE ELEM                      
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
CHKLWX   J     SETCCEQ             EQUAL                                        
CHKLWERR J     ERROR                                                            
*                                                                               
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
         B     CHKNTERR                                                         
*                                                                               
         LA    R4,14(R2)           POINT TO INPUT                               
         LA    R3,INVERR           INVALID INPUT ERROR                          
         ZIC   R5,5(R2)                                                         
         AHI   R5,-6               MINUS 6 BYTE ANNOTATION (TRAFF=)             
*                                                                               
         CHI   R5,0                ANY INPUTS?                                  
         BNE   *+8                                                              
         B     CHKNTERR                                                         
*                                                                               
         CHI   R5,3                                                             
         BNE   *+18                                                             
         CLC   =C'YES',0(R4)       TRAFF=YES?                                   
         BE    CHKNT50                                                          
         B     CHKNTERR                                                         
*                                                                               
         CHI   R5,2                                                             
         BNE   *+18                                                             
         CLC   =C'NO',0(R4)        TRAFF=NO?                                    
         BE    CHKNT50                                                          
         B     CHKNTERR                                                         
*                                                                               
         CHI   R5,1                                                             
         BNE   CHKNTERR            NO OTHER VALID LENGTH                        
         CLI   0(R4),C'Y'          TRAFF=Y?                                     
         BE    CHKNT50                                                          
         CLI   0(R4),C'N'          TRAFF=N?                                     
         BE    CHKNT50                                                          
         B     CHKNTERR                                                         
*                                                                               
CHKNT50  CLI   0(R4),C'Y'          TRAFFIC STATUS IS YES?                       
         BNE   *+12                                                             
         NI    PBDSTAT,X'FF'-X'20'                                              
         B     CHKNTRAX                                                         
         OI    PBDSTAT,X'20'       STATUS IS NO TRAFFIC                         
*                                                                               
CHKNTRAX J     SETCCEQ             EQUAL                                        
CHKNTERR J     ERROR                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADBYER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DDLINKSW,C'D'       DELETE INSERTION UPLOAD?                     
         BE    CKADBERR            NO NEED TO PROC OPTIONAL DATA FLDS           
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD?                      
         BE    CKADB50                                                          
         CLI   DDLINKSW,C'N'       NEW INSERTION UPLOAD?                        
         BE    CKADB50                                                          
         CLI   DDLINKSW,C'C'       CHANGE INSERTION UPLOAD?                     
         BNE   CKADBERR                                                         
*                                                                               
CKADB50  XC    DMCB(24),DMCB       CALL T41118 TO PROC WORKER ELEM              
         MVI   DMCB,X'18'                                                       
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         XC    DMCB(24),DMCB       CALL T41119 TO PROC CUST COLUMN              
         MVI   DMCB,X'19'                                                       
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'03'                                                       
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT41103,0(R1)       REESTABLISH ADDRESS OF T41103                
*                                                                               
CKADBX   J     SETCCEQ             EQUAL                                        
CKADBERR J     SETCCNEQ            NOT EQUAL (T41118 IS NOT CALLED)             
*                                                                               
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
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTFSI   NTR1  BASE=*,LABEL=*      EDIT FSI                                     
*                                                                               
EDTFSI2  LA    R4,8+5(R2)          POINT TO INPUT                               
         LA    R3,INVERR                                                        
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4               ADJUST INPUT LENGTH (FOR FSI=)               
         BP    EDTFSI4                                                          
*                                                                               
         MVI   DUB+1,C'X'          SPECIAL DELETE CODE                          
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
         JNE   ERROR               MUST GET REMAINDER 0                         
*                                                                               
EDTFSI10 LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'82'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTFSI20            MUST ADD ONE                                 
*                                                                               
EDTFSI12 LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE FSI ELEM                  
*                                                                               
EDTFSI20 LA    R4,WORK+10                                                       
         USING PBFSIELD,R4                                                      
         XC    PBFSIEL(20),PBFSIEL                                              
         MVC   PBFSIEL(2),=X'820D' CODE AND LENGHT                              
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
         J     SETCCEQ                                                          
*                                                                               
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTMAN   NTR1  BASE=*,LABEL=*      EDIT MANUAL IO                               
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
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK),(3,PIODATE)                                
*                                                                               
EDTMN10  DS    0H                  NOW EDIT IO NUMBER 4 DIGITS                  
         LA    R5,1(R5)            GET PAST ,                                   
         SR    R1,R1                                                            
         XC    X+60(5),X+60                                                     
         LA    R4,X+60                                                          
EDTMN15  CLI   0(R5),C','                                                       
         BE    EDTMN20             IO NUMBER INVALID                            
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
         MVC   PIONUM+2(2),=X'0101'                                             
*                                                                               
* SET POSITIONAL NUMBER AND NUMBER OF BUYS TO 1                                 
*                                                                               
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
EDTMN40  TM    PBUYCNTL,X'80'                                                   
         BNZ   EDTMANE5            CAN'T BE DELETED BUY                         
EDTMN50  MVI   PIOTURN,C'M'        MANUAL                                       
         MVC   PIOIDATE,PBUYKDAT                                                
         MVC   PIOJOB,PBDJOB                                                    
         MVC   PIOSPACE,PBDSPACE                                                
         XC    PBDIODAT,PBDIODAT                                                
*                                                                               
* NOTE - MANIO ELEMS GO INTO NEWREC                                             
* ON CHANGES CHG ROUTINE WILL FIND AND MOVE THEM                                
*                                                                               
         LA    R4,NEWREC                                                        
         LA    R5,33(R4)                                                        
         MVI   ELCODE,X'70'                                                     
*                                  NOW ADD ELEM TO RECORD                       
EDTMN55  BRAS  RE,NXTELEM                                                       
         BNE   EDTMN60                                                          
         OC    2(3,R5),2(R5)       CHK FOR REAL IO                              
         BNZ   EDTMN55             YES                                          
         MVC   0(50,R5),X          NO JUST MOVE IN MANUAL IO                    
         B     EDTMN70                                                          
*                                                                               
EDTMN60  GOTO1 VRECUP,DMCB,(1,0(R4)),X,(R5)                                     
         B     EDTMANX                                                          
*                                                                               
EDTMN70  MVC   TRCODE,=C'RI'       SET FOR INSERTION RECALL                     
*                                                                               
EDTMANX  J     SETCCEQ                                                          
*                                                                               
EDTMANE1 LA    R3,INVERR           CAN'T HAVE * IN SPACE                        
         J     ERROR                                                            
EDTMANE2 LA    R3,IODERR4          MUST HAVE JOB                                
         J     ERROR                                                            
EDTMANE3 LA    R3,INVDTERR         IO DATE ERROR                                
         J     ERROR                                                            
EDTMANE4 LA    R3,IONUMER          IO NUMBER ERROR                              
         J     ERROR                                                            
EDTMANE5 LA    R3,IOTYPER          TYPE ERROR                                   
         J     ERROR                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPACK  NTR1  BASE=*,LABEL=*      EDIT PACK DATA                               
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
         B     EDTPAC2X            ZERO ENTERED, SET SPECIAL DEL CODE           
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
         BRAS  RE,NXTELEM                                                       
         BNE   EDTPAC20            MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE ELEM                      
*                                                                               
EDTPAC20 XC    WORK+10(10),WORK+10                                              
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
EDTPAC24 MVC   WORK+12(2),DUB+6    PACK DIGITS (PL2)                            
         XC    WORK+14(3),WORK+14  JUST IN CASE                                 
*                                                                               
EDTPAC25 GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
EDTPAC30 J     SETCCEQ                                                          
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSR    NTR1  BASE=*,LABEL=*      EDIT SPECIAL REP CODE                        
*                                                                               
         L     R1,TRADDR                                                        
         CLI   8(R1),C'B'          SEE IF BUYING                                
         BE    EDTSR2                                                           
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7                                   
         OC    PGROSS,PGROSS       PAYMENTS NET TO ZERO?                        
         BZ    EDTSR2                                                           
         LA    R3,SRPDERR                                                       
         J     ERROR                                                            
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
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    EDTSR10             FOUND - OK                                   
*                                                                               
         LA    R3,NFNDERR                                                       
         J     ERROR                                                            
*                                                                               
EDTSR10  DS    0H                  NOW STORE IN PBSREPEL                        
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTSR20             MUST ADD ONE                                 
*                                                                               
         LA    R3,INVERR                                                        
         J     ERROR               CAN'T ALREADY HAVE REP ELEM                  
*                                                                               
EDTSR20  XC    WORK+10(10),WORK+10                                              
         MVC   WORK+10(2),=X'800A' CODE AND LENGHT                              
         MVC   WORK+12(4),WORK     REP CODE OR C'X'                             
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK+10,(R5)                              
*                                                                               
EDTSR30  J     SETCCEQ                                                          
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTRTM   NTR1  BASE=*,LABEL=*      EDIT MAGAZINE RATES                          
*                                                                               
         LA    R3,INVRTERR                                                      
         BRAS  RE,BUMPFLD                                                       
         MVI   PBDCTYP,0                                                        
         MVI   PBDCOSIN,C' '                                                    
         NI    PBDRLIND,X'F7'      SET OFF FROZEN BIT                           
         CLI   5(R2),0                                                          
         JE    SCCEQ_R                                                          
         SR    R7,R7                                                            
         IC    R7,5(R2)            GET LENGTH                                   
         LA    R6,8(R2)            SET ADDRESS                                  
         TM    4(R2),X'04'         TEST VALID ALPHA                             
         BNZ   EDTR7               (FREE, ETC)                                  
*                                                                               
EDTR3    CLI   0(R6),C'S'          GROSS=NET?                                   
         BNE   EDTR3A                                                           
         ZAP   PBDACP,=P'1'        TO STOP AC LOOK-UP, WILL RESET TO 0          
         MVI   PBDCOSIN,C'S'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
EDTR3A   CLI   0(R6),C'R'          ROADSIDE?                                    
         BNE   EDTR3C                                                           
         ZAP   PBDACP,=P'16667'    SET AC RATE                                  
         MVI   PBDCOSIN,C'R'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
EDTR3C   CLI   0(R6),C'C'          COMMISSION ONLY?                             
         BNE   EDTR4                                                            
         CLI   PBDCOSIN,C' '                                                    
         JNE   ERROR                                                            
         MVI   PBDCOSIN,C'C'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
*                                                                               
EDTR4    CLI   0(R6),C'N'          NET TO BE GROSSED UP?                        
         BNE   EDTR5                                                            
         MVI   PBDCTYP,C'N'                                                     
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTR3                                                            
*                                                                               
EDTR5    CLI   0(R6),C'*'          FROZEN RATE?                                 
         BNE   EDTR7                                                            
         OI    PBDRLIND,X'08'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R7,R0                                                            
         B     EDTR3                                                            
*                                                                               
EDTR7    CLI   SVESPROF+28,C'C'    'C' RATE ESTIMATE?                           
         BNE   EDTR8                                                            
         CLI   PBDCOSIN,C'C'       'C' INPUT?                                   
         BE    EDTR10                                                           
         CLI   PBDCOSIN,C' '                                                    
         JNE   ERROR                                                            
         MVI   PBDCOSIN,C'C'       NO INPUT - SET TO 'C'                        
         B     EDTR10                                                           
*                                                                               
EDTR8    CLI   PBDCOSIN,C'C'       'C' RATE ONLY FOR 'C' RATE EST?              
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
         GOTO1 VCASHVAL,DMCB,(R6),(R7)                                          
         CLI   0(R1),X'FF'                                                      
         JE    ERROR                                                            
         L     R0,4(R1)                                                         
         C     R0,=F'849999999'     MAX 8,499,999.99?                           
         JH    ERROR                                                            
*                                                                               
         CVD   R0,DUB                                                           
         ZAP   PBDCOS,DUB                                                       
         LTR   R0,R0                                                            
         BNZ   *+10                                                             
         ZAP   PBDCOS,=P'1'        LIE ABOUT RATE FOR NOW                       
*                                                                               
         TM    ABUPLDSW,ABPLCOSQ   HAVE PLANNED COST MAP CODE?                  
         JNZ   SCCEQ_R                                                          
         CLI   BUYMD,C'S'          SEARCH?                                      
         JNE   SCCEQ_R                                                          
         L     RE,TRADDR                                                        
         CLI   8(RE),C'C'          BUY CHANGE TRANSACTION?                      
         JNE   SCCEQ_R                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM                                                       
         JNE   SCCEQ_R                                                          
         LR    R0,R2               SAVE CURRENT FIELD POINTER                   
         LHI   RF,4                                                             
         BRAS  RE,BUMPFLDS         POINT TO FIRST OPTIONAL DATA FIELD           
         LA    RF,5                                                             
EDTR46   CLC   =C'PC=',8(R2)       PLANNED COST ENTERED?                        
         BNE   *+10                                                             
         LR    R2,R0               RESTORE CURRENT FIELD POINTER                
         J     SCCEQ_R                                                          
         BRAS  RE,BUMPFLD          BUMP TO NEXT OPTIONAL DATA FIELD             
         BCT   RF,EDTR46                                                        
         LR    R2,R0               RESTORE CURRENT FIELD POINTER                
         USING BYPCELD,R5                                                       
         CP    BYPCCST,=P'0'       'FREE' PLANNED COST?                         
         JE    SCCEQ_R                                                          
         CP    PBDCOS,=P'1'        RATE IS FREE?                                
         JE    SCCEQ_R                                                          
         CLC   BYPCIND,PBDCOSIN    PC COST INDICATORS SAME AS RATE?             
         BNE   EDTR_ER2                                                         
         CLC   BYPCNIND,PBDCTYP    PC COST INDICATORS SAME AS RATE?             
         BNE   EDTR_ER2                                                         
         CLC   BYPCTYP,PBDCOSTY    PC COST INDICATORS SAME AS RATE?             
         BNE   EDTR_ER2                                                         
         J     SCCEQ_R                                                          
*                                                                               
EDTR_ER2 LA    R3,RTINCONS         RATE TYPES ARE INCONSISTENT                  
         J     ERROR                                                            
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTRTM   NTR1  BASE=*,LABEL=*      ROUTINE TO FORMAT RATES                      
*                                                                               
         L     R2,TRADDR                                                        
         LHI   RF,4                                                             
         BRAS  RE,BUMPFLDS           TO RATE                                    
         CLI   5(R2),0                                                          
         JNE   EXIT_X                                                           
         MVI   X,C' '                                                           
         MVC   X+1(L'X-1),X                                                     
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         CLI   PBDCTYP,C'N'        NET INPUT SO DISPLAY AS NET                  
         BE    *+12                                                             
         CLI   PBDCOSIN,C'R'       ROADSIDE (DISPLAY AS NET)?                   
         BNE   FMTRTM4                                                          
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               =NET PCT                                     
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
*                                                                               
FMTRTM4  EDIT  (R1),(9,X+1),2,ALIGN=LEFT,FLOAT=-                                
         LA    R1,X+1                                                           
         CLI   PBDCOSIN,C' '       TEST DEFAULT COST TYPE                       
         BE    FMTRTM6                                                          
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCOSIN                                                 
*                                                                               
FMTRTM6  CLI   PBDCTYP,C'N'        DISPLAY AS NET                               
         BNE   FMTRTM9                                                          
         BCTR  R1,0                                                             
         MVC   0(1,R1),PBDCTYP                                                  
*                                                                               
FMTRTM9  DS    0H                                                               
         TM    PBDRLIND,X'08'      TEST FROZEN                                  
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
*                                                                               
         MVC   8(10,R2),0(R1)                                                   
         CP    PBDCOS,=P'0'                                                     
         BNE   FMTRTMX                                                          
*                                                                               
         XC    8(10,R2),8(R2)                                                   
         CLI   PBDCOSIN,C'S'       COST INDICATOR "S"?                          
         BE    *+12                                                             
         CLI   PBDCOSIN,C'R'       COST INDICATOR "R"?                          
         BNE   FMTRTMF                                                          
         MVC   8(1,R2),PBDCOSIN                                                 
         MVC   9(4,R2),=C'FREE'                                                 
         B     FMTRTMX                                                          
FMTRTMF  DS    0H                                                               
         MVC   8(4,R2),=C'FREE'                                                 
*                                                                               
FMTRTMX  DS    0H                                                               
         MVI   7(R2),0                                                          
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         J     EXIT_X                                                           
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKUPLID  NTR1  BASE=*,LABEL=*      EDIT UPLOAD ID, DEFAULT ERR                  
*                                                                               
         LA    R3,INVERR                                                        
         CLI   MADSW,C'Y'          PBU?                                         
         JE    ERROR                                                            
         OC    UPID,UPID           ALREADY HAVE UPLOAD ID?                      
         JNZ   ERROR                                                            
         CLI   5(R2),5                                                          
         JNH   ERROR                                                            
         CLI   5(R2),13            MAX (UPID=XXXXXXXX)?                         
         JH    ERROR                                                            
         LLC   R1,5(R2)                                                         
         AHI   R1,-(5+1)          5 FOR 'UPID=' AND 1 FOR EX                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   UPID(0),13(R2)                                                   
         OC    UPID,SPACES                                                      
         MVI   UPLSW,C'Y'                                                       
         STCM  R2,15,UPCURSOR      SAVE OPTIONAL DATA LINE ADDRESS              
         J     SETCCEQ                                                          
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* EDIT PST OVERRIDE, FORMAT IS PST/PP=X (PP=PROVINCE CODE, X=PST CODE)          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,INVERR                                                        
         CLI   NATION,C'C'         CANADIAN AGY?                                
         JNE   ERROR                                                            
*                                                                               
         LHI   R3,NOFRZCER         NO FINANCIAL CHANGES TO FRZ CLIENT           
         BRAS  RE,CKCLTFRZ         CLIENT FROZEN OPTION FOUND?                  
         JNE   ERROR                                                            
*                                                                               
         LA    R3,INVERR                                                        
         LA    R4,8+4(R2)          POINT TO INPUT                               
         ZIC   R5,5(R2)                                                         
         AHI   R5,-4                                                            
         JNP   ERROR                                                            
*                                                                               
         CHI   R5,4                LENGTH AFTER 'PST/'?                         
         JH    ERROR               MAX IS ONE PROVINCE                          
*                                                                               
         XC    WORK,WORK           BUILD FIELD HEADER FOR PSTVAL                
         MVI   WORK,48                                                          
         STC   R5,WORK+5                                                        
         AHI   R5,-1                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(R4)     EXECUTED                                     
*                                                                               
         XC    WKELEM(PSTLNQ),WKELEM                                            
         LA    R5,WKELEM                                                        
         USING PSTBLKD,R5                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R0,WORK                                                          
         ST    R0,PSTADIN          INPUT ADDRESS                                
         XC    WKELEM+50(20),WKELEM+50                                          
         LA    R0,WKELEM+50                                                     
         ST    R0,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R5)                                                   
         CLI   PSTERR,0                                                         
         JNE   ERROR                                                            
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   EDTPST20                                                         
*                                                                               
         MVC   2(10,R5),WKELEM+50  ELEM EXISTS - JUST MOVE IN NEW DATA          
         B     EDTPST30                                                         
*                                                                               
EDTPST20 XC    WKELEM(25),WKELEM                                                
         MVC   WKELEM+0(02),=X'840C'                                            
         MVC   WKELEM+2(10),WKELEM+50                                           
         GOTO1 VRECUP,DMCB,(1,NEWREC),WKELEM,0(R5)                              
*                                                                               
EDTPST30 J     SETCCEQ                                                          
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPCT   NTR1  BASE=*,LABEL=*      EDIT PERCENT                                 
*                                                                               
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
         CP    0(0,R5),DUB         TEST INPUT NUMBER TOO LARGE                  
         JNE   ERROR                                                            
*                                                                               
         MVC   TRCODE,=C'RZ'                                                    
         J     SETCCEQ                                                          
*                                                                               
ACSSERR  LA    R3,FACCERR                                                       
         J     ERROR                                                            
*                                                                               
FLDINV   LA    R3,INVERR                                                        
         J     ERROR                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                                                               
*                                                                               
GSTTAB   DC    C'SXZT',X'FF'                                                    
CMPMSG   DC    C'** ACTION COMPLETED **'                                        
LSTMSG   DC    C'ENTER ''R'' TO CONTINUE LIST'                                  
*                                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK11D  DSECT                                                                  
*                                                                               
RELOBY11 DS    F                                                                
SV_REG_E DS    F                                                                
SV_REG_3 DS    F                                                                
*                                                                               
SPACES   DS    CL255                                                            
*                                                                               
LKUPKEY  DS    XL16                LOCKUP KEY                                   
*                                                                               
WKELEM   DS    XL256                                                            
WKSVKEY  DS    XL(L'KEY)                                                        
WKSVAIO  DS    XL(L'AREC)                                                       
*                                                                               
WORK11X  EQU   *                                                                
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
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108PPBUY11   06/02/20'                                      
         END                                                                    
