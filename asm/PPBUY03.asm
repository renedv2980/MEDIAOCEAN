*          DATA SET PPBUY03    AT LEVEL 097 AS OF 02/11/20                      
*PHASE T41103A                                                                  
*INCLUDE PPRTLOOK                                                               
*INCLUDE GETADVC                                                                
*INCLUDE SIXPACK                                                                
*INCLUDE PPSPCVAL                                                               
*INCLUDE PPFRGTAB                                                               
*INCLUDE PPGETSPC                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY03 - SUBROUTINES FOR BUY/CHANGE/DELETE'                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 10/08/19 MQ MESSAGE FIX (BLANKING OUT COLUMNS, SEE IAPP-237661)          
*                                                                               
* KWAN 04/15/19 FIX PRISMA UPLOAD ELEMENT WHEN REUSING LINE NUMBER              
*                                                                               
* KWAN 01/31/19 FOR SAME DAY SAME PID CHANGES, ONLY SAVE FIRST AMOUNT           
*                                                                               
* KWAN 09/19/14 PRISMA/RADIA ORIGIN                                             
*                                                                               
* KWAN 03/10/14 SUPPORT MEDIA L (SOCIAL)                                        
*                                                                               
* KWAN 10/22/13 CORRECT COS2 RATE GROSS CALCULATION                             
*                                                                               
* KWAN 04/15/13 MARKING PURCHASE ORDER REC BEING USED BY LEVEL                  
*                                                                               
* KWAN 03/01/12 COS2 FACTOR FIX FOR PRINTPAK BUY WITHIN COS2 $ MODULE           
*                                                                               
* KWAN 09/28/11 COS2 $ RATE - FIX NET CALCULATION IF AC IS ZERO                 
*                                                                               
* BOBY 04/20/11 TRACK CUSTCOL CHANGES FOR IDESK                                 
*                                                                               
* KWAN 04/07/11 RETOOLED IDESK UPLOAD ELEM (SEE PBYDKST2)                       
*                                                                               
* KWAN 04/05/11 NO NEW BUYS & NO DELETIONS FOR IDESK ESTIMATES                  
*                                                                               
* KWAN 03/04/11 SET NO INVOICE STATUS IF NO INVOICE ITEM LINKS PRESENT          
*                                                                               
* KWAN 10/14/10 COS2 $ RATE - IF NET ENTERED, CALCULATE GROSS                   
*                                                                               
* KWAN 07/27/10 INSERTION DATE CHANGE - UPDATE LINKED INVOICE ITEM              
*                                                                               
* KWAN 05/20/10 DISALLOW INSERTION DATE CHANGE IF INVOICES PRESENT              
*                                                                               
* KWAN 03/18/10 PLANNED COST FIX - NO NEED TO GROSS FOR FREE BUYS               
*                                                                               
* SMYE 11/23/09 PREVENT SUBLINE NUMBER GREATER THAN 255 ON DATE CHANGE          
*                                                                               
* KWAN 08/18/09 TRAP RATE CHAGE FOR LIMIT ACCESS USERS                          
*                                                                               
* KWAN 08/06/09 ONLY 1 INVOICE MATCHING STATUS ELEM FOR ADBUYER UPLOAD          
*                                                                               
* SMYE 07/17/08 RE-INPUT BELOW DUE TO "WRONG" VERSION MADE LIVE 6/26/08         
*      02/01/08 PREVENT SUBLINE NUMBER GREATER THAN 255 (AT ADDLINE3)           
*                                                                               
* KWAN 02/20/08 MASTER AD CODE VALIDATION                                       
*                                                                               
* BOBY 02/00/08 RETRIEVE FX RATE FROM CONTROL SYSTEM                            
*               CALCULATE FX AMOUNT FOR BUY                                     
*                                                                               
* KWAN 10/04/07 CREATE IDESK INSERTION UPLOAD ELEM                              
*                                                                               
* KWAN 09/25/06 CREATE PURCHASE ORDER ELEM                                      
*                                                                               
* KWAN 09/17/06 MARK ACTIVITIES FOR REQUEST AND RECEIVE INVOICE                 
*                                                                               
* KWAN 02/21/06 STEWARDSHIP INSERTION                                           
*                                                                               
* KWAN 01/24/06 ASC FIX (AAORTAB WAS NOT PROPERLY POINTED TO)                   
*                                                                               
* SMYE 12/05    CHANGES FOR AD-ID ONLY (NO "NORMAL" JOB CODE)                   
*                                                                               
* KWAN 11/07/05 DON'T REUSE LINE NUMBER IF ESR ELEM PRESENT                     
*                                                                               
* KWAN 10/21/05 DON'T REUSE LINE NUMBER IF EIO ELEM PRESENT                     
*                                                                               
* KWAN 10/19/05 CHECK FOR LOCK WHEN CHANGING INSERTION DATE                     
*                                                                               
* KWAN 09/01/05 NO ADDING INSERTIONS FOR LOCKED CLIENT                          
*                                                                               
* KWAN 11/24/04 NO INSERTION MONTH CHANGE FOR WEBIO INSERTIONS                  
*                                                                               
* SMYE 11/04    NO-OP PBDSDATE IN 10/04 CHANGE BELOW                            
*                                                                               
* SMYE 10/04    FOR PBDCDATE, PBDSDATE, & PBDMDATE CHANGE NON-WORK DAY          
*                                                                               
* KWAN 09/14/04 ADJUST AD CODE VALIDATION FOR ADBUYER UPLOADS                   
*                                                                               
* KWAN 03/19/04 CLE REFRESH FOR ADBUYER, NEED TO REPLY CALC'D VALUE             
*                                                                               
* KWAN 01/15/04 REPLACED XCEF WITH MVCL                                         
*                                                                               
* KWAN 01/24/03 ADBUYER - REFRESH MODE, RETURN CHANGED INS DATE                 
*               ALSO CORRECT SOME "CONTINUATION" ASSEMBLY PROBLEMS              
*                                                                               
* KWAN 11/21/01 ADBUYER - DRAFT INS UPLOAD MODE, DON'T ADD RECORD               
*                                                                               
* BPLA 03/08/01 ACTIVATE SERIAL NUMBERS                                         
*                                                                               
* KWAN 02/14/01 ACTIVATE SERIAL # AND REMOVE PROFILE CHECKING                   
*                                                                               
* BPLA 01/01    SERIAL # CODE NO-OPED FOR NOW                                   
*               SO IT CAN BE RELINKED WITH NEW RATELOOK                         
*                                                                               
* BPLA 01/01    CODE TO SUPPORT SERIAL NUMBERS                                  
*               ADDS ELEMENT TO BUY AND PASSIVE POINTERS                        
*               GENERATION CONTROLLED BY BY PROFILE                             
*                                                                               
* BPLA 05/99    ALLOW COMMENT CHANGES TO DELETED BUYS                           
*               (IF BY PROFILE+5 ALLOWS IT)                                     
*                                                                               
* BPLA 05/99    SAVE AND RESTORE PBDUNITS AROUND CALL TO RATELOOK               
*               FOR OPEN RATES ON CONTRACTS - THEY MIGHT GET                    
*               CLOBBERED FOR FINANCIAL WSJ BUYS WHEN NO CONTRACT               
*               OPEN RATE IS FOUND                                              
*                                                                               
* BPLA 12/98    CHANGES FOR INTERACTIVE AND MAGAZINE LIST BUYING                
*               IN EDTJOB                                                       
*                                                                               
* BPLA 10/98    COPY OF PPBUY03A (LEVEL 15 8/18/98)                             
*               COPY MADE 10/28/98                                              
*               INCLUDE XPRTLOOK AND REACTIVATE OPEN RATE                       
*               CONTRACT LOOK-UP                                                
*                                                                               
* BPLA 07/98    ATTEMPT TO CATCH VERY LARGE UNIT RATES                          
*               RETURNED BY RATELOOK - THIS IS USUALLY                          
*               CAUSED BY THEIR FORGETTING TO APPEND AN                         
*               'L' TO LINE SPACES                                              
*                                                                               
* BPLA 06/98    NO-OP CONTRCT OPEN RATE LOOK-UP FOR NOW                         
*                                                                               
* BPLA 06/98    CLEAR MORE OF SECBLK (FOR MORE PRODUCT ADJ)                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41103   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK03X-WORK03D,T41103,RR=RE,CLEAR=YES                           
*                                                                               
         LR    R7,RC                                                            
         USING WORK03D,R7          R7 = A(GLOBAL STORAGE)                       
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          R8 = A(GLOBAL LITERALS)                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         ST    RE,WKRELO03                                                      
         LR    RE,R7                                                            
         A     RE,=A(WK_AOR_A-WORK03D)                                          
         ST    RE,AAORTAB                                                       
*                                                                               
         L     R0,=V(PPSPCVAL)                                                  
         A     R0,WKRELO03                                                      
         ST    R0,VSPCVAL                                                       
         L     R0,=V(PPFRGTAB)                                                  
         A     R0,WKRELO03                                                      
         ST    R0,VFRGTAB                                                       
*                                                                               
         LR    RE,R7                                                            
         A     RE,=A(WKAIO1-WORK03D)                                            
         ST    RE,AWKAIO1                                                       
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         XC    IDSKDATE,IDSKDATE                                                
         XC    IDSKTIME,IDSKTIME                                                
         MVI   PRSMSTA1,0                                                       
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   SKIP IF NOT IDESK INSERTION UPLOAD?          
         BZ    INITWK70                                                         
         CLI   8+3(R1),SENDMQQ     SKIP IF SEND MQ CALL                         
         BE    INITWK70                                                         
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
INITWK20 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         BE    INITWK70                                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         BE    *+12                                                             
INITWK22 BRAS  RE,NXTWFELM                                                      
         B     INITWK20                                                         
         CLC   =AL2(D#CURDAT),3(R3)                                             
         BNE   INITWK24                                                         
* * * *  GOTO1 VPERVAL,DMCB,(10,6(R3)),(X'40',WKELEM)                           
* * * *  LA    RE,WKELEM                                                        
* * * *  USING PERVALD,RE                                                       
* * * *  MVC   IDSKDATE,PVALBSTA                                                
* * * *  DROP  RE                                                               
         GOTO1 VDATCON,DMCB,(10,6(R3)),(3,IDSKDATE)                             
INITWK24 CLC   =AL2(D#CURTIM),3(R3)                                             
         BNE   INITWK26                                                         
         PACK  DUB,6+0(2,R3)                                                    
         CVB   RE,DUB                                                           
         STC   RE,IDSKTIME+0       BINARY HOURS                                 
         PACK  DUB,6+2(2,R3)                                                    
         CVB   RE,DUB                                                           
         STC   RE,IDSKTIME+1       BINARY MINUTES                               
         PACK  DUB,6+4(2,R3)                                                    
         CVB   RE,DUB                                                           
         STC   RE,IDSKTIME+2       BINARY SECONDS                               
INITWK26 CLC   =AL2(D#PRMIVE),3(R3)                                             
         JNE   INITWK28                                                         
         CLI   6(R3),C'Y'          PRISMA INVOICE ENABLED CAMPAIGN?             
         JNE   INITWK28                                                         
         OI    PRSMSTA1,BYPRMIVQ                                                
         DROP  R3                                                               
*                                                                               
INITWK28 DS    0H                                                               
         J     INITWK22            NEXT MAP CODE IN UPLOAD RECORD               
*                                                                               
INITWK70 DS    0H                                                               
*                                                                               
INITWK_X DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LA    RF,ROUT_TAB         POINT TO ROUTINE TABLE                       
BY03_20  CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNDEFINED ROUTINE                            
         CLC   0(1,RF),8+3(R1)                                                  
         BNE   *+12                                                             
         ICM   RE,15,1(RF)         ADDRESS OF ROUTINE                           
         B     BY03_40                                                          
         LA    RF,L'ROUT_TAB(RF)   POINT TO NEXT TABLE ENTRY                    
         B     BY03_20                                                          
*                                                                               
BY03_40  A     RE,WKRELO03                                                      
         BR    RE                  BRANCH TO ROUTINE                            
*                                                                               
         DC    H'0'                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RCADEL_R BRAS  RE,CKADBELM         RECONSTRUCT ELEM FOR ADBUYER UPLOAD          
         B     EXXMOD                                                           
*                                                                               
ADDINS_R BRAS  RE,ADDLINE          ADD INSERTION RECORD                         
         B     EXXMOD                                                           
*                                                                               
AUTOSC_R BRAS  RE,ASC              AUTO SCHEDULE CHECKING                       
         B     EXXMOD                                                           
*                                                                               
AUTOSR_R BRAS  RE,ASR              AUTO SPACE RESERVATION                       
         B     EXXMOD                                                           
*                                                                               
BLDREC_R BRAS  RE,BLDREC           BUILD BUY RECORD                             
         B     EXXMOD                                                           
*                                                                               
CHGREC_R BRAS  RE,CHGPTR           CHANGE BUY RECORD                            
         JE    EXXMOD                                                           
         J     XIT_R2R3                                                         
*                                                                               
CLEARB_R BRAS  RE,CLRINS           CLEAR BUY RECORD                             
         B     EXXMOD                                                           
*                                                                               
COMPBC_R BRAS  RE,COM              COMPETITIVE BRAND CHECK                      
         B     EXXMOD                                                           
*                                                                               
FNDINS_R BRAS  RE,FNDINS           FIND INSERTION                               
         B     EXXMOD                                                           
*                                                                               
FSILUP_R BRAS  RE,FSILOOK          FSI LOOK-UP                                  
         B     EXXMOD                                                           
*                                                                               
MINULR_R BRAS  RE,MUP              UPDATE UPLOAD MINIO RECORD                   
         B     EXXMOD                                                           
*                                                                               
RTLOOK_R BRAS  RE,RTSUB            RATE LOOK-UP                                 
         B     EXXMOD                                                           
*                                                                               
UPLELM_R BRAS  RE,UPD              ADD BUY PBU UPLOAD ELEM                      
         B     EXXMOD                                                           
*                                                                               
CHGELM_R BRAS  RE,BYCHGELM         PROCESS BUY CHANGE ELEM                      
         BE    EXXMOD                                                           
         J     XIT_R2R3                                                         
*                                                                               
EDTADC_R L     RE,12(R1)           CHECK POSSIBLE "OPTION" PARAM'S              
         LTR   RE,RE                                                            
         BZ    EDTAC50                                                          
         CLI   6(RE),OPT_JOBQ      CALLED FROM OPTIONS (AD-ID ONLY)?            
         BNE   EDTAC50                                                          
         CLI   0(RE),X'FF'         AD CODE FOR AD-ID ONLY?                      
         BNE   EDTAC50                                                          
         MVC   WORK(7),0(RE)       SET WORK FOR A(EDJ) PROC                     
EDTAC50  GOTOR EDJ                                                              
         B     EXXMOD                                                           
*                                                                               
DELCHK_R BRAS  RE,CKINSDEL         CAN INSERTION BE DELETED?                    
         B     EXXMOD                                                           
*                                                                               
PO#ELM_R BRAS  RE,CKBUYPO#         PURCHASE ORDER # ELEM                        
         B     EXXMOD                                                           
*                                                                               
MRKPOR_R BRAS  RE,MARKPO#R         MARK PURCHASE ORDER RECORD                   
         B     EXXMOD                                                           
*                                                                               
UPDPSV_R BRAS  RE,UPDPSV           UPDATE PASSIVE POINTERS                      
         B     EXXMOD                                                           
*                                                                               
DELINS_R BRAS  RE,DELINS           DELETE INSERTION                             
         BE    EXXMOD                                                           
         J     XIT_R2R3                                                         
*                                                                               
EXCGET_R BRAS  RE,EXCGET           GET FX EXCHANGE RATE/AMOUNT                  
         B     EXXMOD                                                           
*                                                                               
CKIDKC_R BRAS  RE,CKIDKC                                                        
         BE    EXXMOD                                                           
         J     XIT_R2R3                                                         
*                                                                               
TRKCC_R  BRAS  RE,TRKCC            TRACK CHANGES TO CUSTOM COLUMNS              
         BE    EXXMOD                                                           
         J     XIT_R2R3                                                         
*                                                                               
SENDMQ_R BRAS  RE,SENDMQ           SEND MQ MESSAGE TO IDESK                     
         BE    EXXMOD                                                           
         J     XIT_R2R3                                                         
*                                                                               
FIXDAT_R BRAS  RE,FIXDAT           FIX INSERTION DATE IN UPLOAD RECORD          
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHECK    TM    DMCB+8,X'FD'        TEST DATAMGR ERRS                            
         BCR   8,RE                                                             
         DC    H'0'                BLOW UP ON DATAMGR ERRS                      
         EJECT                                                                  
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         JNH   B_FLDS_X                                                         
         J     BUMPFLDS                                                         
B_FLDS_X BR    RE                                                               
*                                                                               
BUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NEXTELX             CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NEXTEL                                                           
         LTR   R5,R5               CC IS NOT EQUAL                              
NEXTELX  BR    R9                                                               
*                                                                               
NXTWFELM SR    R0,R0                                                            
         ICM   R0,3,(LQ_LN-LQ_D)(R3)                                            
         AR    R3,R0                                                            
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
         J     EXIT_VRE                                                         
*                                                                               
PUB_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PUB_DDIR                                                         
*                                                                               
PUB_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PUB_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PUB_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
*                                                                               
         LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE ',           +        
               (RF),APUBIO,(TERMNAL,DMWORK)                                     
*                                                                               
EXIT_VRE LR    RE,R0               EXIT VIA SAVED RE                            
         BR    RE                                                               
*                                                                               
GET_ETXT LR    R0,RE               SAVE RETURN ADDRESS                          
         XC    BUYMSG,BUYMSG                                                    
         MVI   ERRAREA,X'FF'                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ERROR    L     R4,ERRAREA          EXITS FROM PROGRAM                           
         MVI   ERRAREA,X'FF'                                                    
         BRAS  RE,GET_ETXT                                                      
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         ST    R2,ADBERRFD         ADDRESS OF ERROR FLD                         
*                                                                               
X_XIT1   XIT1                                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ROUT_TAB DS   0XL5                                                              
         DC   AL1(RCONABEQ),AL4(RCADEL_R)     RECONSTRUCT ADBUYER ELEM          
         DC   AL1(ADDINSRQ),AL4(ADDINS_R)     ADD INSERTION RECORD              
         DC   AL1(AUTOSCKQ),AL4(AUTOSC_R)     AUTO SCHEDULE CHECKING            
         DC   AL1(AUTOSRKQ),AL4(AUTOSR_R)     AUTO SPACE RESERVATION            
         DC   AL1(BLDBRECQ),AL4(BLDREC_R)     BUILD BUY RECORD                  
         DC   AL1(CHGBRECQ),AL4(CHGREC_R)     CHANGE BUY RECORD                 
         DC   AL1(DELBCHKQ),AL4(DELCHK_R)     DELETE BUY RECORD CHECK           
         DC   AL1(CLEARBRQ),AL4(CLEARB_R)     CLEAR BUY RECORD                  
         DC   AL1(COMPBCKQ),AL4(COMPBC_R)     COMPETITIVE BRAND CHECK           
         DC   AL1(EDTADCDQ),AL4(EDTADC_R)     EDIT AD CODE                      
         DC   AL1(FINDINSQ),AL4(FNDINS_R)     FIND INSERTION                    
         DC   AL1(FSILOOKQ),AL4(FSILUP_R)     FSI LOOK-UP                       
         DC   AL1(MINIOULQ),AL4(MINULR_R)     UPDATE UPLOAD MINIO REC           
         DC   AL1(RTLOOKUQ),AL4(RTLOOK_R)     RATE LOOK-UP                      
         DC   AL1(UPLELEMQ),AL4(UPLELM_R)     ADD BUY PBU UPLOAD ELEM           
         DC   AL1(CHGELEMQ),AL4(CHGELM_R)     PROCESS BUY CHANGE ELEM           
         DC   AL1(BYPO#ELQ),AL4(PO#ELM_R)     PURCHASE ORDER # ELEM             
         DC   AL1(MARKPORQ),AL4(MRKPOR_R)     MARK PURCHASE ORDER REC           
         DC   AL1(UPDPSV_Q),AL4(UPDPSV_R)     UPDATE DATE PASSIVE PTRS          
         DC   AL1(DELINS_Q),AL4(DELINS_R)     DELETE INSERTION                  
         DC   AL1(EXCGET_Q),AL4(EXCGET_R)     GET EXCHANGE RATE                 
         DC   AL1(CKIDKC_Q),AL4(CKIDKC_R)     IDESK CONTROL                     
         DC   AL1(TRKCC_Q),AL4(TRKCC_R)       TRACK CUSTOM COL CHANGES          
         DC   AL1(SENDMQQ),AL4(SENDMQ_R)      SEND MQ MESSAGE TO IDESK          
         DC   AL1(FIXDATQ),AL4(FIXDAT_R)      FIX DATE IN UPLOAD ELEM           
         DC   X'FF'                                                             
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MOVE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    MOVE_X              EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         B     MOVE_X                                                           
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
*                                                                               
MOVE_X   J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTSER   NTR1  BASE=*,LABEL=*      FIND A NEW BUY SERIAL# AND ADD ELEM          
*                                                                               
         MVC   WKSVKEY,KEY         SAVE BUY'S KEY                               
         LA    R9,KEY                                                           
         USING PPSERPPD,R9                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(7),REC          AGY/MED/CLT FROM BUYREC                      
         MVI   KEY+3,X'99'         PASSIVE POINTER CODE                         
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(7),KEYSAVE      MUST FIND                                    
         BE    NXTSER10            NONE FOUND SO ADD FIRST                      
         ZAP   DUB,=P'1'           FIRST TIME                                   
         B     NXTSER12                                                         
*                                                                               
NXTSER10 BRAS  RE,PRT_READ         READ FOR UPDATE, LOCK RECORD                 
*                                                                               
         ZAP   DUB,=P'1000000000'                                               
         SP    DUB,PSERKNUM                                                     
         AP    DUB,=P'1'           DUB NOW HAS NEXT SERIAL NUMBER               
*                                                                               
NXTSER12 ZAP   DOUBLE,=P'1000000000'                                            
         SP    DOUBLE,DUB                                                       
         DROP  R9                                                               
*                                                                               
         LA    R9,WKTEMP1          BUILD AND ADD SERIAL # ELEM                  
         XC    WKTEMP1,WKTEMP1                                                  
         USING PSERELED,R9                                                      
         MVI   PSERELEM,X'99'                                                   
         MVI   PSERLEN,X'09'                                                    
         MVC   PSERNUM,DUB+3                                                    
         DROP  R9                                                               
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'99'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT HAVE ONE YET                      
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'FF'        GET TO END OF RECORD                         
         BRAS  R9,NEXTEL                                                        
         GOTO1 VRECUP,DMCB,(1,REC),WKTEMP1,(R5)                                 
*                                                                               
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         JNE   NXTSERX                                                          
         CLI   BYPROF+13,C'Y'      AUTO-GENERATE PBU UPLOAD ID?                 
         JNE   NXTSERX                                                          
         CLI   MADSW,C'Y'          ADDING BUYS THROUGH PBU?                     
         JE    NXTSERX                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'90'                                                     
         BRAS  R9,NEXTEL           FOUND AUTO-GENERATED UPID ELEM?              
         JE    *+6                                                              
         DC    H'0'                                                             
         USING PIUPEL,R5                                                        
         OC    PIUPUSEQ,PIUPUSEQ   HAVE UPLID?                                  
         JZ    *+6                                                              
         DC    H'0'                                                             
         UNPK  PL16(9),DUB+3(5)                                                 
         OI    PL16+8,X'F0'                                                     
         MVC   PIUPUSEQ,PL16+1     LAST 8 DIGITS OF BUY SERIAL#                 
         DROP  R5                                                               
*                                                                               
NXTSERX  MVC   KEY,WKSVKEY         RESTORE KEY                                  
*                                                                               
         J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* THIS ROUTINE ADDS NEW SERIAL NUMBER'S PASSIVE POINTER                         
* DOUBLE SHOULD CONTAIN 9'S COMPLEMENT OF THE 'REAL' SERIAL NUMBER              
* THIS ROUTINE MUST BE CALLED IMMEDIATELY AFTER ADDING A NEW INSERTION          
* DISK ADDRESS OF THE NEW BUY IS ASSUMED TO STILL RESIDE IN WORK                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTPPS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   S#DUPLSW,C'N'       SET NO DUPLICATE ENCOUNTERED                 
*                                                                               
PUTPP10  MVC   X(32),KEY           SAVE BUY'S KEY                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),REC          AGY/MED/CLT FROM BUYREC                      
         LA    R9,KEY                                                           
         USING PPSERPPD,R9                                                      
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKNUM,DOUBLE+3   9'S COMPLEMENT OF SERIAL NUMBER              
*                                                                               
         MVC   WORK+6(6),COMMAND                                                
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(12),KEYSAVE                                                  
         BNE   PUTPP50                                                          
         MVI   S#DUPLSW,C'Y'       SET DUPLICATE ENCOUNTERED                    
*                                                                               
* WRITE BACK BUYREC WITH NEW SERIAL NUMBER                                      
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'99'        REMOVE BAD SERIAL NUMBER ELEMENT             
         BRAS  R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
*                                                                               
         GOTO1 VRECUP,DMCB,(1,REC),(R5)                                         
*                                                                               
         MVC   KEY,X               RESTORE BUY'S KEY                            
         BRAS  RE,NXTSER           FIND ANOTHER SERIAL NUMBER                   
         MVC   COMMAND(6),WORK+6   RESTORE COMMAND AND TRY AGAIN                
         B     PUTPP10                                                          
*                                                                               
PUTPP50  XC    KEY,KEY                                                          
         MVC   KEY(12),KEYSAVE     RESTORE KEY                                  
         MVC   KEY+27(4),WORK      MOVE DISK ADDRESS                            
         MVC   KEY+25(2),REC+27    MOVE CONTROL BYTES                           
         BRAS  RE,PRT_ADD_                                                      
         BRAS  RE,CHECK                                                         
         MVC   COMMAND(6),WORK+6   RESTORE COMMAND                              
*                                                                               
         CLI   S#DUPLSW,C'Y'       DUPLICATE SERIAL NUMBER?                     
         BNE   PUTPP_X                                                          
         MVC   KEY+27(4),WORK                                                   
         L     R0,AWRKREC          READ INTO WRKREC                             
         ST    R0,AREC                                                          
         BRAS  RE,PRT_GETR                                                      
         MVC   KEY+27(4),WORK      SHOULD STILL HAVE DISK ADDRESS               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         BRAS  RE,PRT_PUTR                                                      
         MVC   COMMAND(6),WORK+6   RESTORE COMMAND                              
*                                                                               
PUTPP_X  J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  R9,RB                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDLINE  NTR1  BASE=*,LABEL=*      SUBROUTINE TO ADD 'NEWREC' TO FILE           
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   ADDL_1                                                           
         TM    SVESPROF+29,X'20'   IDESK ESTIMATE?                              
         JZ    ADDL_1                                                           
         LA    R2,BUYTR1H                                                       
         LA    R3,IDKESERQ                                                      
         J     ERROR                                                            
*                                                                               
ADDL_1   BRAS  RE,EXCGET           CHECK FOR FOREIGN EXCHANGE                   
         JNE   ERROR                                                            
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION UPLOAD (ADBUYER)?            
         JE    X_XIT1              YES, RECORD WILL NOT BE ADDED                
         CLI   LKDRFTSW,C'F'       DRAFT MODE (ADBUYER)?                        
         JE    X_XIT1              YES, RECORD WILL NOT BE ADDED                
*                                                                               
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERR TESTS                   
         XC    KEY,KEY                                                          
         MVC   KEY(25),NEWREC      FIND HIGHEST SUBLINE ASSIGNED                
         OI    DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,CHECK                                                         
         CLC   KEYSAVE(24),KEY                                                  
         BNE   ADDLINE3                                                         
ADDLINE2 TM    KEY+25,X'80'        LINE DELETED?                                
         BZ    ADL2A               NO                                           
*                                                                               
         CLI   KEY+24,0            REUSING BAD LINE #, X'00'?                   
         BE    ADL2A                                                            
*                                                                               
         BRAS  RE,PRT_GETR                                                      
         BRAS  RE,CHECK                                                         
         BRAS  RE,DELCHK                                                        
         BZ    ADL612              YES - LINE CAN BE REUSED                     
*                                                                               
ADL2A    MVC   KEYSAVE,KEY         SAVE LAST KEY                                
         BRAS  RE,PRT_RSEQ                                                      
         BRAS  RE,CHECK                                                         
         CLC   KEYSAVE(24),KEY                                                  
         BE    ADDLINE2                                                         
ADDLINE3 MVC   KEY,KEYSAVE         RESTORE LAST GOOD KEY                        
*                                                                               
         CLI   KEY+24,X'FF'                                                     
         BL    ADL3D               OKAY TO "BUMP"                               
         LHI   R3,619              SUBLINE NUMBER EXCEEDED                      
         BRAS  RE,GET_ETXT         ERROR MSG HANDLING                           
         LA    R2,BUYDT1H          POINT TO INSERTION DATE FIELD                
         MVI   ERRAREA,X'FF'                                                    
         J     EXIT                                                             
*                                                                               
ADL3D    IC    RE,KEY+24                                                        
         LA    RE,1(RE)                                                         
         STC   RE,NEWREC+24        SET SUBLINE NUMBER IN RECORD                 
*                                                                               
ADDLINE4 LA    R0,REC                                                           
         LHI   R1,4001                                                          
         LA    RE,NEWREC                                                        
         LHI   RF,1750                                                          
         MVCL  R0,RE               MOVE NEWREC TO REC                           
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    ADL600                                                           
*                                                                               
         BRAS  RE,PRSMUPEL         SET PRISMA UPLOAD ELEMENT                    
*                                                                               
ADL600   BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERROR                                                            
*                                                                               
         BRAS  RE,NXTSER           FIND AND SAVE NEXT SERIAL NUMBER             
         BRAS  RE,PRT_ADDR                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
         MVC   WORK(4),KEY         SAVE D/A                                     
         MVC   COMMAND,=C'DMADD '                                               
         B     ADDLINE8                                                         
*                                                                               
* ADD NEW BUYREC BUT USE DELETED POINTER                                        
* NOTE THAT ADDREC WILL NOT ADD A PRTDIR RECORD                                 
* BECAUSE OF DUPLICATE KEY.                                                     
*                                                                               
ADL612   MVC   NEWREC(25),KEY      MOVE KEY                                     
*                                                                               
         LA    R0,REC                                                           
         LHI   R1,4001                                                          
         LA    RE,NEWREC                                                        
         LHI   RF,1750                                                          
         MVCL  R0,RE               MOVE NEWREC TO REC                           
*                                                                               
         BRAS  RE,PRSMUPEL         SET PRISMA UPLOAD ELEMENT                    
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERROR                                                            
*                                                                               
         BRAS  RE,NXTSER           FIND AND SAVE NEXT SERIAL NUMBER             
         BRAS  RE,PRT_ADDR                                                      
         NI    DMCB+8,X'DF'        DO NOT TEST DUP (X'20')                      
         BRAS  RE,CHECK                                                         
*                                                                               
         MVC   WORK(4),KEY         SAVE DISK ADDRESS                            
         XC    KEY,KEY             UPDATE DIRECTORY                             
         MVC   KEY(25),REC                                                      
         BRAS  RE,PRT_READ                                                      
         BRAS  RE,CHECK                                                         
         MVC   KEY+25(2),REC+27    MOVE CONTROL BYTES                           
         MVC   KEY+27(4),WORK      MOVE DISK ADDRESS                            
         BRAS  RE,PRT_WRIT                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
ADDLINE8 BRAS  RE,PUTPPS           ADD PASSIVE SERIAL NUMBER POINTER            
*                                                                               
         XC    KEY,KEY             NOW ADD CLT/PUB POINTER                      
         MVC   KEY(25),REC                                                      
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),REC+10     PUB/ZONE/ED                                  
         MVC   KEY+13(3),REC+7     PRD                                          
*                                                                               
         CLI   COMMAND+2,C'A'      WAS LAST ACT ADD                             
         BE    ADDLINE9            YES                                          
         BRAS  RE,PRT_READ         READ OLD POINTER                             
         BRAS  RE,CHECK                                                         
         MVC   COMMAND,=C'DMWRT '                                               
*                                                                               
ADDLINE9 MVC   KEY+27(4),WORK      MOVE DISK ADDRESS                            
         MVC   KEY+25(2),REC+27    MOVE CONTROL BYTES                           
         BRAS  RE,PRT_DDIR                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
         NI    DMINBTS,X'F7'       RESET PASS DELETES                           
*                                                                               
         L     R0,TRADDR           GET TR FIELD ADDRESS                         
         SR    R0,RA               CALCULATE REL DSPLMT                         
         LA    R1,SVINS            FIND AN EMPTY SLOT OR A MATCH                
ADDLN10  OC    0(6,R1),0(R1)                                                    
         BZ    ADDLN12                                                          
         CH    R0,0(R1)                                                         
         BE    ADDLN12                                                          
         LA    R1,6(R1)                                                         
         B     ADDLN10                                                          
ADDLN12  STH   R0,0(R1)                                                         
         MVC   2(4,R1),WORK                                                     
         MVC   INSDA,WORK          PASS RECORD ADDRESS                          
*                                                                               
******   BRAS  RE,UPDPSV           UPDATE DATE PASSIVE POINTERS                 
*                                                                               
ADDLNX   MVI   DMOUTBTS,X'FD'      RESTORE DMOUTBTS                             
         J     X_XIT1                                                           
*                                                                               
DELCHK   LA    R2,REC+33           CK LINE# REUSE FOR SELECTED ELEMS            
*                                                                               
DELCHK2  CLI   0(R2),0                                                          
         BER   RE                                                               
         CLI   0(R2),X'70'         IO ELEM                                      
         BE    DELCHK6                                                          
         CLI   0(R2),PWIOELCQ      EIO ELEM?                                    
         BE    DELCHK6                                                          
         CLI   0(R2),PESRELCQ      ESR ELEM?                                    
         BE    DELCHK6                                                          
         CLI   0(R2),X'25'         PAY                                          
         BE    DELCHK8                                                          
         CLI   0(R2),X'26'         BILL                                         
         BE    DELCHK10                                                         
         CLI   0(R2),X'28'         OPEN BILLING                                 
         BE    DELCHK10                                                         
         CLI   0(R2),PBYMELCQ      BUY MOVE ELEM?                               
         BER   RE                                                               
*                                                                               
DELCHK4  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DELCHK2                                                          
*                                                                               
DELCHK6  LA    R4,2(R2)                                                         
         B     DELCHK11                                                         
DELCHK8  LA    R4,2(R2)                                                         
         B     DELCHK11                                                         
DELCHK10 LA    R4,5(R2)                                                         
         B     DELCHK11                                                         
*                                                                               
DELCHK11 OC    0(3,R4),0(R4)       TEST ACTIVE                                  
         BNZR  RE                  YES - NO PURGE                               
         B     DELCHK4                                                          
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRSMUPEL NTR1  BASE=*,LABEL=*      SET PRISMA UPLOAD ELEMENT                    
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYDKELQ                                                  
         BRAS  R9,NEXTEL                                                        
         JNE   PRSMU20                                                          
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
PRSMU20  XC    WKELEM,WKELEM                                                    
         LA    RE,WKELEM                                                        
         USING PBYDKELM,RE                                                      
         MVI   PBYDKELC,PBYDKELQ   IDESK INSERTION UPLOAD ELEM CODE             
         MVI   PBYDKELL,PBYDKLNQ                                                
         MVC   PBYDKDAT,IDSKDATE                                                
         MVC   PBYDKTIM,IDSKTIME                                                
         OI    PBYDKST1,BYDKADDQ                                                
         TM    PLINKSW1,PRSMORGQ   PRISMA ORIGIN?                               
         JZ    *+12                                                             
         OI    PBYDKST1,BYPMADDQ                                                
         NI    PBYDKST1,X'FF'-BYDKADDQ                                          
         TM    PLINKSW1,RADAORGQ   RADIA ORIGIN?                                
         JZ    *+12                                                             
         OI    PBYDKST3,BYRAADDQ                                                
         NI    PBYDKST1,X'FF'-BYDKADDQ                                          
         TM    PRSMSTA1,BYPRMIVQ   PRISMA INVOICE ENABLED CAMPAIGN?             
         JZ    *+8                                                              
         OI    PBYDKST4,BYPRMIVQ                                                
         GOTO1 VRECUP,DMCB,(1,REC),WKELEM,(R5)                                  
*                                                                               
         J     X_XIT1                                                           
         DROP  RB,RE                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ASR      NTR1  BASE=*,LABEL=*      AUTO SPACE RESERVATION                       
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION (ADBUYER)?                   
         BE    ASRX                                                             
         CLI   LKDRFTSW,C'F'       DRAFT MODE (ADBUYER)?                        
         BE    ASRX                                                             
*                                                                               
         OC    SADVDATA,SADVDATA   NEW ADV SCHEME                               
         BZ    ASR2                                                             
         CLC   SVAOR,AGYALPHA      ONLY DO IF I AM THE AOR                      
         BNE   ASRX                OTHERWISE NO ASR'S                           
*                                                                               
ASR2     CLI   PBDBFD-NEWREC+REC,C'T'                                           
         BE    ASRX                                                             
         TM    PBDSTAT-NEWREC+REC,X'0C'                                         
         BO    ASRX                                                             
*                                                                               
         CLC   BUYPB(2),=C'L='     SEE IF LIST BUYING                           
         BE    ASRL                                                             
         CLC   BUYPB(3),=C'LW='    SEE IF LIST BUYING                           
         BE    ASRL                                                             
*                                                                               
* PICK A CONTRACT COVERING BUY DATE                                             
*                                                                               
* MUST USE DATE IN REC                                                          
* NOT BINSDT - SINCE THAT IS NOT SET FOR DELETES                                
*                                                                               
         GOTO1 VDATCON,DMCB,(3,REC+16),(2,DUB)                                  
         LA    RE,SVCON-8                                                       
         LA    R0,SVCON+L'SVCON-1                                               
         B     ASR4B                                                            
ASR4A    CLC   2(2,RE),DUB         CON END BEFORE BUY                           
         BL    ASR4B                                                            
         CLC   DUB(2),0(RE)        BUY BEFORE CON START                         
         BL    ASR4B                                                            
         B     ASRUPD              CONTRACT FOUND, CK UPDATE IS NEEDED          
ASR4B    LA    RE,8(RE)            NEXT SLOT                                    
         CLI   0(RE),0                                                          
         BNE   ASR4A                                                            
         B     ASRX                NO CONTRACT - DONE                           
*                                                                               
* FOR LIST BUYING MUST FIND APPLICABLE CONTRACT                                 
*                                                                               
ASRL     XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         CLI   SVCLPROF+5,C'2'     SLAVE CLIENT?                                
         BNE   *+10                                                             
         MVC   KEY+4(3),SVCLPROF+6 READ MASTER CLT CONTRACTS                    
         MVC   KEY+7(6),BPUB                                                    
         BRAS  RE,PRT_RDHI                                                      
         B     ASRL2H                                                           
ASRL2    BRAS  RE,PRT_RSEQ                                                      
ASRL2H   CLC   KEYSAVE(13),KEY     TEST SAME A/M/REC/CL/PUB                     
         BNE   ASRL2X                                                           
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BRAS  RE,PRT_GETR                                                      
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
         L     R6,ACONIO                                                        
         USING PCONRECD,R6                                                      
         CLC   PCONEND(3),REC+16   CONTRACT END BEFORE BUY                      
         BL    ASRL2                                                            
         CLC   REC+16(3),PCONSTRT  BUY BEFORE CONTRACT START                    
         BL    ASRL2                                                            
         CLI   PCONPRD,C'A'        SEE IF PRD CONTRACT                          
         BL    ASRUPD5                                                          
         CLC   PCONPRD,BUYPR       PRD CONTRACT - MUST MATCH                    
         BNE   ASRL2                                                            
         B     ASRUPD5             CON FND GO SEE IF I NEED TO UPDATE           
         DROP  R6                                                               
*                                                                               
ASRL2X   DS    0H                                                               
         B     ASRX                LIST BUYING - NO CONTRACT                    
*                                                                               
ASRUPD   XC    KEY,KEY                                                          
         MVC   KEY+27(4),4(RE)                                                  
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BRAS  RE,PRT_GETR                                                      
         LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
*                                                                               
ASRUPD5  L     R5,ACONIO                                                        
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'85'        LOOK FOR ASRELEM                             
         BRAS  R9,NEXTEL                                                        
         BNE   ASRX                NO ASR FOR THIS CONTRACT                     
         CLI   SVCLPROF+5,C'2'     SEE IF DOING SLAVE CLIENT                    
         BNE   ASRUPD10            YES                                          
         USING PASRELMD,R5                                                      
         L     R5,ACONIO           LOOK FOR ELEM FOR THIS CLIENT                
         LA    R5,33(R5)                                                        
ASRUPD6  BRAS  R9,NEXTEL                                                        
         BNE   ASRUPD7                                                          
         CLC   PASRCLT,BUYCL                                                    
         BNE   ASRUPD6                                                          
         B     ASRUPD10            FOUND  - UPDATE                              
*                                                                               
ASRUPD7  XC    WKTEMP1,WKTEMP1     MUST ADD ELEM FOR THIS CLT                   
         MVI   WKTEMP1+0,X'85'                                                  
         MVI   WKTEMP1+1,X'0D'                                                  
         MVC   WKTEMP1+5(3),BTODAY                                              
         MVC   WKTEMP1+8(3),BUYCL                                               
         L     R6,ACONIO                                                        
         GOTO1 VRECUP,DMCB,(1,(R6)),WKTEMP1,(R5)                                
         B     ASRUPD12                                                         
*                                                                               
ASRUPD10 CLC   PASRCDAT,BTODAY                                                  
         BE    ASRX                MATCHES TODAY - DONE                         
         MVC   PASRLDAT,PASRCDAT   SAVE LAST RUN DATE                           
         MVC   PASRCDAT,BTODAY     SET TODAY IN CURRENT                         
*                                                                               
ASRUPD12 MVC   AREC,ACONIO                                                      
         BRAS  RE,PRT_PUTR                                                      
         LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
ASRX     J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDREC   NTR1  BASE=*,LABEL=*      BUILD BARE BONE BUY RECORD IN NEWREC         
*                                                                               
         MVI   GLBVALSW,0          INIT GLOBAL VALIDATION SWITCH                
         XC    WKSVADCD,WKSVADCD                                                
         TM    ABUPLDSW,ABADIDAQ   SET AD-ID ALONE UPLOAD?                      
         BZ    *+10                                                             
         MVC   WKSVADCD,PBDJOB     SAVE IT BEFORE NEWREC INITIALIZATION         
*                                                                               
         LA    R0,NEWREC                                                        
         LHI   R1,1750             EXPANDED LENGTH                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,NEWREC           KEY/CNTL/LEN                                 
         MVC   0(2,R1),AGYALPHA                                                 
         MVC   2(1,R1),BUYMD                                                    
         MVI   3(R1),X'20'                                                      
         MVC   4(3,R1),BUYCL                                                    
         MVC   7(3,R1),BUYPR                                                    
         MVC   10(6,R1),BPUB                                                    
         MVC   19(2,R1),BEST                                                    
         MVC   PBDJOB,WKSVADCD     AREADY SET FOR AD-ID ALONE UPLOAD            
*                                                                               
         MVC   PBDELEM(2),=X'2074'                                              
         MVC   PBDBUYDT,BTODAY                                                  
         MVC   PBDFREQ,BFREQ                                                    
         MVC   PBDLIST,SVLSTID     SET LIST ID IN REC                           
         ZAP   PBDCOS,=P'0'                                                     
         ZAP   PBDPRCOS,=P'0'                                                   
         ZAP   PBDCD,=P'0'                                                      
         ZAP   PBDACP,=P'0'                                                     
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
         MVC   PBDBUYER,BUYNM                                                   
         TM    SVESPROF+29,X'40'   STEWARDSHIP ESTIMATE?                        
         BZ    *+8                                                              
         OI    PBDSTAT2,X'40'      INDICATE STEWARDSHIP INSERTION               
*                                                                               
         CLI   NATION,C'C'         SEE IF CANADIAN                              
         BNE   *+14                                                             
         MVI   PBDCNDA,X'80'       SET CANADIAN INDICATOR IN NEW BUYS           
         MVC   PBDGST,BPUBGST      SET PUB'S GST TAX CODE IN NEW BUYS           
*                                                                               
         CLI   BUYNM,C'*'          BUYER'S INITIALS START WITH *?               
         BNE   *+10                (WHICH MEANS NO ASR)                         
         MVC   PBDBUYER,BUYNM+1    DON'T MOVE *                                 
*                                                                               
         LA    R5,173              INITIAL REC LENGTH 33+116+24                 
         LA    R1,PBDELEM                                                       
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0               POINT TO NEXT ELEM                           
         USING PPAYELEM,R1                                                      
         MVC   PPAYELEM(2),=X'2518'                                             
         CLC   =C'ZZZ',BUYPR                                                    
         BE    BLDR2                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         LA    R5,23(R5)           ADD TO REC LEN                               
         USING PBILELEM,R1                                                      
         MVC   PBILELEM(2),=X'2617'                                             
         MVC   PBPRD,BUYPR                                                      
*                                                                               
BLDR2    DS    0H                                                               
         CLI   SVAGPROF+22,C'N'    NO INSERTION ORDERS                          
         BE    BLDR4                                                            
         IC    R0,1(R1)            IO ELEM                                      
         AR    R1,R0                                                            
         LA    R5,50(R5)           ADD TO REC LEN                               
         MVC   0(2,R1),=X'7032'                                                 
*                                                                               
BLDR4    DS    0H                                                               
         DROP  R1                                                               
*                                                                               
         L     R9,TRADDR           SEE IF BUYING                                
         CLI   8(R9),C'B'                                                       
         BNE   BLDR6                                                            
*                                                                               
         IC    R0,1(R1)            IO ELEM                                      
         AR    R1,R0                                                            
         LA    R5,PPIDLEQ(R5)      ADD TO REC LEN                               
         USING PPIDELD,R1                                                       
         MVI   PPIDELM,PPIDELQ     PID ELEM CODE                                
         MVI   PPIDELL,PPIDLEQ                                                  
         MVC   PPIDADD,SVPID       PID - ADD                                    
         XC    PPIDDEL,PPIDDEL     PID - DELETE                                 
         MVI   PPIDPRG,PPIDPPKQ    ADDED VIA PRINTPAK (DEFAULT)                 
         CLI   DDLINKSW,C' '                                                    
         JNH   *+8                                                              
         MVI   PPIDPRG,PPIDADBQ    ADDED VIA ADBUYER                            
         TM    ABUPLDSW,IDSKUPLQ                                                
         JZ    *+8                                                              
         MVI   PPIDPRG,PPIDIDKQ    ADDED VIA IDESK                              
         TM    PLINKSW1,PRSMORGQ                                                
         JZ    *+8                                                              
         MVI   PPIDPRG,PPIDPRMQ    ADDED VIA PRISMA                             
         TM    PLINKSW1,RADAORGQ                                                
         JZ    *+8                                                              
         MVI   PPIDPRG,PPIDRADQ    ADDED VIA RADIA                              
         TM    ABUPLDSW,ADBYIMPQ                                                
         JZ    *+8                                                              
         MVI   PPIDPRG,PPIDIMPQ    ADDED VIA ADBUYER IMPORTS                    
         CLI   MADSW,C'Y'                                                       
         JNE   *+8                                                              
         MVI   PPIDPRG,PPIDPBUQ    ADDED VIA PBU                                
         LR    RE,RA                                                            
         USING TWAD,RE                                                          
         MVC   PPUSRID,TWAUSRID    USER ID                                      
         DROP  R1,RE                                                            
*                                                                               
BLDR6    DS    0H                                                               
*                                                                               
         CLI   NATION,C'C'         SEE IF CANADIAN                              
         BNE   BLDRX                                                            
*                                                                               
         L     RE,TRADDR           SEE IF BUYING                                
         CLI   8(RE),C'B'                                                       
         BNE   BLDRX                                                            
*                                                                               
* ONLY ADD PSTELEM IF BUYING                                                    
*                                                                               
         OC    BPUBPST,BPUBPST     SEE IF PUB HAS PST                           
         BZ    BLDRX                                                            
         IC    R0,1(R1)            BUMP TO NEXT ELEM SLOT                       
         AR    R1,R0                                                            
         LA    R5,12(R5)           ADD TO REC LEN                               
         MVC   0(2,R1),=X'840C'                                                 
         MVC   2(10,R1),BPUBPST    ADD PST CODE ELEM                            
*                                                                               
BLDRX    DS    0H                                                               
         STH   R5,HALF                                                          
         MVC   NEWREC+25(2),HALF   SET REC LEN                                  
         J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO DELETE PRTDIR ITEM FOR 'REC' AND ADD A                          
* NEW POINTER FOR INSERTION DATE IN NEWREC+16                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHGPTR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CKIDKC           CK FOR IDESK CONTROL                         
         JNE   XIT_R2R3                                                         
*                                                                               
         CLI   LKDRFTSW,C'F'       DRAFT MODE (ADBUYER)?                        
         BNE   *+14                                                             
         MVC   REC+16(3),NEWREC+16 NEED TO RETURN "CHANGED" INS DATE            
         J     SETCCEQ                                                          
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BE    *+16                                                             
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         J     ERR_EXIT                                                         
*                                                                               
* IF INSERTION MONTH IS CHANGED, NEED TO CHECK EIO AND ESR ELEM                 
*                                                                               
         CLC   REC+16+1(1),NEWREC+16+1                                          
         BE    CHGPTR1K                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,PWIOELCQ                                                  
         BRAS  R9,NEXTEL                                                        
         BE    CHGPTR1E                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,PESRELCQ                                                  
         BRAS  R9,NEXTEL                                                        
         BNE   CHGPTR1K                                                         
CHGPTR1E LHI   R3,INSMOCHG                                                      
         J     ERR_EXIT            INSERTION DATE CANNOT BE CHANGED             
*                                                                               
CHGPTR1K MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERR TESTS                   
         XC    KEY,KEY                                                          
*                     NO CHANGE ALLOWED IF NEW DATE HAS 255 INSERTIONS          
         MVC   KEY(25),NEWREC                                                   
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,CHECK                                                         
         LA    R4,1                                                             
*                                                                               
CHGPTR1P DS    0H                                                               
         BRAS  RE,PRT_RSEQ                                                      
         BRAS  RE,CHECK                                                         
         LA    R4,1(R4)                                                         
         CHI   R4,255                                                           
         BNL   CHGPTR1V            CANNOT CHANGE TO THIS DATE                   
         CLC   KEYSAVE(21),KEY     SAME THRU DATE                               
         BNE   CHGPTR1X            NO - CONTINUE                                
         CLI   KEY+25,X'FF'        RE-USEABLE RECORD ?                          
         BE    CHGPTR1X            YES - CONTINUE                               
         B     CHGPTR1P            CHECK NEXT                                   
*                                                                               
CHGPTR1V DS    0H                                                               
         LHI   R3,628              SUBLINE NUMBER EXCEEDED                      
         J     ERR_EXIT                                                         
*                                                                               
CHGPTR1X DS    0H                                                               
         NI    DMINBTS,X'F7'       RESET PASS DELETES                           
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         BRAS  RE,PRT_READ                                                      
         BRAS  RE,CHECK                                                         
         OI    KEY+25,X'FF'        TAG DELETED                                  
         BRAS  RE,PRT_WRIT                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
         MVI   KEY+3,X'21'         DELETE CLT/PUB POINTER TOO                   
         MVC   KEY+7(6),REC+10                                                  
         MVC   KEY+13(3),REC+7     PRD                                          
         BRAS  RE,PRT_READ                                                      
         BRAS  RE,CHECK                                                         
         OI    KEY+25,X'FF'        TAG DELETED                                  
         BRAS  RE,PRT_WRIT                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(21),NEWREC      BUILD KEY WITHOUT SUBLINE                    
         MVC   REC+16(3),NEWREC+16 MOVE NEW INSERTION DATE TO REC               
*                                                                               
* FIND NEW SUBLINE NUMBER                                                       
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,CHECK                                                         
         CLC   KEYSAVE(24),KEY                                                  
         BNE   CHGPTR3                                                          
*                                                                               
CHGPTR2  CLI   KEY+25,X'FF'        RE-USE ONLY FF DELETES                       
         BNE   CHGPTR2A                                                         
         MVC   COMMAND,=C'DMWRT '  USE THIS POINTER                             
         MVC   KEYSAVE,KEY         SAVE KEY USED                                
         B     CHGPTR4                                                          
*                                                                               
CHGPTR2A MVC   KEYSAVE,KEY                                                      
         BRAS  RE,PRT_RSEQ                                                      
         BRAS  RE,CHECK                                                         
         CLC   KEYSAVE(21),KEY                                                  
         BE    CHGPTR2                                                          
*                                                                               
CHGPTR3  MVC   KEY,KEYSAVE         RESTORE LAST GOOD KEY                        
         ZIC   RE,KEY+24           ADD 1 TO SUBLINE                             
         CHI   RE,255                                                           
         BNE   *+6                                                              
         DC    H'0'                WOULD CORRUPT FILE WITH ZERO SUBLINE         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+24                                                        
         MVC   KEYSAVE,KEY         SAVE ADDED KEY                               
         MVC   COMMAND,=C'DMADD '                                               
*                                                                               
CHGPTR4  MVC   KEY+25(2),REC+27    MOVE CONTROL BYTES TO KEY                    
         MVC   KEY+27(4),INSDA     MOVE DISK ADDRESS                            
         MVC   REC+24(1),KEY+24    SET SUBLINE IN REC                           
         BRAS  RE,PRT_DDIR                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
* NOW CREATE CLT/PUB POINTER                                                    
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY ADDED/USED                       
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),REC+10                                                  
         MVC   KEY+13(3),REC+7                                                  
         CLI   COMMAND+2,C'A'      LAST ACTION ADDREC?                          
         BE    CHGPTR6                                                          
         BRAS  RE,PRT_READ                                                      
         BRAS  RE,CHECK                                                         
         MVC   COMMAND,=C'DMWRT '                                               
*                                                                               
CHGPTR6  MVC   KEY+25(2),REC+27    MOVE CONTROL                                 
         MVC   KEY+27(4),INSDA                                                  
         BRAS  RE,PRT_DDIR                                                      
         BRAS  RE,CHECK                                                         
*                                                                               
******   BRAS  RE,UPDPSV           UPDATE DATE PASSIVE POINTERS                 
*                                                                               
CHGPTRX  NI    DMINBTS,X'F7'       RESET PASS DELETES                           
         MVI   DMOUTBTS,X'FD'      RESTORE                                      
         J     SETCCEQ                                                          
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKINSDEL NTR1  BASE=*,LABEL=*      CHECK FOR INSERTION DELETION                 
*                                                                               
         MVI   ERRAREA,0           INIT ERR SWITCH                              
*                                                                               
         TM    REC+(PBUYCNTL-PBUYREC),X'80'                                     
         BZ    *+12                                                             
         LHI   R3,INSDELED         INSERTION ALREADY DELETED                    
         B     CKIDL__1                                                         
*                                                                               
         CLI   BYPROF+7,C'D'       CKING PROFILE                                
         BE    CKIDL_10                                                         
         CLI   BYPROF+7,C'B'                                                    
         BNE   CKIDL_40                                                         
*                                                                               
CKIDL_10 GOTO1 VGETINS,DMCB,REC,PVALUES,REC+7,0,0,0                             
*                                                                               
         CLC   PGROSS(12),GROSS    COMPARE UP TO CASH DISCOUNT                  
         BNE   CKIDL_40                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'        PAY ELEM EXIST?                              
CKIDL_16 BRAS  R9,NEXTEL                                                        
         BNE   CKIDL_40            ALLOW RATE CHG/DEL                           
         OC    2(3,R5),2(R5)                                                    
         BZ    CKIDL_16                                                         
*                                                                               
         LHI   R3,PRFIDLER         INSERTION DELETION NOT ALLOWED               
         B     CKIDL__1                                                         
*                                                                               
CKIDL_40 LA    R5,REC+33                                                        
         MVI   ELCODE,PBNVELQ      INVOICE ELEM PRESENT?                        
         BRAS  R9,NEXTEL                                                        
         BNE   CKIDL_60                                                         
         CLI   DDLINKSW,0          ADBUYER?                                     
         BE    CKIDL_91                                                         
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   CKIDL_91                                                         
         LA    R5,REC+33           CK FOR CLEARANCE STATUS                      
         MVI   ELCODE,X'25'        PAY ELEM                                     
         BRAS  R9,NEXTEL                                                        
         BNE   CKIDL_91                                                         
         OC    2(3,R5),2(R5)       PAID DATE PRESENT?                           
         BZ    *-14                CLEARED - ALLOW DELETION                     
*                                                                               
CKIDL_60 TM    ABUPLDSW,ABMOVINQ   BUY MOVE UPLOAD?                             
         BZ    CKIDL_X                                                          
         CLC   REC+(PBUYKPRD-PBUYKEY)(L'PBUYKPRD),BUYPR                         
         BNE   *+14                                                             
         CLC   REC+(PBUYKEST-PBUYKEY)(L'PBUYKEST),BEST                          
         BE    CKIDL_X                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(PBUYKPUB-PBUYKEY),REC                                        
         MVI   KEY+(PESTKRCD-PESTKEY),X'07'                                     
         MVC   KEY+(PESTKEST-PESTKEY)(2),REC+(PBUYKEST-PBUYKEY)                 
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,PRT_GETR                                                      
         CLI   REC+33,X'07'        ESTIMATE FOUND?                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   REC+33+(PESTSTAT-PESTELEM),C'1'                                  
         BE    CKIDL_93                                                         
         CLI   REC+33+(PESTSTAT-PESTELEM),C'2'                                  
         BE    CKIDL_93                                                         
*                                                                               
         MVC   WKBYTE1,REC+33+(PESTRTYP-PESTELEM)                               
         XC    KEY,KEY                                                          
         MVC   KEY(PBUYKPRD-PBUYKEY),REC                                        
         MVI   KEY+(PESTKRCD-PESTKEY),X'07'                                     
         MVC   KEY+(PESTKPRD-PESTKEY)(3),BUYPR                                  
         MVC   KEY+(PESTKEST-PESTKEY)(2),BEST                                   
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,PRT_GETR                                                      
         CLI   REC+33,X'07'        ESTIMATE FOUND?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WKBYTE1,REC+33+(PESTRTYP-PESTELEM)                               
         BE    CKIDL_X                                                          
         B     CKIDL_92            ESTIMATE RATE TYPE MUST BE SAME              
*                                                                               
CKIDL_91 LHI   R3,DLINVERR         CANNOT DELETE INSERTION W/ INV DATA          
         B     CKIDL__1                                                         
*                                                                               
CKIDL_92 LHI   R3,TFESTRTY         ESTIMATE RATE TYPE MUST BE SAME              
         B     CKIDL__2                                                         
*                                                                               
CKIDL_93 LHI   R3,LOCKERR          ESTIMATE IS LOCKED, NO ADD/CHG               
         B     CKIDL__2                                                         
*                                                                               
CKIDL__1 LA    R2,BUYDT1H          POINT TO INSERTION DATE FIELD                
         B     CKIDL_ER                                                         
*                                                                               
CKIDL__2 LA    R2,BUYESH           POINT TO ESTIMATE FIELD                      
*                                                                               
CKIDL_ER BRAS  RE,GET_ETXT                                                      
         MVI   ERRAREA,X'FF'       ERROR ENCOUNTERED                            
         ST    R2,ADBERRFD         SET ERROR FIELD                              
         J     SETCCNEQ                                                         
*                                                                               
CKIDL_X  J     SETCCEQ                                                          
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKBUYPO# NTR1  BASE=*,LABEL=*      PURCHASE ORDER # ELEM                        
*                                                                               
         LA    R5,NEWREC+33                                                     
         LA    R6,NEWREC                                                        
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         BE    *+12                                                             
         LA    R5,REC+33                                                        
         LA    R6,REC                                                           
         LR    R2,R5               POINT TO FIRST ELEM                          
*                                                                               
         LH    R3,=Y(PO#ULTAB-GENOLD)                                           
         AR    R3,RC                                                            
         OC    0(L'PO#ULTAB*PO#_MAXQ,R3),0(R3)                                  
         BNZ   BYPO#_10                                                         
         TM    CHGIND1,X'08'       INSERTION DATE CHANGE?                       
         BZ    BYPO#_X                                                          
         BRAS  RE,REMPO#EL         REMOVE PO# ELEM                              
         B     BYPO#_X                                                          
*                                                                               
BYPO#_10 BRAS  RE,REMPO#EL         REMOVE PO# ELEM                              
         USING PO#PRDCD,R3                                                      
         LR    R5,R2                                                            
         BRAS  R9,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                PO# ELEM SHOULD BE REMOVED                   
         SR    R2,R2                                                            
BYPO#_32 OC    0(L'PO#ULTAB,R3),0(R3)                                           
         BZ    BYPO#_X                                                          
         CHI   R2,PO#_MAXQ         ALL ENTRIES PROCESSED?                       
         BNL   BYPO#_X                                                          
         TM    PO#STATU,PO#_REMQ   REMOVING INACTIVE PO# IN BUY?                
         BNZ   BYPO#_36                                                         
         XC    WKTEMP2,WKTEMP2                                                  
         LA    RE,WKTEMP2                                                       
         USING PBYPOELM,RE                                                      
         MVI   PBYPOELC,PBYPOELQ                                                
         MVI   PBYPOELL,PBYPOSLQ   LENGTH FOR NON-ZZZ BUY (DEFAULT)             
         TM    PO#STATU,PO#_OVRQ   MANUALLY ENTERED PO# (OVERRIDE)?             
         BZ    *+8                                                              
         OI    PBYPOSTA,BYPOOVRQ   SET PO# OVERRIDE STATUS BIT                  
         OC    PO#PRDCD,PO#PRDCD                                                
         BZ    BYPO#_34                                                         
         MVI   PBYPOELL,PBYPOLLQ   LENGTH FOR ZZZ BUY                           
         OI    PBYPOSTA,BYPOZZZQ   ZZZ BUY - WILL SET PRD CODE                  
         MVC   PBYPOPRD,PO#PRDCD                                                
BYPO#_34 OC    PO#SEQNO,PO#SEQNO                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE PURCHASE ORDER SEQ#                
         MVC   PBYPOSQ#,PO#SEQNO                                                
         DROP  RE,R3                                                            
         GOTO1 VRECUP,DMCB,(1,(R6)),WKTEMP2,(R5)                                
BYPO#_36 LA    R3,L'PO#ULTAB(R3)                                                
         LA    R2,1(R2)                                                         
         B     BYPO#_32            PROCESS NEXT PO# TABLE ENTRY                 
*                                                                               
BYPO#_X  J     X_XIT1                                                           
*                                                                               
REMPO#EL STCM  RE,15,WRKSAVRE                                                   
         MVI   ELCODE,PBYPOELQ                                                  
REMPO_20 LR    R5,R2               POINT TO 1ST ELEM, SEARCH FOR MORE           
         BRAS  R9,NEXTEL                                                        
         JNE   REMPO_X                                                          
         GOTO1 VRECUP,DMCB,(1,(R6)),(R5),0                                      
         OI    CHGIND5,PCHGPO#Q    PURCHASE ORDER # CHANGED                     
         J     REMPO_20                                                         
REMPO_X  ICM   RE,15,WRKSAVRE                                                   
         BR    RE                                                               
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MARKPO#R NTR1  BASE=*,LABEL=*      MARK PURCHASE ORDER RECORD                   
*                                                                               
         BRAS  RE,UPDINV_R         UPDATE INVOICE RECORD                        
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION?                             
         JE    X_XIT1                                                           
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         JE    X_XIT1                                                           
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         BE    *+12                                                             
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         JNE   X_XIT1                                                           
         LH    R3,=Y(PO#ULTAB-GENOLD)                                           
         AR    R3,RC                                                            
         OC    0(L'PO#ULTAB*PO#_MAXQ,R3),0(R3)                                  
         JZ    X_XIT1                                                           
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVAIO,AREC                                                     
*                                                                               
         USING PO#PRDCD,R3                                                      
         SR    R2,R2                                                            
MK_PO20  OC    0(L'PO#ULTAB,R3),0(R3)                                           
         BZ    MK_PO_X                                                          
         CHI   R2,PO#_MAXQ         ALL ENTRIES PROCESSED?                       
         BNL   MK_PO_X                                                          
         TM    PO#STATU,PO#_USEQ   NEED TO MARK PO# USED IN BUY?                
         BNZ   MK_PO40                                                          
MK_PO30  LA    R3,L'PO#ULTAB(R3)                                                
         LA    R2,1(R2)                                                         
         B     MK_PO20             PROCESS NEXT PO# TABLE ENTRY                 
*                                                                               
MK_PO40  XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PPO#KEY,RE                                                       
         MVC   PPO#KAGY,AGYALPHA                                                
         MVC   PPO#KMED,BUYMD                                                   
         MVI   PPO#KRCD,PPO#KIDQ                                                
         MVC   PPO#KCLT,BUYCL                                                   
         CLI   SVCLTPLV,P_POLVCQ   PURCHASE ORDER# AT CLIENT LEVEL?             
         JE    MK_PO44                                                          
         MVC   PPO#KPRD,PO#PRDCD   DEFAULT TO PRD CODE FOR ZZZ BUY              
         CLC   PO#PRDCD,SPACES                                                  
         BH    *+10                                                             
         MVC   PPO#KPRD,BUYPR      SET TO SINGLE PRD CODE (NOT ZZZ)             
         CLI   SVCLTPLV,P_POLVPQ   PURCHASE ORDER# AT PRODUCT LEVEL?            
         JE    MK_PO44                                                          
         MVC   PPO#KEST,BEST                                                    
         DROP  RE                                                               
*                                                                               
MK_PO44  BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(L'PPO#KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR                                                      
*                                                                               
         L     R5,AWKAIO1                                                       
         LA    R5,(PO#FIRST-PPO#REC)(R5)                                        
         USING PO#DELMD,R5                                                      
         MVI   ELCODE,PO#DLIDQ                                                  
         CLI   PO#DELID,PO#DLIDQ   PO# DETAIL ELEM?                             
         BE    *+12                                                             
MK_PO56  BRAS  R9,NEXTEL                                                        
         BNE   MK_PO30                                                          
         CLC   PO#DID,PO#SEQNO     PO# SEQUENCE NUMBER MATCH?                   
         BNE   MK_PO56                                                          
         TM    PO#DACTV,PO#DUSDQ   ALREADY MARKED AS BEING USED IN BUY?         
         BNZ   MK_PO30                                                          
         OI    PO#DACTV,PO#DUSDQ                                                
         DROP  R5                                                               
*                                                                               
         BRAS  RE,PRT_PUTR         UPDATE "USED" STATUS IN PO RECORD            
         B     MK_PO30                                                          
*                                                                               
MK_PO_X  MVC   KEY,WKSVKEY         RESTORE ORIGINAL KEY AND AIO                 
         MVC   AREC,WKSVAIO                                                     
         J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UPDINV_R NTR1  BASE=*,LABEL=*      UPDATE LINKED INVOICE ITEM                   
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT INSERTION?                             
         JE    X_XIT1                                                           
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         JE    X_XIT1                                                           
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         JNE   X_XIT1                                                           
*                                                                               
         TM    CHGIND1,X'08'       INSERTION DATE CHANGE?                       
         JZ    X_XIT1                                                           
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         MVC   WKSVAIO,AREC                                                     
*                                                                               
         USING PBNVELM,R5                                                       
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBNVELQ                                                   
UPDIN25  BRAS  R9,NEXTEL                                                        
         JNE   UPDIN_X                                                          
*                                                                               
         USING PNVKEY,RE                                                        
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         MVC   PNVKAGY,AGYALPHA                                                 
         MVC   PNVKMED,BUYMD                                                    
         MVI   PNVKRCD,PNVKRCDQ                                                 
         MVC   PNVKSER#,PBNVSER#                                                
*                                                                               
         LA    RF,PNVKELMK                                                      
         MVI   PNVKELMK+0000000000000,PNVDKIDQ                                  
         MVC   PNVKELMK+1(L'PBNVDSQN),PBNVDSQN                                  
         MVI   PNVKELMK+01+L'PBNVDSQN,PNVDKDSQ                                  
         DROP  RE                                                               
*                                                                               
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(PNVKELMK-PNVKEY),KEYSAVE                                     
         BNE   UPDIN25                                                          
*                                                                               
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR                                                      
*                                                                               
         L     RE,AWKAIO1                                                       
         LA    RE,(PNVFIRST-PNVREC)(RE)                                         
         USING PNVDTLD,RE                                                       
*                                                                               
UPDIN45  CLI   PNVDKCDE,0          END OF RECORD?                               
         BE    UPDIN25                                                          
         CLI   PNVDKCDE,PNVDKIDQ                                                
         BNE   UPDIN47                                                          
         CLC   PNVDKSQN,PBNVDSQN                                                
         BNE   UPDIN47                                                          
         CLI   PNVDKTYP,PNVDKDSQ                                                
         BNE   UPDIN47                                                          
*                                                                               
         B     UPDIN55                                                          
*                                                                               
UPDIN47  LLC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     UPDIN45                                                          
*                                                                               
UPDIN55  CLC   PNVDBYDT,REC+(PBUYKDAT-PBUYKEY)                                  
         BE    UPDIN25                                                          
         MVC   PNVDBYDT,REC+(PBUYKDAT-PBUYKEY)                                  
*                                                                               
         BRAS  RE,PRT_PUTR         UPDATE INSERTION DATE IN LINKED ITEM         
         B     UPDIN25                                                          
*                                                                               
UPDIN_X  MVC   KEY,WKSVKEY         RESTORE ORIGINAL KEY AND AIO                 
         MVC   AREC,WKSVAIO                                                     
         J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CLEAR A DISPLAYED INSERTION EXCEPT TR/DATE                                    
* AND PURGE FORM SVINS LIST                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRINS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(6,R1),0(R1)                                                    
*                                                                               
         LA    RE,2                                                             
         L     R4,TRADDR                                                        
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         BCT   RE,*-6                                                           
*                                                                               
         L     RE,SVNTRNS                                                       
         LH    RE,SVNTRNS                                                       
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
*                                                                               
CLRINS2  IC    R5,0(R4)                                                         
         AHI   R5,-9                                                            
         TM    1(R4),X'20'                                                      
         BZ    CLRINS3                                                          
         CLC   =C'ALLO',8(R4)                                                   
         BE    CLRINS4                                                          
         CLC   =C'OPTI',8(R4)                                                   
         BE    CLRINS4                                                          
         CLC   =C'-EDT',8(R4)                                                   
         BE    CLRINS4                                                          
         CLC   =C'-LIN',8(R4)                                                   
         BE    CLRINS4                                                          
         CLC   =C'-PRE',8(R4)                                                   
         BE    CLRINS4                                                          
*                                                                               
CLRINS3  DS    0H                                                               
         EX    R5,CLROC                                                         
         BZ    CLRINS4                                                          
         EX    R5,CLRXC                                                         
         FOUT  (R4)                                                             
*                                                                               
CLRINS4  LA    R4,9(R4,R5)                                                      
         BCT   RE,CLRINS2                                                       
         B     CLRINS_X                                                         
*                                                                               
CLROC    OC    8(0,R4),8(R4)                                                    
CLRXC    XC    8(0,R4),8(R4)                                                    
*                                                                               
CLRINS_X J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RTSUB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          NEW BUY?                                     
         BE    RTS_20                                                           
         CLC   PBUYKDAT,PBUYKDAT-NEWREC+REC                                     
         BE    RTS_24                                                           
*                                                                               
RTS_20   OC    PBDMDATE,PBDMDATE   SEE IF I HAVE A DATE                         
         BNZ   RTS_24                                                           
*                                                                               
         GOTO1 =V(PPGETSPC),DMCB,(C'B',NEWREC),VDATAMGR,VADDAY,        +        
               RR=WKRELO03                                                      
*                                                                               
         CLI   DMCB,X'FF'          ANY ERR?                                     
         BE    RTS_24                                                           
         MVC   PBDMDATE,DMCB       NO - SAVE MAT CLOSE DATE                     
*                                                                               
RTS_24   XC    BILLPROF,BILLPROF                                                
         MVC   BILLPROF(3),SVAGPROF+8                                           
         CLI   SVCLPROF+1,C'0'                                                  
         BE    *+10                                                             
         MVC   BILLPROF(3),SVCLPROF+2                                           
         CLI   SVESPROF+1,C'0'                                                  
         BE    *+10                                                             
         MVC   BILLPROF(3),SVESPROF+2                                           
*                                                                               
         L     R4,TRADDR           MUST DO RTLOOK TO GET CD%                    
         CLI   8(R4),C'B'          NEW BUY?                                     
         BE    RTS_62                                                           
*                                                                               
* SAVE LAST RATELOOK INDICATORS AND DATE IN CASE NO RATELOOK TOOK PLACE         
* THOSE FIELDS WILL BE UPDATED IF RATELOOK TOO PLACE                            
*                                                                               
         MVC   PBDRLDT,PBDRLDT-NEWREC+REC                                       
         MVC   WORK(1),PBDRLIND-NEWREC+REC                                      
         NI    WORK,X'FF'-X'08'                                                 
         OC    PBDRLIND(1),WORK    RATELOOK IND - EXCEPT X'08' (FROZEN)         
*                                                                               
         CP    PBDACP,=P'0'        AC OVERRIDDEN?                               
         BNE   *+10                                                             
         MVC   PBDACP,PBDACP-NEWREC+REC                                         
         CP    PBDCD,=P'0'         CD OVERRIDDEN?                               
         BNE   *+10                                                             
         MVC   PBDCD,PBDCD-NEWREC+REC                                           
*                                                                               
         OC    PBDTAX,PBDTAX       TAX OVERRIDDEN?                              
         BNZ   RTS_36                                                           
         CLI   PBDELEM-NEWREC+REC+1,105                                         
         BL    RTS_36                                                           
         MVC   PBDTAX,PBDTAX-NEWREC+REC                                         
         OC    PBDTAX,PBDTAX                                                    
         BNZ   RTS_36                                                           
*                                                                               
* NO TAX ON OLD BUY SO DON'T LOOK ONE UP IF INS WAS BIILED OR PAID              
*                                                                               
         LA    R5,REC+33           CHECK FOR BILLING                            
         MVI   ELCODE,X'26'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_28                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         MVC   PBDTAX,=X'000001'   PREVENTS RE-LOOKUP OF TAX                    
         B     RTS_36              GETS RESET TO ZERO AFTER RATELOOK            
*                                                                               
RTS_28   LA    R5,REC+33           CHECK FOR OPEN BILLING                       
         MVI   ELCODE,X'28'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_30                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         MVC   PBDTAX,=X'000001'   PREVENTS RE-LOOKUP OF TAX                    
         B     RTS_36              GETS RESET TO ZERO AFTER RATELOOK            
*                                                                               
RTS_30   LA    R5,REC+33           CHECK FOR PAYMENTS                           
         MVI   ELCODE,X'25'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_36                                                           
         OC    2(3,R5),2(R5)                                                    
         BZ    *-14                                                             
         MVC   PBDTAX,=X'000001'   PREVENTS RE-LOOKUP OF TAX                    
*                                                                               
* PREVENT RE-LOOKUP OF BILLABLE/PAYABLE DATES FOR BILLED/PAID BUYS              
*                                                                               
RTS_36   OC    PBDBDATE,PBDBDATE                                                
         BNZ   RTS_42                                                           
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'26'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_40                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         MVC   PBDBDATE,PBDBDATE-NEWREC+REC                                     
         B     RTS_42                                                           
*                                                                               
RTS_40   LA    R5,REC+33                                                        
         MVI   ELCODE,X'28'        OPEN BILLING                                 
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_42                                                           
         OC    5(3,R5),5(R5)                                                    
         BZ    *-14                                                             
         MVC   PBDBDATE,PBDBDATE-NEWREC+REC                                     
*                                                                               
RTS_42   OC    PBDPDATE,PBDPDATE                                                
         BNZ   RTS_46                                                           
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'25'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_46                                                           
         OC    2(3,R5),2(R5)                                                    
         BZ    *-14                                                             
         MVC   PBDPDATE,PBDPDATE-NEWREC+REC                                     
*                                                                               
RTS_46   CLC   PBUYKDAT,PBUYKDAT-NEWREC+REC                                     
         BNE   RTS_50                                                           
*                                                                               
         OC    PBDPDATE,PBDPDATE   PAYABLE DATE OVERIDDEN?                      
         BNZ   *+10                                                             
         MVC   PBDPDATE,PBDPDATE-NEWREC+REC MOVE PAYABLE                        
*                                                                               
         OC    PBDBDATE,PBDBDATE   BILLABLE DATE OVERIDDEN?                     
         BNZ   RTS_54                                                           
         MVC   PBDBDATE,PBDBDATE-NEWREC+REC MOVE OLD BILLABLE                   
         B     RTS_54                                                           
*                                                                               
RTS_50   OC    PBDPDATE,PBDPDATE   PAY DATE OVERRIDE OR RESTORE?                
         BZ    RTS_54              NO                                           
         CLI   BILLPROF,C'1'       USED TO CALCULATE BILLABLE?                  
         BNE   RTS_54                                                           
         OC    PBDBDATE,PBDBDATE   BILL DATE OVERRIDE OR RESTORE?               
         BNZ   RTS_54                                                           
*                                                                               
         B     RTS_62              DO RATE LOOK                                 
*                                                                               
RTS_54   L     R2,TRADDR                                                        
         BRAS  RE,BUMPFLD          POINT TO INSERTION DATE                      
         TM    4(R2),X'20'         DATE MODIFIED?                               
         BZ    RTS_62              YES - DO RATELOOK                            
         CP    PBDCOS,=P'0'        COST ENTERED?                                
         BE    RTS_62              NO - LOOK IT UP                              
*                                                                               
         CLI   BUYMD,C'N'          TEST NEWSPAPERS                              
         BNE   RTS_58                                                           
         CLI   PBDCL,0             TEST PREMIUM ENTERED                         
         BE    *+14                NO                                           
         CP    PBDPRCOS,=P'0'      YES - IS COST GIVEN                          
         BE    RTS_62              NO  - LOOK IT UP                             
         CLC   PBDSPACE(2),=C'* '  SPACE BUY?                                   
         BNH   RTS_56                                                           
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    RTS_56                                                           
         CLC   PBDSPACE(2),=C'# '                                               
         BE    RTS_56                                                           
*                                                                               
         CP    PBDUNITS,=P'0'      NEED CLE?                                    
         BNE   RTS_90                                                           
         CP    PBDACP,=P'0'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'1'        PREVENT RE-LOOKUP OF AC                      
         CP    PBDCD,=P'0'                                                      
         BNE   *+10                                                             
         ZAP   PBDCD,=P'1'         PREVENT RE-LOOKUP OF CD                      
         B     RTS_62                                                           
*                                                                               
RTS_56   B     RTS_90                                                           
*                                                                               
RTS_58   CLI   BUYMD,C'O'          OUTDOOR                                      
         BNE   RTS_58M                                                          
         CP    PBDSHOW,=P'0'                                                    
         BE    RTS_58H                                                          
         CP    PBDREG,=P'0'        REG ENTERED?                                 
         BE    RTS_58D             NO - LOOK IT UP                              
         CP    PBDILLUM,=P'0'                                                   
         BNE   RTS_58H                                                          
*                                                                               
RTS_58D  CP    PBDACP,=P'0'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'1'        PREVENT RE-LOOKUP OF AC                      
         CP    PBDCD,=P'0'                                                      
         BNE   *+10                                                             
         ZAP   PBDCD,=P'1'         PREVENT RE-LOOKUP OF CD                      
         B     RTS_62              FOR REG/ILLUM LOOK-UP                        
*                                                                               
RTS_58H  BRAS  RE,BUMPFLD2                                                      
*                                                                               
RTS_58M  BRAS  RE,BUMPFLD2                                                      
         BRAS  RE,BUMPFLD2         POINT TO CLOSE DATE                          
*                                                                               
         TM    4(R2),X'20'         CHANGED?                                     
         BNZ   *+12                NO                                           
         CLI   BILLPROF,C'3'       USED?                                        
         BE    RTS_60                                                           
         CLI   BUYMD,C'O'          OUTDOOR - NO ON-SALE DATE                    
         BE    RTS_90                                                           
         CLI   BUYMD,C'I'          INTERACTIVE - NO ON-SALE DATE                
         BE    RTS_90                                                           
         CLI   BUYMD,C'L'          SOCIAL - NO ON-SALE DATE                     
         BE    RTS_90                                                           
         CLI   BUYMD,C'B'          MOBILE - NO ON-SALE DATE                     
         BE    RTS_90                                                           
         CLI   BUYMD,C'D'          DIGITAL AUDIO - NO ON-SALE DATE              
         BE    RTS_90                                                           
         CLI   BUYMD,C'V'          NVIDEO - NO ON-SALE DATE                     
         BE    RTS_90                                                           
         CLI   BUYMD,C'W'          LVIDEO - NO ON-SALE DATE                     
         BE    RTS_90                                                           
         BRAS  RE,BUMPFLD          POINT TO ON-SALE DATE                        
         TM    4(R2),X'20'         CHANGED?                                     
         BNZ   *+12                NO                                           
         CLI   BILLPROF,C'2'       USED?                                        
         BE    RTS_60              YES                                          
         B     RTS_90                                                           
*                                                                               
RTS_60   CP    PBDACP,=P'0'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'1'        PREVENT RE-LOOKUP OF AC                      
         CP    PBDCD,=P'0'                                                      
         BNE   *+10                SO RATELOOK WON'T LOOK-UP CD                 
         ZAP   PBDCD,=P'1'         FOR BILLABLE DATE LOOK-UPS                   
         B     RTS_62              IT WILL BE RESET TO 0                        
*                                                                               
RTS_62   L     RE,ACONIO                                                        
         XC    0(250,RE),0(RE)     CLEAR CONTRACT AREA                          
         CLC   BUYPB(2),=C'L='     LIST BUY?                                    
         BE    RTS_LSBY            YES - GO READ PUBDIR AND CONTRACTS           
         CLC   BUYPB(3),=C'LW='    WSJ BUY SCREEN?                              
         BE    RTS_LSBY            YES - GO READ PUBDIR AND CONTRACTS           
*                                                                               
* PICK A CONTRACT COVERING BUY DATE                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BINSDT),(2,DUB)                                  
         LA    RE,SVCON-8                                                       
         LA    R0,SVCON+L'SVCON-1                                               
         B     RTS_66                                                           
*                                                                               
RTS_64   CLC   2(2,RE),DUB         CONTRACT END BEFORE BUY?                     
         BL    RTS_66                                                           
         CLC   DUB(2),0(RE)        BUY BEFORE CONTRACT START?                   
         BL    RTS_66                                                           
         B     RTS_70                                                           
RTS_66   LA    RE,8(RE)            NEXT SLOT                                    
         CLI   0(RE),0                                                          
         BNE   RTS_64                                                           
         CLI   SVCLPROF+12,C'N'    CONTRACT NOT REQUIRED?                       
         BE    RTS_80                                                           
         CLI   SVCLPROF+12,C'T'    CONTRACT NOT REQUIRED FOR TEST BUY?          
         BNE   *+12                                                             
         CLI   PBDBFD,C'T'         TEST BUY?                                    
         BE    RTS_80                                                           
*                                                                               
         CLI   BUYMD,C'N'                                                       
         BNE   *+12                                                             
         CLI   BFLAT,C'S'                                                       
         BNE   RTS_80                                                           
         LA    R3,CONERR                                                        
         L     R2,TRADDR                                                        
         J     ERROR                                                            
*                                                                               
RTS_70   XC    KEY,KEY             READ CONTRACT                                
         MVC   KEY+27(4),4(RE)                                                  
         MVC   AREC,ACONIO                                                      
         TM    SVAORC,X'08'        CONTRACT RATE LOOK-UP?                       
         BZ    RTS_72                                                           
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    RTS_72                                                           
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    RTS_72                                                           
         LA    R3,ADVFERR          ADVERTISER FILE NOT ACTIVE ERR MSG           
         LA    R2,BUYMDH                                                        
         J     ERROR                                                            
*                                                                               
RTS_72   BRAS  RE,PRT_GETR                                                      
*                                                                               
         TM    SVAORC,X'08'                                                     
         BZ    RTS_74                                                           
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    RTS_74                                                           
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
RTS_74   LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
*                                                                               
RTS_80   XC    KEY,KEY             GET PUB RECORD                               
         MVC   KEY+27(4),SVPUBDA                                                
         BRAS  RE,PUB_GETR                                                      
*                                                                               
         MVC   TRCODE,=C'RZ'       SET SW FOR AUTO DISPLAY                      
         CLI   SVESPROF+30,C'Y'    FINANCIAL CLIENT?                            
         BNE   RTS_86                                                           
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'        OPEN RATE ELEM                               
         BRAS  R9,NEXTEL                                                        
         BE    RTS_86              FOUND - SO DON'T LOOK IT UP                  
         ZAP   DOUBLE,PBDCOS       SAVE RATE INPUT                              
         ZAP   PBDCOS,=P'0'        FORCE RATE LOOK-UP                           
         ZAP   DUB,PBDUNITS        RATELOOK MIGHT CLOBBER UNITS                 
*                                                                               
         CLI   MADSW,C'Y'          $MAD UPLOAD/UPDATE?                          
         BNE   RTS_82                                                           
         CLC   SAVUHDR+PHDRUTYP-PHDRD(2),=C'E2'                                 
         BNE   RTS_82                                                           
*                                                                               
         L     R5,ATHISTMP                                                      
         LA    R5,2(R5)                                                         
         USING PINSD,R5                                                         
         CLC   PINSTYPE,=C'INS*'                                                
         BE    *+6                                                              
         DC    H'0'                MUST BE AN INSERTION OBJ                     
         CLI   PINSSPRD,C'Y'       SEE IF SPREAD                                
         BE    RTS_82              IF YES DO NOT ATTEMPT SPACE LOOK-UP          
         DROP  R5                                                               
*                                                                               
* PARAMETER LIST IS OKAY, DMCB IS FOLLOWED BY DUB IN PPGENOLD                   
*                                                                               
         GOTO1 =V(RATELOOK),DMCB,(NATION,NEWREC),(ACOVRD,APUBIO),      +        
               ACONIO,(0,BILLPROF),(C'E',VADDAY),VDATCON,VSPCVAL,      +        
               VFRGTAB,RR=WKRELO03                                              
*                                                                               
* MUST FIND OPEN RATE FROM PUB                                                  
*                                                                               
RTS_82   GOTO1 =V(RATELOOK),DMCB,(NATION,NEWREC),(ACOVRD,APUBIO),      +        
               (C'O',ACONIO),(0,BILLPROF),VADDAY,VDATCON,RR=WKRELO03            
*                                                                               
         CLI   0(R1),0             OPEN RATE FOUND IN CONTRACT?                 
         BE    RTS_84              USE IT                                       
*                                                                               
* IF NOT TRY FOR RATE FROM PUB BY CLEARING CONTRACT                             
*                                                                               
         ZAP   PBDUNITS,DUB        RESTORE UNITS BEFORE TRYING AGAIN            
         L     R1,ACONIO                                                        
         MVC   X(1),0(R1)                                                       
         MVI   0(R1),0             FOOL RATELOOK THERE'S NO CONTRACT            
*                                                                               
         GOTO1 =V(RATELOOK),DMCB,(NATION,NEWREC),(ACOVRD,APUBIO),      X        
               ACONIO,(0,BILLPROF),VADDAY,VDATCON,RR=WKRELO03                   
*                                                                               
         L     RE,ACONIO                                                        
         MVC   0(1,RE),X           RESTORE PCONREC                              
*                                                                               
         CLI   0(R1),0                                                          
         BE    RTS_84                                                           
*                                                                               
         L     R2,TRADDR           POSITION CURSOR                              
         SR    R3,R3                                                            
         IC    R3,0(R1)                                                         
         CLI   0(R1),130           SEE IF OPEN RATE NOT FOUND                   
         JNE   ERROR                                                            
         CLI   8(R2),C'C'          SEE IF CHANGE                                
         JNE   ERROR               NO-MUST FIND OPEN RATE                       
         LR    RF,R5               SV NEWREC'S R5 (WHERE TO ADD ELEM)           
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'35'        SEE IF WSJ BUY                               
         BRAS  R9,NEXTEL                                                        
         JE    ERROR               MUST FIND OPEN RATE                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'30'                                                     
         BRAS  R9,NEXTEL           FIND OLD 30 ELEM                             
         JNE   ERROR                                                            
         XC    X(15),X                                                          
         MVC   X(13),0(R5)         SAVE OLD 30 ELEM IN X                        
         LR    R5,RF               RESTORE R5                                   
         B     RTS_84M             ADD OLD ELEM TO NEW REC                      
*                                                                               
RTS_84   XC    X(15),X                                                          
         LA    R3,X                                                             
         USING PORELEMD,R3                                                      
         MVC   0(2,R3),=X'300D'                                                 
         MVC   PORCOSTY,PBDCOSTY                                                
         MVC   PORCOS,PBDCOS                                                    
RTS_84M  ZAP   PBDCOS,DOUBLE       RESTORE RATE/COST                            
*                                                                               
         GOTO1 VRECUP,DMCB,(1,NEWREC),X,(R5)                                    
         B     RTS_88                                                           
         DROP  R3                                                               
*                                                                               
RTS_86   CLI   MADSW,C'Y'          $MAD UPLOAD/UPDATE?                          
         BNE   RTS_88                                                           
         CLC   SAVUHDR+PHDRUTYP-PHDRD(2),=C'E2'                                 
         BNE   RTS_88                                                           
*                                                                               
* PARAMETER LIST IS OKAY, DMCB IS FOLLOWED BY DUB IN PPGENOLD                   
*                                                                               
         GOTO1 =V(RATELOOK),DMCB,(NATION,NEWREC),(ACOVRD,APUBIO),      +        
               ACONIO,(0,BILLPROF),(C'E',VADDAY),VDATCON,VSPCVAL,      +        
               VFRGTAB,RR=WKRELO03                                              
*                                                                               
RTS_88   L     R1,ACONIO                                                        
         ST    R1,DMCB+8                                                        
         CLI   SVESPROF+30,C'Y'    FINANCIAL CLIENT?                            
         BNE   *+8                                                              
         MVI   DMCB+8,C'R'         SET TO REQUIRE CONTRACT RATE                 
         LA    R1,BILLPROF                                                      
         ST    R1,DMCB+12                                                       
         CLI   SVESPROF+28,C'C'    'C' RATE ESTIMATE?                           
         BNE   *+8                                                              
         MVI   DMCB+12,C'C'        SET TO LOOK AT 'C' RATE CONT ELEMS           
*                                                                               
         GOTO1 =V(RATELOOK),DMCB,(NATION,NEWREC),(ACOVRD,APUBIO),      +        
               ,,VADDAY,VDATCON,RR=WKRELO03                                     
*                                                                               
         CLI   0(R1),0                                                          
         BE    RTS_90                                                           
         L     R2,TRADDR           POSITION CURSOR                              
         SR    R3,R3                                                            
         IC    R3,0(R1)                                                         
*                                                                               
         CLI   DDLINKSW,C'F'       DRAFT MODE?                                  
         JE    RTS_X               SUPPRESS ERROR FOR DRAFT UPDATES             
*                                                                               
         J     ERROR                                                            
*                                                                               
RTS_90   BRAS  RE,WKDAYADJ         ADJUST CLOSE & ON-SALE DATES                 
*                                                                               
         CP    PBDCOS,=P'1'        FREE RATE?                                   
         BNE   *+10                                                             
         ZAP   PBDCOS,=P'0'        RESET TO 0                                   
*                                                                               
* ATTEMPT TO PREVENT DUMPS IN GETINS, CHECK LARGE UNIT RATES HERE               
*                                                                               
         CLI   PBUYKMED,C'N'       NEWSPAPERS?                                  
         BNE   RTS_92D                                                          
         CLI   PBDCOSTY,C'U'       TEST UNIT RATE                               
         BNE   RTS_92D                                                          
         CP    PBDCOS,=P'300000000'                                             
         BH    RTS_92                                                           
*                                                                               
         ZAP   WORK(16),PBDUNITS                                                
         MP    WORK(16),PBDCOS     PBDCOS HAS 5 DECIMALS                        
         DP    WORK(16),=P'100'                                                 
         ZAP   WORK(16),WORK(14)                                                
         DP    WORK(16),=P'10'                                                  
         CP    WORK(14),=P'-2100000000'                                         
         BL    RTS_92                                                           
*                                                                               
         CP    WORK(14),=P'2100000000'                                          
         BL    RTS_92D                                                          
*                                                                               
RTS_92   L     R2,TRADDR           SET R2 TO TRANSACTION ADDRESS                
         LHI   RF,4                                                             
         BRAS  RE,BUMPFLDS         CURSOR TO RATE FIELD                         
         LA    R3,INVRTERR         INVALID RATE                                 
         J     ERROR                                                            
*                                                                               
RTS_92D  CP    PBDPRCOS,=P'1'      FREE PREMIUM?                                
         BNE   *+10                                                             
         ZAP   PBDPRCOS,=P'0'      RESET TO ZERO                                
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          NEW BUY?                                     
         BNE   RTS_92G                                                          
         CP    PBDCD,=P'1'         CD OVERRIDE OF 0.0?                          
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'         RESET TO ZERO                                
         CP    PBDACP,=P'1'        AC LOOK-UP OF ZERO?                          
         BNE   *+10                                                             
         ZAP   PBDACP,=P'0'        SET TO ZERO                                  
         CLC   PBDTAX,=X'000001'   TAX LOOK-UP OF ZERO?                         
         BNE   *+10                                                             
         XC    PBDTAX,PBDTAX       RESET TO ZERO                                
         CP    PBDUNITS,=P'-1'                                                  
         BNE   *+16                                                             
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
*                                                                               
RTS_92G  CLI   BUYMD,C'O'          CHECK OUTDOOR                                
         BNE   RTS_92K                                                          
         CP    PBDREG,=P'99999'                                                 
         BNE   *+10                                                             
         ZAP   PBDREG,=P'0'        RESET TO 0                                   
         CP    PBDILLUM,=P'99999'                                               
         BNE   RTS_92K                                                          
         ZAP   PBDILLUM,=P'0'      RESET TO 0                                   
*                                                                               
RTS_92K  CLC   AGYALPHA,=C'JW'                                                  
         BNE   RTS_92P                                                          
         CLC   PBDSPACE(2),=C'$$'                                               
         BNE   RTS_92P                                                          
         B     RTS_94              SKIP BILL/PAY DATE RANGE CHECKS              
*                                                                               
RTS_92P  L     R2,TRADDR                                                        
         LHI   R3,BDERR                                                         
         LA    R4,=AL2(6,3)                                                     
         LA    R5,PBDBDATE                                                      
         BRAS  RE,RTS_TSTD         TEST BILLABLE DATE                           
*                                                                               
         LHI   R3,PDERR                                                         
         LA    R4,=AL2(5,5)                                                     
         LA    R5,PBDPDATE                                                      
         BRAS  RE,RTS_TSTD         TEST PAYABLE DATE                            
*                                                                               
RTS_94   LA    R5,NEWREC+33                                                     
         MVI   ELCODE,PORELMEQ     FIND COS2 $ RATE ELEM                        
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_94F                                                          
         USING PORELEM,R5                                                       
         TM    PORCOSS1,PORCOS$Q   NON-FINANCIAL COS2 $?                        
         JZ    RTS_94F                                                          
         CLI   PORC$TYP,PORC$NEQ   ENTERED AS NET?                              
         JNE   RTS_94F                                                          
*                                                                               
         CP    PBDACP,=P'0'        HAVE AGENCY COMMISSION?                      
         JE    RTS_94F                                                          
         CP    PBDACP,=P'1'        AGENCY COMMISSION LOOK-UP?                   
         JE    RTS_94F                                                          
         CLI   SVTRCODE,C'C'       BUY CHANGE TRANSACTION?                      
         JE    RTS_94F             GROSS IS ALREADY CALCULATED                  
*                                                                               
         ZAP   DUB,PORCOS                                                       
         BRAS  RE,CALC_GRS         CALCULATE GROSS AND RETURN IN DUB            
         BNE   *+10                                                             
         ZAP   PORCOS,DUB                                                       
         DROP  R5                                                               
*                                                                               
RTS_94F  CLI   PBDCTYP,C'N'        NET INPUT?                                   
         BE    *+12                                                             
         CLI   PBDCOSIN,C'R'       ROADSIDE?                                    
         BNE   RTS_94G                                                          
         CP    PBDACP,=P'0'        HAVE AGENCY COMMISSION?                      
         BE    RTS_94G                                                          
         CP    PBDACP,=P'1'        AGENCY COMMISSION LOOK-UP?                   
         BE    RTS_94G                                                          
*                                                                               
         ZAP   DUB,PBDCOS                                                       
         BRAS  RE,CALC_GRS         CALCULATE GROSS AND RETURN IN DUB            
         BNE   *+10                                                             
         ZAP   PBDCOS,DUB                                                       
*                                                                               
         CP    PBDCOS,=P'0'        FREE INSERTION?                              
         BE    RTS_94G             AMOUNT ALREADY IN GROSS                      
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,BYPCIDQ      FIND PLANNED COST ELEM                       
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_94G                                                          
         USING BYPCELD,R5                                                       
         CLI   BYPCIND,C'X'        SPECIAL ELEM DELETION CODE?                  
         BE    RTS_94G                                                          
         ZAP   DUB,BYPCCST                                                      
         BRAS  RE,CALC_GRS         CALCULATE GROSS AND RETURN IN DUB            
         BNE   *+10                                                             
         ZAP   BYPCCST,DUB                                                      
         DROP  R5                                                               
*                                                                               
RTS_94G  CLI   PBDPCTYP,C'N'       NET PREMIUM CHARGE INPUT?                    
         BNE   RTS_94M                                                          
*                                                                               
         CP    PBDACP,=P'0'                                                     
         BE    RTS_94M                                                          
         CP    PBDACP,=P'1'                                                     
         BE    RTS_94M                                                          
*                                                                               
         ZAP   DUB,PBDPRCOS        GROSS UP COST, UNLESS THERE' NO AC           
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         BNP   RTS_94M                                                          
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDPRCOS,DUB                                                     
*                                                                               
RTS_94M  CLI   SVESPROF+30,C'Y'    FINANCIAL CLIENT?                            
         BNE   RTS_X                                                            
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'30'        FIND OPEN RATE ELEM                          
         BRAS  R9,NEXTEL                                                        
         BNE   RTS_X                                                            
*                                                                               
         USING PORELEMD,R5                                                      
         CLI   PORCOSTY,C'C'       OPEN = CONTRACT?                             
         BNE   RTS_X                                                            
         MVC   PORCOS,PBDCOS       SET OPEN TO CONTRACT                         
         MVC   PORCOSTY,PBDCOSTY                                                
         DROP  R5                                                               
*                                                                               
RTS_X    J     X_XIT1                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RTS_LSBY DS    0H                  FIND APPLICABLE CONTRACT - LIST BUY          
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA     R_LSBY10                                     
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         CLI   SVCLPROF+5,C'2'     SUB CLIENT?                                  
         BNE   *+10                                                             
         MVC   KEY+4(3),SVCLPROF+6 READ MASTER CLT CONTRACTS                    
         MVC   KEY+7(6),BPUB                                                    
*                                                                               
         TM    SVAORC,X'08'        ADV CONTRACT LOOK-UP?                        
         BZ    R_LSBY10                                                         
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    R_LSBY10                                                         
*                                                                               
         TM    SVAORC,X'01'        PUB LINK?                                    
         BZ    *+10                                                             
         MVC   KEY+7(6),SVADVPUB                                                
         MVC   KEY(2),SVAOR        AOR                                          
         MVC   KEY+4(3),SVADV      ADV                                          
*                                                                               
         L     RF,ACOMFACS         MUST SWITCH TO AOR                           
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    R_LSBY10                                                         
         LA    R3,ADVFERR          AOR NOT ACTIVE                               
         J     ERROR                                                            
*                                                                               
R_LSBY10 BRAS  RE,PRT_RDHI                                                      
         B     R_LSBY26                                                         
R_LSBY22 BRAS  RE,PRT_RSEQ                                                      
*                                                                               
R_LSBY26 CLC   KEYSAVE(13),KEY     SAME A/M/REC/CL/PUB?                         
         BNE   R_LSBY30                                                         
         MVC   AREC,ACONIO                                                      
         BRAS  RE,PRT_GETR                                                      
         LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
         L     R6,ACONIO                                                        
         USING PCONRECD,R6                                                      
         CLC   PCONEND(3),BINSDT   CONTRACT END BEFORE BUY?                     
         BL    R_LSBY22                                                         
         CLC   BINSDT,PCONSTRT     BUY BEFORE CONTRACT START?                   
         BL    R_LSBY22                                                         
         CLI   PCONPRD,C'A'        PRODUCT CONTRACT?                            
         BL    R_LSBY40            ALL PRODUCTS                                 
         CLC   PCONPRD,BUYPR       MATCH BUY PRODUCT?                           
         BNE   R_LSBY22                                                         
         B     R_LSBY40                                                         
         DROP  R6                                                               
*                                                                               
R_LSBY30 L     R6,ACONIO                                                        
         XC    0(256,R6),0(R6)     CLEAR CON                                    
*                                                                               
R_LSBY40 TM    SVAORC,X'08'        AOR CONTRACT LOOK-UP                         
         BZ    R_LSBY44                                                         
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    R_LSBY44                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                TROUBLE SWITCHING BACK                       
*                                                                               
R_LSBY44 XC    KEY,KEY                                                          
         MVC   KEY(1),BUYMD                                                     
         MVC   KEY+1(6),BPUB                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BRAS  RE,PUB_RDHI                                                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    R_LSBY60                                                         
         CLI   SVAGPROF+16,C'0'    TEST DEFAULT TO SRDS                         
         BE    R_LSBY50            NO                                           
         MVC   KEYSAVE+7(2),=C'ZZ' TEST FOUND DEFAULT                           
         CLC   KEYSAVE(25),KEY                                                  
         BE    R_LSBY60                                                         
*                                                                               
R_LSBY50 MVC   KEY,KEYSAVE         READ DEFAULT                                 
         BRAS  RE,PUB_READ                                                      
R_LSBY60 MVC   SVPUBDA,KEY+27      SAVE DISK ADDRESS                            
         BRAS  RE,PUB_GETR                                                      
         L     R5,APUBIO                                                        
         USING PUBREC,R5                                                        
         OC    PUBKILL,PUBKILL     TEST VS KILL DATE                            
         BZ    R_LSBY64                                                         
         CLC   BINSDT,PUBKILL                                                   
         BL    R_LSBY64                                                         
         LA    R3,KDATERR          KILL DATE ERR MSG                            
         L     R2,TRADDR                                                        
         J     ERROR                                                            
*                                                                               
R_LSBY64 CLI   NATION,C'C'         CANADIAN?                                    
         BNE   R_LSBY68                                                         
         L     R6,TRADDR                                                        
         CLI   8(R6),C'B'          BUYING?                                      
         BNE   R_LSBY68                                                         
         MVI   PBDGST,C'S'         DEFAULT TO STANDARD                          
         CLI   PUBGST,0                                                         
         BE    R_LSBY68                                                         
         MVC   PBDGST,PUBGST       OTHERWISE SET FROM PUB                       
*                                                                               
R_LSBY68 L     R6,ACONIO           TEST HAVE CONTRACT                           
         CLI   0(R6),0                                                          
         BNE   R_LSBY80            YES - OK                                     
*                                                                               
         CLI   SVCLPROF+12,C'N'    CONTRACT REQUIRED?                           
         BE    R_LSBY80            NO                                           
         CLI   SVCLPROF+12,C'T'    NOT REQUIRED FOR TEST BUYS?                  
         BNE   *+12                                                             
         CLI   PBDBFD,C'T'         TEST BUY?                                    
         BE    R_LSBY80            YES - CONTRACT NOT REQUIRED                  
*                                                                               
         CLI   BUYMD,C'N'                                                       
         BNE   R_LSBY78            ERR FOR NON-NEWS                             
         LA    R2,PUBREC+33        FOR NEWS CHECK FLAT OR SLIDING SCALE         
*                                                                               
R_LSBY74 CLI   0(R2),X'20'                                                      
         BE    R_LSBY76                                                         
         CLI   0(R2),0                                                          
         BE    R_LSBY80                                                         
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     R_LSBY74                                                         
*                                                                               
         USING PUBGENEL,R2                                                      
R_LSBY76 CLI   PUBFLAT,C'S'        SLIDING SCALE                                
         BNE   R_LSBY80            NO - OK                                      
*                                                                               
R_LSBY78 LA    R3,CONERR                                                        
         L     R2,TRADDR                                                        
         J     ERROR                                                            
         DROP  R5,R2                                                            
*                                                                               
R_LSBY80 B     RTS_80                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RTS_TSTD DS    0H                  TEST DATE                                    
         OC    0(3,R5),0(R5)                                                    
         BZR   RE                                                               
         SR    RF,RF                                                            
         MVC   WORK(2),PBUYKDAT                                                 
         IC    RF,WORK+1                                                        
         SH    RF,0(R4)                                                         
         STC   RF,WORK+1                                                        
         BP    RTS_T20                                                          
         AHI   RF,12                                                            
         STC   RF,WORK+1                                                        
         IC    RF,WORK                                                          
         BCTR  RF,R0                                                            
         STC   RF,WORK                                                          
*                                                                               
RTS_T20  CLC   WORK(2),0(R5)                                                    
         JH    ERROR               DATE BELOW LIMIT                             
*                                                                               
         MVC   WORK(2),PBUYKDAT                                                 
         IC    RF,WORK+1                                                        
         AH    RF,2(R4)                                                         
         STC   RF,WORK+1                                                        
         CLI   WORK+1,12                                                        
         BNH   RTS_T50                                                          
         AHI   RF,-12                                                           
         STC   RF,WORK+1                                                        
         IC    RF,WORK                                                          
         LA    RF,1(RF)                                                         
         STC   RF,WORK                                                          
*                                                                               
RTS_T50  CLC   WORK(2),0(R5)                                                    
         JL    ERROR               DATE ABOVE LIMIT                             
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CALC_GRS LR    R3,RE                                                            
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,PBDACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         JNP   CALCGNEQ                                                         
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         JNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         CVD   R1,DUB              RETURN GROSS                                 
CALCGEQ  SR    RE,RE                                                            
         CR    RE,RE               SET CC EQUAL                                 
         J     CALCG_X                                                          
CALCGNEQ LA    RE,1                                                             
         LTR   RE,RE               SET CC NOT EQUAL                             
CALCG_X  LR    RE,R3                                                            
         BR    RE                                                               
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNDINS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,NODSPERR         MUST RECALL BEFORE CHANGE                    
         L     R4,TRADDR                                                        
         SR    R4,RA                                                            
         LA    R1,SVINS                                                         
         LA    R0,12                                                            
*                                                                               
F_INS20  CH    R4,0(R1)                                                         
         BE    F_INS30                                                          
         LA    R1,6(R1)                                                         
         BCT   R0,F_INS20                                                       
         L     R2,TRADDR                                                        
         J     ERROR                                                            
*                                                                               
F_INS30  MVC   INSDA,2(R1)                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),INSDA                                                  
         ST    R1,INSADR                                                        
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,PRT_GETR                                                      
*                                                                               
         CLI   REC+33+1,X'69'                                                   
         BNL   *+6                                                              
         DC    H'0'                VERY OLD BUY                                 
         NI    DMINBTS,X'F7'       RESET PASS DELETES                           
*                                                                               
*        SAVE BILLABLE AND PAYABLE DATES BECAUSE THEY                           
*        GET OVERIDDEN BY NEW DATES DURING CHANGES                              
*                                                                               
         MVC   SVMDATE,PBDMDATE-PBUYREC+REC SAVE MATERIALS CLOSE DATE           
         MVC   SVCDATE,PBDCDATE-PBUYREC+REC SAVE SPACE     CLOSE DATE           
         MVC   SVPDATE,PBDPDATE-PBUYREC+REC SAVE PAYABLE DATE                   
         MVC   SVBDATE,PBDBDATE-PBUYREC+REC SAVE BLBABLE DATE                   
*                                                                               
* CAN'T CHANGE OR DELETE A BUY COVERED BY A LOCKED CONTRACT                     
* WON'T FIND CONTRACT SINCE DATE WAS OR'D WITH X'F0'                            
*                                                                               
F_INS40  CLI   SVCLPROF+12,C'N'    CONTRACT IS REQUIRED?                        
         BE    F_INS80                                                          
         CLI   SVCLPROF+12,C'T'    CONTRACT NOT REQUIRED FOR TEST BUYS?         
         BNE   F_INS60                                                          
         CLI   PBDBFD-PBUYREC+REC,C'T'                                          
         BE    F_INS80                                                          
*                                                                               
F_INS60  GOTO1 VDATCON,DMCB,(3,REC+16),(2,DUB)                                  
         LA    RE,SVCON-8                                                       
         LA    R0,SVCON+L'SVCON-1                                               
         B     F_INS70                                                          
*                                                                               
F_INS64  CLC   2(2,RE),DUB                                                      
         BL    F_INS70                                                          
         CLC   DUB(2),0(RE)                                                     
         BL    F_INS70                                                          
         B     F_INS80                                                          
*                                                                               
F_INS70  LA    RE,8(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   F_INS64                                                          
         CLI   BUYMD,C'N'                                                       
         BNE   *+12                                                             
         CLI   BFLAT,C'S'                                                       
         BNE   F_INS80                                                          
         LA    R3,CONERR           CONTRACT REQUIRED                            
         L     R2,TRADDR           WON'T FIND IF LOCKED                         
         J     ERROR                                                            
*                                                                               
F_INS80  TM    REC+27,X'80'                                                     
         BZ    F_INS_X                                                          
*                                                                               
         L     R4,TRADDR                                                        
         CLI   8(R4),C'D'          TRYING TO DELETE DELETED BUY?                
         BE    F_INS_E5                                                         
*                                                                               
         CLC   BUYPB(2),=C'L='     NEVER FOR LIST BUYING                        
         BE    F_INS_E3                                                         
         CLC   BUYPB(3),=C'LW='    NEVER FOR WSJ LIST BUYING                    
         BE    F_INS_E3                                                         
         CLI   BYPROF+5,C'Y'       CHANGE TO DELETED BUY ALLOWED?               
         BE    F_INS_X             CHANGES LIMITED TO COMMENTS                  
         B     F_INS_E3                                                         
*                                                                               
F_INS_X  J     X_XIT1                                                           
*                                                                               
F_INS_E3 LA    R3,CHGDLERR                                                      
         J     ERROR                                                            
*                                                                               
F_INS_E5 LA    R3,TRERR            INVALID TRANSACTION CODE                     
         J     ERROR                                                            
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                     *         
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
         XC    WORK,WORK                                                        
L        USING LKKEYD,WORK                                                      
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
*                                                                               
TSTLK2   GOTO1 (RF),DMCB,('LKTESTQ',WORK),ACOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         BE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK2                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   TSTLK4                                                           
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK3   GOTO1 (RF),DMCB,('LKTESTQ',WORK),ACOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         BE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK3                                                           
*                                                                               
TSTLK4   XC    WORK,WORK                                                        
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,BUYMD                                                  
         MVC   L.LOCKCLT,BUYCL                                                  
         MVC   L.LOCKPUB,REC+10    PACKED BASE PUB NUMBER                       
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
TSTLK5   GOTO1 (RF),DMCB,('LKTESTQ',WORK),ACOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         BE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK5                                                           
*                                                                               
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT?                                  
         BNE   SETCCEQ                                                          
         MVC   L.LOCKCLT,SVCLPROF+6                                             
TSTLK6   GOTO1 (RF),DMCB,('LKTESTQ',WORK),ACOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         BE    SETCCNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLK6                                                           
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
         J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB,L                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*      FOR PBDCDATE, & PBDMDATE CHANGE NON-WORK DAY                   *         
*       DATES TO FIRST PRIOR WORK DAY FOR NEW BUYS                    *         
*       IF PROFILE SO INDICATES                                       *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WKDAYADJ NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,TRADDR                                                        
         CLI   8(R4),C'B'          NEW BUY ?                                    
         BNE   WKDAYX              NO                                           
*                                                                               
         CLI   BYPROF+10,C'Y'      "PRIOR" DATE IF DATE NOT WORKDAY ?           
         BNE   WKDAYX              NO                                           
*                                                                               
* ADJUST DATES TO PRIOR WORKDAY IF NECESSARY                                    
*                                                                               
         OC    PBDCDATE,PBDCDATE   CLOSE DATE IN NEWREC ?                       
         BZ    WKDAYM              NO                                           
         GOTOR VPPWKDAY,DMCB,(NATION,PBDCDATE),PBDCDATE,ACOMFACS                
WKDAYM   DS    0H                                                               
         OC    PBDMDATE,PBDMDATE   MAT'L CLOSE DATE IN NEWREC ?                 
         BZ    WKDAYS              NO                                           
         GOTOR VPPWKDAY,DMCB,(NATION,PBDMDATE),PBDMDATE,ACOMFACS                
WKDAYS   DS    0H                                                               
*NOP*    OC    PBDSDATE,PBDSDATE   ON SALE DATE IN NEWREC ?                     
*NOP*    BZ    WKDAYX              NO                                           
*NOP*    GOTOR VPPWKDAY,DMCB,(NATION,PBDSDATE),PBDSDATE,ACOMFACS                
*                                                                               
WKDAYX   J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* THIS ROUTINE ADDS UPLOAD ELEMENT TO PBUYREC FOR NEW/CHGED BUYS                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UPD      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    X(PIUPELXQ),X       ELEMENT WITH EXTENDED UPID                   
         LA    R2,X                                                             
         USING PIUPELD,R2                                                       
         MVI   PIUPEL90,PIUPELCQ                                                
         MVI   PIUPELLN,PIUPELXQ                                                
         MVC   PIUPDAT,BTODAY                                                   
         MVI   PIUPUSRC,X'02'                                                   
*                                                                               
         MVI   WKBYTE2,0           INIT AUTO-GENERATE UPLOAD ID FLAG            
         CLI   MADSW,C'Y'          SEE IF FROM $MAD UPLOAD                      
         BE    UPMAD                                                            
         CLI   UPLSW,C'Y'          SEE IF UPLOADING USING 'UPID='               
         BE    UPEL0                                                            
*                                                                               
         CLI   SVTRCODE,C'B'       NEW INSERTION?                               
         JNE   UPEL_X                                                           
         CLI   BYPROF+13,C'Y'      AUTO-GENERATE PBU UPLOAD ID?                 
         JNE   UPEL_X                                                           
         MVI   WKBYTE2,C'Y'        AUTO-GENERATE UPLOAD ID                      
         J     UPEL5                                                            
*                                                                               
* FIRST BE SURE ESTIMATE HAS BEEN UPLOADED                                      
*                                                                               
UPEL0    XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'90'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         MVC   KEY+10(2),BEST                                                   
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(12),KEYSAVE     CHK THROUGH EST                              
         BNE   UPEL54              NO MINIO RECS FOR EST                        
*                                                                               
* DUMMY UP UPLOAD ELEM                                                          
*                                                                               
UPEL5    GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(0,PIUPUDAT+2)                         
         MVC   PIUPUDAT(2),=C'19'                                               
         CLC   PIUPUDAT+2(2),=C'70'                                             
         BH    *+10                                                             
         MVC   PIUPUDAT(2),=C'20'                                               
*                                                                               
         MVC   PIUPUAD,PBDJOB                                                   
         CLI   BUYMD,C'N'          SEE IF NEWSPAPERS                            
         BE    UPNEW                                                            
         CLI   BUYMD,C'O'          SEE IF OUTDOOR                               
         BE    UPOUT                                                            
*                                  MUST BE MAGS/TRADE/SUPPLEMENTS               
         MVC   PIUPUSP,PBDSPACE                                                 
         B     UPEL30                                                           
*                                                                               
UPNEW    MVC   PIUPUSP(8),PBDSPACE                                              
         B     UPEL30                                                           
*                                                                               
UPOUT    CLI   PBDSPACE,X'FF'                                                   
         BNE   UPEL30                                                           
         ZAP   DUB,PBDSHOW                                                      
         CVB   R0,DUB                                                           
         CVD   R0,DUB                                                           
         UNPK  PIUPUSH,DUB+6(2)                                                 
         OI    PIUPUSH+2,X'F0'                                                  
         ZAP   DUB,PBDREG                                                       
         CVB   R0,DUB                                                           
         CVD   R0,DUB                                                           
         UNPK  PIUPURG,DUB+6(2)                                                 
         OI    PIUPURG+2,X'F0'                                                  
         ZAP   DUB,PBDILLUM                                                     
         CVB   R0,DUB                                                           
         CVD   R0,DUB                                                           
         UNPK  PIUPUIL,DUB+6(2)                                                 
         OI    PIUPUIL+2,X'F0'                                                  
*                                                                               
UPEL30   CLI   WKBYTE2,C'Y'        AUTO-GENERATE UPLOAD ID?                     
         JNE   UPEL36                                                           
         XC    PIUPUSEQ,PIUPUSEQ   IT'LL BE SET WHEN SERIAL# ASSIGNED           
         XC    PIUPUQXT,PIUPUQXT                                                
         J     UPEL50                                                           
*                                                                               
UPEL36   MVC   PIUPUSEQ,UPID                                                    
         OC    PIUPUSEQ,=CL8' '                                                 
         B     UPEL50                                                           
*                                                                               
UPMAD    DS    0H                                                               
         OC    INSDA,INSDA         SEE IF NEW BUY                               
         BNZ   UPEL_X              NO, THEN DO NOTHING FOR MAD UPLOADS          
*                                                                               
         L     R5,ATHISTMP                                                      
         LA    R5,2(R5)            MUST BUMP PAST LENGHT                        
*                                                                               
         USING PINSD,R5                                                         
         CLC   PINSTYPE,=C'INS*'                                                
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
         MVI   PIUPUSRC,X'01'                                                   
         MVC   PIUPUDAT(73),PINSDATE                                            
         MVC   PIUPUSEQ,PINSUNIQ                                                
         MVC   PIUPUQXT,PINSUNIQ+L'PIUPUSEQ                                     
*                                                                               
         MVC   PIUPTYPE,SAVUHDR+PHDRUTYP-PHDRD                                  
         CLC   SAVUHDR+PHDRUTYP-PHDRD(2),=C'E2'                                 
         BNE   UPEL50                                                           
         MVC   PIUPEDRP,PINSPUB    EDR PUB CODE                                 
*                                                                               
UPEL50   DS    0H                                                               
         LA    R5,NEWREC+33                                                     
         LA    R4,NEWREC                                                        
         OC    INSDA,INSDA         SEE IF NEW BUY                               
         BZ    UPEL53                                                           
*                                                                               
         LA    R5,REC+33                                                        
         LA    R4,REC                                                           
*                                                                               
UPEL53   MVI   ELCODE,X'90'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   UPEL55                                                           
         CLI   UPLSW,C'Y'          SEE IF UPLOADING USING UPID=                 
         BE    UPEL54                                                           
         DC    H'0'                SOMETHING SCREWY                             
*                                                                               
UPEL54   ICM   R2,15,UPCURSOR      BUY ALREADY HAS UPLOAD ELEM                  
         LA    R3,INVERR           OR NO UPLOAD RECS FOUND FOR THIS EST         
         XC    UPID,UPID                                                        
         MVI   UPLSW,0                                                          
         J     ERROR                                                            
*                                                                               
UPEL55   GOTO1 VRECUP,DMCB,(1,(R4)),X,(R5)                                      
*                                                                               
UPEL_X   J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  R2,R5,RB                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FIND AND UPDATE UPLOAD MINIO RECORD                                           
* IF DELETING OR ADDING/CHANGING USING UPID=                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MUP      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MADSW,C'Y'          $MAD UPLOAD/UPDATE?                          
         BE    MUPX                DONE, MINIO REC IS UPDATED BY $MAD           
*                                                                               
* TO SAVE PIUPUSEQ(8),PIUPTYPE(3) AND PIUPEDRP(13)                              
* FROM BUY UPLOAD ELEM (PIUPLELEM)                                              
*                                                                               
         XC    WORK(24),WORK                                                    
         LA    R5,NEWREC+33                                                     
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'                                                       
         BE    *+8                                                              
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'90'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   MUPX                                                             
*                                                                               
         OC    INSDA,INSDA         SEE IF I HAVE DISK ADDR                      
         BZ    MUP5                                                             
         TM    PBUYCNTL-PBUYREC+REC,X'80'                                       
         BNZ   MDEL                                                             
*                                                                               
MUP5     CLI   UPLSW,0             SEE IF UPLOADING USING UPID=                 
         BE    MUPX                NO THEN JUST EXIT                            
*                                                                               
         USING PIUPELD,R5                                                       
         TM    PIUPUSRC,X'02'      SEE IF UPLOADED USING UPID=                  
         BZ    MUPX                NO - THEN EXIT                               
         CLC   PIUPUSEQ,=CL8' '    SEE IF I HAVE AN ID                          
         BE    MUPX                NO THEN EXIT                                 
         MVC   WORK(8),PIUPUSEQ                                                 
         B     MDEL5                                                            
         DROP  R5                                                               
*                                                                               
MDEL     DS    0H                  DELETING                                     
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'90'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   MUPX                EXIT IF I DON'T FIND                         
         USING PIUPELD,R5                                                       
         MVC   WORK(8),PIUPUSEQ    UNIQUE ID                                    
         CLC   WORK(8),=CL8' '                                                  
         BE    MUPX                ELEMENT HAS NO UNIQUE ID                     
*                                                                               
         CLI   PIUPELLN,PIUPEL9Q                                                
         BL    MDEL5                                                            
*                                                                               
         MVC   WORK+8(3),PIUPTYPE                                               
         MVC   WORK+11(13),PIUPEDRP                                             
         DROP  R5                                                               
*                                                                               
MDEL5    L     R5,AMINBLK          UPLOAD ELEM                                  
         USING MINBLKD,R5                                                       
         BRAS  RE,MINIT            INITIALIZE MINIO BLOCK                       
*                                                                               
* BUILD THE MASTER KEY                                                          
*                                                                               
         LA    R4,NEWREC                                                        
*                                                                               
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'                                                       
         BE    *+8                                                              
         LA    R4,REC                                                           
*                                                                               
         LA    R3,MINMKEY                                                       
         USING PEUPKEY,R3                                                       
*                                                                               
         MVC   PEUPAGY,PBUYKAGY-NEWREC(R4)                                      
         MVC   PEUPMED,PBUYKMED-NEWREC(R4)                                      
         MVI   PEUPTYPE,X'90'                                                   
         MVC   PEUPCLT,PBUYKCLT-NEWREC(R4)                                      
         MVC   PEUPPRD,PBUYKPRD-NEWREC(R4)                                      
         MVC   PEUPEST,PBUYKEST-NEWREC(R4)                                      
*                                                                               
         CLC   WORK+8(2),=C'E2'    SEE IF FROM EDR UPLOAD                       
         BNE   MDEL5F                                                           
*                                                                               
         CLC   WORK+19(5),=C'*****'                                             
         BNE   MDEL5F                                                           
         PACK  DUB,WORK+11(8)                                                   
         LM    R0,R1,DUB                                                        
         SRDL  R0,4                                                             
         STCM  R1,15,PEUPPUB                                                    
         MVI   PEUPPUB+4,0                                                      
         MVI   PEUPPUB+5,X'FF'                                                  
         B     MDEL5X                                                           
*                                                                               
MDEL5F   MVC   PEUPPUB,PBUYKPUB-NEWREC(R4)                                      
         DROP  R3                                                               
*                                                                               
* READ AND UPDATE THE STATUS ELEMENT FOR AN EXISTING MINIO RECORD               
*                                                                               
MDEL5X   DS    0H                                                               
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
*                                                                               
         GOTO1 VMINIO,DMCB,('MINRD',(R5))                                       
*                                                                               
         CLI   MINERR,MINESNF                                                   
         BE    MDEL10              MINIO RECORD NOT FOUND                       
         CLI   MINERR,0                                                         
         BE    DELMIN                                                           
         DC    H'0'                                                             
*                                                                               
MDEL10   DS    0H                  HERE IF MINIO RECORD NOT FOUND               
         OC    INSDA,INSDA         SEE IF I HAVE DISK ADDR                      
         BZ    MDEL11                                                           
         TM    PBUYCNTL-PBUYREC+REC,X'80'                                       
         BNZ   MUPX                                                             
*                                                                               
MDEL11   DS    0H                                                               
*                                                                               
* ADD MINIO RECORD WITH X'01' STATUS ELEMENT                                    
*                                                                               
         XC    DUMEL(40),DUMEL                                                  
         LA    R3,DUMEL                                                         
         USING PEUPEL01,R3                                                      
         MVI   PEUPEL01,X'01'      ELEMENT CODE                                 
         MVI   PEUPEL1L,PEUPEL1Q   ELEMENT LENGTH                               
         GOTO1 VDATCON,DMCB,(5,0),(3,PEUPUDT)  UPLOAD DATE                      
         MVI   PEUPSEQN,1          FIRST UPLOAD OF THIS PUB                     
         DROP  R3                                                               
*                                                                               
         GOTO1 VMINIO,DMCB,('MINADD',(R5))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VMINIO,DMCB,('MINCLS',(R5))                                      
         CLI   MINERR,0                                                         
         BE    DELMIN              PROCESS THE INSERTION                        
         DC    H'0'                                                             
*                                                                               
* WORK ON MINIO ELEMENT                                                         
*                                                                               
DELMIN   XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
*                                                                               
         GOTO1 =V(SIXPACK),DMCB,WORK,MINEKEY+1,16,RR=WKRELO03                   
*                                                                               
         GOTO1 VMINIO,DMCB,('MINRD',(R5))                                       
*                                                                               
         OC    INSDA,INSDA         SEE IF I HAVE DISK ADDR                      
         BZ    DELMIN2                                                          
         TM    PBUYCNTL-PBUYREC+REC,X'80'                                       
         BNZ   MDEL20              DELETED BUYREC                               
*                                                                               
DELMIN2  DS    0H                                                               
*                                                                               
* MUST GET NOT FOUND WHEN ADDING/CHANGING                                       
*                                                                               
         CLI   MINERR,MINERNF                                                   
         BE    MDEL15              OK TO ADD MINIO ELEMENT                      
*                                                                               
MDELERR  DS    0H                                                               
*                                                                               
         OC    INSDA,INSDA         SEE IF I HAVE DISK ADDR                      
         BZ    MDELERR5                                                         
         TM    PBUYCNTL-PBUYREC+REC,X'80'                                       
         BNZ   MUPX                                                             
*                                                                               
MDELERR5 ICM   R2,15,UPCURSOR      ADDR OF OPT DATA LINE WITH UPID=             
         LA    R3,INVERR                                                        
         J     ERROR                                                            
*                                                                               
MDEL15   DS    0H                  HERE WHEN ADDING MINIO ELEM                  
         XC    DUMEL(40),DUMEL                                                  
         LA    R2,DUMEL                                                         
         USING PEUPMEL,R2                                                       
         MVI   PEUPEL90,X'90'                                                   
         MVI   PEUPEL9L,PEUP90LQ   ELEMENT LENGTH                               
         MVC   PEUPUNID,MINEKEY+1  UNIQUE ID (6 PACKED)                         
         MVI   PEUPUSRC,X'02'      UPLOAD FROM $BUY                             
         MVC   PEUPUNIQ,WORK       UNIQUE ID (CHARACTERS)                       
         MVC   PEUPUIQX,WORK+L'PEUPUNIQ                                         
*                                                                               
         LA    R3,REC                                                           
         L     RE,TRADDR           SEE IF NEW BUY                               
         CLI   8(RE),C'B'                                                       
         BNE   MDEL17                                                           
         LA    R3,NEWREC                                                        
         CLI   PBUYKLIN-PBUYREC(R3),0                                           
         BE    MUPX                                                             
*                                                                               
MDEL17   MVC   PEUPPDT,PBUYKDAT-PBUYREC(R3)                                     
         MVC   PEUPPLIN,PBUYKLIN-PBUYREC(R3)                                    
*                                                                               
* NEW BUY MAY NOT HAVE A LINE NUMBER YET                                        
*                                                                               
         GOTO1 VMINIO,DMCB,('MINADD',(R5))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     MDEL30                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
MDEL20   CLI   MINERR,MINERNF                                                   
         BE    MUPX                EXIT IF MINIO ELEMENT NOT FOUND              
*                                                                               
MDEL25   CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,MINELEM          POINT TO FOUND ELEMENT                       
         USING PEUPMEL,R2                                                       
*                                                                               
         CLC   PEUPPDT,PBUYKDAT-PBUYREC+REC                                     
         BNE   MUPX                SKIP IF NOT CORRECT INSERTION                
         CLC   PEUPPLIN,PBUYKLIN-PBUYREC+REC                                    
         BNE   MUPX                SKIP IF NOT CORRECT INSERTION                
*                                                                               
         GOTO1 VMINIO,DMCB,('MINDEL',(R5))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
* MUST CLOSE AFTER MAKING CHANGES                                               
*                                                                               
MDEL30   GOTO1 VMINIO,DMCB,('MINCLS',(R5))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
MUPX     J     X_XIT1                                                           
*                                                                               
MINIT    DS    0H                  INIT MINIO PARAMETER BLOCK                   
         ST    RE,SAVERE                                                        
         LR    RE,R5               CLEAR MINIO PARAMETER BLOCK                  
         LH    RF,=Y(MINBLKL)                                                   
         XCEFL                                                                  
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=C'PRTFIL  ' FILE NAME                                    
         MVC   MINDIR,=C'PRTDIR  ' DIR NAME                                     
         MVI   MINFKLEN,25         KEY LENGTH                                   
         MVI   MINNCTL,2           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=Y(LENMINRC)                                            
         MVI   MINEKLEN,L'PEUPEKEY ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,PEUPEKEY-PEUPREC                                        
         MVC   MINBUFF,AMINBUFF    A(FIRST BUFFER)                              
         MVC   MINRTAB,AMINRECT    A(AREA FOR RECORD TABLE)                     
         LA    RF,DUMEL                                                         
         ST    RF,MINELEM          A(AREA FOR MINIO ELEMENT)                    
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   MINRECUP,VRECUP     A(RECUP)                                     
         MVC   MINRTABL,=Y(MINRCTBL)                                            
         MVC   MINMAXEL,=H'250'    MAX LENGTH OF ELEMENT                        
         MVI   MINNBUF,NUMMNBUF    NUMBER OF AVAILABLE BUFFERS                  
         L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
                                                                                
*                                                                               
         DROP  R5,RB                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ASC      NTR1  BASE=*,LABEL=*      AUTOMATIC SCHEDULE CHECKING                  
*                                                                               
         NI    DMINBTS,X'7F'       SET OFF READ FOR UPDATE                      
*                                                                               
         CLI   PBDBFD-PBUYREC+NEWREC,C'T'                                       
         BE    ASCXX                                                            
         CLI   PBDSPACE-PBUYREC+NEWREC,C'*'                                     
         BE    ASCXX                                                            
         CLI   PBDSPACE-PBUYREC+NEWREC,C'#'                                     
         BE    ASCXX                                                            
*                                                                               
         CLI   NEWREC+2,C'O'       SEE IF OUTDOOR                               
         BNE   ASC2                                                             
         CP    PBDSHOW-PBUYREC+NEWREC,=P'0'                                     
         BNE   ASC2                                                             
*                                                                               
* MUST SEE IF FIRST COMMENT ELEMENT IS LEST THAN 17                             
* AND STARTS WITH AN * - THIS MEANS IT IS NOT A "REAL" BUY                      
* IF IT STARTS WITH A # - THIS MEANS TO IGNORE FOR SCHEDULE CHECKING            
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'66'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   ASC2                                                             
         CLI   1(R5),19                                                         
         BH    ASC2                                                             
         CLI   2(R5),C'*'                                                       
         BE    ASCXX                                                            
         CLI   2(R5),C'#'          IGNORE FOR SCHEDULE CHECKING?                
         BE    ASCXX                                                            
*                                                                               
ASC2     CLC   BUYPB(2),=C'L='     SEE IF LIST BUYING                           
         BE    ASCL                                                             
         CLC   BUYPB(3),=C'LW='    SEE IF LIST BUYING                           
         BE    ASCL                                                             
*                                                                               
         OC    SADVDATA,SADVDATA   NEW ADV SYSTEM?                              
         BZ    ASC3                                                             
         TM    SVAORC,X'08'        LOOKING UP CONTRACT RATES?                   
         BO    ASC3                YES, I CAN USE CONTRACTS IN SVCON            
         TM    SVAORC,X'10'        ADV SCHEDULE?                                
         BO    ASCL                MUST USE LIST LOGIC                          
*                                                                               
* OTHERWISE JUST USE CONTRACTS IN SVCON                                         
* AS THEY WILL BE THE AGY'S CONTRACTS                                           
*                                                                               
* PICK A CONTRACT COVERING BUY DATE                                             
*                                                                               
* MUST USE DATE IN REC                                                          
* NOT BINSDT - SINCE THAT IS NOT SET FOR DELETES                                
*                                                                               
ASC3     GOTO1 VDATCON,DMCB,(3,NEWREC+16),(2,DUB)                               
         LA    RE,SVCON-8                                                       
         LA    R0,SVCON+L'SVCON-1                                               
         B     ASC4B                                                            
ASC4A    CLC   2(2,RE),DUB         CON END BEFORE BUY                           
         BL    ASC4B                                                            
         CLC   DUB(2),0(RE)        BUY BEFORE CON START                         
         BL    ASC4B                                                            
         B     ASCUPD              CONTRACT FOUND                               
*                                                                               
ASC4B    LA    RE,8(RE)            NEXT SLOT                                    
         CLI   0(RE),0                                                          
         BNE   ASC4A                                                            
         B     ASCXX               NO CONTRACT - DONE                           
*                                                                               
* FOR LIST BUYING MUST FIND APPLICABLE CONTRACT                                 
*                                                                               
ASCL     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'10'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         CLI   SVCLPROF+5,C'2'     TEST SLAVE CLIENT                            
         BNE   *+10                                                             
         MVC   KEY+4(3),SVCLPROF+6 READ MASTER CLT CONTRACTS                    
         MVC   KEY+7(6),BPUB                                                    
*                                                                               
         OC    SADVDATA,SADVDATA   NEW ADV SYSTEM                               
         BZ    ASCL1               NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    ASCL1                                                            
         CLC   SVAOR,AGYALPHA      SEE IF AM THE AOR                            
         BE    ASCL1                                                            
*                                                                               
         TM    SVAORC,X'01'        TEST PUB LINK                                
         BZ    *+10                                                             
         MVC   KEY+7(6),SVADVPUB                                                
         MVC   KEY(2),SVAOR        AOR                                          
         MVC   KEY+4(3),SVADV      ADV                                          
*                                                                               
* MUST SWITCH TO AOR                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASCL1                                                            
         LA    R3,ADVFERR          AOR NOT ACTIVE                               
         J     ERROR                                                            
*                                                                               
ASCL1    BRAS  RE,PRT_RDHI                                                      
         B     ASCL2B                                                           
ASCL2    BRAS  RE,PRT_RSEQ                                                      
ASCL2B   CLC   KEYSAVE(13),KEY     TEST SAME A/M/REC/CL/PUB                     
         BNE   ASCL2X                                                           
         MVC   AREC,ACONIO         READ INTO CONIO                              
         BRAS  RE,PRT_GETR                                                      
*                                                                               
ASCL2C   DS    0H                                                               
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
         L     R6,ACONIO                                                        
         USING PCONRECD,R6                                                      
         CLC   PCONEND(3),NEWREC+16                                             
         BL    ASCL2                                                            
         CLC   NEWREC+16(3),PCONSTRT                                            
         BL    ASCL2                                                            
         CLI   PCONPRD,C'A'        SEE IF PRD CONTRACT                          
         BL    ASCL2F                                                           
         CLC   PCONPRD,BUYPR       PRD CONTRACT - MUST MATCH                    
         BNE   ASCL2                                                            
*                                                                               
ASCL2F   DS    0H                                                               
         OC    SADVDATA,SADVDATA   NEW ADV SYSTEM                               
         BZ    ASCL2G              NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    ASCL2G                                                           
         CLC   SVAOR,AGYALPHA      SEE IF AM THE AOR                            
         BE    ASCL2G                                                           
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
ASCL2G   DS    0H                                                               
         B     ASCUPD5             CON FND GO CHECK FOR PCATMAX                 
*                                                                               
ASCL2X   DS    0H                                                               
         OC    SADVDATA,SADVDATA   NEW ADV SYSTEM                               
         BZ    ASCL2XX             NO                                           
         TM    SVAORC,X'10'        ADV SCHEDULE CHECKING                        
         BZ    ASCL2XX                                                          
         CLC   SVAOR,AGYALPHA      SEE IF AM THE AOR                            
         BE    ASCL2XX                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
ASCL2XX  DS    0H                                                               
         B     ASCX                LIST BUYING - NO CONTRACT                    
*                                                                               
ASCUPD   XC    KEY,KEY                                                          
         MVC   KEY+27(4),4(RE)                                                  
         MVC   AREC,ACONIO         READ INTO CONIO                              
*                                                                               
         OC    SADVDATA,SADVDATA   SEE IF NEW2 ADV SYSTEM                       
         BZ    ASCUPD3                                                          
*                                                                               
* FIRST CHECK ADV SYSTEM CONTRACT RATE LOOK-UP                                  
* IF NOT,THEN I CAN ONLY CHECK AGENCY'S CONTRACT                                
*                                                                               
         TM    SVAORC,X'08'                                                     
         BZ    ASCUPD3                                                          
*                                                                               
*                                                                               
* NOTE THAT I CAN ONLY CHECK ADV SCHEDULES IF I AM DOING                        
* CONTRACT RATE LOOK-UP                                                         
*                                                                               
         TM    SVAORC,X'10'        ONES IF ADV SYSTEM CHECKING                  
         BZ    ASCX                                                             
*                                                                               
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    ASCUPD3                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASCUPD3                                                          
         LA    R3,ADVFERR          *** ADVERTISER FILE NOT ACTIVE ***           
         LA    R2,BUYMDH                                                        
         J     ERROR                                                            
*                                                                               
ASCUPD3  BRAS  RE,PRT_GETR                                                      
*                                                                               
         OC    SADVDATA,SADVDATA   NEW ADV SYSTEM                               
         BZ    ASCUPD4                                                          
*                                                                               
         TM    SVAORC,X'10'        ONES IF ADV SYSTEM CHECKING                  
         BZ    ASCUPD4                                                          
         CLC   SVAOR,AGYALPHA      SEE IF I'M THE AOR                           
         BE    ASCUPD4                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
ASCUPD4  LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
*                                                                               
ASCUPD5  L     R6,ACONIO           NEEDED TO REFERENCE PCONPRD                  
         L     R5,ACONIO                                                        
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'50'        LOOK FOR PCATELEM                            
         BRAS  R9,NEXTEL                                                        
         BNE   ASCX                NO ASR FOR THIS CONTRACT                     
*                                                                               
         USING PCATELEM,R5                                                      
         OC    PCATMAX,PCATMAX     CHECK FOR MAX/ISSUE                          
         BNZ   ASCUPD7                                                          
         OC    PCATMAXZ,PCATMAXZ   OR MAX ACROSS ZONES/EDTS                     
         BZ    ASCX                                                             
*                                                                               
ASCUPD7  MVC   SCONPRD,PCONPRD     MUST SAVE PRODUCT                            
         MVC   SCONMAX,PCATMAX     AND MAXIMUM FOR ASC400                       
         MVC   SCONMAXZ,PCATMAXZ   SAVE ACROSS ZONE/EDTION INDICATOR            
         DROP  R5                                                               
         DROP  R6                                                               
*                                                                               
         TM    SVAORC,X'10'                                                     
         BZ    ASC60                                                            
*                                                                               
ASC10    DS    0H                                                               
         XC    HALF,HALF           TO COUNT INSERTIONS                          
         XC    HALF2,HALF2         TO COUNT INSERTIONS ACROSS Z/E               
*                                                                               
         L     R0,AAORTAB          CLEAR AOR AGY/CLT TABLE                      
         LHI   R1,6000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
* GOTO GETADVC AND RETURN LIST OF AGY/CLTS IN WRKREC                            
*                                                                               
* MUST SWITCH TO CONTROL                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'          CONTROL SYSTEM                               
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASC20                                                            
         LA    R3,CNTFERR          ** CONTROL SYSTEM NOT ACTIVE **              
         LA    R2,BUYMDH                                                        
         J     ERROR                                                            
*                                                                               
ASC20    MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),BUYMD                                                  
         MVC   WORK+2(2),SVAOR                                                  
         MVC   WORK+4(3),SVADV                                                  
         XC    WORK+7(2),WORK+7                                                 
         L     RF,AAORTAB          A(AGY/CLT TABLE)                             
*                                                                               
         GOTO1 =V(GETADVC),DMCB,WORK,(RF),VDATAMGR,RR=WKRELO03                  
*                                                                               
* MUST SWITCH BACK                                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ASC25    DS    0H                                                               
         L     R6,AAORTAB                                                       
ASC27    CLC   0(2,R6),=X'FFFF'    END OF LIST                                  
         BE    ASC50X                                                           
*                                                                               
         USING GETADVCD,R6                                                      
         CLC   GETVAGY,AGYALPHA    SEE IF I'M PROCESSING MYSELF                 
         BE    ASC30                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),GETVSE                                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    ASC30                                                            
         LA    R3,AGYFERR          *** AGENCY FILE NOT ACTIVE ***               
         LA    R2,BUYMDH                                                        
         J     ERROR                                                            
*                                                                               
ASC30    DS    0H                                                               
         SR    R3,R3               RESET ERR NUMBER                             
         MVC   WKAGY,GETVAGY                                                    
         MVC   WKCLT,GETVACLT                                                   
         MVC   WKPUB,BPUB          SET TO BPUB                                  
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BNE   ASC32                                                            
         TM    SVAORC,X'01'        SEE IF PUB LINK REQUIRED                     
         BZ    ASC32C                                                           
ASC32    MVC   WKPUB,SVADVPUB      SET TO ADV PUB                               
ASC32C   TM    GETVCNTL,X'01'      SEE IF PUB LINK                              
         BZ    ASC40                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(3),SVADV      ADVERTISER                                   
         MVC   KEY+5(2),SVAOR                                                   
         MVC   KEY+7(2),WKAGY                                                   
         MVC   KEY+9(6),WKPUB      FIND LINK FOR WKPUB                          
         BRAS  RE,PUB_RDHI                                                      
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ASC45               LINK NOT FOUND - SKIP                        
         MVC   WKPUB,KEY+15                                                     
*                                                                               
ASC40    BRAS  RE,ASC400                                                        
*                                                                               
* NOTE - ERR RETURN (R3 NOT ZERO)                                               
* IS NOW CHECKED AT ASC48 (AFTER I SWITCH BACK)                                 
*                                                                               
ASC45    DS    0H                                                               
         CLC   GETVAGY,AGYALPHA    SEE IF I/M PROCESSING MYSELF                 
         BE    ASC48                                                            
*                                                                               
* MUST SWITCH BACK                                                              
*                                                                               
ASC47    L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ASC48    DS    0H                                                               
         L     R2,TRADDR                                                        
         LTR   R3,R3               ERR ENCOUNTERED?                             
         JNZ   ERROR                                                            
         LA    R6,GETVLEN(R6)                                                   
         B     ASC27                                                            
*                                                                               
ASC50X   DS    0H                                                               
         B     ASCX                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
ASC60    DS    0H                  NON ADV PROCESSING                           
         XC    HALF,HALF           TO COUNT INSERTIONS                          
         XC    HALF2,HALF2         FOR BUYS ACROSS ZONES/EDTS                   
*                                                                               
         MVC   WKAGY,AGYALPHA                                                   
         MVC   WKCLT,BUYCL                                                      
         MVC   WKPUB,BPUB                                                       
*                                                                               
         BRAS  RE,ASC400           FIRST CHECK FOR BUYS FOR THIS CLT            
         L     R2,TRADDR                                                        
         LTR   R3,R3                                                            
         JNZ   ERROR                                                            
         CLI   SVCLPROF+5,C'2'     SEE IF SLAVE CLIENT                          
         BNE   ASCX                NO THE DONE                                  
*                                                                               
* MUST SEARCH THE OTHER SLAVES                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'02'                                                      
         BRAS  RE,PRT_RDHI                                                      
         B     ASC66                                                            
ASC65    BRAS  RE,PRT_RSEQ                                                      
ASC66    CLC   KEY(4),KEYSAVE                                                   
         BNE   ASCX                                                             
         CLC   KEY+4(3),BUYCL      SKIP THIS CLIENT                             
         BE    ASC65                                                            
         MVC   AREC,AWRKREC                                                     
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC             RESET AREC                                   
         L     RF,AWRKREC                                                       
         USING PCLTREC,RF                                                       
         CLI   PCLTPROF+5,C'2'     SEE IF SLAVE                                 
         BNE   ASC65                                                            
         CLC   SVCLPROF+6(3),PCLTPROF+6                                         
         BNE   ASC65               CHECK FOR RIGHT MASTER                       
         DROP  RF                                                               
*                                                                               
         MVC   X(64),KEY           SAVE KEY AND KEYSAVE                         
         MVC   WKCLT,KEY+4         PROCESS THIS SLAVE                           
         BRAS  RE,ASC400                                                        
         L     R2,TRADDR                                                        
         LTR   R3,R3                                                            
         JNZ   ERROR                                                            
         MVC   KEY(64),X           RESTORE KEY AND KEYSAVE                      
         BRAS  RE,PRT_RDHI                                                      
         B     ASC65                                                            
*                                                                               
ASCX     DS    0H                                                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         L     R4,TRADDR                                                        
         CLI   8(R4),C'C'          SEE IF CHANGE                                
         BNE   ASCXX                                                            
*                                                                               
         MVC   KEY+27(4),INSDA     MUST REREAD THE BUY I'M PROCESSING           
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVC   AREC,AWRKREC                                                     
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         NI    DMINBTS,X'F7'       RESET DMINBTS                                
*                                                                               
ASCXX    DS    0H                                                               
         OI    DMINBTS,X'80'       RESET READ FOR UPDATE                        
         J     X_XIT1                                                           
         EJECT                                                                  
*                                                                               
* LOOK FOR BUYS AND COUNT IN HALF AND HALF2                                     
*                                                                               
ASC400   NTR1                                                                   
*                                                                               
         SR    R3,R3               RESET ERR NUMBER                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),WKAGY                                                     
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+4(3),WKCLT      CLIENT CODE                                  
         MVC   KEY+7(4),WKPUB      BASE PUB                                     
*                                                                               
         OC    SCONMAXZ,SCONMAXZ   SEE IF CHECKING ACROSS ZONES/EDTS            
         BNZ   *+10                                                             
         MVC   KEY+11(2),WKPUB+4   ADD ZONE/EDT                                 
*                                                                               
         CLI   SCONPRD,C'A'        CHK FOR PRODUCT CONTRACT                     
         BL    *+10                                                             
         MVC   KEY+13(3),SCONPRD   ONLY SEARCH FOR THIS PRODUCT                 
         BRAS  RE,PRT_RDHI                                                      
         B     ASC406                                                           
ASC405   BRAS  RE,PRT_RSEQ                                                      
*                                                                               
ASC406   CLC   KEY(11),KEYSAVE     CHECK AGY/MED/CLT/BASE PUB                   
         BNE   XIT_R3R4                                                         
         OC    SCONMAXZ,SCONMAXZ   SEE IF CHECKING ACROSS ZONES/EDTS            
         BNZ   ASC406C                                                          
*                                                                               
         CLC   KEY(13),KEYSAVE     CHECK AGY/MED/CLT/FULL PUB                   
         BNE   XIT_R3R4                                                         
*                                                                               
ASC406C  DS    0H                                                               
         CLI   SCONPRD,C'A'                                                     
         BL    ASC408                                                           
         CLC   KEY+13(3),KEYSAVE+13                                             
         BE    ASC408                                                           
*                                                                               
         OC    SCONMAXZ,SCONMAXZ   SEE IF CHECKING ACROSS ZONES/EDTS            
         BZ    XIT_R3R4            NO - THEN DONE SEARCHING                     
         B     ASC405              MUST CONTINUE                                
*                                                                               
ASC408   CLC   KEY+16(3),NEWREC+16 MUST MATCH DATES                             
         BNE   ASC405                                                           
         TM    KEY+25,X'80'        TEST DELETED                                 
         BNZ   ASC405                                                           
         MVC   AREC,AWRKREC        MUST READ BUY                                
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC             RESET AREC TO REC                            
         L     R6,AWRKREC                                                       
*                                                                               
         CLI   PBDBFD-PBUYREC(R6),C'T'                                          
         BE    ASC405                                                           
         CLI   PBDSPACE-PBUYREC(R6),C'*'                                        
         BE    ASC405                                                           
*                                                                               
         CLI   PBDSPACE-PBUYREC(R6),C'#'                                        
         BE    ASC405                                                           
*                                                                               
         CLI   2(R6),C'O'         SEE IF OUTDOOR                                
         BNE   ASC410                                                           
         CP    PBDSHOW-PBUYREC(3,R6),=P'0'                                      
         BNE   ASC410                                                           
         LA    R5,33(R6)                                                        
         MVI   ELCODE,X'66'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   ASC410                                                           
         CLI   1(R5),19                                                         
         BH    ASC410                                                           
         CLI   2(R5),C'*'                                                       
         BE    ASC405              SKIP THIS BUY                                
*                                                                               
         CLI   2(R5),C'#'          SEE IF THEI BUY IS TO BE IGNORED             
         BE    ASC405              FOR SCHEDULE CHECKING                        
*                                                                               
ASC410   LH    R4,HALF2            BUMP ACROSS ZONE/EDTS COUNTER                
         LA    R4,1(R4)                                                         
         STH   R4,HALF2                                                         
         OC    SCONMAXZ,SCONMAXZ   SEE IF I HAVE A MAX/ISSUE                    
         BZ    ASC420                                                           
         CLC   HALF2,SCONMAXZ                                                   
         BNL   ASC430                                                           
*                                                                               
ASC420   DS    0H                                                               
         CLC   KEY+11(2),WKPUB+4   SEE IF "RIGHT" ZONE/EDT                      
         BNE   ASC405                                                           
*                                                                               
         LH    R4,HALF             BUMP BUY COUNTER                             
         LA    R4,1(R4)                                                         
         STH   R4,HALF                                                          
         OC    SCONMAX,SCONMAX     SEE IF I HAVE A MAX/ISSUE                    
         BZ    ASC405              NO - KEEP PROCESSING                         
         CLC   HALF,SCONMAX                                                     
         BL    ASC405                                                           
*                                                                               
ASC430   LA    R3,CONMERR          CONTRACT MAX BUYS PER ISSUE REACHED          
         L     R2,TRADDR           CURSOR TO TRANSACTION                        
*                                                                               
XIT_R3R4 XIT1  REGS=(R3,R4)                                                     
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COM      NTR1  BASE=*,LABEL=*      COMPETITIVE BRAND CHECKING                   
*                                                                               
         NI    DMINBTS,X'7F'       SET OFF READ FOR UPDATE                      
*                                                                               
         OC    SVPADJCS,SVPADJCS   SEE IF THIS PRODUCT HAS ADJ CODES            
         BZ    COMXX               NO - JUST EXIT                               
*                                                                               
         CLI   BYPROF+3,C'Y'       SEE IF CONSIDERING TEST BUYS                 
         BE    COM1                YES - THEN SKIP TEST BUY CHECK               
*                                                                               
         CLI   PBDBFD-PBUYREC+NEWREC,C'T'                                       
         BE    COMXX                                                            
*                                                                               
COM1     DS    0H                                                               
         CLI   PBDSPACE-PBUYREC+NEWREC,C'*'                                     
         BE    COMXX                                                            
         CLI   PBDSPACE-PBUYREC+NEWREC,C'#'                                     
         BE    COMXX                                                            
*                                                                               
         CLI   NEWREC+2,C'O'       SEE IF OUTDOOR                               
         BNE   COM2                                                             
         CP    PBDSHOW-PBUYREC+NEWREC,=P'0'                                     
         BNE   COM2                                                             
*                                                                               
* MUST SEE IF FIRST COMMENT ELEMENT IS LEST THAN 17                             
* AND STARTS WITH AN * - THIS MEANS IT IS NOT A "REAL" BUY                      
* IF IT STARTS WITH A # - THIS MEANS TO IGNORE FOR SCHEDULE CHECKING            
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'66'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   COM2                                                             
         CLI   1(R5),19                                                         
         BH    COM2                                                             
         CLI   2(R5),C'*'                                                       
         BE    COMXX                                                            
         CLI   2(R5),C'#'          MEANS IGNORE FOR SCHEDULE CHECKING           
         BE    COMXX                                                            
*                                                                               
COM2     DS    0H                                                               
*                                                                               
COM10    DS    0H                                                               
         OC    SADVDATA,SADVDATA   SEE IF NEW ADVERTISER SYSTEM                 
         BZ    COM60               USE NORMAL CHECKING                          
*                                                                               
         L     R0,AAORTAB          CLEAR AOR AGY/CLT TABLE                      
         LHI   R1,6000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
* GOTO GETADVC AND RETURN LIST OF AGY/CLTS IN WRKREC                            
*                                                                               
* MUST SWITCH TO CONTROL                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'          CONTROL SYSTEM                               
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    COM20                                                            
         LA    R3,CNTFERR          ** CONTROL SYSTEM NOT ACTIVE **              
         LA    R2,BUYMDH                                                        
         J     ERROR                                                            
*                                                                               
COM20    MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),BUYMD                                                  
         MVC   WORK+2(2),SVAOR                                                  
         MVC   WORK+4(3),SVADV                                                  
         XC    WORK+7(2),WORK+7                                                 
         L     RF,AAORTAB          A(AOR AGY/CLT TABLE)                         
*                                                                               
         GOTO1 =V(GETADVC),DMCB,WORK,(RF),VDATAMGR,RR=WKRELO03                  
*                                                                               
* MUST SWITCH BACK                                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COM25    DS    0H                                                               
         L     R6,AAORTAB                                                       
COM27    CLC   0(2,R6),=X'FFFF'    END OF LIST                                  
         BE    COM50X                                                           
*                                                                               
         USING GETADVCD,R6                                                      
         CLC   GETVAGY,AGYALPHA    SEE IF I'M PROCESSING MYSELF                 
         BE    COM30                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),GETVSE                                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    COM30                                                            
         LA    R3,AGYFERR          *** AGENCY FILE NOT ACTIVE ***               
         LA    R2,BUYMDH                                                        
         J     ERROR                                                            
*                                                                               
COM30    DS    0H                                                               
         SR    R3,R3               RESET ERR NUMBER                             
         MVC   WKAGY,GETVAGY                                                    
         MVC   WKCLT,GETVACLT                                                   
         MVC   WKPUB,BPUB          SET TO BPUB                                  
         CLC   SVAOR,AGYALPHA      SEE IF I AM THE AOR                          
         BNE   COM32                                                            
         TM    SVAORC,X'01'        SEE IF PUB LINK REQUIRED                     
         BZ    COM32C                                                           
COM32    MVC   WKPUB,SVADVPUB      SET TO ADV PUB                               
COM32C   TM    GETVCNTL,X'01'      SEE IF PUB LINK                              
         BZ    COM40                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(3),SVADV      ADVERTISER                                   
         MVC   KEY+5(2),SVAOR                                                   
         MVC   KEY+7(2),WKAGY                                                   
         MVC   KEY+9(6),WKPUB      FIND LINK FOR WKPUB                          
         BRAS  RE,PUB_RDHI                                                      
         CLC   KEY(15),KEYSAVE                                                  
         BNE   COM45               LINK NOT FOUND - SKIP                        
         MVC   WKPUB,KEY+15                                                     
*                                                                               
COM40    BRAS  RE,COM400                                                        
*                                                                               
* NOTE - ERR RETURN (R3 NOT ZERO)                                               
* IS NOW CHECKED AT COM48 (AFTER I SWITCH BACK)                                 
*                                                                               
COM45    DS    0H                                                               
         CLC   GETVAGY,AGYALPHA    SEE IF I/M PROCESSING MYSELF                 
         BE    COM48                                                            
*                                                                               
* MUST SWITCH BACK                                                              
*                                                                               
COM47    L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COM48    DS    0H                                                               
         L     R2,TRADDR                                                        
         LTR   R3,R3               ERR ENCOUNTERED?                             
         JNZ   ERROR                                                            
         LA    R6,GETVLEN(R6)                                                   
         B     COM27                                                            
*                                                                               
COM50X   DS    0H                                                               
         B     COMX                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
COM60    DS    0H                  NON-ADV PROCESSING                           
         XC    HALF,HALF           TO COUNT INSERTIONS                          
         XC    HALF2,HALF2         FOR BUYS ACROSS ZONES/EDTS                   
*                                                                               
         MVC   WKAGY,AGYALPHA                                                   
         MVC   WKCLT,BUYCL                                                      
         MVC   WKPUB,BPUB                                                       
*                                                                               
         BRAS  RE,COM400           FIRST CHECK FOR BUYS FOR THIS CLT            
         L     R2,TRADDR                                                        
         LTR   R3,R3                                                            
         JNZ   ERROR                                                            
*                                                                               
         CLI   BYPROF+2,C'Y'       SEE IF DOING SLAVE CLIENT CHECKING           
         BNE   COMX                IF NOT THEN I'M DONE                         
         CLI   SVCLPROF+5,C'2'     SEE IF SLAVE CLIENT                          
         BNE   COMX                NO THE DONE                                  
*                                                                               
* MUST SEARCH THE OTHER SLAVES                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'02'                                                      
         BRAS  RE,PRT_RDHI                                                      
         B     COM66                                                            
COM65    BRAS  RE,PRT_RSEQ                                                      
COM66    CLC   KEY(4),KEYSAVE                                                   
         BNE   COMX                                                             
         CLC   KEY+4(3),BUYCL      SKIP THIS CLIENT                             
         BE    COM65                                                            
         MVC   AREC,AWRKREC                                                     
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC             RESET AREC                                   
         L     RF,AWRKREC                                                       
         USING PCLTREC,RF                                                       
         CLI   PCLTPROF+5,C'2'     SEE IF SLAVE                                 
         BNE   COM65                                                            
         CLC   SVCLPROF+6(3),PCLTPROF+6                                         
         BNE   COM65               CHECK FOR RIGHT MASTER                       
         DROP  RF                                                               
*                                                                               
         MVC   X(64),KEY           SAVE KEY AND KEYSAVE                         
         MVC   WKCLT,KEY+4         PROCESS THIS SLAVE                           
         BRAS  RE,COM400                                                        
         L     R2,TRADDR                                                        
         LTR   R3,R3                                                            
         JNZ   ERROR                                                            
         MVC   KEY(64),X           RESTORE KEY AND KEYSAVE                      
         BRAS  RE,PRT_RDHI                                                      
         B     COM65                                                            
*                                                                               
COMX     DS    0H                                                               
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         L     R4,TRADDR                                                        
         CLI   8(R4),C'C'          SEE IF CHANGE                                
         BNE   COMXX                                                            
*                                  MUST REREAD THE BUY I'M PROCESSING           
         MVC   KEY+27(4),INSDA                                                  
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVC   AREC,AWRKREC                                                     
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         NI    DMINBTS,X'F7'       RESET DMINBTS                                
*                                                                               
COMXX    DS    0H                                                               
         OI    DMINBTS,X'80'       RESET READ FOR UPDATE                        
         J     X_XIT1                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUNTINE BUILDS A TABLE OF PRODUCTS (READING X'A6' POINTERS)             
* THAT HAVE A PPRDEXCL CODE THAT MATCHES THE PRODUCT I'M PROCESSING             
* IT THEN CHECKS FOR BUYS FOR THOSE PRODUCTS FOR THE SAME PUB                   
* AND DATE AND RETURNS AN ERR IF ONE IS FOUND                                   
*                                                                               
COM400   NTR1                                                                   
*                                                                               
         XC    SECBLK(240),SECBLK                                               
         XC    SECBLK+240(240),SECBLK+240                                       
         XC    SECBLK+480(240),SECBLK+480                                       
         XC    SECBLK+720(240),SECBLK+720                                       
*                                                                               
         LA    R3,SVPADJCS         PRODUCT ADJANCENY CODES                      
         LA    R5,3                FOR BCT                                      
*                                                                               
         LA    R6,SECBLK           (SAVED FROM PRODUCT IN PPBUY01)              
*                                                                               
COM400A  CLI   0(R3),C' '          SEE IF CODE PRESENT                          
         BNH   COM400U                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),WKAGY                                                     
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'A6'                                                      
         MVC   KEY+4(3),WKCLT      CLIENT CODE                                  
         MVC   KEY+7(1),0(R3)                                                   
         BRAS  RE,PRT_RDHI                                                      
         B     COM400T                                                          
*                                                                               
COM400S  BRAS  RE,PRT_RSEQ                                                      
*                                                                               
COM400T  CLC   KEY(8),KEYSAVE      CHECK THROUGH CODE                           
         BNE   COM400U                                                          
         CLC   KEY+8(3),BUYPR      SKIP THIS PRODUCT                            
         BE    COM400S                                                          
         MVC   0(3,R6),KEY+8       SAVE PRODUCT CODE                            
         LA    R6,3(R6)                                                         
         B     COM400S                                                          
*                                                                               
COM400U  LA    R3,1(R3)                                                         
         BCT   R5,COM400A                                                       
*                                                                               
COM400X  DS    0H                                                               
         SR    R3,R3               RESET ERR  NUMBER                            
*                                                                               
         OC    SECBLK(3),SECBLK    SEE IF I FOUND ANY PRODUCTS                  
         BZ    XIT_R3                                                           
*                                                                               
COM402   DS    0H                                                               
         LA    R4,SECBLK                                                        
COM402A  CLI   0(R4),0             SEE IF AT END OF LIST                        
         BE    XIT_R3                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),WKAGY                                                     
         MVC   KEY+2(1),BUYMD      MEDIA                                        
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+4(3),WKCLT      CLIENT CODE                                  
         MVC   KEY+7(6),WKPUB      USE FULL PUB                                 
*                                                                               
         MVC   KEY+13(3),0(R4)     ONLY SEARCH FOR THIS PRODUCT                 
         BRAS  RE,PRT_RDHI                                                      
         B     COM406                                                           
COM405   BRAS  RE,PRT_RSEQ                                                      
*                                                                               
COM406   CLC   KEY(16),KEYSAVE     CHECK AGY/MED/CLT/FULL PUB/PRD               
         BE    COM408                                                           
         B     COM460              GO TRY NEXT PRODUCT                          
*                                                                               
COM408   CLC   KEY+16(3),NEWREC+16 MUST MATCH DATES                             
         BNE   COM405                                                           
         TM    KEY+25,X'80'        TEST DELETED                                 
         BNZ   COM405                                                           
         MVC   AREC,AWRKREC        MUST READ BUY                                
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC             RESET AREC TO REC                            
         L     R6,AWRKREC                                                       
*                                                                               
         CLI   BYPROF+3,C'Y'       SEE IF CHECKING TEST STATUS                  
         BE    COM409                                                           
*                                                                               
         CLI   PBDBFD-PBUYREC(R6),C'T'                                          
         BE    COM405                                                           
*                                                                               
COM409   DS    0H                                                               
         CLI   PBDSPACE-PBUYREC(R6),C'*'                                        
         BE    COM405                                                           
*                                                                               
         CLI   PBDSPACE-PBUYREC(R6),C'#'                                        
         BE    COM405                                                           
*                                                                               
         CLI   2(R6),C'O'         SEE IF OUTDOOR                                
         BNE   COM410                                                           
         CP    PBDSHOW-PBUYREC(3,R6),=P'0'                                      
         BNE   COM410                                                           
         LA    R5,33(R6)                                                        
         MVI   ELCODE,X'66'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   COM410                                                           
         CLI   1(R5),19                                                         
         BH    COM410                                                           
         CLI   2(R5),C'*'                                                       
         BE    COM405              SKIP THIS BUY                                
*                                                                               
         CLI   2(R5),C'#'          SEE IF THEI BUY IS TO BE IGNORED             
         BE    COM405              FOR SCHEDULE CHECKING                        
*                                                                               
COM410   DS    0H                                                               
*                                                                               
COM430   LA    R3,COMPERR          COMPETITIVE BRAND ERR MSG                    
         L     R2,TRADDR           CURSOR TO TRANSACTION                        
         B     XIT_R3                                                           
*                                                                               
COM460   DS    0H                                                               
         LA    R4,3(R4)            BUMP TO NEXT PRODUCT (IN SECBLK)             
         B     COM402A                                                          
*                                                                               
XIT_R3   XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FSILOOK  NTR1  BASE=*,LABEL=*      LOOK-UP FSI DATA AND CREATE ELEM             
*                                                                               
         LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'82'                                                     
         BRAS  R9,NEXTEL           ALREADY HAS A FSI ELEM?                      
         BNE   FSIL5                                                            
         USING PBFSIELD,R5                                                      
         CLI   PBFSI,C'Y'                                                       
         BE    FSIL20              LOOK-UP FSI                                  
         B     FSILX               ALREADY HAVE FSI DATA                        
         DROP  R5                                                               
*                                                                               
FSIL5    OC    PBDJOB,PBDJOB       JOB CODE PRESENT IN NEWREC?                  
         BZ    FSILX                                                            
         L     R6,AJOBIO                                                        
         USING PJOBRECD,R6                                                      
         CLI   PJOBFSI,C'Y'                                                     
         BNE   FSILX                                                            
         DROP  R6                                                               
*                                                                               
FSIL20   L     R2,TRADDR                                                        
         CLI   8(R2),C'C'          CHANGE?                                      
         BNE   FSIL30              NO                                           
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'82'                                                     
         BRAS  R9,NEXTEL           LOOKING FOR FSI ELEM IN REC                  
         BE    FSILX               FOUND IT                                     
*                                                                               
FSIL30   ZAP   DUB(6),=P'0'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBUYREC                                                   
         MVI   KEY+3,X'27'                                                      
         MVC   KEY+4(6),PBUYKPUB                                                
         MVC   KEY+10(3),PBUYKCLT                                               
*                                                                               
FSIL50   BRAS  RE,PRT_RDHI                                                      
*                                                                               
         CLC   KEY(8),KEYSAVE      BASE PUB NUMBER                              
         BNE   FSIL100                                                          
         CLC   KEY(10),KEYSAVE     PUB + ZONE/EDT                               
         BE    FSIL60                                                           
         CLI   KEYSAVE+8,X'FF'     LOOKING FOR "ALL ZONE/EDT" REC?              
         BE    FSIL100                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(8),KEYSAVE                                                   
         MVC   KEY+10(3),PBUYKCLT                                               
         MVC   KEY+8(2),=X'FFFF'                                                
         B     FSIL50                                                           
*                                                                               
FSIL60   CLC   KEY+10(3),PBUYKCLT  FOUND TARGET CLIENT?                         
         BE    FSIL80                                                           
*                                                                               
FSIL65   CLC   KEY+10(3),=X'FFFFFF'                                             
         BE    FSIL80                                                           
         CLC   KEYSAVE+10(3),=X'FFFFFF'                                         
         BE    FSIL100                                                          
         XC    KEY,KEY                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+10(3),=X'FFFFFF'                                             
         BRAS  RE,PRT_RDHI                                                      
         B     FSIL65                                                           
*                                                                               
FSIL80   MVC   AREC,AWRKREC                                                     
         BRAS  RE,PRT_GETR                                                      
         L     R5,AREC                                                          
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'01'                                                     
FSIL85   CLI   0(R5),0                                                          
         BE    FSIL100                                                          
         CLI   0(R5),X'01'                                                      
         BE    FSIL95                                                           
FSIL90   BRAS  R9,NEXTEL                                                        
         BNE   FSIL100                                                          
         USING PFSIEL01,R5                                                      
FSIL95   MVC   FULL(3),PFSIDATE                                                 
         XC    FULL(3),=X'FFFFFF'  UNCOMPLEMENT FSI DATE                        
         CLC   PBUYKDAT,FULL                                                    
         BL    FSIL90              KEEP LOOKING                                 
         ZAP   DUB(6),PFSINUM      USE THIS NUMBER                              
         DROP  R5                                                               
*                                                                               
FSIL100  CP    DUB(6),=P'0'        FSI DATA NOT FOUND                           
         BNE   FSIL105                                                          
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         LA    R3,FSIERR           FSI DATA NOT FOUND                           
         L     R2,TRADDR                                                        
         J     ERROR                                                            
*                                                                               
FSIL105  LA    R5,NEWREC+33                                                     
         MVI   ELCODE,X'82'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   FSIL120                                                          
         USING PBFSIELD,R5                                                      
         MVC   PBFSI,DUB+1         PL5                                          
         B     FSILX                                                            
         DROP  R5                                                               
*                                                                               
FSIL120  XC    WORK(20),WORK       MUST ADD FSI ELEM TO NEWREC                  
         LA    R2,WORK                                                          
         USING PBFSIELD,R2                                                      
         MVC   WORK(2),=X'820D'                                                 
         MVC   PBFSI,DUB+1         PL5                                          
         MVI   PBFSIIND,X'01'      LOOKED-UP                                    
         MVC   PBFSILDT,BTODAY                                                  
         GOTO1 VRECUP,DMCB,(1,NEWREC),WORK,(R5)                                 
         DROP  R2                                                               
*                                                                               
FSILX    LA    R0,REC                                                           
         ST    R0,AREC                                                          
         J     X_XIT1                                                           
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDJ      NTR1  BASE=*,LABEL=*      EDIT AND PROCESS JOB RECORD                  
*                                                                               
         TM    SVPRDSTA,X'20'      NO TRAFFIC INSERTION?                        
         BZ    *+12                                                             
         LA    R3,159              CAN'T HAVE ADCODE                            
         J     ERROR                                                            
*                                                                               
         CLI   WORK,X'FF'          CALLED FROM OPTIONS (AD-ID=) ?               
         BE    *+8                 YES - SKIP "MOVE" PROC                       
         BRAS  RE,MOVE                                                          
*                                  BUILD JOB KEY                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         CLC   WORK(3),=C'*AD'     AD-ID ONLY ?                                 
         BNE   *+10                NO                                           
         MVC   WORK(6),ADIDJOB     REPLACE "MOVE" DATA WITH PBDJOB FLD          
         MVC   KEY+10(6),WORK      SAVED IN PPBUY05 FMTJOB (RECALL)             
*                                                                               
         CLC   SVJOB,WORK                                                       
         BE    EDJ6                                                             
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R3,NFNDERR                                                       
         J     ERROR                                                            
*                                                                               
         MVC   SVJOB,WORK                                                       
         MVC   SVJOBDA,KEY+27      SAVE DA                                      
         CLI   SVJOB,C'!'          MASTER AD CODE?                              
         BE    EDJ6                                                             
         MVI   KEY+16,X'FF'                                                     
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(16),KEYSAVE                                                  
         BE    EDJ6                                                             
         XC    SVJOB(10),SVJOB     CLEAR SVJOB AND SVJOBDA                      
         LA    R3,JOBERR2          NO INSTRUCTION RECORD                        
         J     ERROR                                                            
*                                                                               
EDJ6     DS    0H                                                               
         L     R6,AJOBIO                                                        
         USING PJOBRECD,R6                                                      
*                                                                               
         MVC   PBDJOB,SVJOB                                                     
         CLC   KEY(16),PJOBKEY                                                  
         BE    EDJ6D               ALREADY HAVE RECORD                          
         MVC   KEY+27(4),SVJOBDA                                                
         MVC   AREC,AJOBIO         READ INTO JOBIO                              
         BRAS  RE,PRT_GETR                                                      
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
EDJ6D    CLI   SVJOB,C'!'          MASTER AD CODE?                              
         BNE   EDJ6H                                                            
         L     R5,AJOBIO                                                        
         LA    R5,(PJOBELEM-PJOBREC)(R5)                                        
         MVI   ELCODE,PJSUBIDQ     SUB AD CODE ELEM CODE                        
         BRAS  R9,NEXTEL                                                        
         BE    EDJ6H                                                            
         LHI   R3,SUBADMIS         MISSING SUB AD CODE OR SUB AD ID             
         J     ERROR                                                            
*                                                                               
* CHECK JOB DATES VS INS DATE                                                   
*                                                                               
EDJ6H    CLC   NEWREC+16(3),PJOBSTA                                             
         BL    EDJ6M                                                            
         OC    PJOBEND,PJOBEND                                                  
         BZ    EDJ7                                                             
         CLC   NEWREC+16(3),PJOBEND                                             
         BNH   EDJ7                                                             
*                                                                               
EDJ6M    DS    0H                                                               
         LA    R3,JOBERR3                                                       
         J     ERROR                                                            
*                                                                               
EDJ7     DS    0H                  CHECK FOR BUY PUB IN PUBLIST RECORD          
         CLI   PJOBPLIS,C' '       PUB LIST IN JOB RECORD ?                     
         BNH   EDJ7K               NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),PJOBKEY      AGY/MED/RC/CLT                               
         MVI   KEY+3,X'17'         PUB LIST RECORD                              
         MVC   KEY+7(3),PJOBPLIS   LIST CODE                                    
         MVI   KEY+10,X'01'        "LINE' NUMBER                                
EDJ7B    BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(10),KEYSAVE                                                  
         BE    EDJ7D               GET THE LIST RECORD                          
         LA    R3,JOBERR5          PUB LIST RECORD NOT FOUND                    
         CLC   KEYSAVE+4(3),=C'ZZZ'                                             
         BNE   EDJ7C                                                            
*                                                                               
         XC    SVJOB(10),SVJOB     CLEAR SVJOB AND SVJOBDA                      
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(49),=C'** INVALID - NO RECORD FOUND FOR PUB LIST=X        
                *** **'                                                         
         MVC   BUYMSG+43(3),PJOBPLIS                                            
         FOUT  BUYMSGH                                                          
         MVI   ERRAREA,C'M'        FAKE ERR                                     
         J     EXIT                RECORD NOT FOUND                             
*                                                                               
EDJ7C    MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),=C'ZZZ'                                                 
         B     EDJ7B               TRY FOR "ZZZ" CLIENT CODE LIST REC           
*                                                                               
EDJ7D    DS    0H                  GET THE PUB LIST RECORD                      
         MVC   AREC,AWRKREC        READ INTO WRKREC                             
         BRAS  RE,PRT_GETR                                                      
         LA    RF,REC              RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
         L     R5,AWRKREC          PUB LIST RECORD IN WRKREC                    
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'20'                                                     
         CLI   0(R5),X'20'         LIST PUB ELEMENT ?                           
         BE    EDJ7F               YES - TEST FOR PUB                           
EDJ7E    BRAS  R9,NEXTEL                                                        
         BNE   EDJ7H               LOOK FOR ANOTHER RECORD                      
EDJ7F    CLC   NEWREC+10(6),2(R5)  BUY PUB = LIST PUB ?                         
         BE    EDJ7K               YES - OK - CONTINUE                          
         B     EDJ7E               LOOK FOR ANOTHER LIST PUB ELEMENT            
*                                                                               
EDJ7H    BRAS  RE,PRT_RSEQ                                                      
         CLC   KEY(10),KEYSAVE     SAME AGY/MED/RC/CLT/LIST ?                   
         BE    EDJ7D               YES - GET THE RECORD                         
*                                  NO - ERR                                     
         XC    SVJOB(10),SVJOB     CLEAR SVJOB AND SVJOBDA                      
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(48),=C'** INVALID - PUBLICATION NOT IN PUB LIST= X        
               *** **'                                                          
         MVC   BUYMSG+42(3),KEYSAVE+7                                           
         FOUT  BUYMSGH                                                          
         MVI   ERRAREA,C'M'        FAKE ERR                                     
         J     EXIT                                                             
*                                                                               
EDJ7K    DS    0H                                                               
*                                                                               
         CLI   WORK+6,OPT_JOBQ     CALLED FROM OPTIONS (AD-ID ONLY)?            
         BNE   EDJ7N               NO                                           
         XC    WORK,WORK                                                        
         J     EXIT                YES - DONE                                   
*                                                                               
EDJ7N    DS    0H                                                               
*                                                                               
         ST    R2,SAVR2                                                         
         BRAS  RE,BUMPFLD                                                       
         CLI   SVSCRN,X'FE'        TEST WSJ SCREEN                              
         BE    EDJ20                                                            
*                                                                               
         CLI   5(R2),0             TEST INPUT                                   
         BNE   EDJ8D                                                            
         CLI   BUYMD,C'N'                                                       
         BE    EDJ8                                                             
         CLI   BUYMD,C'O'          OUTDOOR - SKIP TO ALLOCATIONS                
         BE    EDJ9                                                             
*                                  MAG SPACE                                    
         MVC   8(17,R2),PJOBSPC                                                 
         MVI   5(R2),17                                                         
         FOUT  (R2)                                                             
         B     EDJ9                                                             
*                                  NEW LINES/COLS                               
EDJ8     DS    0H                                                               
         XC    X(10),X                                                          
         CLI   PJOBSPC,C' '        TEST HAVE SPACE DESC                         
         BNH   EDJ8A                                                            
*                                                                               
         MVC   X(8),PJOBSPC        YES - USE IT AND IGNORE LINES/CLMS           
         LA    R5,8                CAN NOW USE 8 CHARS                          
         LA    R1,X+7                                                           
EDJ80    CLI   0(R1),C' '          SCAN BACKWARDS FOR NON SPACE                 
         BH    EDJ8C               SO INPUT LENGHT WILL                         
         MVI   0(R1),0             CHG SPACE TO 0                               
         BCTR  R5,0                BE SET PROPERLY                              
         BCTR  R1,0                                                             
         B     EDJ80                                                            
*                                                                               
EDJ8A    DS    0H                                                               
         LA    R5,X                                                             
         ZAP   DUB,PJOBTUNS                                                     
         BRAS  RE,EDALINX                                                       
*                                                                               
         AR    R5,R0                                                            
         CLI   PJOBUIND,X'89'      LOWER CASE  I                                
         BE    *+12                                                             
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         CP    PJOBCOLS,=P'0'                                                   
         BE    EDJ8B                                                            
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         ZAP   DUB,PJOBCOLS                                                     
         EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
*                                                                               
EDJ8B    DS    0H                                                               
         LA    R0,X                                                             
         SR    R5,R0                                                            
*                                                                               
* IF LENGTH EXCEEDES 8 -  JUST PUT OUT TOTAL INCHES/LINES                       
*                                                                               
         CHI   R5,9                                                             
         BL    EDJ8C                                                            
         XC    X(9),X                                                           
         LA    R5,X                                                             
         ZAP   DUB,PJOBTUNS                                                     
         BRAS  RE,EDALINX                                                       
*                                                                               
         AR    R5,R0                                                            
         CLI   PJOBUIND,X'89'      LOWER CASE  I                                
         BE    *+12                                                             
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         LA    R0,X                                                             
         SR    R5,R0                                                            
*                                                                               
EDJ8C    MVC   8(8,R2),X                                                        
         FOUT  (R2)                                                             
         STC   R5,5(R2)            LENGTH                                       
*                                                                               
EDJ8D    DS    0H                  PREMIUM                                      
         CLI   BUYMD,C'N'                                                       
         BNE   EDJ9                                                             
         BRAS  RE,BUMPFLD2                                                      
         CLI   5(R2),0                                                          
         BNE   EDJ9                                                             
         CLI   PJOBPRM,C' '                                                     
         BNH   EDJ9                                                             
         MVC   8(3,R2),PJOBPRM                                                  
         FOUT  (R2)                                                             
         MVI   5(R2),2                                                          
*                                                                               
EDJ9     DS    0H                  ALLOCATIONS                                  
         CLC   BUYPR,=C'ZZZ'                                                    
         BNE   EDJ10                                                            
*                                                                               
* POINT TO ALLOC FIELD                                                          
*                                                                               
         LA    R5,13               13 BEYOND JOB                                
         CLI   BUYMD,C'O'                                                       
         BE    EDJ9B                                                            
         LA    R5,11               11 FLDS BEYOND SPACE FOR INTERACTIVE         
         CLI   BUYMD,C'I'                                                       
         BE    EDJ9A                                                            
         CLI   BUYMD,C'L'          TREAT SOCIAL SAME AS INTERACTIVE             
         BE    EDJ9A                                                            
         CLI   BUYMD,C'B'          TREAT MOBILE SAME AS INTERACTIVE             
         BE    EDJ9A                                                            
         CLI   BUYMD,C'D'          TREAT DIGITAL AUDIO SAME AS INTERAC          
         BE    EDJ9A                                                            
         CLI   BUYMD,C'V'          TREAT NVIDEO SAME AS INTERACTIVE             
         BE    EDJ9A                                                            
         CLI   BUYMD,C'W'          TREAT LVIDEO SAME AS INTERACTIVE             
         BE    EDJ9A                                                            
         LA    R5,12               12 FLDS BEYOND SPACE FOR MAGS                
         CLI   BUYMD,C'N'                                                       
         BNE   EDJ9A                                                            
*                                                                               
* MUST BE NEWSPAPERS                                                            
*                                                                               
         LA    R5,9                9 FOR PREMIUM (EXCEPT FOR LIST BUY)          
EDJ9A    CLI   SVSCRN,X'F9'                                                     
         BH    EDJ9B                                                            
         LA    R5,4                4 FOR LIST BUYING                            
         CLI   BUYMD,C'N'                                                       
         BE    EDJ9B                                                            
         LA    R5,7                                                             
         CLI   BUYMD,C'I'          7 FOR INTERACTV LIST BUYING                  
         BE    EDJ9B                                                            
         CLI   BUYMD,C'L'          TREAT SOCIAL SAME AS INTERACTIVE             
         BE    EDJ9B                                                            
         CLI   BUYMD,C'B'          TREAT MOBILE SAME AS INTERACTIVE             
         BE    EDJ9B                                                            
         CLI   BUYMD,C'D'          TREAT DIGITAL AUDIO SAME AS INTERAC          
         BE    EDJ9B                                                            
         CLI   BUYMD,C'V'          TREAT NVIDEO SAME AS INTERACTIVE             
         BE    EDJ9B                                                            
         CLI   BUYMD,C'W'          TREAT LVIDEO SAME AS INTERACTIVE             
         BE    EDJ9B                                                            
*                                                                               
         LA    R5,9                9 FOR OTHER MEDIA (EXCEPT OUTDOOR)           
*                                                                               
* NO LIST BUYING FOR OUTDOOR  (YET)                                             
*                                                                               
EDJ9B    BRAS  RE,BUMPFLD                                                       
         BCT   R5,*-4                                                           
         CLI   5(R2),0                                                          
         BNE   EDJ10                                                            
         CLI   PJOBALO,C' '                                                     
         BNH   EDJ10                                                            
         MVC   8(47,R2),PJOBALO                                                 
         FOUT  (R2)                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)            LENGTH                                       
*                                                                               
EDJ10    DS    0H                                                               
         L     R2,SAVR2            RESTORE TO JOB FIELD                         
         J     X_XIT1                                                           
*                                                                               
EDJ20    CLC   BUYPR,=C'ZZZ'                                                    
         BNE   EDJ25                                                            
         LHI   RF,6                                                             
         BRAS  RE,BUMPFLDS                                                      
         CLI   5(R2),0                                                          
         BNE   EDJ25                                                            
         CLI   PJOBALO,C' '                                                     
         BNH   EDJ25                                                            
         MVC   8(47,R2),PJOBALO                                                 
         FOUT  (R2)                                                             
         LA    R5,47(R2)                                                        
         CLI   7(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         SR    R5,R2                                                            
         STC   R5,5(R2)            LENGTH                                       
*                                                                               
EDJ25    LA    R2,BUYTR1H                                                       
         LHI   RF,11                                                            
         BRAS  RE,BUMPFLDS         GET TO FIRST EDITION                         
         LA    R9,8                FOR BCT                                      
EDJ26    OC    8(3,R2),8(R2)       CHK IF USED                                  
         BZ    EDJ30               NO - MUST BE DONE                            
         BRAS  RE,BUMPFLD                                                       
         CLI   5(R2),0             SEE IF INPUT                                 
         BNE   EDT26X                                                           
         XC    X(10),X                                                          
         CLI   PJOBSPC,C' '        TEST HAVE SPACE DESC                         
         BNH   EDJ26A                                                           
*                                                                               
         MVC   X(8),PJOBSPC        YES - USE IT AND IGNORE LINES/CLMS           
         LA    R5,8                CAN NOW USE 8 CHARS                          
         LA    R1,X+7                                                           
EDJ260   CLI   0(R1),C' '          SCAN BACKWARDS FOR NON SPACE                 
         BH    EDJ26C              SO INPUT LENGHT WILL                         
         MVI   0(R1),0             CHG SPACE TO 0                               
         BCTR  R5,0                BE SET PROPERLY                              
         BCTR  R1,0                                                             
         B     EDJ260                                                           
*                                                                               
EDJ26A   DS    0H                                                               
         LA    R5,X                                                             
         ZAP   DUB,PJOBTUNS                                                     
         BRAS  RE,EDALINX                                                       
*                                                                               
         AR    R5,R0                                                            
         CLI   PJOBUIND,X'89'      LOWER CASE  I                                
         BE    *+12                                                             
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         CP    PJOBCOLS,=P'0'                                                   
         BE    EDJ26B                                                           
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         ZAP   DUB,PJOBCOLS                                                     
         EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
EDJ26B   DS    0H                                                               
         LA    R0,X                                                             
         SR    R5,R0                                                            
*                                                                               
* IF LENGTH EXCEEDES 8 -  JUST PUT OUT TOTAL INCHES/LINES                       
*                                                                               
         CHI   R5,9                                                             
         BL    EDJ26C                                                           
         XC    X(9),X                                                           
         LA    R5,X                                                             
         ZAP   DUB,PJOBTUNS                                                     
         BRAS  RE,EDALINX                                                       
*                                                                               
         AR    R5,R0                                                            
         CLI   PJOBUIND,X'89'      LOWER CASE  I                                
         BE    *+12                                                             
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         LA    R0,X                                                             
         SR    R5,R0                                                            
*                                                                               
EDJ26C   MVC   8(8,R2),X                                                        
         FOUT  (R2)                                                             
         STC   R5,5(R2)            LENGTH                                       
*                                  NOW DO PREMIUMS                              
EDT26X   BRAS  RE,BUMPFLD                                                       
         CLI   5(R2),0             CK FOR INPUT                                 
         BNE   EDJ27                                                            
         CLI   PJOBPRM,C' '                                                     
         BNH   EDJ27                                                            
         MVC   8(3,R2),PJOBPRM                                                  
         FOUT  (R2)                                                             
         MVI   5(R2),2                                                          
*                                                                               
EDJ27    BRAS  RE,BUMPFLD2                                                      
         BCT   R9,EDJ26                                                         
*                                                                               
EDJ30    B     EDJ10                                                            
*                                                                               
EDALINX  CLI   PJOBUIND,X'89'      LOWER CASE I                                 
         BNE   EDALZ                                                            
         EDIT  (P8,DUB),(6,0(R5)),2,ALIGN=LEFT                                  
         BR    RE                                                               
*                                                                               
EDALZ    DS    0H                                                               
         EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    RE                                                               
*                                                                               
                                                                                
*                                                                               
         DROP  R6,RB                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK IF NEWREC HAS OTHER ELEMS NEED TO BE MOVED TO REC (ADBUYER)             
*                                                                               
* REGULAR COMMENTS   - X'66'      MAX OF 5                                      
* I/O COMMENTS       - X'67'      MAX OF 5                                      
* POSITION COMMENTS  - X'68'      MAX OF 5                                      
* SRC COMMENTS       - X'6A'      MAX OF 5                                      
* TEARSHEET          - X'95'      ONLY 1                                        
* TEARSHEET COMMENTS - X'69'      MAX OF 4                                      
* INTERNET CONTRACT  - X'97'      ONLY 1                                        
* INTERNET SITE LOCA - X'98'      ONLY 1      (FOR MEDIA I ONLY)                
* ADDITIONAL CHARGES - X'44'      MAX OF 10                                     
* INV MATCHING STATS - PBMATELQ   MAX OF 1                                      
*                                                                               
* NOTE: ELEMS LISTED BELOW DOES NOT HAVE SAME LOGIC AS ELEMS LISTED             
*       ABOVE. ELEMS BELOW DOES NOT REQUIRE REMOVAL OF ELEMS IN REC             
*                                                                               
* CUSTOM COLUMN      - BYCCIDQ    NO MAX                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADBELM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DDLINKSW,C' '       ADBUYER UPLOAD?                              
         BNH   MVELM80                                                          
*                                                                               
* MAKE SURE ALL STATUSES IN PBDSTAT ARE TRANSFERRED TO REC                      
*                                                                               
MVELM10  OC    REC+33+PBDSTAT-PBDELEM(1),NEWREC+33+PBDSTAT-PBDELEM              
*                                                                               
         TM    ABCBITSW,MATSTACQ   MATCH STAT BIT CHG'D?                        
         BZ    MVELM10F                                                         
         TM    NEWREC+33+PBDSTAT-PBDELEM,X'40'                                  
         BNZ   MVELM10F                                                         
         NI    REC+33+PBDSTAT-PBDELEM,X'FF'-X'40'                               
MVELM10F TM    ABCBITSW,TSRSTACQ   TS RECEIVED STAT BIT CHG'D?                  
         BZ    MVELM10M                                                         
         TM    NEWREC+33+PBDSTAT-PBDELEM,X'10'                                  
         BNZ   MVELM10M                                                         
         NI    REC+33+PBDSTAT-PBDELEM,X'FF'-X'10'                               
*                                                                               
MVELM10M CLI   BUYMD,C'N'          NEED TO RESET CLE FOR NEWSPAPER?             
         BNE   *+22                                                             
         MVC   REC+33+PBDUIND-PBDELEM(L'PBDUIND),PBDUIND                        
         MVC   REC+33+PBDUNITS-PBDELEM(L'PBDUNITS),PBDUNITS                     
         MVC   REC+33+PBDCLMS-PBDELEM(L'PBDCLMS),PBDCLMS                        
*                                                                               
         TM    ADBCHGSW,X66CHGD    X66 ELEM CHANGE DETECTED?                    
         BZ    *+16                                                             
         MVI   MVELBYTE,X'66'                                                   
         BRAS  RE,MVPROCOM         CK IF REGULAR COMMENTS CHANGED               
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         TM    ADBCHGSW,X67CHGD    X67 ELEM CHANGE DETECTED?                    
         BZ    *+16                                                             
         MVI   MVELBYTE,X'67'                                                   
         BRAS  RE,MVPROCOM         CK IF I/O COMMENTS CHANGED                   
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         TM    ADBCHGSW,X68CHGD    X68 ELEM CHANGE DETECTED?                    
         BZ    *+16                                                             
         MVI   MVELBYTE,X'68'                                                   
         BRAS  RE,MVPROCOM         CK IF POSITION COMMENTS CHANGED              
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         TM    ADBCHGSW,X6ACHGD    X6A ELEM CHANGE DETECTED?                    
         BZ    *+16                                                             
         MVI   MVELBYTE,X'6A'                                                   
         BRAS  RE,MVPROCOM         CK IF SRC COMMENTS CHANGED                   
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         MVI   MVELBYTE,X'95'      MOVE TEARSHEET ELEM                          
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         TM    ADBCHGSW,X69CHGD    X69 ELEM CHANGE DETECTED?                    
         BZ    *+12                                                             
         MVI   MVELBYTE,X'69'      MOVE TEARSHEET COMMENT ELEM                  
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         MVI   MVELBYTE,X'97'      MOVE INTERNET CONTRACT ELEM                  
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         MVI   MVELBYTE,X'98'      MOVE SITE LOCATION ELEM                      
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         MVI   MVELBYTE,X'A6'      MOVE ISSUE NAME ELEM                         
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         TM    ABCBITSW,ACHVERRQ   ANY ADDITIONAL CHARGE ERROR?                 
         BNZ   *+24                                                             
         TM    ABCBITSW,ACHCHGDQ   ADDITIONAL CHARGE CHANGED?                   
         BZ    *+16                                                             
         MVI   MVELBYTE,X'44'      MOVE ADDITIONAL CHARGE ELEM                  
         OI    CHGIND4,X'10'       ADDITIONAL CHARGE CHANGED                    
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         TM    ADBCHGSW,X52CHGD    INVOICE MATCHING STATUS IS CHANGED?          
         BZ    *+16                                                             
         MVI   MVELBYTE,PBMATELQ   MOVE INV MATCHING STATUS ELEM                
         BRAS  RE,MVPROCEL                                                      
         BRAS  RE,PRC_BYIH         PROCESS BUY INVOICE HISTORY ELEM             
*                                                                               
         MVI   MVELBYTE,BYCCIDQ    MOVE CUSTOM COLUMN ELEM                      
         BRAS  RE,MVPROCEL                                                      
*                                                                               
         MVI   MVELBYTE,X'91'      MOVE COS2 FACTOR ELEM                        
         BRAS  RE,MVPROCEL                                                      
*                                                                               
MVELM80  TM    GENBYSW1,C2FC2$MQ   COS2 FACTOR VALIDATED IN C2$ MODULE?         
         JZ    *+16                                                             
         MVI   MVELBYTE,X'91'      MOVE COS2 FACTOR ELEM                        
         OI    CHGIND4,X'40'       COS2 CHANGED                                 
         BRAS  RE,MVPROCEL                                                      
         TM    GLBVALSW,BUYCOS2Q   COS2 $ CHANGED?                              
         JZ    *+8                                                              
         OI    CHGIND4,X'40'       COS2 CHANGED                                 
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYDKELQ                                                  
         BRAS  R9,NEXTEL                                                        
         BNE   MVELM86                                                          
         USING PBYDKELM,R5                                                      
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   MVELM84                                                          
         MVC   PBYDKLDT,BTODAY                                                  
         CLI   DDLINKSW,C' '       ADBUYER UPLOAD?                              
         BH    *+12                                                             
         MVI   PBYDKST2,BYMFCHGQ   CHANGED VIA MF                               
         J     SETCCEQ                                                          
         MVI   PBYDKST2,BYABCHGQ   CHANGED VIA ADBUYER                          
         J     SETCCEQ                                                          
MVELM84  MVC   PBYDKDAT,IDSKDATE                                                
         MVC   PBYDKTIM,IDSKTIME                                                
         MVI   PBYDKST1,BYDKCHGQ   CHANGED VIA IDESK                            
         TM    PLINKSW1,PRSMORGQ   PRISMA ORIGIN?                               
         JZ    *+12                                                             
         OI    PBYDKST1,BYPMCHGQ                                                
         NI    PBYDKST1,X'FF'-BYDKCHGQ                                          
         TM    PLINKSW1,RADAORGQ   RADIA ORIGIN?                                
         JZ    *+12                                                             
         OI    PBYDKST3,BYRACHGQ                                                
         NI    PBYDKST1,X'FF'-BYDKCHGQ                                          
         TM    PRSMSTA1,BYPRMIVQ   PRISMA INVOICE ENABLED CAMPAIGN?             
         JZ    *+8                                                              
         OI    PBYDKST4,BYPRMIVQ                                                
         J     SETCCEQ                                                          
         DROP  R5                                                               
*                                                                               
MVELM86  CLI   DDLINKSW,C' '       ADBUYER UPLOAD?                              
         JNH   SETCCNEQ                                                         
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JZ    SETCCEQ                                                          
         XC    WKELEM,WKELEM                                                    
         LA    RE,WKELEM                                                        
         USING PBYDKELM,RE                                                      
         MVI   PBYDKELC,PBYDKELQ   IDESK INSERTION UPLOAD ELEM CODE             
         MVI   PBYDKELL,PBYDKLNQ                                                
         MVC   PBYDKDAT,IDSKDATE                                                
         MVC   PBYDKTIM,IDSKTIME                                                
         OI    PBYDKST1,BYDKADDQ+BYDKCHGQ                                       
         TM    PLINKSW1,PRSMORGQ   PRISMA ORIGIN?                               
         JZ    *+12                                                             
         OI    PBYDKST1,BYPMADDQ+BYPMCHGQ                                       
         NI    PBYDKST1,X'FF'-(BYDKADDQ+BYDKCHGQ)                               
         TM    PLINKSW1,RADAORGQ   RADIA ORIGIN?                                
         JZ    *+12                                                             
         OI    PBYDKST3,BYRAADDQ+BYRACHGQ                                       
         NI    PBYDKST1,X'FF'-(BYDKADDQ+BYDKCHGQ)                               
         TM    PRSMSTA1,BYPRMIVQ   PRISMA INVOICE ENABLED CAMPAIGN?             
         JZ    *+8                                                              
         OI    PBYDKST4,BYPRMIVQ                                                
         GOTO1 VRECUP,DMCB,(1,REC),WKELEM,(R5)                                  
         DROP  RE                                                               
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
MVELMREM ST    RE,MVELSVRE         REMOVING EXISTING ELEM FROM REC              
MVEREM20 LA    R5,REC+33                                                        
MVEREM22 BRAS  R9,NEXTEL                                                        
         BNE   MVEREM80                                                         
         CLI   0(R5),BYCCIDQ       CUSTOM COLUMN                                
         BNE   MVEREM30                                                         
         LH    RE,=Y(CCREMTAB-GENOLD)                                           
         AR    RE,RC                                                            
         XC    MVELHALF,MVELHALF                                                
         OC    0(2*CCREMMXQ,RE),0(RE)                                           
         BZ    MVEREM80                                                         
         USING BYCCELD,R5                                                       
MVEREM24 CLC   BYCCSQN,0(RE)                                                    
         BE    MVEREM30                                                         
         LH    RF,MVELHALF                                                      
         CHI   RF,CCREMMXQ                                                      
         BH    MVEREM22                                                         
         AHI   RF,1                                                             
         STH   RF,MVELHALF                                                      
         LA    RE,2(RE)            NEXT ENTRY IN TABLE                          
         B     MVEREM24                                                         
         DROP  R5                                                               
MVEREM30 GOTO1 VRECUP,DMCB,(1,REC),(R5),0                                       
         B     MVEREM20                                                         
MVEREM80 L     RE,MVELSVRE                                                      
         BR    RE                                                               
*                                                                               
MVELMADD ST    RE,MVELSVRE         ADDING NEW ELEM TO REC                       
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'FF'                                                     
         BRAS  R9,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(1,REC),(R6),(R5)                                    
         L     RE,MVELSVRE                                                      
         BR    RE                                                               
*                                                                               
MVPROCEL ST    RE,MVTOP_RE         SAVE RETURN ADDRESS                          
         MVC   ELCODE,MVELBYTE                                                  
         LA    RE,MVELMTB1         POINT TO ELEM CODE TABLE 1                   
MVPELM12 CLI   0(RE),0             END OF TABLE?                                
         BE    MVPELM25                                                         
         CLC   ELCODE,0(RE)        MATCH THAT IN TABLE?                         
         BE    MVPELM14            YES, REMOVE ELEMS SET IN TABLE 1             
         LA    RE,1(RE)            POINT TO NEXT ELEM CODE IN TABLE             
         B     MVPELM12                                                         
MVPELM14 BRAS  RE,MVELMREM         REMOVE ELEM ACCORDING TO TABLES              
MVPELM25 LA    R5,NEWREC+33                                                     
         BRAS  R9,NEXTEL           ELEM TO BE MOVED FOUND?                      
         BNE   MVPELM50                                                         
         ST    R5,MVELFULL         ADDRESS OF ELEM TO BE ADDED                  
         BRAS  RE,MVELMREM         REMOVE ALL EXISTING ELEM                     
         L     R6,MVELFULL         ADDRESS OF ELEM TO BE ADDED                  
         BRAS  RE,MVELMADD         NOW ADD NEWREC ELEM TO REC                   
MVPELM40 L     R5,MVELFULL                                                      
         MVC   ELCODE,MVELBYTE                                                  
         BRAS  R9,NEXTEL           MORE NEWREC ELEM TO BE MOVED?                
         BNE   MVPELMX                                                          
         ST    R5,MVELFULL         ADDRESS OF NEWREC ELEM TO BE ADDED           
         LR    R6,R5                                                            
         BRAS  RE,MVELMADD         NOW ADD NEWREC ELEM TO REC                   
         B     MVPELM40            GO BACK FOR MORE TO BE ADDED                 
MVPELM50 CLI   MVELBYTE,BYCCIDQ                                                 
         BNE   MVPELMX                                                          
         BRAS  RE,MVELMREM         CK CC ELEMS NEED TO BE REMOVED               
MVPELMX  L     RE,MVTOP_RE                                                      
         BR    RE                                                               
*                                                                               
MVPROCOM ST    RE,MVTOP_RE         SAVE RETURN ADDRESS                          
         XC    MVELCNT1,MVELCNT1                                                
         XC    MVELCNT2,MVELCNT2                                                
         XC    MVELFULL,MVELFULL                                                
         SR    RF,RF                                                            
         LA    R5,NEWREC+33                                                     
         MVC   ELCODE,MVELBYTE     X'66', X'67',X'68',X'6A' COMMENT             
MVPCOM10 BRAS  R9,NEXTEL                                                        
         BNE   *+12                                                             
         AHI   RF,1                                                             
         B     MVPCOM10                                                         
         STH   RF,MVELCNT1         NUMBER OF ELEM FOUND IN NEWREC               
         SR    RF,RF                                                            
         LA    R5,REC+33                                                        
         MVC   ELCODE,MVELBYTE     X'66', X'67',X'68',X'6A' COMMENT             
MVPCOM20 BRAS  R9,NEXTEL                                                        
         BNE   *+12                                                             
         AHI   RF,1                                                             
         B     MVPCOM20                                                         
         STH   RF,MVELCNT2         NUMBER OF ELEM FOUND IN REC                  
         CLC   MVELCNT1,MVELCNT2                                                
         BE    MVPCOM50            BE = SAME NUMBER OF ELEM IN BOTH             
MVPCOM30 CLI   MVELBYTE,X'66'                                                   
         BNE   *+8                                                              
         OI    CHGIND1,X'02'       REGULAR COMMENT IS CHANGED                   
         CLI   MVELBYTE,X'67'                                                   
         BNE   *+8                                                              
         OI    CHGIND1,X'01'       I/O COMMENT IS CHANGED                       
         CLI   MVELBYTE,X'68'                                                   
         BNE   *+8                                                              
         OI    CHGIND3,X'01'       POSITION COMMENT IS CHANGED                  
         CLI   MVELBYTE,X'6A'                                                   
         BNE   *+8                                                              
         OI    CHGIND4,X'04'       SRC COMMENT IS CHANGED                       
         B     MVPCOMX                                                          
MVPCOM50 LH    RE,MVELCNT1                                                      
         CHI   RE,0                NO ELEM IS FOUND?                            
         BE    MVPCOMX                                                          
         SR    R4,R4                                                            
MVPCOM60 LH    RE,MVELCNT1                                                      
         CR    RE,R4               ALL ELEMS ARE CHECKED?                       
         BE    MVPCOMX                                                          
         LA    R5,REC+33                                                        
         MVC   ELCODE,MVELBYTE     X'66', X'67',X'68',X'6A' COMMENT             
         BRAS  R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                THERE MUST BE ONE!                           
         LR    R6,R5               SAVE ADDRESS OF ELEM FOUND IN REC            
         L     R5,MVELFULL                                                      
         CHI   R4,0                FIRST TIMES?                                 
         BH    *+8                                                              
         LA    R5,NEWREC+33                                                     
         BRAS  R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                THERE MUST BE ONE!                           
         ST    R5,MVELFULL                                                      
         AHI   R4,1                                                             
         ZIC   RE,1(R5)            LENGTH OF COMMENT ELEM IN NEWREC             
         ZIC   RF,1(R6)            LENGTH OF COMMENT ELEM IN REC                
         CR    RE,RF                                                            
         BNE   MVPCOM30            COMMENT LENGTHS DON'T MATCH, CHANGED         
         CHI   RE,2                                                             
         BH    *+6                                                              
         DC    H'0'                NO EMPTY COMMENT ELEM IS ALLOWED!            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R5),1(R6)                                                    
         BNE   MVPCOM30            COMMENTS DON'T MATCH, CHANGED                
         B     MVPCOM60            GO BACK FOR MORE                             
MVPCOMX  L     RE,MVTOP_RE                                                      
         BR    RE                                                               
*                                                                               
PRC_BYIH LR    R3,RE               PROCESS BUY INVOICE HISTORY ELEM             
         LA    R5,REC+33                                                        
         USING PBMATELM,R5                                                      
         MVI   ELCODE,PBMATELQ                                                  
         BRAS  R9,NEXTEL                                                        
         JNE   P_BYIH_X                                                         
         MVC   WKBYTE1,PBMTSTAT    SAVE STATUS FOR INVOICE HISTORY ELEM         
         DROP  R5                                                               
                                                                                
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBNVELQ                                                   
         BRAS  R9,NEXTEL           BUY LINKED TO ANY INVOICE ITEMS?             
         JE    P_BYIH20                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBMATELQ                                                  
         BRAS  R9,NEXTEL           INVOICE STATUS ELEM FOUND?                   
         JNE   P_BYIH_X                                                         
         USING PBMATELM,R5                                                      
         MVI   PBMTSTAT,PBMTSNIQ   FORCE STATUS TO NO INVOICE                   
         MVI   PBMTDSTA,PBMTDNAQ   FORCE STATUS TO NOT APPLICABLE               
         MVC   WKBYTE1,PBMTSTAT    SAVE STATUS FOR INVOICE HISTORY ELEM         
         DROP  R5                                                               
                                                                                
                                                                                
P_BYIH20 XC    WKTEMP1,WKTEMP1     BUILD BUY INVOICE HISTORY ELEM               
         LA    RE,WKTEMP1                                                       
         USING PBYIELEM,RE                                                      
         MVI   PBYIELCO,PBYIELCQ   ELEM CODE                                    
         MVI   PBYIELLN,PBYIELLQ   ELEM LENGTH                                  
         MVC   PBYIDATE,BTODAY     TODAY'S DATE                                 
         MVC   PBYI_PID,SVPID      PID                                          
         MVC   PBYISTAT,WKBYTE1    INVOICE MATCHING STATUS                      
         DROP  RE                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYIELCQ                                                  
P_BYIH40 BRAS  R9,NEXTEL                                                        
         JNE   P_BYIH50                                                         
         CLC   WKTEMP1(PBYIELLQ),0(R5)                                          
         JE    P_BYIH_X                                                         
         J     P_BYIH40                                                         
P_BYIH50 GOTO1 VRECUP,DMCB,(1,REC),WKTEMP1,(R5)                                 
P_BYIH_X LR    RE,R3                                                            
         BR    RE                                                               
*                                                                               
* TABLE OF ELEM CODES THAT REQUIRE REMOVAL OF ALL ELEMS FROM REC                
*                                                                               
MVELMTB1 DC    X'66'               REGULAR   COMMENT                            
         DC    X'67'               I/O       COMMENT                            
         DC    X'68'               POSITION  COMMENT                            
         DC    X'69'               TEARSHEET COMMENT                            
         DC    X'6A'               SRC       COMMENT                            
         DC    X'44'               ADDITIONAL CHARGES                           
         DC    AL1(PBMATELQ)       INVOICE MATCHING STATUS                      
         DC    X'00'                                                            
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BYCHGELM NTR1  BASE=*,LABEL=*      PROCESS BUY CHANGE ELEM                      
*                                                                               
         CLI   CHGIND1,0                                                        
         BNE   BYCHG20                                                          
         CLI   CHGIND2,0                                                        
         BNE   BYCHG20                                                          
         CLI   CHGIND3,0                                                        
         BNE   BYCHG20                                                          
         CLI   CHGIND4,0                                                        
         BNE   BYCHG20                                                          
         CLI   CHGIND5,0                                                        
         BE    BYCHG_X             NO 'REAL' CHANGES                            
*                                                                               
BYCHG20  MVI   WKBYTE1,0           INIT IDESK CONTROL CHECK SWITCH              
         TM    CHGIND1,X'08'       INSERTION DATE CHANGED?                      
         BZ    *+8                                                              
         MVI   WKBYTE1,C'Y'                                                     
         TM    CHGIND1,X'40'       RATE CHANGED?                                
         BZ    *+8                                                              
         MVI   WKBYTE1,C'Y'                                                     
         TM    CHGIND3,X'10'       PLANNED COST CHANGED?                        
         BZ    *+8                                                              
         MVI   WKBYTE1,C'Y'                                                     
         CLI   WKBYTE1,0           NEED TO CHECK FOR IDESK INSERTIONS?          
         BE    *+12                                                             
         BRAS  RE,CKIDKC           CK FOR IDESK CONTROL                         
         JNE   XIT_R2R3                                                         
*                                                                               
         MVI   DAYPIDSW,0          INIT SAME DAY SAME PID SWITCH                
*                                                                               
         GOTOR VDATCON,DMCB,(3,BTODAY),(2,DUB)                                  
         GOTOR VGETINS,DMCB,REC,PVALUES,REC+7,0,0,0                             
         XC    WKELEM(25),WKELEM                                                
         LA    R5,REC+33                                                        
         USING PCHGELEM,R5                                                      
         MVI   ELCODE,X'24'                                                     
         SR    R1,R1                                                            
         BRAS  R9,NEXTEL                                                        
         BNE   *+10                                                             
         LR    R1,R5               POINT TO CURRENT CHG ELEM                    
         B     *-10                                                             
*                                                                               
         LTR   R1,R1               HAVE LATEST CHG ELEM?                        
         BZ    BYCHG24                                                          
         LR    R5,R1               POINT TO PREVIOUS CHG ELEM                   
         CLC   PCHGDAT,DUB         TODAY'S CHG ELEM?                            
         BNE   BYCHG23D                                                         
         LA    RE,PCHG_XSS                                                      
         CLI   1(R5),PCHGNEWS      SHORT ELEM?                                  
         BE    BYCHG23B                                                         
         LA    RE,PCHG_XLS                                                      
         CLI   1(R5),PCHGNEWL      LONG ELEM?                                   
         BE    BYCHG23B                                                         
         B     BYCHG23D            OLDER CHG ELEM LENGTH                        
         DROP  R5                                                               
*                                                                               
         USING PCHGEXT,RE                                                       
BYCHG23B CLC   PCHGPID,SVPID       SAME PERSON ON SAME DAY?                     
         BNE   BYCHG23D                                                         
         MVI   DAYPIDSW,C'Y'       YES, IT'S SAME DAY SAME PID                  
         OC    CHGIND5,PCHGIND5    SAVE INDICATOR FOR LATER UPDATE              
         B     BYCHG23F                                                         
         DROP  RE                                                               
*                                                                               
BYCHG23D IC    R0,1(R5)            BY PASS CURRENT CHG ELEM                     
         AR    R5,R0                                                            
         B     BYCHG24                                                          
*                                                                               
BYCHG23F MVC   WKELEM(25),0(R5)    SAME PERSON ON SAME DAY - SAVE IT            
         GOTOR VRECUP,DMCB,(1,REC),(R5)                                         
*                                                                               
BYCHG24  LA    R3,WKELEM                                                        
         USING PCHGELD,R3                                                       
*                                                                               
         MVI   PCHGELEM,X'24'                                                   
         CLI   PCHGLEN,0                                                        
         BNE   *+8                                                              
         MVI   PCHGLEN,PCHGNEWS    CREATING OF NEW ELEM                         
*                                                                               
         MVC   PCHGDAT,DUB         TODAY'S DATE - PACKED                        
         OC    PCHGIND1(3),CHGIND1 SET CHG INDICATOR 1, 2 AND 3                 
         OC    PCHGIND4,CHGIND4    SET CHG INDICATOR 4                          
*                                                                               
         TM    PCHGIND1,X'40'      RATE CHANGED?                                
         BZ    BYCHG24H                                                         
         CLI   T411FFD+12,X'FF'    NO HEX SECURITY FOR BUY?                     
         BE    BYCHG24H                                                         
         TM    T411FFD+12,X'01'    DISPLAY ONLY?                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    T411FFD+12,X'10'    ONLY ALLOWING INV STAT CHANGES?              
         BZ    *+6                                                              
         DC    H'0'                LIMIT ACCESS USERS CAN'T CHANGE RATE         
*                                                                               
BYCHG24H OC    SVGROSS(12),SVGROSS NEED TO CK COST CHANGED?                     
         BZ    *+14                                                             
         CLC   SVGROSS(12),GROSS   COST CHANGED?                                
         BNE   BYCHG26                                                          
         CLI   PCHGLEN,PCHGNEWS    SHORT ELEM?                                  
         BNE   BYCHG27                                                          
         LA    RE,PCHG_XSS         START OF EXTENSION FOR SHORT ELEM            
         USING PCHGEXT,RE                                                       
         MVC   PCHGPID,SVPID                                                    
         OC    PCHGIND5,CHGIND5    SET CHG INDICATOR 5                          
         B     BYCHG70                                                          
         DROP  RE                                                               
*                                                                               
* NOTE: SAME DAY SAME PID RATE CHANGES WILL RECORD THE FIRST CHANGE             
*                                                                               
BYCHG26  MVI   PCHGLEN,PCHGNEWL    LONG CHG ELEM WITH COST                      
         CLI   DAYPIDSW,C'Y'       SAME DAY SAME PID?                           
         JE    *+10                                                             
         MVC   PCHGGRS(12),SVGROSS                                              
BYCHG27  MVC   PCHGPID,SVPID                                                    
         OC    PCHGIND5,CHGIND5    SET CHG INDICATOR 5                          
*                                                                               
BYCHG70  DS    0H                                                               
*******  NI    CHGIND5,X'FF'-(X'04'+X'02'+X'01')                                
*                                                                               
         GOTOR VRECUP,DMCB,(1,REC),WKELEM,(R5)                                  
*                                                                               
         CLI   MADSW,C'Y'                                                       
         BNE   BYCHG_X                                                          
         OC    PBUNBYDT,PBUNBYDT   INSERTION DATE CHANGED?                      
         BZ    BYCHG_X                                                          
         LA    R5,REC+33           UPDATE UPLOAD ELEMENT                        
         MVI   ELCODE,X'90'                                                     
         USING PIUPEL,R5                                                        
         BRAS  R9,NEXTEL                                                        
         BNE   BYCHG80                                                          
         GOTO1 VDATCON,DMCB,(3,PBUNBYDT),(20,PIUPUDAT) NEW DATE                 
*                                                                               
BYCHG80  BRAS  RE,UPDBLIN          BUYLINE ON MINIO REC NEEDS UPDATING          
         DROP  R5                                                               
*                                                                               
BYCHG_X  J     SETCCEQ                                                          
*                                                                               
                                                                                
*                                                                               
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO UPDATE DATE PASSIVE POINTERS                                    
*        HANDLES POINTER FOR                                                    
*              SPACE CLOSING DATE                                               
*              MATERIALS CLOSING DATE                                           
*              BILLABLE DATE                                                    
*              PAYABLE DATE                                                     
*                                                                               
*        DELETES POINTER FOR OLD DATE IN REC                                    
*        ADDS POINTER FOR NEW DATE IN NEWREC                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UPDPSV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LKDRFTSW,C'F'       IF DRAFT MODE (ADBUYER)?                     
         BE    UPDPSVX                SKIP UPDATING PASSIVES                    
         CLI   DDLINKSW,C'F'       IF DRAFT ADD (ADBUYER)?                      
         BE    UPDPSVX                SKIP UPDATING PASSIVES                    
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BNZ   UPPERR1                BUY LOCKED                                
*                                                                               
         LA    R2,UPDPSVTB         ESTABLISH TABLE OF DATES                     
         USING UPDPSVTB,R2                                                      
*                                                                               
*        SKIP IF TERMINAL IN NON-UPDATIVE MODE                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CXTRAINF-COMFACSD)(RF)                                       
         USING XTRAINFD,RF         ESTABLISH EXTRAINEOUS INFO                   
*                                                                               
         TM    XIFLAG1,XIROSYS+XIROMODE SKIP IF READ ONLY SYSTEM                
         BNZ   UPDPSVX                       OR READ ONLY MODE                  
*                                                                               
         OC    INSDA,INSDA         DIE IF NO DISK ADDRESS                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  RF                                                               
*                                                                               
UPPLOOP  DS    0H                                                               
*                                                                               
         CLI   UPPTPPID,X'FF'      DONE AT END OF TABLE                         
         BE    UPPDONE                                                          
*                                                                               
*        FIND DATE FOR PASSIVE IN OLD RECORD                                    
*                                                                               
         CLI   UPPTPPID,PBYPMCLQ   IF MATERIALS CLOSING DATE                    
         BNE   *+14                                                             
         MVC   UPPDTOLD,SVMDATE       USE SAVED MATERIALS CLOSE DATE            
         B     UPPDTE10                                                         
*                                                                               
         CLI   UPPTPPID,PBYPCLSQ   IF SPACE     CLOSING DATE                    
         BNE   *+14                                                             
         MVC   UPPDTOLD,SVCDATE       USE SAVED SPACE     CLOSE DATE            
         B     UPPDTE10                                                         
*                                                                               
         CLI   UPPTPPID,PBYPPAYQ   IF PAYABLE DATE                              
         BNE   *+14                                                             
         MVC   UPPDTOLD,SVPDATE       USE SAVED PAYABLE DATE                    
         B     UPPDTE10                                                         
*                                                                               
         CLI   UPPTPPID,PBYPBLBQ   IF BLBABLE DATE                              
         BNE   *+14                                                             
         MVC   UPPDTOLD,SVBDATE       USE SAVED BLBABLE DATE                    
         B     UPPDTE10                                                         
*                                                                               
UPPDTE10 DS    0H                                                               
*                                                                               
*        FIND DATE FOR PASSIVE IN NEW RECORD                                    
*                                                                               
         CLC   =C'DL',SVTRCODE     SKIP IF DELETE TRANSACTION                   
         BE    UPPDTE20                                                         
*                                                                               
         LA    R5,PBDELEM-PBUYKEY+NEWREC   1ST ELEM IN NEWREC                   
         CLI   SVTRCODE,C'C'                                                    
         BNE   *+8                                                              
         LA    R5,PBDELEM-PBUYKEY+REC      1ST ELEM IN REC                      
*                                                                               
         MVC   ELCODE,UPPTELID     SET ID OF NEEDED ELEMENT                     
*                                                                               
         CLC   ELCODE,0(R5)        SKIP IF ELEMENT FOUND                        
         BE    *+12                                                             
         BRAS  R9,NEXTEL           FIND ELEMENT                                 
         BNZ   UPPCONT             SKIP IF ELEMENT NOT FOUND                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,UPPTDISP         GET DISPLACEMENT OF DATE                     
         LA    R1,0(RF,R5)         POINT TO DATE FOR PASSIVE                    
         MVC   UPPDTNEW,0(R1)      SAVE NEW DATE                                
*                                                                               
UPPDTE20 DS    0H                                                               
*                                                                               
UPPOLD   DS    0H                                                               
*                                                                               
         CLC   =C'DL',SVTRCODE     SKIP IF DELETE TRANSACTION                   
         BE    *+14                                                             
         CLC   UPPDTOLD,UPPDTNEW   SKIP IF DATE IS UNCHANGED                    
         BE    UPPOLDX                                                          
*                                                                               
*        DELETE PASSIVE FOR OLD RECORD                                          
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH PASSIVE KEY                        
         USING PBYPPSVD,R4                                                      
*                                                                               
         MVC   PBYPAGY,PBUYKAGY-PBUYKEY+REC  SET AGENCY                         
         MVC   PBYPMED,PBUYKMED-PBUYKEY+REC  SET MEDIA                          
         MVI   PBYPRCD,PBYPRIDQ    SET BASIC RECORD ID                          
         MVC   PBYPDTYP,UPPTPPID   SET DATE TYPE ID                             
         MVC   PBYPCLT,PBUYKCLT-PBUYKEY+REC  SET CLIENT                         
         MVC   PBYPPRD,PBUYKPRD-PBUYKEY+REC  SET PRODUCT                        
         MVC   PBYPEST,PBUYKEST-PBUYKEY+REC  SET ESTIMATE                       
         MVC   PBYPDTE,UPPDTOLD    SET DATE                                     
         MVC   PBYPDADR,INSDA      SET DISK ADDRESS                             
*                                                                               
*        READ OLD PASSIVE                                                       
*                                                                               
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERR TESTS                   
         BRAS  RE,PRT_READ         READ OLD PASSIVE                             
*                                                                               
         TM    DMCB+8,X'10'        SKIP IF PASSIVE NOT FOUND                    
         BO    UPPOLDX                                                          
*                                                                               
         BRAS  RE,CHECK            CHECK FOR DATAMGR ERRORS                     
*                                                                               
         MVI   PBYPCNTL,X'FF'      TAG DELETED AND PURGEABLE                    
*                                                                               
         CLC   =C'DL',SVTRCODE     IF DELETE TRANSACTION                        
         BNE   *+10                                                             
         MVC   PBYPCNTL,REC+27        COPY CONTROL BYTES FROM RECORD            
*                                                                               
         BRAS  RE,PRT_WRIT         RE-WRITE PASSIVE                             
         BRAS  RE,CHECK            CHECK FOR DATAMGR ERRORS                     
*                                                                               
UPPOLDX  DS    0H                                                               
*                                                                               
*        CREATE PASSIVE FOR NEW RECORD                                          
*                                                                               
         CLC   =C'DL',SVTRCODE     SKIP IF DELETE TRANSACTION                   
         BE    UPPNEWX                                                          
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH PASSIVE KEY                        
         USING PBYPPSVD,R4                                                      
*                                                                               
         MVC   PBYPAGY,PBUYKAGY-PBUYKEY+NEWREC  SET AGENCY                      
         MVC   PBYPMED,PBUYKMED-PBUYKEY+NEWREC  SET MEDIA                       
         MVI   PBYPRCD,PBYPRIDQ    SET BASIC RECORD ID                          
         MVC   PBYPDTYP,UPPTPPID   SET DATE TYPE ID                             
         MVC   PBYPCLT,PBUYKCLT-PBUYKEY+NEWREC  SET CLIENT                      
         MVC   PBYPPRD,PBUYKPRD-PBUYKEY+NEWREC  SET PRODUCT                     
         MVC   PBYPEST,PBUYKEST-PBUYKEY+NEWREC  SET ESTIMATE                    
         MVC   PBYPDTE,UPPDTNEW    SET DATE                                     
         MVC   PBYPDADR,INSDA      SET DISK ADDRESS                             
*                                                                               
*        READ NEW PASSIVE                                                       
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERR TESTS                   
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         BRAS  RE,PRT_READ         READ FOR NEW PASSIVE                         
*                                                                               
         TM    DMCB+8,X'10'        SKIP IF PASSIVE NOT FOUND                    
         BO    UPPNEW10                                                         
*                                                                               
         BRAS  RE,CHECK            CHECK FOR DATAMGR ERRORS                     
*                                                                               
         CLC   PBYPCNTL,REC+27     SKIP IF CONTROL BYTES UNCHANGED              
         BE    UPPNEW05                                                         
*                                                                               
         MVC   PBYPCNTL,REC+27     SET CONTROL BYTES                            
*                                                                               
         BRAS  RE,PRT_WRIT         RE-WRITE PASSIVE                             
         BRAS  RE,CHECK            CHECK FOR DATAMGR ERRORS                     
*                                                                               
UPPNEW05 DS    0H                                                               
*                                                                               
         B     UPPNEWX                                                          
*                                                                               
UPPNEW10 DS    0H                  ADD NEW PASSIVE                              
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
*                                                                               
         MVC   PBYPCNTL,REC+27     SET CONTROL BYTES                            
         MVC   PBYPDISK,INSDA      SET DISK ADDRESS                             
*                                                                               
         BRAS  RE,PRT_ADD_         ADD NEW PASSIVE                              
         BRAS  RE,CHECK            CHECK FOR ERRORS                             
*                                                                               
UPPNEWX  DS    0H                                                               
*                                                                               
UPPCONT  DS    0H                                                               
         LA    R2,UPPTENTL(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     UPPLOOP                                                          
*                                                                               
UPPDONE  DS    0H                                                               
*                                                                               
UPDPSVX  NI    DMINBTS,X'F7'       RESET PASS DELETES                           
         MVI   DMOUTBTS,X'FD'      RESTORE                                      
         XIT1                                                                   
*                                                                               
UPPERR1  DS    0H                  BUY RECORD LOCKED                            
*                                                                               
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         B     UPDPSVER                                                         
*                                                                               
UPDPSVER DS    0H                                                               
         J     ERROR                                                            
*                                                                               
*        TABLE FOR PROCESSING PASSIVE POINTERS FOR DATES                        
*                                                                               
         DS    0D                  ALIGNMENT                                    
         DC    CL8'UPDPSVTB'                                                    
*                                                                               
UPDPSVTB DS    0X                  TABLE OF PASSIVES TO BE UPDATED              
UPPTPPID DC    AL1(PBYPMCLQ)       DATE ID - MATERIALS CLOSING                  
UPPTELID DC    XL1'20'             ELEMENT ID                                   
UPPTDISP DC    AL1(PBDMDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
UPPTENTL EQU   *-UPDPSVTB          LENGTH OF TABLE ENTRY                        
*                                                                               
         DC    AL1(PBYPCLSQ)       DATE ID - SPACE     CLOSING                  
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDCDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    AL1(PBYPBLBQ)       DATE ID - BILLABLE DATE                      
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDBDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    AL1(PBYPPAYQ)       DATE ID - PAYABLE DATE                       
         DC    XL1'20'             ELEMENT ID                                   
         DC    AL1(PBDPDATE-PBDELEM) DISPLACEMENT IN ELEMENT                    
         DS    XL1                 SPARE                                        
*                                                                               
         DC    X'FF'               END OF TABLE                                 
*                                                                               
                                                                                
*                                                                               
         DROP  RB,R2,R4                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DELINS   NTR1  BASE=*,LABEL=*      PROCESS DELETE INSERTION                     
*                                                                               
         XC    WORK(PPIDLEQ),WORK                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,PPIDELQ      PID ELEM CODE                                
         BRAS  R9,NEXTEL                                                        
         BNE   DELINS10                                                         
         USING PPIDELD,WORK                                                     
         MVC   PPIDELM(PPIDLEQ),0(R5)                                           
         GOTO1 VRECUP,DMCB,(1,REC),(R5),0     DELETE A7                         
*                                                                               
DELINS10 OC    WORK(4),WORK                                                     
         BNZ   DELINS20                                                         
         MVI   PPIDELM,PPIDELQ     PID ELEM CODE                                
         MVI   PPIDELL,PPIDLEQ     PID ELEM LENGTH                              
         XC    PPIDADD,PPIDADD     CLEAR WHO ADDED                              
         MVC   PPIDDEL,SVPID       SAVE INFO WHO DELETED                        
         MVI   PPIDPRG,PPIDPPKQ    ADDED VIA PRINTPAK (DEFAULT)                 
         CLI   DDLINKSW,C' '                                                    
         BNH   *+8                                                              
         MVI   PPIDPRG,PPIDADBQ    ADDED VIA ADBUYER                            
         CLI   DDLINKSW,C'K'                                                    
         BNE   *+8                                                              
         MVI   PPIDPRG,PPIDIDKQ    ADDED VIA IDESK                              
         TM    PLINKSW1,PRSMORGQ                                                
         JZ    *+8                                                              
         MVI   PPIDPRG,PPIDPRMQ    ADDED VIA PRISMA                             
         TM    PLINKSW1,RADAORGQ                                                
         JZ    *+8                                                              
         MVI   PPIDPRG,PPIDRADQ    ADDED VIA RADIA                              
         TM    ABUPLDSW,ADBYIMPQ                                                
         BZ    *+8                                                              
         MVI   PPIDPRG,PPIDIMPQ    ADDED VIA ADBUYER IMPORTS                    
         CLI   MADSW,C'Y'                                                       
         BNE   *+8                                                              
         MVI   PPIDPRG,PPIDPBUQ    ADDED VIA PBU                                
         LR    RE,RA                                                            
         USING TWAD,RE                                                          
         MVC   PPUSRID,TWAUSRID    USER ID                                      
         DROP  RE                                                               
         B     DELINS26                                                         
*                                                                               
DELINS20 MVC   PPIDDEL,SVPID       SAVE INFO WHO DELETED                        
*                                                                               
DELINS26 GOTO1 VRECUP,DMCB,(1,REC),WORK,(R5)   ADD A7                           
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BZ    DELINS30                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,PBYDKELQ                                                  
         BRAS  R9,NEXTEL                                                        
         BNE   DELINS30                                                         
         USING PBYDKELM,R5                                                      
         MVC   PBYDKDAT,IDSKDATE                                                
         MVC   PBYDKTIM,IDSKTIME                                                
         OI    PBYDKST1,BYDKDELQ                                                
         TM    PLINKSW1,PRSMORGQ   PRISMA ORIGIN?                               
         JZ    *+12                                                             
         OI    PBYDKST1,BYPMDELQ                                                
         NI    PBYDKST1,X'FF'-BYDKDELQ                                          
         TM    PLINKSW1,RADAORGQ   RADIA ORIGIN?                                
         JZ    *+12                                                             
         OI    PBYDKST3,BYRADELQ                                                
         NI    PBYDKST1,X'FF'-BYDKDELQ                                          
         TM    PRSMSTA1,BYPRMIVQ   PRISMA INVOICE ENABLED CAMPAIGN?             
         JZ    *+8                                                              
         OI    PBYDKST4,BYPRMIVQ                                                
         DROP  R5                                                               
*                                                                               
DELINS30 DS    0H                                                               
*                                                                               
DELINS_X J     SETCCEQ                                                          
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXCGET   NTR1  BASE=*,LABEL=*      GET FOREIGN EXCHANGE RATE/AMOUNT             
*                                                                               
         XC    SVFXPROF,SVFXPROF   FOREIGN EXCHANGE PROFILE VALUES              
         MVC   WORK+00(04),=C'P0FX'                                             
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL    NOTE: NO CLT OFFICE FOR FX PROFILE           
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'90',WORK),SVFXPROF,VDATAMGR                         
         CLI   SVFXPROF+00,C'Y'                                                 
         BNE   EXCGETX             FX PROFILE NOT SET TO USE FX FEATURE         
*                                                                               
*        FIND WHERE BUYREC IS                                                   
*                                                                               
         LA    R4,NEWREC                                                        
*                                                                               
         CLI   SVTRCODE,C'B'                                                    
         BE    *+8                                                              
         LA    R4,REC                                                           
*                                                                               
         USING PBUYREC,R4                                                       
*                                                                               
         LA    R5,PBDELEM          1ST ELEM IN BUY                              
         USING PBDELEM,R5          ESTABLISH BUY DESCRIPTION ELM                
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT A CANADIAN AGENCY                
         BNO   EXCGETX                                                          
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         OC    FXREP,FXREP         SKIP IF NO FX REP FOR AGENCY                 
         BZ    EXCGETX                                                          
*                                                                               
         L     RF,APUBIO           POINT TO PUBREC                              
                                                                                
         CLC   =C'90',PUBSTACD-PUBREC(RF) SKIP IF CANADIAN PUB                  
         BE    EXCGETX                                                          
*                                                                               
*        SKIP IF BUY RECORD LOCKED                                              
*                                                                               
         BRAS  RE,TSTLOCK          CHECKING FOR DATA LOCKINGS                   
         BNZ   EXGERR1                BUY LOCKED                                
*                                                                               
*        SKIP IF TERMINAL IN NON-UPDATIVE MODE                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,(CXTRAINF-COMFACSD)(RF)                                       
         USING XTRAINFD,RF         ESTABLISH EXTRAINEOUS INFO                   
*                                                                               
         TM    XIFLAG1,XIROSYS+XIROMODE SKIP IF READ ONLY SYSTEM                
         BNZ   EXCGETX                       OR READ ONLY MODE                  
*                                                                               
         DROP  RF                                                               
*                                                                               
*        SKIP IF FOREIGN EXCHANGE RATE ELEMENT PRESENT                          
*                                                                               
         MVI   ELCODE,BYCCIDQ      SET ID OF STANDARD CUSTOM COL                
         LA    R5,PBDELEM-PBUYREC(R4)    POINT TO FIRST ELEMENT IN BUY          
*                                                                               
EXGFXLP  DS    0H                                                               
*                                                                               
         BRAS  R9,NEXTEL           FIND CUSTOM COLUMN ELEMENT                   
         BNZ   EXGFXDN             NO ELEMENT FOUND                             
*                                                                               
         USING BYCCELD,R5          ESTABLISH CUSTOM COLUMN ELEMENT              
*                                                                               
         CLC   =AL2(FXRATEQ),BYCCSQN    LOOK FOR FXRATE ELEMENT                 
         BE    EXGFXFD                                                          
*                                                                               
EXGFXCN  DS    0H                                                               
         B     EXGFXLP                                                          
*                                                                               
EXGFXDN  DS    0H                                                               
         B     EXGFXNF             NO FXRATE ELEMENT                            
*                                                                               
EXGFXFD  DS    0H                                                               
*                                                                               
         ZAP   EXCRATE,BYCCDATA(8) SAVE RATE                                    
*                                                                               
         DROP  R5                                                               
*                                                                               
         B     EXGFDX                                                           
*                                                                               
EXGFXNF  DS    0H                                                               
*                                                                               
         CLI   SVTRCODE,C'B'       SKIP IF NOT A NEW BUY                        
         BNE   EXCGETX                                                          
*                                                                               
*        READ FOREIGN EXCHANGE FROM CONTROL SYSTEM                              
*                                                                               
         XC    EXCKEY,EXCKEY                                                    
         LA    R2,EXCKEY           ESTABLISH EXCHANGE RECORD KEY                
         USING GEXCD,R2                                                         
*                                                                               
         MVI   GEKREC,GEKRECQ      RECORD ID                                    
         MVC   GEKAGY,PBUYKAGY     AGENCY                                       
         MVI   GEKSYS,X'04'        SET SYSTEM ID                                
         MVC   GEKCURF,=C'CAD'     FROM CURRENCY                                
         MVC   GEKCURT,=C'USD'     TO CURRENCY                                  
         MVI   GEKCTYP,GEKBOOQ     BOOKING RATE                                 
         MVI   GEKMED,X'FF'                                                     
         MVC   GEKCLI,PBUYKCLT     CLIENT                                       
         MVI   GEKPRO,X'FF'                                                     
         MVI   GEKCAM,X'FF'                                                     
*                                                                               
         GOTOR VDATCON,DMCB,(3,PBUYKDAT),(2,GEKPEND) BUY DATE                   
*                                                                               
         MVC   EXCKEYSV,EXCKEY     SAVE STARTING KEY                            
*                                                                               
         GOTOR VDATAMGR,DMCB,=CL7'DMRDHI',=C'GENDIR',GEKEY,GEKEY                
         CLI   DMCB+8,0            NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GEXCD(GEKPEND-GEXCD),EXCKEYSV MATCH ON CLIENT                    
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPEND-GEXCD+EXCKEYSV MATCH ON PERIOD                   
         BNH   EXGFD               RECORD FOUND                                 
*                                                                               
         MVC   EXCKEY,EXCKEYSV     RESTORE STARTING KEY                         
         MVC   GEKCLI,=3X'FF'      SET TO DEFAULT CLIENT                        
*                                                                               
         MVC   EXCKEYSV,EXCKEY     SAVE STARTING KEY                            
*                                                                               
         GOTOR VDATAMGR,DMCB,=CL7'DMRDHI',=C'GENDIR',GEKEY,GEKEY                
         CLI   DMCB+8,0            NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GEXCD(GEKPEND-GEXCD),EXCKEYSV MATCH ON CLIENT                    
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPEND-GEXCD+EXCKEYSV MATCH ON PERIOD                   
         BNH   EXGFD               RECORD FOUND                                 
*                                                                               
         MVC   EXCKEY,EXCKEYSV     RESTORE STARTING KEY                         
*                                                                               
         MVC   GEKAGY,=X'0000'     CLEAR AGENCY                                 
         MVI   GEKSYS,X'FF'        FINANCIAL TIMES                              
         MVI   GEKCTYP,C'C'        FINANCIAL TIMES RATE                         
*                                                                               
         MVC   EXCKEYSV,EXCKEY     SAVE STARTING KEY                            
*                                                                               
         GOTOR VDATAMGR,DMCB,=CL7'DMRDHI',=C'GENDIR',GEKEY,GEKEY                
         CLI   DMCB+8,0            NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GEXCD(GEKPEND-GEXCD),EXCKEYSV MATCH ON CLIENT                    
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPEND-GEXCD+EXCKEYSV MATCH ON PERIOD                   
         BNH   EXGFD               RECORD FOUND                                 
*                                                                               
         B     EXGERR2             CAN'T FIND RATE                              
*                                                                               
EXGFD    DS    0H                  HAVE THE EXCHANGE RECORD                     
*                                                                               
*        READ IN EXCHAGE RECORD                                                 
*                                                                               
         GOTOR VDATAMGR,DMCB,=CL7'GETREC',=C'GENFIL',GEDDA,AWRKREC,    X        
               (TERMNAL,DMWORK)                                                 
         CLI   DMCB+8,0            NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AWRKREC          POINT TO READ RECORD                         
*                                                                               
         MVI   ELCODE,GEXELQ       LOOK FOR EXCHANGE ELEMENT                    
         LA    R5,GEKEY+GEFIRST    POINT TO FIRST ELEMENT                       
         CLC   ELCODE,0(R5)        CHECK FIRST ELEMENT                          
         BE    *+12                                                             
         BRAS  R9,NEXTEL           FIND THE ELEMENT                             
         BNE   EXGERR2                                                          
*                                                                               
         USING GEXEL,R5            ESTABLISH EXCHANGE ELEMENT                   
*                                                                               
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+2(5),GEXRATE    GET RATE                                     
         SRP   DUB,64-1,5          SHIFT RIGHT ONE DIGIT                        
         ZAP   EXCRATE,DUB         RETURN RATE                                  
*                                                                               
*        ADD !FXRATE ELEMENT                                                    
*                                                                               
         LA    R5,WKTEMP1          BUILD AND ADD !FXRATE ELEMENT                
         XC    WKTEMP1,WKTEMP1                                                  
         USING BYCCELD,R5          ESTABLISH CUSTON COLUMN ELEMENT              
*                                                                               
         MVI   BYCCELM,BYCCIDQ     SET ELEMENT ID                               
         MVI   BYCCLEN,BYCCHDRL+8  ELEMENT LENGTH                               
         MVC   BYCCSQN,=AL2(FXRATEQ)  STANDARD CUSTON COL #                     
         ZAP   BYCCDATA(8),EXCRATE ADD RATE TO ELEMENT                          
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,X'FF'        GET TO END OF RECORD                         
*                                                                               
         LA    R5,PBDELEM-PBUYREC(R4)  1ST ELEM IN BUYREC                       
         BRAS  R9,NEXTEL                                                        
*                                                                               
*        ADD FXRATE STD CUSTOM COLUMN ELEMENT                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(1,(R4)),WKTEMP1,(R5)                                
*                                                                               
EXGFDX   DS    0H                                                               
*                                                                               
EXGFXAC2 LA    R5,PBDELEM-PBUYKEY(R4)                                           
         MVI   ELCODE,X'44'                                                     
*                                                                               
EXGFXACL BRAS  R9,NEXTEL                                                        
         BNE   EXGFXACD                                                         
*                                                                               
         USING PACELEM,R5                                                       
*                                                                               
         CLC   =C'FX',PACCODE                                                   
         BNE   EXGFXACL            FIND FX CHARGE AND DELETE IT                 
*                                                                               
EXGFXACF GOTO1 VRECUP,DMCB,(1,(R4)),(R5),(R5)                                   
         B     EXGFXAC2                                                         
         DROP  R5                                                               
*                                                                               
EXGFXACD DS    0H                                                               
*                                                                               
*        GET BUY VALUES                                                         
*                                                                               
         GOTOR VGETINS,DMCB,(R4),PVALUES,PBUYKPRD-PBUYKEY(R4),         +        
               (C'F',0),0,0                                                     
*                                                                               
*        CALCULATE GROSS FX                                                     
*                                                                               
         L     RF,GROSS            GET GROSS                                    
         CVD   RF,DUB              CVD                                          
         ZAP   PL16,DUB                                                         
         SP    EXCRATE,=P'100000'  DIFFERENCE IN CAD TO USD RATE                
         MP    PL16,EXCRATE        CALCULATE FX AMOUNT                          
         SRP   PL16,64-5,5         ROUND TO 2 DECIMALS                          
*                                                                               
         LA    R5,WKTEMP1          BUILD AND ADD FX ADDITIONAL CHG ELM          
         XC    WKTEMP1,WKTEMP1                                                  
         USING PACELEM,R5          ESTABLISH ADDITIONAL CHARGE ELEMENT          
         MVI   PACELEM,X'44'       SET ELEMENT ID                               
         MVI   PACELEM+1,32        ELEMENT LENGTH                               
         MVC   PACCODE,=C'FX'      ADDITIONAL CHARGE CODE                       
         MVI   PACGN,C'G'          GROSS ENTERED                                
         MVI   PACCD,C'Y'          SUBJECT TO CASH DISCOUNT                     
         MVI   PACAC,C'Y'          COMMISSIONABLE                               
         ZAP   PACAMT,PL16         FX AMOUNT                                    
         DROP  R5                                                               
*                                                                               
*        ADD FX ADDITIONAL CHARGE ELEMENT                                       
*                                                                               
         LA    R5,PBDELEM-PBUYKEY(R4)                                           
         MVI   ELCODE,X'44'        POINT TO 1ST AC ELEM OR END OF REC           
         BRAS  R9,NEXTEL                                                        
         GOTO1 VRECUP,DMCB,(1,(R4)),WKTEMP1,(R5)                                
*                                                                               
EXCGETX  DS    0H                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1                                                                   
*                                                                               
EXGERR1  DS    0H                  BUY RECORD LOCKED                            
         LA    R2,BUYTR1H                                                       
         LA    R3,DATALOCK                                                      
         B     EXCGETER                                                         
*                                                                               
EXGERR2  DS    0H                  CANNOT FIND FX RATE                          
         LA    R2,BUYTR1H                                                       
         LA    R3,EXCRTNFD                                                      
         B     EXCGETER                                                         
*                                                                               
EXCGETER DS    0H                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT_R2R3 XIT1  REGS=(R2,R3)        ERROR CURSOR AND MSG                         
*                                                                               
                                                                                
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIDKC   NTR1  BASE=*,LABEL=*      CHECK IDESK CONTROL                          
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         JNZ   CKIDKC10                                                         
         TM    SVESPROF+29,X'20'   IDESK ESTIMATE?                              
         JZ    CKIDKC10                                                         
         LA    R2,BUYTR1H                                                       
         LA    R3,IDKESERQ                                                      
         J     ERR_EXIT                                                         
*                                                                               
CKIDKC10 TM    REC+(PBDSTAT2-PBUYREC),X'20'                                     
         BZ    CKIDKC_X                                                         
*                                                                               
         TM    ABUPLDSW,IDSKUPLQ   IDESK INSERTION UPLOAD?                      
         BNZ   CKIDKC_X                                                         
*                                                                               
         XC    SVIDKPRF,SVIDKPRF   IDESK CONTROL PROFILE VALUES                 
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
         MVC   WORK+06(1),BUYMD                                                 
         MVC   WORK+07(3),BUYCL                                                 
         CLI   SVCLTOFC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFC                                              
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),SVIDKPRF,VDATAMGR                         
         CLI   SVIDKPRF+00,C'Y'                                                 
         BE    CKIDKC_E            CANNOT CHG/DEL IDESK INSERTION               
*                                                                               
CKIDKC_X J     SETCCEQ             SET CC EQUAL                                 
*                                                                               
CKIDKC_E LHI   R3,IDSKCERR         CANNOT CHG/DEL IDESK INSERTIONS              
         LA    R2,BUYTR1H          CURSOR TO TRANSACTION FIELD                  
*                                                                               
ERR_EXIT L     R4,ERRAREA          EXITS FROM PROGRAM                           
         MVI   ERRAREA,X'FF'                                                    
         BRAS  RE,GET_ETXT                                                      
         OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
         ST    R2,ADBERRFD         ADDRESS OF ERROR FLD                         
         LTR   RB,RB               SET CC NOT EQUAL                             
         J     XIT_R2R3                                                         
*                                                                               
         DROP  RB                                                               
*                                                                               
                                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRKCC    NTR1  BASE=*,LABEL=*      TRACK STDCOL CHANGES                         
*                                                                               
         TM    REC+(PBDSTAT2-PBUYREC),X'20'  SKIP IF NOT BY IDESK               
         BNO   TRKCC_X                                                          
*                                                                               
         OC    SVIDKPRF,SVIDKPRF   SKIP IF IDK PROFILE LOADED                   
         BNZ   TRKCCPRX                                                         
*                                                                               
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
*                                  GET AGENCY LEVEL PROFILE                     
         GOTO1 (RF),DMCB,(X'D0',WORK),SVIDKPRF,VDATAMGR                         
*                                                                               
TRKCCPRX DS    0H                                                               
*                                                                               
         CLI   SVIDKPRF+1,C'Y'     SKIP IF PROFILE NOT SET                      
         BNE   TRKCC_X                                                          
*                                                                               
         TM    CHGIND5,PCHGTRKQ    SKIP IF NO TRACKED STD COLS CHG'D            
         BNO   TRKCC_X                                                          
         BRAS  RE,TSTINVCC         HAVE INVOICE COLUMNS IN REQUEST?             
         JE    TRKMQST                                                          
*                                                                               
         LA    R5,REC+(PBDELEM-PBUYKEY)  POINT TO 1ST ELM                       
         MVI   ELCODE,BYCCIDQ      LOOKING FOR CUSTCOL ELEMENTS                 
         MVI   WKBYTE2,0           INIT TRACKED INVOICE COLUMNS FLAG            
         USING BYCCELD,R5                                                       
TRKCCK12 BRAS  R9,NEXTEL           CUSTOM COLUMN ELEMENT FOUND?                 
         JE    TRKCCK18                                                         
         CLI   WKBYTE2,C'Y'        ONLY NON-INVOICE COLUMNS?                    
         JE    TRKCC_X                                                          
         J     TRKMQST                                                          
TRKCCK18 TM    BYCCSWS,BYCCTRKQ    IF TRACKED STDCOL FOUND                      
         JZ    TRKCCK12                                                         
         CLC   BYCCSQN,=AL2(8211)  SEQ# < INVOICE STATUS?                       
         JL    TRKCCK24                                                         
         CLC   BYCCSQN,=AL2(8214)  SEQ# > INVOICE AMOUNT?                       
         JH    TRKCCK24                                                         
         J     TRKMQST             HAVE INVOICE COLUMNS                         
TRKCCK24 MVI   WKBYTE2,C'Y'        HAVE NON-INVOICE COLUMNS                     
         J     TRKCCK12                                                         
         DROP  R5                                                               
                                                                                
*                                                                               
*        BUILD MQ MESSAGE                                                       
*                                                                               
TRKMQST  DS    0H                                                               
*                                                                               
         LAY   R2,MQMSG            POINT TO MQ MESSAGE BUILD AREA               
*                                                                               
         CLC   4(L'QULABEL,R2),QULABEL  SKIP IF NOT 1ST TIME                    
         BE    TRKMQINX                                                         
*                                                                               
*        ADD QUEUE LABEL, AGENCY ALPHA TO MESSAGE                               
*                                                                               
         LA    R2,4(R2)            BYPASS LENGTH POSITION                       
*                                                                               
*        QUEUE LABEL                                                            
*                                                                               
         MVC   0(L'QULABEL,R2),QULABEL LABEL                                    
         LA    R2,L'QULABEL(R2)    BUMP PAST LABEL                              
*                                                                               
         MVC   0(L'CRLF,R2),CRLF   ADD CRLF                                     
         LA    R2,L'CRLF(R2)       BUMP PAST CRLF                               
*                                                                               
*        AGENCY ALPHA                                                           
*                                                                               
         MVC   0(L'QUMCAGY,R2),QUMCAGY    AGENCY ALPHA ID MAPCODE               
         MVI   L'QUMCAGY(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCAGY+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         MVC   0(L'PBUYKAGY,R2),PBUYKAGY-PBUYKEY+REC  PASS AGY ALPHA            
         LA    R2,L'PBUYKAGY(R2)   BUMP PAST AGENCY ALPHA                       
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
*        USER'S PID AND NAME                                                    
*                                                                               
         BRAS  RE,TRNPID           TRANSLATE USER'S PID                         
*                                                                               
         MVC   0(L'QUMCPID,R2),QUMCPID    PID ALPHA ID MAPCODE                  
         MVI   L'QUMCPID(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCPID+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         MVC   0(L'WKPID,R2),WKPID    PASS PID                                  
         LA    R2,L'WKPID(R2)      BUMP PAST PID                                
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
         MVC   0(L'QUMCPNM,R2),QUMCPNM    PID NAME ID MAPCODE                   
         MVI   L'QUMCPNM(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCPNM+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         MVC   0(L'WKPIDNM,R2),WKPIDNM PASS PID NAME                            
*                                                                               
*        FIND END OF NAME                                                       
*                                                                               
         LA    R2,L'WKPIDNM-1(R2)  POINT TO END OF MAX NAME                     
         LA    R0,L'WKPIDNM        MAX LENGTH                                   
*                                                                               
         CLI   0(R2),C' '          FIND LAST CHARACTOR OF NAME                  
         BH    *+10                                                             
         BCTR  R2,0                BACK UP A POSITION                           
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         MVC   0(L'CRLF,R2),CRLF   ADD CRLF                                     
         LA    R2,L'CRLF(R2)       BUMP PAST CRLF                               
*                                                                               
         LAY   RF,MQMSG                                                         
         SR    R2,RF               CALCULATE MSG LENGTH                         
         SHI   R2,4                DECREMENT FOR LENGTH FIELD                   
         STCM  R2,15,0(RF)         SET MESSAGE LENGTH                           
*                                                                               
TRKMQINX DS    0H                                                               
*                                                                               
*        ADD BUY SERIAL NUMBER TO MSG                                           
*        MED/CLT/SER#                                                           
*                                                                               
         LAY   R2,MQMSG            START OF MESSAGE                             
         ICM   RF,15,0(R2)         CURRENT LENGTH                               
         LA    R2,4(RF,R2)         NEXT AVAILABLE SPACE                         
*                                                                               
         MVC   0(L'QUMCSER#,R2),QUMCSER#  BUY SERIAL NUMBER MAPCODE             
         MVI   L'QUMCSER#(R2),SEMICOLN    SEPARATOR                             
         LA    R2,L'QUMCSER#+1(R2)        BUMP PAST MAPCODE                     
*                                                                               
         MVC   0(L'PBUYKMED,R2),PBUYKMED-PBUYKEY+REC       MEDIA                
         MVC   L'PBUYKMED(L'PBUYKCLT,R2),PBUYKCLT-PBUYKEY+REC CLT               
*                                                                               
         LA    R2,L'PBUYKMED+L'PBUYKCLT(R2)  BUMP POINTER                       
*                                                                               
         LA    R5,REC+(PBDELEM-PBUYKEY)  POINT TO 1ST ELM                       
         MVI   ELCODE,X'99'                                                     
         BRAS  R9,NEXTEL           FIND BUY SERIAL ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                NEED TO FIND IT                              
*                                                                               
         USING PSERELEM,R5         ESTABLISH SERIAL NUMBER ELM                  
*                                                                               
         ZAP   DUB,PSERNUM         COPY BUY SERIAL NUMBER                       
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(9,R2),DUB         CONVERT TO CH                                
*                                                                               
         LA    R2,9(R2)            BUMP POINTER                                 
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*        ADD STANDARD COLUMNS TO MSG                                            
*                                                                               
         LA    R5,REC+(PBDELEM-PBUYKEY)  POINT TO 1ST ELM                       
         MVI   ELCODE,BYCCIDQ      LOOKING FOR CUSTCOL ELEMENTS                 
*                                                                               
TRKCCLP  DS    0H                                                               
*                                                                               
         USING BYCCELD,R5          ESTABLISH STDCOL ELM                         
*                                                                               
         BRAS  R9,NEXTEL           FIND NEXT STDCOL ELM                         
         BNE   TRKCCDN             NO MORE                                      
*                                                                               
         TM    BYCCSWS,BYCCTRKQ    IF TRACKED STDCOL FOUND                      
         BNO   TRKCCCN                NO                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,BYCCSQN        GET STDCOL SQN                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(4,R2),DUB         RETURN SQN                                   
*                                                                               
         MVI   4(R2),SEMICOLN      SEPARATOR                                    
         LA    R2,5(R2)            BUMP PAST MAPCODE                            
*                                                                               
         LLC   RF,BYCCLEN          SET ELEMENT LENGTH                           
         SHI   RF,BYCCHDRL         DATA LENGTH                                  
         BNP   TRKCC20             NO DATA                                      
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         CLC   BYCCSQN,=AL2(8214)  IF INVOICE AMOUNT                            
         BNE   TRKCC10                                                          
*                                                                               
         MVI   0(R2),C'$'          ALWAYS DOLLARS                               
         AHI   R2,1                BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,BYCCDATA        COPY NUMBER                                  
*                                                                               
         EDIT  (P8,DUB),(17,0(R2)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT              
*                                                                               
         AR    R2,R0               BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         B     TRKCC20                                                          
*                                                                               
TRKCC10  DS    0H                  TEST DATA                                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),BYCCDATA    ADD STD COL TO MESSAGE                       
*                                                                               
         LA    R2,1(RF,R2)         BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
TRKCC20  DS    0H                                                               
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
TRKCCMCD DS    0H                                                               
*                                                                               
TRKCCCN  DS    0H                  NEXT STANDARD COLUMN                         
         B     TRKCCLP                                                          
*                                                                               
TRKCCDN  DS    0H                  END OF STANDARD COLUMNS                      
*                                                                               
*        TOTAL PAID                                                             
*                                                                               
         GOTOR VGETINS,DMCB,REC,PVALUES,REC+7,0,0,0                             
*                                                                               
         MVC   0(L'QUMCTLPD,R2),QUMCTLPD  TOTAL PAID MAPCODE                    
         MVI   L'QUMCTLPD(R2),SEMICOLN    SEPARATOR                             
         LA    R2,L'QUMCTLPD+1(R2)        BUMP PAST MAPCODE                     
*                                                                               
         MVI   0(R2),C'$'          ALWAYS DOLLARS                               
         AHI   R2,1                BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         ICM   RF,15,PAID          TOTAL PAID AMOUNT                            
         CVD   RF,DUB              CVD                                          
*                                                                               
         EDIT  (P8,DUB),(17,0(R2)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT              
*                                                                               
         AR    R2,R0               BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         MVC   0(L'CRLF,R2),CRLF   ADD CRLF                                     
         LA    R2,L'CRLF(R2)       BUMP PAST CRLF                               
*                                                                               
         LAY   RF,MQMSG                                                         
         SR    R2,RF               CALCULATE MSG LENGTH                         
         SHI   R2,4                DECREMENT FOR LENGTH FIELD                   
         STCM  R2,15,0(RF)         SET MESSAGE LENGTH                           
*                                                                               
         B     TRKCC_X             ALL DONE                                     
*                                                                               
*        SEARCH FOR CC ELMS AND ADD TO MESSAGE                                  
*                                                                               
*        INIT MQIO FOR MQ OUTPUT                                                
*                                                                               
*        SEND MQ MESSAGE                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTO1 (RF),DMCB,(X'C0',WORK),SVIDKPRF,VDATAMGR                         
         CLI   SVIDKPRF+00,C'Y'                                                 
         JE    CKIDKC_E            CANNOT CHG/DEL IDESK INSERTION               
*                                                                               
TRKCC_X  J     SETCCEQ             SET CC EQUAL                                 
*                                                                               
*                                                                               
TRKCCER  DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         J     XIT_R2R3                                                         
*                                                                               
SEMICOLN EQU   X'5E'               SEMI-COLON                                   
TAB      EQU   X'05'               TAB                                          
CRLF     DC    X'0D25'             CRLF                                         
*                                                                               
QULABEL  DC    CL16'IDKINV**********'     QUEUE LABEL                           
QUMCAGY  DC    CL4'0003'           AGENCY MAPCODE                               
QUMCSER# DC    CL4'0020'           BUY SERIAL NUMBER                            
QUMCPID  DC    CL4'0100'           USER'S PID                                   
QUMCPNM  DC    CL4'0110'           USER'S PID NAME                              
QUMCTLPD DC    CL4'0150'           TOTAL PAID DOLLARS                           
*                                                                               
QUMCTAB  DS    0D                  TABLE OF MAPCODES                            
QUSTCLID DS    AL2                 STANDARD COLUMN ID                           
QUSTCLMC DS    CL4                 STANDARD COLUMN MAPCODE                      
QUMCTABL EQU   *-QUMCTAB           LENGTH OF TABLE ENTRY                        
         ORG   QUMCTAB                                                          
         DC    AL2(8211),CL4'8211'  AB INVOICE STATUS                           
         DC    AL2(8212),CL4'8212'  AB INVOICE COMMENTS                         
         DC    AL2(8213),CL4'8213'  AB INVOICE NUMBER                           
         DC    AL2(8214),CL4'8214'  AB INVOICE AMOUNT                           
         DC    X'FF'               EOT                                          
*                                                                               
         DROP  R5,RB                                                            
*                                                                               
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY                                                                 *         
*        SVPID    A(PID)                                              *         
*        WKPID    8CH PID                                             *         
*        WKPIDNM  USER'S NAME                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WKPID,SPACES        INIT OUTPUT                                  
         MVC   WKPIDNM,SPACES      INIT OUTPUT                                  
*                                                                               
         OC    SVPID,SVPID         SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,SPACES      IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,PBUYKAGY       USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,SVPID       SET PID                                      
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL7'DMRDHI',=CL7'CTFILE',KEY,AWKAIO1              
*                                                                               
         L     R4,AWKAIO1          POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
         MVC   WKPID,2(RE)         SAVE 8 CH PID                                
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,SPACES      IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,PBUYKAGY       USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,WKPID       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL7'DMRDHI',=CL7'CTFILE',KEY,AWKAIO1              
*                                                                               
         L     R4,AWKAIO1          POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENT NOT FOUND                       
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WKPIDNM          BUILD NAME IN WORKAREA                       
         XC    WKPIDNM,WKPIDNM                                                  
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WKPID(7),=CL7'UNKNOWN'                                           
         MVC   WKPIDNM(7),=CL7'UNKNOWN'                                         
*                                                                               
         LA    R1,WKPIDNM+7        POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WKPIDNM          START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WKPIDNM,(R0) SQUASH NAME                           
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SENDMQ   NTR1  BASE=*,LABEL=*      SEND MQ MESSAGE TO IDESK                     
*                                                                               
         TM    REC+(PBDSTAT2-PBUYREC),X'20'  SKIP IF NOT BY IDESK               
         BNO   SENDMQX                                                          
*                                                                               
         OC    SVIDKPRF,SVIDKPRF   SKIP IF IDK PROFILE LOADED                   
         BNZ   SNDMQPRX                                                         
*                                                                               
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
*                                  GET AGENCY LEVEL PROFILE                     
         GOTO1 (RF),DMCB,(X'D0',WORK),SVIDKPRF,VDATAMGR                         
*                                                                               
SNDMQPRX DS    0H                                                               
*                                                                               
         CLI   SVIDKPRF+1,C'Y'     SKIP IF PROFILE NOT SET                      
         BNE   SENDMQX                                                          
*                                                                               
*        SEND MQ MESSAGE                                                        
*                                                                               
         LAY   R2,MQMSG            POINT TO MESSAGE                             
         ICM   R4,15,0(R2)         GET MESSAGE LENGTH                           
         BZ    SENDMQX             NONE TO SEND                                 
         XC    DUB,DUB             INIT ERROR AREA                              
*                                                                               
         L     RF,ACOMFACS         GET DDMQIO ADDRESS                           
         L     RF,CMQIO-COMFACSD(RF)                                            
*                                                                               
         GOTO1 (RF),WORK,=CL8'PUT',4(R2),(R4),0,0,DUB,C'UNIT',0                 
*                                                                               
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SENDMQX  J     SETCCEQ             SET CC EQUAL                                 
*                                                                               
SENDMQER DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         J     XIT_R2R3                                                         
*                                                                               
                                                                                
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTINVCC NTR1  BASE=*,LABEL=*      TEST FOR INVOICE STANDARD COLUMNS            
*                                                                               
         CLI   DDLINKSW,0          UPDATE REQUEST FROM LINK?                    
         JE    SETCCNEQ                                                         
*                                                                               
         L     R3,VTIA             FIRST 4096 BYTES HAVE WORKER REC             
         LA    R3,4(R3)            POINT TO WORKER ELEM                         
         USING LQ_EL,R3                                                         
TSTICC12 CLI   LQ_EL,LQ_RDATQ      RETURNED DATA HEADER ELEM?                   
         JE    SETCCNEQ                                                         
         CLI   LQ_EL,LQ_RQSTQ      REQUEST DATA ELEM?                           
         JE    *+12                                                             
TSTICC22 BRAS  RE,NXTWFELM                                                      
         J     TSTICC12                                                         
         CLC   =AL2(D#CCSEQN),3(R3)                                             
         JNE   TSTICC22                                                         
         CLC   =C'8211',6(R3)      INVOICE STATUS?                              
         JE    SETCCEQ                                                          
         CLC   =C'8212',6(R3)      INVOICE COMMENT?                             
         JE    SETCCEQ                                                          
         CLC   =C'8213',6(R3)      INVOICE NUMBER?                              
         JE    SETCCEQ                                                          
         CLC   =C'8214',6(R3)      INVOICE AMOUNT?                              
         JE    SETCCEQ                                                          
         J     TSTICC22                                                         
                                                                                
*                                                                               
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FIXDAT   NTR1  BASE=*,LABEL=*      FIX DATE IN PBU RECORD                       
*                                                                               
         MVI   FIXDTSTA,C'Y'       SET STATUS TO FIX DATE                       
*                                                                               
         BRAS  RE,UPDBLIN          RESTORE OLD BUYLINE AND DATE                 
*                                                                               
         MVI   FIXDTSTA,0                                                       
*                                                                               
         XIT1                                                                   
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UPDBLIN  NTR1  BASE=*,LABEL=*      UPDATE BUYLINES IN PBU RECORD                
*                                                                               
         L     R5,AMINBLK          UPLOAD ELEM                                  
         USING MINBLKD,R5                                                       
         BRAS  RE,MINIT            INITIALIZE MINIO BLOCK                       
*                                                                               
* BUILD THE MASTER KEY                                                          
*                                                                               
         LA    R4,NEWREC                                                        
         L     RE,TRADDR                                                        
         CLI   8(RE),C'B'                                                       
         BE    *+8                                                              
         LA    R4,REC                                                           
*                                                                               
         LA    R3,MINMKEY                                                       
         USING PEUPKEY,R3                                                       
*                                                                               
         MVC   PEUPAGY,PBUYKAGY-PBUYREC(R4)                                     
         MVC   PEUPMED,PBUYKMED-PBUYREC(R4)                                     
         MVI   PEUPTYPE,X'90'                                                   
         MVC   PEUPCLT,PBUYKCLT-PBUYREC(R4)                                     
         MVC   PEUPPRD,PBUYKPRD-PBUYREC(R4)                                     
         MVC   PEUPEST,PBUYKEST-PBUYREC(R4)                                     
         MVC   PEUPPUB,PBUYKPUB-PBUYREC(R4)                                     
         DROP  R3                                                               
*                                                                               
* READ AND UPDATE THE INSERTION ELEMENT FOR AN EXISTING MINIO RECORD            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
         GOTO1 VMINIO,DMCB,('MINRD',(R5))                                       
         CLI   MINERR,0            THIS ELEMENT MUST EXIST                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'       INSERTION ELEMENT                            
         GOTO1 VMINIO,DMCB,('MINHI',(R5))                                       
         B     UPDLN20                                                          
*                                                                               
UPDLN10  GOTO1 VMINIO,DMCB,('MINSEQ',(R5))                                      
*                                                                               
UPDLN20  CLI   MINERR,0                                                         
         BE    UPDLN30                                                          
         CLI   MINERR,MINEEOF      NO MORE INSERTIONS?                          
         BE    UPDLN50                                                          
         DC    H'0'                                                             
*                                                                               
UPDLN30  L     R2,MINELEM          POINT TO FOUND ELEMENT                       
         USING PEUPMEL,R2                                                       
*                                                                               
         BRAS  RE,MATCHINS         MATCH CORRECT INSERTION                      
         BNE   UPDLN10                                                          
*                                                                               
UPDLN40  MVC   PEUPPLIN,PBUYKLIN-PBUYREC(R4) UPDATE LINE#                       
*                                                                               
         CLI   FIXDTSTA,C'Y'       IF FIXING DATE,                              
         BNE   UPDLN45                                                          
         MVC   PEUPPDT,PBUYKDAT-PBUYREC(R4)  RESTORE ORIGINAL DATE              
*                                                                               
UPDLN45  GOTO1 VMINIO,DMCB,('MINWRT',(R5))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
* MUST CLOSE AFTER MAKING CHANGES                                               
*                                                                               
UPDLN50  GOTO1 VMINIO,DMCB,('MINCLS',(R5))                                      
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R5,RB                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         USING PEUPMEL,R2                                                       
MATCHINS NTR1  BASE=*,LABEL=*                                                   
         USING PIUPEL,R5                                                        
         MVI   ELCODE,PIUPELCQ                                                  
         LA    R5,33(R4)                                                        
         BRAS  R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'00'               ELEMENT SHOULD EXIST                         
         CLC   PEUPUNIQ,PIUPUSEQ   SAME UNIQUE INSERTION ID?                    
         JNE   SETCCNEQ                                                         
         CLI   PIUPELLN,PIUPELXQ   NEW LENGTH?                                  
         JL    SETCCEQ                                                          
         CLC   PEUPUIQX,PIUPUQXT   SAME EXTENDED UNIQUE ID?                     
         JNE   SETCCNEQ                                                         
*                                                                               
         J     SETCCEQ                                                          
         DROP  R2,R5,RB                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK03D  DSECT                                                                  
*                                                                               
SPACES   DS    CL255               C' '                                         
*                                                                               
WKBYTE1  DS    X                                                                
WKBYTE2  DS    X                                                                
WKFULL1  DS    F                                                                
WKFULL2  DS    F                                                                
WKDUB1   DS    D                                                                
WKDUB2   DS    D                                                                
WRKSAVRE DS    F                   SAVE RETURN ADDRESS VIA RE                   
WKRELO03 DS    F                   RELO FACOR                                   
*                                                                               
AWKAIO1  DS    A                                                                
*                                                                               
MVTOP_RE DS    F                   FOR CKADBELM ROUTINE                         
MVELFULL DS    F                                                                
MVELSVRE DS    F                                                                
MVELCNT1 DS    H                                                                
MVELCNT2 DS    H                                                                
MVELHALF DS    H                                                                
MVELBYTE DS    X                                                                
*                                                                               
WKSVADCD DS    XL(L'PBDJOB)                                                     
WKSVKEY  DS    XL(L'KEY)                                                        
WKSVAIO  DS    XL(L'AREC)                                                       
WKELEM   DS    XL256                                                            
WKTEMP1  DS    XL256                                                            
WKTEMP2  DS    XL256                                                            
*                                                                               
S#DUPLSW DS    X                   SERIAL# DUPLICATE SWITCH                     
*                                                                               
DAYPIDSW DS    C                   SAME DAY SAME PID SWITCH                     
*                                                                               
UPPDTNEW DS    XL3                 NEW PASSIVE DATE - BINARY YMD                
UPPDTOLD DS    XL3                 OLD PASSIVE DATE - BINARY YMD                
*                                                                               
IDSKDATE DS    XL3                 IDESK INSERTION UPLOAD DATE                  
IDSKTIME DS    XL3                 IDESK INSERTION UPLOAD TIME                  
PRSMSTA1 DS    X                   PRISMA STATUS 1                              
*                                                                               
EXCKEY   DS    XL64                EXCHANGE RECORD KEY                          
EXCKEYSV DS    XL64                EXCHANGE RECORD KEY SAVE                     
EXCRATE  DS    PL8                 EXCHANGE RATE                                
FXRATEQ  EQU   X'200F'             FXRATE STANDARD CUSTOM COLUMN CODE           
PL16     DS    PL16                WORK PACKED FIELD                            
*                                                                               
SVFXPROF DS    CL16                FOREIGN EXCHANGE PROFILE VALUES              
SVIDKPRF DS    CL16                IDESK CONTROL PROFILE VALUES                 
*                                                                               
WKPID    DS    CL8                 USER'S PID                                   
WKPIDNM  DS    CL60                USER'S NAME                                  
*                                                                               
FIXDTSTA DS    C                   FIX DATE STATUS = Y                          
*                                                                               
WK_AOR_A DS    (WK_AOR_Q)X         WORKING STORAGE AREA FOR AOR TABLE           
WK_AOR_Q EQU   300*20              300 AOR ENTRIES AT 20 BYTES EACH             
*                                                                               
WKAIO1   DS    XL4096                                                           
*                                                                               
WORK03X  EQU   *                   END OF LOCAL WORKING STORAGE AREA            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        INCLUDES                                                               
*                                                                               
*PPGENBYPP                                                                      
*PPGENBPNV                                                                      
*PPBUYWRK1                                                                      
*PPBUYWRK2                                                                      
*GEGENEXC                                                                       
         PRINT OFF                                                              
       ++INCLUDE PPGENBYPP                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPGENPNV          NEW INVOICE RECORD                           
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE GEGENEXC                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF         EXTRA USER INFO                              
       ++INCLUDE FATWA             TERMINAL WORK AREA                           
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097PPBUY03   02/11/20'                                      
         END                                                                    
