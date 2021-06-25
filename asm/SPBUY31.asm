*          DATA SET SPBUY31    AT LEVEL 087 AS OF 11/03/20                      
*PHASE T21131C                                                                  
*===============================================================                
* REMEMBER IF ANY NEW SCREENS ADDED TO UPDATE SPECIFIC TESTS                    
* IN SPBUY00. LOOK AT ALL REFERENCES TO SVSCR THERE !                           
*===============================================================                
                                                                                
*=YYMMMDD=======================================================                
*                                                                               
* 20NOV04 WHOA SPEC-36302 - INCORPORATE AUTO-AVAIL UUIDS FOR DARE MG'S          
*                                                                               
* 20JUL20 HWON SPEC-48021 - IGNORE DELETED MG SEQ REC IF NOT CANCEL STA         
*                                                                               
* 20JUN09 HWON SPEC-39434 - SUPPORT MULTI-COMMENT MKGD REJECTIONS               
*         HWON SPEC-45148 - FIX DUMPS CAUSED BY OPTIONAL PURPOSE CODES          
*                                                                               
* 19MAR15 HWON SPEC-29067 - DON'T ALLOW APPROVAL IF INVALID UPGRADE.            
*         HWON DON'T TEST PAID IF MAKEGOOD STATUS IS OKAYED/ONHOLD.             
*                                                                               
* 18NOV07 MZEI SPEC-17258 - SUPPORT PREVIOUS REJECTION COMMENTS                 
*                                                                               
* 30MAY15 HWON SPEC-20019 DON'T ALLOW -S PROGRAM NAME IF NOT AUTHORIZED         
*                                                                               
* 14JAN01 MHER READ CABLE MKO/MKN RECS FROM XSPFIL                              
*                                                                               
* 13OCT06 HWON FIX MATCHMS CODE TO NOT LOOK FOR ZERO BUYLINE                    
*                                                                               
* 07FEB27 MHER LET BASE LOAD T21132 OVER T21131 AND RESTORE                     
*              SO CALL 32 VIA GOBUY32                                           
*                                                                               
* 06JAN07 MHER  ORBITUARIES                                                     
*                                                                               
* 05NOV14 SUPPORT 2-DEC DEMOS FOR PERCENTAGE UPGRADE FORMULAS                   
*                                                                               
**DDMMMYY**                                                                     
* 05APR04 HWON SELF APPLY MAKEGOODS (SVDARPRF+13)                               
*                                                                               
* 21JAN04 ALLOW USER TO ADD BUYER COMMENTS AFTER ERROR                          
*                                                                               
* 21OCT03 HWON ALLOW MAKEGOODS WITH EARNED DISCOUNTS (SVDARPRF+12)              
*                                                                               
* 22OCT03 WHOA TEST IF PAID SPOT IS ALREADY MADEGOOD                            
*                                                                               
* 11AUG03 MHER ALLOW MAKEGOODS IN PAID MONTHS  (SVDARPRF+10)                    
*            -OR- ALLOWED TO MISS PAID SPOTS   (SVDARPRF+11) ONHOLD             
*                                                                               
* 20JUL12 HWON UPDATE TO USE ONLY DOSTELD ELEMENT                               
*===============================================================                
                                                                                
T21131   TITLE 'SPBUY31 - SPOTPAK BUY - MGE DISPLAY'                            
T21131   CSECT                                                                  
                                                                                
         PRINT NOGEN                                                            
*&&ONLIN SET   Y                                                                
PFKEYAPP EQU   4                   MGEAPP                                       
PFKSKED  EQU   5                   SPOTS/WEEK                                   
PFKGRID  EQU   6                   CHANGE IN SPOTS/WEEK                         
PFKDEM   EQU   7                   6 DEMOS                                      
PFKCPP   EQU   8                   6 DEMOS/CPP                                  
PFKEYREJ EQU   9                   MGEREJ                                       
PFKEYUNP EQU   10                  RESET MGPENDING                              
PFKEYSAP EQU   11                  SELF OK                                      
*                                                                               
         NMOD1 BSPOOLX-BSPOOLD,T21131,RR=R7,CLEAR=YES                           
*                                                                               
         LR    R8,RC                                                            
         USING BSPOOLD,R8          LOCAL WORKING STORAGE                        
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         XC    BUYMSG,BUYMSG                                                    
*                                                                               
         C     R7,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R7,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
         B     HAVERELO                                                         
*                                                                               
RELO     DS    A                                                                
*                                                                               
HAVERELO OI    WRKRUPSW,WRKRUPSW_ISDRMG                                         
*                                                                               
* RESTORE PREVIOUS MGWORK DATA TO ADDRESSABLE STORAGE IN BSPOOL                 
*                                                                               
         LHI   RE,SVMGSAVE-BUYSAVE   RESTORE MGE DATA                           
         AR    RE,RA                                                            
         LHI   RF,SVMGSAVX-SVMGSAVE                                             
         LA    R0,MGSAVE                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
***NOTE***                                                                      
* DATADISP IS DIFFERENT FOR BCAST (SPOT) VS CABLE (XSPOT) MKGDS                 
*   BE CAREFUL WHEN USING GETEL FOR NON MG RECORDS                              
***NOTE***                                                                      
         LA    R0,MNRFRST-MNKEY    SET DSPL TO FIRST DARE ELEM                  
         CLI   BUYST,C'0'                                                       
         BL    *+8                                                              
         LA    R0,MNXFRST-MNKEY                                                 
****     STH   R0,DSPFRST                                                       
         STH   R0,DATADISP                                                      
*                                                                               
         CLI   MGROUTE,C' '        TEST HAVE ROUTING CODE YET                   
         BH    B010                                                             
         BRAS  RE,GETROUTE                                                      
**       BE    B010                WE HAVE A ROUTING CODE                       
**       MVC   NERRCD,=Y(NOROUTE)  777 - MISSING ROUTE ON IDI RECORD            
**       J     ERREXITN                                                         
*                                                                               
B010     MVI   MGFLAG,0            RESET                                        
         MVI   MGNOMSG,C'N'        RESET                                        
         MVC   QDMTRACE,=CL8'DMTRACE'   AVOID LITERALS IN CALLTSAR              
         MVC   QDMDATA,=CL8'DATA'                                               
*                                                                               
         CLC   =C'MGE',SVBUTRCD    HAVE THEY BEEN ELSEWHERE ?                   
         BE    B020                NO                                           
         CLC   =C'BGE',SVBUTRCD    EDIT ROUTINES MAY CHANGE IT !                
         BE    B020                                                             
         CLC   =C'CGE',SVBUTRCD                                                 
         BE    B020                                                             
         MVI   SVMGINIT,C'N'       NO, SET TO NEW                               
*                                                                               
B020     CLI   SVMGINIT,C'E'       TEST HEADLINE CHANGE                         
         BE    B040                NO                                           
         MVI   MGMSCFLG,0                                                       
         NI    MGEINP1H+4,X'DF'    FORCE REVALIDATION                           
         XC    MGLSTKEY,MGLSTKEY   INVALIDATE PREVIOUS                          
         XC    MGLSTGRP,MGLSTGRP                                                
* HEADLINE HAS CHANGED - GET DAR AND OM PROFILES                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SDAR'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         MVC   WORK+16(16),WORK    SAVE KEY                                     
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVDARPRF,VDATAMGR                                 
*                                                                               
         CLI   SVDARPRF+13,C'N'    SELF-APPLY ON?                               
         BNE   B030                                                             
         XC    MGEPF3,MGEPF3                                                    
         MVC   MGEPF3(7),=X'C6F17ED9A39995'  F1=RTRN IN LOWER CASE              
         OI    MGEPF3+6,X'80'      TRANSMIT                                     
*                                                                               
B030     MVC   WORK(16),WORK+16                                                 
         MVC   WORK(4),=C'S0OM'    READ OM PROFILE                              
         GOTO1 (RF),DMCB,WORK,SVOMPROF                                          
* HEADLINE HAS CHANGED                                                          
         CLI   SVSCR,X'F3'         TEST HAVE RIGHT SCREEN                       
         BE    B040                                                             
         BRAS  RE,RSTRF3                                                        
         LA    R2,MGEINP1H                                                      
         NI    4(R2),X'DF'         SET NOT VALIDATED                            
*                                                                               
T        USING TSARD,TSARBLK                                                    
B040     MVI   T.TSACTN,TSAINI                                                  
         MVC   T.TSACOM,VCOMFACS   A(COMFACS)                                   
         OI    T.TSINDS,TSIXTTWA+TSIALLOC  USE 14K RECORDS AND TEMPEST          
         MVI   T.TSPAGN,20         USE 20 PAGE                                  
         MVI   T.TSKEYL,L'TSARKEY                                               
         LHI   R1,L'TSARREC        RECORD LENGTH                                
         STH   R1,T.TSRECL                                                      
         LA    R1,TSARREC                                                       
         ST    R1,T.TSAREC                                                      
*                                                                               
         CLI   SVMGINIT,C'E'       DID WE ALREADY INITIALIZE TSAR?              
         BNE   *+8                                                              
         MVI   T.TSACTN,TSARES     SET TO RESTORE                               
*                                                                               
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         MVI   SVMGINIT,C'E'       SET 'INITIALIZED' FLAG                       
         OI    T.TSINDS,TSIREUSE   AND ALWAYS SET TSAR TO REUSE                 
*                                                                               
B050     MVI   NEEDTOTS,C'N'                                                    
         TM    MGEINP1H+4,X'20'    INPUT LINE WAS CHANGED?                      
         BO    B060                NO                                           
         MVI   NEEDTOTS,C'Y'                                                    
         MVC   MGTSRNUM,=Y(1)      START AT TOP OF DISPLAY AGAIN                
         MVC   MGTSRFRS,=Y(1)                                                   
         CLI   SVSCR,X'F3'         MAIN SCREEN NEEDS TO BE THERE                
         BE    B090                                                             
         BRAS  RE,RSTRF3                                                        
         NI    MGEINP1H+4,X'DF'    INVALIDATE                                   
*                                                                               
B060     CLI   SVSCR,X'F3'         TEST MAIN SCREEN                             
         BNE   B500                NO - PROCESS SUBSCREEN                       
*                                                                               
B070     CLI   PFKEY,PFKEYAPP      TEST APPROVE PFKEY                           
         BE    B090                YES - GO VALIDATE INPUT LINE                 
         CLI   PFKEY,PFKEYREJ      TEST REJECT PFKEY                            
         BE    B090                YES - VALIDATE INPUT LINE                    
         CLI   PFKEY,PFKEYUNP      TEST UNPEND PFKEY                            
         BE    B090                YES - VALIDATE INPUT LINE                    
*                                                                               
         CLI   SVDARPRF+13,C'N'    PROFILE SET TO SELF-APPLY?                   
         BE    B080                NO                                           
         CLI   PFKEY,PFKEYSAP      YES, TEST SELF APPLY PFKEY                   
         BE    B090                     NO                                      
*                                                                               
B080     BRAS  RE,TESTSEL          TEST ANY SELECT FIELDS ENTERED               
         BNE   B600                YES                                          
         B     B120                GO DISPLAY NEXT                              
*                                                                               
B090     BRAS  RE,VALINP           VALIDATE INPUT AND READ ORDER                
         BE    B100                SAME ORDER/FLIGHT AS PREVIOUS                
*                                                                               
         CLI   MGFLAG,C'J'         TEST REJECT                                  
         BNE   *+12                                                             
         TM    MGMSCFLG,MGTSROVF   FOR REJECT ACTION AND TSAR OVERFLOW          
         BO    B100                 SKIP CHECKS BELOW & ALLOW REJECT            
*                                                                               
         MVI   T.TSACTN,TSAINI     REINITIALIZE TSAR                            
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         XC    MGTSRNUM,MGTSRNUM   RESET                                        
         XC    MGTSRFRS,MGTSRFRS   RESET                                        
*                                                                               
         BRAS  RE,GETDEMNS         GET DEMO NAMES                               
         MVC   MGEHL2+35(6),MGDEMNM1                                            
         OI    MGEHL2H+6,X'80'     FORCE TO XMT                                 
*                                                                               
* REBUILD THE OFFER NOW                                                         
*                                                                               
         BRAS  RE,GETMGSTA         EXTRACT CURRENT STATUS                       
         BRAS  RE,FNDMISS                                                       
         BRAS  RE,GETMKO                                                        
         BRAS  RE,GETSTAT          PUT STATUS ON SCREEN                         
*                                                                               
* READ NOTICE RECORD AND CHECK ACTION VALID FOR STATUS                          
*                                                                               
B100     BRAS  RE,CHKSTAT                                                       
*                                                                               
         CLI   MGFLAG,C'J'         TEST REJECT                                  
         BE    B150                                                             
*                                                                               
         LHI   R0,TSAROVFL                                                      
         STCM  R0,3,NERRCD                                                      
         TM    MGMSCFLG,MGTSROVF   TSAR OVERFLOW                                
         JO    ERREXITK                                                         
*                                                                               
         CLI   MGFLAG,C'A'         TEST APPROVE (ACTION MGEAPP)                 
         BE    B200                                                             
         CLI   MGFLAG,C'S'         TEST SELF-APPLY (ACTION MGESAP)              
         BE    B200                                                             
         CLI   MGFLAG,C'C'         TEST ACCEPT (ACTION MGEACC)                  
         BE    B200                NEED TO BUILD TSAR DATA                      
         CLI   MGFLAG,C'G'         TEST GENERATE (ACTION MGEZZT)                
         BE    B200                NEED TO BUILD TSAR DATA                      
*                                                                               
         CLI   MGFLAG,C'U'         TEST UNPEND (PF10)                           
         BE    B160                                                             
*                                                                               
         CLI   MGFLAG,C'X'         TEST NEXT (SAME ORD/FLT/GRP)                 
         BE    B120                                                             
         CLI   MGFLAG,C'E'         TEST EVAL (NEW OFFER)                        
         BE    B120                                                             
         DC    H'0'                                                             
*                                                                               
*=================                                                              
* EVALUATE OFFER                                                                
*=================                                                              
B110     DS    0H                                                               
         BRAS  RE,FNDMISS          FIND MISSED SPOTS (FROM NOTICE REC)          
         BRAS  RE,GETMKO           GET MAKEGOOD OFFER TOTALS                    
*                                                                               
B120     CLI   SVSCR,X'F3'         TEST HAVE RIGHT SCREEN                       
         BE    *+8                                                              
         BRAS  RE,RSTRF3                                                        
*                                                                               
         CLI   NEEDTOTS,C'Y'                                                    
         BNE   B130                                                             
         MVI   MGFLAG,C'T'         SET FOR NEW TOTALS ONLY                      
         BRAS  RE,GETMKO           GET OFFER TOTALS                             
*                                                                               
B130     BRAS  RE,DISPLAY          DISPLAY THE MAKEGOOD GROUP                   
         B     B180                                                             
         EJECT                                                                  
*===========================                                                    
* PROCESS MAKEGOOD REJECTION                                                    
*===========================                                                    
B150     BRAS  RE,SENDMGRJ         SEND MKGREJ                                  
*                                                                               
         BRAS  RE,REJECTED                                                      
         MVC   BUYMSG(31),0(R1)        SET REJECTED MESSAGE                     
         XC    MGLSTGRP,MGLSTGRP       CLEAR HISTORY                            
         NI    MGEINP1H+4,X'FF'-X'20'  FORCE INPUT LINE EDIT                    
         B     B170                                                             
*                                                                               
*==================================                                             
* PROCESS THE MAKEGOOD UNPEND SPOTS                                             
*==================================                                             
B160     BRAS  RE,SETPNDG              GO CLEAR PENDING BITS                    
         BRAS  RE,UNPENDED                                                      
         MVC   BUYMSG(28),0(R1)        SET UNPENDED MESSAGE                     
         XC    MGLSTGRP,MGLSTGRP       CLEAR HISTORY                            
         NI    MGEINP1H+4,X'FF'-X'20'  FORCE INPUT LINE EDIT                    
*                                                                               
*                                                                               
B170     GOTO1 VDATAMGR,DMCB,(0,=C'COMMIT'),0  COMMIT                           
*                                                                               
B180     BRAS  RE,GETSTAT              SET STATUS ON SCREEN                     
*                                                                               
B190     LHI   RE,SVMGSAVE-BUYSAVE   SAVE MGE DATA                              
         AR    RE,RA                                                            
         LHI   RF,SVMGSAVX-SVMGSAVE                                             
         LA    R0,MGSAVE                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
* SAVE TSAR BUFFERS                                                             
         MVI   T.TSACTN,TSASAV                                                  
         BRAS  RE,CALLTSAR                                                      
         J     EXIT                                                             
*                                                                               
*================================================================               
* ACTION IS APPROVE   - SEND MG APPROVAL TO REP                                 
*        OR ACCEPT    - TO CALL SPBUY32 AND GENERATE MAKEGOOD                   
*        OR GENERATE  - TO CALL SPBUY32 AND GENERATE MAKEGOOD                   
*        OR SELFAPPLY - APPROVE & ACCEPT                                        
*  ALL MISSED SPOTS HAVE BEEN FOUND                                             
*  ALL MAKEGOOD OFFERS HAVE DAYPARTS                                            
*  ALL MAKEGOOD OFFERS HAVE DEMOS                                               
*  EACH MAKEGOOD OFFER HAS LESS THAN MAX SPOTS                                  
*  MAKEGOOD STATUS MUST BE NEW OR AMENDED TO APPROVE OR REJECT                  
*================================================================               
         SPACE 1                                                                
B200     BRAS  RE,TESTMGPD         TEST PAID SPOTS IN OFFER PERIOD              
*                                                                               
         XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
         XC    MGONESTA,MGONESTA                                                
         XC    MGONEMSS,MGONEMSS                                                
         XC    MGONETIM,MGONETIM                                                
*                                                                               
B210     BRAS  RE,CALLTSAR                                                      
*                                                                               
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   B260                                                             
*                                                                               
B220     MVC   MGONEMSS,MSACTBUY   SAVE FIRST MISSED BUYLINE                    
         MVC   MGONESTA,MSSTTN     SAVE STATION!                                
         MVC   MGONETIM,MSSTIM     SAVE START/END TIME                          
         MVC   MGONEDAY,MSDAYS     SAVE DAYS                                    
         MVC   HALF,MSREP          SAVE THE REP                                 
         CLI   SVDARPRF+3,C'Y'     TEST COPY FROM MISSED BUYLINE                
         BE    B230                                                             
         XC    MGONEMSS,MGONEMSS   ELSE SUPPRESS OPTION                         
         XC    MGONESTA,MGONESTA                                                
*                                                                               
B230     MVC   NERRCD,=Y(BADMSSD)                                               
         OC    MSACTBUY,MSACTBUY   BUYLINE FOUND                                
         BZ    B390                NO - DISPLAY THE GROUP                       
         CLC   MGONEMSS,MSACTBUY   TEST SAME MISSED LINE                        
         BE    B250                YES                                          
         MVC   NERRCD,=Y(BADREPCD)                                              
         CLC   MSREP,HALF          SAME REP CODE?                               
         BE    B240                YES                                          
         CLI   SVDARPRF+14,C'Y'    NO, MULTIPLE TRADE CODES?                    
         BNE   B390                NO - DISPLAY THE GROUP                       
         CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   B390                NO                                           
*                                                                               
B240     XC    MGONEMSS,MGONEMSS   ELSE SUPPRESS COPY OPTION                    
         XC    MGONESTA,MGONESTA                                                
*                                                                               
B250     MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   B400                                                             
*                                                                               
         CLI   MSTYPE,MSTYPEQ      TEST MISSED SPOT                             
         BNE   B260                                                             
         OC    MGONETIM,MGONETIM   DID WE SAVE THE FIRST MISSED?                
         BZ    B220                NO, GO SAVE IT                               
         B     B230                                                             
*                                                                               
B260     CLI   MOTYPE,MOTYPEQ      TEST MAKEGOOD                                
         BNE   B250                                                             
         CLI   MOCOMNUM,0          TEST COMMENT                                 
         BNE   B250                                                             
*                                                                               
         CLI   MOERR,0             TEST ANY REP CAUSED ERRORS                   
         BE    B270                                                             
         MVC   NERRCD,=Y(OUTOFEST) MOERR == X'80'                               
         TM    MOERR,X'80'                                                      
         BO    B390                                                             
         MVC   NERRCD,=Y(SLNERR)   MOERR == X'40'                               
         TM    MOERR,X'40'                                                      
         BO    B390                                                             
         MVC   NERRCD,=Y(OUTOFWK)  MOERR == X'20'                               
         TM    MOERR,X'20'                                                      
         BO    B390                                                             
         MVC   NERRCD,=Y(PBSLNERR) MOERR == X'10'                               
         TM    MOERR,X'10'                                                      
         BO    B390                                                             
         MVC   NERRCD,=Y(BADSPTS)  MOERR == X'08'                               
         TM    MOERR,X'08'                                                      
         BO    B390                                                             
         MVC   NERRCD,=Y(PGMERR)   MOERR == X'04'                               
         B     B390                                                             
*                                                                               
B270     MVC   NERRCD,=Y(MISSDEMO)                                              
         OC    MODEM1(MOPROG-MODEM1),MODEM1                                     
***NOP   BZ    B390                                                             
*                                                                               
B280     MVC   NERRCD,=Y(BADDPT)                                                
         CLI   MODPT,C' '          TEST DAYPART ASSIGNED                        
         BL    B390                                                             
*                                                                               
         TM    SVCOPT4,X'02'       CLT USING EARNED DISCOUNT FOR TRADE?         
         BZ    B300                NO                                           
         CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   B300                NO                                           
         XC    NERRCD,NERRCD                                                    
         MVI   ERRCD,COST2ERR                                                   
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         BO    B290                YES, ALLOW ZERO COS2                         
         OC    MOCOS2,MOCOS2                                                    
         BZ    B390                                                             
B290     MVI   ERRCD,REPERR                                                     
         OC    MOREP,MOREP                                                      
         BZ    B390                                                             
*                                                                               
B300     CLIY  SVB0PROF+9,C'O'     OPTIONAL PURPOSE CODES                       
         JE    B310                YES, DON'T GIVE ERROR IF NO PURPCOD          
         CLIY  SVB0PROF+9,C'Y'     TEST USING PURPOSE CODES                     
         BNE   B310                                                             
         MVC   NERRCD,=Y(NOPURCOD)                                              
         TM    MOFLAGS,X'80'       TEST PURPOSE CODE IN RECORD                  
         BZ    B390                NO - ERROR                                   
*                                                                               
B310     CLIY  SVB0PROF+7,C'1'     TEST REASON CODE SCHEME 1                    
         BNE   B320                NO                                           
         MVC   NERRCD,=Y(NORSN)                                                 
         CLI   MOSEQNUM,1          TEST SEQNUM 1                                
         BNE   B320                NO - CODE ONLY ON SEQ 1 FOR OFFER            
         TM    MOFLAGS,X'40'       TEST REASON CODE IN RECORD                   
         BZ    B390                NO - ERROR                                   
*                                                                               
B320     MVC   NERRCD,=Y(BADSPTS)                                               
         LLC   R0,MOWKS                                                         
         LLC   R1,MONPW                                                         
         MR    R0,R0                                                            
         IC    R0,SVMAXSPT                                                      
         CR    R1,R0               MORE THAN 208 SPOTS?                         
         BNH   B330                 NO                                          
         B     B390                 YES, ERROR                                  
*                                                                               
B330     TM    MOFLAGS,X'20'       TEST UPGRADE IN RECORD                       
         BZ    B250                                                             
         LA    R6,SVDEMOS          POINT TO DEMOS IN DISPLAYED ORDER            
         LA    R0,L'SVDEMLST(R6)   GET ADDR OF END OF DEMO LIST                 
         OC    SVBRDEMS,SVBRDEMS   HAVE BRAND DEMO LIST?                        
         BZ    B332                 NO                                          
         LA    R6,SVBRDEMS         POINT TO DEMOS IN DISPLAYED ORDER            
         LA    R0,L'SVBRDEMS(R6)   GET ADDR OF END OF DEMO LIST                 
*                                                                               
B332     CLI   2(R6),0             CHECK IF ESTIMATE HAS NSI DEMOS              
         BNE   B334                 YES, THEN TEST VALID MG UPG                 
         LA    R6,3(R6)            NEXT DEMO IN ELEMENT                         
         CR    R6,R0               EOL?                                         
         BNL   B250                 YES, CAN EXIT                               
         OC    0(3,R6),0(R6)       ANY MORE DEMOS?                              
         BNZ   B332                 YES, GO BACK                                
         B     B250                                                             
*                                                                               
B334     BRAS  RE,TSTVMGUP         TEST VALID MG UPGRADE                        
         B     B250                 YES                                         
*                                                                               
B390     MVI   PFKEY,3             SET TO DISPLAY 'NEXT'                        
         MVC   MGTSRNUM,T.TSRNUM   SET STARTING RECNUM                          
         BRAS  RE,DISPLAY          DISPLAY FROM FIRST LINE IN ERROR             
         OC    NERRCD,NERRCD       TWO BYTE ERROR CODE?                         
         JNZ   ERREXITN            YES                                          
         J     ERREXIT                                                          
*                                                                               
*========================================                                       
* PROCESS APPROVE OR SELFAPPLY                                                  
*========================================                                       
B400     CLI   MGFLAG,C'A'         TEST APPROVE                                 
         BE    B410                 YES                                         
*                                                                               
         CLI   MGFLAG,C'S'         TEST SELF APPLY AND                          
         BNE   B460                                                             
         CLI   SVMNSTAT,MNSTNEW    TEST STATUS NEW OR                           
         BE    B410                 YES                                         
         CLI   SVMNSTAT,MNSTAMND   AMEND STATUS?                                
         BE    B410                 YES                                         
*                                                                               
         CLI   SVMNSTAT,MNSTAPP    TEST STATUS APPROVED?                        
         BE    B460                 YES, CALL SPBUY32 & GENERATE MKGD           
         MVC   NERRCD,=Y(BADMGEPF)                                              
         J     ERREXITN                                                         
*                                                                               
B410     BRAS  RE,SENDMGAP         SEND MKGAPP                                  
*                                                                               
         CLI   MGFLAG,C'S'             SELF APPLY                               
         BE    B450                    YES                                      
         BRAS  RE,ONITSWAY             POINT R1 TO MESSAGE                      
         MVC   BUYMSG(28),0(R1)                                                 
         XC    MGLSTGRP,MGLSTGRP       CLEAR HISTORY                            
         NI    MGEINP1H+4,X'FF'-X'20'  FORCE INPUT LINE EDIT                    
         B     B170                                                             
*                                                                               
*=================================================================              
* PROCESS THE MAKEGOOD APPLY ACTION                                             
*=================================================================              
*  REQUIRED TO COMMIT THE APPROVAL STEP OF THE SELF-APPLY                       
B450     GOTO1 VDATAMGR,DMCB,(0,=C'COMMIT'),0  COMMIT                           
*                                                                               
B460     MVC   SVPASSWD,MGPASSWD   SET SAVED PASSWORD FROM ORDER                
*                                                                               
         L     RF,VGOBUY32                                                      
         GOTO1 (RF),DMCB,(RC),(R8)   CALL SPBUY32                               
*                                                                               
* UPDATE THE MAKEGOOD COLOR ELEMENT                                             
*                                                                               
         SPACE 1                                                                
         SR    R6,R6               TELL BLDSTAT NO INSERT                       
         MVI   BYTE,MNSTOKAY                                                    
         TM    MGMSCFLG,MGMFSAPP   SELL APPLY?                                  
         BZ    *+8                                                              
         MVI   BYTE,MNSTSAPP                                                    
         MVI   BYTE2,0                                                          
         BRAS  RE,BLDSTAT          BUILD MKN STATUS ELEMENT                     
         BRAS  RE,BLDCOLOR         NOW UPDATE STATUS IN ORDER                   
*                                                                               
         LA    R2,BUYINP1H                                                      
         MVC   WORK(10),8(R2)      SAVE INPUT                                   
         MVI   8(R2),C'*'                                                       
         MVC   9(10,R2),WORK       AND RESTORE INPUT WITH * PRECEDING           
         OI    6(R2),X'80'         AND XMT                                      
         J     EXIT                EXIT                                         
         EJECT                                                                  
*=================================================================              
* INPUT IS NOT DIRECTED TO MGE SCREEN (F3)                                      
* SELECT OF MKO (MAKEGOOD LINE) GOES TO F4 (OR F2 FOR ORBIT DATA)               
* SELECT OF MKN (MISSED SPOT) GOES TO F5                                        
* F8 (SKEVAL) HAS ITS OWN PFKEYS EXCEPT FOR 12 - RETURN TO 31                   
*=================================================================              
         SPACE 1                                                                
B500     CLI   SVSCR,X'F8'         TEST SKEVAL SCREEN LOADED                    
         BNE   B510                                                             
         CLI   PFKEY,12                                                         
         BE    B520                                                             
         BRAS  RE,GOSKEVAL                                                      
         B     B190                SAVE BUFFERS AND EXIT                        
*                                                                               
B510     CLI   PFKEY,12            TEST RETURN                                  
         BNE   B550                                                             
         OC    MGSELECT,MGSELECT   TEST ANY MORE SELECTS                        
         BNZ   B600                YES - GO PROCESS                             
*                                                                               
B520     BRAS  RE,RSTRF3           RESTORE F3 SCREEN                            
*                                                                               
         MVI   MGFLAG,C'T'         ASK FOR NEW TOTALS                           
         BRAS  RE,GETMKO                                                        
         MVC   MGTSRNUM,MGTSRFRS   SET TO DISPLAY PREVIOUS SCREEN               
         BRAS  RE,DISPLAY          WITH UPDATED TOTALS                          
         B     B180                                                             
         SPACE 1                                                                
*==========================================================                     
* PASS CONTROL TO APPROPRIATE EDIT ROUTINE                                      
*==========================================================                     
*                                                                               
B550     CLI   SVSCR,X'F4'         TEST MKO SEL                                 
         BNE   B560                                                             
         CLI   PFKEY,10            TEST ADD BUYER COMMENTS                      
         BE    B560                                                             
         CLI   PFKEY,11            TEST TOGGLE COMMENTS                         
         BE    B660                                                             
*                                                                               
B560     MVC   T.TSRNUM,MGTSRNUM   SET TSAR RECORD NUMBER                       
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         CLI   SVSCR,X'F4'         TEST MKO                                     
         BE    *+12                                                             
         CLI   SVSCR,X'F2'         OR MKO ORB                                   
         BNE   B570                                                             
*                                                                               
         MVI   MGFLAG,C'4'         SET OFFER CHANGE                             
         BRAS  RE,CHKSTAT          MAKE SURE CAN CHANGE                         
*                                                                               
         BRAS  RE,VALMKO           EDIT THE CHANGE                              
*                                                                               
         MVI   MGNOMSG,C'Y'        SET NO MESSAGE DISPLAY                       
         BRAS  RE,SELMKO           DISPLAY THE UPDATED RECORD                   
         B     B180                                                             
*                                                                               
B570     CLI   SVSCR,X'F5'                                                      
         JNE   *+2                                                              
*                                                                               
         MVI   MGFLAG,C'5'         SET NOTICE CHANGE                            
         BRAS  RE,CHKSTAT          MAKE SURE CAN CHANGE                         
*                                                                               
         BRAS  RE,VALMKN                                                        
         CLI   MGFLAG,C'E'         TEST REDISPLAY                               
         BE    B110                GO DO IT                                     
         B     B180                SAVE STORAGE/TSAR                            
         EJECT                                                                  
*==========================================================                     
* PROCESS THE NEXT SELECTION ON MAIN (F3) SCREEN                                
*==========================================================                     
B600     CLI   SVMNSTAT,MNSTNEW    TEST NEW MG?                                 
         BE    *+12                                                             
         CLI   SVMNSTAT,MNSTAMND   OR AMENDED MG?                               
         BNE   B610                 NO                                          
         BRAS  RE,TESTMGPD         WILL EXIT IF OFFER CAN'T BE APP'D            
*                                                                               
B610     LA    R4,MGSELECT         PROCESS NEXT SELECT                          
         LA    R0,L'MGSELECT                                                    
*                                                                               
B620     CLI   0(R4),C'S'                                                       
         BE    B630                                                             
         CLI   0(R4),C'O'          TEST ORBIT CHANGE                            
         BE    B630                                                             
         LA    R4,1(R4)                                                         
         BCT   R0,B620                                                          
         B     B520                GO RESTORE F3 SCREEN                         
*                                                                               
B630     MVC   SVSELECT,0(R4)      SAVE SELECT VALUE                            
         MVI   0(R4),C'X'          SET PROCESSED FLAG                           
         LA    R0,MGSELECT                                                      
         SR    R4,R0               GIVES LINE NUMBER                            
         LH    R0,MGTSRFRS         FIRST TSAR RECNUM                            
         AR    R0,R4               GIVES SELECTED TSAR RECNUM                   
*                                                                               
         STH   R0,T.TSRNUM                                                      
         MVC   MGTSRNUM,T.TSRNUM   AND SAVE FOR LATER                           
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
*                                                                               
         CLI   MSTYPE,MOTYPEQ      TEST OFFER                                   
         BNE   B640                                                             
         MVI   BYTE,X'F4'                                                       
         CLI   SVSELECT,C'S'       NORMAL SELECT                                
         BE    B650                                                             
         MVI   BYTE,X'F2'          ORBIT SELECT                                 
         CLI   SVSELECT,C'O'                                                    
         BE    B650                                                             
         DC    H'0'                                                             
*                                                                               
B640     MVC   NERRCD,=Y(BADSEL)                                                
         CLI   SVSELECT,C'S'       NORMAL SELECT ONLY                           
         JNE   ERREXITK                                                         
         MVI   BYTE,X'F5'                                                       
         CLI   MSTYPE,MSTYPEQ      TEST MISSED                                  
         BE    B650                                                             
         DC    H'0'                                                             
                                                                                
         SPACE 1                                                                
*==================================================                             
* DISPLAY THE SELECTED LINE                                                     
*==================================================                             
         SPACE 1                                                                
B650     CLC   SVSCR,BYTE          TEST HAVE RIGHT SCREEN                       
         BE    B660                YES                                          
*                                                                               
         MVC   ELEM(1),MGEINP1H+5  SAVE INPUT LINE LENGTH                       
         MVC   ELEM+1(78),MGEINP1  SAVE DATA ON INPUT LINE                      
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9021100'                                           
         MVC   DMCB+7(1),BYTE                                                   
         GOTO1 VCALLOV,DMCB,BUYHL1H                                             
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   SVSCR,BYTE                                                       
*                                                                               
         LA    R2,MGEINP1H                                                      
         OI    4(R2),X'20'         SET PREV VALIDATED                           
         MVC   5(1,R2),ELEM        RESTORE INPUT LENGTH                         
         MVC   8(78,R2),ELEM+1     RESTORE DATA                                 
*                                                                               
B660     CLI   SVSCR,X'F4'         MKO SELECT                                   
         BE    *+12                                                             
         CLI   SVSCR,X'F2'         OR MKO ORBIT SELECT                          
         BNE   B670                                                             
         BRAS  RE,SELMKO                                                        
         B     B180                SAVE STORAGE/TSAR                            
*                                                                               
B670     CLI   SVSCR,X'F5'                                                      
         JNE   *+2                 DIE!                                         
         BRAS  RE,SELMKN                                                        
         B     B180                SAVE STORAGE/TSAR                            
         EJECT                                                                  
*=================================================================              
* TEST ANY SELECT FIELDS CHOSEN ON MGE SCREEN AND SAVE IN MGSELECT              
*=================================================================              
         SPACE 1                                                                
TESTSEL  NTR1                                                                   
         XC    MGSELECT,MGSELECT   CLEAR PREVIOUS SELECT LIST                   
         LA    R1,MGSELECT                                                      
*                                                                               
         LA    R2,MGESEL1H                                                      
         USING LINED,R2                                                         
*                                                                               
TESTSEL2 MVC   NERRCD,=Y(BADSEL)                                                
         CLI   LINSELH+5,0         TEST NO INPUT                                
         BE    TESTSEL4                                                         
         CLI   LINSELH+5,1         TEST INPUT LEN 1                             
         JNE   ERREXITN                                                         
         CLI   LINSEL,C'S'         TEST 'S'                                     
         BE    TESTSEL4                                                         
         CLI   LINSEL,C'O'         ORBIT INPUT                                  
         JNE   ERREXITK                                                         
*                                                                               
TESTSEL4 MVC   0(1,R1),LINSEL                                                   
         LA    R1,1(R1)                                                         
         LA    R2,LINNEXT                                                       
         LA    R0,MGESELL          LAST SELECT FIELD                            
         CR    R2,R0                                                            
         BL    TESTSEL2                                                         
         OC    MGSELECT,MGSELECT   SET COND CODE ON EXIT                        
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* DISPLAYS THE MAKEGOOD GROUP                                                   
* DISPLAY SEQUENCE IS                                                           
*   *MGE=XX ON BUY INPUT LINE                                                   
*   GROUP COMMENT                                                               
*   MISSED LINES                                                                
*   OFFERED LINES                                                               
*====================================================================           
         SPACE 1                                                                
DISPLAY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVSCR,X'F3'         MAKE SURE HAVE RIGHT SCREEN                  
         BE    *+8                                                              
         BRAS  RE,RSTRF3                                                        
*                                                                               
         TWAXC MGESEL1H,MGELINLH,PROT=Y   CLEAR ALL DISPLAY LINES               
         LA    R2,MGELAST                                                       
         MVI   1(R2),X'01'                                                      
         MVI   2(R2),X'01'                                                      
* UNPROTECT ALL THE SELECT FIELDS                                               
         LA    R2,MGESEL1H                                                      
         USING LINED,R2                                                         
*                                                                               
DSP2     NI    LINSELH+1,X'FF'-X'20'  SEL FIELD - UNPROTECT                     
         NI    LINSELH+6,X'FF'-X'08'  SEL FIELD - NORMAL INT                    
         OI    LINDSPH+1,X'20'        DATA FIELD - PROTECT                      
         NI    LINDSPH+6,X'FF'-X'08'  DATA FIELD - NORMAL INT                   
         OI    LINSELH+6,X'80'        FORCE XMT                                 
         OI    LINDSPH+6,X'80'        FORCE XMT                                 
         LA    R2,LINNEXT                                                       
         LA    R0,MGESELLH                                                      
         CR    R2,R0                                                            
         BNH   DSP2                                                             
*                                                                               
         LA    R2,MGESEL1H         R2 = FIRST DISPLAY LINE                      
         CLI   PFKEY,0             ENTER KEY LEAVES YOU ALONE                   
         BNE   DSP4                                                             
         SR    R0,R0                                                            
         ICM   R0,3,MGTSRFRS                                                    
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         STH   R0,MGTSRNUM                                                      
         B     DSP30                                                            
*                                                                               
DSP4     ICM   R0,3,MGTSRNUM       DON'T ALLOW RECNUM = 0                       
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         STCM  R0,3,MGTSRNUM                                                    
*                                                                               
         CLI   PFKEY,1             GET TO THE TOP                               
         BE    DSP20                                                            
*                                                                               
         CLI   PFKEY,2             GO BACK                                      
         BE    DSP10                                                            
*                                                                               
         CLI   PFKEY,12            PF12 = RESTORE/REDISPLAY                     
         BE    DSP30                                                            
*                                                                               
         CLI   PFKEY,3             NEXT                                         
         BE    DSP30                                                            
*                                                                               
         CLI   PFKEY,PFKDEM        DEMO DISPLAY                                 
         BE    DSPDEM                                                           
         CLI   PFKEY,PFKCPP                                                     
         BE    DSPCPP                                                           
         CLI   PFKEY,PFKSKED                                                    
         BE    DSPSKE                                                           
         CLI   PFKEY,PFKGRID                                                    
         BE    DSPSKE                                                           
         B     DSP4ERR                                                          
*                                                                               
DSPSKE   BRAS  RE,GOSKEVAL                                                      
         B     DSPXIT                                                           
*                                                                               
DSPDEM   CLI   DSPFMT,C'D'         TEST DEMO FMT NOW                            
         BE    DSPDEM2                                                          
         MVI   DSPFMT,C'D'                                                      
         B     DSPDEM10                                                         
*                                                                               
DSPCPP   CLI   DSPFMT,C'C'         TEST CPP FMT NOW                             
         BE    DSPDEM2             YES - BACK TO STD                            
         MVI   DSPFMT,C'C'         ELSE SET FOR CPP                             
         B     DSPDEM10                                                         
*                                                                               
DSPDEM2  MVI   DSPFMT,0            CLEAR DEMO FORMAT                            
         BRAS  RE,STDHL1                                                        
         MVC   MGEHL1(38),0(R1)                                                 
         OI    MGEHL1H+6,X'80'                                                  
         BRAS  RE,STDHL2                                                        
         MVC   MGEHL2(41),0(R1)                                                 
         MVC   MGEHL2+35(6),MGDEMNM1                                            
         OI    MGEHL2H+6,X'80'                                                  
         B     DSP10                                                            
*                                                                               
DSPDEM10 BRAS  RE,DEMHL1                                                        
         MVC   MGEHL1(38),0(R1)                                                 
         OI    MGEHL1H+6,X'80'                                                  
         MVC   MGEHL2(41),SPACES                                                
         OI    MGEHL2H+6,X'80'                                                  
*                                                                               
         LA    R1,MGEHL2                                                        
         LA    R0,6                                                             
         LA    RE,MGDEMNM1                                                      
*                                                                               
DSP8     MVC   0(6,R1),0(RE)                                                    
         LA    R1,7(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   R0,DSP8                                                          
         B     DSP10                                                            
*                                                                               
DSP4ERR  MVC   NERRCD,=Y(BADMGEPF)                                              
         CLI   PFKEY,0             TEST THEY HIT A PFKEY                        
         BNE   *+10                                                             
         MVC   NERRCD,=Y(BADINPUT)                                              
         J     ERREXITN                                                         
*                                                                               
DSP10    SR    R0,R0                                                            
         ICM   R0,3,MGTSRFRS       GET FIRST LINE CURRENT PAGE                  
         AHI   R0,-(NUMLINES-1)                                                 
         BNP   DSP20               IF NOT POSITIVE, START AT 1 !                
         STH   R0,MGTSRFRS         SET FIRST LINE CURRENT PAGE                  
         STH   R0,MGTSRNUM         AND SET TO START READING THERE               
         B     DSP30                                                            
         SPACE 1                                                                
DSP20    MVC   MGTSRNUM,=Y(1)      SET TO READ FIRST TSAR REC                   
*                                                                               
DSP30    BRAS  RE,DSPTOTS          DISPLAY TOTAL LINES                          
*                                                                               
         MVI   T.TSACTN,TSAGET     NOW GET FIRST TSAR ENTRY                     
         MVC   T.TSRNUM,MGTSRNUM   SET STARTING TSAR RECNUM                     
         MVC   MGTSRFRS,MGTSRNUM   AND SAVE FIRST RECNUM                        
         BRAS  RE,CALLTSAR                                                      
         BE    DSP41                                                            
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(34),=C'PROCESSING ERROR - PLEASE RE-INPUT'                
         MVI   ERRAREA,X'FF'                                                    
         TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK                                     
         JNZ   *+2                 NO $ABEND FOR SPOT DESKTOP OR MM             
         DC    H'0',C'$ABEND'                                                   
*                                                                               
DSP40    MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         TM    T.TSERRS,X'80'      TEST EOF                                     
         BO    DSPX10              HIT ENTER FOR FIRST                          
*                                                                               
DSP41    CLI   MOTYPE,MOTYPEQ                                                   
         BNE   DSP42                                                            
         CLI   MOCOMNUM,0                                                       
         BE    DSP42                                                            
         CLI   MOCOMNUM,X'10'      TEST ORBIT OR COMMENT                        
         BL    DSP41A                                                           
*                                                                               
         MVC   LINDSP(L'MOCOM),MOCOM  DISPLAY COMMENT DATA                      
         B     DSP41B                                                           
*                                                                               
DSP41A   BRAS  RE,DSPORB           DISPLAY ORBIT DATA                           
*                                                                               
DSP41B   OI    LINSELH+1,X'20'     PROTECT SEL FIELD                            
         B     DSP50                                                            
         SPACE 1                                                                
*===========================================================                    
* SEE IF THE LINE WILL FIT                                                      
*===========================================================                    
         SPACE 1                                                                
DSP42    LA    RE,LINNEXT-LINED(R2)                                             
*                                                                               
         LA    R0,MGESELLH         POINT TO LAST SEL FIELD                      
         CR    RE,R0               REACHED EOS ?                                
         BH    DSPX                YES                                          
*                                                                               
         CLI   MSTYPE,MOTYPEQ      TEST OFFER                                   
         BNE   DSP44                                                            
* FIND HIGHEST COMMENT NUMBER TO MAKE SURE WHOLE OFFER FITS ON SCREEN           
         SR    R4,R4               CLEAR COUNTER                                
         MVC   HALF,T.TSRNUM       SAVE CURRENT TSAR RECNUM                     
*                                                                               
DSP43A   MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   DSP43D                                                           
         CLI   MOCOMNUM,0          IF NO COMMENT NUMBER, DONE                   
         BE    DSP43D                                                           
         CLI   MOCOMNUM,X'10'      TEST ORBIT OR COMMENT                        
         BL    DSP43B              ORBIT                                        
         AHI   R4,1                COMMENT - ADD 1 TO COUNTER                   
         B     DSP43A                                                           
*                                                                               
DSP43B   LA    RE,MOORB                                                         
         LHI   RF,4                MAX ORBITS/REC                               
*                                                                               
DSP43C   CLI   0(RE),0                                                          
         BE    DSP43A                                                           
         AHI   R4,1                                                             
         AHI   RE,L'MOORB                                                       
         BCT   RF,DSP43C                                                        
         B     DSP43A                                                           
*                                                                               
DSP43D   MHI   R4,LINNEXT-LINED                                                 
         AR    R4,R2                                                            
         LA    R0,MGESELLH         POINT TO LAST SEL FIELD                      
         CR    R4,R0               REACHED EOS ?                                
         BH    DSPX                YES                                          
*                                                                               
DSP43X   MVC   T.TSRNUM,HALF       RESTORE RECORD NUMBER                        
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
DSP44    MVC   MGTSRNUM,T.TSRNUM   SAVE LAST DISPLAYED TSAR RECNUM              
         CLI   MSTYPE,MSTYPEQ      TEST MISSED                                  
         BNE   DSP46                                                            
         BRAS  RE,DSPMSS           SHOW THE MISSED ENTRY                        
         JNE   *+2                                                              
         B     DSP50                                                            
*                                                                               
DSP46    CLI   MSTYPE,MOTYPEQ      TEST OFFER                                   
         BNE   DSP48                                                            
         BRAS  RE,DSPOFF           SHOW THE OFFER ENTRY                         
         JNE   *+2                                                              
         B     DSP50                                                            
*                                                                               
DSP48    CLI   MSTYPE,MGCTYPEQ     TEST COMMENT                                 
         JNE   *+2                                                              
         MVC   LINDSP,MGCCOM                                                    
         OI    LINSELH+1,X'20'     PROTECT THE SELECT FIELD                     
*                                                                               
DSP50    LA    R2,LINNEXT          BUMP TO NEXT LINE                            
         B     DSP40               GO FOR THE NEXT ENTRY                        
*                                                                               
DSPX     BRAS  RE,MOREMSG          END OF SCREEN, SAY PF3 FOR NEXT              
         B     DSPX20                                                           
*                                                                               
DSPX10   BRAS  RE,DSPLMSG          SET TO SAY 'USE S FOR SELECT'                
         CLC   MGTSRCNT,=H'12'     TEST ALL RECS FIT ON ONE PAGE                
         BNH   *+8                 YES                                          
         BRAS  RE,NOMORMSG         ELSE SAY USE PF3 FOR FIRST                   
         MVC   MGTSRNUM,=Y(1)      SET TO START AT TOP                          
*                                                                               
DSPX20   MVC   BUYMSG(40),0(R1)                                                 
*                                                                               
DSPX22   LA    R0,MGELINL         LAST INPUT FIELD                              
*                                                                               
DSPX24   CR    R2,R0                                                            
         BH    DSPXIT                                                           
         OI    LINSELH+1,X'20'     PROTECT UNUSED INPUT LINES                   
         LA    R2,LINNEXT                                                       
         B     DSPX24                                                           
*                                                                               
DSPXIT   J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* DISPLAY GROUP TOTALS FOR MISSED AND OFFERED                                   
*==============================================================                 
         SPACE 1                                                                
         USING LINETOTD,R2                                                      
DSPTOTS  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         MVC   T.TSRNUM,=Y(1)      SET TO ADD UP MISSED                         
         MVI   T.TSACTN,TSAGET                                                  
*                                                                               
DSPT2    BRAS  RE,CALLTSAR                                                      
         BNE   DSPT10                                                           
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ      TEST MISSED SPOT                             
         BL    DSPT2                                                            
         BH    DSPT10                                                           
*                                                                               
         L     R0,ELEM                                                          
         AHI   R0,1                EACH MISSED IS ONE SPOT                      
         ST    R0,ELEM                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,MSCOST                                                      
         A     R0,ELEM+4                                                        
         ST    R0,ELEM+4                                                        
* TOTALS FOR 6 DEMOS                                                            
         LA    R1,MSDEM1                                                        
         LA    RE,ELEM+8                                                        
         LA    RF,6                                                             
*                                                                               
DSPT4    ICM   R0,15,0(R1)         SKIP ZERO DEMO VALUES AS IT THROWS           
         BZ    DSPT6                OFF 2-DECIMAL PRECISION                     
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         L     R4,0(RE)                                                         
         N     R4,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         AR    R4,R0                                                            
         TM    0(R1),X'40'                                                      
         BZ    *+8                                                              
         O     R4,=X'40000000'                                                  
         ST    R4,0(RE)                                                         
*                                                                               
DSPT6    LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,DSPT4                                                         
         B     DSPT2                                                            
*                                                                               
DSPT10   LA    R2,MGETOTMH         POINT TO MISSED TOTAL LINE                   
         XC    16(71,R2),16(R2)    LEAVE WORD MISSED                            
         OI    6(R2),X'80'                                                      
         LA    R4,ELEM             POINT TO MISSED TOTS                         
         BAS   RE,FMTTOTS                                                       
*                                                                               
         LA    R2,MGETOTOH         POINT TO OFFERED TOTAL LINE                  
         XC    16(71,R2),16(R2)    LEAVE WORD                                   
         OI    6(R2),X'80'                                                      
         LA    R4,MKOSPTS                                                       
         BAS   RE,FMTTOTS                                                       
         J     EXIT                                                             
*                                                                               
FMTTOTS  MVC   LINTSPTS(5),=X'E29796A3A2'  'SPOTS' W/O USING LOWERCASE          
         L     R0,0(R4)                                                         
         EDIT  (R0),(4,LINTSPTS+7),0,ALIGN=LEFT                                 
*                                                                               
         L     R0,4(R4)                                                         
         EDIT  (R0),(9,LINTDOLS),2,ALIGN=LEFT,FLOAT=$                           
*                                                                               
         CLI   DSPFMT,C'A'                                                      
         BH    FMTTOT10                                                         
*                                                                               
         MVC   LINTPTS(6),MGDEMNM1                                              
*                                                                               
         L     R0,8(R4)                                                         
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    8(R4),X'40'                                                      
         BO    FMTTOT2                                                          
         EDIT  (R0),(6,LINTPTS+7),1                                             
         BR    RE                                                               
*                                                                               
FMTTOT2  EDIT  (R0),(6,LINTPTS+7),2                                             
         BR    RE                                                               
*                                                                               
FMTTOT10 LA    R4,8(R4)            POINT TO FIRST DEMO                          
         LA    RF,6                SET FOR 6 DEMOS                              
         LA    R1,LDMTPTS1+1                                                    
*                                                                               
FMTTOT12 L     R0,0(R4)                                                         
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    0(R4),X'40'                                                      
         BO    FMTTOT14                                                         
         EDIT  (R0),(6,(R1)),1                                                  
         B     FMTTOT16                                                         
*                                                                               
FMTTOT14 EDIT  (R0),(6,(R1)),2                                                  
*                                                                               
FMTTOT16 LA    R4,4(R4)                                                         
         LA    R1,7(R1)                                                         
         CHI   RF,6                                                             
         BNE   *+6                                                              
         BCTR  R1,0                FIRST DEMO HAS 1 SPACE AFTER                 
         BCT   RF,FMTTOT12                                                      
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* DISPLAY ORBIT DATA                                                            
*================================================================               
                                                                                
DSPORB   NTR1  BASE=*,LABEL=*                                                   
         USING LINED,R2                                                         
*                                                                               
         LA    R4,MOORB                                                         
         USING MOORBDAY,R4                                                      
         LHI   R5,4                MAX ORBITS/RECORD                            
*                                                                               
DSPORB2  MVC   LINDATE(7),=C'<ORBIT>'                                           
*                                                                               
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),DMCB,(BYTE,MOORBDAY),LINDAYS                                
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MOORBTIM,LINTIME                                       
*                                                                               
         MVC   LINPROG(7),MOORBPRG                                              
         OC    LINPROG,SPACES                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MOORBDEM                                                    
         N     R0,=X'00003FFF'     DROP FLAGS                                   
         TM    MOORBDEM,X'40'      TEST 2-DEC                                   
         BO    DSPORB10                                                         
         EDIT  (R0),(5,LINDEMO),1                                               
         B     DSPORB20                                                         
*                                                                               
DSPORB10 EDIT  (R0),(5,LINDEMO),2                                               
*                                                                               
DSPORB20 CLI   DSPFMT,C'D'         TEST DEMO FORMAT                             
         BNE   DSPORB22                                                         
*                                                                               
E        USING LDMDTLS,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVC   E.LDMLIN,=C'ORB'                                                 
         MVC   E.LDMDAYS,LINDAYS                                                
         MVC   E.LDMTIME,LINTIME                                                
         MVC   E.LDMPROG(7),LINPROG                                             
         DROP  E                                                                
         MVC   DUB(5),LINDEMO      SAVE FIRST DEMO DISPLAY                      
         XC    LINDSP,LINDSP                                                    
         MVC   LDMDTLS,ELEM                                                     
         MVC   LDMDEM1(5),DUB      RESTORE DEMO                                 
*                                                                               
DSPORB22 OI    LINSELH+1,X'20'     PROTECT SEL FIELD                            
         CLI   L'MOORB(R4),0                                                    
         BE    DSPORBX                                                          
         AHI   R4,L'MOORB                                                       
         LA    R2,LINNEXT                                                       
         BCT   R5,DSPORB2                                                       
*                                                                               
DSPORBX  XIT1  REGS=(R2)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* DISPLAY A MISSED SPOT                                                         
*================================================================               
         SPACE 1                                                                
DSPMSS   NTR1  BASE=*,LABEL=*                                                   
         USING LINED,R2                                                         
*                                                                               
         OI    LINSELH+6,X'08'     HIGHLIGHT SELECT FLD                         
         OI    LINDSPH+6,X'08'     HIGHLIGHT DISPLAY LINE                       
*                                                                               
         CLI   DSPFMT,C'A'                                                      
         BH    DSM1                                                             
*                                                                               
         MVI   LINDATE-1,C'*'      FOLLOWS LINNET                               
         MVI   LINDATE+L'LINDATE,C'*'                                           
         MVI   LINDAYS+L'LINDAYS,C'*'                                           
         MVI   LINNPW+L'LINNPW,C'*'                                             
         MVI   LINTIME+L'LINTIME,C'*'                                           
         MVI   LINDPT+L'LINDPT,C'*'                                             
         MVI   LINSLN+L'LINSLN,C'*'                                             
         MVI   LINPROG+L'LINPROG,C'*'                                           
         MVI   LINCOST+L'LINCOST,C'*'                                           
*                                                                               
DSM1     MVI   LINLIN+1,C'?'                                                    
         SR    R0,R0                                                            
         ICM   R0,3,MSACTBUY                                                    
         BNZ   DSM2                                                             
         NI    LINSELH+1,X'FF'-X'20'        UNPROTECT SEL FIELD                 
         MVC   MSPROG(15),=C' ==NOT FOUND== '                                   
         B     DSM3                                                             
*                                                                               
DSM2     CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   LINLIN+1,C' '       OVERWRITE '?'                                
         UNPK  LINLIN+2(3),DUB                                                  
*                                                                               
DSM3     CLI   BUYST,C'0'                                                       
         BL    DSM4                                                             
         OC    MSACTBUY,MSACTBUY                                                
         BZ    *+10                                                             
         UNPK  LINLIN(3),DUB                                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MSSTTN                                                 
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVI   LINNET-1,C'-'                                                    
         MVC   LINNET,WORK+20        SHOW NETWORK ONLY!                         
*                                                                               
DSM4     GOTO1 VDATCON,DMCB,MSELDTQ,(4,LINDATE)                                 
*                                                                               
         LLC   R1,SVEOWSDY         OUT-OF-WEEK START DAY                        
         SLL   R1,4                                                             
         STC   R1,BYTE                                                          
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),DMCB,(BYTE,MSDAYS),DUB                                      
         MVC   LINDAYS,DUB                                                      
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MSSTIM,LINTIME                                         
*                                                                               
         MVC   LINDPT,MSDPT                                                     
*                                                                               
         LLC   R0,MSSLN                                                         
         EDIT  (R0),(3,LINSLN)                                                  
*                                                                               
         MVC   LINPROG,MSPROG                                                   
* ENTER MSSD IN OPTIONS TO SEE THE ELEMDT/ELEMNO                                
*                                                                               
         LHI   R4,SVDAROPT-BUYSAVE                                              
         AR    R4,RA                                                            
         TM    0(R4),X'80'                                                      
         BZ    DSM6                                                             
         MVI   LINPROG+6,C' '                                                   
         LA    R4,LINPROG+7                                                     
         GOTO1 VDATCON,DMCB,(2,MSACTDAT),(4,(R4))                               
         MVI   5(R4),C'-'                                                       
         LLC   R0,MSACTEL                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R4),DUB                                                      
*                                                                               
DSM6     SR    R0,R0                                                            
         ICM   R0,7,MSCOST                                                      
         EDIT  (R0),(9,LINCOST),2,ALIGN=LEFT                                    
*                                                                               
         CLI   DSPFMT,C'A'                                                      
         BH    DSM10                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,MSDEM1                                                     
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    MSDEM1,X'40'                                                     
         BO    DSM8                                                             
         EDIT  (R0),(5,LINDEMO),1                                               
         J     YES                                                              
*                                                                               
DSM8     EDIT  (R0),(5,LINDEMO),2                                               
         J     YES                                                              
         EJECT                                                                  
*================================================================               
* DEMO DISPLAY FORMAT                                                           
* DSPFMT=D FOR COMPRESSED DETAILS, 6 DEMOS ON RIGHT                             
* DSPFMT=C FOR COMPRESSED DETAILS, 6 CPPS ON RIGHT                              
*================================================================               
         SPACE 1                                                                
E        USING LDMDTLS,ELEM                                                     
DSM10    XC    ELEM,ELEM                                                        
         MVC   E.LDMLIN,LINLIN                                                  
         MVC   E.LDMDAYS,LINDAYS                                                
         MVC   E.LDMTIME,LINTIME                                                
         MVC   E.LDMDPT,LINDPT                                                  
         MVC   E.LDMSLN,LINSLN+1                                                
         MVC   E.LDMPROG,LINPROG                                                
         DROP  E                                                                
         XC    LINDSP,LINDSP                                                    
         MVC   LDMDTLS,ELEM                                                     
* NOW SHOW DEMO VALUES OR CPPS                                                  
         LA    R4,LDMDEM1                                                       
         LA    R5,MSDEM1                                                        
         LHI   RF,6                                                             
*                                                                               
DSM20    CLI   DSPFMT,C'C'         TEST CPP                                     
         BE    DSM30               YES                                          
         SR    R0,R0                                                            
         ICM   R0,15,0(R5)                                                      
         BZ    DSM40                                                            
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    0(R5),X'40'         TEST 2-DEC                                   
         BO    DSM22                                                            
         EDIT  (R0),(5,(R4)),1                                                  
         B     DSM40                                                            
*                                                                               
DSM22    EDIT  (R0),(5,(R4)),2                                                  
         B     DSM40                                                            
*                                                                               
DSM30    ICM   R1,15,0(R5)         POINTS                                       
         N     R1,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    0(R5),X'40'         TEST 2-DEC                                   
         BO    DSM32                                                            
         MHI   R1,10               FORCE IT TO 2-DEC                            
*                                                                               
DSM32    LTR   RE,R1               GET 2-DEC POINTS VALUE                       
         BZ    DSM40               ITS ZERO, SKIP                               
         SR    R1,R1               DISPLAY CPP                                  
         ICM   R1,7,MSCOST         GET DOLLARS (IN PENNIES)                     
         M     R0,=F'200'          X 2 X10 X10                                  
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         CHI   R0,9999             NEED FOUR DIGITS ?                           
         BH    DSM34                                                            
         EDIT  (R0),(5,(R4)),2                                                  
         B     DSM40                                                            
*                                                                               
DSM34    AR    R0,R0               R0 X 2                                       
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(5,(R4)),0,FLOAT=$                                          
*                                                                               
DSM40    LA    R4,7(R4)                                                         
         CHI   RF,6                                                             
         BNE   *+6                                                              
         BCTR  R4,0                FIRST DEMO IS 1 SPACE SHORT                  
         LA    R5,4(R5)                                                         
         BCT   RF,DSM20                                                         
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* BUILD THE HEADLINE FOR THE WEEKLY GRID                                        
*====================================================================           
         SPACE 1                                                                
GRIDHL   NTR1  BASE=*,LABEL=*                                                   
         OI    MGESEL1H+1,X'20'    PROTECT SEL FIELDS 1 AND 2                   
         OI    MGESEL2H+1,X'20'                                                 
         XC    MGELIN1,MGELIN1                                                  
         OI    MGELIN1H+6,X'80'    XMT                                          
         XC    MGELIN2,MGELIN2                                                  
         OI    MGELIN2H+6,X'80'    XMT                                          
*                                                                               
         XC    BUDATA,BUDATA                                                    
         XC    ELEM,ELEM                                                        
*                                                                               
         MVC   WORK(6),SVSTART                                                  
         LA    R4,BUDATA+6                                                      
         LA    R5,14               MAX WEEKS                                    
         LA    R6,ELEM                                                          
*                                                                               
GHL2     GOTO1 VDATCON,DMCB,WORK,(2,(R6))  GET 2BYTE DATES                      
         GOTO1 (RF),(R1),,(4,(R4))  GET MMMDD DATES                             
*                                                                               
         LA    R4,5(R4)                                                         
         LA    R6,2(R6)                                                         
         GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),SVEND                                                    
         BH    GHL10                                                            
         BCT   R5,GHL2                                                          
*                                                                               
* COUNT NUMBER OF WEEKS IN EACH MONTH                                           
*                                                                               
GHL10    LA    R4,BUDATA           WEEK COUNTERS                                
*                                                                               
         LHI   R0,1                                                             
         LA    R1,BUDATA+6         FIRST MMMDD DATE                             
*                                                                               
GHL12    CLC   0(3,R1),5(R1)       NEXT WEEK IN SAME MONTH                      
         BE    GHL16                                                            
*                                                                               
GHL14    STC   R0,0(R4)            SET NUM WEEKS THIS MONTH                     
         SR    R0,R0               RESET COUNTER                                
         CLI   5(R1),0             ANY MORE WEEKS                               
         BE    GHL20               NO                                           
         LA    R4,1(R4)            NEXT COUNTER                                 
*                                                                               
GHL16    LA    R1,5(R1)                                                         
         AHI   R0,1                                                             
         B     GHL12                                                            
*                                                                               
GHL20    LA    R1,MGELIN1          MONTH LINE                                   
*                                                                               
         LA    R4,BUDATA           POINT TO WEEKS/MONTH LIST                    
         LA    R5,BUDATA+6         POINT TO FIRST WEEK                          
*                                                                               
GHL22    SR    RE,RE                                                            
         ICM   RE,1,0(R4)          TEST ANY WEEKS THIS MONTH                    
         BZ    GHL24               NO - DONE                                    
         BCTR  RE,0                                                             
         MHI   RE,L'GHLTAB                                                      
         LA    RE,GHLTAB(RE)       POINT TO TABLE ENTRY                         
         LLC   RF,0(RE)            DATA LEN                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),2(RE)       MOVE IN TITLE                                
*                                                                               
         LLC   RF,1(RE)            GET DSPL TO XXX                              
         AR    RF,R1                                                            
         MVC   0(3,RF),0(R5)       MOVE MONTH OVER XXX                          
*                                                                               
         LLC   RF,0(RE)            GET ENTRY LENGTH FROM TABLE                  
         AR    R1,RF               NEXT POSN IN MONTH LINE                      
*                                                                               
         LLC   R0,0(R4)            NUMBER OF WEEKS THIS MONTH                   
         MHI   R0,5                                                             
         AR    R5,R0               FIRST MMMDD OF NEXT MONTH                    
         LA    R4,1(R4)            NEXT MONTH WEEK COUNT                        
         B     GHL22                                                            
*                                                                               
GHL24    LA    R5,BUDATA+6         POINT TO FIRST WEEK                          
         LA    R1,MGELIN2          POINT TO WEEK LINE                           
*                                                                               
GHL26    MVC   1(2,R1),3(R5)                                                    
         LA    R1,3(R1)                                                         
         LA    R5,5(R5)            NEXT WEEK                                    
         CLI   0(R5),0                                                          
         BNE   GHL26                                                            
         XC    BUDATA,BUDATA                                                    
         J     EXIT                                                             
*                                                                               
GHLTAB   DS    0CL17               LEN TO MOVE, DSPL TO XXX                     
         DC    AL1(3,0),CL15'XXX'                                               
         DC    AL1(6,2),CL15' -XXX-'                                            
         DC    AL1(9,3),CL15' --XXX---'                                         
         DC    AL1(12,5),CL15' ----XXX----'                                     
         DC    AL1(15,6),CL15' -----XXX------'                                  
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* DISPLAY AN OFFER                                                              
*================================================================               
         SPACE 1                                                                
DSPOFF   NTR1  BASE=*,LABEL=*                                                   
         USING LINED,R2                                                         
*                                                                               
         MVI   LINLIN+L'LINLIN+1,C'*'                                           
         CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BL    DSPOFF2                                                          
         MVI   LINLIN,C'*'                                                      
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MOSTTN                                                 
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVC   LINLIN+1(3),WORK+20   SHOW NETWORK                               
         MVI   LINLIN+4,C'*'                                                    
*                                                                               
DSPOFF2  TM    MOERR,X'80'         TEST PERIOD ERROR                            
         BZ    *+14                                                             
         MVC   LINLIN(3),=C'PER'                                                
         MVI   LINLIN+L'LINLIN,C'!'                                             
*                                                                               
         TM    MOERR,X'40'         TEST SLN ERROR                               
         BZ    *+14                                                             
         MVC   LINLIN(3),=C'SLN'                                                
         MVI   LINLIN+L'LINLIN,C'!'                                             
*                                                                               
         TM    MOERR,X'20'         TEST OOW ERROR                               
         BZ    *+14                                                             
         MVC   LINLIN(3),=C'OOW'                                                
         MVI   LINLIN+L'LINLIN,C'!'                                             
*                                                                               
         TM    MOERR,X'10'         TEST PIGGYBACK SPOT LENGTH ERROR             
         BZ    *+14                                                             
         MVC   LINLIN(3),=C'PLN'                                                
         MVI   LINLIN+L'LINLIN,C'!'                                             
*                                                                               
         TM    MOERR,X'08'         TEST TOO MANY SPOTS ERROR                    
         BZ    *+14                                                             
         MVC   LINLIN(3),=C'SPT'                                                
         MVI   LINLIN+L'LINLIN,C'!'                                             
*                                                                               
         TM    MOERR,X'04'         TEST INVALID PROGRAM NAME                    
         BZ    *+14                                                             
         MVC   LINLIN(3),=C'PGM'                                                
         MVI   LINLIN+L'LINLIN,C'!'                                             
*                                                                               
         CLI   DSPFMT,C'A'                                                      
         BH    DSPOFF4                                                          
         MVI   LINDATE+L'LINDATE,C'*'                                           
         MVI   LINDAYS+L'LINDAYS,C'*'                                           
         MVI   LINNPW+L'LINNPW,C'*'                                             
         MVI   LINTIME+L'LINTIME,C'*'                                           
         MVI   LINDPT+L'LINDPT,C'*'                                             
         MVI   LINSLN+L'LINSLN,C'*'                                             
         MVI   LINPROG+L'LINPROG,C'*'                                           
         MVI   LINCOST+L'LINCOST,C'*'                                           
                                                                                
* IN THIS VERSION, DISPLAY OFFER PERIOD SLIGHTLY TO THE LEFT                    
                                                                                
DSPOFF4  LA    R5,LINDATE-3                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(8,MOSTRT),(4,(R5))                                 
         LA    R5,5(R5)                                                         
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         LLC   R0,MOWKS                                                         
         EDIT  (R0),(2,(R5)),ALIGN=LEFT                                         
         AR    R5,R0                                                            
         MVI   0(R5),C'W'                                                       
*                                                                               
         LLC   R1,MOOOW            OUT-OF-WEEK START DAY                        
         SLL   R1,4                                                             
         STC   R1,BYTE                                                          
**NOP**  OI    BYTE,X'80'          11 BYTE OUTPUT                               
         GOTO1 VDAYUNPK,DMCB,(BYTE,MODAYS),WORK                                 
*                                                                               
         LA    R1,WORK                                                          
         LA    R0,11                                                            
*                                                                               
DSPOFF6  CLI   0(R1),C','          CHANGE COMMAS TO SLASHES                     
         BNE   *+8                                                              
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,DSPOFF6                                                       
*                                                                               
         MVC   LINDAYS,WORK                                                     
*                                                                               
         LLC   R0,MONPW                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINNPW,DUB                                                       
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MOSTIM,LINTIME                                         
*                                                                               
         MVC   LINDPT,MODPT                                                     
*                                                                               
         LLC   R0,MOSLN                                                         
         EDIT  (R0),(3,LINSLN)                                                  
*                                                                               
         MVC   LINPROG,MOPROG                                                   
         OC    MOBUYLIN,MOBUYLIN   TEST BUYLINE WAS CREATED                     
         BZ    DSPOFF10                                                         
         MVC   LINPROG+12(3),SPACES                                             
         MVC   LINPROG+13(2),MOMGGRP                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MOBUYLIN                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINLIN,DUB                                                       
*                                                                               
DSPOFF10 CLI   DSPFMT,C'A'         TEST SPECIAL DISPLAY FORMAT                  
         BH    DSPOFF20                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,MOCOST                                                      
         EDIT  (R0),(9,LINCOST),2,ALIGN=LEFT                                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,MODEM1                                                     
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    MODEM1,X'40'        TEST 2-DEC                                   
         BO    DSPOFF12                                                         
         EDIT  (R0),(5,LINDEMO),1                                               
         B     DSPOFF14                                                         
*                                                                               
DSPOFF12 EDIT  (R0),(5,LINDEMO),2                                               
*                                                                               
DSPOFF14 NI    LINSELH+1,X'FF'-X'20' SEL FIELD - UNPROTECT                      
         OI    LINSELH+6,X'88'       SEL FIELD - XMT/HIGH INT                   
*                                                                               
         CLI   MOSEQNUM,1                                                       
         BE    DSPOFFX                                                          
* PROTECT LINE IF NOT SEQNUM 1                                                  
         MVI   LINLIN+L'LINLIN+1,C'+'   CHANGE * TO + SO IT'S OBVIOUS           
         OI    LINSELH+1,X'20'        SEL FIELD - PROTECT                       
         NI    LINSELH+6,X'FF'-X'08'  SEL FIELD - NORMAL INT                    
         OI    LINDSPH+1,X'20'        DATA FIELD - PROTECT                      
         NI    LINDSPH+6,X'FF'-X'08'  DATA FIELD - NORMAL INT                   
         OI    LINSELH+6,X'80'        FORCE XMT                                 
         OI    LINDSPH+6,X'80'        FORCE XMT                                 
         B     DSPOFFX                                                          
*================================================================               
* DEMO DISPLAY FORMAT                                                           
* DSPFMT=D FOR COMPRESSED DETAILS, 6 DEMOS ON RIGHT                             
*================================================================               
         SPACE 1                                                                
E        USING LDMDTLS,ELEM                                                     
DSPOFF20 XC    ELEM,ELEM                                                        
         MVC   E.LDMLIN,LINLIN                                                  
         CLI   E.LDMLIN,C' '                                                    
         BH    *+10                                                             
         MVC   E.LDMLIN,=C'...'                                                 
         MVC   E.LDMDAYS,LINDAYS                                                
         MVC   E.LDMTIME,LINTIME                                                
         MVC   E.LDMDPT,LINDPT                                                  
         MVC   E.LDMSLN,LINSLN+1                                                
         MVC   E.LDMPROG,LINPROG                                                
         DROP  E                                                                
         XC    LINDSP,LINDSP                                                    
         MVC   LDMDTLS,ELEM                                                     
* NOW SHOW DEMO VALUES                                                          
         LA    R4,LDMDEM1                                                       
         LA    R5,MODEM1                                                        
         LHI   RF,6                                                             
*                                                                               
DSPOFF22 CLI   DSPFMT,C'C'         TEST CPP DISPLAY                             
         BE    DSPOFF30            YES                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,0(R5)                                                      
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         BZ    DSPOFF40                                                         
         TM    0(R5),X'40'                                                      
         BO    DSPOFF24                                                         
         EDIT  (R0),(5,(R4)),1                                                  
         B     DSPOFF40                                                         
*                                                                               
DSPOFF24 EDIT  (R0),(5,(R4)),2                                                  
         B     DSPOFF40                                                         
*                                                                               
DSPOFF30 ICM   R1,15,0(R5)         POINTS                                       
         N     R1,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    0(R5),X'40'         TEST 2-DEC                                   
         BO    DSPOFF32                                                         
         MHI   R1,10               FORCE TO 2-DEC                               
*                                                                               
DSPOFF32 LTR   RE,R1               GET 2-DEC POINTS VALUE                       
         BZ    DSPOFF40            ITS ZERO, SKIP                               
         SR    R1,R1                                                            
         ICM   R1,7,MOCOST         GET DOLLARS (IN PENNIES)                     
         M     R0,=F'200'          X 2 X 10 X 10                                
         DR    R0,RE               GIVES CPP TO 2 DEC                           
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         CHI   R0,9999             TEST FITS IN 5 CHARS                         
         BH    DSPOFF34            NO                                           
         EDIT  (R0),(5,(R4)),2                                                  
         B     DSPOFF40                                                         
*                                                                               
DSPOFF34 AR    R0,R0               R0 X 2                                       
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(5,(R4)),0,FLOAT=$                                          
*                                                                               
DSPOFF40 LA    R4,7(R4)                                                         
         CHI   RF,6                                                             
         BNE   *+6                                                              
         BCTR  R4,0                FIRST DEMO IS 1 SPACE SHORT                  
         LA    R5,4(R5)                                                         
         BCT   RF,DSPOFF22                                                      
*                                                                               
DSPOFFX  J     YES                                                              
         EJECT                                                                  
*=====================================================================*         
* DISPLAY SELECTED MAKEGOOD OFFER                                               
*=====================================================================*         
         SPACE 1                                                                
SELMKO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MGNOMSG,C'Y'        TEST SUPPRESS MESSAGE                        
         BE    SMK2                                                             
         BRAS  RE,RECDISP          POINT R1 TO MESSAGE TEXT                     
         MVC   BUYMSG(42),0(R1)                                                 
*                                                                               
SMK2     CLI   SVSCR,X'F2'         TEST MKO ORB SELECT                          
         BNE   SMK2A                                                            
         TWAXC MORLINH,MORORB8H,TRNS=Y   CLEAR MKO ORB FIELDS                   
         LA    R2,MORLAST                                                       
         MVI   1(R2),X'01'                                                      
         MVI   2(R2),X'01'                                                      
         B     SMK4                                                             
*                                                                               
SMK2A    TWAXC MKOLINH,MKOCOM5H,TRNS=Y   CLEAR ALL UNP FIELDS                   
         LA    R2,MKOLAST                                                       
         MVI   1(R2),X'01'                                                      
         MVI   2(R2),X'01'                                                      
*                                                                               
         OC    MGONEMSS,MGONEMSS   TEST ONLY ONE MISSED BUY                     
         BZ    SMK4                0 MEANS MORE THAN ONE                        
* READ THE ONE MISSED BUYLINE                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         CLI   BUYST,C'0'          TEST CABLE                                   
         BL    *+10                                                             
         MVC   KEY+6(3),MGONESTA   REMEMBER TO USE MISSED STATION!              
         MVC   KEY+11(2),MGONEMSS                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   AREC,AREC1                                                       
         GOTOR GETREC                                                           
*                                                                               
SMK4     MVC   MYSTTN,MOSTTN       SET CABLE STA/NET                            
         BRAS  RE,READMKO          READ THE MAKEGOOD OFFER                      
*                                                                               
         CLI   MOERR,0                                                          
         BE    SMK6                                                             
         MVC   MKOLIN,=C'PER'      INVALID PERIOD                               
         TM    MOERR,X'80'                                                      
         BO    SMK6                                                             
         MVC   MKOLIN,=C'SLN'      INVALID SPOT LENGTH                          
         TM    MOERR,X'40'                                                      
         BO    SMK6                                                             
         MVC   MKOLIN,=C'OOW'      INVALID ROTATOR                              
         TM    MOERR,X'20'                                                      
         BO    SMK6                                                             
         MVC   MKOLIN,=C'PLN'      INVALID PIGGY LENGTH                         
         TM    MOERR,X'10'                                                      
         BO    SMK6                                                             
         MVC   MKOLIN,=C'SPT'      INVALID TOTAL SPOTS                          
         TM    MOERR,X'08'                                                      
         BO    SMK6                                                             
         MVC   MKOLIN,=C'PGM'      INVALID PROGRAM NAME                         
*                                                                               
SMK6     GOTO1 VDATCON,DMCB,(8,MOSTRT),(4,MKODATE)                              
         LA    R5,MKODATE+5                                                     
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         LLC   R0,MOWKS                                                         
         EDIT  (R0),(2,(R5)),ALIGN=LEFT                                         
         AR    R5,R0                                                            
         MVI   0(R5),C'W'                                                       
*                                                                               
         LLC   R1,MOOOW            OUT-OF-WEEK START DAY                        
         SLL   R1,4                                                             
         STC   R1,BYTE                                                          
         GOTO1 VDAYUNPK,DMCB,(BYTE,MODAYS),WORK                                 
*                                                                               
         LA    R1,WORK                                                          
         LA    R0,11                                                            
*                                                                               
SMK11    CLI   0(R1),C','          CHANGE COMMAS TO SLASHES                     
         BNE   *+8                                                              
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,SMK11                                                         
*                                                                               
         MVC   MKODAYS,WORK                                                     
*                                                                               
         LLC   R0,MONPW                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKONPW,DUB                                                       
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MOSTIM,MKOTIME                                         
*                                                                               
         MVC   MKODPT,MODPT                                                     
         CLI   MKODPT,C' '                                                      
         BH    SMK12                                                            
         OC    MGONEMSS,MGONEMSS                                                
         BZ    SMK12                                                            
         L     RE,AREC1            COPY DPT NO MATTER WHAT DAY/TIM              
         MVC   MKODPT,BDDAYPT-BUYREC(RE)                                        
*                                                                               
SMK12    LLC   R0,MOSLN                                                         
         EDIT  (R0),(3,MKOSLN)                                                  
*                                                                               
         MVC   MKOPROG,MOPROG                                                   
         CLI   MKOPROG,C' '                                                     
         BH    SMK14                                                            
         OC    MGONEMSS,MGONEMSS                                                
         BZ    SMK14                                                            
         CLC   MGONEDAY,MODAYS     TEST SAME DAYS                               
         BNE   SMK14                                                            
         CLC   MGONETIM,MOSTIM     TEST SAME TIMES                              
         BNE   SMK14                                                            
         L     RE,AREC1                                                         
         MVC   MKOPROG,BDPROGRM-BUYREC(RE)                                      
*                                                                               
SMK14    SR    R0,R0                                                            
         ICM   R0,7,MOCOST                                                      
         EDIT  (R0),(9,MKOCOST),2,ALIGN=LEFT                                    
*                                                                               
         CLI   SVSCR,X'F2'         TEST MKO ORB                                 
         BNE   SMK14X                                                           
         BAS   RE,SMKORB                                                        
         B     SMK70                                                            
                                                                                
*=========================================================                      
* DISPLAY CLIENT COST/COS2 AND REP CODE IF APPLICABLE                           
*=========================================================                      
                                                                                
SMK14X   TM    SVCOPT4,X'02'       CLT USING EARNED DISCOUNT FOR TRADE?         
         BZ    SMK15B              NO                                           
         CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   SMK15B              NO                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,MOCOS2                                                      
         BNZ   SMK15                                                            
         OC    MGONEMSS,MGONEMSS                                                
         BZ    SMK15                                                            
         CLC   MGONEDAY,MODAYS     TEST SAME DAYS                               
         BNE   SMK15                                                            
         CLC   MGONETIM,MOSTIM     TEST SAME TIMES                              
         BNE   SMK15                                                            
         L     RE,AREC1            COPY COST2 FROM MISSED                       
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST-BUYREC(RE)                                           
*                                                                               
SMK15    EDIT  (R0),(9,MKOCOS2),2,ALIGN=LEFT                                    
*                                                                               
         MVC   HALF,MOREP                                                       
         OC    MOREP,MOREP                                                      
         BNZ   SMK15A                                                           
         OC    MGONEMSS,MGONEMSS                                                
         BZ    SMK18                                                            
         CLC   MGONEDAY,MODAYS     TEST SAME DAYS                               
         BNE   SMK18                                                            
         CLC   MGONETIM,MOSTIM     TEST SAME TIMES                              
         BNE   SMK18                                                            
         MVC   MKORPCD,SVTRDDTA+5                                               
         B     SMK18                                                            
*                                                                               
         L     RE,AREC1            COPY COST2 FROM MISSED                       
         MVC   HALF,BDREP-BUYREC(RE)                                            
SMK15A   LHI   RE,VRCPACK-BUYSAVE  ONLY CHECK THAT FIRST 2 DIGITS MATCH         
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',HALF),MKORPCD                                    
         B     SMK18                                                            
*                                                                               
SMK15B   LA    R2,MKOCS2TH         POINT TO DISCOUNT COST TITLE                 
         XC    MKOCS2T,MKOCS2T     CLEAR IT                                     
         OI    6(R2),X'80'         XMT                                          
         LA    R2,MKORPCTH         POINT TO REP CODE TITLE                      
         XC    MKORPCT,MKORPCT                                                  
         OI    R6(R2),X'80'        XMT                                          
*                                                                               
         LA    R2,MKOCOS2H         POINT TO DISCOUNT COST FIELD                 
         XC    MKOCOS2,MKOCOS2     CLEAR CURRENT VALUE                          
         OI    1(R2),X'20'         PROTECT                                      
         NI    4(R2),X'DF'         INVALIDATE                                   
         OI    6(R2),X'80'         XMT                                          
         LA    R2,MKORPCDH         POINT TO DISCOUNT COST FIELD                 
         XC    MKORPCD,MKORPCD     CLEAR CURRENT VALUE                          
         OI    1(R2),X'20'         PROTECT                                      
         NI    4(R2),X'DF'         INVALIDATE                                   
         OI    6(R2),X'80'         XMT                                          
         SPACE 1                                                                
*=========================================================                      
* DISPLAY ADJACENCY CODE IF APPLICABLE                                          
*=========================================================                      
         SPACE 1                                                                
SMK18    CLI   SVCPROF+9,C'0'      TEST ADJ CODE REQD                           
         BNE   SMK18B              YES                                          
         MVC   MKOHL2+75(5),SPACES                                              
         OI    MGEHL2H+6,X'80'     TRANSMIT HEADLINE                            
         OI    MKOADJH+1,X'20'     PROTECT ADJ CODE FIELD                       
         OI    MKOADJH+4,X'20'     SET VALIDATED                                
         B     SMK19                                                            
*                                                                               
SMK18B   MVC   MKOADJ(1),MOADJ                                                  
         CLI   MKOADJ,0            ANY VALUE THERE ?                            
         BH    SMK18C                                                           
         OC    MGONEMSS,MGONEMSS                                                
         BZ    SMK18C                                                           
         CLC   MGONEDAY,MODAYS     TEST SAME DAYS                               
         BNE   SMK18C                                                           
         CLC   MGONETIM,MOSTIM     TEST SAME TIMES                              
         BNE   SMK18C                                                           
         L     RE,AREC1                                                         
         MVC   MKOADJ(1),BDPROGT-BUYREC(RE)                                     
*                                                                               
SMK18C   MVI   MKOADJ+1,C' '                                                    
         CLI   SVCPROF+9,C'1'      TEST ALPHA ADJ                               
         BE    SMK19               YES                                          
         LLC   R0,MKOADJ                                                        
         SRDL  R0,4                                                             
         STC   R0,HALF                                                          
         OI    HALF,X'F0'                                                       
         SRL   R1,28                                                            
         STC   R1,HALF+1                                                        
         OI    HALF+1,X'F0'                                                     
         MVC   MKOADJ,HALF                                                      
         EJECT                                                                  
*=========================================================                      
* DISPLAY PURPOSE CODE IF PRESENT                                               
*=========================================================                      
         SPACE 1                                                                
SMK19    LA    R2,MKOPURH          POINT TO PURPOSE CODE FIELD                  
         XC    MKOPUR,MKOPUR       CLEAR CURRENT VALUE                          
         OI    1(R2),X'20'         PROTECT                                      
         NI    4(R2),X'DF'         INVALIDATE                                   
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         CLIY  SVB0PROF+9,C'O'     OPTIONAL PURPOSE CODES                       
         JE    *+14                STILL CAN DISPLAY IF AVAILABLE               
         CLIY  SVB0PROF+9,C'Y'     TEST USING PURPOSE CODES                     
         BNE   SMK20                                                            
         NI    1(R2),X'DF'         UNPROTECT INPUT FIELD                        
*                                                                               
SMK19A   L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOPRPELQ     CHECK FOR X'63' PURPOSE CODE ELEM            
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
         USING MOPRPELD,R6                                                      
SMK19B   BRAS  RE,NEXTEL                                                        
         BNE   SMK20                                                            
         CLC   MORECID,MOPRPOFR    MATCH OFFER/REC #                            
         BNE   SMK19B                                                           
         MVC   MKOPUR,MOPRPCOD     MOVE PURPOSE CODE                            
         OI    MKOPURH+4,X'20'     SET CODE VALIDATED                           
         DROP  R6                                                               
         SPACE 1                                                                
*=========================================================                      
* DISPLAY REASON CODE IF APPLICABLE                                             
*=========================================================                      
         SPACE 1                                                                
SMK20    LA    R2,MKORSCTH         POINT TO REASON CODE TITLE                   
         XC    MKORSCT,MKORSCT     CLEAR IT                                     
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         LA    R2,MKORSCH          POINT TO REASON CODE FIELD                   
         XC    MKORSC,MKORSC       CLEAR CURRENT VALUE                          
         OI    1(R2),X'20'         PROTECT                                      
         NI    4(R2),X'DF'         INVALIDATE                                   
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         LA    R2,MKORSTTH         POINT TO REASON TEXT TITLE                   
         XC    MKORSTT,MKORSTT     CLEAR IT                                     
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         LA    R2,MKORSTH          POINT TO REASON TEXT                         
         XC    MKORST,MKORST       CLEAR CURRENT VALUE                          
         OI    1(R2),X'20'         PROTECT                                      
         NI    4(R2),X'DF'         INVALIDATE                                   
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         CLIY  SVB0PROF+7,C'1'     TEST REASON CODE SCHEME 1                    
         BNE   SMK21               NO                                           
*                                                                               
         LA    R2,MKORSCTH         POINT TO REASON CODE TITLE                   
         MVC   8(11,R2),=X'D98581A296954083968485'  = 'REASON CODE'             
         LA    R2,MKORSCH          POINT TO INPUT FIELD                         
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
*                                                                               
         LA    R2,MKORSTTH         POINT TO TEXT TITLE HDR                      
         MVC   8(4,R2),=X'E385A7A3'                                             
         LA    R2,MKORSTH          POINT TO TEXT INPUT FIELD HDR                
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
*                                                                               
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MORSNELQ     CHECK FOR X'64' REASON CODE ELEMENT          
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
         USING MORSNEL,R6                                                       
SMKRC2   BRAS  RE,NEXTEL                                                        
         BNE   SMK21                                                            
         CLC   MORECID,MORSNOFR    MATCH OFFER/REC #                            
         BNE   SMKRC2                                                           
         MVC   MKORSC,MORSNCOD     MOVE REASON CODE                             
         OI    MKORSCH+4,X'20'     SET CODE VALIDATED                           
         OI    MKORSTH+4,X'20'     SET TEXT VALIDATED TOO                       
*                                                                               
         LLC   RE,MORSNLEN                                                      
         AHI   RE,-MORSNOVR-1      GET TEXT LENGTH & SETUP FOR EX               
         BNP   SMK21                                                            
         EX    RE,*+4                                                           
         MVC   MKORST(0),MORSNTXT                                               
         OC    MKORST,SPACES                                                    
         DROP  R6                                                               
         SPACE 1                                                                
*==============================================================                 
* DISPLAY UPGRADE EXPRESSION                                                    
*==============================================================                 
         SPACE 1                                                                
SMK21    LA    R2,MKOUPGDH                                                      
*                                                                               
         L     R6,AREC3            CHECK FOR UPGRADE ELEMENT IN REC             
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOUPGELQ     X'61'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
         USING MOUPGELD,R6                                                      
SMK22    BRAS  RE,NEXTEL                                                        
         BNE   SMK23                                                            
         CLC   MORECID,MOUPGOFR    MATCH OFFER/REC #                            
         BNE   SMK22                                                            
         MVC   8(L'MOUPGTXT,R2),MOUPGTXT                                        
         OI    4(R2),X'20'         SET PREV VALIDATED                           
         B     SMK24                                                            
         DROP  R6                                                               
*                                                                               
SMK23    LHI   RF,SVUPTXT-BUYSAVE                                               
         AR    RF,RA                                                            
         CLI   0(RF),C' '          TEST FOR DATA IN SAVE AREA                   
         BNH   SMK25                                                            
         MVI   MGUPOPT,C'O'        SET UPGRADE FROM OPTIONS                     
         MVC   8(32,R2),0(RF)                                                   
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
SMK24    LA    R1,MKOUPGD+L'MKOUPGD-1                                           
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BNH   *-6                 BUMPING BACKWARDS, BE CAREFUL                
         LA    R0,MKOUPGD-1                                                     
         SR    R1,R0                                                            
         STC   R1,5(R2)                                                         
*                                                                               
SMK25    BRAS  RE,GETDEMNS         GET DEMO NAMES IN ELEM                       
*                                                                               
         LA    R2,MKODN1H          FIRST DEMO NAME                              
         LA    RE,ELEM                                                          
         LA    RF,14                                                            
*                                                                               
SMK27    MVC   8(6,R2),0(RE)       MOVE DEMO NAME                               
*                                                                               
         AHI   R2,MKODM1H-MKODN1H  ADVANCE TO DEMO FLDHDR                       
         NI    1(R2),X'FF'-X'20'   UNPROTECT THE FIELD                          
*                                                                               
         CLI   0(RE),C' '          TEST ANYTHING THERE                          
         BH    *+8                                                              
         OI    1(R2),X'20'         ELSE PROTECT THE FIELD                       
*                                                                               
         AHI   R2,MKODN2H-MKODM1H  ADVANCE TO NAME FLDHDR                       
         LA    RE,6(RE)                                                         
         BCT   RF,SMK27                                                         
*                                                                               
* NOW DISPLAY THE VALUES                                                        
*                                                                               
         MVC   DUB(2),=X'5004'     FAKE A X'50' ELEMENT FOR GETMKDEM            
         MVC   DUB+2(2),MORECID                                                 
         LA    R1,DUB                                                           
         ST    R1,LAST50EL         SET ELEM ADDRESS                             
*                                                                               
         BRAS  RE,GETMKDEM                                                      
*                                                                               
         MVI   MGLOOKUP,C'Y'       SET FLAG TO LOOKUP & BUILD DEMO ELEM         
         OC    LAST60EL,LAST60EL   TEST DEMO ELEM FOUND                         
         BNZ   SMK30               YES - USE THOSE DEMOS                        
         OC    MGONEMSS,MGONEMSS   TEST ONLY ONE MISSED LINE                    
         BZ    SMK29               NO - LOOK UP DEMOS                           
         CLC   MGONEDAY,MODAYS     TEST SAME DAYS                               
         BNE   SMK29                                                            
         CLC   MGONETIM,MOSTIM     TEST SAME TIMES                              
         BNE   SMK29                                                            
         MVI   MGLOOKUP,C'N'       SET FLAG TO MOVE DEMO ELEM ONLY              
*                                                                               
SMK29    LHI   RE,SVMGSAVE-BUYSAVE   SAVE MGE DATA                              
         AR    RE,RA                                                            
         LHI   RF,SVMGSAVX-SVMGSAVE                                             
         LA    R0,MGSAVE                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BRAS  RE,DEMOLOOK         GO LOOK UP DEMOS                             
         LA    RE,ELEM             DEMOLOOK BUILDS X'60' IN ELEM                
         OC    0(L'ELEM,RE),0(RE)                                               
         BZ    *+8                                                              
         ST    RE,LAST60EL         POINT TO DEMO ELEM IN MISSED BUY             
*                                                                               
         OC    LAST5EEL,LAST5EEL   ALREADY HAVE X'5E' ELEM?                     
         BNZ   SMK30                                                            
         LA    RE,MSTRBUY          DEMOLOOK BUILDS X'5E' IN ELEM                
         OC    0(L'MSTRBUY,RE),0(RE)                                            
         BZ    *+8                                                              
         ST    RE,LAST5EEL         POINT TO DEMO ELEM IN MISSED BUY             
*                                                                               
SMK30    LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         LA    R2,MKODM1H          POINT TO FIRST DEMO VALUE                    
         LA    R5,14               SET FOR MAX DEMOS                            
*                                                                               
SMK31    ICM   R6,15,LAST60EL      HAVE X'60' DEMO ELEM ADDR?                   
         BZ    SMK32               NO                                           
         BAS   RE,GETDVAL          EDIT VALUE INTO FIELD                        
         BE    SMK33                                                            
*                                                                               
SMK32    ICM   R6,15,LAST5EEL      HAVE X'5E' DEMO ELEM ADDR?                   
         BZ    SMK33               NO                                           
         BAS   RE,GETDVAL          EDIT VALUE INTO FIELD                        
*                                                                               
SMK33    OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         AHI   R2,MKODM2H-MKODM1H  ADVANCE TO NEXT VALUE FIELD                  
         LA    R4,3(R4)            NEXT DEMO CODE                               
         CLI   1(R4),0             TEST ANY MORE CODES                          
         BE    SMK34                                                            
         BCT   R5,SMK31            REPEAT FOR # DEMOS                           
         SPACE 1                                                                
*==============================================================                 
* NOW DISPLAY COMMENTS                                                          
* IF ANY X'70' (BUYER) COMMENTS, DISPLAY THEM                                   
* IF PF11 IS ENTERED, DISPLAY THE X'30' COMMENTS AND                            
*    CHANGE THE COMMENT TITLE FIELD AND THE PFKEY LINE                          
*==============================================================                 
         SPACE 1                                                                
SMK34    CLI   PFKEY,11            TEST TOGGLE COMMENTS                         
         BNE   SMK40                                                            
* CLEAR AND UNPROTECT COMMENTS                                                  
         LA    R2,MKOCOM1H                                                      
         LA    R0,5                                                             
*                                                                               
SMK35    XC    8(L'MKOCOM1,R2),8(R2)                                            
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         OI    6(R2),X'80'         XMT                                          
         AHI   R2,MKOCOM2H-MKOCOM1H                                             
         BCT   R0,SMK35                                                         
*                                                                               
         CLI   MKOCTTL,C'B'        TEST BYR COMS ON SCREEN                      
         BE    SMK50               YES - GO DISPLAY REP COMMENTS                
*                                                                               
SMK40    LA    R2,MKOCTTLH         POINT TO COMMENT TITLE                       
         MVC   8(5,R2),BUYER                                                    
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         LA    R2,MKOPFKH          POINT TO PFK DESC                            
         MVC   13(7,R2),REPCOMS    TOGGLE TO REP COMMENTS                       
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         LA    R2,MKOCOM1H                                                      
         L     R6,AREC3                                                         
         OC    MGMKODA2,MGMKODA2                                                
         BZ    *+8                                                              
         L     R6,AREC4                                                         
****     AH    R6,DSPFRST                                                       
*                                                                               
         MVI   ELCODE,MOBBCELQ     SEE IF ANY X'70' (BYR) COMS                  
         MVI   SVBYRCOM,C'N'       ASSUME NO BYRCOMS                            
*                                                                               
         BRAS  RE,GETEL                                                         
****     BRAS  RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
         USING MOBBCELD,R6                                                      
SMK42    BRAS  RE,NEXTEL                                                        
         BNE   SMK48                                                            
         CLC   MORECID,MOBBCOFR                                                 
         BNE   SMK42                                                            
*                                                                               
         MVI   SVBYRCOM,C'Y'       SET BYRCOMS FOUND                            
*                                                                               
         LLC   RE,MOBBCLEN                                                      
         AHI   RE,-MOBBCOVH-1      GET TEXT LENGTH & SETUP FOR EX               
         BM    SMK42                                                            
         EX    RE,*+4                                                           
         MVC   8(0,R2),MOBBCTXT                                                 
         OI    6(R2),X'80'         XMT                                          
         AHI   R2,MKOCOM2H-MKOCOM1H                                             
         B     SMK42                                                            
         DROP  R6                                                               
*                                                                               
SMK48    CLI   SVBYRCOM,C'Y'       TEST ANY BYRCOMS FOUND                       
         BE    SMK70               YES - DONE                                   
*                                                                               
* DISPLAY REP COMMENTS                                                          
*                                                                               
SMK50    LA    R2,MKOCTTLH         POINT TO COMMENT TITLE                       
         MVC   8(5,R2),REP         SAY THESE ARE REPCOM                         
         OI    6(R2),X'80'         XMT                                          
         LA    R2,MKOPFKH                                                       
         MVC   13(7,R2),BYRCOMS    SET PFKEY TO TOGGLE TO BYRCOM                
         OI    6(R2),X'80'         XMT                                          
*                                                                               
         CLI   SVBYRCOM,C'Y'       TEST ANY BYRCOMS ?                           
         BNE   SMK54               NO                                           
* PROTECT REP COMMENTS SO CAN'T CHANGE                                          
         LA    R2,MKOCOM1H                                                      
         LA    R0,5                                                             
*                                                                               
SMK52    OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         XMT                                          
         AHI   R2,MKOCOM2H-MKOCOM1H                                             
         BCT   R0,SMK52                                                         
*                                                                               
SMK54    LA    R2,MKOCOM1H                                                      
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MOBCELQ      X'30'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
         USING MOBCELD,R6                                                       
SMK56    BRAS  RE,NEXTEL                                                        
         BNE   SMK60                                                            
         CLC   MORECID,MOBCOFFR    MATCH OFFER/REC #                            
         BNE   SMK56                                                            
*                                                                               
SMK58    LLC   RE,MOBCLEN                                                       
         AHI   RE,-MOBCOVRH-1      GET TEXT LENGTH & SETUP FOR EX               
         BM    SMK56                                                            
         EX    RE,*+4                                                           
         MVC   8(0,R2),MOBCTEXT                                                 
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         OI    6(R2),X'80'         XMT                                          
         AHI   R2,MKOCOM2H-MKOCOM1H                                             
         B     SMK56                                                            
         DROP  R6                                                               
*                                                                               
SMK60    LA    R0,MKOCOM1H                                                      
         CR    R2,R0               TEST ANY REP COMMENTS                        
         BH    SMK70                                                            
         MVC   8(21,R2),=C'** NO REP COMMENTS **'                               
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         OI    6(R2),X'80'         XMT                                          
         B     SMK70                                                            
                                                                                
*================================================================               
* PROTECT ALL INPUT FIELDS UNLESS OFFER STAT IS NEW OR AMENDED                  
*================================================================               
                                                                                
SMK70    CLI   SVMNSTAT,MNSTNEW                                                 
         BE    SMK80                                                            
         CLI   SVMNSTAT,MNSTAMND                                                
         BE    SMK80                                                            
*                                                                               
         LA    R2,MKOLINH                                                       
*                                                                               
SMK72    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SMK74                                                            
         OI    1(R2),X'20'         PROTECT                                      
         B     SMK72                                                            
*                                                                               
SMK74    MVC   0(3,R2),=X'000101'                                               
         CLI   SVSCR,X'F2'         TEST MKO ORB                                 
         BE    SMKX                                                             
*                                                                               
         CLI   SVBYRCOM,C'Y'       IF BUYER COMS                                
         BE    SMK80               THEY GET A CHOICE                            
         BRAS  RE,F4RETURN         ELSE SAY PF12 TO RETURN                      
         MVC   MKOPFK(30),0(R1)                                                 
         B     SMKX                                                             
*                                                                               
SMK80    CLI   MKOCTTL,C'B'        TEST BUYER COMMENTS DISPLAYED                
         BNE   SMK82                                                            
         BRAS  RE,F4TGLREP         SAY PF11 FOR REPCOMS                         
         MVC   MKOPFK(30),0(R1)                                                 
         B     SMKX                                                             
*                                                                               
SMK82    BRAS  RE,F4TGLBYR                                                      
         MVC   MKOPFK(30),0(R1)    SAY PF11 FOR BYRCOMS                         
         CLI   SVBYRCOM,C'Y'       IF THERE ARE BUYER COMMENTS                  
         BE    SMKX                                                             
         BRAS  RE,F4BYRCOM                                                      
         MVC   MKOPFK(30),0(R1)    SAY PF11 TO ADD BYRCOMS                      
*                                                                               
SMKX     J     EXIT                                                             
*                                                                               
BUYER    DC    X'C2A4A88599'       AVOID LOWERCASE IN PROGRAM                   
REP      DC    X'D985974040'                                                    
REPCOMS  DC    X'D98597839694A2'                                                
BYRCOMS  DC    X'C2A899839694A2'                                                
*                                                                               
GETDVAL  NTR1                                                                   
         LLC   R1,1(R6)            GET L'ELEM PASSED                            
         LA    RF,8                DEFAULT 8 BYTE ENTRIES                       
*                                                                               
         CLI   0(R6),MONTELQ       TEST POINTING AT X'5E' EL                    
         BNE   GDV1                                                             
         LA    RF,9                9 BYTE ENTRIES                               
         AHI   R1,-4                                                            
         SR    R0,R0                                                            
         DR    R0,RF               SET FOR NUM DEMOS                            
         LA    R6,4(R6)            POINT TO DEMO DATA                           
         B     GDV4                                                             
*                                                                               
GDV1     CLI   0(R6),MODMELQ       TEST POINTING AT X'60' EL                    
         BNE   GDV2                                                             
         AHI   R1,-4                                                            
         SRL   R1,3                SET FOR BCT ON NUM DEMOS                     
         LA    R6,4(R6)            POINT TO DEMO DATA                           
         B     GDV4                                                             
*                                                                               
GDV2     CLI   0(R6),X'02'         TEST POINTING AT 02 DEMEL                    
         JNE   *+2                                                              
         AHI   R1,-24                                                           
         SRL   R1,3                SET FOR BCT ON NUM DEMOS                     
         LA    R6,24(R6)           POINT TO DEMO DATA                           
*                                                                               
GDV4     CLC   0(3,R4),0(R6)       FIND MATCH DEMO CATEGORY?                    
         BE    GDV6                 YES                                         
         LA    R6,0(RF,R6)         BUMP TO NEXT                                 
         BCT   R1,GDV4             REPEAT FOR NUM DEMOS                         
         J     NO                   NO MATCH, EXIT WITH CC=NEQ                  
*                                                                               
GDV6     ICM   R0,15,4(R6)                                                      
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         LA    R4,8(R2)                                                         
         TM    4(R6),X'40'         TEST 2-DEC                                   
         BO    GDV8                                                             
         EDIT  (R0),(6,(R4)),1,ALIGN=LEFT                                       
         B     GDV10                                                            
*                                                                               
GDV8     EDIT  (R0),(6,(R4)),2,ALIGN=LEFT                                       
*                                                                               
GDV10    AR    R4,R0               POINT TO END OF FIELD                        
         TM    4(R6),X'80'         TEST OVERRIDE                                
         BZ    *+8                                                              
         MVI   0(R4),C'*'                                                       
*                                                                               
GDVX     J     YES                 EXIT WITH CC=EQ                              
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* DISPLAY ORBIT DATA FOR CHANGE                                                 
*==============================================================                 
                                                                                
SMKORB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MORORB1H                                                      
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
SMKORB2  MVI   ELCODE,MORNELQ      SEARCH FOR X'41' ORBIT ELEMENTS              
         BRAS  RE,NEXTEL                                                        
         BNE   SMKORBX                                                          
*                                                                               
         USING MORNELD,R6                                                       
         CLC   MORECID,MORNOFFR    MATCH OFFER/REC #                            
         BNE   SMKORB2                                                          
*                                                                               
         LA    R4,8(R2)                                                         
*                                                                               
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),DMCB,(BYTE,MORNDAYS),0(R4)                                  
         AHI   R4,11                                                            
         BAS   RE,BACKUP                                                        
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MORNSTIM,0(R4)                                         
         AHI   R4,12                                                            
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(7,R4),MORNPROG                                                 
         OC    LINPROG,SPACES                                                   
         AHI   R4,8                                                             
         BAS   RE,BACKUP                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MORNDEM                                                     
         N     R0,=X'00003FFF'     DROP FLAGS                                   
         TM    MORNDEM,X'40'       TEST 2-DEC                                   
         BO    SMKORB10                                                         
         EDIT  (R0),(5,0(R4)),1,ALIGN=LEFT                                      
         B     SMKORB12                                                         
*                                                                               
SMKORB10 EDIT  (R0),(5,0(R4)),2,ALIGN=LEFT                                      
*                                                                               
SMKORB12 AHI   R2,MORORB2H-MORORB1H  NEXT OUTPUT FIELD                          
         B     SMKORB2                                                          
*                                                                               
SMKORBX  J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
BACKUP   CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         AHI   R4,2                                                             
         BR    RE                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*=====================================================================*         
* VALIDATE DATA ON MKO (F4) SCREEN                                              
* ELEMENT STACK IS BUILT IN AREC5 AND MERGED INTO RECORDS AT END                
*=====================================================================*         
         SPACE 1                                                                
VALMKO   NTR1  BASE=*,LABEL=*                                                   
         MVC   MYSTTN,MOSTTN       SET CABLE STA/NET                            
         BRAS  RE,READMKO          READ RECORDS TO GET DISK ADDRS               
*                                                                               
         MVC   SVTSRNUM,T.TSRNUM   SAVE FIRST TSAR SEQNUM                       
*                                                                               
         L     R0,AREC5            CLEAR EDITED ELEMENT STACK                   
         LHI   R1,REC5X-REC5       LENGTH OF REC5                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR REC5                                   
*                                                                               
         CLI   SVSCR,X'F2'         TEST ORB SEL                                 
         BE    VMKORB                                                           
                                                                                
*=============================================================                  
* VALIDATE DAYPART CODE AND TEST GOALS EXIST IF REQUIRED                        
*=============================================================                  
                                                                                
VMK20    LA    R2,MKODPTH                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,DPTEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
         MVC   MODPT,MKODPT        MOVE FROM SCREEN TO TSAR                     
         BRAS  RE,TSTGOAL          TEST GOALS FOR THIS DPT/SLN                  
         EJECT                                                                  
*=============================================================                  
* EDIT PROGRAM                                                                  
*=============================================================                  
         SPACE 1                                                                
         LA    R2,MKOPROGH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,PGMEDT                                                    
         GOTO1 CALLEDT                                                          
         MVC   MOPROG,BUPROG                                                    
         SPACE 1                                                                
*=============================================================                  
* EDIT ADJACENCY CODE / PURPOSE CODE / REASON CODE                              
*=============================================================                  
         SPACE 1                                                                
         MVI   BUADJ,0             CLEAR EDITED VALUE                           
         CLI   SVCPROF+9,C'0'      TEST ADJ CODES USED                          
         BE    VMK40               NO                                           
*                                                                               
         LA    R2,MKOADJH                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ADJEDT       SET TO CALL EDIT ROUTINE                     
         GOTO1 CALLEDT                                                          
*                                                                               
VMK40    MVC   MOADJ,BUADJ         MOVE VALUE TO TSAR REC                       
*                                                                               
         BRAS  RE,VALPURP          VALIDATE PURPOSE CODE IF REQD                
         BNE   VMK41                                                            
         OI    MOFLAGS,X'80'       SET PURPOSE CODE PRESENT                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,MOPRPELQ       X'63' ELEMENT                                
         MVI   ELEM+1,MOPRPLNQ     SET LENGTH                                   
         MVC   ELEM+2(2),MORECID   OFFERNUM/RECNUM                              
         MVC   ELEM+4(6),MKOPUR                                                 
         OC    ELEM+4(6),SPACES                                                 
         BAS   RE,STACKEL                                                       
*                                                                               
VMK41    BRAS  RE,VALRSN           VALIDATE REASON CODE IF REQD                 
         BNE   VMK42                                                            
         OI    MOFLAGS,X'40'       SET REASON CODE PRESENT                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,MORSNELQ       X'64' ELEMENT                                
         MVI   ELEM+1,MORSNOVR     SET LENGTH                                   
         MVC   ELEM+2(2),MORECID   OFFERNUM/RECNUM                              
         MVC   ELEM+4(6),MKORSC    REASON CODE                                  
         OC    ELEM+4(6),SPACES                                                 
*                                                                               
         LA    R2,MKORSTH          POINT TO TEXT FIELD HEADER                   
         CLI   5(R2),0                                                          
         BE    VMK41X                                                           
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ELEM+10(0),8(R2)    REASON CODE TEXT                             
         OC    ELEM+10(40),SPACES                                               
         LLC   RF,ELEM+1           GET CURRENT LENGTH                           
         LA    RF,1(RE,RF)         ADD TEXT LENGTH                              
         STC   RF,ELEM+1                                                        
*                                                                               
VMK41X   BAS   RE,STACKEL          ADD UPGRADE ELEM TO STACK                    
*=============================================================                  
* VALIDATE THE COS2 IF APPLICABLE                                               
*=============================================================                  
VMK42    TM    SVCOPT4,X'02'       CLT USING EARNED DISCOUNT FOR TRADE?         
         BZ    VMK43               NO                                           
         CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   VMK43               NO                                           
*                                                                               
         LA    R2,MKOCOS2H                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,COS2EDT                                                   
         GOTO1 CALLEDT                                                          
         MVC   MOCOS2,BUCOST2+1                                                 
*                                                                               
         LA    R2,MKORPCDH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,REPEDT                                                    
         GOTO1 CALLEDT                                                          
         MVC   MOREP,BUREP                                                      
*                                                                               
         MVI   ERRCD,REPERR                                                     
         CLI   SVDARPRF+14,C'Y'    MULTIPLE TRADE CODES?                        
         BE    *+14                                                             
         CLC   MOREP,SVTRDDTA      NO, REP CODE MUST MATCH                      
         B     *+10                                                             
         CLC   SVTRDDTA+2(2),MKORPCD  DO THEY MATCH FOR 2 DIGITS?               
         JNE   ERREXITL                                                         
*                                                                               
VMK43    MVI   T.TSACTN,TSAPUT                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
*                                                                               
         MVI   T.TSACTN,TSASAV     NEED TO WRITE TSAR BUFFER NOW TOO!           
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         BAS   RE,UPDMKO           WRITE OFFER RECORD BACK TO  FILE             
*                                                                               
         MVC   DUB(5),MOSTTN       SAVE STATION/ID/NUM                          
         MVI   T.TSACTN,TSANXT     GET NEXT TSAR RECORD                         
         BRAS  RE,CALLTSAR                                                      
         BNE   VMK45                                                            
         CLI   MOTYPE,MOTYPEQ                                                   
         BNE   VMK45                                                            
         CLC   DUB(5),MOSTTN       SAME OFFER/REQ                               
         BNE   VMK45               GO BACK AND UPDATE                           
         CLI   MOCOMNUM,0          TEST COMMENT                                 
         BE    VMK20               NO - CONTINUE                                
*                                                                               
VMK45    MVC   T.TSRNUM,SVTSRNUM                                                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR         RESTORE SEQNUM 1 REC                         
         SPACE 1                                                                
*=============================================================                  
* EDIT UPGRADE                                                                  
*=============================================================                  
         SPACE 1                                                                
VMK50    DS    0H                                                               
         LAY   RF,BUUPGD                                                        
         XC    0(L'BUUPGD,RF),0(RF) CLEAR EDITED VALUE                          
*                                                                               
         LA    R2,MKOUPGDH                                                      
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VMK51               YES - GO EDIT                                
*                                                                               
         LHI   RF,SVUPTXT-BUYSAVE                                               
         AR    RF,RA                                                            
         CLI   0(RF),C' '          TEST FOR DATA IN NEW SAVE AREA               
         BNH   VMK60               NO                                           
         MVC   8(32,R2),0(RF)                                                   
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         LA    RF,39(R2)           POINT TO END OF FIELD                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,1                                                             
         LA    R0,8(R2)                                                         
         SR    RF,R0                                                            
         STC   RF,5(R2)            SET DATA LENGTH                              
         OI    4(R2),X'20'         SET VALIDATED                                
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     VMK54                                                            
*                                                                               
VMK51    TM    4(R2),X'20'         TEST PREVIOUSLY VALID                        
         BO    VMK54                                                            
*                                                                               
         CLI   8(R2),C'='          TEST USE MISSED DEMO PERCENTAGE              
         BNE   VMK51A                                                           
*                                                                               
         LH    R0,T.TSRNUM         SAVE TSAR RECORD NUMBER                      
         BRAS  RE,GTMSSDEM                                                      
         STH   R0,T.TSRNUM                                                      
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         B     VMK60                                                            
*                                                                               
VMK51A   CLC   =C'UPT=',8(R2)        TEST VALID FORMULA                         
         BE    VMK51B                                                           
         CLC   =C'UPP=',8(R2)                                                   
         BE    VMK51B                                                           
         MVI   ERRCD,BADUPGD       NO, BAD UPGRADE                              
         J     ERREXITL                                                         
*                                                                               
VMK51B   CLC   =C'UPT=X',8(R2)     TEST REMOVE UPGRADE                          
         BNE   VMK52                                                            
         OI    4(R2),X'20'         SET VALIDATED                                
         B     VMK54               AND INSERT ELEM IN OFFER                     
*                                                                               
VMK52    MVI   BUTRCODE,C'B'       TELL EDIT IT'S A NEW BUY                     
         MVC   BUDAYS,MODAYS                                                    
         MVC   BUTIME,MOSTIM                                                    
*                                                                               
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   FSTOPS,C'='                                                      
         GOTO1 FLDVAL              THIS CALL NEEDED FOR COMPATABILITY           
*                                                                               
         MVI   EDTVAL,UPEDT                                                     
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   ERRCD,BADUPGD                                                    
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,X'FF'         SHOULD BE NO MORE INPUT                      
         JNE   ERREXITL                                                         
         OI    MOFLAGS,X'20'       SET UPGRADE PRESENT                          
*                                                                               
VMK54    XC    ELEM,ELEM                                                        
         MVI   ELEM,MOUPGELQ       X'61' ELEMENT                                
         MVI   ELEM+1,MOUPGOVH     SET LENGTH                                   
         MVC   ELEM+2(2),MORECID   OFFERNUM/RECNUM                              
         MVC   ELEM+4(28),8(R2)    UPGRADE TEXT                                 
         BAS   RE,STACKEL          ADD UPGRADE ELEM TO STACK                    
         EJECT                                                                  
*=============================================================                  
* LOOK UP DEMOS AND OVERRIDE VALUES AS NEEDED                                   
*=============================================================                  
         SPACE 1                                                                
VMK60    MVI   MGLOOKUP,C'Y'                                                    
         BRAS  RE,DEMOLOOK         GET BOOK/UPGRADE VALUES IN ELEM              
*                                                                               
VMK63    MVI   BUTRCODE,C'C'       TEST DEMOEDT IT'S A CHANGE                   
         LA    R2,MKODM1H          POINT TO FIRST DEMO INPUT FIELD              
*&&DO                                                                           
         LLC   R0,ELEM+1                                                        
         AHI   R0,-4                                                            
         SRL   R0,3                SET R0 FOR BCT ON NUM DEMOS                  
         CHI   R0,14               SET MAX DEMO TO 14                           
         BNH   *+8                                                              
         LHI   R0,14                                                            
*&&                                                                             
*                                                                               
         LA    R6,SVDEMOS          POINT TO DEMOS IN DISPLAYED ORDER            
         LA    R0,L'SVDEMLST(R6)   GET ADDR OF END OF DEMO LIST                 
         OC    SVBRDEMS,SVBRDEMS   HAVE BRAND DEMO LIST?                        
         BZ    *+12                 NO                                          
         LA    R6,SVBRDEMS         POINT TO DEMOS IN DISPLAYED ORDER            
         LA    R0,L'SVBRDEMS(R6)   GET ADDR OF END OF DEMO LIST                 
         ST    R0,FULL             SET ADDR OF END OF DEMO LIST                 
*                                                                               
VMK64    CLI   5(R2),0             TEST ANY INPUT                               
         BE    VMK66               NO                                           
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BZ    VMK65               NO - THIS IS AN OVERRIDE                     
         LLC   RE,5(R2)            GET INPUT LENGTH                             
         LA    RE,8-1(RE,R2)       POINT TO LAST INPUT CHAR                     
         CLI   0(RE),C'*'          WAS IT AN OVERRIDE BEFORE                    
         BNE   VMK66               NO - JUST SKIP IT                            
*                                                                               
VMK65    MVC   SPDEMTYP,1(R6)                                                   
         LA    R4,8(R2)            POINT TO DATA                                
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,DEMEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
         CLC   BUDEM,=X'FFFF'      TEST 'NO-OVERRIDE'                           
         BE    VMK66                                                            
*                                                                               
         GOTOR FNDDEM,DMCB,(8,ELEM)      MATCH DEMO IN X'60' MODMEL?            
         BE    VMK65A                     YES, UPDATE DEMO VALUE                
         OC    MSTRBUY,MSTRBUY           HAVE X'5E' MONTEL?                     
         JZ    *+2                        NO, SOMETHING WRONG                   
         GOTOR FNDDEM,DMCB,(9,MSTRBUY)   MATCH DEMO IN X'5E' MONTEL?            
         JNE   *+2                        NO, SOMETHING WRONG                   
*                                                                               
VMK65A   MVI   3(R1),100           SET HUT VALUE = 100                          
         MVC   4(4,R1),BUNEWDEM                                                 
         OI    4(R1),X'80'         SET OVERRIDE IND                             
*                                                                               
VMK66    AHI   R2,MKODM2H-MKODM1H  NEXT INPUT FIELD                             
         LA    R6,3(R6)            NEXT DEMO IN ELEMENT                         
         C     R6,FULL             EOL?                                         
         BNL   VMK68                YES                                         
         OC    0(3,R6),0(R6)       ANY MORE DEMOS?                              
         BNZ   VMK64                YES, GO BACK                                
*                                                                               
VMK68    BAS   RE,STACKEL          ADD ELEM TO SAVE STACK                       
         OC    MSTRBUY,MSTRBUY     HAVE X'5E' MONTEL?                           
         BZ    VMK70                NO                                          
         MVC   ELEM,MSTRBUY        YES, ADD X'5E' TO SAVE STACK                 
         BRAS  RE,STACKEL                                                       
         B     VMK70                                                            
*                                                                               
FNDDEM   NTR1                                                                   
         LLC   RF,0(R1)            L'EACH DEMO ENTRY                            
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         LLC   R1,1(RE)                                                         
         AHI   R1,-4                                                            
         SR    R0,R0                                                            
         DR    R0,RF                                                            
         LR    R0,R1               R0 = # OF ENTRRIES                           
         LA    R1,4(RE)            R1 = MKGD DEMO LIST                          
*                                                                               
FNDDEM2  CLC   0(3,R6),0(R1)       MATCH DEMO                                   
         BE    FNDDEMX              YES, RETURN R1 ADDR                         
         LA    R1,0(RF,R1)                                                      
         BCT   R0,FNDDEM2                                                       
         J     NO                                                               
*                                                                               
FNDDEMX  XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*=================================================================              
* EDIT COMMENTS                                                                 
* IF REP COMMENTS DISPLAYED, MUST USE PF11 TO ADD BUYER COMMENTS                
*=================================================================              
         SPACE 1                                                                
VMK70    CLI   MKOCTTL,C'B'        TEST BUYER COMMENTS DISPLAYED                
         BE    VMK74                                                            
         CLI   PFKEY,10            TEST ADD BUYER COMMENTS                      
         BE    VMK74                                                            
* SHOULDN'T BE ANY COMMENT CHANGES - SHOULD HAVE HIT PF10                       
         LA    R2,MKOCOM1H                                                      
         LA    R0,5                                                             
         MVC   NERRCD,=Y(ADDBYCOM)                                              
VMK72    CLI   5(R2),0             TEST INPUT                                   
         BE    *+12                NO                                           
         TM    4(R2),X'20'         TEST PREVIOUSLY VALID                        
         JZ    ERREXITK                                                         
         AHI   R2,MKOCOM2H-MKOCOM1H  NEXT INPUT FIELD                           
         BCT   R0,VMK72                                                         
         B     VMK90                                                            
*                                                                               
VMK74    LA    R2,MKOCOM1H                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,MOBBCELQ       X'70' - BUYER BUY COMMENT ELEM               
         MVI   ELEM+1,4            SET MIN LEN                                  
         MVC   ELEM+2(2),MORECID                                                
         MVI   ELEM+4,1            SET SEQUENCE NUMBER                          
         MVI   ELEM+5,1            SET COMMENT NUMBER                           
*                                                                               
VMK76    CLI   5(R2),0             TEST ANY DATA                                
         BE    VMK78               NO                                           
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   =C'** NO REP COMMENTS **',8(R2)                                  
         BE    VMK78                                                            
*                                                                               
         EX    RF,*+4                                                           
         MVC   ELEM+6(0),8(R2)                                                  
         LA    R0,7(RF)            SET DATALENGTH + 5 + 1                       
         STC   R0,ELEM+1                                                        
*                                                                               
         BAS   RE,STACKEL          ADD ELEM TO SAVE STACK                       
*                                                                               
VMK78    AHI   R2,MKOCOM2H-MKOCOM1H  NEXT INPUT FIELD                           
         IC    RF,ELEM+4             GET SEQUENCE NUMBER                        
         AHI   RF,1                                                             
         STC   RF,ELEM+4                                                        
         STC   RF,ELEM+5             SET AS COMMENT NUMBER TOO                  
         XC    ELEM+6(L'ELEM-6),ELEM+6  CLEAR TEXT                              
         CLI   ELEM+4,5                                                         
         BNH   VMK76                                                            
         B     VMK90                                                            
         EJECT                                                                  
*===========================================================                    
* VALIDATE ORBIT INPUT                                                          
*===========================================================                    
                                                                                
VMKORB   XC    NERRCD,NERRCD                                                    
         LA    R2,MORORB1H                                                      
*                                                                               
         MVC   T.TSRNUM,SVTSRNUM                                                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR         RESTORE SEQNUM 1 REC                         
*                                                                               
         MVI   T.TSACTN,TSANXT     READ FIRST ORBIT RECORD                      
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         CLI   MOCOMNUM,1          TEST ORBIT RECORD                            
         BNE   ORBERR1             NO - ERROR                                   
*                                                                               
VMKORB10 LA    R6,MOORB            POINT TO FIRST ORB IN TSARREC                
         USING MOORB,R6                                                         
         LHI   R7,4                POSITION POINTER                             
*                                                                               
VMKORB12 LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
*                                                                               
         MVI   EDTVAL,DAYEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
         MVI   EDTVAL,TIMEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
         MVC   BUPROG,SPACES                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   VMKORB14                                                         
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         B     VMKORB16                                                         
*                                                                               
VMKORB14 CLI   FLEN+1,7                                                         
         BH    ORBERR2                                                          
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   BUPROG(0),0(R4)                                                  
*                                                                               
VMKORB16 MVI   EDTVAL,DEMEDT                                                    
         GOTO1 CALLEDT                                                          
*                                                                               
         CLC   MOORBDAY,BUDAYS                                                  
         BNE   ORBERR3             CAN'T CHANGE DAYS OR TIMES                   
         CLC   MOORBTIM,BUTIME                                                  
         BNE   ORBERR3             CAN'T CHANGE DAYS OR TIMES                   
         MVC   MOORBPRG,BUPROG                                                  
         MVC   MOORBDEM,BUNEWDEM+2                                              
*                                                                               
E        USING MORNELD,ELEM                                                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,MORNELQ        X'41'                                        
         MVI   ELEM+1,19                                                        
         MVC   E.MORNOFFR(2),MORECID                                            
         MVC   E.MORNDAYS,BUDAYS                                                
         MVC   E.MORNSTIM(4),BUTIME                                             
         MVC   E.MORNPROG,BUPROG                                                
         MVC   E.MORNDEM,BUNEWDEM+2                                             
*                                                                               
         BAS   RE,STACKEL                                                       
*                                                                               
         AHI   R2,MORORB2H-MORORB1H                                             
         LA    R0,MORORB8H                                                      
         CR    R2,R0                                                            
         BH    VMKORB20                                                         
         AHI   R6,L'MOORB          NEXT ORBIT POSITION                          
         BCTR  R7,0                DECREMENT COUNT                              
*                                                                               
         LTR   R7,R7               TEST NEED NEXT TSAR RECORD                   
         BZ    VMKORB20                                                         
         CLI   0(R6),0             TEST ANOTHER ORBIT IN TSARREC                
         BNZ   VMKORB12            YES - ELSE DONE                              
*                                                                               
VMKORB20 MVI   T.TSACTN,TSAPUT                                                  
         BRAS  RE,CALLTSAR         WRITE TSAR RECORD                            
*                                                                               
         CLI   MOCOMNUM,2          TEST LAST ORBIT RECORD                       
         BE    VMKORB30                                                         
         MVI   T.TSACTN,TSANXT     ELSE GET NEXT ORBIT RECORD                   
         BRAS  RE,CALLTSAR                                                      
         CLI   MOCOMNUM,2          TEST ANOTHER ORBIT RECORD                    
         BE    VMKORB10                                                         
         EJECT                                                                  
*=============================================================                  
* INSERT ORBIT ELEMENTS INTO RECORD                                             
* ORBIT ELEMENTS ARE STANDALONE                                                 
* THERE ARE NO OTHER ELEMENTS IN THE STACK                                      
*=============================================================                  
                                                                                
VMKORB30 MVC   T.TSRNUM,SVTSRNUM                                                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),MGMKODA1  READ FIRST MKO REC                           
         MVC   MYDA,MGMKODA1       DA HERE FOR CABLE                            
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         GOTOR MYGETREC                                                         
                                                                                
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
VMKORB32 LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
VMKORB34 CLI   0(R6),0                                                          
         BE    VMKORB36                                                         
         CLI   0(R6),MORNELQ       X'41'                                        
         BL    VMKORB32                                                         
         BH    VMKORB36            WHEN NEXT ELEM IS HIGH, INSERT               
         GOTO1 VRECUP,DMCB,(C'S',AREC3),(R6)  DELETE EXISTING ELEM              
         B     VMKORB34                                                         
*                                                                               
VMKORB36 L     R5,AREC5            POINT TO ELEMENT STACK                       
*                                                                               
VMKORB38 GOTOR MYRECUP,DMCB,AREC,(R5),(C'R',(R6))                               
         CLI   8(R1),C'R'          TEST OVERFLOW                                
         JNE   *+2                 RECUP RETURNS R ON OK/0 FOR ERROR            
*                                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0               SET TO ADD NEXT AFTER THIS ONE !             
*                                                                               
         LLC   R0,1(R5)                                                         
         AR    R5,R0               NEXT ELEM IN STACK                           
         CLI   0(R5),0                                                          
         BNE   VMKORB38                                                         
         B     VMK170                                                           
         DROP  R6                                                               
*                                                                               
ORBERR1  MVC   NERRCD,=Y(NONEWORB)  CAN'T ADD ORBIT NOT SENT BY REP             
         J     ERREXITN                                                         
ORBERR2  MVC   NERRCD,=Y(ORBPRG7)   ORB PROGRAM MAX 7 CHARS                     
         J     ERREXITN                                                         
ORBERR3  MVC   NERRCD,=Y(NOORBCHG)  CAN'T CHANGE ORBIT DAYS/TIMES               
         J     ERREXITN                                                         
         EJECT                                                                  
*============================================================                   
* ADD ELEMENT TO SAVE STACK                                                     
*============================================================                   
         SPACE 1                                                                
STACKEL  NTR1                                                                   
         L     RE,AREC5                                                         
*                                                                               
STACKEL2 CLI   0(RE),0                                                          
         BE    STACKEL4                                                         
         LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     STACKEL2                                                         
*                                                                               
STACKEL4 LLC   RF,ELEM+1                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),ELEM                                                     
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* MKO EDITING COMPLETED - UPDATE RECORD(S)                                      
* NOTE THAT THE NEEDED RECORD IS ALWAYS READ TO AREC3                           
* ALL ELEMENTS BUT X'70' COMMENTS ARE IN FIRST RECORD                           
*=====================================================================*         
         SPACE 1                                                                
VMK90    XC    KEY,KEY                                                          
         MVC   KEY+14(4),MGMKODA1  READ FIRST MKO REC                           
         MVC   MYDA,MGMKODA1       IN CASE IT'S LOCAL CABLE                     
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         GOTOR MYGETREC                                                         
*                                                                               
* DEMO ELEMENT                                                                  
******NOTE******                                                                
* CODE TO PUT COMSCORE DEMO UPDATES TO RECORD ARE NOT SUPPORTED HERE            
*  NOR SHOULD IT BE SUPPORTED. NOONE SHOULD BE ENTERING COMSCORE                
*  DEMOS IN SPOT BUY MGE SCREEN                         -HWON 7/13/2017         
******NOTE******                                                                
*                                                                               
         L     R6,AREC3            FIND EXISTING AND DELETE IT                  
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MODMELQ      X'60'                                        
*                                                                               
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
         USING MODMELD,R6                                                       
VMK92    BRAS  RE,NEXTEL                                                        
         BNE   VMK94                                                            
         CLC   MORECID,MODMOFFR    MATCH OFFER/REC #                            
         BNE   VMK92                                                            
         BRAS  RE,DELEL            DELETE EXISTING                              
         DROP  R6                                                               
*                                                                               
VMK94    L     R5,AREC5            FIND NEW DEMEL IN ELEMENT STACK              
*                                                                               
VMK96    XC    MODEM1(24),MODEM1   CLEAR  DEMO VALUES IN TSAR REC               
         CLI   0(R5),0             TEST THERE IS A NEW ELEMENT                  
         BE    VMKPUR              NO - SKIP DEMEL                              
*                                                                               
         CLI   0(R5),MODMELQ       HAVE X'60' MKGD BUY DEMO ELEM?               
         BE    VMK100                                                           
         LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VMK96                                                            
*                                                                               
VMK100   MVC   ELEM(256),0(R5)     MOVE TO 'ELEM' (LEN AT 1(R5))                
*                                                                               
VMK102   L     R6,AREC3            PUT DEMEL AFTER LAST X'50' ELEM              
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOBDELQ      X'50'                                        
         SR    R7,R7                                                            
*                                                                               
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMK104   BRAS  RE,NEXTEL                                                        
         BNE   VMK106                                                           
         CLC   2(2,R6),MORECID     MATCH OFFER/REC #                            
         BNE   VMK104                                                           
         LR    R7,R6               SAVE LAST X'50' ELEM ADDR                    
         B     VMK104                                                           
*                                                                               
VMK106   LTR   R6,R7               SHOULD HAVE FOUND A X'50' ELEM               
         JZ    *+2                                                              
*                                                                               
         ST    R6,LAST50EL         SAVE 50 ELEM ADDR FOR GETMKDEM               
VMK107   LLC   R0,1(R6)            ADD DEMO ELEM AFTER X'50'                    
         AR    R6,R0                                                            
         CLI   0(R6),MONTELQ       HAVE X'5E'?                                  
         BE    VMK107               YES, ADD MODMEL AFTER                       
         BAS   RE,VMKADDEL                                                      
         BNE   VMK102              GO FIND WHERE TO ADD AGAIN                   
*                                                                               
* UPDATE DEMO VALUES IN TSAR RECORD(S)                                          
*                                                                               
         BRAS  RE,GETMKDEM                                                      
*                                                                               
         MVC   SVTSRNUM,T.TSRNUM   SAVE CURRENT TSAR RECNUM                     
*                                                                               
VMK108   MVC   MODEM1(24),BUDEMS   MOVE DEMOS TO TSAR REC                       
         MVC   MODPT,MKODPT        COPY DAYPART                                 
         MVC   MOPROG,BUPROG       AND PROGRAM                                  
*                                                                               
         MVI   T.TSACTN,TSAPUT                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         MVC   DUB(5),MOSTTN       SAVE STATION/OFFER/REC                       
*                                                                               
VMK108A  MVI   T.TSACTN,TSANXT     GET NEXT TSAR RECORD                         
         BRAS  RE,CALLTSAR                                                      
         BNE   VMK108X                                                          
         CLC   DUB(5),MOSTTN       SAME STATION/OFFERNUM/RECNUM                 
         BNE   VMK108X             NO - DONE                                    
         CLI   MOCOMNUM,0          TEST FOR A COMMENT                           
         BE    VMK108              NO - GO UPDATE                               
         B     VMK108A             ELSE CONTINUE TILL CHANGE OFFER/REC          
*                                                                               
VMK108X  MVC   T.TSRNUM,SVTSRNUM                                                
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR         RESTORE SEQNUM 1 REC                         
         EJECT                                                                  
*=====================================================                          
* PURPOSE CODE ELEMENT                                                          
*=====================================================                          
         SPACE 1                                                                
VMKPUR   L     R6,AREC3            FIND EXISTING AND DELETE IT                  
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOPRPELQ     X'63'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMKPUR2  BRAS  RE,NEXTEL                                                        
         BNE   VMKPUR4                                                          
         CLC   2(2,R6),MORECID     MATCH OFFER/REC #                            
         BNE   VMKPUR2                                                          
         BRAS  RE,DELEL            DELETE EXISTING                              
*                                                                               
VMKPUR4  L     R5,AREC5            FIND NEW ELEM IN STACK                       
*                                                                               
VMKPUR6  CLI   0(R5),0             TEST THERE IS A NEW ELEMENT                  
         BE    VMKRC               NO - SKIP                                    
         CLI   0(R5),MOPRPELQ      X'63'                                        
         BE    VMKPUR10                                                         
         LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VMKPUR6                                                          
*                                                                               
VMKPUR10 MVC   ELEM(256),0(R5)     MOVE TO 'ELEM' (LEN AT 1(R5))                
*                                                                               
VMKPUR12 L     R6,AREC3            PUT  AFTER LAST X'50' ELEM                   
****     AH    R6,DSPFRST           AND AFTER X'60' IF PRESENT                  
         AH    R6,DATADISP          AND AFTER X'60' IF PRESENT                  
         SR    R7,R7                                                            
*                                                                               
         MVI   ELCODE,MOBDELQ      X'50'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMKPUR14 BRAS  RE,NEXTEL                                                        
         BNE   VMKPUR16                                                         
         CLC   2(2,R6),MORECID                                                  
         BNE   VMKPUR14                                                         
         LR    R7,R6               SAVE LAST X'50' ADDRESS                      
         B     VMKPUR14                                                         
*                                                                               
VMKPUR16 LTR   R6,R7                                                            
         JZ    *+2                                                              
*                                                                               
VMKPUR18 LLC   R0,1(R6)            ADD ELEM AFTER X'60' OR 61                   
         AR    R6,R0                                                            
         CLI   0(R6),MODMELQ       X'60'                                        
         BE    VMKPUR18                                                         
         CLI   0(R6),MOUPGELQ      X'61'                                        
         BE    VMKPUR18                                                         
         BAS   RE,VMKADDEL                                                      
         BNE   VMKPUR12            GO BACK AND LOCATE AGAIN                     
         EJECT                                                                  
*=====================================================                          
* REASON CODE ELEMENT                                                           
*=====================================================                          
         SPACE 1                                                                
VMKRC    L     R6,AREC3            FIND EXISTING AND DELETE IT                  
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MORSNELQ     X'64'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMKRC2   BRAS  RE,NEXTEL                                                        
         BNE   VMKRC4                                                           
         CLC   2(2,R6),MORECID                                                  
         BNE   VMKRC2                                                           
         BRAS  RE,DELEL            DELETE EXISTING                              
*                                                                               
VMKRC4   L     R5,AREC5            FIND NEW ELEM IN STACK                       
*                                                                               
VMKRC6   CLI   0(R5),0             TEST THERE IS A NEW ELEMENT                  
         BE    VMK110              NO - SKIP                                    
         CLI   0(R5),MORSNELQ      X'64'                                        
         BE    VMKRC10                                                          
         LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VMKRC6                                                           
*                                                                               
VMKRC10  MVC   ELEM(256),0(R5)     MOVE TO 'ELEM' (LEN AT 1(R5))                
*                                                                               
VMKRC12  L     R6,AREC3            PUT  AFTER LAST X'50' ELEM                   
****     AH    R6,DSPFRST           AND AFTER X'60' IF PRESENT                  
         AH    R6,DATADISP          AND AFTER X'60' IF PRESENT                  
         SR    R7,R7                                                            
*                                                                               
         MVI   ELCODE,MOBDELQ      X'50'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMKRC14  BRAS  RE,NEXTEL                                                        
         BNE   VMKRC16                                                          
         CLC   2(2,R6),MORECID                                                  
         BNE   VMKRC14                                                          
         LR    R7,R6               SAVE LAST X'50' ADDRESS                      
         B     VMKRC14                                                          
*                                                                               
VMKRC16  LTR   R6,R7                                                            
         JZ    *+2                                                              
*                                                                               
VMKRC18  LLC   R0,1(R6)            ADD ELEM AFTER X'60' OR 61                   
         AR    R6,R0                                                            
         CLI   0(R6),MODMELQ       X'60'                                        
         BE    VMKRC18                                                          
         CLI   0(R6),MOUPGELQ      X'61'                                        
         BE    VMKRC18                                                          
         CLI   0(R6),MOPRPELQ      X'63'                                        
         BE    VMKRC18                                                          
         BAS   RE,VMKADDEL                                                      
         BNE   VMKRC12             GO BACK AND LOCATE AGAIN                     
         EJECT                                                                  
*==========================================================                     
* UPGRADE ELEMENT                                                               
*==========================================================                     
         SPACE 1                                                                
VMK110   L     R6,AREC3            FIND EXISTING AND DELETE IT                  
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MOUPGELQ     X'61'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMK112   BRAS  RE,NEXTEL                                                        
         BNE   VMK114                                                           
         CLC   2(2,R6),MORECID                                                  
         BNE   VMK112                                                           
         BRAS  RE,DELEL            DELETE EXISTING                              
*                                                                               
VMK114   L     R5,AREC5            FIND NEW ELEM IN STACK                       
*                                                                               
VMK116   CLI   0(R5),0             TEST THERE IS A NEW ELEMENT                  
         BE    VMK130              NO - SKIP                                    
         CLI   0(R5),MOUPGELQ      X'61'                                        
         BE    VMK120                                                           
         LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VMK116                                                           
*                                                                               
VMK120   MVC   ELEM(256),0(R5)     MOVE TO 'ELEM' (LEN AT 1(R5))                
*                                                                               
VMK122   L     R6,AREC3            PUT DEMEL AFTER LAST X'50' ELEM              
****     AH    R6,DSPFRST           AND AFTER X'60' IF PRESENT                  
         AH    R6,DATADISP          AND AFTER X'60' IF PRESENT                  
         SR    R7,R7                                                            
*                                                                               
         MVI   ELCODE,MOBDELQ      X'50'                                        
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
VMK124   BRAS  RE,NEXTEL                                                        
         BNE   VMK126                                                           
         CLC   2(2,R6),MORECID                                                  
         BNE   VMK124                                                           
         LR    R7,R6               SAVE LAST X'50' ADDRESS                      
         B     VMK124                                                           
*                                                                               
VMK126   LTR   R6,R7                                                            
         JZ    *+2                                                              
*                                                                               
VMK128   LLC   R0,1(R6)            ADD DEMO ELEM AFTER X'50'                    
         AR    R6,R0                                                            
         CLI   0(R6),MONTELQ       AFTER X'5E'                                  
         BE    VMK128                                                           
         CLI   0(R6),MODMELQ       AFTER X'60'                                  
         BE    VMK128                                                           
         BAS   RE,VMKADDEL                                                      
         BNE   VMK122              GO BACK AND LOCATE AGAIN                     
         EJECT                                                                  
VMK130   OC    MGMKODA2,MGMKODA2   TEST TWO RECORDS                             
         BZ    VMK140              NO                                           
         GOTOR MYPUTREC            WRITE RECORD 1 NOW                           
*                                                                               
         MVC   KEY+14(4),MGMKODA2                                               
         MVC   MYDA,MGMKODA2       DA HERE FOR LOCAL CABLE                      
         GOTOR MYGETREC            AND GET SECOND REC (INTO AREC3)              
         SPACE 1                                                                
*==============================================================                 
* COMMENTS - NOTE IF TWO RECORDS EXIST, SECOND IS NOW IN AREC3                  
*==============================================================                 
         SPACE 1                                                                
VMK140   MVI   ELCODE,MOBBCELQ     DELETE X'70' COMMENT ELEMS FROM REC          
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         BRAS  RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
VMK142   BRAS  RE,NEXTEL                                                        
         BNE   VMK146                                                           
*                                                                               
VMK143   CLC   2(2,R6),MORECID     MATCH OFFER/REC #                            
         BNE   VMK142                                                           
*                                                                               
VMK144   BRAS  RE,DELEL                                                         
         CLI   0(R6),MOBBCELQ      TEST ANOTHER X'70' COMMENT                   
         BNE   VMK146                                                           
         CLC   2(2,R6),MORECID     MATCH OFFER/REC #                            
         BE    VMK144                                                           
*                                                                               
         L     R6,AREC3                                                         
VMK146   BAS   RE,FNDEOR           FIND&SET R6 TO EOR                           
*                                                                               
* NOW FIND COMMENT ELEMENTS IN STACK                                            
*                                                                               
VMK150   L     R5,AREC5                                                         
         MVI   ELCODE,MOBBCELQ     SET X'70' ELEMENT CODE                       
*                                                                               
VMK152   CLI   0(R5),0             NO MORE COMMENTS?                            
         BE    VMK170               YES                                         
         CLC   0(1,R5),ELCODE                                                   
         BNE   VMK164                                                           
         XC    ELEM,ELEM                                                        
         LLC   RF,1(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ELEM(0),0(R5)       MOVE TO 'ELEM'                               
*                                                                               
VMK154   BAS   RE,VMKADDEL                                                      
         BE    VMK160                                                           
* RECORD JUST SPLIT - NEED TO  FIND EOR AND ADD ELEMENT                         
         L     R6,AREC3                                                         
         BAS   RE,FNDEOR           FIND&SET R6 TO EOR AGAIN                     
         B     VMK154               AND ADD ELEMENT                             
*                                                                               
VMK160   L     R6,AREC3                                                         
         BAS   RE,FNDEOR           FIND&SET R6 TO EOR AGAIN                     
*                                                                               
VMK164   LLC   R0,1(R5)            NEXT ELEMENT IN STACK                        
         AR    R5,R0                                                            
         B     VMK152                                                           
*                                                                               
FNDEOR   DS    0H                  FIND AND SET R6 TO EOR                       
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
FNDEOR10 LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   FNDEOR10                                                         
         BR    RE                                                               
*                                                                               
VMK170   BRAS  RE,MYPUTREC                                                      
*                                                                               
         BRAS  RE,GOODUPD          POINT R1 TO MESSAGE TEXT                     
         MVC   BUYMSG(41),0(R1)                                                 
* IF TWO RECORDS, MAKE SURE FIRST IS IN AREC3/SECOND IN AREC4                   
         OC    MGMKODA2,MGMKODA2   TEST 2 RECORDS                               
         BZ    VMK180                                                           
*                                                                               
         MVC   KEY+14(4),MGMKODA1                                               
         MVC   MYDA,MGMKODA1       DA HERE FOR CABLE                            
         MVC   AREC,AREC3                                                       
         BRAS  RE,MYGETREC                                                      
*                                                                               
         MVC   KEY+14(4),MGMKODA2                                               
         MVC   MYDA,MGMKODA2       DA HERE FOR CABLE                            
         MVC   AREC,AREC4                                                       
         BRAS  RE,MYGETREC                                                      
*                                                                               
VMK180   J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* NEED TO WRITE THE OFFER RECORD WITH THE UPDATED TSAR DATA                     
*==============================================================                 
         SPACE 1                                                                
UPDMKO   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),MGMKODA1  READ FIRST MKO REC                           
         MVC   MYDA,MGMKODA1       DA GOES HERE FOR LOCAL CABLE                 
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         BRAS  RE,MYGETREC                                                      
*                                                                               
UPDMK2   L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MOMBELQ      FIND X'20' MAKEGOOD BUY ELEMENT              
         BRAS  RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
UPDMK4   BRAS  RE,NEXTEL                                                        
         BE    UPDMK5                                                           
         MVI   SVMGINIT,C'N'       SET TO START AGAIN                           
         MVC   NERRCD,=Y(OFFCHANG)                                              
         LA    RE,MGEINP1                                                       
         MVC   0(74,RE),1(RE)                                                   
         MVI   74(RE),0                                                         
         J     ERREXITN                                                         
*                                                                               
UPDMK5   CLC   2(2,R6),MORECID     MATCH OFFER/RECNUM                           
         BNE   UPDMK4                                                           
*                                                                               
         MVC   ELEM(128),0(R6)     SAVE OLD ELEMENT                             
         BRAS  RE,DELEL                                                         
*                                                                               
E        USING MOMBELD,ELEM                                                     
         MVC   E.MOMBDYPT,MODPT                                                 
         MVI   E.MOMBLEN,MOMBOVRH       SET MINIMUM ELEMENT LENGTH              
         CLI   MOPROG,0                 TEST HAVE PROGRAM NAME                  
         BE    UPDMK6                   NO                                      
         MVC   E.MOMBPROG(L'MOPROG),MOPROG                                      
         LA    R0,MOMBOVRH                                                      
         AHI   R0,L'MOPROG                                                      
         STC   R0,E.MOMBLEN             UPDATE ELEM LENGTH                      
         DROP  E                                                                
*                                                                               
UPDMK6   BAS   RE,VMKADDEL                                                      
         BE    UPDMK10                                                          
* FIND THE X'50' ELEM AND INSERT ELEM IN FRONT OF IT!                           
         L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MOBDELQ      FIND CORRESPONDING X'50' ELEM                
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
UPDMK8   BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         CLC   2(2,R6),MORECID     MATCH OFFER/RECNUM                           
         BNE   UPDMK8                                                           
         B     UPDMK6              GO BACK AND TRY AGAIN                        
*                                                                               
UPDMK10  CLI   MOADJ,0             TEST ADJACENCY CODE                          
         BNE   UPDMK12                                                          
         OC    MOCOS2,MOCOS2                                                    
         BNZ   UPDMK12                                                          
         OC    MOREP,MOREP                                                      
         BZ    UPDMKX                                                           
*                                                                               
* TEST SUPPLEMENTAL ELEM PRESENT                                                
UPDMK12  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),MOMBSELQ      X'21'                                        
         BE    UPDMK20                                                          
* NO SUPP ELEM - INSERT IT NOW IN FRONT OF ELEM AT (R6)                         
         XC    ELEM,ELEM                                                        
         MVI   ELEM,MOMBSELQ       X'21'                                        
         MVI   ELEM+1,MOMBSLNQ                                                  
         BAS   RE,VMKADDEL                                                      
*                                                                               
         USING MOMBSELD,R6                                                      
UPDMK20  MVC   MOMBSADJ,MOADJ      SET ADJ CODE IN ELEM                         
         MVC   MOMBSCS2,MOCOS2     SET COST 2                                   
         MVC   MOMBSREP,MOREP      SET REP CODE                                 
*                                                                               
UPDMKX   BRAS  RE,MYPUTREC                                                      
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
VMKADDEL NTR1                                                                   
*                                                                               
VMKADD2  GOTOR MYRECUP,DMCB,AREC,ELEM,(C'R',(R6))                               
         CLI   8(R1),C'R'          TEST OVERFLOW                                
         JE    EXIT                RECUP RETURNS R ON OK/0 FOR ERROR            
         OC    MGMKODA2,MGMKODA2   RECORD SPLIT ALREADY                         
         BZ    VMKADD4             NO                                           
VMKERR   MVC   NERRCD,=Y(TOOMUCH)                                               
         J     ABENDX2             TOO MUCH DATA                                
         SPACE 1                                                                
*==================================================================             
* COPY REC3 TO REC4. DELETE ALL X'70' COMMENTS FROM REC3                        
*                    DELETE ALL BUT X'70' ELEMENTS FROM REC4                    
* REC3 WILL BE PUT BACK TO THE FILE AND                                         
* REC4 WILL BE ADDED TO THE FILE                                                
* NOTE THAT IF WE GET THIS FAR, CURRENT RECORD MUST BE REC3 SINCE               
* SECOND RECORD DID NOT EXIST                                                   
*==================================================================             
         SPACE 1                                                                
VMKADD4  L     RE,AREC3                                                         
         L     RF,AREC4                                                         
         SR    RF,RE                                                            
         L     R0,AREC4                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE TO REC4 FROM REC3                       
*                                                                               
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
VMKADD10 CLI   0(R6),0                                                          
         BE    VMKADD18                                                         
         CLI   0(R6),MOBBCELQ      X'70'                                        
         BNE   VMKADD12                                                         
         GOTOR MYRECUP,DMCB,AREC3,(R6)                                          
         B     VMKADD10                                                         
*                                                                               
VMKADD12 LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VMKADD10                                                         
*                                                                               
VMKADD18 MVC   AREC,AREC3          WRITE NEW VERSION OF REC3                    
         BRAS  RE,MYPUTREC                                                      
*                                                                               
VMKADD20 L     R6,AREC4                                                         
         LA    R0,MOKSEQ-MOKEY                                                  
         CLI   BUYST,C'0'                                                       
         BL    *+8                                                              
         LA    R0,MOXKSEQ-MOXKEY                                                
*                                                                               
         AR    R6,R0               POINT TO SEQNUM FIELD                        
         LLC   R0,0(R6)                                                         
         AHI   R0,1                BUMP SEQUENCE NUMBER                         
         STC   R0,0(R6)                                                         
*                                                                               
         L     R6,AREC4                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
VMKADD22 CLI   0(R6),0                                                          
         BE    VMKADD30                                                         
         CLI   0(R6),MOBBCELQ      X'70'                                        
         BE    VMKADD24                                                         
         GOTOR MYRECUP,DMCB,AREC4,(R6)                                          
         B     VMKADD22                                                         
*                                                                               
VMKADD24 LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VMKADD22                                                         
*                                                                               
VMKADD30 L     R6,AREC4                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         CLI   0(R6),0             SHOULD HAVE AT LEAST ONE COMMENT             
         BE    VMKERR              OTHERWISE, ABEND WITH MESSAGE                
         MVC   AREC,AREC4                                                       
         BRAS  RE,MYADDREC                                                      
         MVC   MGMKODA2,MYDA       SAVE DISK ADDRESS                            
*                                                                               
         MVC   AREC,AREC3          ALL RECORDS READ TO REC3 !                   
         CLI   ELEM,MOBBCELQ       X'70' DO WE NEED REC3 OR REC4 ?              
         BE    VMKADD32            REC4                                         
*                                                                               
         MVC   KEY+14(4),MGMKODA1                                               
         MVC   MYDA(4),MGMKODA1                                                 
*                                                                               
VMKADD32 BRAS  RE,MYGETREC                                                      
         LTR   RB,RB               SET CC NEQ                                   
         J     EXIT                AND GO FIND OUT WHERE TO ADD ELEM            
         EJECT                                                                  
*=====================================================================*         
* TEST GOALS EXIST FOR ALL WEEKS IN THIS TSAR MAKEGOOD NOTICE                   
*=====================================================================*         
         SPACE 1                                                                
TSTGOAL  NTR1  WORK=(R9,TGWORKL)                                                
         USING TGWORKD,R9                                                       
         LA    RE,HALF                                                          
         ST    RE,AGLDATA                                                       
         USING GLDATAD,RE                                                       
         MVC   GLDATA_DPT,MODPT                                                 
         MVC   GLDATA_SLN,MOSLN                                                 
         DROP  RE                                                               
*                                                                               
         LLC   R5,MOWKS            GET NUMBER OF WEEKS                          
*                                                                               
         GOTO1 VDATCON,DMCB,(8,MOSTRT),GOALDT                                   
         B     TSTGL4                                                           
*                                                                               
TSTGL2   GOTO1 VADDAY,DMCB,GOALDT,GOALDT,7                                      
*                                                                               
TSTGL4   GOTO1 VDATCON,DMCB,GOALDT,(2,CGOALDT)                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM+2(2),CGOALDT         FAKE A REGEL                           
         NI    WRKRUPSW,X'FF'-WRKRUPSW_ISDRMG                                   
         GOTO1 TESTGLS,DMCB,ELEM                                                
         OI    WRKRUPSW,WRKRUPSW_ISDRMG                                         
         BCT   R5,TSTGL2                                                        
         XC    AGLDATA,AGLDATA                                                  
         J     EXIT                                                             
         DROP  R9                                                               
*                                                                               
TGWORKD  DSECT                   ** TSTGOAL LOCAL WORKING STORAGE **            
GOALDT   DS    CL6                 TEMP USE YYMMDD FOR GOAL LOOKUPS             
CGOALDT  DS    CL2                 COMP DATE FOR GOAL LOOKUPS                   
TGWORKL  EQU   *-TGWORKD                                                        
T21131   CSECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* DISPLAY SELECTED MAKEGOOD NOTICE                                              
*=====================================================================*         
         SPACE 1                                                                
SELMKN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SELDISP          POINT R1 TO MESSAGE TEXT                     
         MVC   BUYMSG(38),0(R1)                                                 
         TWAXC MKNLINH,MKNCOM5H,TRNS=Y   CLEAR ALL UNP FIELDS                   
         LA    R2,MKNLAST                                                       
         MVI   1(R2),X'01'                                                      
         MVI   2(R2),X'01'                                                      
*                                                                               
SMN10    MVC   MYSTTN,MSSTTN       FOR CABLE, READ THE RIGHT STA/NET            
         BRAS  RE,READMKN          READ THE NOTICE RECORD TO AREC2              
*                                                                               
         LA    R2,MKNLINH                                                       
         SR    R0,R0                                                            
         ICM   R0,3,MSACTBUY                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  10(3,R2),DUB                                                     
*                                                                               
         CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BL    SMN12                                                            
*                                                                               
         UNPK  8(3,R2),DUB                                                      
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MSSTTN                                                 
         GOTO1 STAPACK,DMCB,(C'U',WORK),WORK+10,(X'80',WORK+15)                 
         MVI   11(R2),C'-'                                                      
         MVC   12(3,R2),WORK+20    SHOW NETWORK ONLY!                           
*                                                                               
SMN12    GOTO1 VDATCON,DMCB,MSELDTQ,(4,MKNDATE)                                 
         OI    MKNDATEH+4,X'20'    SET VALID                                    
*                                                                               
         L     RF,VDAYUNPK                                                      
         GOTO1 (RF),(R1),MSDAYS,DUB                                             
         MVC   MKNDAYS,DUB                                                      
*                                                                               
         L     RF,VUNTIME                                                       
         GOTO1 (RF),(R1),MSSTIM,MKNTIME                                         
         OI   MKNTIMEH+4,X'20'     SET VALID                                    
*                                                                               
         MVC   MKNDPT,MSDPT                                                     
*                                                                               
         LLC   R0,MSSLN                                                         
         EDIT  (R0),(3,MKNSLN),ALIGN=LEFT                                       
         OI    MSSLN+4,X'20'                                                    
*                                                                               
         MVC   MKNPROG(L'MSPROG-1),MSPROG                                       
*                                                                               
SMN14    SR    R0,R0                                                            
         ICM   R0,7,MSCOST                                                      
         EDIT  (R0),(9,MKNCOST),2,ALIGN=LEFT                                    
         OI    MKNCOSTH+4,X'20'                                                 
         SPACE 1                                                                
*================================================================               
* PROTECT DATE/TIME/SLN/COST IF ACTUAL BUY KNOWN                                
*================================================================               
         SPACE 1                                                                
         LA    R2,MKNDATEH                                                      
         OI    1(R2),X'20'            PROTECT                                   
         NI    6(R2),X'FF'-X'08'      NORMAL INT                                
         LA    R2,MKNTIMEH                                                      
         OI    1(R2),X'20'                                                      
         NI    6(R2),X'FF'-X'08'                                                
         LA    R2,MKNSLNH                                                       
         OI    1(R2),X'20'                                                      
         NI    6(R2),X'FF'-X'08'                                                
         LA    R2,MKNCOSTH                                                      
         OI    1(R2),X'20'                                                      
         NI    6(R2),X'FF'-X'08'                                                
*                                                                               
         OC    MSACTBUY,MSACTBUY                                                
         BNZ   SMN20                                                            
*                                                                               
         LA    R2,MKNDATEH                                                      
         NI    1(R2),X'FF'-X'20'      UNPROTECT                                 
         OI    6(R2),X'08'            HIGH INT                                  
         LA    R2,MKNTIMEH                                                      
         NI    1(R2),X'FF'-X'20'      UNPROTECT                                 
         OI    6(R2),X'08'            HIGH INT                                  
         LA    R2,MKNSLNH                                                       
         NI    1(R2),X'FF'-X'20'      UNPROTECT                                 
         OI    6(R2),X'08'            HIGH INT                                  
         LA    R2,MKNCOSTH                                                      
         NI    1(R2),X'FF'-X'20'      UNPROTECT                                 
         OI    6(R2),X'08'            HIGH INT                                  
*                                                                               
         LA    R0,5                                                             
         LA    R2,MKNCOM1H                                                      
         OI    1(R2),X'20'         PROTECT COMMENTS                             
         AHI   R2,MKNCOM2H-MKNCOM1H                                             
         BCT   R0,*-8                                                           
         B     SMN40                                                            
         SPACE 1                                                                
*==============================================================                 
* NOW DISPLAY GROUP COMMENTS FROM OFFER REC IN REP COMMENTS                     
* TSAR RECORD NEEDS TO BE RESTORED AFTER TSAR READS                             
*==============================================================                 
         SPACE 1                                                                
SMN20    XC    DMCB,DMCB                                                        
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB                                                        
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         NI    SVSTAT,X'FF'-X'80'                                               
         TM    FATSTAT6,X'80'      TEST STEREO USER                             
         BNO   *+8                                                              
         OI    SVSTAT,X'80'                                                     
         DROP  RE                                                               
*                                                                               
         LA    R2,MKNRC1H              FIRST REP COMMENT FIELD                  
         XC    TSARREC,TSARREC                                                  
         MVI   MGCTYPE,MGCTYPEQ                                                 
         MVI   T.TSACTN,TSARDH                                                  
*                                                                               
SMN22    BRAS  RE,CALLTSAR                                                      
         CLI   MGCTYPE,MGCTYPEQ                                                 
         BNE   SMN24                                                            
*                                                                               
         MVC   8(75,R2),MGCCOM                                                  
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         XMT                                          
         TM    SVSTAT,X'80'        TEST STEREO                                  
         BZ    *+8                                                              
         NI    1(R2),X'FF'-X'20'   UNPROTECT FOR CUT/PASTE                      
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,MKNSTATH         ONLY HAVE 3 LINES FOR REP COMMENTS           
         CR    R2,R0                                                            
         BNL   SMN24               CAN'T SHOW MORE THAN 3 LINES                 
         MVI   T.TSACTN,TSANXT                                                  
         B     SMN22                                                            
*                                                                               
SMN24    MVC   T.TSRNUM,MGTSRNUM   CURRENT TSAR RECORD NUMBER                   
         MVI   T.TSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         SPACE 1                                                                
*==============================================================                 
* NOW DISPLAY BUY COMMENTS                                                      
*==============================================================                 
         SPACE 1                                                                
SMN25    LA    R0,5                                                             
         LA    R2,MKNCOM1H                                                      
         NI    1(R2),X'FF'-X'20'       UNPROTECT COMMENTS                       
         AHI   R2,MKNCOM2H-MKNCOM1H                                             
         BCT   R0,*-8                                                           
*                                                                               
         LA    R2,MKNCOM1H                                                      
         L     R6,AREC2                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
SMN26    CLI   0(R6),0             EOR?                                         
         BE    SMN30                                                            
         CLI   0(R6),MNMBCELQ      X'70' - MISSED SPOT COMMENT ELEM             
         BNE   SMN27                                                            
         CLC   MSACTBUY+1(1),2(R6)  MATCH BUYLINE                               
         BNE   SMN29                                                            
         CLI   MSACTBUY,0          MAKE SURE MSACTBUY IS NOT > 255              
         BNE   SMN29                                                            
         LA    RF,MNMBCTXT-MNMBCELD(R6)                                         
         LLC   RE,1(R6)                                                         
         AHI   RE,-5               SET FOR EX                                   
         BM    SMN29               -EMPTY LINE??? SKIP IT!                      
         B     SMN28                                                            
*                                                                               
SMN27    CLI   0(R6),MNB2CELQ      X'75' - MISSED 2 BYTE BYLN COMMENT           
         BNE   SMN29                                                            
         CLC   MSACTBUY,MNB2CBUY-MNB2CELD(R6)  MATCH BUYLINE                    
         BNE   SMN29                                                            
         LA    RF,MNB2CTXT-MNB2CELD(R6)                                         
         LLC   RE,1(R6)                                                         
         AHI   RE,-6               SET FOR EX                                   
         BM    SMN29               -EMPTY LINE??? SKIP IT!                      
*                                                                               
SMN28    EX    RE,*+4                                                           
         MVC   8(0,R2),0(RF)                                                    
         AHI   R2,MKNCOM2H-MKNCOM1H                                             
*                                                                               
SMN29    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SMN26                                                            
*                                                                               
SMN30    LA    R0,MKNCOM1H                                                      
         CR    R2,R0               TEST ANYTHING DISLAYED                       
         BNE   SMN40               YES                                          
* READ THE BUYLINE                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+6(3),MSSTTN                                                  
         MVC   KEY+11(2),MSACTBUY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,BDELEM                                                        
SMN32    MVI   ELCODE,X'66'                                                     
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   SMN40                                                            
*                                                                               
         LLC   RE,1(R6)                                                         
         AHI   RE,-4               SET FOR EX                                   
         BM    SMN32                                                            
         CHI   RE,L'MKNCOM1-1      MAKE SURE WE DON'T BLOW THE FIELD            
         BNL   SMN35                                                            
         EX    RE,*+4                                                           
         MVC   8(0,R2),3(R6)                                                    
SMN34    AHI   R2,MKNCOM2H-MKNCOM1H                                             
         B     SMN32                                                            
SMN35    MVC   8(L'MKNCOM1,R2),3(R6)                                            
         B     SMN34                                                            
*                                                                               
* PROTECT ALL INPUT FIELDS UNLESS OFFER STAT IS NEW OR AMENDED                  
*                                                                               
SMN40    CLI   SVMNSTAT,MNSTNEW                                                 
         BE    SELMNX                                                           
         CLI   SVMNSTAT,MNSTAMND                                                
         BE    SELMNX                                                           
*                                                                               
         LA    R2,MKNLINH                                                       
*                                                                               
SMN42    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SMN44                                                            
         OI    1(R2),X'20'         PROTECT                                      
         B     SMN42                                                            
*                                                                               
SMN44    MVC   0(3,R2),=X'000101'                                               
*                                                                               
SELMNX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* VALIDATE DATA ON MKN (F5) SCREEN                                              
*=====================================================================*         
         SPACE 1                                                                
VALMKN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYSTTN,MSSTTN       FOR CABLE, NEED STA/NET                      
         BRAS  RE,READMKN          READ NOTICE RECORD                           
*                                                                               
         OC    MSACTBUY,MSACTBUY   TEST ACTUAL BUY KNOWN                        
         BNZ   VMN40               YES -  GO EDIT COMMENTS (IF ANY)             
* SEE IF SOME INPUT FIELD WAS CHANGED                                           
         TM    MKNDATEH+4,X'20'                                                 
         BZ    VMN10                                                            
         TM    MKNTIMEH+4,X'20'                                                 
         BZ    VMN10                                                            
         TM    MKNSLNH+4,X'20'                                                  
         BZ    VMN10                                                            
         TM    MKNCOSTH+4,X'20'                                                 
         BZ    VMN10                                                            
         MVC   NERRCD,=Y(NOCHNGS)                                               
         J     ERREXITN                                                         
         SPACE 1                                                                
*=============================================================                  
* EDIT MISSED DATE - RESULT IN BUELDT                                           
*=============================================================                  
VMN10    MVC   BUYKEY,SVKEY        INITIALIZE FOR ELDTEDT                       
         MVC   BUYKEY+6(3),MSSTTN                                               
         LA    R2,MKNDATEH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
         SPACE 1                                                                
*=============================================================                  
* EDIT TIME - RESULT IN BUTIME                                                  
*=============================================================                  
         LA    R2,MKNTIMEH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,TIMEDT                                                    
         GOTO1 CALLEDT                                                          
         SPACE 1                                                                
*=============================================================                  
* EDIT SLN - RESULT IN BUSLN                                                    
*=============================================================                  
         SPACE 1                                                                
         LA    R2,MKNSLNH                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,SLNEDT                                                    
         GOTO1 CALLEDT                                                          
         SPACE 1                                                                
*=============================================================                  
* EDIT COST - RESULT IN BUCOST                                                  
*=============================================================                  
         SPACE 1                                                                
         LA    R2,MKNCOSTH                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,COSTEDT                                                   
         GOTO1 CALLEDT                                                          
         SPACE 1                                                                
*=============================================================                  
* IF MSACTBUY 0, BUYLINE DATA WAS EDITED.                                       
* IF NOT, ONLY COMMENTS COULD BE INPUT.                                         
*=============================================================                  
         SPACE 1                                                                
         OC    MSACTBUY,MSACTBUY   TEST ACTUAL LINE KNOWN                       
         BNZ   VMN40               GO EDIT COMMENTS                             
*                                                                               
         L     R6,AREC2                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MNMSELQ      X'10' - MISSED BUYLINE ELEMENT               
         BRAS  RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
VMN20    BRAS  RE,NEXTEL                                                        
         BNE   VMN22                                                            
         USING MNMSELD,R6                                                       
         CLI   MNMSBLIN,0          CANNOT CHANGE KNOWN BUYLINE                  
         BNE   VMN20                                                            
         OC    MNMSBLN2,MNMSBLN2                                                
         BNZ   VMN20                                                            
*                                                                               
         CLC   MSSEQNUM,MNMSSEQN   MATCH SEQNUM                                 
         BNE   VMN20                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUELDT),(19,MNMSBDAT)                            
         MVC   MNMSSTIM(4),BUTIME  START/END TIME                               
         MVC   MNMSTSLN,BUSLN                                                   
         MVC   MNMSCOST,BUCOST                                                  
         B     VMN20               SEE IF MORE THAN 1 SPOT                      
*                                                                               
* WRITE THE NOTICE RECORD BACK TO THE FILE                                      
*                                                                               
VMN22    BRAS  RE,MYPUTREC                                                      
*                                                                               
         MVI   T.TSACTN,TSAINI     REINITIALIZE TSAR                            
         OI    T.TSINDS,TSIREUSE   REUSE                                        
         BRAS  RE,CALLTSAR                                                      
         MVI   MGFLAG,C'E'         SET FLAG TO REDISPLAY OFFER                  
         J     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* VALIDATE MAKEGOOD NOTICE COMMENTS AND UPDATE NOTICE RECORD                    
*=============================================================                  
         SPACE 1                                                                
VMN40    L     R6,AREC2            DELETE ALL MISSED BUYLINE COMMENTS           
****     AH    R6,DSPFRST             FOR THE BUYLINE # IN MSACTBUY             
         AH    R6,DATADISP            FOR THE BUYLINE # IN MSACTBUY             
*                                                                               
VMN42    CLI   0(R6),0                                                          
         BE    VMN50                                                            
         CLI   0(R6),MNMBCELQ      X'70' - 1-BYTE LINE MISSED COM ELEM          
         BNE   VMN44                                                            
         CLC   MSACTBUY+1(1),MNMBCBUY-MNMBCELD(R6)   MATCH BUYLINE              
         BNE   VMN48                                                            
         CLI   MSACTBUY,0          BUYLINE CAN'T BE > 255 FOR THIS ELEM         
         BNE   VMN48                                                            
         B     VMN46                                                            
*                                                                               
VMN44    CLI   0(R6),MNB2CELQ      X'75' - 2-BYTE LINE MISSED COM ELEM          
         BNE   VMN48                                                            
         CLC   MSACTBUY,MNB2CBUY-MNB2CELD(R6)   MATCH BUYLINE                   
         BNE   VMN48                                                            
*                                                                               
VMN46    GOTOR MYRECUP,DMCB,AREC2,(R6)                                          
         B     VMN42                                                            
*                                                                               
VMN48    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VMN42                                                            
********                                                                        
* NOW WE CAN ADD NEW ONES                                                       
********                                                                        
VMN50    XC    ELEM,ELEM                                                        
         MVI   ELEM,MNMBCELQ       X'70' - MISSED BUYLINE COMMENT ELEM          
         MVC   ELEM+MNMBCBUY-MNMBCELD(1),MSACTBUY+1  SET BUYLINE #              
*                                                                               
         CLI   SV1OR2,2                                                         
         BNE   VMN51                                                            
         MVI   ELEM,MNB2CELQ       X'75' - MISSED 2 BYTE BYLN COMMENT           
         MVC   ELEM+MNB2CBUY-MNB2CELD(2),MSACTBUY  SET BUYLINE #                
*                                                                               
VMN51    LA    R2,MKNCOM1H                                                      
*                                                                               
VMN52    LA    RE,ELEM+MNMBCLIN-MNMBCELD                                        
         CLI   SV1OR2,2                                                         
         BNE   *+8                                                              
         LA    RE,ELEM+MNB2CLIN-MNB2CELD                                        
*                                                                               
         LLC   RF,0(RE)            INCREMENT COMMENT NUMBER                     
         AHI   RF,1                                                             
         STC   RF,0(RE)                                                         
*                                                                               
         ICM   RF,1,5(R2)                                                       
         BZ    VMN55                                                            
         BCTR  RF,0                                                             
         LA    RE,ELEM+MNMBCTXT-MNMBCELD                                        
         CLI   SV1OR2,2                                                         
         BNE   *+8                                                              
         LA    RE,ELEM+MNB2CTXT-MNB2CELD                                        
*                                                                               
         EX    RF,*+4                                                           
         MVC   0(0,RE),8(R2)                                                    
*                                                                               
         CLI   SV1OR2,2                                                         
         BE    *+12                                                             
         AHI   RF,MNMBCOVH                                                      
         B     *+8                                                              
         AHI   RF,MNB2COVH                                                      
         STC   RF,ELEM+1           SET ELEMENT LENGTH                           
*                                                                               
         MVC   NERRCD,=Y(TOOMUCH)                                               
         GOTOR MYRECUP,DMCB,AREC2,ELEM,(C'R',(R6))                              
         CLI   8(R1),C'R'                                                       
         JNE   ABENDX2                                                          
         LLC   R0,1(R6)            NEXT COMMENT GOES AFTER THIS ONE             
         AR    R6,R0                                                            
*                                                                               
VMN55    AHI   R2,MKNCOM2H-MKNCOM1H NEXT COMMENT                                
         LA    R0,MKNCOM5H                                                      
         CR    R2,R0                                                            
         BNH   VMN52                                                            
*                                                                               
         BRAS  RE,MYPUTREC                                                      
*                                                                               
         BRAS  RE,GOODUPD          POINT R1 TO MESSAGE TEXT                     
         MVC   BUYMSG(41),0(R1)                                                 
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* GET DEMO NAMES INTO ELEM                                                      
*================================================================               
                                                                                
GETDEMNS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM           CLEAR OUTPUT AREA                            
         LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
*                                                                               
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'                                                   
         BNE   GDMN2                                                            
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    GDMN2                                                            
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
GDMN2    XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AE0'  DEMOCON                                  
         GOTO1 VCALLOV,DMCB                                                     
*                                                                               
         L     RF,DMCB                                                          
         LHI   RE,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RE,RA                                                            
         STCM  RE,15,DMCB+16                                                    
*                                                                               
         GOTO1 (RF),(R1),(14,(R4)),(6,ELEM),(C'S',ADBLOCK),SVUSRNMS             
*                                                                               
         MVC   MGDEMNM1(36),ELEM   SAVE FIRST 6 DEMO NAMES                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* ABEND EXIT ROUTINES                                                           
*=====================================================================*         
         SPACE 1                                                                
ABENDX   CLI   ERRCD,NEWERRS       IS THIS A 2 CHAR ERROR                       
         JNE   ABENDX4                                                          
*                                                                               
ABENDX2  XC    WORK,WORK           DEFINE CONTROL BLOCK                         
         LA    R1,WORK                                                          
         USING GETTXTD,R1                                                       
         MVI   GTMTYP,GTMERR       SET MESSAGE TYPE TO ERRO                     
         MVC   GTMSGNO,NERRCD      AND MESSAGE NUMBER                           
         MVI   GTMSYS,3            AND MESSAGE SYSTEM                           
         DROP  R1                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),WORK           PUT OUT SYSTEM MESSAGE                       
         J     ABENDX10                                                         
*                                                                               
ABENDX4  MVC   ERRCD,BMGEERR+1                                                  
         LA    R1,DMCB+12                                                       
         LA    RE,BUYMSG                                                        
         ST    RE,0(R1)                                                         
         MVC   0(1,R1),ERRCD                                                    
         LA    RE,DMCB                                                          
         ST    RE,4(R1)                                                         
         MVI   4(R1),X'FF'                                                      
         MVI   8(R1),3             SET SYSTEM NUMBER                            
         GOTO1 VGETMSG                                                          
         OI    BUYMSGH+6,X'80'                                                  
         OI    BUYINP1H+6,X'C0'    POSITION CURSOR                              
*                                                                               
ABENDX10 TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK                                     
         JNZ   *+2                 NO $ABEND FOR SPOT DESKTOP OR MM             
         DC    H'0',C'$ABEND'      AND UNWIND                                   
         EJECT                                                                  
*                                                                               
INVLFLD  MVI   ERRCD,INVERR        INVALID DATA                                 
         J     ERREXIT                                                          
         SPACE 1                                                                
*===========================================================                    
* NOTE - BE CAREFUL HERE                                                        
* CALLERS JUMP TO THIS ROUTINE, SO NO BASE REGISTER INSTRUCTIONS                
* WILL EXECUTE SUCCESSFULLY !                                                   
*===========================================================                    
         SPACE 1                                                                
*                                                                               
ERREXITK MVI   ERRCD,NEWERRS                                                    
ERREXITL MVI   BYTE,C'N'           SET DO NOT RESET                             
         J     ERREXIT2                                                         
*                                                                               
ERREXITN MVI   ERRCD,NEWERRS                                                    
*                                                                               
ERREXIT  MVI   BYTE,0              RESET INPUT STATUS                           
*                                                                               
ERREXIT2 LHI   RE,SVMGSAVE-BUYSAVE   SAVE MGE DATA                              
         AR    RE,RA                                                            
         LHI   RF,SVMGSAVX-SVMGSAVE                                             
         LA    R0,MGSAVE                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
* SAVE TSAR BUFFERS                                                             
         CLI   SVMGINIT,C'E'       TEST TSAR INITIALIZED                        
         JNE   ERREXIT4                                                         
         MVI   T.TSACTN,TSASAV                                                  
         BRAS  RE,CALLTSAR                                                      
*                                                                               
* REMOVE * FROM FIRST CHAR OF INPUT LINE 1 ON F3 SCREEN                         
*                                                                               
ERREXIT4 CLI   BYTE,C'N'           TEST DO NOT RESET                            
         JE    ERREXIT6                                                         
         CLI   SVSCR,X'F3'                                                      
         JNE   ERREXIT5                                                         
         LA    RE,MGEINP1                                                       
         CLI   0(RE),C'*'                                                       
         JNE   ERREXIT5                                                         
         MVC   0(74,RE),1(RE)                                                   
         MVI   74(RE),0                                                         
*                                                                               
ERREXIT5 NI    MGEINP1H+4,X'DF'    SET NOT VALIDATED                            
         OI    MGEINP1H+6,X'80'    XMT                                          
*                                                                               
ERREXIT6 GOTO1 ERROR                                                            
*                                                                               
YES      SR    RC,RC                                                            
EQXIT    EQU   YES                                                              
*                                                                               
NO       LTR   RC,RC                                                            
NEQXIT   EQU   NO                                                               
*                                                                               
EXIT     DS    0H                                                               
XIT      XIT1                                                                   
                                                                                
*===========================================================                    
* CALL SPBUY38 AND EXIT                                                         
*===========================================================                    
                                                                                
GOSKEVAL NTR1  BASE=*,LABEL=*                                                   
         L     RF,VGOBUY38                                                      
         GOTO1 (RF),DMCB,(RC),(R8)                                              
         J     EXIT                                                             
         SPACE 1                                                                
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         LA    RE,64                   TSAR BLOCK + TSARKEY                     
         STC   RE,TSARTRC              SET DATA LENGTH IN BYTE 0                
*                                                                               
         CLI   T.TSACTN,TSAGET                                                  
         JE    CALLTS4                                                          
         CLI   T.TSACTN,TSANXT                                                  
         JE    CALLTS4                                                          
*                                                                               
CALLTS2  GOTO1 VDATAMGR,DMCB,QDMTRACE,QDMDATA,TSARTRC                           
         ORG   *-2                                                              
         DC    X'0700'                 0DEF TO ENABLE TRACE                     
*                                                                               
CALLTS4  GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         CLI   T.TSACTN,TSAGET                                                  
         JE    CALLTS6                                                          
         CLI   T.TSACTN,TSANXT                                                  
         JNE   CALLTS8                                                          
*                                                                               
CALLTS6  GOTO1 VDATAMGR,DMCB,QDMTRACE,QDMDATA,TSARTRC                           
         ORG   *-2                                                              
         DC    X'0700'                 0DEF TO ENABLE TRACE                     
*                                                                               
CALLTS8  CLI   T.TSERRS,0              SET CC ON EXIT                           
         LR    RE,R0                                                            
         BER   RE                                                               
* AN ERROR HAS OCCURRED - MESSAGE IF TSAR OVERFLOW ON ADD                       
         CLI   T.TSACTN,TSAADD     TEST ACTION = ADD                            
         JNE   CALLTSX                                                          
         TM    T.TSERRS,X'80'      TEST EOF                                     
         JZ    CALLTSX                                                          
*                                                                               
         LHI   R0,TSAROVFL                                                      
         STCM  R0,3,NERRCD                                                      
*&&DO                                                                           
         CLI   MGFLAG,C'J'         REJECT, ALLOW THE DISPLAY                    
         BE    CALLTSX                                                          
*&&                                                                             
         CLI   MGFLAG,C'E'         IF EVALUATE OR                               
         JNE   ABENDX                                                           
         OI    MGMSCFLG,MGTSROVF   TSAR OVERFLOW                                
*                                                                               
         J     ERREXITK                                                         
*                                                                               
CALLTSX  LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         EJECT                                                                  
*======================================================================         
* VALIDATES THE INPUT LINE                                                      
*                                                                               
* INPUT STRING LOOKS LIKE MGE(XXX)(01)=AA,PRD-PR2                               
*              WHERE XXX=ACC,APP,REJ                                            
*                                                                               
* ON EXIT:     MGFLAG = E  FOR EVALUATE                                         
*                       A  FOR APPROVE                                          
*                       J  FOR REJECT                                           
*                       X  FOR NEXT                                             
*                       U  FOR UNPEND (RESET MGPENDING FLAGS)                   
*======================================================================         
         SPACE 1                                                                
VALINP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   MGFLAG,0            CLEAR                                        
         MVI   MGFLTCOD,0          RESET FLIGHT CODE                            
*                                                                               
         LA    R2,MGEINP1H         INPUT LINE                                   
         CLI   PFKEY,0             NO POINT IF NO PFKEY                         
         BE    VLN10                                                            
         CLC   =C'MGE=',8(R2)      STILL MGE                                    
         BE    VLN10                                                            
         CLC   8(4,R2),=C'*MGE'                                                 
         BNE   VLN10                                                            
         CLI   12(R2),C'='                                                      
         BE    VLN2                                                             
         CLC   12(2,R2),=C'00'     INPUT MAY LOOK LIKE MGE99=                   
         BL    VLN10               BUT NOT UNLESS IT'S NUMERIC                  
         CLI   14(R2),C'='                                                      
         BNE   VLN10                                                            
* CAN USE PFKEY FOR MGEAPP OR MGEREJ OR MGESAP IF *MGE                          
VLN2     LLC   R0,PFKEY                                                         
         CLI   PFKEY,12                                                         
         BNH   *+8                                                              
         AHI   R0,-12                                                           
         STC   R0,PFKEY                                                         
*                                                                               
         LA    R1,=C'APP'                                                       
         CLI   PFKEY,PFKEYAPP                                                   
         BE    VLN4                                                             
         LA    R1,=C'REJ'                                                       
         CLI   PFKEY,PFKEYREJ                                                   
         BE    VLN4                                                             
         CLI   SVDARPRF+13,C'N'    SELF-APPLY ON?                               
         BE    VLN3                                                             
         LA    R1,=C'SAP'                                                       
         CLI   PFKEY,PFKEYSAP                                                   
         BE    VLN4                                                             
*                                                                               
* UNPEND FOR DDS TERMINALS ONLY                                                 
VLN3     CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   VLN10                                                            
         LA    R1,=C'UNP'                                                       
         CLI   PFKEY,PFKEYUNP                                                   
         BNE   VLN10                                                            
*                                                                               
VLN4     LLC   RE,5(R2)            GET INPUT LENGTH                             
         L     RF,AREC3                                                         
         MVC   0(78,RF),8(R2)      SAVE THE INPUT LINE                          
         MVC   12(3,R2),0(R1)      CREATE *MGEXXX                               
         AHI   RE,-5               ADJUST FOR *MGE AND BCTR                     
*                                                                               
         CHI   RE,70               CAN'T RESTORE MORE THAN 71 CHARS             
         BNH   *+8                                                              
         LHI   RE,70                                                            
*                                                                               
         EX    RE,*+4              GIVES LENGTH TO RESTORE                      
         MVC   15(0,R2),4(RF)      RESTORE REMAINDER OF LINE                    
*                                                                               
         LLC   RE,5(R2)                                                         
         AHI   RE,3                ADJUST INPUT LENGTH                          
         STC   RE,5(R2)                                                         
         OI    6(R2),X'80'         TRANSMIT LINE BACK TO SCREEN                 
*                                                                               
VLN10    L     R4,AREC3            POINT TO PARSNIP TABLE                       
         XC    0(256,R4),0(R4)                                                  
*                                                                               
         L     RF,VCOMFACS         SCAN THE INPUT STRING                        
         L     RF,CPARSNIP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(5,(R4)),('PSNNPARQ',PSNPLIST)                    
         MVC   NERRCD,=Y(BADINPUT)                                              
         CLI   DMCB+8,0                                                         
         BNE   VALINERR                                                         
*                                                                               
         MVC   NERRCD,=Y(NOTMGE)                                                
         USING PSND,R4                                                          
         CLI   0(R4),0                                                          
         BE    VALINERR                                                         
*                                                                               
         CLI   0(R4),C'F'          FIELD?                                       
         BNE   VALINERR                                                         
*                                                                               
         CLI   PSNLEN,3            SHOULD BE MGE,*MGE,MGEXXX,*MGEXXX            
         BL    VALINERR                      MGEXXX01, OR *MGEXXX01             
         CLI   PSNLEN,9                                                         
         BH    VALINERR                                                         
*                                                                               
         ZIC   R1,PSNLEN           R1 = L(MGE STRING BEFORE '=')                
         L     RE,PSNCOMP                                                       
         CLI   0(RE),C'*'                                                       
         BNE   *+10                                                             
         LA    RE,1(RE)                                                         
         BCTR  R1,0                R1 = L(MGE STRING B040 '=' W/O '*')          
*                                                                               
         CLC   =C'MGE',0(RE)                                                    
         BNE   VALINERR                                                         
*                                                                               
         CHI   R1,5                R1 > 6 IF INPUT IS MGEXXX                    
         BH    VLN22               AND THERE IS NO FLIGHT                       
*                                                                               
         LA    RE,3(RE)            ELSE ADJUST INPUT POINTER                    
         AHI   R1,-3               AND LENGTH                                   
         B     VLN30               AND GO SEE IF THERE IS A FLIGHT              
*                                                                               
VLN22    NI    MGMSCFLG,X'FF'-MGMFSAPP                                          
         MVI   MGFLAG,C'A'                                                      
         CLC   =C'MGEAPP',0(RE)    MAKEGOOD APPROVAL?                           
         BE    VLN24                                                            
*                                                                               
         MVI   MGFLAG,C'J'                                                      
         CLC   =C'MGEREJ',0(RE)    MAKEGOOD REJECTION?                          
         BE    VLN24                                                            
*                                                                               
         MVI   MGFLAG,C'U'                                                      
         CLC   =C'MGEUNP',0(RE)    MAKEGOOD UNPEND ?                            
         BE    VLN24                                                            
*                                                                               
         MVI   MGFLAG,C'G'                                                      
         CLC   =C'MGEZZT',0(RE)    GENERATE BUYS FROM ON HOLD STUFF             
         BE    VLN24                                                            
*                                                                               
         MVI   MGFLAG,C'S'                                                      
         CLC   =C'MGESAP',0(RE)    SELF APPLY?                                  
         BNE   VLN23                                                            
         OI    MGMSCFLG,MGMFSAPP                                                
         B     VLN24                                                            
*                                                                               
VLN23    MVC   NERRCD,=Y(NOTAPRJ)                                               
         CLI   T211FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   VALINERR                                                         
         MVI   MGFLAG,C'C'         SET 'ACCEPT'                                 
         CLC   =C'MGEACC',0(RE)                                                 
         BNE   VALINERR                                                         
*                                                                               
VLN24    LA    RE,6(RE)            RE = A(AFTER THE MGE STRING)                 
         AHI   R1,-6                                                            
*                                                                               
VLN30    LTR   R1,R1               ANYTHING FOLLOWING MGE LIKE FLIGHT?          
         BZ    VLN40               NO                                           
*                                                                               
         LR    RF,RE               MAKE SURE WE HAVE A FLIGHT NUMBER            
         LR    R0,R1                                                            
*                                                                               
         MVC   NERRCD,=Y(BADFLITE)                                              
VLN32    CLI   0(RF),C'0'                                                       
         JL    ERREXITN                                                         
         CLI   0(RF),C'9'                                                       
         JH    ERREXITN                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,VLN32                                                         
*                                                                               
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)         YES, GET THE BINARY VALUE                    
         CVB   R0,DUB                                                           
         STC   R0,MGFLTCOD         SAVE THE FLIGHT NUMBER                       
*                                                                               
VLN40    LA    R4,32(R4)           POINT TO GROUP CODE ENTRY                    
*                                                                               
         MVC   NERRCD,=Y(BADGROUP)                                              
         CLI   PSNLEN,0            IT MUST BE THERE                             
         BE    VALINERR                                                         
*                                                                               
         MVI   SVCSHTRD,0          CLEAR CASH/TRADE INPUT                       
         L     RE,PSNCOMP          GET THE REP GROUP CODE                       
         LLC   RF,PSNLEN                                                        
         CHI   RF,3                IF TEXT AFTER "MGE=" 3 CHARACTERS?           
         BL    VLN45                 LESS THAN 3                                
         BH    VLN43                 MORE THAN 3                                
         TM    PSNSTAT,PSNALNQ     X'20' - IS IT ALPHANUMERIC??                 
         BNZ   VLN45               YES, SO WE KNOW NOT /C NOR /T                
VLN43    AHI   RF,-2               ADJUST GROUP LENGTH FOR /C /T                
         LA    R1,0(RE,RF)         POINT TO END OF GROUP CODE                   
         CLI   0(R1),C'/'          IS IT CASH/TRADE DIVIDER                     
         JNE   VALINERR                                                         
         MVC   SVCSHTRD,1(R1)      SAVE THE C OR T                              
*                                                                               
         CLI   1(R1),C'T'                                                       
         BE    VLN45                                                            
         CLI   1(R1),C'C'                                                       
         BE    VLN45                                                            
         MVC   NERRCD,=Y(NOCSHTRD)                                              
         B     VALINERR                                                         
*                                                                               
VLN45    MVC   MGGRPCOD,SPACES                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   MGGRPCOD(0),0(RE)                                                
         OC    MGGRPCOD,SPACES                                                  
         EJECT                                                                  
VLN50    XC    MGQPRD1(8),MGQPRD1 CLEAR EBC/BINARY PRDS                         
*                                                                               
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
*                                                                               
         CLC   =C'POL',BUYPR        IF HEADLINE PRD IS POL                      
         BE    VLN60                PRODUCTS MUST BE ENTERED                    
* USE HEADLINE PRODUCT                                                          
         MVC   MGQPRD1,BUYPR                                                    
         LA    R1,MGQPRD1                                                       
         BRAS  RE,GETBPRD                                                       
         JNE   *+2                                                              
         MVC   MGBPRD1,0(R1)                                                    
         B     VLN70                                                            
*                                                                               
         EJECT                                                                  
*===================================================================            
* EDIT PRODUCT CODES                                                            
*===================================================================            
         SPACE 1                                                                
VLN60    L     R7,AMGWORK                                                       
         USING MGWORK,R7                                                        
*                                                                               
         MVC   NERRCD,=Y(MGEPRREQ)  YES, NEED A PRD CODE FOR MGE                
         CLI   PSNTAG,C'F'         IS THIS A 'FIELD' ?                          
         BNE   VALINERR                                                         
*                                                                               
         CLI   PSNLEN,2            PRODUCTS HAVE A LENGTH OF 2 OR 3             
         BL    VALINERR                                                         
         CLI   PSNLEN,7                                                         
         BH    VALINERR                                                         
*                                                                               
         L     RE,PSNCOMP          SAVE PRODUCT                                 
         MVC   MGQPRD1,0(RE)                                                    
         CLI   MGQPRD1+2,C','      IF TWO BYTE PRODUCT (SINGLE)                 
         BE    *+12                                                             
         CLI   MGQPRD1+2,C'-'        OR TWO BYTE PRIMARY PRODUCT (PB)           
         BNE   *+8                                                              
         MVI   MGQPRD1+2,C' '      REPLACE LAST BYTE WITH A SPACE               
         MVC   SVMGPRD1,MGQPRD1                                                 
*                                                                               
         MVC   NERRCD,=Y(BADMGPRD)                                              
         LA    R1,MGQPRD1                                                       
         BRAS  RE,GETBPRD                                                       
         BNE   VALINERR                                                         
         MVC   MGBPRD1,0(R1)                                                    
*                                                                               
         CLI   PSNLEN,4            GOT A PIGGYBACK?                             
         BNH   VLN65               NO                                           
*                                                                               
         L     R1,PSNCOMP          SAVE PRODUCT                                 
         LA    RE,4(R1)            POINT TO 2ND PRD                             
         CLI   2(R1),C'-'          BUT IF 1ST PRD ONLY 2 BYTES                  
         BNE   *+8                                                              
         LA    RE,3(R1)            POINT TO 2ND PRD HERE                        
*                                                                               
         MVC   MGQPRD2,0(RE)                                                    
         CLI   MGQPRD2+2,C','      2 BYTE PIGGYBACK?                            
         BNE   *+8                                                              
         MVI   MGQPRD2+2,C' '                                                   
         OC    MGQPRD2,SPACES                                                   
         MVC   SVMGPRD2,MGQPRD2                                                 
*                                                                               
         MVC   NERRCD,=Y(BADMGPR2)                                              
         LA    R1,MGQPRD2                                                       
         BRAS  RE,GETBPRD                                                       
         BNE   VALINERR                                                         
         MVC   MGBPRD2,0(R1)                                                    
*                                                                               
* SET LOWER BINARY PRODUCT CODE FIRST (EVEN IF IT'S 0)                          
*                                                                               
VLN65    LA    RF,SVMGBPR1                                                      
         CLC   0(1,RF),1(RF)                                                    
         BL    VLN67                                                            
         IC    R0,0(RF)                                                         
         MVC   0(1,RF),1(RF)                                                    
         STC   R0,1(RF)                                                         
         DROP  R7                                                               
*                                                                               
VLN67    CLI   MGFLAG,C'J'         IS IT A MGEREJ                               
         BE    VLN68                                                            
         MVC   NERRCD,=Y(NOTXPCTD)                                              
         OC    PSNFLD,PSNFLD       IS THERE ANOTHER FIELD                       
****     BNZ   VALINERR     **REMOVED DUE TO SPT DESKTOP BUG V3.3.0.6**         
         B     VLN80        **SENDING PRD CD AT END EVEN THO BPOL**             
*                                                                               
* THIS IS A REJECTION                                                           
*                                                                               
VLN68    LA    R4,32(R4)                                                        
         CLI   1(R4),0             IS THERE ANY INPUT                           
         BNE   VLN70               YES - MUST BE REJ COMMENT                    
         MVC   NERRCD,=Y(NOREJCOM)                                              
         B     VALINERR                                                         
*                                                                               
VLN70    MVC   NERRCD,=Y(NOTXPCTD)                                              
         CLI   MGFLAG,C'J'         TEST MGEREJ                                  
         BE    VLN72                                                            
         CLI   1(R4),0                                                          
****     BNE   VALINERR     **REMOVED DUE TO SPT DESKTOP BUG V3.3.0.6**         
         B     VLN80        **SENDING PRD CD AT END EVEN THO BPOL**             
*                                                                               
VLN72    MVC   NERRCD,=Y(NOREJCOM)                                              
         CLI   PSNLEN,0                                                         
         BE    VALINERR                                                         
         L     RE,PSNCOMP          GET DATA ADDR                                
         SR    RF,RF                                                            
         ICM   RF,1,PSNLEN         GET COMMENT LENGTH                           
         JZ    *+2                                                              
*                                                                               
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         JZ    VLN78                                                            
         CLC   MGREJCOM(4),=X'FFFFFFFF' MULTI MG REJ COMMENTS?                  
         BE    VLN80               YES, DON'T CLEAR                             
*                                                                               
VLN78    XC    MGREJCOM,MGREJCOM                                                
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   MGREJCOM(0),0(RE)   SAVE REJECTION COMMENT                       
         EJECT                                                                  
*=====================================================================*         
* READ ORDER AND FORMAT MGORDER INTO MGORDERQ                                   
*=====================================================================*         
         SPACE 1                                                                
VLN80    BRAS  RE,READORD                                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,15,MGORDER       BINARY ORDER NUMBER                          
         SR    RF,RF                                                            
         BCTR  RF,0                SET REG TO X'FF'                             
         XR    RE,RF               UN-COMPLEMENT ORDER NUMBER                   
*                                                                               
         ST    RE,FULL                                                          
         TM    FULL,X'80'          NEW STYLE ORDER #?                           
         BNZ   VLN81               YES                                          
*                                                                               
         LR    R0,RE                                                            
         SRL   R0,16                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MGORDERQ(4),DUB                                                  
*                                                                               
         LR    R0,RE                                                            
         SLL   R0,16                                                            
         SRL   R0,16                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MGORDERQ+4(4),DUB                                                
         B     VLN81X                                                           
*                                                                               
VLN81    NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  MGORDERQ(8),DUB                                                  
         EJECT                                                                  
*======================================================                         
* PROCESS ORDER RECORD                                                          
*======================================================                         
VLN81X   DS    0H                                                               
         L     R6,AREC                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
*                                                                               
         CLI   0(R6),DOIDELQ       DIE IF NO DARE ID ELEMENT                    
         JNE   *+2                                                              
         USING DOIDELD,R6                                                       
*                                                                               
         MVC   MGREPCON,DOIDCON    SAVE REP CONTRACT NUMBER                     
         MVC   MGREPSAL,DOIDSPER   SAVE SALESPERSON                             
         XC    MGLSTTRN,MGLSTTRN                                                
         DROP  R6                                                               
* NEED TO UPDATE REP ID IF IT HAS CHANGED SINCE XMT                             
         MVC   NERRCD,=Y(NOXMTEL)                                               
         XC    HALF,HALF                                                        
VLN82    CLI   0(R6),0             IF NO XMIT ELEM, ERROR                       
         JE    ERREXITN                                                         
         CLI   0(R6),DORPELQ2      HAVE X'09' NEW REP CHANGE ELEM?              
         BNE   VLN84                                                            
*                                                                               
         USING DOREPELD,R6                                                      
         OC    HALF,HALF           DO WE ALREADY HAVE A REP ID #?               
         BNZ   VLN86               YES, 1ST DOREPEL IS MOST RECENT              
         MVC   HALF,DORPNREP       SAVE NEW REP ID NUMBER                       
         B     VLN86                                                            
*                                                                               
         USING DOSTELD,R6                                                       
VLN84    CLI   DOSTEL,DOSTELQ      X'12' NEW STATUS ELEMENT?                    
         BNE   VLN86                                                            
         CLI   DOSTSTAT,DDLVRD     DELIVERED STATUS                             
         BE    VLN88                                                            
*                                                                               
VLN86    SR    R0,R0                                                            
         IC    R0,1(R6)            SKIP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     VLN82                                                            
*                                                                               
         USING DOSTELD,R6                                                       
VLN88    MVC   MGLSTTRN,0(R6)      SAVE THIS TRANSMISSION ELEMENT               
         LA    R6,MGLSTTRN                                                      
         OC    HALF,HALF           TEST NEW REP FOUND PREVIOUSLY                
         BNZ   VLN90                                                            
         MVC   HALF,DOSTIDNM       SAVE REPID NUMBER FOR GETID CALL             
         B     VLN95                                                            
*                                                                               
VLN90    MVC   DOSTIDNM,HALF       OVERRIDE REPID IF NEW REP                    
         DROP  R6                                                               
         EJECT                                                                  
*==========================================================                     
* SAVE TRADE DATA IF ANY                                                        
*==========================================================                     
         SPACE 1                                                                
VLN95    MVI   SVTRDTYP,0                                                       
         XC    SVTRDDTA,SVTRDDTA                                                
*                                                                               
         L     R6,AREC1                                                         
         USING DOKEY,R6                                                         
*                                                                               
         LA    R6,DORFRST                                                       
VLN97    CLI   0(R6),0             IF NO SUPP ELEM                              
         BE    VLN100                                                           
         CLI   0(R6),DOSPELQ                                                    
         BE    VLN98                                                            
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VLN97                                                            
*                                                                               
         USING DOSPELD,R6                                                       
VLN98    MVC   SVTRDTYP,DOSPTMTH   SET TRADE TYPE                               
         CLI   SVTRDTYP,X'99'      LOWER CASE R                                 
         BE    *+12                                                             
         CLI   SVTRDTYP,C'R'       OR UPPERCASE R                               
         BNE   VLN99                                                            
         OC    DOSPTDAT(3),DOSPTDAT   ANY REP CODE?                             
         BZ    VLN99                  NONE, WALTER'S FAULT                      
*                                                                               
         LHI   RE,VRCPACK-BUYSAVE                                               
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'P',DOSPTDAT),SVTRDDTA                               
         MVC   SVTRDDTA+2(3),DOSPTDAT                                           
         B     VLN100                                                           
*                                                                               
VLN99    MVI   SVTRDTYP,0          UNKNOWN TRADE TYPE - CLEAR                   
*                                                                               
VLN100   CLI   SVTRDTYP,0          TEST THIS IS CASH/TRADE ORDER                
         BNE   VLN102              YES                                          
         CLI   SVCSHTRD,0          NO - DID THEY INPUT /C OR /T                 
         BE    VLNX                NO - FINE                                    
         MVC   NERRCD,=Y(NOTRDORD) ELSE THEY SHOULDN'T HAVE                     
         J     ERREXITN                                                         
*                                                                               
VLN102   CLI   SVCSHTRD,0          CASH/TRADE ORDER - MUST SAY /C /T            
         BNE   VLNX                                                             
         MVC   NERRCD,=Y(NOCSHTRD)                                              
         J     ERREXITN                                                         
*                                                                               
VLNX     OI    4(R2),X'20'         SET INPUT VALIDATED                          
*                                                                               
         BRAS  RE,GETID            GET ALPHA FOR REPID IN HALF                  
         MVC   MGDESTID,WORK                                                    
*                                                                               
         CLI   MGFLAG,C'C'         ACCEPT ALWAYS REBUILDS !                     
         BE    VLNX8                                                            
         CLI   MGFLAG,C'G'         SO DOES 'GENERATE'                           
         BE    VLNX8                                                            
         CLI   MGFLAG,C'J'         SO DOES 'REJECT'                             
         BE    VLNX8                                                            
         CLI   MGFLAG,C'S'         SO DOES 'SELF-APPLY'                         
         BE    VLNX8                                                            
*                                                                               
         CLC   MGLSTKEY,KEY        TEST SAME ORDER/FLIGHT/PRDS                  
         BNE   VLNX2                                                            
         CLC   MGLSTGRP,MGGRPCOD   TEST SAME GROUP                              
         BNE   VLNX2                                                            
         CLI   MGFLAG,0            TEST ALREADY SET                             
         BH    *+8                                                              
         MVI   MGFLAG,C'X'         SET NE'X'T                                   
* NEXT INSTRUCTIONS ARE TO TRY TO STOP DUMPS THAT OCCUR WHEN THE                
* TSAR DATA HAS BEEN CLEARED AND NOT REBUILT, BUT THEY RE-ENTER THE             
* SAME MAKEGOOD ORDER DATA                                                      
         MVI   T.TSACTN,TSAGET     NOW GET FIRST TSAR ENTRY                     
         MVC   T.TSRNUM,MGTSRNUM   SET STARTING TSAR RECNUM                     
         MVC   MGTSRFRS,MGTSRNUM   AND SAVE FIRST RECNUM                        
         BRAS  RE,CALLTSAR                                                      
         JE    YES                 CONTINUE IF NO ERROR                         
         XC    MGTSRNUM,MGTSRNUM   RESET !                                      
         XC    MGTSRFRS,MGTSRFRS   RESET !                                      
*                                                                               
* NEW INPUT, REBUILD MAKEGOOD GROUP                                             
*                                                                               
VLNX2    MVC   NERRCD,=Y(BADMGEPF)                                              
         CLI   PFKEY,0                                                          
         BNE   *+10                                                             
         MVC   NERRCD,=Y(BADINPUT)                                              
*                                                                               
VLNX4    CLI   MGFLAG,0            DID THEY ASK FOR MORE THAN 'E'               
         BNE   VALINER4            NO                                           
*                                                                               
VLNX6    MVI   MGFLAG,C'E'         SET 'E'VALUATE                               
*                                                                               
VLNX8    MVC   MGLSTKEY,KEY        SAVE ORDER KEY                               
         MVC   MGLSTGRP,MGGRPCOD   SAVE GROUP CODE                              
         J     NO                                                               
         SPACE 1                                                                
*===================================================================            
* SET CURSOR POSITION ON ERROR                                                  
*===================================================================            
         SPACE 1                                                                
VALINERR ICM   RE,15,PSNCOMP       TEST FOR A FIELD ADDRESS                     
         BZ    VALINER2                                                         
         LA    RF,8(R2)                                                         
         SR    RE,RF                                                            
*                                                                               
VALINER2 L     R1,VTIOB                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2               DISPLACEMENT TO FIELD IN TWA                 
         SR    R0,R3               R3 = A(TWA)                                  
         STH   R0,TIOBCURD                                                      
         STC   RE,TIOBCURI         POINT TO THE PLACE IN FIELD                  
*                                                                               
VALINER4 NI    4(R2),X'DF'         INVALIDATE INPUT                             
         OI    6(R2),X'80'         XMT                                          
         XC    MGLSTKEY,MGLSTKEY                                                
         XC    MGLSTGRP,MGLSTGRP                                                
         J     ERREXITN                                                         
         DROP  R1,R4                                                            
         SPACE 2                                                                
*==================================================================             
* GETS THE BINARY PRD CODE FOR EBCDIC PRD AT 0(R1)                              
* EXIT WITH CC EQUAL IF PRODUCT FOUND                                           
*==================================================================             
         SPACE 1                                                                
GETBPRD  L     RF,ASVCLIST                                                      
         OC    0(3,R1),SPACES                                                   
*                                                                               
GBPRD10  CLI   0(RF),C'A'                                                       
         JL    GBPRDNO                                                          
         CLC   0(3,R1),0(RF)                                                    
         JE    GBPRDYES                                                         
         LA    RF,4(RF)                                                         
         J     GBPRD10                                                          
*                                                                               
GBPRDYES LA    R1,3(RF)            POINT TO BINARY PRD                          
         CR    RE,RE               RETURN WITH CC EQ                            
         BR    RE                                                               
*                                                                               
GBPRDNO  SR    R1,R1               RETURN WITH CC NEQ                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 1                                                                
*---------------------------------------------------------------------*         
* PARSNIP PARAMETER LIST                                                        
*---------------------------------------------------------------------*         
PSNPLIST DC    AL1(1),C'=',AL1(1),C',',AL1(1),C',',AL1(1),C','                  
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* READ MAKEGOOD NOTICE RECORD AND CHECK STATUS TO ACTION       *                
*==============================================================*                
         SPACE 1                                                                
CHKSTAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,GETMGSTA                                                      
*                                                                               
         LHI   RE,SVDAROPT-BUYSAVE                                              
         AR    RE,RA                                                            
         TM    0(RE),SVDAROPT_NOERRS   TEST SUPPRESS ERRORS                     
         BO    CHKSTATX                                                         
*                                                                               
         BRAS  RE,SETACTAB         POINT R1 TO ACTION TABLE                     
         USING ACTTABD,R1                                                       
         SPACE 1                                                                
*===========================================================                    
* NOTE THAT IF THE ACTION ISN'T IN THE TABLE, YOU CAN DO IT                     
* NO MATTER WHAT THE STATUS OF THE OFFER                                        
*===========================================================                    
         SPACE 1                                                                
CKST10   CLC   ACTCHAR,MGFLAG                                                   
         BE    CKST12                                                           
         LLC   R0,ACTLEN                                                        
         AR    R1,R0                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   CKST10                                                           
         B     CHKSTATX            ACTION NOT IN TABLE                          
*                                                                               
CKST12   LA    RE,ACTSTATS                                                      
         LLC   R0,ACTLEN           GET LENGTH FOR THIS ENTRY                    
         AHI   R0,-4               GIVES NUMBER OF STATUS ENTRIES               
*                                                                               
CKST14   CLC   SVMNSTAT,0(RE)      IS THIS THE CURRENT STATUS                   
         BE    CHKSTATX            YES - OK                                     
         LA    RE,1(RE)                                                         
         BCT   R0,CKST14                                                        
*                                                                               
         MVC   NERRCD,2(R1)        SET ERROR CODE                               
         MVC   MGTSRNUM,=Y(1)                                                   
         MVC   MGTSRFRS,=Y(1)                                                   
         BRAS  RE,DISPLAY                                                       
         MVI   SVMGINIT,C'N'       SET TO START AGAIN                           
         NI    MGEINP1H+4,X'DF'    SET NOT VALIDATED                            
         OI    MGEINP1H+6,X'80'    XMT                                          
         J     ERREXITN                                                         
*                                                                               
CHKSTATX J     EXIT                                                             
         DROP  R1,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* READ MAKEGOOD NOTICE AND GET/SET STATUS IN SVMNSTAT                           
*=====================================================================          
GETMGSTA NTR1  BASE=*,LABEL=*                                                   
         MVC   AREC,AREC2                                                       
         XC    MYSTTN,MYSTTN       READ HEADEND REC FOR CABLE                   
         BRAS  RE,READMKN          READ MAKEGOOD NOTICE RECORD                  
*                                                                               
         L     R6,AREC                                                          
****     AH    R6,DSPFRST          POINT TO FIRST ELEMENT                       
         AH    R6,DATADISP         POINT TO FIRST ELEMENT                       
         USING MNSTELD,R6                                                       
*                                                                               
GTMST10  CLI   MNSTSTAT,MNSTDELV   BYPASS DELIVERED STATUS                      
         BNE   GTMST20                                                          
*                                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),MNSTELQ       TEST STATUS ELEMENT                          
         BE    GTMST10                                                          
         DC    H'0'                SET ERROR CONDITION                          
*                                                                               
GTMST20  MVC   SVMNSTAT,MNSTSTAT   SAVE STATUS FOR LATER                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* READ MAKEGOOD OFFER RECORD AND GET MAKEGOOD TOTAL PTS/DOLS/SPOTS              
*  ON ENTRY                                                                     
*  MGFLAG      C'T' - ONLY GET TOTALS, DON'T UPDATE TSAR                        
*=====================================================================          
         SPACE 1                                                                
GETMKO   NTR1  BASE=*,LABEL=*,WORK=(R9,GMWORKL)                                 
         USING GMWORKD,R9                                                       
*                                                                               
         MVI   NEEDTOTS,C'N'       RESET FLAG                                   
         XC    MYSTTN,MYSTTN       CLEAR CABLE STA/NET                          
         XC    MKOSPTS(32),MKOSPTS CLEAR ACCUMS                                 
*                                                                               
GMKRDMKO BRAS  RE,READMKO                                                       
*                                                                               
         L     R6,AREC3                                                         
         ST    R6,AREC             RESTORE MKO1 POINTER                         
*                                                                               
         MVC   MYSTTN,SVKEY+6                                                   
         CLI   BUYST,C'0'                                                       
         BL    *+10                                                             
         MVC   MYSTTN,MOXKSTTN-MOXKEY(R6)                                       
*                                                                               
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOBDELQ      FIND X'50' MG BUY DETAIL ELEMS               
         BRAS  RE,FIRSTEL                                                       
         B     *+12                                                             
*                                                                               
GMK010   MVI   ELCODE,MOBDELQ      RESTORE ELEM VALUE                           
         BRAS  RE,NEXTEL                                                        
         BNE   GMK017                                                           
         ST    R6,LAST50EL                                                      
*                                                                               
         USING MOBDELD,R6                                                       
         LLC   R0,MOBDNWKS         GET NUMBER OF WEEKS                          
         LLC   R1,MOBDNSPW         GET SPOTS/WEEK                               
         MR    R0,R0               GIVES TOTALS SPOTS IN R1                     
         STH   R1,HALF             SAVE TOTAL SPOTS IN HALF                     
*                                                                               
         A     R1,MKOSPTS          BUMP SPOT COUNT                              
         ST    R1,MKOSPTS                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,MOBDCOST       GET SPOT COST                                
         MH    RF,HALF             SPOT COST X TOTAL SPOTS                      
         A     RF,MKODOLS                                                       
         ST    RF,MKODOLS                                                       
*                                                                               
         BRAS  RE,GETMKDEM         GET DEMO VALUES                              
*                                                                               
         LA    R1,BUDEMS                                                        
         LA    RE,MKOPTS                                                        
         LA    RF,6                                                             
*                                                                               
GMK012   ICM   R0,15,0(R1)         SKIP ZERO DEMO VALUES AS IT THROWS           
         BZ    GMK016               OFF 2-DECIMAL PRECISION                     
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
*                                                                               
GMK014   MH    R0,HALF             X SPOTS                                      
         L     R4,0(RE)                                                         
         N     R4,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         AR    R4,R0                                                            
         TM    0(R1),X'40'         TEST DEMO IN 2-DEC                           
         BZ    *+8                                                              
         O     R4,=X'40000000'                                                  
         ST    R4,0(RE)                                                         
*                                                                               
GMK016   LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,GMK012                                                        
         B     GMK010                                                           
*                                                                               
GMK017   CLI   MGFLAG,C'T'         TEST TOTS ONLY REQUEST                       
         BNE   GMK020               NO, PROCESS DETAILS                         
*                                                                               
         CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BL    GMK090               NO, NO FINISH UP                            
*                                                                               
* READ FOR MORE NETWORK OFFER RECORDS                                           
*                                                                               
GMKRDMOR L     R6,AREC3                                                         
         MVC   WORK(32),0(R6)      GET LAST MKO KEY PROCESSED                   
         LA    RE,MOXKSTTN-MOXKEY+WORK                                          
         MVC   3(2,RE),=X'FFFF'    FORCE NEXT STATION                           
         BRAS  RE,MYHIGH                                                        
         CLC   WORK(27),WORK2                                                   
         BNE   GMK090                                                           
         MVC   MYSTTN,MOXKSTTN-MOKEY+WORK  MOVE STA/NET                         
         B     GMKRDMKO                                                         
         EJECT                                                                  
***********************************************************************         
*         **** ADD TSAR RECORDS FOR DETAILS ****                                
* FIRST SEARCH FOR X'20' MKGD BUY ELEMENTS AND SAVE BUY DETAILS                 
***********************************************************************         
*                                                                               
         USING LINED,R2                                                         
GMK020   L     R6,AREC3                                                         
         ST    R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         ST    R6,LAST20EL                                                      
*                                                                               
         MVI   ELCODE,MOMBELQ      FIND X'20' MAKEGOOD BUY ELEM                 
         BRAS  RE,FIRSTEL                                                       
         B     GMK026                                                           
*                                                                               
GMK024   MVI   ELCODE,MOMBELQ      FIND X'20' MAKEGOOD BUY ELEM                 
         L     R6,LAST20EL                                                      
         USING MOBDELD,R6                                                       
         BRAS  RE,NEXTEL                                                        
GMK026   BNE   GMK060                                                           
         ST    R6,LAST20EL                                                      
         USING MOMBELD,R6                                                       
* MOVE DATA TO A TSAR RECORD                                                    
         XC    MOREC,MOREC                                                      
         MVI   MOTYPE,MOTYPEQ                                                   
*                                                                               
         MVC   MOSTTN,SVKEY+6      MOVE BUY STATION                             
         CLI   BUYST,C'0'          IF LOCAL CABLE                               
         BL    *+14                                                             
         L     RE,AREC3                                                         
         MVC   MOSTTN,MOXKSTTN-MOXKEY(RE)                                       
*                                                                               
         MVC   MORECID,MOMBOFFR    MOVE OFFERNUM/RECNUM                         
         MVC   MODAYS,MOMBDAYS                                                  
         MVC   MOOOW,MOMBOROT                                                   
         MVC   MOSTIM,MOMBSTIM                                                  
         MVC   MOETIM,MOMBETIM                                                  
         MVC   MOSLN,MOMBTSLN                                                   
         MVC   MOLNUNIT,MOMBUNIT                                                
         MVC   MOPRD1LN,MOMBPTIM                                                
         MVC   MOSTYP,MOMBSTYP                                                  
         MVC   MODPT,MOMBDYPT                                                   
         LLC   RE,1(R6)                                                         
         AHI   RE,-MOMBOVRH                                                     
         LTR   RE,RE                                                            
         BNP   GMK029                                                           
*                                                                               
GMK027   CHI   RE,L'MOPROG         L(PROGRAM) LONGER THAN FIELD?                
         BL    GMK028                                                           
         MVC   MOPROG,MOMBPROG     YES, DON'T OVERWRITE OTHER FIELDS            
         B     GMK029                                                           
*                                                                               
GMK028   BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   MOPROG(0),MOMBPROG                                               
         DROP  R6                                                               
*                                                                               
GMK029   LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),MOMBSELQ      TEST X'21' SUPP ELEM FOLLOWS                 
         BNE   GMK030                                                           
         USING MOMBSELD,R6                                                      
         MVC   MOADJ,MOMBSADJ                                                   
         MVC   MOCOS2,MOMBSCS2                                                  
         MVC   MOREP,MOMBSREP                                                   
         SPACE 1                                                                
***********************************************************************         
* NOW SEARCH FOR A X'80' ELEMENT AND SAVE BUYLINE NUMBER AND MAKEGOOD           
* GROUP CODE IF OFFER HAS ALREADY BEEN OKAYED                                   
***********************************************************************         
         SPACE 1                                                                
GMK030   MVI   ELCODE,MNUMELQ      GET X'80' GROUP CODE IF OKAYED               
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
****     BRAS  RE,FIRSTEL                                                       
****     B     *+8                                                              
*                                                                               
GMK031   BRAS  RE,NEXTEL                                                        
         BNE   GMK032                                                           
         USING MNUMELD,R6                                                       
         CLC   MORECID,MNUMOFR     MATCH RECNUM/SEQNUM                          
         BNE   GMK031                                                           
         MVC   MOBUYLIN,MNUMNUM    SAVE ACTUAL BUYLINE NUMBER                   
         MVC   MOMGGRP,MNUMCD      AND BUY MAKEGOOD GROUP                       
         DROP  R6                                                               
*                                                                               
GMK032   L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOPRPELQ     CHECK FOR X'63' PURPOSE CODE ELEM            
         BRAS  RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
GMK033   BRAS  RE,NEXTEL                                                        
         BNE   GMK034                                                           
         CLC   MORECID,2(R6)                                                    
         BNE   GMK033                                                           
         OI    MOFLAGS,X'80'       SET PURPOSE CODE FOUND                       
*                                                                               
GMK034   L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MORSNELQ     CHECK FOR X'64' REASON CODE ELEM             
*                                                                               
GMK035   BRAS  RE,NEXTEL                                                        
         BNE   GMK036                                                           
         CLC   MORECID,2(R6)                                                    
         BNE   GMK035                                                           
         OI    MOFLAGS,X'40'       SET REASON CODE FOUND                        
                                                                                
GMK036   L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOUPGELQ     CHECK FOR X'61' UPGRADE ELEM                 
*                                                                               
GMK037   BRAS  RE,NEXTEL                                                        
         BNE   GMK040                                                           
         CLC   MORECID,2(R6)                                                    
         BNE   GMK037                                                           
         CLI   MOUPGTXT-MOUPGEL(R6),C' '  HAVE VALID UPGRADE?                   
         BNH   GMK040                       NO                                  
         OI    MOFLAGS,X'20'       SET UPGRADE FOUND                            
         SPACE 1                                                                
***********************************************************************         
* NOW SEARCH FOR X'50' ELEMENTS, ADD COST/STDATE/WKS/NPW                        
* AND ADD A TSAR RECORD FOR EACH                                                
***********************************************************************         
         SPACE 1                                                                
GMK040   XC    GMSPOTS,GMSPOTS                                                  
         L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MOBDELQ      X'50'                                        
*                                                                               
         BRAS  RE,FIRSTEL                                                       
         B     GMK047                                                           
*                                                                               
GMK045   MVI   ELCODE,MOBDELQ                                                   
         BRAS  RE,NEXTEL                                                        
*                                                                               
GMK047   BNE   GMK050                                                           
*                                                                               
         ST    R6,LAST50EL                                                      
         USING MOBDEL,R6                                                        
*                                                                               
         CLC   MORECID(2),MOBDOFFR  SAME OFFERNUM/RECNUM                        
         BNE   GMK045                                                           
*                                                                               
         MVC   MOSTTN,MYSTTN        SET STA/NET FOR CABLE                       
         MVC   MOSEQNUM,MOBDSEQ     SEQNUM KEEPS IT UNIQUE                      
         MVC   MOCOST,MOBDCOST                                                  
         MVC   MOSTRT,MOBDBDAT      START DATE (PWOS JULIAN)                    
         MVC   MOWKS,MOBDNWKS                                                   
         MVC   MONPW,MOBDNSPW                                                   
*                                                                               
         BRAS  RE,CHKMKO            CHECK PERIOD/SLN                            
         BRAS  RE,GETMKDEM          GET DEMO VALUES (CHANGES ELCODE!)           
         MVC   MODEM1(24),BUDEMS                                                
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         B     GMK045                                                           
         EJECT                                                                  
***********************************************************************         
* NOW SEARCH FOR X'41' ORBIT DESCRIPTION ELEMENTS AND ADD TSAR RECS             
* ORBIT POSNS 1-4 GO IN RECORD 1, 5-8 GO IN RECORD 2                            
***********************************************************************         
         SPACE                                                                  
GMK050   L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         MVI   MOCOMNUM,1                                                       
*                                                                               
GMK053   XC    MOCOM,MOCOM         CLEAR COMMENT/ORBIT DATA                     
         MVI   ELCODE,MORNELQ      X'41'                                        
*                                                                               
         LA    R4,MOORB            POINT TO FIRST ORBIT POSN                    
         USING MOORBDAY,R4                                                      
         LHI   R5,4                POSN COUNTER                                 
*                                                                               
         CLI   MOCOMNUM,1          FIRST TIME?                                  
         BNE   GMK056              NO, THEN DON'T GET FIRST                     
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GMK056   BRAS  RE,NEXTEL                                                        
         BNE   GMK058                                                           
*                                                                               
         USING MORNELD,R6                                                       
         CLC   MORECID(2),MORNOFFR  SAME OFFERNUM/RECNUM                        
         BNE   GMK056                                                           
*                                                                               
         MVC   MOORBDAY,MORNDAYS                                                
         MVC   MOORBTIM(4),MORNSTIM                                             
         MVC   MOORBPRG,MORNPROG                                                
         MVC   MOORBDEM,MORNDEM                                                 
         AHI   R4,L'MOORB                                                       
         BCT   R5,GMK056                                                        
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
*                                                                               
         MVI   MOCOMNUM,2                                                       
         B     GMK053                                                           
*                                                                               
GMK058   CHI   R5,4                TEST TSAR ADD PENDING                        
         BE    GMK059                                                           
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
GMK059   B     GMK024                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* NOW SEARCH FOR X'30' REP OFFER COMMENT ELEMENTS AND ADD TSAR RECS             
***********************************************************************         
         SPACE 1                                                                
GMK060   XC    TSARREC,TSARREC                                                  
         MVI   MOTYPE,MOTYPEQ                                                   
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
*                                                                               
         MVC   MOSTTN,SVKEY+6                                                   
         CLI   BUYST,C'0'                                                       
         BL    *+10                                                             
         MVC   MOSTTN,MOXKSTTN-MOXKEY(R6)  NEED STA/NET FOR CABLE               
*                                                                               
****     AH    R6,DSPFRST                                                       
         MVI   ELCODE,MOBCELQ      X'30'                                        
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GMK061   BRAS  RE,NEXTEL                                                        
         BNE   GMK063                                                           
         USING MOBCELD,R6                                                       
*                                                                               
GMK062   MVC   MORECID(3),MOBCOFFR  MOVE OFFER/REC/SEQNUM                       
         MVC   MOCOMNUM,MOBCLINE    MOVE COMMENT NUMBER                         
         OI    MOCOMNUM,X'10'       COMMENTS ARE X'11'+                         
         XC    MOCOM,MOCOM                                                      
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         BM    GMK061                                                           
         EX    RE,*+4                                                           
         MVC   MOCOM(0),3(R6)                                                   
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         B     GMK061                                                           
*                                                                               
GMK063   CLC   AREC,AREC3          TEST POINTING AT REC3                        
         BNE   GMK070              NO                                           
         L     R6,AREC4                                                         
         OC    0(13,R6),0(R6)      TEST FOR MKO2 RECORD                         
         BZ    GMK070              NO                                           
         ST    R6,AREC             SET THAT'S WHERE WE ARE                      
****     AH    R6,DSPFRST                                                       
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         BE    GMK062              AND CONTINUE SEARCH                          
         EJECT                                                                  
***********************************************************************         
* NOW SEARCH FOR X'22' BUY AUTO AVAILS UUID ELEMENTS AND ADD TSAR RECS          
***********************************************************************         
         SPACE 1                                                                
GMK070   XC    TSARREC,TSARREC                                                  
         MVI   MOTYPE,MOTYPEQ                                                   
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
         XC    HALF,HALF           NO PREV OFFER/REC # YET                      
*                                                                               
         MVC   MOSTTN,SVKEY+6                                                   
         CLI   BUYST,C'0'                                                       
         BL    *+10                                                             
         MVC   MOSTTN,MOXKSTTN-MOXKEY(R6)  NEED STA/NET FOR CABLE               
*                                                                               
****     AH    R6,DSPFRST                                                       
         MVI   ELCODE,MOMBAELQ     X'22' ELEM                                   
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GMK071   BRAS  RE,NEXTEL                                                        
         BNE   GMK077                                                           
*                                                                               
         USING MOMBAAUD,R6                                                      
GMK072   CLC   HALF,MOMBAOFR        SAME OFFER/REC?                             
         JE    GMK073               YES                                         
         MVI   BYTE2,X'20'          UUIDS ARE X'20' TO X'25'                    
         J     GMK074                                                           
*                                                                               
GMK073   LLC   RE,BYTE2             INCREMENT THE SEQ                           
         LA    RE,1(R1)                                                         
         STC   RE,BYTE2                                                         
*                                                                               
GMK074   MVC   HALF,MOMBAOFR        MOVE OFFER/REC                              
         MVC   MORECID(2),MOMBAOFR  MOVE OFFER/REC                              
         MVI   MOSEQNUM,1                                                       
         MVC   MOCOMNUM,BYTE2       UUIDS ARE X'20'-X'25'                       
         XC    MOCOM,MOCOM                                                      
*                                                                               
         LLC   R5,1(R6)                                                         
         SHI   R5,MOMBAOLQ          RE=L(UUID TEXT)                             
*                                                                               
         CHI   R5,L'MOCOM           UUID TEXT LONGER THAN MOCOM?                
         JH    GMK075               YES, WE HAVE TO ADD MORE MOCOMS             
*                                                                               
         BCTR  R5,0                 SHORTER, ADJUST FOR EXECUTED MOVE           
         CHI   R5,0                 NO TEXT?                                    
         JNH   GMK071               NEXT ELEMENT THEN                           
*                                                                               
         EX    R5,*+4               WE HAVE TEXT                                
         MVC   MOCOM(0),MOMBAAU     MOVE IT                                     
         MVI   T.TSACTN,TSAADD      AND SAVE IT TO TSAR                         
         BRAS  RE,CALLTSAR                                                      
         J     GMK071               NEXT ELEMENT                                
*                                                                               
GMK075   LA    R7,3(R6)             COPY 70 CHARACTER OF UUID TO MOCOM          
GMK076   MVC   MOCOM,0(R7)                                                      
         SHI   R5,L'MOCOM                                                       
         LA    R7,L'MOCOM(R7)                                                   
*                                                                               
         MVI   T.TSACTN,TSAADD      ADD IT TO TSAR                              
         BRAS  RE,CALLTSAR                                                      
*                                                                               
         LTR   R5,R5                NOTHING LEFT?                               
         JZ    GMK071                                                           
         LLC   RE,BYTE2             BUMP NUMBER USED IN MOCOMNUM                
         AHI   RE,1                                                             
         STC   RE,BYTE2                                                         
         MVC   MOCOMNUM,BYTE2       UUIDS ARE X'20'-X'25'                       
*                                                                               
         CHI   R5,L'MOCOM           REST OF UUID STILL > MOCOM?                 
         JH    GMK076               YES                                         
         XC    MOCOM,MOCOM          CLEAR OUT ANY PREVIOUS TEXT                 
         BCTR  R5,0                 AND MOVE IN THE REMAINING                   
         EX    R5,*+4                                                           
         MVC   MOCOM(0),3(R6)                                                   
         MVI   T.TSACTN,TSAADD      ADD IT TO TSAR                              
         BRAS  RE,CALLTSAR                                                      
         J     GMK071               NEXT UUID ELEMENT                           
*                                                                               
GMK077   CLC   AREC,AREC3          TEST POINTING AT REC3                        
         BNE   GMK080              NO                                           
         L     R6,AREC4                                                         
         OC    0(13,R6),0(R6)      TEST FOR MKO2 RECORD                         
         BZ    GMK080              NO                                           
         ST    R6,AREC             SET THAT'S WHERE WE ARE                      
****     AH    R6,DSPFRST                                                       
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         BE    GMK072              AND CONTINUE SEARCH                          
         EJECT                                                                  
***********************************************************************         
* NOW SEARCH FOR X'10' GROUP COMMENT ELEMENTS AND ADD TSAR RECS                 
***********************************************************************         
         SPACE 1                                                                
GMK080   XC    TSARREC,TSARREC                                                  
         MVI   MGCTYPE,MGCTYPEQ                                                 
         MVI   MGCSEQ,0                                                         
         L     R6,AREC3                                                         
         ST    R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         MVI   ELCODE,MOCMELQ      X'10'                                        
         BRAS  RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
GMK082   BRAS  RE,NEXTEL                                                        
         BNE   GMK086                                                           
*                                                                               
GMK084   IC    R0,MGCSEQ                                                        
         AHI   R0,1                                                             
         STC   R0,MGCSEQ                                                        
         USING MOCMELD,R6                                                       
         MVC   MGCCOM,SPACES                                                    
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         BM    GMK082                                                           
         EX    RE,*+4                                                           
         MVC   MGCCOM(0),3(R6)                                                  
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         B     GMK082                                                           
*                                                                               
GMK086   CLC   AREC,AREC3          TEST POINTING AT REC3                        
         BNE   GMK089              NO                                           
         L     R6,AREC4                                                         
         OC    0(13,R6),0(R6)      TEST FOR MKO2 RECORD                         
         BZ    GMK089              NO                                           
         ST    R6,AREC             SET THAT'S WHERE WE ARE                      
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         BRAS  RE,FIRSTEL                                                       
         BE    GMK084              AND CONTINUE SEARCH                          
*                                                                               
GMK089   CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BNL   GMKRDMOR             YES, GO BACK AND RD MORE NTWK RECS          
         EJECT                                                                  
***********************************************************************         
* SEE IF ONLY ONE MISSED AND COPY MISSED BUYLINES DETAILS                       
***********************************************************************         
         SPACE 1                                                                
GMK090   XC    MGONEMSS,MGONEMSS   RESET                                        
         XC    MGONESTA,MGONESTA                                                
         XC    MSREC,MSREC         SEE IF ONLY ONE MISSED LINE                  
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
GMK094   BRAS  RE,CALLTSAR                                                      
         BNE   GMKX                                                             
*                                                                               
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   GMK094                                                           
         B     GMK098                                                           
*                                                                               
GMK096   BRAS  RE,CALLTSAR                                                      
         BNE   GMKX                                                             
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   GMK096                                                           
         CLC   MGONEMSS,MSACTBUY   TEST SAME MISSED LINE                        
         BE    GMK096              YES                                          
         XC    MGONEMSS,MGONEMSS   ELSE SUPPRESS COPY OPTION                    
         B     GMKX                                                             
*                                                                               
GMK098   MVC   MGONEMSS,MSACTBUY   SAVE FIRST MISSED BUYLINE                    
         MVC   MGONESTA,MSSTTN                                                  
         MVC   MGONETIM,MSSTIM     SAVE START/END TIME                          
         MVC   MGONEDAY,MSDAYS     SAVE DAYS                                    
         CLI   SVDARPRF+3,C'Y'     TEST COPY FROM MISSED BUYLINE                
         BE    GMK096                                                           
         XC    MGONEMSS,MGONEMSS   ELSE SUPPRESS OPTION                         
         XC    MGONESTA,MGONESTA                                                
*                                                                               
GMKX     MVC   MGTSRCNT,T.TSPRECN  SAVE NUMBER OF RECORDS                       
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================          
* CHECK OFFER WITHIN ESTIMATE PERIOD                                            
* CHECK FOR VALID SLN                                                           
* IF ERRORS, SET MOERR FLAG                                                     
*=====================================================================          
         SPACE 1                                                                
CHKMKO   NTR1                                                                   
*                                                                               
         LLC   R0,MOWKS            GET #WEEKS                                   
         SR    R1,R1                                                            
         LLC   R1,MONPW            GET #SPOTS/WEEK                              
         MR    R0,R0               GET #SPOTS                                   
         LLH   RE,GMSPOTS          GET SPOT ACCUMULATOR                         
         AR    R1,RE                                                            
         STH   R1,GMSPOTS          SAVE SPOT ACCUMULATOR                        
         LLC   RF,SVMAXSPT                                                      
         CR    R1,RF               TOO MANY SPOTS ON THE LINE?                  
         BNH   CHKMK010                                                         
         OI    MOERR,X'08'                                                      
*                                                                               
CHKMK010 GOTO1 VDATCON,DMCB,(8,MOSTRT),WORK  GET YYMMDD START DATE              
*                                                                               
         CLC   WORK(6),SVSTART               TEST PRIOR TO EST START            
         BNL   *+8                                                              
         OI    MOERR,X'80'                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,MOWKS                                                       
         BNP   CHKMK020                                                         
*                                                                               
         BCTR  R0,0                                                             
         MHI   R0,7                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         CLC   WORK+6(6),SVEND                                                  
         BNH   *+8                                                              
         OI    MOERR,X'80'         SET NOT IN EST PERIOD                        
*                                                                               
CHKMK020 LHI   RF,VSLNTAB-BUYSAVE                                               
         AR    RF,RA                                                            
         L     R1,0(RF)            POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         CLI   BUYMD,C'T'                                                       
         BE    CHKMK040                                                         
         CLI   BUYMD,C'N'                                                       
         BE    CHKMK040                                                         
         CLI   BUYMD,C'C'                                                       
         BE    CHKMK040                                                         
*                                                                               
         MVI   BYTE,C'R'                                                        
         CLI   BUYMD,C'R'                                                       
         BE    CHKMK040                                                         
         CLI   BUYMD,C'X'                                                       
         BE    CHKMK040                                                         
         DC    H'0'                                                             
*                                                                               
CHKMK040 CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    CHKMK050                                                         
         CLC   AGYALPHA,0(R1)      MATCH AGY                                    
         BNE   *+14                                                             
CHKMK050 CLC   BYTE,2(R1)          MATCH MEDIA                                  
         BE    CHKMK060                                                         
*                                                                               
         BXLE  R1,RE,CHKMK040      NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
CHKMK060 AHI   R1,4                POINT BEYOND TABLE ID                        
         LLC   RE,MOSLN            GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BNE   CHKMK070            NON-ZERO IS VALID                            
         OI    MOERR,X'40'         SET INVALID SLN                              
*                                                                               
CHKMK070 SR    RE,RE                                                            
         ICM   RE,8,MODAYS                                                      
         JZ    *+2                 MUST HAVE SOME DAY BIT ON                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SVEOWSDY       (R1) = ESTIMATE OOW START DAY                
         BZ    CHKMK130                                                         
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         SLL   RE,1                                                             
         LHI   R0,1                                                             
CHKMK080 CHI   R0,7                                                             
         BH    CHKMK090                                                         
         LTR   RE,RE                                                            
         BNM   *+8                                                              
         STCM  R0,1,0(RF)                                                       
         SLL   RE,1                                                             
         AHI   R0,1                                                             
         AHI   RF,1                                                             
         B     CHKMK080                                                         
*                                                                               
CHKMK090 MVC   WORK+7(7),WORK                                                   
         SR    R2,R2               (R2) = MG START DAY                          
         ICM   R2,1,MOOOW                                                       
         SR    R2,R1               START DAY == ESTIMATE START DAY              
         BZ    CHKMK120            THE SAME, NO ROTATOR PROBLEM                 
         BP    CHKMK100            (R2) = # OF BYTES TO COMPARE                 
         AHI   R2,7                                                             
         B     CHKMK110                                                         
CHKMK100 AHI   R1,7                                                             
*                                                                               
CHKMK110 LA    RF,WORK-1(R1)       POINT TO NEXT WEEK                           
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       ARE ANY OF THOSE DAYS ON?                    
         BNZ   CHKMK170            YES, WE HAVE OOW                             
CHKMK120 MVC   MOOOW,SVEOWSDY                                                   
         B     CHKMK180                                                         
*                                                                               
CHKMK130 SR    R0,R0               CLEAR COUNTER                                
CHKMK140 LTR   RE,RE                                                            
         BM    CHKMK150                                                         
         SLL   RE,1                MOVE DAY BITS LEFT                           
         BCT   R0,CHKMK140         AND ADD 1 TO COUNTER                         
*                                                                               
CHKMK150 LPR   R0,R0               THIS IS START DAY NUMBER                     
CHKMK160 CLM   R0,1,MOOOW          MUST START ON FIRST ROT DAY                  
         BE    CHKMK180                                                         
CHKMK170 OI    MOERR,X'20'         ELSE SET OOW ERROR                           
*                                                                               
CHKMK180 CLI   MGBPRD2,0           DO WE HAVE PIGGYBACK PRODUCTS?               
         BE    CHKMK190            NO                                           
         TM    MOSLN,X'01'         YES, SPOT LENGTH IS SPLITTABLE?              
         BZ    CHKMK190            YES                                          
         OI    MOERR,X'10'         NO, SET ERROR                                
*                                                                               
* VALIDATE PGM, TO PREVENT UNAUTHORIZED USE OF -S                               
*                                                                               
CHKMK190 LA    RF,16               MAXIMUM PROGRAM NAME LENGTH                  
CHKMK195 LA    R4,MOPROG(RF)                                                    
         CLI   0(R4),C' '          FIND LAST NON-SPACE PGM NAME                 
         BH    CHKMK200                                                         
         BCT   RF,CHKMK195                                                      
                                                                                
CHKMK200 LA    R4,MOPROG-1(RF)     POINT TO NEXT TO LAST CHAR                   
         CLC   0(2,R4),=C'-S'      PGM NAME USING -S?                           
         BNE   CHKMKX               NO                                          
         CLI   SVAPROF+6,C'Y'      TEST -S AUTH REQUIRED                        
         BNE   CHKMKX               NO                                          
         TM    T211FFD+12,X'80'    TEST SECURITY BIT ON?                        
         BO    CHKMKX               YES                                         
         OI    MOERR,X'04'         INVALID PROGRAM NAME                         
*                                                                               
CHKMKX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
GMWORKD  DSECT                   ** GETMKO LOCAL WORKING STORAGE **             
GMSPOTS  DS    H                                                                
GMWORKL  EQU   *-GMWORKD                                                        
T21131   CSECT                                                                  
         EJECT                                                                  
*==============================================================                 
* GET DEMO VALUES FROM MKO RECORD(S) AND RETURN IN BUDEMS(24)                   
*==============================================================                 
         SPACE 1                                                                
GETMKDEM NTR1  BASE=*,LABEL=*                                                   
         XC    BUDEMS,BUDEMS                                                    
         SR    R0,R0                                                            
         ST    R0,FULL                                                          
         ST    R0,LAST60EL         CLEAR OUTPUT VALUES                          
         ST    R0,LAST5EEL         CLEAR OUTPUT VALUES                          
*                                                                               
         MVI   ELCODE,MODMELQ      X'60' LOOK FOR DEMO ELEMENT                  
*                                                                               
GMKD010  L     R6,AREC3                                                         
****     AH    R6,DSPFRST                                                       
         L     R5,LAST50EL                                                      
*                                                                               
****     BRAS  RE,FIRSTEL          FIND FIRST DEMO ELEM                         
         BRAS  RE,GETEL            FIND FIRST DEMO ELEM                         
         B     *+8                                                              
*                                                                               
GMKD020  BRAS  RE,NEXTEL                                                        
         BNE   GMKD100                                                          
*                                                                               
         CLC   2(2,R6),2(R5)       MATCH OFFER NUM/REC NUM                      
         BNE   GMKD020                                                          
*                                                                               
         CLI   0(R6),MODMELQ       PROCESSING X'60'?                            
         BNE   *+8                                                              
         ST    R6,LAST60EL         YES, SAVE ADDRESS                            
*                                                                               
         CLI   0(R6),MONTELQ       PROCESSING X'5E'?                            
         BNE   *+8                                                              
         ST    R6,LAST5EEL         YES, SAVE ADDRESS                            
*                                                                               
         LA    R4,SVDEMOS          POINT TO APPROPRIATE DEMO LIST               
         LA    R0,L'SVDEMLST(R4)   GET END OF LIST                              
         OC    SVBRDEMS,SVBRDEMS   HAVE BRAND DEMO LIST?                        
         BZ    *+12                 NO                                          
         LA    R4,SVBRDEMS         POINT TO APPROPRIATE DEMO LIST               
         LA    R0,L'SVBRDEMS(R4)   GET END OF LIST                              
         ST    R0,FULL             SAVE EOL ADDRESS                             
         LA    R5,BUDEMS                                                        
*                                                                               
GMKD030  CLI   ELCODE,MODMELQ      PROCESSING X'60'?                            
         BNE   GMKD040                                                          
         L     R6,LAST60EL         GET X'60' DEMO ELEM ADDR                     
         LLC   R1,1(R6)                                                         
         AHI   R1,-4                                                            
         BNP   GMKD100                                                          
         SRL   R1,3                SET FOR BCT ON NUM DEMOS                     
         B     GMKD050                                                          
*                                                                               
GMKD040  CLI   ELCODE,MONTELQ      PROCESSING X'5E'?                            
         JNE   *+2                                                              
         L     R6,LAST5EEL         GET X'5E' DEMO ELEM ADDR                     
         LLC   R1,1(R6)                                                         
         AHI   R1,-4                                                            
         BNP   GMKD100                                                          
         SR    R0,R0               GET NUM DEMOS IN R1                          
         D     R0,=A(9)            SET FOR BCT ON NUM DEMOS                     
*                                                                               
GMKD050  LA    R6,4(R6)                                                         
*                                                                               
GMKD060  CLC   0(3,R4),0(R6)       MATCH DEMO CAT?                              
         BE    GMKD080              YES                                         
         CLI   ELCODE,MODMELQ      PROCESSING X'60'?                            
         BNE   *+12                                                             
         LA    R6,L'MODMDEMO(R6)   NEXT DEMO ENTRY                              
         B     GMKD070                                                          
*                                                                               
         CLI   ELCODE,MONTELQ      PROCESSING X'5E'?                            
         JNE   *+2                                                              
         LA    R6,L'MONTDEMO(R6)   NEXT DEMO ENTRY                              
*                                                                               
GMKD070  BCT   R1,GMKD060                                                       
         B     GMKD090             THEY MUST HAVE CHANGED THE ESTHDR            
*                                                                               
GMKD080  ICM   RF,15,4(R6)                                                      
*        N     RF,=X'7FFFFFFF'     *DO NOT* DROP OVERRIDE FLAGS                 
         ST    RF,0(R5)            RETURN DEMO VALUE (WITH 2-DEC FLAG)          
*                                                                               
GMKD090  LA    R4,3(R4)            BUMP TO NEXT DEMO                            
         C     R4,FULL             EOL?                                         
         BNL   GMKD100              YES                                         
         LA    R5,4(R5)                                                         
         OC    0(3,R4),0(R4)       HAVE MORE DEMO CATS?                         
         BNZ   GMKD030              YES                                         
*                                                                               
GMKD100  CLI   ELCODE,MONTELQ      HAVE WE PROCESS X'5E' YET?                   
         BE    GMKDX                YES, WE'RE DONE                             
         MVI   ELCODE,MONTELQ       NO, GO BACK AND PROCESS X'5E' TOO           
         B     GMKD010                                                          
GMKDX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* ON ENTRY AREC2 CONTAINS THE MAKEGOOD NOTICE RECORD                  *         
* BUILD TSAR TABLE OF ALL MISSED SPOTS AND COMMENTS                   *         
* FOR THIS NOTICE RECORD                                              *         
* FOR LOCAL CABLE, THERE ARE MULTIPLE NOTICE RECORDS FOR THE HEADEND  *         
* AND FOR EACH NETWORK WITH MISSED SPOTS                              *         
*=====================================================================*         
         SPACE 1                                                                
FNDMISS  NTR1  BASE=*,LABEL=*                                                   
         XC    MGPURP,MGPURP       CLEAR PURPOSE CODE                           
*                                                                               
FMS010   L     R6,AREC2                                                         
         USING MNXKEY,R6                                                        
*                                                                               
         CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BL    FMS020                                                           
         OC    MNXKSTTN,MNXKSTTN   TEST STATION PRESENT                         
         BZ    FMS050              NO- MISSED SPOTS HAVE STATION                
         DROP  R6                                                               
*                                                                               
FMS020   DS    0H                                                               
****20   AH    R6,DSPFRST                                                       
         MVI   ELCODE,MNMSELQ      MISSED BUYLINE ELEM                          
         USING MNMSELD,R6                                                       
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
FMS030   BRAS  RE,NEXTEL                                                        
         BNE   FMS050                                                           
*                                                                               
         MVC   MGMSSCNT,MNMSNSPT   SAVE NUMBER OF SPOTS                         
         CLI   MGMSSCNT,0          CHECK FOR REP ERROR                          
         BE    FMS030                                                           
*                                                                               
FMS040   IC    R0,ELEMNO           COUNT NUMBER OF SPOTS IN NOTICE REC          
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
* BUILD A TSAR RECORD                                                           
         XC    MSREC,MSREC                                                      
         MVI   MSTYPE,MSTYPEQ      SET MISSED SPOT RECORD                       
*                                                                               
         MVC   MSSTTN,SVKEY+6      MOVE STATION                                 
         CLI   BUYST,C'0'          TEST CABLE                                   
         BL    *+14                                                             
         L     RE,AREC2                                                         
         MVC   MSSTTN,MNXKSTTN-MNXKEY(RE)  MOVE STATION/NETWORK                 
*                                                                               
         MVC   MSSEQNUM,MNMSSEQN   SET X'10' ELEM SEQ NUMBER                    
         MVC   MSELEMNO,ELEMNO     ELEM NUM MAKES KEY UNIQUE                    
*                                                                               
         LLC   R0,MNMSBLIN         GET 1-BYTE LINE NUMBER                       
         OC    MNMSBLN2,MNMSBLN2   IF 2-BYTE LINE NUMBER IS THERE               
         BZ    *+8                 USE IT                                       
         ICM   R0,3,MNMSBLN2                                                    
         STCM  R0,3,MSBUYLIN                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(8,MNMSBDAT),(2,MSELDT)  GET 2 BYTE DATE            
         GOTO1 (RF),(R1),,MSELDTQ                    GET 6 BYTE DATE            
*                                                                               
         GOTO1 VGETDAY,DMCB,MSELDTQ,WORK                                        
         MVC   MSELDAY,DMCB        SET DAY OF WEEK (1=MONDAY)                   
*                                                                               
         MVC   MSDAYS,MNMSDAYS                                                  
         MVC   MSOOW,MNMSOROT                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MNMSSTIM       BUY STORES HOURS 0000 TO 2400                
         CHI   R0,2400             AND WE STORE HOURS 600 TO 3000               
         BNH   *+8                                                              
         AHI   R0,-2400                                                         
         STCM  R0,3,MSSTIM                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,MNMSETIM                                                    
         CHI   R0,2400                                                          
         BNH   *+8                                                              
         AHI   R0,-2400                                                         
         STCM  R0,3,MSETIM                                                      
*                                                                               
         CLC   MSSTIM,MSETIM       IF START/END EQUAL                           
         BNE   *+10                                                             
         XC    MSETIM,MSETIM       SET END=X'0000' (LIKE BDTIME)                
*                                                                               
         MVC   MSSLN,MNMSTSLN                                                   
         MVC   MSCOST,MNMSCOST                                                  
*                                                                               
         MVI   T.TSACTN,TSAADD                                                  
         BRAS  RE,CALLTSAR                                                      
         MVC   MGTSRNUM,T.TSRNUM   SAVE TSAR REC NUM                            
         LLC   R0,MGMSSCNT                                                      
         BCTR  R0,0                                                             
         STC   R0,MGMSSCNT                                                      
         LTR   R0,R0                                                            
         BNZ   FMS040                                                           
         B     FMS030                                                           
                                                                                
*===================================================================            
* FOR LOCAL CABLE READ FOR (MORE) NOTICE RECORDS TO GET SPOTS ON                
* ALL STATIONS                                                                  
*===================================================================            
                                                                                
FMS050   CLI   BUYST,C'0'                                                       
         BL    FMS100                                                           
         BRAS  RE,MYSEQ                                                         
         CLC   WORK(28),WORK2      TEST SAME THRU GROUP                         
         BNE   FMS100                                                           
         MVC   AREC,AREC2                                                       
         BRAS  RE,MYGETREC                                                      
         XC    MGPURP,MGPURP       CLEAR PURPOSE CODE                           
         B     FMS010                                                           
         EJECT                                                                  
*====================================================================           
* READ THROUGH SPOT BUYLINES AND FIND MISSED SPOTS                              
*====================================================================           
         SPACE 1                                                                
FMS100   XC    MSREC,MSREC                                                      
         MVC   MSBUYLIN,=Y(1)      SKIP OVER UNKNOWN BUYLINES                   
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
         MVC   AREC,AREC1          READ BUYLINES TO REC1                        
         B     *+8                                                              
*                                                                               
FMS110   MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   FMS200              NO MORE, CHK FOR 'SPOT NOT FOUND'            
         CLI   MSTYPE,MSTYPEQ                                                   
         BL    FMS110                                                           
         BH    FMS200              NO MORE, CHK FOR 'SPOT NOT FOUND'            
         OC    MSBUYLIN,MSBUYLIN   SKIP THESE!                                  
         BZ    FMS110                                                           
*                                                                               
         CLC   KEY+6(3),MSSTTN     TEST NEXT IS SAME STATION                    
         BNE   FMS120                                                           
         CLC   KEY+11(2),MSBUYLIN  TEST NEXT IS SAME BUYLINE                    
         BE    FMS130                                                           
         SPACE 1                                                                
*================================================================*              
* READ THE KNOWN BUYLINES IN THE TSAR KEYS                       *              
*================================================================*              
         SPACE 1                                                                
FMS120   XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+6(3),MSSTTN     MOVE ACTUAL STA/NET                          
****     MVC   MYSTTN,MSSTTN       SAVE ACTUAL STATION                          
*                                                                               
         MVC   KEY+11(2),MSBUYLIN                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BNE   FMS110                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
FMS130   BRAS  RE,MATCHMS                                                       
         BNE   FMS110              NO MATCH, READ NEXT TSAR                     
*                                                                               
         MVI   T.TSACTN,TSAPUT     WRITE UPDATED RECORD                         
         BRAS  RE,CALLTSAR                                                      
         JE    FMS110                                                           
         DC    H'0'                                                             
         EJECT                                                                  
*===================================================================            
* CHECK IF ANY SPOTS NOT FOUND                                                  
*===================================================================            
FMS200   XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
         B     *+8                                                              
*                                                                               
FMS210   MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   FMSX                                                             
         CLI   MSTYPE,MSTYPEQ      MISS LINE TSAR-REC?                          
         BL    FMS210                                                           
         BH    FMSX                                                             
         OC    MSACTBUY,MSACTBUY   TEST ACTUAL BUY UNKNOWN                      
         BNZ   FMS210               YES                                         
*                                                                               
*=================================================================*             
* READ THROUGH ALL BUYLINES                                                     
*=================================================================*             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+6(3),MSSTTN     SET ACTUAL MISSED STATION                    
         GOTO1 HIGH                                                             
         B     FMS230                                                           
*                                                                               
FMS220   GOTO1 SEQ                                                              
*                                                                               
FMS230   CLC   KEY(10),KEYSAVE     NO MORE LINES FOR THIS STATION?              
         BNE   FMS210               YES, LEAVE AS SPOT NOT FOUND                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,MATCHMS                                                       
         BNE   FMS220              NO MATCH, READ SEQ FOR NEXT BUYLINE          
*                                                                               
         MVI   T.TSACTN,TSAPUT     WRITE UPDATED RECORD                         
         BRAS  RE,CALLTSAR                                                      
         B     FMS210                                                           
*                                                                               
FMSX     J     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* MATCH TSAR MISSED RECORD TO BUY DESC DATA AND THEN TO A SPOT      *           
* DPT/ADJCD/DEMEL/UPGRDEL ARE SAVED AND MAYBE USED LATER            *           
*===================================================================*           
         SPACE 1                                                                
MATCHMS  NTR1                                                                   
         L     R2,AREC                                                          
         USING BUYREC,R2                                                        
*                                                                               
         CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   MMS0A               NO                                           
         BAS   RE,CHKTRD           MATCH ON REP                                 
         JNE   NO                                                               
         B     MMS0X                                                            
*                                                                               
CHKTRD   ST    RE,FULL                                                          
         LHI   RE,VRCPACK-BUYSAVE  ONLY CHECK THAT FIRST 2 DIGITS MATCH         
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'U',BDREP),SVTRDDTA+5                                
         L     RE,FULL                                                          
         CLI   SVDARPRF+14,C'Y'    MULTIPLE TRADE CODES?                        
         BE    CHKTRD10                                                         
         CLC   SVTRDDTA+2(3),SVTRDDTA+5   DO THEY MATCH FOR 3 DIGITS?           
         BR    RE                                                               
CHKTRD10 CLC   SVTRDDTA+2(2),SVTRDDTA+5   DO THEY MATCH FOR 2 DIGITS?           
         BR    RE                                                               
*                                                                               
MMS0A    CLI   SVTRDTYP,X'99'      TEST CASH SIDE OF SPREP TRADE ORD            
         BNE   MMS0X                                                            
         BAS   RE,CHKTRD           MATCH SPECIAL REP CODE                       
         JE    NO                                                               
*&&DO                                                                           
         OC    SVESTREP,SVESTREP   DO WE HAVE AN EST REP?                       
         BZ    MMS0X               NO                                           
         CLC   BDREP,SVESTREP      DOES MISSED LINE REP MATCH EST REP?          
         JNE   NO                  NO, SKIP THIS SPOT                           
*&&                                                                             
* CALCULATE START OF WEEK FOR MISSED SPOT IN THIS BUYLINE                       
* IN CASE NO EXACT MATCH                                                        
*                                                                               
MMS0X    MVC   MGWKDATE,MSELDT                                                  
         LLC   RE,BDSEDAY          GET START/END DAYS FROM BUY                  
         SRDL  RE,4                GIVES START DAY                              
         SRL   RF,28               GIVES END DAY                                
*                                                                               
         LLC   R0,MSELDAY          GET DAY OF MISSED DATE                       
* NEED TO ADJUST IF OOWR                                                        
         CR    RE,RF               TEST OOWR (EG, SA-TU = 6-2)                  
         BNH   MMS1                                                             
         AHI   RF,7                ADD 7 TO END DATE                            
         CR    R0,RE               TEST SPOT DAY BEFORE BUY START DAY           
         BNL   *+8                 NO                                           
         AHI   R0,7                ADD 7 TO SPOT DAY (1 BECOMES 8)              
* MAKE SURE ACTUAL DAY IS IN ROTATION. IF NOT,                                  
* MAYBE THEY'RE GOING TO MISS AN OTO                                            
MMS1     CR    R0,RE               TEST SPOT DAY PRIOR TO BUY ST DAY            
         BL    MMS2                                                             
         CR    R0,RF               TEST SPOT DAY AFTER BUY END DAY              
         BH    MMS2                                                             
         SR    R0,RE                                                            
         BZ    MMS2                                                             
         LNR   R0,R0               BACK UP TO WEEK START DAY                    
         GOTO1 VADDAY,DMCB,MSELDTQ,WORK,(R0)                                    
         GOTO1 VDATCON,DMCB,WORK,(2,MGWKDATE)                                   
*                                                                               
MMS2     CLC   BDSEC,MSSLN             MATCH SLN                                
         JNE   NO                                                               
         CLC   BDTIMST(4),MSSTIM       MATCH START/END TIMES                    
         BNE   *+14                    NO - MAYBE AN ORBIT                      
*??????????? SHOULD WE MATCH ON DAYS ????                                       
         CLC   BDDAY,MSDAYS            MATCH DAYS                               
         B     MMS10                   YES - GO SEARCH FOR DATE                 
*??????????? SHOULD WE MATCH ON DAYS ????                                       
*                                                                               
         LA    R2,BDELEM                                                        
MMS4     LLC   R0,1(R2)            SEE IF THIS IS AN ORBIT                      
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    NO                                                               
         CLI   0(R2),X'67'                                                      
         BNE   MMS4                                                             
* MATCH DAY AND TIME TO ORBIT ELEMENT                                           
         LLC   R0,1(R2)                                                         
         SRL   R0,4                SET R0 FOR BCT                               
         USING ORBELEM,R2          NOTE R2 POINTS TO START OF ELEM !            
*                                                                               
MMS6     CLC   ORBDAY,MSDAYS                                                    
         BNE   MMS8                                                             
         CLC   ORBTIME,MSSTIM                                                   
         BE    MMS10                                                            
*                                                                               
MMS8     LA    R2,16(R2)                                                        
         BCT   R0,MMS6                                                          
         J     NO                  DAYS/TIMES DON'T MATCH                       
         SPACE 1                                                                
*=================================================================*             
* NEED TO SEARCH BUY RECORD FOR SPOT ON THIS DATE                               
* COSTS MUST MATCH                                                              
* IF NOT DAILY SKED, THEN SPOT MUST BE WITHIN ROTATION RANGE                    
* IF MISSED SPOT IS PAID, ONLY CONSIDER IF ALREADY -OTO'D                       
* IF MATCH, CALL CHKDUP TO MAKE SURE THIS SPOT NOT PREVIOUSLY USED              
* IF OFFER IS OKAYED, ONLY USE MISSED SPOTS                                     
* IF OFFER IS APPROVED, ONLY USE MG PENDING SPOTS                               
* ELSE DO NOT USE MISSED OR PENDING SPOTS                                       
*=================================================================*             
         SPACE 1                                                                
MMS10    L     R2,AREC                                                          
         USING BUYREC,R2                                                        
         LA    R2,BDELEM                                                        
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
         USING REGELEM,R2                                                       
MMS12    LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    NO                                                               
         CLI   0(R2),X'0B'                                                      
         BL    MMS12                                                            
         CLI   0(R2),X'0C'                                                      
         BH    MMS12                                                            
         TM    6(R2),X'80'         TEST MINUS                                   
         BO    MMS12                                                            
*                                                                               
         LLC   R0,ELEMNO                                                        
         CLC   RDATE,ELEMDT                                                     
         BE    *+6                                                              
         SR    R0,R0                                                            
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
         MVC   ELEMDT,RDATE                                                     
*                                                                               
         CLI   SVMNSTAT,MNSTSAPP                                                
         BE    MMS13                                                            
         CLI   SVMNSTAT,MNSTOKAY   TEST MG OKAYED                               
         BNE   MMS14                                                            
MMS13    TM    RSTATUS,X'42'       TEST SPOT MISSED/MG ON NEW LINE              
         BNO   MMS12                                                            
         B     MMS20                                                            
*                                                                               
MMS14    CLI   SVMNSTAT,MNSTAPP    TEST MG APPROVED                             
         BE    *+12                                                             
         CLI   SVMNSTAT,MNSTGOIN   TEST GOING TO BE OK                          
         BNE   MMS16                                                            
         TM    RSTATUS,X'10'       TEST MAKEGOOD PENDING                        
         B     MMS16               CONSIDER ANY SPOT                            
*                                                                               
MMS16    CLI   1(R2),10            TEST ALLOCATED                               
         BNH   MMS12               NO - SKIP                                    
*                                                                               
         OC    RPAY,RPAY           IS THE SPOT PAID                             
         BNZ   MMS18               YES -                                        
         TM    RSTATUS,X'40'       TEST MISSED                                  
         BO    MMS12               YES - SKIP                                   
         B     MMS20                                                            
*                                                                               
MMS18    TM    RSTATUS,X'40'       PAID - ONLY USE IF ALREADY MISSED            
         BZ    MMS12                                                            
         TM    RSTATUS,X'02'              ALREADY MADEGOOD?                     
         BO    MMS12                      YES, SKIP TO NEXT SPOT                
*                                                                               
MMS20    CLC   ELEMDT,MSELDT       TEST MATCH ON DATE                           
         BE    MMS22               YES                                          
         TM    BDSTAT2,X'80'       DAILY BUY                                    
         BO    MMS12               YES - MUST GET EXACT MATCH                   
         CLC   ELEMDT,MGWKDATE     ELSE MATCH TO START OF WEEK                  
         BNE   MMS12                                                            
*                                                                               
MMS22    CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   MMS23                                                            
         CLI   SVDARPRF+12,C'Y'    YES, ZERO COST TRADE?                        
         BNE   MMS23               NO, CHECK COST                               
         OC    MSCOST,MSCOST       YES, MAKE SURE COST IS ZERO                  
         BNZ   MMS12                                                            
         B     MMS26                                                            
*                                                                               
MMS23    L     RE,AREC1            CHECK MATCH ON COST                          
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST-BUYREC(RE)                                           
*                                                                               
         CLI   SVTRDTYP,C'R'       TEST TRADE SIDE OF SPREP TRADE ORD           
         BNE   MMS24                                                            
         TM    SVCOPT4,X'02'       CLT USING EARNED DISCOUNT FOR TRADE?         
         BZ    MMS24                                                            
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,X'71'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   MMS12                                                            
         ICM   R0,15,2(R6)                                                      
*                                                                               
MMS24    TM    RSTATUS,X'20'       TEST COST OVERRIDE                           
         BZ    *+8                 NO                                           
         ICM   R0,7,RPCOST                                                      
         CLM   R0,7,MSCOST                                                      
         BNE   MMS12                                                            
*                                                                               
MMS26    CLI   1(R2),14            TEST P/B SPOT                                
         BH    MMS28               YES                                          
         CLI   MGBPRD2,0           NO, TEST PIGGYBACK ENTERED                   
         BNE   MMS12                 YES, SKIP IT                               
         SR    R0,R0                                                            
         ICM   R0,1,SVPOLPRD                                                    
         BNZ   *+8                                                              
         ICM   R0,1,MGBPRD1                                                     
         CLM   R0,1,RPPRD          MATCH PRODUCT                                
         BNE   MMS12               NO                                           
         B     MMS40                                                            
*                                                                               
MMS28    CLI   MGBPRD2,0           TEST PIGGYBACK ENTERED                       
         BE    MMS12               NO                                           
         CLC   MGBPRD1,RPPRD       PRD1 = PRD1                                  
         BNE   MMS30               NO                                           
         CLC   MGBPRD2,RPPRD+4     PRD2 = PRD2                                  
         BNE   MMS12                                                            
         B     MMS40                                                            
*                                                                               
MMS30    CLC   MGBPRD2,RPPRD       PRD2 = PRD1                                  
         BNE   MMS12                                                            
         CLC   MGBPRD1,RPPRD+4                                                  
         BNE   MMS12                                                            
*                                                                               
MMS40    CLI   0(R2),X'0B'         TEST REGEL                                   
         BNE   MMS42                                                            
* NWS SOMETIMES PUTS REGELS OUTSIDE BUYDESC DATES - SO CHECK FOR THAT           
         GOTO1 VDATCON,DMCB,(2,2(R2)),(3,FULL)                                  
         L     RE,AREC                                                          
         CLC   FULL(3),BDSTART-BUYREC(RE)                                       
         BL    MMS12                                                            
         CLC   FULL(3),BDEND-BUYREC(RE)                                         
         BH    MMS12                                                            
*                                                                               
MMS42    BAS   RE,CHKDUP           CHECK DUPLICATE MISSED SPOT                  
         BNE   MMS12               TRY AGAIN                                    
*                                                                               
         MVC   MSACTDAT,ELEMDT     SET DATE THAT MATCHED                        
         MVC   MSACTEL,ELEMNO      SET SPOT FOUND FLAG IN TSAR REC              
         MVC   MSACTBUY,KEY+11     SET ACTUAL BUYLINE IN TSAR REC               
*                                  NOTE THAT MSACTDAT IS SET                    
         TM    6(R2),X'20'         TEST SPOT HAS COST OVERRIDE                  
         BZ    *+8                                                              
         OI    MSIND,X'80'         SET FLAG IN TSAR REC                         
         L     RE,AREC1                                                         
         MVC   MSPROG,BDPROGRM-BUYKEY(RE)                                       
         MVC   MSDPT,BDDAYPT-BUYKEY(RE)                                         
         MVC   MSADJ,BDPROGT-BUYKEY(RE)                                         
         MVC   MSREP,BDREP-BUYKEY(RE)                                           
* FIND DEMO IN BUYREC *                                                         
         LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         LA    R0,17(R4)           END OF 6TH DEMO IN LIST                      
         ST    R0,FULL                                                          
         LA    R5,MSDEM1                                                        
*                                                                               
MMS43    L     R2,AREC                                                          
         USING BUYREC,R2                                                        
         LA    R2,BDELEM                                                        
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),2                                                          
         JNE   *+2                                                              
         LLC   R0,1(R2)            HAVING LEN OF THIS ELEMENT HELPS !           
         AHI   R0,-24                                                           
         BNP   MMS50                                                            
         SRL   R0,3                SET FOR BCT ON NUM DEMOS                     
         LA    R1,24(R2)                                                        
*                                                                               
MMS44    CLC   0(3,R4),0(R1)                                                    
         BE    MMS46                                                            
         LA    R1,8(R1)                                                         
         BCT   R0,MMS44                                                         
         B     MMS54                                                            
*                                                                               
MMS46    L     RF,4(R1)                                                         
         N     RF,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         CLI   1(R1),C'R'          ONLY AMERICAN RATINGS?                       
         BE    MMS48                                                            
         CLI   1(R1),C'E'          AND ESKIMO RATINGS                           
         BNE   MMS52               CAN HAVE 2-DECIMAL DEMO RATING               
MMS48    LHI   RE,SV00PROF-BUYSAVE                                              
         AR    RE,RA                                                            
         CLI   9(RE),C'Y'          2-DECIMAL RATING TURNED ON?                  
         BNE   MMS50               NO                                           
         TM    4(R1),X'40'         YES, ALREADY 2-DEC RATING?                   
         BO    *+8                 YES,                                         
         MHI   RF,10               NO, MULTIPLY BY 10                           
         O     RF,=X'40000000'     TURN ON 2-DEC RATING                         
         B     MMS52                                                            
*                                                                               
MMS50    TM    4(R1),X'40'         DID WE GET 2-DEC RATING?                     
         BZ    MMS52                                                            
         SR    RE,RE                                                            
         M     RE,=F'2'                                                         
         AHI   RF,1                                                             
         D     RE,=F'10'                                                        
         SRL   RF,1                                                             
MMS52    ST    RF,0(R5)                                                         
*                                                                               
MMS54    LA    R4,3(R4)            NEXT DEMO IN LIST                            
         C     R4,FULL             TEST DONE 6 DEMOS                            
         BH    MMS56               YES - DONE                                   
         LA    R5,4(R5)            NEXT SAVE DEMO                               
         OC    0(3,R4),0(R4)       TEST ANY MORE DEMOS                          
         BNZ   MMS43                                                            
*                                                                               
MMS56    DS    0H                                                               
**                                                                              
** SET OPTIONAL PURPOSE CODES TO NOT REQUIRE PURPOSE CODES                      
**                                                                              
**       CLIY  SVB0PROF+9,C'O'     OPTIONAL PURPOSE CODES                       
**       JE    *+14                SHOULD STILL MATCH PREVIOUS                  
         CLIY  SVB0PROF+9,C'Y'     TEST USING PURPOSE CODES                     
         BNE   MMS70                                                            
*                                                                               
         L     R2,AREC                                                          
         USING BUYREC,R2                                                        
         LA    R2,BDELEM                                                        
MMS58    LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JNE   MMS60                                                            
         CLI   9(RF),C'Y'          TEST USING PURPOSE CODES                     
         BE    MMS68                                                            
         OC    MGPURP,MGPURP       TEST SAVED PURPOSE CODE YET                  
         BNZ   *+10                                                             
         MVC   MGPURP,SPACES       THEN SET TO SPACE                            
         CLC   MGPURP,SPACES       WAS PREVIOUS ALSO SPACE?                     
         BNE   MMS68                NO, ERROR                                   
         B     MMS70                                                            
*                                                                               
MMS60    CLI   0(R2),X'70'                                                      
         BNE   MMS58                                                            
         OC    MGPURP,MGPURP       TEST SAVED PURPOSE CODE YET                  
         BNZ   *+10                                                             
         MVC   MGPURP,3(R2)        MOVE PURPOSE CODE                            
         CLC   MGPURP,3(R2)        TEST SAME PURPOSE CODE AS PRV                
         BE    MMS70                                                            
MMS68    MVC   NERRCD,=Y(MISSPURP)                                              
         J     ERREXITN                                                         
*                                                                               
MMS70    J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ THROUGH TSAR RECORDS AND CHECK FOR DUPLICATE MISSED SPOT                 
* CURRENT TSAR RECNUM IS SAVED AND RESTORED AT END OF ROUTINE                   
* RETURN WITH CC NEQ IF DUPLICATE FOUND                                         
*=====================================================================*         
         SPACE 1                                                                
CHKDUP   NTR1                                                                   
         MVC   HALF,T.TSRNUM       SAVE CURRENT TSAR RECNUM                     
*                                                                               
         XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
CHKDUP2  BRAS  RE,CALLTSAR                                                      
         BNE   CHKDUP10                                                         
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BL    CHKDUP2                                                          
         BH    CHKDUP10                                                         
*                                                                               
         CLC   MSSTTN,KEY+6        SAME STATION?                                
         BNE   CHKDUP2                                                          
         CLC   MSACTBUY,KEY+11     SAME LINE NUMBER                             
         BNE   CHKDUP2             NO                                           
         CLC   ELEMDT,MSACTDAT     SAME SPOT DATE                               
         BNE   CHKDUP2                                                          
         CLC   ELEMNO,MSACTEL      SAME SPOT NUMBER                             
         BNE   CHKDUP2                                                          
         MVI   BYTE,1              WILL BE USED FOR CCNEQ ON EXIT               
         B     CHKDUP12                                                         
*                                                                               
CHKDUP10 MVI   BYTE,0              WILL BE USED FOR CCEQ ON EXIT                
*                                                                               
CHKDUP12 MVI   T.TSACTN,TSAGET     RESTORE CURRENT TSAR RECORD                  
         MVC   T.TSRNUM,HALF                                                    
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         CLI   BYTE,0                                                           
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* READ THROUGH TSAR RECORDS FOR MISSED SPOTS                                    
* AND SET MAKEGOOD PENDING FLAGS IN ELEMENTS                                    
*=====================================================================*         
         SPACE 1                                                                
SETPNDG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MSREC,MSREC                                                      
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
SET8     BRAS  RE,CALLTSAR                                                      
         BNE   SETX                                                             
*                                                                               
         MVI   T.TSACTN,TSANXT                                                  
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   SET8                                                             
*                                                                               
SET10    XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+6(3),MSSTTN                                                  
         MVC   KEY+11(2),MSACTBUY  SET ACTUAL BUYLINE                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JE    SET11                                                            
         CLI   MGFLAG,C'U'         TEST UNPEND ACTION                           
         JE    SET8                                                             
         DC    H'0'                                                             
*                                                                               
SET11    MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
SET12    XC    ELEMDT,ELEMDT                                                    
         L     R2,AREC                                                          
         USING BUYREC,R2                                                        
         LA    R2,BDELEM                                                        
         USING REGELEM,R2                                                       
*                                                                               
SET14    LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   SET14X                                                           
         XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(36),=C'BUYLINES HAVE CHANGED - PLEASE RETRY'              
         MVI   ERRAREA,X'FF'                                                    
         TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK                                     
         JNZ   *+2                 NO $ABEND FOR SPOT DESKTOP OR MM             
         DC    H'0',C'$ABEND'                                                   
*                                                                               
SET14X   CLI   0(R2),X'0B'                                                      
         BL    SET14                                                            
         CLI   0(R2),X'0C'                                                      
         BH    SET14                                                            
         TM    6(R2),X'80'         TEST MINUS                                   
         BO    SET14                                                            
*                                                                               
         LLC   R0,ELEMNO                                                        
         CLC   RDATE,ELEMDT                                                     
         BE    *+6                                                              
         SR    R0,R0                                                            
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
         MVC   ELEMDT,RDATE                                                     
*                                                                               
         OC    RPAY,RPAY           TEST PAID                                    
         BNZ   SET16               YES                                          
*                                                                               
         TM    RSTATUS,X'40'       TEST MISSED                                  
         BO    SET14               YES - SKIP                                   
         CLI   1(R2),10            TEST ALLOCATED                               
         BNH   SET14               NO - SKIP                                    
         B     SET20                                                            
*                                                                               
SET16    TM    RSTATUS,X'40'       IF PAID, MUST BE MISSED ALREADY              
         BZ    SET14               NO - SKIP                                    
*                                                                               
SET20    CLC   ELEMDT,MSACTDAT     TEST MATCH ON DATE                           
         BNE   SET14               YES                                          
         CLC   ELEMNO,MSACTEL      TEST MATCH ON SPOT NUM                       
         BNE   SET14                                                            
         CLI   MGFLAG,C'U'         TEST UNPEND ACTION                           
         BE    SET26                                                            
         OI    6(R2),X'10'         SET SPOT PENDING FLAG                        
         B     SET28                                                            
*                                                                               
SET26    NI    6(R2),X'FF'-X'10'                                                
*                                                                               
SET28    MVI   T.TSACTN,TSANXT                                                  
         BRAS  RE,CALLTSAR                                                      
         BNE   SET30                                                            
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   SET30                                                            
         CLC   MSSTTN,KEY+6        TEST SAME STATION?                           
         BNE   SET29                                                            
         CLC   MSACTBUY,KEY+11     TEST SAME BUYLINE                            
         BE    SET12               YES                                          
SET29    GOTO1 PUTREC              WRITE PREVIOUS BUY                           
         B     SET10               AND READ NEW                                 
*                                                                               
SET30    GOTO1 PUTREC              WRITE LAST BUYREC                            
         SPACE 1                                                                
*==============================================================                 
* RESTORE THE MGE SCREEN                                                        
*==============================================================                 
         SPACE 1                                                                
SETX     GOTO1 VCALLOV,DMCB,BUYHL1H,X'D90211F3'                                 
         MVI   SVSCR,X'F3'                                                      
*                                                                               
         XC    MGEINP1,MGEINP1                                                  
         LA    R2,MGEINP1                                                       
*                                                                               
         TM    MGMSCFLG,MGMFSAPP   SELF-APPLY?                                  
         BZ    SETX20              NO, DON'T BOTHER                             
*                                                                               
         MVC   0(7,R2),=C'MGESAP='                                              
         MVC   7(3,R2),MGGRPCOD                                                 
         LA    R2,9(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   SVCSHTRD,0                                                       
         BE    SETX10                                                           
         MVI   0(R2),C'/'                                                       
         MVC   1(1,R2),SVCSHTRD                                                 
         LA    R2,2(R2)                                                         
*                                                                               
SETX10   CLC   =C'POL',BUYPR        IF HEADLINE PRD IS POL                      
         BNE   SETX30                                                           
         MVI   0(R2),C','                                                       
         MVC   1(3,R2),MGQPRD1                                                  
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         OC    MGQPRD2,MGQPRD2                                                  
         BZ    SETX15                                                           
         MVI   1(R2),C'-'                                                       
         MVC   2(3,R2),MGQPRD2                                                  
         LA    R2,5(R2)                                                         
SETX15   CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         B     SETX30                                                           
*                                                                               
SETX20   MVC   0(4,R2),=C'MGE='                                                 
         MVC   4(3,R2),MGGRPCOD                                                 
*                                                                               
         LA    R2,6(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
SETX30   LA    R0,MGEINP1          CALCULATE LENGTH OF INPUT LINE               
         SR    R2,R0                                                            
         STC   R2,MGEINP1H+5                                                    
*                                                                               
         LA    R2,MGEINP1H                                                      
         MVI   4(R2),X'80'         INPUT THIS TIME                              
         MVI   6(R2),X'80'         TRANSMIT                                     
*                                                                               
         CLI   SVDARPRF+13,C'N'    SELF-APPLY ON?                               
         JNE   EXIT                                                             
         XC    MGEPF3,MGEPF3                                                    
         MVC   MGEPF3(7),=X'C6F17ED9A39995'  F1=RTRN IN LOWER CASE              
         OI    MGEPF3+6,X'80'      TRANSMIT                                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ MAKEGOOD NOTICE RECORD INTO AREC2                                        
*=====================================================================*         
         SPACE 1                                                                
READMKN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BUYST,C'0'          TEST US CABLE                                
         BNL   RDMKN10                                                          
*                                                                               
         XC    KEY,KEY                                                          
K        USING DAREMGND,KEY                                                     
         MVI   K.MNKTYPE,MNKTYPQ                                                
         MVI   K.MNKSUBTY,MNKSTYPQ                                              
         MVC   K.MNKAGMD,SVAGYMD                                                
         MVC   K.MNKBYR,BUYBU                                                   
         OC    K.MNKBYR,SPACES                                                  
         MVC   K.MNKORDER,MGORDER                                               
         MVC   K.MNKGROUP,MGGRPCOD                                              
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,MYHIGH                                                        
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDMKN2                                                           
         MVI   ERRCD,NOTFOUND                                                   
         MVI   SVMGINIT,C'N'       INVALIDATE ALL INPUT                         
         NI    MGEINP1H+4,X'DF'                                                 
         J     ERREXIT                                                          
*                                                                               
RDMKN2   MVC   AREC,AREC2                                                       
         BRAS  RE,MYGETREC                                                      
         J     RDMKNX                                                           
*                                                                               
RDMKN10  XC    WORK,WORK           THIS CODE FOR CABLE                          
K        USING DAREMGND,WORK                                                    
         MVI   K.MNXKTYPE,MNXKTYPQ                                              
         MVI   K.MNXKSBTY,MNXKSBTQ                                              
         MVC   K.MNXKAGMD,SVAGYMD                                               
         MVC   K.MNXKORDR,MGORDER                                               
         MVC   K.MNXKGRP,MGGRPCOD                                               
         MVC   K.MNXKSTTN,MYSTTN                                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,MYHIGH                                                        
*                                                                               
         CLC   WORK(28),WORK2                                                   
         BE    RDMKN12                                                          
         MVI   ERRCD,NOTFOUND                                                   
         MVI   SVMGINIT,C'N'       INVALIDATE ALL INPUT                         
         NI    MGEINP1H+4,X'DF'                                                 
         J     ERREXIT                                                          
*                                                                               
RDMKN12  MVC   AREC,AREC2                                                       
         BRAS  RE,MYGETREC                                                      
*                                                                               
RDMKNX   NI    DMINBTS,X'F7'       RESET PASS DELETES                           
         J     EXIT                                                             
         DROP  K                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ THE MAKEGOOD OFFER RECORDS INTO REC3/REC4                      *         
*=====================================================================*         
         SPACE 1                                                                
READMKO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BUYST,C'0'          TEST LOCAL CABLE MG                          
         BNL   RDMKO10              YES                                         
*                                                                               
         XC    KEY,KEY             HAVE BROADCAST MG                            
K        USING MOKEY,KEY                                                        
         MVI   K.MOKTYPE,MOKTYPQ                                                
         MVI   K.MOKSUBTY,MOKSTYPQ                                              
         MVC   K.MOKAGMD,SVAGYMD                                                
         MVC   K.MOKORDER,MGORDER                                               
         MVC   K.MOKMGCD,MGGRPCOD                                               
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,MYHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   MGMKODA1,KEY+14     SAVE DA OF FIRST REC                         
         XC    MGMKODA2,MGMKODA2   CLEAR SECOND REC DA                          
*                                                                               
         L     R6,AREC3            USE AREC3 FOR OFFER RECORD                   
         ST    R6,AREC                                                          
****     USING MOKEY,R6                                                         
*                                                                               
         BRAS  RE,MYGETREC                                                      
*                                                                               
         L     R6,AREC                                                          
****     USING MOKEY,R6                                                         
*                                                                               
         AH    R6,DATADISP                                                      
         USING MOSQELD,R6                                                       
*                                                                               
         LLC   R0,MOSQNUM                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MGVRSNQ,DUB         SAVE EBCDIC VERSION                          
         DROP  R6                                                               
*                                                                               
RDMKO02  L     R6,AREC4                                                         
         ST    R6,AREC                                                          
         XC    0(256,R6),0(R6)                                                  
         BRAS  RE,MYSEQ            TEST FOR ANOTHER MKO RECORD                  
*                                                                               
         CLC   KEY(10),KEYSAVE     TYPE/MGORD/CODE                              
         BNE   RDMKOX                                                           
         CLI   KEY+10,MOKSEQDQ     X'FF' DEFAULT DEMO RECORD?                   
         BE    RDMKOX               YES, IGNORE IT                              
         CLI   KEY+10,MOKSQSDQ     X'FE' SELLER DEMO RECORD?                    
         BE    RDMKOX               YES, IGNORE IT                              
*                                                                               
         TM    KEY+MOKSTAT-MOKEY,X'80' SEQ RECORD DELETED?                      
         BZ    RDMKO04                  NO                                      
         CLI   SVMNSTAT,MNSTCAN        CANCEL STATUS?                           
         BNE   RDMKOX                   NO, SKIP IT                             
*                                                                               
RDMKO04  MVC   MGMKODA2,KEY+14     SAVE DISK ADDRESS                            
         MVC   MYDA,KEY+14         PUT HERE FOR MYGETREC                        
         B     RDMKO14                                                          
         EJECT                                                                  
*                                                                               
* READ CABLE OFFER RECORD                                                       
*                                                                               
RDMKO10  XC    WORK,WORK                                                        
K        USING MOXKEY,WORK                                                      
*                                                                               
         MVI   K.MOXKTYPE,MOXKTYPQ                                              
         MVI   K.MOXKSBTY,MOXKSBTQ                                              
         MVC   K.MOXKAGMD,SVAGYMD                                               
         MVC   K.MOXKORDR,MGORDER                                               
         MVC   K.MOXKMGCD,MGGRPCOD                                              
         MVC   K.MOXKSTTN,MYSTTN   SET STA/NET                                  
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,MYHIGH                                                        
         CLC   WORK(30),WORK2      SAME A-M/ORD/GRP/STA                         
         BNE   RDMKOX              NO MORE!                                     
*                                                                               
         MVC   MGMKODA1,MYDA       SAVE DA OF FIRST REC                         
         XC    MGMKODA2,MGMKODA2   CLEAR SECOND REC DA                          
*                                                                               
         L     R6,AREC3            USE AREC3 FOR OFFER RECORD                   
         ST    R6,AREC                                                          
         BRAS  RE,MYGETREC                                                      
*                                                                               
         L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
*                                                                               
         CLI   0(R6),MOSQELQ                                                    
         BNE   RDMKO12                                                          
*                                                                               
         USING MOSQELD,R6                                                       
         LLC   R0,MOSQNUM                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MGVRSNQ,DUB         SAVE EBCDIC VERSION                          
         DROP  R6                                                               
*                                                                               
RDMKO12  L     R6,AREC4                                                         
         ST    R6,AREC                                                          
         XC    0(256,R6),0(R6)                                                  
*                                                                               
         MVC   WORK2,WORK                                                       
         BRAS  RE,MYSEQ                                                         
*                                                                               
         CLC   WORK(30),WORK2      TYPE/A-M/ORD/GRP/STN-NTWK                    
         BNE   RDMKOX                                                           
         CLI   WORK+30,0           HAVE DATA TYPE RECORD?                       
         BNE   RDMKOX                                                           
*                                                                               
         TM    WORK+MOXKSTAT-MOXKEY,X'80' SEQ RECORD DELETED?                   
         BZ    RDMKO13                     NO                                   
         CLI   SVMNSTAT,MNSTCAN           CANCEL STATUS?                        
         BNE   RDMKOX                      NO, SKIP IT                          
*                                                                               
RDMKO13  MVC   MGMKODA2,MYDA       SAVE THE DISK ADDRESS                        
                                                                                
RDMKO14  BRAS  RE,MYGETREC         READ MKO CONTINUATION REC                    
*                                                                               
RDMKOX   NI    DMINBTS,X'F7'       RESET PASS DELETES                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* READ THE ORDER RECORD                                                         
*===============================================================                
         SPACE 1                                                                
READORD  NTR1  BASE=*,LABEL=*                                                   
         MVC   NERRCD,=Y(BADORDER)                                              
*                                                                               
         XC    KEY,KEY             READ THE ORDER RECORD                        
K        USING DOKEY,KEY                                                        
*                                                                               
         MVI   K.DCKTYPE,DCKTYPQ                                                
         MVI   K.DCKSUBTY,DCKSTYPQ                                              
         MVC   K.DCKAGMD,SVAGYMD                                                
         MVC   K.DCKCLT,SVCLT                                                   
         MVC   K.DCKPRD,MGBPRD1                                                 
         MVC   K.DCKEST,SVEST                                                   
         MVC   K.DCKSTA,SVSTA                                                   
         CLI   BUYST,C'0'          TEST LOCAL CABLE                             
         BL    *+8                                                              
         NI    K.DCKSTA+2,X'80'    DROP NETWORK                                 
         MVC   K.DCKPRD2,MGBPRD2                                                
         MVC   K.DCKFLTNM,MGFLTCOD                                              
         CLI   SVCSHTRD,C'T'       IS THIS A TRADE MG                           
         BNE   *+8                                                              
         MVI   K.DCKFLAG,X'80'     SET TRADE FLAG                               
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         NI    KEY+12,X'FF'-DCKFSCSW  CLEAR THE X'01' BIT                       
         CLC   KEY(13),KEYSAVE     GOT THE SAME KEY?                            
         JNE   ERREXITN            NO ORDERS FOR THIS CLT/PRD/EST..             
*                                                                               
         MVC   MGORDDA,KEY+14      SAVE DISK ADDR IN CASE NEEDED                
         L     R6,AREC1            USE AREC1 FOR ORDER RECORD                   
         ST    R6,AREC                                                          
         USING DOKEY,R6                                                         
*                                                                               
         GOTO1 GETREC                                                           
         MVC   MGORDER,DOKORDER    SAVE ORDER NUMBER FROM KEY                   
         DROP  R6                                                               
*                                                                               
         BRAS  RE,CLCORDDT         SET ORDRDATE BASED OFF ORDER #               
*                                                                               
         XC    MGPASSWD,MGPASSWD                                                
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,DOWHOELQ                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   READORDX                                                         
*                                                                               
         USING DOWHOEL,R6                                                       
         MVC   MGPASSWD,DOWHOPID                                                
*                                                                               
READORDX J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALCULATE ORDRDATE PWOS JULIAN DATE                                           
*  ON ENTRY :  ORDER # IS IN MGORDER                                            
*              ORDER RECORD IS IN AREC1                                         
*                                                                               
*   ON EXIT :  ORDRDATE IS THE ORDER CREATION DATE IN PWOS JULIAN               
***********************************************************************         
                                                                                
CLCORDDT NTR1  BASE=*,LABEL=*                                                   
         MVC   HALF,MGORDER        THE 1ST 2 BYTES OF ORDER #                   
         XC    HALF,=X'FFFF'                                                    
*                                                                               
         TM    HALF,X'80'          NEW STYLE ORDER NUMBER?                      
         BZ    CALODT20                                                         
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,DOSPELQ      X'03' ELEM                                   
         BRAS  RE,NEXTEL                                                        
         BNE   CALODTDI            WE SHOULD ALWAYS HAVE ONE                    
         USING DOSPELD,R6                                                       
         OC    DOSPCDAT,DOSPCDAT                                                
         BZ    CALODTDI                                                         
         MVC   ORDRDATE,DOSPCDAT                                                
         B     CALODTX                                                          
         DROP  R6                                                               
*                                                                               
CALODT20 SR    R1,R1                                                            
         ICM   R1,3,HALF           CONTAINS THE DATE OF CREATION                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   HALF,DUB+6          SAVE OFF THE DDDF                            
         SRP   DUB,64-3,0          ISOLATE # OF YEARS FROM 1990                 
         AP    DUB,=P'90'           AS ORDER #'S BASED OFF 1990                 
         SRP   DUB,3,0                                                          
         MVC   DUB+6(2),HALF       PUT BACK  DDDF TO GET A FULL DATE            
         SRP   DUB,1,0                                                          
         MVC   ORDRDATE,DUB+4      WE NOW HAVE PWOS JULIAN                      
*                                                                               
CALODTX  J     EXIT                                                             
*                                                                               
CALODTDI DC    H'0'                SHOULD NEVER GET HERE                        
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* BUILD A DUMMY BUY RECORD AND LOOK UP DEMOS                                    
* NOTE UPGRADE IS DETERMINED FROM THE INPUT UPGRADE FIELD                       
* UPGRADES ENTERED IN THE OPTIONS FIELD ARE COPIED DOWN                         
*======================================================================         
         SPACE 1                                                                
DEMOLOOK NTR1  BASE=*,LABEL=*,WORK=(R9,DMWORKL)                                 
         USING DMWORKD,R9                                                       
         MVC   DMQSTA,QSTA                 SAVE THE REAL STATION                
         MVC   DMBSTA,BUYMSTA+2            SAVE STATION FROM KEY                
*                                                                               
         CLI   MGLOOKUP,C'N'       TEST NO LOOKUP REQ'D                         
         BE    DLK040              GO BUILD X'60' DEMO ELEMENT                  
*                                                                               
         BRAS  RE,BLDDBUY          BUILD DUMMY BUY                              
*                                                                               
         LA    R2,MKOUPGDH         POINT TO INPUT FLDHDR                        
         CLI   8(R2),C' '          TEST UPGRADE PRESENT                         
         BNH   DLK030              NO                                           
         CLC   =C'UPT=X',8(R2)     OR SUPPRESSED                                
         BE    DLK030                                                           
*                                                                               
* SBTK IS SOMEHOW UPLOAD A BOOK HERE INSTEAD OF AN UPGRADE                      
* AND ITS DIEING IN THE UPGRADE VALIDATION             -HWON 7/13/2017          
*                                                                               
         CLC   =C'UPT=',8(R2)      VALID UPGRADE?                               
         BE    DLK010               YES                                         
         CLC   =C'UPP=',8(R2)                                                   
         BNE   DLK030               NO                                          
*                                                                               
DLK010   LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         MVI   FSTOPS,C'='                                                      
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL              RESCAN FOR UPEDT COMPATABILITY               
*                                                                               
         MVI   EDTVAL,UPEDT                                                     
         GOTO1 CALLEDT                                                          
         L     RE,ADBLOCK                                                       
         USING SPDEMUPD,RE                                                      
         LAY   RF,BUUPGD                                                        
         MVC   SVUPIN,0(RF)        SAVE EBCDIC INPUT                            
         MVC   SVUPOPTS,SPUPTYPE   SAVE UPGRADE DEFAULT VALUES                  
         MVC   SVOPTBK,SPUPFBK                                                  
         MVC   SVUPFIL,SPUPFIL                                                  
         DROP  RE                                                               
*                                                                               
DLK020   MVI   EDTVAL,UPEDT        CALL UPEDT TO FORMAT DBLOCK NOW              
         MVI   ADBLOCK,C'X'        SET FLAG FOR DUMMY EDIT                      
         GOTO1 CALLEDT                                                          
         GOTO1 DEMUPGD                                                          
*                                                                               
         L     RE,ADBLOCK                                                       
         USING SPDEMUPD,RE                                                      
         XC    SVUPIN,SVUPIN       CLEAR UPGRADE                                
         XC    SVUPOPTS,SVUPOPTS                                                
         XC    SVOPTBK,SVOPTBK                                                  
         XC    SVUPFIL,SVUPFIL                                                  
         DROP  RE                                                               
         B     DLK040                                                           
*                                                                               
DLK030   GOTO1 DEMLKUP                                                          
         SPACE 1                                                                
*==========================================================                     
* FORMAT A X'60' (MAKEGOOD OFFER) DEMO ELEMENT IN ELEM                          
* FROM THE X'02' DEMO ELEMENT IN DUMMY BUYREC                                   
* OVERHEAD IS 4 BYTES + 8*N'DEMOS AND ELCODE=X'60'                              
*==========================================================                     
         SPACE 1                                                                
DLK040   MVC   BUYMSTA+2(3),DMBSTA      RESTORE STATION TO KEY                  
         MVC   QSTA(5),DMQSTA           RESTORE THE REAL STATION                
*                                                                               
         XC    ELEM,ELEM           RESET MODMEL                                 
         XC    MSTRBUY,MSTRBUY     RESET MONTEL                                 
         LA    RE,BDELEM                                                        
         SR    RF,RF                                                            
         ICM   RF,1,1(RE)                                                       
         AR    RE,RF               POINT TO NDELEM IN DUMMY BUY                 
*                                                                               
         IC    RF,1(RE)            GET DEMEL LENGTH                             
         AHI   RF,-24              ADJ FOR OVERHD                               
         BNP   DLKX                                                             
         LA    RE,24(RE)           POINT TO NDELEM DEMO LIST                    
         SRL   RF,3                SET FOR BCT ON NUM DEMOS                     
         LA    R1,ELEM+4           BUILD MODMEL IN ELEM                         
         LA    R2,MSTRBUY+4        BUILD MONTEL IN MSTRBUY                      
*                                                                               
DLK050   CLI   2(RE),0             NON-TRAD DEMO?                               
         BE    DLK060                                                           
         MVC   0(8,R1),0(RE)       SAVE TRAD DEMO IN X'60' MODMEL               
         LA    R1,8(R1)            NEXT ENTRY IN X'60'                          
         B     DLK070                                                           
DLK060   MVC   0(8,R2),0(RE)       SAVE NON-TRAD DEMO IN X'5E' MONTEL           
         LA    R2,9(R2)            NEXT ENTRY IN X'5E'                          
DLK070   LA    RE,8(RE)            NEXT DEMO IN DUMMY BUY NDELEM                
         BCT   RF,DLK050                                                        
*                                                                               
         OC    ELEM,ELEM           DO WE HAVE NIELSEN DEMOS?                    
         BZ    DLK080               NO                                          
         MVI   ELEM,MODMELQ        SET X'60' ELEMENT CODE                       
         MVC   ELEM+2(2),MORECID   SET OFFERNUM/SEQNUM                          
         LA    RF,ELEM                                                          
         SR    R1,RF               GET LENGTH                                   
         STC   R1,ELEM+1           SET LENGTH                                   
*                                                                               
DLK080   OC    MSTRBUY,MSTRBUY     DO WE HAVE NON-TRAD DEMOS?                   
         BZ    DLK090               NO                                          
         MVI   MSTRBUY,MONTELQ      X'5E' SET ELEMENT CODE                      
         MVC   MSTRBUY+2(2),MORECID SET OFFERNUM/SEQNUM                         
         LA    RF,MSTRBUY                                                       
         SR    R2,RF               GET LENGTH                                   
         STC   R2,MSTRBUY+1        SET LENGTH                                   
*                                                                               
DLK090   CLI   MKOUPGD,C' '        TEST UPGRADE DONE                            
         BNH   DLKX                NO                                           
         CLC   =C'UPT=X',8(R2)     OR SUPPRESSED                                
         BE    DLKX                                                             
*                                                                               
* SBTK IS SOMEHOW UPLOAD A BOOK HERE INSTEAD OF AN UPGRADE                      
*                                                      -HWON 7/13/2017          
*                                                                               
         CLC   =C'UPT=',8(R2)      VALID UPGRADE?                               
         BE    DLK100               YES                                         
         CLC   =C'UPP=',8(R2)      VALID UPGRADE?                               
         BNE   DLKX                 NO                                          
*                                                                               
* ALL UPGRADE DEMOS ARE OVERRIDES                                               
*                                                                               
DLK100   LLC   R0,ELEM+1                                                        
         AHI   R0,-4                                                            
         SRL   R0,3                SET FOR BCT ON NUM DEMOS                     
         LA    R1,ELEM+4                                                        
*                                                                               
         OI    4(R1),X'80'         SET OVERRIDE FLAG                            
         LA    R1,8(R1)                                                         
         BCT   R0,*-8                                                           
*                                                                               
DLKX     J     EXIT                                                             
         LTORG                                                                  
         DROP  R9                                                               
*                                                                               
DMWORKD  DSECT                   ** DEMOLOOK LOCAL WORKING STORAGE **           
DMQSTA   DS    CL5                                                              
DMBSTA   DS    XL3                                                              
DMWORKL  EQU   *-DMWORKD                                                        
T21131   CSECT                                                                  
         EJECT                                                                  
*=================================================================              
* TEST VALID MAKEGOOD UPGRADE                                                   
*=================================================================              
         USING TVMGWRKD,R9                                                      
TSTVMGUP NTR1  BASE=*,LABEL=*,WORK=(R9,TVMGWRKL)                                
*                                                                               
         MVC   MYSTTN,MOSTTN       SET CABLE STA/NET                            
         BRAS  RE,READMKO          READ THE MAKEGOOD OFFER                      
*                                                                               
         L     R6,AREC3            CHECK FOR UPGRADE ELEMENT IN REC             
****     AH    R6,DSPFRST                                                       
         MVI   ELCODE,MOUPGELQ     X'61'                                        
****     BRAS  RE,FIRSTEL                                                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
TVMU020  BRAS  RE,NEXTEL                                                        
         BNE   TVMUX                                                            
         USING MOUPGELD,R6                                                      
         CLC   MORECID,MOUPGOFR                                                 
         BNE   TVMU020                                                          
         CLI   MOUPGTXT,C' '       HAVE VALID UPGRADE?                          
         BNH   TVMUX                                                            
         CLC   =C'UPT=X',MOUPGTXT  OR SUPPRESSED                                
         BE    TVMUX                                                            
*                                                                               
* SBTK IS SOMEHOW UPLOAD A BOOK HERE INSTEAD OF AN UPGRADE                      
* AND ITS DIEING IN THE UPGRADE VALIDATION             -HWON 7/13/2017          
*                                                                               
         CLC   =C'UPT=',MOUPGTXT   VALID UPGRADE?                               
         BE    TVMU030              YES                                         
         CLC   =C'UPP=',MOUPGTXT                                                
         BNE   TVMUX                NO                                          
*                                                                               
TVMU030  LHI   RE,SVMGSAVE-BUYSAVE   SAVE MGE DATA                              
         AR    RE,RA                                                            
         LHI   RF,SVMGSAVX-SVMGSAVE                                             
         LA    R0,MGSAVE                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BRAS  RE,BLDDBUY          BUILD DUMMY BUY                              
*                                                                               
         LA    R2,TVMGFLDH                                                      
         MVC   TVMGFLDH,=X'280003CD60200020'                                    
         MVC   TVMGFLD,MOUPGTXT                                                 
         LA    RF,TVMGFLD                                                       
         ST    RF,FADDR                                                         
         MVI   FSTOPS,C'='                                                      
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL              RESCAN FOR UPEDT COMPATABILITY               
*                                                                               
         MVI   EDTVAL,UPEDT                                                     
         GOTO1 CALLEDT                                                          
         L     RE,ADBLOCK                                                       
         USING SPDEMUPD,RE                                                      
         LAY   RF,BUUPGD                                                        
         MVC   SVUPIN,0(RF)        SAVE EBCDIC INPUT                            
         MVC   SVUPOPTS,SPUPTYPE   SAVE UPGRADE DEFAULT VALUES                  
         MVC   SVOPTBK,SPUPFBK                                                  
         MVC   SVUPFIL,SPUPFIL                                                  
         DROP  RE                                                               
*                                                                               
** CHECK IF USER ENTERED OVERRIDES                                              
*                                                                               
         NI    WRKRUPSW,X'FF'-WRKRUPSW_NODMER                                   
         LA    RE,SVDEMOS          RE = A(POL DEMOS IN DISPLAYED ORDER)         
         OC    SVBRDEMS,SVBRDEMS   HAVE BRAND DEMO LIST?                        
         BZ    *+8                  NO                                          
         LA    RE,SVBRDEMS         RE = A(BRD DEMOS IN DISPLAYED ORDER)         
*                                                                               
         LA    RF,MODEM1           RF = A(MODEM LIST)                           
         LA    R0,MOPROG-MODEM1                                                 
         SRL   R0,2                R0 = (LOOP FOR NUM OF MODEMS)                
TVMU045  OC    0(3,RE),0(RE)       ANY MORE DEMOS?                              
         BZ    TVMU055              NO                                          
         CLI   2(RE),0             NSI DEMO?                                    
         BE    TVMU050              NO, SKIP IT                                 
         TM    0(RF),X'80'         HAVE OVERRIDE?                               
         BZ    TVMU060              NO, DONE                                    
TVMU050  LA    RF,L'MODEM1(RF)     NEXT DEMO IN MODEMS                          
         LA    RE,3(RE)            NEXT DEMO IN EST DEMO LIST                   
         BCT   R0,TVMU045                                                       
TVMU055  OI    WRKRUPSW,WRKRUPSW_NODMER                                         
*                                                                               
TVMU060  MVI   EDTVAL,UPEDT        CALL UPEDT TO FORMAT DBLOCK NOW              
         MVI   ADBLOCK,C'X'        SET FLAG FOR DUMMY EDIT                      
         GOTO1 CALLEDT                                                          
         GOTO1 DEMUPGD                                                          
*                                                                               
         NI    WRKRUPSW,X'FF'-WRKRUPSW_NODMER                                   
*                                                                               
         L     RE,ADBLOCK                                                       
         USING SPDEMUPD,RE                                                      
         XC    SVUPIN,SVUPIN       CLEAR UPGRADE                                
         XC    SVUPOPTS,SVUPOPTS                                                
         XC    SVOPTBK,SVOPTBK                                                  
         XC    SVUPFIL,SVUPFIL                                                  
TVMUX    J     EXIT                                                             
         DROP  RE,R6,R9                                                         
         LTORG                                                                  
TVMGWRKD DSECT                                                                  
TVMGFLDH DS    CL8                                                              
TVMGFLD  DS    CL(L'MKOUPGD)                                                    
TVMGWRKL EQU   *-TVMGWRKD                                                       
T21131   CSECT                                                                  
         EJECT                                                                  
*===========================================                                    
* BUILD A DUMMY BUY RECORD AND LOOK UP DEMOS                                    
*===========================================                                    
BLDDBUY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,BUYREC                                                        
         XC    0(256,R6),0(R6)     CLEAR PART OF RECORD AREA                    
         MVC   BUYKEY(13),SVKEY                                                 
*&&DO                                                                           
         CLC   BUYMSTA+2(3),=X'1AC022'     IF DABC                              
         BE    BDB010                                                           
         CLC   BUYMSTA+2(3),=X'20C1E2'     IF DROT                              
         BE    BDB010                                                           
         CLC   BUYMSTA+2(3),=X'1E6E82'     IF DKUI                              
         BE    BDB010                                                           
         CLC   BUYMSTA+2(3),=X'214102'     IF DTAK                              
         BE    BDB010                                                           
         B     BDB020                                                           
*                                                                               
BDB010   MVC   BUYMSTA+2(3),=X'C414A2'     USE WABC INSTEAD                     
         MVC   QSTA(5),=C'WABCT'                                                
*&&                                                                             
BDB020   LHI   R0,BDELEMX-BUYREC   LENGTH OF BUYKEY AND BDELEM                  
         STCM  R0,3,BUYRLEN        SET LENGTH IN RECORD                         
         MVC   BUYALPHA,AGYALPHA                                                
*                                                                               
         MVI   BDELEM,X'01'                                                     
         MVI   BDELEM+1,BDELEMX-BDELEM                                          
*                                                                               
         MVC   BDTIMST,MOSTIM                                                   
         MVC   BDTIMEND,MOETIM                                                  
         MVC   BDDAY,MODAYS                                                     
         GOTO1 VDATCON,DMCB,(8,MOSTRT),(3,BDSTART)                              
         MVC   BDEND,BDSTART                                                    
         MVC   BDWKS,MOWKS                                                      
*                                                                               
         XC    SVSPLMKT,SVSPLMKT   MAKE SURE THIS IS CLEAR                      
         MVI   BUDEMSW,0           HAVEN'T BUILT DEMO ELEM YET                  
         GOTO1 VBLDDEM             BUILD DEMO ELEM AND INSERT                   
         J     EXIT                                                             
*=============================================================                  
* EDIT PURPOSE CODE IF REQUIRED                                                 
*=============================================================                  
         SPACE 1                                                                
VALPURP  NTR1  BASE=*,LABEL=*                                                   
         CLIY  SVB0PROF+9,C'O'     OPTIONAL PURPOSE CODE                        
         JE    *+14                                                             
         CLIY  SVB0PROF+9,C'Y'     TEST PURPOSE CODE OPTION                     
         JNE   NO                                                               
*                                                                               
         MVC   NERRCD,=Y(NOPURCOD)                                              
         LA    R2,MKOPURH                                                       
         CLI   5(R2),0             TEST INPUT                                   
         JNE   VALP1                                                            
         CLI   9(RF),C'O'          OPTIONAL? (RF STILL BEING USED HERE)         
         JE    NO                  THEN JUST EXIT                               
         J     ERREXITN            NO, REQUIRED SO GIVE AN ERROR                
*                                                                               
VALP1    TM    4(R2),X'20'         TEST PREV VALIDATED                          
         BO    VALPURPX                                                         
*                                                                               
         OC    MGPURP,MGPURP       ANY MISSED PURP CODES                        
         BZ    VALP2               NO - EDIT INPUT ONE                          
         MVC   NERRCD,=Y(MKGDPURP) MSSD/MKGD PURPOSE CODES DIFFER               
         MVC   DUB,MKOPUR                                                       
         OC    DUB,SPACES                                                       
         CLC   MGPURP,DUB                                                       
         JNE   ERREXITN                                                         
*                                                                               
K        USING PRPRECD,KEY         READ PURPOSE CODE RECORD                     
*                                                                               
VALP2    XC    KEY,KEY                                                          
         MVI   K.PRPKTYP,PRPKTYPQ                                               
         MVI   K.PRPKSUB,PRPKSUBQ                                               
         MVC   K.PRPKAGY,AGYALPHA                                               
         MVC   K.PRPKMED,BUYMD                                                  
         MVC   K.PRPCODE,MKOPUR                                                 
         OC    K.PRPCODE,SPACES                                                 
         MVC   NERRCD,=Y(PURCODNF)                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   ERREXITN                                                         
*                                                                               
VALPURPX OI    4(R2),X'20'         SET VALIDATED                                
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*========================================================*                      
* EDIT REASON CODE IF REQUIRED                                                  
* SYNTAX IS RC=12345(,THIS IS MY REASON)                                        
*========================================================*                      
         SPACE 1                                                                
VALRSN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLIY  SVB0PROF+7,C'1'     TEST REASON CODE SCHEME 1                    
         JNE   NO                  EXIT WITH CC NEQ                             
*                                                                               
         LA    R2,MKORSCH          REASON CODE FIELD                            
         MVC   NERRCD,=Y(NORSN)                                                 
         CLI   5(R2),0             TEST NO INPUT                                
         JE    ERREXITN                                                         
         TM    4(R2),X'20'         TEST REASON CODE PREV VALID                  
         BZ    VALRSN2                                                          
         TM    MKORSTH+4,X'20'     TEST REASON TEXT PREV VALID                  
         JO    YES                 EXIT WITH CC EQ                              
*                                                                               
VALRSN2  MVC   NERRCD,=Y(NORSNCOD)                                              
         LA    R2,MKORSCH                                                       
         CLI   5(R2),0                                                          
         JE    ERREXITN                                                         
*                                                                               
         MVC   NERRCD,=Y(BADRSN)                                                
         CLI   5(R2),6               MAX 6 CHAR REASON CODE                     
         JH    ERREXITN                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D18'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(6),8(R2)                                                   
         OC    KEY+4(6),SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   ERREXITN                                                         
*                                                                               
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AREC                                                          
         USING RSNRECD,R6                                                       
*                                                                               
         CLI   RSNELINP,C'Y'       TEST TEXT REQUIRED                           
         BE    VALRSN10                                                         
         CLI   MKORSTH+5,0         TEST FOR TEXT                                
         BE    VALRSNX             NO - EXIT WITH CC EQ                         
         LA    R2,MKORSTH          POINT TO TEXT FLDHDR                         
         J     INVLFLD             ELSE INVALID INPUT FIELD                     
*                                                                               
VALRSN10 MVC   NERRCD,=Y(NORSNTXT)                                              
         LA    R2,MKORSTH          POINT TO TEXT FLDHDR                         
         CLI   5(R2),0                                                          
         JE    ERREXITN                                                         
*                                                                               
VALRSNX  OI    MKORSCH+4,X'20'                                                  
         OI    MKORSTH+4,X'20'                                                  
         J     YES                 EXIT WITH CC EQ                              
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* SEND A MAKEGOOD APPROVAL TO THE REP                                           
*=====================================================================*         
         SPACE 1                                                                
SENDMGAP NTR1  BASE=*,LABEL=*                                                   
         XC    MYSTTN,MYSTTN       READ HEADEND REC FOR CABLE                   
         BRAS  RE,READMKN                                                       
*                                                                               
         L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         CLI   0(R6),MNSTELQ       MAKEGOOD GROUP STATUS ELEMENT?               
         BE    SDMA05              THERE ARE ALREADY STATUS ELEMS               
*                                                                               
         MVI   BYTE,MNSTNEW                                                     
         MVI   BYTE2,0                                                          
         BRAS  RE,BLDSTAT                                                       
*                                                                               
SDMA05   DS    0H                                                               
         MVI   BYTE,MNSTAPP                                                     
         MVI   BYTE2,0                                                          
         TM    MGMSCFLG,MGAUTOMG   AUTOMG APPROVE/REJECT?                       
         BZ    *+12                                                             
         BRAS  RE,BLDINF                                                        
         OI    BYTE2,MNSTFAMG      YES                                          
         BRAS  RE,BLDSTAT                                                       
*                                                                               
**       TM    MGMSCFLG,MGAUTOMG   AUTOMG APPROVE/REJECT?                       
**       BZ    *+8                                                              
**       BRAS  RE,BLDINF                                                        
*                                                                               
         BRAS  RE,MYPUTREC                                                      
         DROP  R6                                                               
*                                                                               
         BRAS  RE,BLDCOLOR         SET ORDER STATUS                             
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'80',0),F#UTLD                                       
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
         TM    F@TTEST,X'80'       USER SIGNED ON WITH UPDATE=NO?               
         BZ    SDMA06                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOUPDTNG)  NO UPDATING ERROR                           
         J     ERREXITN                                                         
         DROP  R1                                                               
*                                                                               
SDMA06   BRAS  RE,SETPNDG          SET PENDING BITS IN MISSED SPOTS             
         BRAS  RE,SENDIT           SEND APPROVAL                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* SEND A MAKEGOOD REJECTION TO THE REP                                          
*=====================================================================*         
         SPACE 1                                                                
SENDMGRJ NTR1  BASE=*,LABEL=*,WORK=(R9,SMGRWRKL)                                
         USING SMGRWRKD,R9                                                      
*                                                                               
         LA    R0,SMGRWRKD         CLEAR SMGRWRKD                               
         LHI   R1,SMGRWRKL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    MYSTTN,MYSTTN       FOR CABLE READ HEADEND REC                   
         BRAS  RE,READMKN                                                       
         MVC   NERRCD,=Y(NOREJCOM)                                              
         CLI   MGREJCOM,C' '                                                    
         JNH   ERREXITN                                                         
*                                                                               
         L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         CLI   0(R6),MNSTELQ       MAKEGOOD GROUP STATUS ELEMENT?               
         BE    SDMR10              THERE ARE ALREADY STATUS ELEMS               
*                                                                               
         MVI   BYTE,MNSTNEW        INSERT NEW STATUS IF NONE                    
         MVI   BYTE2,0                                                          
         BRAS  RE,BLDSTAT                                                       
*                                                                               
SDMR10   DS    0H                                                               
         MVI   BYTE,MNSTREJ        INSERT REJECT STATUS                         
         MVI   BYTE2,0                                                          
         TM    MGMSCFLG,MGAUTOMG   AUTOMG APPROVE/REJECT?                       
         BZ    *+12                                                             
         BRAS  RE,BLDINF                                                        
         OI    BYTE2,MNSTFAMG      YES                                          
         BRAS  RE,BLDSTAT                                                       
*                                                                               
**       TM    MGMSCFLG,MGAUTOMG   AUTOMG APPROVE/REJECT?                       
**       BZ    *+8                                                              
**       BRAS  RE,BLDINF                                                        
*                                                                               
         SR    R5,R5                                                            
         MVI   ELCODE,MNSTELQ      FIND NEXT REJ. ELEM TO SAVE TO MRC           
SDMR20   BRAS  RE,NEXTEL                                                        
         BNE   SDMR30                                                           
         LR    R5,R6                                                            
         USING MNSTELD,R5                                                       
         CLI   MNSTSTAT,MNSTREJ                                                 
         BNE   SDMR20                                                           
*                                                                               
         USING MCMRJCD,R4                                                       
         LA    R4,SVMCMRJ                                                       
         MVI   SVMGRELC,MCMRJCQ    SET FIRST ELEM CODE TO X'45'                 
*                                                                               
SDMR30   L     R6,AREC             DELETE OLD REJCOM(S)                         
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
         MVI   ELCODE,MNMRJCQ      X'40' - REJCOM ELEM                          
         USING MNMRJCD,R6                                                       
SDMR35   BRAS  RE,FIRSTEL          GET (NEXT) OLD REJCOM?                       
         BNE   SDMR40               NO                                          
*                                                                               
         LA    RF,SVMCMRJX                                                      
         CR    R4,RF               MORE THAN 5 LINES OF MG REJ CMNTS?           
         JNL   *+2                  - SHOULD NOT BE POSSIBLE                    
*                                                                               
         LTR   R5,R5               FOUND REJECT STATUS ELEM?                    
         JZ    *+2                  NO, BUT HAVE REJECT COMMENTS??              
         MVC   MCMRJC,SVMGRELC                                                  
         MVC   MCMRJDAT,MNSTDATE   SET DATE                                     
         MVC   MCMRJTIM,MNSTTIME   SET TIME                                     
         LLC   RE,MNMRLEN                                                       
         LAY   RE,-MNMROVRH-1(RE)                                               
         MVC   MCMRJTXT(0),MNMRTEXT SAVE THE OLD REJCOM                         
         EX    RE,*-6                                                           
         LA    RE,MCMRJOVR+1(RE)                                                
         CHI   RE,L'MGREJCOM       ELEM GREATER THAN 70?                        
         BNH   *+8                  NO                                          
         LA    RE,L'MGREJCOM        YES, TRUNCATE THE REJCOM                    
         AHI   RE,MCMRJOVR                                                      
         STC   RE,MCMRJLEN         SET ELEM LENGTH                              
*                                                                               
         BRAS  RE,DELEL            NOW DELETE REJCOM                            
*                                                                               
         LA    R4,L'SVMCMRJ(R4)    NEXT SAVE MG REJ COMMENT                     
         MVI   SVMGRELC,MCMRJCQ2     X'46'                                      
         B     SDMR35                                                           
*                                                                               
         DROP  R6,R5,R4                                                         
*                                                                               
SDMR40   SR    R5,R5               CLEAR R5                                     
         TM    SVXFRCTL,SVXFR_SDT  TEST SPOT DESKTOP MODE                       
         JZ    SDMR45                                                           
         CLC   MGREJCOM(4),=X'FFFFFFFF' MULTI MG REJ COMMENTS?                  
         BNE   SDMR45              YES, DON'T CLEAR                             
         LA    R5,MGREJCOM+4       R5 = A(1ST MULTI-REJ COMMENT)                
*                                                                               
SDMR45   XC    ELEM,ELEM                                                        
         MVI   ELEM,MNMRJCQ                                                     
         MVI   ELEM+1,2            SET OVERHEAD                                 
*                                                                               
         LTR   R5,R5               PROCESSING MULTI-REJ COMMENT?                
         BZ    SDMR50               NO                                          
         ICM   RF,15,0(R5)         DOES THIS LINE HAVE A COMMENT?               
         BZ    SDMR60               NO, SKIP IT                                 
*                                  RF = LQ_D                                    
         LLH   R1,LQ_LN-LQ_D(RF)   CALC ELEM LENGTH                             
         AHI   R1,-(LQ_VALUE-LQ_D)+MNMROVRH                                     
         STC   R1,ELEM+1           SET ELEM LENGTH                              
         AHI   R1,-MNMROVRH-1      SETUP FOR EX                                 
         MVC   ELEM+2(0),LQ_VALUE-LQ_D(RF)                                      
         EX    R1,*-6              MOVE MG REJ COMM TO ELEM                     
         B     SDMR55                                                           
*                                                                               
SDMR50   MVC   ELEM+2(L'MGREJCOM),MGREJCOM  MOVE MAX LEN                        
         LA    R1,ELEM+71          POINT TO LAST CHAR                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8              BACK UP TO LAST NON-BLANK                    
         LA    R0,ELEM-1           CALC LEN                                     
         SR    R1,R0                                                            
         STC   R1,ELEM+1           SET ELEMENT LENGTH                           
*                                                                               
SDMR55   BRAS  RE,ADDEL                                                         
*                                                                               
SDMR60   LTR   R5,R5               PROCESSING MULTI-REJ COMMENT?                
         BZ    SDMR65               NO                                          
         LLC   R1,1(R6)                                                         
         LA    R6,0(R1,R6)         BUMP INSERTION ADDRESS IN AREC               
         LA    R5,4(R5)            BUMP TO NEXT MG REJ COMMENT                  
         LA    RF,MGREJCOM+(5*4)   RF = A(5TH MG REJ COMMENT)                   
         CR    R5,RF               PROC'D ALL 5 MG REJ COMMENTS?                
         BNH   SDMR45               NO                                          
*                                                                               
SDMR65   BRAS  RE,MYPUTREC                                                      
*                                                                               
         BRAS  RE,BLDCOLOR         SET ORDER STATUS                             
                                                                                
*===========================================================                    
* SEND THE REJECTION                                                            
*===========================================================                    
                                                                                
         BRAS  RE,SENDIT                                                        
*                                                                               
         OC    SVMCMRJ,SVMCMRJ     HAVE REJCOM TO SAVE?                         
         BZ    SMGRJX               NO                                          
         BRAS  RE,UPDTMRC           YES, SAVE REJCOM TO MRC RECORD              
*                                                                               
SMGRJX   J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* READ MAKEGOOD REJECTION COMMENT RECORD INTO AIO2                              
* UPDATE RECORD WITH OLD REJECTION COMMENT                                      
*=====================================================================*         
         SPACE 1                                                                
UPDTMRC  NTR1  LABEL=*                                                          
*                                                                               
         LA    R7,SVMCMRJ                                                       
*                                                                               
         XC    WORK,WORK                                                        
K        USING DAREMGCD,WORK       BUILD DARE MG REJ COMMENT KEY                
         MVI   K.MCXKTYPE,MCXKTYPQ   X'0D'                                      
         MVI   K.MCXKSBTY,MCXKSBTQ   X'3B'                                      
         MVC   K.MCXKAGMD,SVAGYMD    A/M                                        
         MVC   K.MCXKORDR,MGORDER    ORDER                                      
         MVC   K.MCXKGRP,MGGRPCOD    GROUP CODE                                 
*                                                                               
         MVC   WORK2,WORK                                                       
         GOTO1 VDATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',WORK2,WORK           
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         JNZ   *+2                                                              
*                                                                               
         CLC   WORK(MCXKSEQ-MCXKEY),WORK2  MATCH AM/ORD#/GRP/DTYP               
         BNE   UPMRC50                      NO, GO ADD NEW REC                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFIL',WORK+36,AREC2,DMWORK         
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         JNZ   *+2                                                              
*                                                                               
         L     R6,AREC2                                                         
         USING DAREMGCD,R6         BUILD DARE MG REJ COMMENT REC                
         LA    R6,MCXFRST                                                       
*                                                                               
UPMRC10  GOTO1 SVRECUP,DMCB,(X'FE',AREC2),(R7),(C'R',(R6)),XSPREC2              
         CLI   8(R1),C'R'          TEST OVERFLOW                                
         JNE   UPMRC40             RECUP RETURNS R ON OK/0 FOR ERROR            
*                                                                               
         LLC   RE,1(R7)                                                         
         LA    R6,0(RE,R6)                                                      
*                                                                               
         LA    R7,L'SVMCMRJ(R7)                                                 
         LA    RF,SVMCMRJX                                                      
         CR    R7,RF               END OF MGREJ COMMENTS?                       
         JNL   UPMRC35                                                          
         OC    0(L'SVMCMRJ,R7),0(R7)   HAVE MGREJ COMMENT?                      
         JNZ   UPMRC10                   YES                                    
*                                                                               
UPMRC35  GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',WORK+36,AREC2,DMWORK         
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         JZ    UPMRCX                                                           
         DC    H'0'                                                             
*                                                                               
UPMRC40  LLH   RE,K.MCXKSEQ        REC OVERFLOW                                 
         BCTR  RE,0                GET NEXT SEQ RECORD (FF COMP)                
         STCM  RE,3,K.MCXKSEQ      SET NEXT SEQ (FF COMP)                       
         B     UPMRC55                                                          
*                                                                               
UPMRC50  MVC   WORK(L'MCXKEY),WORK2  SETUP NEW RECORD                           
         MVC   K.MCXKSEQ,=X'FFFF'  SET FIRST SEQ (FF COMP)                      
*                                                                               
UPMRC55  L     R6,AREC2                                                         
         XC    0(256,R6),0(R6)     CLEAR PART OF RECORD AREA                    
         MVC   0(L'MCXKEY,R6),K.MCXKEY                                          
         MVI   MCXRLEN+1,MCXFRST-MCXKEY                                         
         LA    R6,MCXFRST                                                       
*                                                                               
UPMRC57  GOTO1 SVRECUP,DMCB,(X'FE',AREC2),(R7),(C'R',(R6)),XSPREC2              
         CLI   8(R1),C'R'          TEST OVERFLOW                                
         JNE   *+2                  DIE, SOMETHING VERY WRONG                   
*                                                                               
         LLC   RE,1(R7)                                                         
         LA    R6,0(RE,R6)                                                      
*                                                                               
         LA    R7,L'SVMCMRJ(R7)                                                 
         LA    RF,SVMCMRJX                                                      
         CR    R7,RF               END OF MGREJ COMMENTS?                       
         JNL   UPMRC60                                                          
         OC    0(L'SVMCMRJ,R7),0(R7)   HAVE MGREJ COMMENT?                      
         JNZ   UPMRC57                   YES                                    
*                                                                               
UPMRC60  GOTO1 VDATAMGR,DMCB,=C'ADDREC',=C'XSPFIL',WORK+36,AREC2,DMWORK         
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         JNZ   *+2                                                              
UPMRCX   J     EXIT                                                             
         DROP  R6,R9,K                                                          
         LTORG                                                                  
XSPREC2  DC    AL2(42,32,5970)                                                  
         EJECT                                                                  
*                                                                               
SMGRWRKD DSECT                                                                  
*                                                                               
SVMCMRJ  DS    5XL(L'MGREJCOM+MCMRJOVR) MISS SPOT REJ COMMENTS (1-5)            
SVMCMRJX DS    0X                                                               
*                                                                               
SVMGRELC DS    X                   SAVE MG ELCODE                               
*SMGRADD  DS    C                                                               
SMGRWRKL EQU   *-SMGRWRKD                                                       
T21131   CSECT                                                                  
         EJECT                                                                  
*=================================================================              
* SEND APPROVAL/REJECTION                                                       
*=================================================================              
         SPACE 1                                                                
SENDIT   NTR1  BASE=*,LABEL=*,WORK=(R9,SIWORKL)                                 
         USING SIWORKD,R9                                                       
         ZAP   LINCOUNT,=P'0'                                                   
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSPOOL       GET A(SPOOL)                                 
         GOTO1 VCALLOV,DMCB                                                     
         MVC   SPOOL,0(R1)                                                      
*                                                                               
         MVC   BSPOOLID,=C'DAR'     SETUP PRINT QUEUE INFO                      
         XC    BSPOOLKEY,BSPOOLKEY                                              
*                                                                               
         LA    R1,BSPOOLKEY                                                     
         USING PQPLD,R1                                                         
*                                                                               
         MVC   PLDESC,=CL11'MKGD APPRVL'                                        
         CLI   MGFLAG,C'A'         TEST APPROVAL                                
         BE    SNDT10                                                           
         CLI   MGFLAG,C'S'         TEST SELF APPLY                              
         BE    SNDT10                                                           
         MVC   PLDESC,=CL11'MKGD RJCTED'                                        
*                                                                               
SNDT10   MVI   PLCLASS,C'G'          DARE CLASS G                               
         OI    BSPOOLIND,BSPUINIT    ALLOWS ME TO SET THE CLASS                 
         DROP  R1                                                               
*                                                                               
         LA    R2,BSPOOLKEY                                                     
         USING PQPLD,R2                                                         
*                                                                               
         MVI   BUSERLANG,0                                                      
         MVC   PLSUBID,BSPOOLID                                                 
         MVC   PLUSER,10(R3)       TWAORIG                                      
         MVC   BSPOOLDM,VDATAMGR                                                
         MVC   BRCDATCON,VDATCON                                                
         MVC   BRCCOMFAC,VCOMFACS                                               
         MVC   BSPOOLBUF,VTIA                                                   
         MVI   BSPMODE,0                                                        
         XC    BVPRINT,BVPRINT       NOT OFF-LINE                               
         XC    BABOX,BABOX           NO BOXES                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   BSPOOLRPN,PLREPNO                                                
         DROP  R2                                                               
*                                                                               
         MVC   BP+4(5),=C'*HDR*'      HEADER RECORD                             
         MVC   BP+9(14),=CL14'EDICT=*DDSDARA'                                   
         MVI   BP+34,C'W'             WIDE REPORT                               
         MVI   BP+35,C'P'            /PAGE FOR ESYLNK AND SUPRESS TOP           
         MVC   BP+54+8(1),BUYMD       1ST 8 BYTES IS AGENCY BY DAVID            
         MVC   BP+54+8+1(L'QCLT),QCLT                                           
         BRAS  RE,PRINTIT                                                       
*                                                                               
         LA    R2,BP                                                            
         USING EDICTD,R2                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'DA'      DARE                                         
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
*                                                                               
         MVC   EDIPROG,=C'MAP'     MAKEGOOD APPROVAL                            
         CLI   MGFLAG,C'A'         TEST APPROVAL                                
         BE    SNDT20                                                           
         CLI   MGFLAG,C'S'         TEST SELF APPLY                              
         BE    SNDT20                                                           
         MVC   EDIPROG,=C'MRJ'     MAKEGOOD REJECTION                           
*                                                                               
SNDT20   MVC   HALF,MGLSTTRN+DOSTIDNM-DOSTELD                                   
         BRAS  RE,GETID                                                         
         MVC   EDIRDRAG,WORK                                                    
*                                                                               
         MVC   EDIRDRST(5),QSTA                                                 
         MVC   EDIRDRMD,BUYMD                                                   
         MVC   EDIRDRCL(L'QCLT),QCLT                                            
         MVC   EDIRDRP1(L'MGQPRD1),MGQPRD1                                      
         MVC   EDIRDRP2(L'MGQPRD2),MGQPRD2                                      
         LLC   R0,SVKEY+9          HEADLINE ESTIMATE NUM                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EDIRDRES,DUB                                                     
*                                                                               
         MVC   EDIRDRAN,MGORDERQ                                                
         MVC   EDIRDRCN,MGREPCON                                                
         MVC   EDIRDRBY,BUYBU                                                   
         MVC   EDIRDRSP,MGREPSAL                                                
         BRAS  RE,PRINTIT                                                       
* START COUNTING LINES NOW !                                                    
         ZAP   LINCOUNT,=P'0'                                                   
*                                                                               
X        USING MOFRAPPD,BP                                                      
*                                                                               
         MVC   X.MOAPTID,=C'MKGAPP'  MAKEGOOD OFFER APPROVAL (MKGAPP)           
         CLI   MGFLAG,C'A'           TEST APPROVAL                              
         BE    SNDT30                                                           
         CLI   MGFLAG,C'S'           TEST SELF APPLY                            
         BE    SNDT30                                                           
         MVC   X.MOAPTID,=C'MKGREJ'  MAKEGOOD OFFER REJECTION (MKGREJ)          
*                                                                               
SNDT30   MVC   X.MOAPORDR,MGORDERQ                                              
         MVC   X.MOAPFRID,MGAGYID                                               
         MVC   X.MOAPTOID,MGDESTID                                              
         MVC   X.MOAPROUT,MGROUTE                                               
*                                                                               
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 09/23/02                 
         LA    R1,MOREPTAB                                                      
MEDOC10  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOC30             YES                                          
         ZIC   R4,1(R1)            LENGTH OF REP ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP?                                
         BE    MEDOC15             YES, CHECK OFFICE                            
         CLC   2(0,R1),X.MOAPTOID                                               
         LLC   R4,0(R1)            NO, BUMP TO NEXT REP                         
         AR    R1,R4                                                            
         B     MEDOC10                                                          
*                                                                               
***  POINT TO OFFICE FOR COMPARE                                                
MEDOC15  LLC   R4,1(R1)                                                         
         LA    R6,X.MOAPTOID                                                    
         LA    R6,0(R4,R6)         R6=A(OFFICE CODE IN EDICT)                   
         LA    R1,2(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
MEDOC20  CLI   0(R1),0             END OF REP/OFFICE LIST?                      
         BE    MEDOC120            DO NOTHING                                   
         CLC   0(2,R1),0(R6)       MATCH ON OFFICE?                             
         BE    MEDOC25             YES                                          
         LA    R1,5(R1)            NO, BUMP TO NEXT OFFICE IN TABLE             
         B     MEDOC30                                                          
*                                                                               
MEDOC25  CLC   ORDRDATE,2(R1)      ON/AFTER THE CUTOFF DATE?                    
         BNL   MEDOC30             YES                                          
         MVC   2(3,R6),=C'-DS'     MOVE IT IN                                   
         B     MEDOC120                                                         
***********************************                                             
* NBC CUTOVER TO WIDEORBIT                                                      
* REP-DATE  OR  LOCAL STATION-DATE                                              
***********************************                                             
MEDOC30  LA    R1,WORPDTAB                                                      
MEDOC33  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOC40             YES                                          
         LLC   R4,3(R1)            L(REP/LOCAL STATION) ENTRY                   
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP/LOCAL STATION?                  
         BE    MEDOC36             YES, CHECK DATE                              
         CLC   4(0,R1),X.MOAPTOID                                               
         LLC   R4,3(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,4(R1,R4)                                                      
         B     MEDOC33                                                          
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
MEDOC36  LLC   R4,3(R1)                                                         
         LA    R6,X.MOAPTOID                                                    
         CLC   =C'NBC',X.MOAPTOID  IS IT A REP?                                 
         BE    MEDOC37                                                          
         CLC   =C'MON',X.MOAPTOID                                               
         BNE   *+12                                                             
MEDOC37  LA    R6,2(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         B     MEDOC39                                                          
*                                                                               
         BCTR  R4,0                TO ACCOUNT FOR SPACE IN ENTRY                
         LA    R6,0(R4,R6)         R6=A(AFTER LOCAL STATION IN EDICT)           
*                                                                               
MEDOC39  CLC   ORDRDATE,0(R1)      ON/AFTER THE CUTOFF DATE?                    
         BL    MEDOC120            NO, NO CHANGE TO THE RECEIVING ID            
         MVC   0(3,R6),=C'-WO'     GOES TO MEDIAOCEAN                           
         B     MEDOC120                                                         
***********************************                                             
* REP-OFFICE-DATE                                                               
*                                                                               
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY DATE                 
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT DATE, ORDER STAYS ON REPPAK                 
***********************************                                             
MEDOC40  LA    R1,MORODTAB                                                      
MEDOC43  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOC50             YES                                          
         LLC   R4,3(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    MEDOC46             YES, CHECK DATE                              
         CLC   4(0,R1),X.MOAPTOID                                               
         LLC   R4,3(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,4(R1,R4)                                                      
         B     MEDOC43                                                          
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
MEDOC46  LLC   R4,3(R1)                                                         
         LA    R6,X.MOAPTOID                                                    
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
*                                                                               
         CLC   ORDRDATE,0(R1)      BEFORE THE CUTOFF DATE?                      
         BNL   MEDOC120            NO, GOTO TO REPPAK                           
         MVC   0(3,R6),=C'-MO'     OTHERWISE, STAY ON MEDIAOCEAN                
         B     MEDOC120                                                         
***********************************                                             
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY STATION/DATE         
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT STATION/DATE, ORDER STAYS ON REPPAK         
***********************************                                             
MEDOC50  LA    R1,MOOFSTTB                                                      
MEDOC55  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOC80             YES                                          
         ZIC   R4,1(R1)            L(REP-OFFICE ENTRY)                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    MEDOC60             YES, CHECK STATION AND DATE                  
         CLC   2(0,R1),X.MOAPTOID                                               
         XR    R4,R4               NO, BUMP TO NEXT REP                         
         ICM   R4,3,0(R1)          NO, BUMP TO NEXT REP                         
         AR    R1,R4                                                            
         B     MEDOC55                                                          
*                                                                               
***  POINT TO OFFICE FOR COMPARE                                                
MEDOC60  LLC   R4,1(R1)                                                         
         LA    R6,X.MOAPTOID                                                    
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         LA    R1,2(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
MEDOC65  CLI   0(R1),0             END OF STATION/DATE LIST?                    
         BE    MEDOC80             YES, THEN STAYS ON REPPAK                    
         CLC   QSTA,0(R1)          MATCH ON STATION?                            
         BE    MEDOC70             NO, CHECK NEXT ENTRY                         
         LA    R1,14(R1)           NO, BUMP TO NEXT ENTRY IN TABLE              
         B     MEDOC65                                                          
*                                                                               
MEDOC70  OC    5(3,R1),5(R1)       WAS THE STATION ALREADY CONVERTED?           
         BZ    MEDOC75             YES, AUTOMATICALLY GO TO MEDIAOCEAN          
         CLC   ORDRDATE,5(R1)      ON/AFTER THE CUTOFF DATE?                    
         BL    MEDOC80             NO, NO CHANGE TO THE RECEIVING ID            
*                                                                               
         OC    8(3,R1),8(R1)       ANY END DATE?                                
         BZ    MEDOC75             NONE, GOES TO MEDIAOCEAN                     
         CLC   ORDRDATE,8(R1)      ON/AFTER THE END DATE (REPPAK)?              
         BL    MEDOC75             NO, STAYS ON MEDIAOCEAN                      
*                                                                               
         CLC   ORDRDATE,11(R1)     ON/AFTER NEW START (-MO AGAIN)?              
         BL    MEDOC80             NO, NO CHANGE TO THE RECEIVING ID            
*                                                                               
MEDOC75  MVC   0(3,R6),=C'-MO'     GOES TO MEDIAOCEAN                           
*                                                                               
***********************************                                             
* WIDEORBIT  REP-OFFICE-STATION-DATE                                            
*                                                                               
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO WIDEORBIT BY STATION/DATE          
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT STATION/DATE, ORDER STAYS ON REPPAK         
***********************************                                             
MEDOC80  LA    R1,MOOFSTTB                                                      
         AHI   R1,WOOFSTTB-MOOFSTTB  TO SOLVE ADDRESSABILITY PROBLEM            
MEDOC85  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOC110            YES                                          
         ZIC   R4,1(R1)            L(REP-OFFICE ENTRY)                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    MEDOC90             YES, CHECK STATION AND DATE                  
         CLC   2(0,R1),X.MOAPTOID                                               
         XR    R4,R4               NO, BUMP TO NEXT REP                         
         ICM   R4,3,0(R1)          NO, BUMP TO NEXT REP                         
         AR    R1,R4                                                            
         B     MEDOC85                                                          
*                                                                               
***  POINT TO OFFICE FOR COMPARE                                                
MEDOC90  LLC   R4,1(R1)                                                         
         LA    R6,X.MOAPTOID                                                    
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         LA    R1,2(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
MEDOC95  CLI   0(R1),0             END OF STATION/DATE LIST?                    
         BE    MEDOC110            YES, THEN STAYS ON REPPAK                    
         CLC   QSTA,0(R1)          MATCH ON STATION?                            
         BE    MEDOC100            NO, CHECK NEXT ENTRY                         
         LA    R1,14(R1)           NO, BUMP TO NEXT ENTRY IN TABLE              
         B     MEDOC95                                                          
*                                                                               
MEDOC100 OC    5(3,R1),5(R1)       WAS THE STATION ALREADY CONVERTED?           
         BZ    MEDOC105            YES, AUTOMATICALLY GO TO WIDEORBIT           
         CLC   ORDRDATE,5(R1)      ON/AFTER THE CUTOFF DATE?                    
         BL    MEDOC110            NO, NO CHANGE TO THE RECEIVING ID            
*                                                                               
         OC    8(3,R1),8(R1)       ANY END DATE?                                
         BZ    MEDOC105            NONE, GOES TO WIDEORBIT                      
         CLC   ORDRDATE,8(R1)      ON/AFTER THE END DATE (REPPAK)?              
         BL    MEDOC105            NO, STAYS ON WIDEORBIT                       
*                                                                               
         CLC   ORDRDATE,11(R1)     ON/AFTER NEW START (-WO AGAIN)?              
         BL    MEDOC110            NO, NO CHANGE TO THE RECEIVING ID            
MEDOC105 MVC   0(3,R6),=C'-WO'     GOES TO WIDEORBIT                            
*                                                                               
MEDOC110 DS    0H                                                               
*                                                                               
MEDOC120 MVC   X.MOAPQSTA(5),QSTA                                               
         MVC   X.MOAPRPCN,MGREPCON                                              
*                                                                               
         MVC   X.MOAPOFRI,MGGRPCOD   MAKEGOOD GROUP CODE                        
         MVC   X.MOAPSEQN,MGVRSNQ    VERSION NUMBER                             
*&&DO                                                                           
*  NOT NEEDED ANYMORE - THIS WAS A PROBLEM 3 YEARS AGO                          
         CLC   =C'PET',MGDESTID                                                 
         BNE   FTSX                                                             
         CLC   =C'KTTV',QSTA                                                    
         BE    FTS2                                                             
         CLC   =C'WFLD',QSTA                                                    
         BE    FTS2                                                             
         CLC   =C'WTXF',QSTA                                                    
         BE    FTS2                                                             
         CLC   =C'WFXT',QSTA                                                    
         BE    FTS2                                                             
         CLC   =C'WNYW',QSTA                                                    
         BNE   FTSX                                                             
*                                                                               
FTS2     MVC   X.MOAPTOID(3),=C'FTS'                                            
*&&                                                                             
FTSX     L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0         SETUP 'RETURN TO SENDER' DATA                
         L     R6,DMCB                                                          
         USING FACTSD,R6                                                        
*                                                                               
         MVC   X.MOAPDATE(2),FADATE+6     YY                                    
         MVC   X.MOAPDATE+2(2),FADATE     MM                                    
         MVC   X.MOAPDATE+4(2),FADATE+3   DD                                    
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,DUB                                                        
         STCM  R0,15,DUB+4                                                      
         AP    DUB(4),DUB+4(4)     DDS TIME IS OFFSET BY 6AM                    
                                                                                
         CP    DUB(4),=P'240000'      PAST MIDNIGHT?                            
         JL    SNDT40                                                           
         GOTO1 VADDAY,DMCB,X.MOAPDATE,(X'20',X.MOAPDATE),F'1'                   
*                                                                               
SNDT40   L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
*                                                                               
         GOTO1 (RF),(R1),MGTIME,X.MOAPTIME,L'MGTIME                             
*                                                                               
         LA    R4,X.MOAPRTNS                                                    
         USING RTN2SNDR,R4                                                      
*                                                                               
         MVC   RTNBUYER,BUYBU      NEED THIS FOR DARE NOTIFICATIONS             
*                                                                               
         GOTO1 (RF),(R1),FASYSID,RTNSYSID,1                                     
         DROP  R6                                                               
*                                                                               
         MVC   RTNPWRCD,14(R3)     AGENCY POWER CODE                            
*                                                                               
         GOTO1 (RF),(R1),SVAGYMD,RTNAGYMD,1                                     
         DROP  R4                                                               
*                                                                               
         BRAS  RE,PRINTIT                                                       
*                                                                               
         CLI   MGFLAG,C'J'         TEST REJECT                                  
         BNE   SENDITX                                                          
*                                                                               
         CLC   MGREJCOM(4),=X'FFFFFFFF' MULTI MG REJ COMMENTS?                  
         BNE   SNDT60              YES, DON'T CLEAR                             
         LA    R5,MGREJCOM+4       R5 = A(1ST MULTI-REJ COMMENT)                
         ICM   R4,15,0(R5)         MUST HAVE ATLEAST ONE COMMENT!               
         JZ    *+2                                                              
*                                                                               
X        USING MOFRCOMD,BP                                                      
SNDT50   MVC   X.MORCTID,=C'MKGRCM'  MAKEGOOD REJECTION COMMENT                 
         MVC   X.MORCORDR,MGORDERQ                                              
         MVC   X.MORCOFRI,MGGRPCOD                                              
         MVC   X.MORCSEQN,MGVRSNQ                                               
*                                                                               
*                                  R4 = LQ_D                                    
         LLH   R1,LQ_LN-LQ_D(R4)   CALC ELEM LENGTH                             
         AHI   R1,-(LQ_VALUE-LQ_D)+MNMROVRH                                     
         STC   R1,ELEM+1           SET ELEM LENGTH                              
         AHI   R1,-MNMROVRH-1      SETUP FOR EX                                 
         MVC   X.MORCTEXT(0),LQ_VALUE-LQ_D(R4)                                  
         EX    R1,*-6              MOVE MG REJ COMM TO PRINTLINE                
         OC    X.MORCTEXT,SPACES                                                
*                                                                               
SNDT55   LA    R5,4(R5)            BUMP TO NEXT MG REJ COMMENT                  
         LA    RF,MGREJCOM+(5*4)   RF = A(5TH MG REJ COMMENT)                   
         CR    R5,RF               PROC'D ALL 5 MG REJ COMMENTS?                
         BH    SNDT65               YES, PRINT LAST LINE                        
         ICM   R4,15,0(R5)         DOES THIS LINE HAVE A COMMENT?               
         BZ    SNDT65               NO, PRINT LAST LINE                         
         MVI   X.MORCCONT,C'*'     MORE TO FOLLOW                               
         BRAS  RE,PRINTIT                                                       
         B     SNDT50                                                           
*                                                                               
SNDT60   MVC   X.MORCTID,=C'MKGRCM'  MAKEGOOD REJECTION COMMENT                 
         MVC   X.MORCORDR,MGORDERQ                                              
         MVC   X.MORCOFRI,MGGRPCOD                                              
         MVC   X.MORCSEQN,MGVRSNQ                                               
         MVC   X.MORCTEXT,MGREJCOM                                              
SNDT65   BRAS  RE,PRINTIT                                                       
*                                                                               
X        USING MOFRTLRD,BP                                                      
SNDT70   MVC   X.MOTLTID,=C'MKGTLR'                                             
         MVC   X.MOTLORDR,MGORDERQ                                              
         AP    LINCOUNT,=P'1'      ADD 1 FOR TRAILER                            
         OI    LINCOUNT+1,X'0F'                                                 
         UNPK  X.MOTLRCCT,LINCOUNT SET LINE COUNT IN TRAILER                    
*                                                                               
         CLI   BUYST,C'0'                                                       
         JL    SNDT80                                                           
         MVC   X.MOTLOFID,MGGRPCOD                                              
         MVC   X.MOTLSEQN,MGVRSNQ                                               
*                                                                               
SNDT80   BRAS  RE,PRINTIT                                                       
         DROP  X                                                                
*                                                                               
SENDITX  MVC   BP(26),=C'*** END OF DDS MESSAGE ***'                            
         BRAS  RE,PRINTIT                                                       
*                                                                               
         MVI   BSPMODE,X'FF'        CLOSE THE PRINTQ                            
         BRAS  RE,PRINTIT                                                       
         J     EXIT                                                             
*                                                                               
PRINTIT  LR    R0,RE                                                            
         MVI   BLINE,1                                                          
         AP    LINCOUNT,=P'1'      COUNT LINES PRINTED                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
ADDEL    LR    R0,RE                                                            
         GOTOR MYRECUP,DMCB,(C'S',AREC),ELEM,(C'R',(R6))                        
         CLI   DMCB+8,C'R'                                                      
         JNE   *+2                 DIE, RECORD OVERFLOW                         
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTOR MYRECUP,DMCB,(C'S',AREC),(R6)                                    
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
       ++INCLUDE DDMOREPTAB                                                     
*                                                                               
         LTORG                                                                  
         DROP  R9                                                               
         EJECT                                                                  
                                                                                
*                                                                               
SIWORKD  DSECT                   ** SENDIT LOCAL WORKING STORAGE **             
LINCOUNT DS    PL2                                                              
SIWORKL  EQU   *-SIWORKD                                                        
T21131   CSECT                                                                  
         EJECT                                                                  
*=====================================================================*         
* BUILD A NEW STATUS ELEMENT IN ELEM AND INSERT AT 0(R6)                        
* UNLESS R6 = 0                                                                 
* BYTE CONTAINS STATUS VALUE                                                    
*=====================================================================*         
         SPACE 1                                                                
BLDSTAT  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
E        USING MNSTELD,ELEM                                                     
         MVI   E.MNSTEL,MNSTELQ                                                 
         MVI   E.MNSTLEN,MNSTLENQ                                               
         MVC   E.MNSTSTAT,BYTE                                                  
         MVC   SVMNSTAT,BYTE       UPDATE CURRENT STATUS                        
*                                                                               
         OC    E.MNSTFLAG,BYTE2    TEST & SET X'80' - AUTOMG FLAG               
         JZ    *+8                  NO FLAG                                     
         MVI   E.MNSTLEN,MNSTFLNQ   YES, SET NEW MNSTFLNQ L'ELEM                
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(19,E.MNSTDATE)                               
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,DUB                                                        
         STCM  R0,15,DUB+4                                                      
         AP    DUB(4),DUB+4(4)     DDS TIME IS OFFSET BY 6AM                    
*                                                                               
         CP    DUB(4),=P'240000'      PAST MIDNIGHT?                            
         JL    BLDSTAT2                                                         
         SP    DUB(4),=P'240000'      YUP, BUMP TO NEXT DAY AND ADJUST          
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,ELEM+100) <==ELEM IS TEMP ANYWAYS          
         GOTO1 VADDAY,DMCB,ELEM+100,ELEM+100,F'1'                               
         GOTO1 VDATCON,DMCB,(0,ELEM+100),(19,E.MNSTDATE)                        
*                                                                               
BLDSTAT2 ICM   R1,15,DUB                                                        
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,E.MNSTTIME     CURRENT TIME PWOS                            
         STH   R1,MGTIME                                                        
*                                                                               
         LTR   R6,R6               TEST TO ADD TO CURRENT RECORD                
         BZ    *+8                                                              
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  E                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* UPDATE EXISTING MNIFEL OR BUILD NEW ONE AND SET AUTOMG FLAG                   
*=====================================================================*         
         SPACE 1                                                                
BLDINF   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AREC                                                          
****     AH    R6,DSPFRST                                                       
         AH    R6,DATADISP                                                      
BLDINF10 CLI   0(R6),0             END OF REC?                                  
         JE    BLDINF30                                                         
         CLI   0(R6),MNIFELQ       MG INFO ELEMENT?                             
         BH    BLDINF30                                                         
         JE    BLDINF20            THERE ARE ALREADY STATUS ELEMS               
         LLC   RF,1(R6)                                                         
         LA    R6,0(RF,R6)                                                      
         J     BLDINF10                                                         
*                                                                               
         USING MNIFELD,R6                                                       
BLDINF20 OI    MNIFFLG1,MNIFF1AA   SET AUTOMG FLAG TO EXISTING MNIFEL           
         J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
BLDINF30 XC    ELEM,ELEM                                                        
E        USING MNIFELD,ELEM                                                     
         MVI   E.MNIFEL,MNIFELQ                                                 
         MVI   E.MNIFLEN,MNIFLNEQ                                               
         OI    E.MNIFFLG1,MNIFF1AA SET AUTOMG FLAG TO NEW MNIFEL                
*                                                                               
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         DROP  E                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* SET NEW STATUS IN ORDER RECORD                                                
* SVMNSTAT HAS CURRENT                                                          
* UPDATE THE ORDER STATUS ELEMENT IF NEEDED                                     
* UPDATE PASSIVE COLOR DIRECTORY POINTER IF NEEDED                              
* ELEM CONTAINS STATUS ELEMENT FROM MAKEGOOD NOTICE RECORD                      
*=====================================================================*         
         SPACE 1                                                                
BLDCOLOR NTR1  BASE=*,LABEL=*,WORK=(R9,BCWORKL)                                 
         USING BCWORKD,R9                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1ORDER                                          
         BRAS  RE,READORD          GET THE ORDER RECORD                         
*                                                                               
         OC    SVPASSWD,SVPASSWD   TEST USER HAS A PID                          
         BZ    BLDCLR3             NO                                           
* SAVE PID OF APPROVER                                                          
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,DOWHOELQ     X'52'                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCLR2                                                          
*                                                                               
         USING DOWHOEL,R6                                                       
         MVC   DOWHOPID,SVPASSWD                                                
         B     BLDCLR3                                                          
         DROP  R6                                                               
*                                                                               
BLDCLR2  XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING DOWHOEL,RE                                                       
         MVI   DOWHOEL,DOWHOELQ                                                 
         MVI   DOWHOLEN,DOWHOLNQ                                                
         MVC   DOWHOPID,SVPASSWD                                                
         DROP  RE                                                               
         GOTO1 RECUP,DMCB,AREC,ELEM,(R6)                                        
*                                                                               
BLDCLR3  NI    MISCFLG1,X'FF'-MF1CNFCM                                          
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,DOSPELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCLR4                                                          
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM          CONFIRM WITH COMMENT?                 
         BZ    *+8                                                              
         OI    MISCFLG1,MF1CNFCM                                                
         DROP  R6                                                               
*                                                                               
BLDCLR4  MVC   HALF(1),SVMNSTAT                                                 
         BRAS  RE,SETSTADR         POINT R1 TO STATUS TABLE                     
         MVC   BCTABLEN,=AL2(L'STATTAB)                                         
         CLI   HALF,0                                                           
         BE    BLDCLRX                                                          
         B     BLDCLR6                                                          
*                                                                               
BLDCLR5  BRAS  RE,ORDSTADR         POINT R1 TO STATUS TABLE                     
         MVC   BCTABLEN,=AL2(L'STATTAB2)                                        
         OI    MISCFLG1,MF1ORDER                                                
*                                                                               
         L     R6,AREC1            GET MOST CURRENT ORDSTAT                     
         LA    R6,24(R6)                                                        
         MVI   ELCODE,DOSTELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCLRX                                                          
         USING DOSTELD,R6                                                       
         MVC   HALF(1),DOSTSTAT     MOVE STATUS                                 
         DROP  R6                                                               
*                                                                               
BLDCLR6  CLC   HALF(1),0(R1)                                                    
         BNE   BLDCLR9                                                          
*                                                                               
         TM    MISCFLG1,MF1ORDER                                                
         BZ    BLDCLR15                                                         
*                                                                               
         USING ORDD,R1                                                          
         CLI   ORDTYP,0            IS TYPE BIT SET?                             
         BE    BLDCLR15                                                         
*                                                                               
         CLI   SVOMPROF+2,C'A'     PROFILE SET TO AUTO-CONFIRM?                 
         BE    BLDCLR7                                                          
         CLI   SVOMPROF+2,C'B'     PROFILE SET TO BUYER-CONFIRM?                
         BNE   BLDCLR15            ALL CONFIRMS ARE BLACK!!!                    
*                                                                               
BLDCLR7  TM    ORDTYP,ORDTNCMT            TEST WITH NO COMMENTS                 
         BNO   *+12                                                             
         TM    MISCFLG1,MF1CNFCM          ARE THERE COMMENTS ?                  
         BZ    BLDCLR15                   NO, GOT IT                            
         TM    ORDTYP,ORDTCMTS            TEST WITH COMMENTS                    
         BNO   BLDCLR9                                                          
         TM    MISCFLG1,MF1CNFCM          ARE THERE COMMENTS ?                  
         BO    BLDCLR15                   YES, GOT IT                           
         DROP  R1                                                               
*                                                                               
BLDCLR9  AH    R1,BCTABLEN                                                      
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCLR6                                                          
         DC    H'0'                                                             
*                                                                               
BLDCLR15 MVC   BCCOLOR,1(R1)       SAVE CORRESPONDING COLOR                     
         TM    MISCFLG1,MF1ORDER                                                
         BNZ   BLDCLR25                                                         
         MVC   BCCOLOR,9(R1)       SAVE CORRESPONDING COLOR                     
*                                                                               
         L     R6,AREC1            UPDATE OUR MKGD GROUP COLOR ELEM             
         LA    R6,24(R6)                                                        
         MVI   ELCODE,MGCOLELQ                                                  
BLDCLR20 BRAS  RE,NEXTEL                                                        
         BNE   BLDCLR25                                                         
         USING MGCOLEL,R6                                                       
         CLC   MGGRPCOD,MGCOLCOD   THIS IS OURS?                                
         BNE   BLDCLR20                                                         
         MVC   MGCOLCOL,BCCOLOR    SET COLOR FOR MKGD IN ORDER REC              
*                                                                               
         CLI   BCCOLOR,C'K'        NEED TO DELETE MAKEGOOD ELEM?                
         BNE   BLDCLR25                                                         
         GOTO1 RECUP,DMCB,AREC,(R6)  DELETE THE ELEMENT                         
*                                                                               
BLDCLR25 CLI   BCCOLOR,C'G'        TEST GREEN                                   
         BE    BLDCLR50            YES - GREEN ALWAYS WINS                      
*                                                                               
         L     R6,AREC1            FIND MG GROUP COLOR ELEMENTS                 
         USING DOKEY,R6                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,MGCOLELQ                                                  
*                                                                               
BLDCLR30 BRAS  RE,NEXTEL                                                        
         BNE   BLDCLR45                                                         
         USING MGCOLEL,R6                                                       
         CLI   MGCOLCOL,C'G'                                                    
         BNE   BLDCLR40                                                         
         MVI   BCCOLOR,C'G'        SET TO GREEN AND DONE                        
         B     BLDCLR50                                                         
*                                                                               
BLDCLR40 CLI   MGCOLCOL,C'R'                                                    
         BNE   BLDCLR30                                                         
         MVI   BCCOLOR,C'R'                                                     
         B     BLDCLR30                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR45 TM    MISCFLG1,MF1ORDER                                                
         BNZ   BLDCLR50                                                         
         CLI   BCCOLOR,C'K'        NO MORE MGS?                                 
         BNE   BLDCLR49                                                         
         CLI   SVOMPROF+2,C'A'     PROFILE SET TO AUTO-CONFIRM?                 
         BNE   BLDCLR5                                                          
         L     R6,AREC1            ADD FULLY CONFIRMED STATUS!!                 
         LA    R6,24(R6)                                                        
         MVI   ELCODE,DOSTELQ                                                   
         BRAS  RE,NEXTEL                                                        
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,QCFMD      AM I CONFIRMED?                              
         BNE   BLDCLR5                                                          
*                                                                               
         CLI   DOSTLEN,DOSTLNQ3    DO I HAVE A TYPE FIELD?                      
         BE    BLDCLR46                                                         
         TM    MISCFLG1,MF1CNFCM                                                
         BZ    BLDCLR5                                                          
         B     BLDCLR47                                                         
BLDCLR46 TM    DOSTTYPE,DCNFMCOM   CONFIRMED WITH COMMENTS?                     
         BZ    BLDCLR48                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR47 LA    R4,ELEM             NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4          MNSTOKAY STATUS ELEM IN ELEM                 
         MVI   DOSTEL,DOSTELQ      DON'T DESTROY DATE                           
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVI   DOSTSTAT,QCFMD                                                   
         XC    DOSTTYPE,DOSTTYPE                                                
         OI    DOSTTYPE,DCNFMFUL                                                
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AREC),ELEM,(R6)                                
BLDCLR48 NI    MISCFLG1,X'FF'-MF1CNFCM                                          
         B     BLDCLR5                                                          
         DROP  R4                                                               
*                                                                               
BLDCLR49 CLI   BCCOLOR,C'G'                                                     
         BNE   BLDCLR5                                                          
*                                                                               
BLDCLR50 L     R6,AREC1            FIND COLOR ELEMENT IN ORDER REC              
         USING DOKEY,R6                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,COLELQ                                                    
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCLR70                                                         
*                                                                               
K        USING DSCKTYPE,KEY                                                     
         USING COLOREL,R6                                                       
*                                                                               
         XC    KEY,KEY             READ AND DELETE OLD COLOR POINTER            
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,SVAGYMD                                               
         MVC   K.DSCKBYR,BUYBU                                                  
         OC    K.DSCKBYR,SPACES                                                 
         MVC   K.DSCKSTAT,COLCOL                                                
         MVC   K.DSCKDATE,COLDATE                                               
         MVC   K.DSCKORDR,MGORDER                                               
         DROP  K                                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR70                  CONVERSION NOT YET RUN !               
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
*                                                                               
Y        USING COLOREL,ELEM+128                                                 
BLDCLR70 XC    ELEM+128(128),ELEM+128    BUILD NEW COLOR ELEMENT                
         MVI   Y.COLEL,COLELQ                                                   
         MVI   Y.COLELLEN,COLLENQ                                               
         MVC   Y.COLCOL,BCCOLOR                                                 
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,Y.COLDATE)                                 
         XC    Y.COLDATE,=X'FFFF'       COMPLEMENT DATE                         
*                                                                               
         L     R6,AREC1                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,COLELQ                                                    
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCLR80                                                         
         GOTO1 RECUP,DMCB,AREC,(R6)    DELETE OLD COLOR ELEM                    
*                                                                               
BLDCLR80 GOTO1 RECUP,DMCB,AREC,ELEM+128,(R6)                                    
*                                                                               
         GOTO1 PUTREC              WRITE THE ORDER RECORD                       
* CREATE NEW COLOR POINTER                                                      
K        USING DSCKTYPE,KEY                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,SVAGYMD                                               
         MVC   K.DSCKBYR,BUYBU                                                  
         OC    K.DSCKBYR,SPACES                                                 
         MVC   K.DSCKSTAT,Y.COLCOL                                              
         MVC   K.DSCKDATE,Y.COLDATE                                             
         MVC   K.DSCKORDR,MGORDER                                               
         MVC   KEY+14(4),MGORDDA   MOVE SAVED DISK ADDRESS                      
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR90                                                         
         NI    KEY+13,X'7F'        UNDELETE                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         NI    DMINBTS,X'FF'-X'08'                                              
         B     BLDCLRX                                                          
*                                                                               
BLDCLR90 MVC   KEY,KEYSAVE         RESTORE                                      
         GOTO1 ADD                                                              
*                                                                               
BLDCLRX  J     EXIT                                                             
         DROP  Y,K                                                              
         EJECT                                                                  
BCWORKD  DSECT                   ** BLDCOLOR LOCAL WORKING STORAGE **           
BCCOLOR  DS    C                                                                
BCTABLEN DS    H                                                                
BCWORKL  EQU   *-BCWORKD                                                        
T21131   CSECT                                                                  
         EJECT                                                                  
*=====================================================================*         
* LOOKS UP THE AGENCY'S SIGNON ID AND ALSO GETS THE ROUTING CODE                
*                                                                               
* ON EXIT:     (CC)                NE - NO ROUTING CODE                         
*=====================================================================*         
         SPACE 1                                                                
GETROUTE NTR1  BASE=*,LABEL=*                                                   
         XC    MGROUTE,MGROUTE     CLEAR DARE ROUTING CODE                      
*                                                                               
         MVC   HALF,T211FFD+10     SET INPUT ID                                 
         BRAS  RE,GETID            READ ID RECORD TO AREC1                      
         MVC   MGAGYID,WORK        SAVE ALPHA AGYID                             
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOROUTE)                                               
*                                                                               
         L     R6,AREC3                                                         
         USING CTIKEY,R6                                                        
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'33'        DARE PARTNER INFO                            
         BRAS  RE,NEXTEL                                                        
         BNE   GETROUTN                                                         
*                                                                               
         USING CTUSAD,R6                                                        
GETROUT2 MVC   MGROUTE,CTUSADRC    SAVE DARE ROUTING CODE                       
         CLC   MGROUTE,SPACES                                                   
         JH    YES                                                              
*                                                                               
GETROUTN LHI   RE,SVDAROPT-BUYSAVE                                              
         AR    RE,RA                                                            
         TM    0(RE),SVDAROPT_NOERRS   TEST SUPPRESS ERRORS                     
         JZ    ERREXITN                                                         
         J     YES                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ ID RECORD FOR ID IN HALF                                                 
* OUTPUT ALPHA ID RETURNED IN WORK                                              
* RECORD RETURNED IN AREC3                                                      
*=====================================================================*         
         SPACE 1                                                                
GETID    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
K        USING CTIKEY,WORK                                                      
         MVI   K.CTIKTYP,CTIKTYPQ                                               
         MVC   K.CTIKNUM,HALF                                                   
         DROP  K                                                                
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADIDREC)                                              
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',WORK,AREC3               
*                                                                               
         L     R6,AREC3                                                         
         USING CTIREC,R6                                                        
*                                                                               
         CLC   WORK(25),0(R6)                                                   
         BE    GETID2                                                           
         LHI   RE,SVDAROPT-BUYSAVE                                              
         AR    RE,RA                                                            
         TM    0(RE),SVDAROPT_NOERRS   TEST SUPPRESS ERRORS                     
         JZ    ERREXITN                                                         
*                                                                               
GETID2   LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        FIND DESC ELEM                               
         BRAS  RE,NEXTEL                                                        
         JNE   ERREXITN                                                         
*                                                                               
         USING CTDSCD,R6                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(10),CTDSC                                                   
         DROP  R6                                                               
         J     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*==================================================                             
*  RESTORE MGE SCREEN                                                           
*==================================================                             
         SPACE 1                                                                
RSTRF3   NTR1  BASE=*,LABEL=*                                                   
         MVC   ELEM(1),MGEINP1H+5  SAVE INPUT LINE LENGTH                       
         MVC   ELEM+1(78),MGEINP1  SAVE DATA ON INPUT LINE                      
*                                                                               
         GOTO1 VCALLOV,DMCB,MGEHL1H,X'D90211F3',0                               
         MVI   SVSCR,X'F3'                                                      
         MVC   MGEHL2+35(6),MGDEMNM1  RESTORE DEMO NAME                         
*                                                                               
         CLI   DSPFMT,C'A'         TEST DEMO/CPP DISPLAY                        
         BL    RSTRF3D             NO                                           
*                                                                               
         BRAS  RE,DEMHL1                                                        
         MVC   MGEHL1(38),0(R1)                                                 
         OI    MGEHL1H+6,X'80'                                                  
         MVC   MGEHL2(41),SPACES                                                
         OI    MGEHL2H+6,X'80'                                                  
*                                                                               
         LA    R0,6                                                             
         LA    R1,MGEHL2                                                        
         LA    RE,MGDEMNM1                                                      
*                                                                               
RSTRF3A  MVC   0(6,R1),0(RE)                                                    
         LA    R1,7(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   R0,RSTRF3A                                                       
*                                                                               
RSTRF3D  LA    R2,MGEINP1H                                                      
         MVC   5(1,R2),ELEM        RESTORE INPUT LENGTH                         
         MVC   8(78,R2),ELEM+1     RESTORE DATA                                 
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         CLI   SVDARPRF+13,C'N'    SELF-APPLY ON?                               
         BNE   RSTRF3H                                                          
         XC    MGEPF3,MGEPF3                                                    
         MVC   MGEPF3(7),=X'C6F17ED9A39995'  F1=RTRN IN LOWER CASE              
         OI    MGEPF3+6,X'80'      TRANSMIT                                     
*                                                                               
RSTRF3H  LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   RSTRF3H                                                          
         MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
*                                                                               
*===========================================================                    
* DELETE ALL C'B' RECORDS FROM TSAR BUFFER THAT WERE CREATED                    
* BY SPBUY38 BUT WHICH ARE NOT EXPECTED HERE                                    
*===========================================================                    
                                                                                
         CLI   SVMGINIT,C'E'       TEST WE INITIALIZED TSAR                     
         BNE   RSTRF3X                                                          
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
RSTRF3L  BRAS  RE,CALLTSAR                                                      
         BNE   RSTRF3X                                                          
         CLI   MSTYPE,C'B'                                                      
         BNE   RSTRF3O                                                          
* DELETE TSAR BUY RECORD                                                        
         MVI   T.TSACTN,TSADEL                                                  
         BRAS  RE,CALLTSAR                                                      
         MVI   T.TSACTN,TSAGET                                                  
         B     RSTRF3L                                                          
*                                                                               
RSTRF3O  MVI   T.TSACTN,TSANXT                                                  
         B     RSTRF3L                                                          
*                                                                               
RSTRF3X  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* DEMO OVERRIDES ARE TO BE A PERCENTAGE OF THE MISSED DEMOS                     
* INPUT IN UPGRADE FIELD IS '=PCT' TO 4 DECIMALS                                
* NEED TO ADD UP MISSED DEMOS AND PUT VALUES IN DEMO INPUT FIELDS               
*==============================================================                 
         SPACE 1                                                                
GTMSSDEM NTR1  BASE=*,LABEL=*                                                   
         LLC   R0,5(R2)                                                         
         BCTR  R0,0                ADJUST FOR = IDIOT!                          
         GOTO1 VCASHVAL,DMCB,(4,9(R2)),(R0)                                     
         MVC   NERRCD,=Y(BADDMPCT)                                              
         ICM   R0,15,4(R1)         TEST ZERO RESULT                             
         JZ    ERREXITN                                                         
         C     R0,=F'1000'         NO LESS THAN 1 PERCENT                       
         JL    ERREXITN                                                         
         C     R0,=F'20000'        NO MORE THAN 200 PERCENT                     
         JH    ERREXITN                                                         
         ST    R0,FULL             SAVE RESULT IN FULL                          
*                                                                               
         XC    BUDEMS,BUDEMS       CLEAR ACCUMS                                 
         LLC   R4,MOWKS                                                         
         LLC   R5,MONPW                                                         
         MR    R4,R4               GIVES NUMBER OF SPOTS IN R5                  
         STCM  R5,15,BUDEMS+56     SAVE MISSED SPOT COUNT                       
*                                                                               
         MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
*                                                                               
GMD2     BRAS  RE,CALLTSAR                                                      
         BNE   GMD20                                                            
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
         CLI   MSTYPE,MSTYPEQ                                                   
         BNE   GMD2                                                             
*                                                                               
         MVC   NERRCD,=Y(BADMSSD)                                               
         OC    MSACTBUY,MSACTBUY   BUYLINE FOUND                                
         JE    ERREXITN            NO - CAN'T DO THIS YET                       
* READ THE MISSED BUYLINE                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+6(3),MSSTTN                                                  
         MVC   KEY+11(2),MSACTBUY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,SVDEMOS          POINT TO APPROPRIATE DEMO LIST               
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         LA    R5,BUDEMS           POINT TO ACCUMS                              
*                                                                               
GMD10    BAS   RE,GMDPULL          FIND DEMO AND ADD TO ACCUM                   
         LA    R5,4(R5)            NEXT ACCUM                                   
         LA    R4,3(R4)            NEXT DEMO                                    
         OC    0(3,R4),0(R4)       ANY MORE DEMOS?                              
         BNZ   GMD10                YES                                         
         B     GMD2                                                             
*                                                                               
GMD20    MVC   NERRCD,=Y(NOMSSDEM)                                              
         OC    BUDEMS(56),BUDEMS                                                
         JZ    ERREXITN                                                         
* APPLY PERCENTAGE TO THE DEMOS AND DIVIDE BY NUMBER OF SPOTS                   
         LLC   R4,MOWKS                                                         
         LLC   R5,MONPW                                                         
         MR    R4,R4               GIVES NUMBER OF SPOTS IN R5                  
*                                                                               
         LA    R1,BUDEMS                                                        
         LA    R0,14                                                            
*                                                                               
GMD22    ICM   RF,15,0(R1)                                                      
         N     RF,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         AR    RF,RF               X 2                                          
         M     RE,FULL             X INPUT PCTG                                 
         D     RE,=F'10000'                                                     
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
         SR    RE,RE                                                            
         AR    RF,RF               X 2                                          
         ICM   R5,15,BUDEMS+56     RETRIEVE TOTAL SPOTS                         
         DR    RE,R5               AND DIVIDE                                   
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         TM    0(R1),X'40'         2-DECIMAL PLACE PRECISION?                   
         BZ    *+8                                                              
         O     RF,=X'40000000'     YES, TURN IT BACK ON                         
         STCM  RF,15,0(R1)                                                      
         LA    R1,4(R1)                                                         
         BCT   R0,GMD22                                                         
* NOW MOVE VALUES TO DEMO INPUT FIELDS                                          
         LA    R2,MKODM1H                                                       
         LA    R1,BUDEMS                                                        
*                                                                               
         LA    R4,SVDEMOS          POINT TO APPROPRIATE DEMO LIST               
         OC    SVBRDEMS,SVBRDEMS                                                
         BZ    *+8                                                              
         LA    R4,SVBRDEMS                                                      
*                                                                               
GDM30    ICM   R0,15,0(R1)                                                      
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         TM    0(R1),X'40'         TEST 2-DEC                                   
         BO    GDM32                                                            
         EDIT  (R0),(6,8(R2)),1,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GDM34                                                            
*                                                                               
GDM32    EDIT  (R0),(6,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
GDM34    STC   R0,5(R2)           SET INPUT FIELD LENGTH                        
         NI    4(R2),X'DF'        UNSET VALIDATED                               
         OI    6(R2),X'80'        XMT                                           
*                                                                               
         LA    R1,4(R1)                                                         
         AHI   R2,MKODM2H-MKODM1H  NEXT DEMO VALUE                              
         LA    R4,3(R4)                                                         
         CLI   1(R4),0             TEST ANY MORE DEMOS                          
         BNZ   GDM30                                                            
*                                                                               
         XC    MKOUPGD,MKOUPGD     CLEAR UPGRADE INPUT                          
         OI    MKOUPGDH+6,X'80'    XMT                                          
         J     EXIT                                                             
*                                                                               
GMDPULL  NTR1                                                                   
         L     R6,AREC1            POINT TO BUYREC                              
         LA    R6,24(R6)                                                        
         LLC   R0,1(R6)                                                         
         AR    R6,R0               POINT TO DEMO ELEMENT                        
         CLI   0(R6),2                                                          
         JNE   *+2                                                              
         LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   GMDX                                                             
         SRL   R0,3                SET FOR BCT                                  
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
GMDP2    CLC   0(3,R4),0(R6)       MATCH DEMO                                   
         BE    GMDP4                                                            
         LA    R6,8(R6)            NEXT DEMO                                    
         BCT   R0,GMDP2                                                         
         B     GMDX                                                             
*                                                                               
GMDP4    ICM   R0,15,4(R6)         GET DEMO VALUE                               
         N     R0,=X'3FFFFFFF'     DROP OVERRIDE & 2DEC FLAGS                   
         NI    0(R5),X'40'                                                      
         A     R0,0(R5)            ADD TO BUDEMS                                
         ST    R0,0(R5)                                                         
         TM    4(R6),X'40'         IS IT 2-DECIMAL?                             
         BZ    *+8                                                              
         OI    0(R5),X'40'                                                      
*                                                                               
GMDX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* CHECK FOR PAID SPOTS IN MONTHS OF MAKEGOOD SPOTS                              
*=====================================================================*         
         SPACE 1                                                                
TESTMGPD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVDARPRF+10,C'N'    TEST ALLOW MKGD IN PAID MONTH                
         JNE   EXIT                YES - IGNORE                                 
* BUILD TABLE OF BROADCAST MONTH DATES FOR THIS ESTIMATE                        
         XC    WORK,WORK           CLEAR ADCON AREA                             
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A1D' GETBROAD                                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   WORK+0(4),0(R1)                                                  
*                                                                               
         MVC   WORK+4(4),VADDAY                                                 
         MVC   WORK+8(4),VGETDAY                                                
         MVC   WORK+12(4),VDATCON                                               
         GOTO1 VMOBILE,DMCB,(12,SVSTART),PRDLIST,WORK,0                         
* NOW MOVE TO 5 BYTE MONTH TABLE                                                
         LHI   R0,12                                                            
         LA    R1,PRDLIST                                                       
         L     RE,ABLOCK                                                        
*                                                                               
TESTMG00 MVC   0(4,RE),0(R1)                                                    
         LA    R1,4(R1)                                                         
         LA    RE,5(RE)                                                         
         BCT   R0,TESTMG00                                                      
                                                                                
*===================================================*                           
* FOR BRAODCAST, ONLY READ BUYLINES FOR THE STATION                             
* FOR CABLE, READ FOR ALL NETWORKS FOR THE SYSCODE                              
*  AND                                                                          
* FOR BPOL, READ BUYLINES BY BRAND                                              
* FOR TPOL, READ BUYLINES BY POL                                                
*===================================================*                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         CLI   BUYST,C'0'          TEST US CABLE                                
         BL    TESTMG01                                                         
         NI    KEY+8,X'80'         DROP NETWORK                                 
         OI    KEY+8,X'01'         SKIP SYCODE ONLY BUYS                        
*                                                                               
TESTMG01 CLI   SVPOLPRD,0          IF BRAND POL BY BRAND                        
         BE    *+14                                                             
         MVC   KEY+3(1),SVPOLPRD   USE THE BRAND, STUPID                        
         B     TESTMG05                                                         
*                                                                               
         CLI   SVCPROF,C'2'        CLT PROF SAYS THIS CLT IS BPOL?              
         BNE   TESTMG05                                                         
         MVC   KEY+3(1),MGBPRD1    YES, WE HAVE THE BRAND SET                   
*                                                                               
TESTMG05 GOTO1 HIGH                                                             
         B     TESTMG12                                                         
*                                                                               
TESTMG10 GOTO1 SEQ                                                              
*                                                                               
TESTMG12 CLI   BUYST,C'0'          TEST US CABLE                                
         BL    TESTMG15                                                         
*==================*                                                            
* CABLE STATION                                                                 
*==================*                                                            
         CLC   KEY(6),KEYSAVE      MATCH M/C/P/M                                
         BNE   TESTMG20             NO, DONE READING BUYS                       
         MVC   WORK(3),KEY+6       GET THE CABLE STATION FROM KEY AND           
         MVC   WORK+3(3),SVSTA     SVKEY AND                                    
         NI    WORK+2,X'80'         DROP THE NETWORKS                           
         NI    WORK+5,X'80'         TO SEE IF WE                                
         CLC   WORK(3),WORK+3      MATCH ON SYSCODE?                            
         BNE   TESTMG20             NO, DONE READING BUYS                       
         CLC   KEY+9(1),SVEST      MATCH ON ESTIMATE?                           
         BE    TESTMG17             YES, GO GET THE BUY RECORD                  
         MVI   KEY+9,X'FF'         IF EST HI, SET EST AND REST OF KEY           
         MVC   KEY+10(3),KEY+9     -TO X'FF' TO SKIP THIS NETWORK               
         BH    TESTMG05            -AND GO READHI                               
         MVC   KEY+9(1),SVEST      IF EST LOW, SET EST=SVEST AND                
         XC    KEY+10(3),KEY+10    -CLEAR REST OF KEY                           
         B     TESTMG05            -AND GO READHI                               
*==================*                                                            
* BROADCAST STATION                                                             
*==================*                                                            
TESTMG15 CLC   KEY(10),KEYSAVE     MATCH M/C/P/M/S/E                            
         BNE   TESTMG20                                                         
*                                                                               
TESTMG17 GOTO1 GETREC                                                           
         BRAS  RE,SETPAID                                                       
         B     TESTMG10                                                         
         EJECT                                                                  
*=======================================================*                       
* NOW READ THROUGH TSAR OFFERS FOR SPOTS IN PAID MONTHS                         
*=======================================================*                       
                                                                                
TESTMG20 MVI   T.TSACTN,TSAGET                                                  
         MVC   T.TSRNUM,=Y(1)                                                   
         BRAS  RE,CALLTSAR                                                      
         JNE   *+2                                                              
         MVI   T.TSACTN,TSANXT                                                  
*                                                                               
TESTMG22 BRAS  RE,CALLTSAR                                                      
         JNE   EXIT                                                             
*                                                                               
         CLI   MSTYPE,MOTYPEQ                                                   
         BNE   TESTMG22                                                         
         CLI   MOCOMNUM,0                                                       
         BNE   TESTMG22                                                         
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,MOWKS          SAVE NUMBER OF WEEKS                         
         JZ    *+2                                                              
*                                                                               
         GOTO1 VDATCON,DMCB,(8,MOSTRT),WORK                                     
*                                                                               
TESTMG30 GOTO1 VDATCON,DMCB,WORK,(2,HALF)                                       
         BAS   RE,TESTPAID         SEE IF THIS MONTH PAID                       
*                                                                               
TESTMG32 GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         BCT   R5,TESTMG30                                                      
         B     TESTMG22                                                         
*                                                                               
TESTMGX  J     EXIT                                                             
         EJECT                                                                  
         DROP  T                                                                
*===========================================================*                   
* SET FLAGS IN BLOCK IF PAID SPOTS IN EACH BROADCAST MONTH                      
*===========================================================*                   
                                                                                
SETPAID  NTR1                                                                   
         L     R2,AREC                                                          
         AHI   R2,24                                                            
         USING REGELEM,R2                                                       
*                                                                               
SETPD2   LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    EXIT                                                             
         CLI   0(R2),X'0B'                                                      
         BL    SETPD2                                                           
         CLI   0(R2),X'0C'                                                      
         BH    SETPD2                                                           
         OC    4(2,R2),4(R2)       TEST PAID                                    
         BZ    SETPD2                                                           
* FIND MONTH IN TABLE                                                           
         L     R1,ABLOCK                                                        
         LHI   R0,12                                                            
*                                                                               
SETPD10  CLC   2(2,R2),0(R1)       TEST PRIOR TO MONTH START                    
         BL    SETPD12                                                          
         CLC   2(2,R2),2(R1)       TEST AFTER MONTH END                         
         BH    SETPD12                                                          
         MVI   4(R1),C'Y'          SET PAID FLAG                                
         B     SETPD2                                                           
*                                                                               
SETPD12  LA    R1,5(R1)                                                         
         BCT   R0,SETPD10                                                       
         DC    H'0'                HOW CAN I GET HERE ???                       
*                                                                               
*===============================================*                               
* CHECK SPOT DATE AGAINST BLOCK FOR MONTH PAID                                  
*===============================================*                               
TESTPAID NTR1                                                                   
         L     R1,ABLOCK                                                        
         LHI   R0,12                                                            
*                                                                               
TESTPD2  CLC   HALF(2),0(R1)       TEST PRIOR TO MONTH START                    
         BL    TESTPD4                                                          
         CLC   HALF(2),2(R1)       TEST AFTER MONTH END                         
         BH    TESTPD4                                                          
         CLI   4(R1),C'Y'          TEST MONTH PAID                              
         BNE   TESTPD4                                                          
         MVC   NERRCD,=Y(MNTHPAID) EXIT WITH FATAL ERROR                        
         J     ERREXITN                                                         
*                                                                               
TESTPD4  LA    R1,5(R1)                                                         
         BCT   R0,TESTPD2                                                       
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* FIND THE WORDS TO DESCRIBE THE STATUS OF THE ORDER                            
*==============================================================                 
         SPACE 1                                                                
GETSTAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,MGESTUSH                                                      
         CLI   SVSCR,X'F3'                                                      
         BE    GETSTAT2                                                         
*                                                                               
         LA    R2,MKOSTATH                                                      
         CLI   SVSCR,X'F4'                                                      
         BE    GETSTAT2                                                         
*                                                                               
         LA    R2,MKNSTATH                                                      
         CLI   SVSCR,X'F5'                                                      
         BE    GETSTAT2                                                         
*                                                                               
         CLI   SVSCR,X'F8'                                                      
         BE    GETSTATX            NO STATUS ON SKEVAL SCRN                     
         CLI   SVSCR,X'F2'                                                      
         BE    GETSTATX            OR MKO ORB SCRN                              
         DC    H'0'                                                             
*                                                                               
GETSTAT2 LA    R1,STATTAB                                                       
*                                                                               
GETSTAT4 CLC   SVMNSTAT,0(R1)                                                   
         BE    GETSTAT6                                                         
         LA    R1,L'STATTAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   GETSTAT4                                                         
         LA    R1,=C' ?????? '                                                  
*                                                                               
GETSTAT6 MVC   8(8,R2),1(R1)                                                    
         OI    6(R2),X'80'         XMT                                          
*                                                                               
GETSTATX XIT1                                                                   
*                                                                               
SETSTADR BASR  R1,RE               POINT R1 TO STATTAB AND RETURN               
*                                                                               
STATTAB  DS    0XL10                                                            
         DC    AL1(MNSTNEW),CL8'* NEW *',C'G'                                   
         DC    AL1(MNSTAPP),CL8'*APPRVD*',C'R'                                  
         DC    AL1(MNSTREJ),CL8'*RJCTD*',C'R'                                   
         DC    AL1(MNSTERR),CL8'*ERROR*',C'G'                                   
         DC    AL1(MNSTCAN),CL8'*CANCLD*',C'K'                                  
         DC    AL1(MNSTOKAY),CL8'*OKAYED*',C'K'                                 
         DC    AL1(MNSTAMND),CL8'*AMNDED*',C'G'                                 
         DC    AL1(MNSTCANM),CL8'*CAN/MO*',C'R'                                 
         DC    AL1(MNSTGOIN),CL8'*TOBEOK*',C'G'                                 
         DC    AL1(MNSTHOLD),CL8'*ON HLD*',C'G'                                 
         DC    AL1(MNSTDELV),CL8'*DLVRD*',C'R'                                  
         DC    AL1(MNSTSAPP),CL8'*S-APP*',C'R'                                  
         DC    X'FF'                                                            
*                                                                               
ORDSTADR BASR  R1,RE               POINT R1 TO STATUS TABLE                     
STATTAB2 DS    0XL3                                                             
         DC    AL1(DSENT),C'G',AL1(0)                                           
         DC    AL1(DFXSENT),C'G',AL1(0)                                         
         DC    AL1(DEMSENT),C'G',AL1(0)                                         
         DC    AL1(QRJCT),C'G',AL1(0)                                           
         DC    AL1(QEMPTY),C'G',AL1(0)                                          
         DC    AL1(QERRORED),C'G',AL1(0)                                        
         DC    AL1(QFAXCNCL),C'G',AL1(0)                                        
         DC    AL1(QRCLAPPR),C'G',AL1(0)                                        
         DC    AL1(QRCLDELN),C'G',AL1(0)                                        
         DC    AL1(QRCLUNKN),C'G',AL1(0)                                        
         DC    AL1(QRCLTRNS),C'G',AL1(0)                                        
         DC    AL1(QRCLWIP),C'G',AL1(0)                                         
         DC    AL1(QSNTPNDG),C'G',AL1(0)                                        
         DC    AL1(QSNTXCNF),C'G',AL1(0)                                        
         DC    AL1(QSNTXREJ),C'G',AL1(0)                                        
         DC    AL1(QTOBESNT),C'G',AL1(0)                                        
*                                                                               
         DC    AL1(QAPP),C'R',AL1(0)                                            
         DC    AL1(QRECALL),C'R',AL1(0)                                         
         DC    AL1(QRCLCONF),C'R',AL1(0)                                        
         DC    AL1(QRCLREJD),C'R',AL1(0)                                        
         DC    AL1(DDLVRD),C'R',AL1(0)                                          
         DC    AL1(QFAXDLVD),C'R',AL1(0)                                        
         DC    AL1(DFXDLVD),C'R',AL1(0)                                         
         DC    AL1(DEMDLVD),C'R',AL1(0)                                         
*                                                                               
* DON'T CHANGE ORDER OF BELOW STATUSES!!!!                                      
         DC    AL1(QCFMD),C'K',AL1(ORDTNCMT) CONFIRMED W/O COMMENTS (1)         
         DC    AL1(QCFMD),C'R',AL1(ORDTCMTS) CONFIRMED WITH COMMENTS(2)         
*                                                                               
         DC    AL1(QBYRCNFM),C'K',AL1(0)                                        
         DC    AL1(QCFMDPND),C'K',AL1(0)                                        
         DC    AL1(QNODARE),C'K',AL1(0)                                         
         DC    AL1(QUNDARE),C'K',AL1(0)                                         
         DC    X'FF'                                                            
*                                                                               
MISCFLG1 DC    AL1(0)                                                           
MF1CNFCM EQU   X'80'               THIS ORDER IS CONFIRMED W/COMMENTS           
MF1ORDER EQU   X'40'               PROCESS THE ENTIRE ORDER COLOR               
         SPACE 1                                                                
*============================================================                   
* TABLE ENTRIES ARE                                                             
* MGFLAG (ACTION)                                                               
* DSPL TO NEXT ENTRY                                                            
* ADDR OF ERROR IF ACTION NOT VALID                                             
* VAR LEN LIST OF VALID STATUSES FOR THIS ACTION                                
*============================================================                   
         SPACE 1                                                                
SETACTAB BASR  R1,RE               POINT R1 TO ACTTAB AND RETURN                
*                                                                               
ACTTAB   DS    0C                                                               
ACTJ     DC    C'J',AL1(ACTJX-ACTJ),Y(APPRJERR)  REJECT                         
         DC    AL1(MNSTNEW,MNSTAMND)                                            
ACTJX    EQU   *                                                                
*                                                                               
ACTA     DC    C'A',AL1(ACTAX-ACTA),Y(APPRJERR)  APPROVE                        
         DC    AL1(MNSTNEW,MNSTAMND)                                            
ACTAX    EQU   *                                                                
*                                                                               
ACT4     DC    C'4',AL1(ACT4X-ACT4),Y(MGCHGERR)  CHANGE OFFER                   
         DC    AL1(MNSTNEW,MNSTAMND)                                            
ACT4X    EQU   *                                                                
*                                                                               
ACT5     DC    C'5',AL1(ACT5X-ACT5),Y(MGCHGERR)  CHANGE MISSED                  
         DC    AL1(MNSTNEW,MNSTAMND)                                            
ACT5X    EQU   *                                                                
*                                                                               
ACTU     DC    C'U',AL1(ACTUX-ACTU),Y(UNPNDERR)  UNPEND (DDS ONLY)              
         DC    AL1(MNSTAPP,MNSTHOLD,MNSTGOIN,MNSTERR,MNSTCAN,MNSTCANM)          
ACTUX    EQU   *                                                                
*                                                                               
ACTC     DC    C'C',AL1(ACTCX-ACTC),Y(ACCPTERR)  ACCEPT (DDS ONLY)              
         DC    AL1(MNSTGOIN,MNSTHOLD)                                           
ACTCX    EQU   *                                                                
*                                                                               
ACTS     DC    C'S',AL1(ACTSX-ACTS),Y(BADMGEPF)  SELF APPLY                     
         DC    AL1(MNSTNEW,MNSTAPP,MNSTAMND)                                    
ACTSX    EQU   *                                                                
         DC    X'FF'                                                            
         LTORG                                                                  
ACTTABD  DSECT                                                                  
ACTCHAR  DS    CL1                                                              
ACTLEN   DS    XL1                                                              
ACTERR   DS    XL2                                                              
ACTSTATS DS    0X                                                               
*                                                                               
T21131   CSECT                                                                  
         EJECT                                                                  
*===============================================================                
* MKO/MKN RECORDS LIVE ON XSPFILES, SO DIRECT COMMANDS TO                       
* XSPFILES AS NEEDED                                                            
*===============================================================                
                                                                                
MYADDREC NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYADDX                                                           
         GOTO1 ADDREC                                                           
         J     EXIT                                                             
MYADDX   GOTO1 VDATAMGR,DMCB,=C'ADDREC',=C'XSPFIL',MYDA,AREC,DMWORK             
         J     EXIT                                                             
                                                                                
MYGETREC NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYGETX                                                           
         GOTO1 GETREC                                                           
         J     EXIT                                                             
MYGETX   GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MYDA,AREC,DMWORK             
         J     EXIT                                                             
*                                                                               
MYPUTREC NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         BNL   MYPUTX                                                           
         GOTO1 PUTREC                                                           
         J     EXIT                                                             
MYPUTX   GOTO1 VDATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',MYDA,AREC,DMWORK             
         J     EXIT                                                             
*                                                                               
MYHIGH   NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYHIGHX                                                          
         GOTO1 HIGH                                                             
         J     EXIT                                                             
*                                                                               
MYHIGHX  MVC   WORK2,WORK                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WORK2,WORK                   
         MVC   MYDA,WORK+36        SAVE DISK ADDRESS                            
         J     EXIT                                                             
*                                                                               
MYSEQ    NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'                                                       
         JNL   MYSEQX                                                           
         GOTO1 SEQ                                                              
         J     EXIT                                                             
*                                                                               
MYSEQX   GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',WORK2,WORK                   
         MVC   MYDA,WORK+36        SAVE DISK ADDRESS                            
         J     EXIT                                                             
*                                                                               
MYRECUP  NTR1  BASE=*,LABEL=*                                                   
         CLI   BUYST,C'0'          TEST US CABLE                                
         BL    MYRECUP5                                                         
* WE CAN'T USE THE C'T' IN THE 1ST PARAM BECAUSE RECUP WAS NOT CHANGED          
*  TO USE THE MAX RECORD SIZE THAT DMFILTAB HAS FOR XSPFIL                      
         MVI   0(R1),X'FE'                                                      
         LA    RE,XSPREC                                                        
         ST    RE,12(R1)                                                        
MYRECUP5 GOTO1 SVRECUP                                                          
         J     EXIT                                                             
XSPREC   DC    AL2(42,32,5970)                                                  
         EJECT                                                                  
*===============================================================                
* BOOK INCLUDED BELOW HAS MESSAGES IN MIXED CASE                                
* IF THEY ARE IN THE PROGRAM, YOU HAVE TO KEEP TURNING CAPS ON                  
       ++INCLUDE SPBUY31MSG                                                     
*===============================================================                
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
TLINED   DSECT                                                                  
         DS    CL8                 FAKE HEADER                                  
LTMISS   DS    CL7                                                              
         DS    CL1                                                              
LTMSRTG  DS    CL5                                                              
         DS    CL1                                                              
LTMG     DS    CL7                                                              
         DS    CL1                                                              
LTMGRTG  DS    CL5                                                              
         EJECT                                                                  
DEMOLIND DSECT                                                                  
DEMOBKH  DS    CL8                                                              
DEMOBK   DS    CL7                                                              
DEMOVALH DS    CL8                                                              
DEMOVAL  DS    CL6                                                              
DEMNEXTL DS    0C                                                               
         EJECT                                                                  
COMMLIND DSECT                                                                  
COMMENTH DS    CL8                                                              
COMMENT  DS    CL70                                                             
COMNEXTL DS    0C                                                               
*                                                                               
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER OR MG STATUS                           
ORDCOL   DS    XL1                 COLOR CODE                                   
ORDTYP   DS    XL1                 SECONDARY TYPE COMPARE                       
ORDTCMTS EQU   X'80'               WITH COMMENTS                                
ORDTNCMT EQU   X'40'               NO COMMENTS                                  
ORDLNQ   EQU   *-ORDD              LENGTH OF EACH ENTRY                         
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE SPDEMUPD                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPBUY31WRK                                                     
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SPGENRSN                                                       
       ++INCLUDE SPGENPURP                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDLINKD                                                        
         EJECT                                                                  
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087SPBUY31   11/03/20'                                      
         END                                                                    
